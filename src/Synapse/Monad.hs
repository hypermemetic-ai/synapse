{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Effect stack for Synapse operations
--
-- = The Monad
--
-- @
-- SynapseM = ExceptT SynapseError (ReaderT SynapseEnv IO)
-- @
--
-- Provides:
-- - Error handling via 'SynapseError'
-- - Environment with cache and cycle detection
-- - IO for network calls
module Synapse.Monad
  ( -- * The Monad
    SynapseM
  , runSynapseM
  , runSynapseM'

    -- * Environment
  , SynapseEnv(..)
  , initEnv
  , defaultEnv
  , withRequestContext

    -- * Errors
  , SynapseError(..)
  , BackendErrorType(..)
  , TransportContext(..)
  , TransportErrorCategory(..)
  , throwNav
  , throwTransport
  , throwTransportWith
  , throwParse
  , throwBackend

    -- * Cycle Detection
  , checkCycle
  , withFreshVisited

    -- * Cache Operations
  , lookupCache
  , insertCache

    -- * Connectome node cache (PLX-121) — keyed on the hash and nothing else
  , lookupNode
  , insertNode

    -- * The root document, fetched once per process
  , readRootDocument
  , writeRootDocument

    -- * Wire log (PLX-121)
  , getWireLog
  , wireEvent

    -- * Injectable document fetcher (PLX-121)
  , getDocFetcher
  , withDocFetcher

    -- * Logging
  , getLogger
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks, local)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T

import Synapse.Schema.Types
import Synapse.Backend.Discovery (Backend(..))
import qualified Synapse.Log as Log
import qualified Katip
import qualified Plexus.Connectome.Types as C
import Synapse.Wire (WireEvent, WireLog, newWireLog, recordEvent)

-- | Environment for Synapse operations
data SynapseEnv = SynapseEnv
  { seHost    :: !Text                        -- ^ Hub host
  , sePort    :: !Int                         -- ^ Hub port
  , seBackend :: !Text                        -- ^ Backend name (first CLI argument)
  , seCache   :: !(IORef (HashMap PluginHash PluginSchema))  -- ^ Schema cache
  , seVisited :: !(IORef (HashSet PluginHash))               -- ^ Cycle detection
  , seLogger  :: !Log.Logger                  -- ^ Structured logger
  , seToken   :: !(Maybe Text)                -- ^ JWT sent as Cookie: access_token=<jwt>
  , seCookies :: ![(Text, Text)]              -- ^ SAFE-S04/REQ-5: extra cookies for WS upgrade
  , seHeaders :: ![(Text, Text)]              -- ^ SAFE-S04/REQ-5: extra headers for WS upgrade
  , seNodeCache :: !(IORef (HashMap Text C.Activation))
    -- ^ PLX-121: Connectome nodes keyed on a Dynamic edge's __advertised
    -- hash__ and nothing else. Not the namespace, not the path, not a
    -- timestamp. A hash is a content identity, so a hit is safe by
    -- construction — but only while the key is the hash alone. This replaces
    -- the @(namespace, listMethodName)@ + 5-second-TTL shape: a mutable key
    -- invalidated by time, which was the correct response to a wire that had
    -- no content identity, and is obsolete now that one exists.
  , seRootDoc :: !(IORef (Maybe C.Connectome))
    -- ^ PLX-121: the whole-tree document. Fetched at most once per process —
    -- there is no advertised hash for the root to key a cache on, so this is
    -- memoisation, not caching, and it is named as such.
  , seWireLog :: !WireLog
    -- ^ PLX-121: the captured transcript of fetches and cache hits.
  , seDocFetcher :: !(Maybe (Maybe Text -> IO (Either Text Value)))
    -- ^ PLX-121: an injectable Connectome-document fetcher. 'Nothing' — the
    -- production value — goes over the wire.
    --
    -- This seam exists because c1 and c2 are stated in /counts of round
    -- trips/, and a count you cannot assert on is not a criterion. A live
    -- substrate proves the fetch happens and what it costs; only an injected
    -- fetcher can prove that the SECOND descent to a given hash calls nothing
    -- at all. Both are run: see @test\/ConnectomeNavSpec.hs@ and the captured
    -- wire log.
  }

-- | Errors that can occur during Synapse operations
data SynapseError
  = NavError NavError
  | TransportError Text                    -- Keep for compatibility
  | TransportErrorContext TransportContext -- NEW: with context
  | ParseError Text
  | ValidationError Text
  | BackendError BackendErrorType [Backend]
  deriving stock (Show, Eq)

-- | Backend-specific error types
data BackendErrorType
  = BackendNotFound Text
  | BackendUnreachable Text
  | NoBackendsAvailable
  | ProtocolHandshakeFailed Text Int  -- host, port - _info request failed/timed out
  deriving stock (Show, Eq)

-- | Transport error with connection details and categorization
data TransportContext = TransportContext
  { tcMessage  :: Text
  , tcHost     :: Text
  , tcPort     :: Int
  , tcBackend  :: Text
  , tcPath     :: Path
  , tcCategory :: TransportErrorCategory
  } deriving stock (Show, Eq)

-- | Categories of transport errors
data TransportErrorCategory
  = ConnectionRefused
  | ConnectionTimeout
  | ProtocolError
  | UnknownTransportError
  deriving stock (Show, Eq)

-- | The Synapse monad stack
newtype SynapseM a = SynapseM
  { unSynapseM :: ExceptT SynapseError (ReaderT SynapseEnv IO) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader SynapseEnv
    , MonadError SynapseError
    )

-- Note: PluginHash is a type alias for Text, which is already Hashable

-- | Run a SynapseM action with the given environment
runSynapseM :: SynapseEnv -> SynapseM a -> IO (Either SynapseError a)
runSynapseM env action = runReaderT (runExceptT (unSynapseM action)) env

-- | Run a SynapseM action with default host/port and specified backend (with error-level logger)
runSynapseM' :: Text -> SynapseM a -> IO (Either SynapseError a)
runSynapseM' backend action = do
  logger <- Log.makeLogger Katip.ErrorS
  env <- defaultEnv backend logger
  runSynapseM env action

-- | Initialize environment with given host/port/backend/logger/token.
-- Extra cookies/headers default to empty; use 'withRequestContext' to add
-- them after construction (SAFE-S04/REQ-5).
initEnv :: Text -> Int -> Text -> Log.Logger -> Maybe Text -> IO SynapseEnv
initEnv host port backend logger token = do
  cache <- newIORef HM.empty
  visited <- newIORef HS.empty
  nodeCache <- newIORef HM.empty
  rootDoc <- newIORef Nothing
  wireLog <- newWireLog
  pure SynapseEnv
    { seHost    = host
    , sePort    = port
    , seBackend = backend
    , seCache   = cache
    , seVisited = visited
    , seLogger  = logger
    , seToken   = token
    , seCookies = []
    , seHeaders = []
    , seNodeCache = nodeCache
    , seRootDoc = rootDoc
    , seWireLog = wireLog
    , seDocFetcher = Nothing
    }

-- | Augment a SynapseEnv with extra cookies and headers for the WS upgrade.
-- (SAFE-S04/REQ-5: --cookie/--header CLI flags and SYNAPSE_COOKIE_*/SYNAPSE_HEADER_* env)
withRequestContext :: [(Text, Text)] -> [(Text, Text)] -> SynapseEnv -> SynapseEnv
withRequestContext cookies headers env = env
  { seCookies = seCookies env ++ cookies
  , seHeaders = seHeaders env ++ headers
  }

-- | Default environment (localhost:4444, requires backend and logger)
defaultEnv :: Text -> Log.Logger -> IO SynapseEnv
defaultEnv backend logger = initEnv "127.0.0.1" 4444 backend logger Nothing

-- | Throw a navigation error
throwNav :: NavError -> SynapseM a
throwNav = throwError . NavError

-- | Throw a transport error
throwTransport :: Text -> SynapseM a
throwTransport = throwError . TransportError

-- | Throw a parse error
throwParse :: Text -> SynapseM a
throwParse = throwError . ParseError

-- | Throw a backend error
throwBackend :: BackendErrorType -> [Backend] -> SynapseM a
throwBackend errorType backends = throwError (BackendError errorType backends)

-- | Throw a transport error with context
throwTransportWith :: TransportContext -> SynapseM a
throwTransportWith = throwError . TransportErrorContext

-- ============================================================================
-- Cycle Detection
-- ============================================================================

-- | Check for cycles before descending into a child
-- Throws 'Cycle' error if hash was already visited
checkCycle :: PluginHash -> Path -> SynapseM ()
checkCycle hash path = do
  visitedRef <- asks seVisited
  visited <- liftIO $ readIORef visitedRef
  when (hash `HS.member` visited) $
    throwNav $ Cycle hash path
  liftIO $ modifyIORef' visitedRef (HS.insert hash)

-- | Run an action with a fresh visited set, restoring afterwards
-- Used at the start of each navigation to reset cycle detection
withFreshVisited :: SynapseM a -> SynapseM a
withFreshVisited action = do
  ref <- asks seVisited
  old <- liftIO $ readIORef ref
  liftIO $ writeIORef ref HS.empty
  result <- action
  liftIO $ writeIORef ref old
  pure result

-- ============================================================================
-- Cache Operations
-- ============================================================================

-- | Look up a schema in the cache by hash
lookupCache :: PluginHash -> SynapseM (Maybe PluginSchema)
lookupCache hash = do
  cacheRef <- asks seCache
  cache <- liftIO $ readIORef cacheRef
  pure $ HM.lookup hash cache

-- | Insert a schema into the cache
insertCache :: PluginHash -> PluginSchema -> SynapseM ()
insertCache hash schema = do
  cacheRef <- asks seCache
  liftIO $ modifyIORef' cacheRef (HM.insert hash schema)

-- ============================================================================
-- Connectome node cache (PLX-121)
-- ============================================================================

-- | Look a Connectome node up by a Dynamic edge's advertised hash.
--
-- The key is the advertised hash __exactly as advertised__. It is deliberately
-- never the hash of the document that came back: RFC §5.2 says a parent MUST
-- fold a Dynamic child's advertised hash and MUST NOT recompute one, and
-- 'Synapse.Cache.fetchCached' has the opposite asymmetry (it looks up on the
-- advertised hash and inserts on the fetched one), which makes a hit
-- unobservable the moment the two disagree. That asymmetry belongs to PLX-143;
-- this cache simply does not have it, because lookup and insert take the same
-- key from the same place.
lookupNode :: Text -> SynapseM (Maybe C.Activation)
lookupNode key = do
  ref <- asks seNodeCache
  HM.lookup key <$> liftIO (readIORef ref)

-- | Insert under the advertised hash. See 'lookupNode' for why that and not
-- the fetched node's own hash.
insertNode :: Text -> C.Activation -> SynapseM ()
insertNode key act = do
  ref <- asks seNodeCache
  liftIO $ modifyIORef' ref (HM.insert key act)

-- | The memoised whole-tree document, if it has been fetched this process.
readRootDocument :: SynapseM (Maybe C.Connectome)
readRootDocument = asks seRootDoc >>= liftIO . readIORef

writeRootDocument :: C.Connectome -> SynapseM ()
writeRootDocument doc = do
  ref <- asks seRootDoc
  liftIO $ writeIORef ref (Just doc)

-- ============================================================================
-- Wire log (PLX-121)
-- ============================================================================

getWireLog :: SynapseM WireLog
getWireLog = asks seWireLog

-- | Record one observable wire event.
wireEvent :: WireEvent -> SynapseM ()
wireEvent ev = do
  wl <- asks seWireLog
  liftIO $ recordEvent wl ev

-- | The Connectome-document fetcher in force. 'Nothing' means "go to the wire".
getDocFetcher :: SynapseM (Maybe (Maybe Text -> IO (Either Text Value)))
getDocFetcher = asks seDocFetcher

-- | Replace the document fetcher for one action. Used by the tests that assert
-- round-trip counts.
withDocFetcher
  :: (Maybe Text -> IO (Either Text Value))
  -> SynapseM a
  -> SynapseM a
withDocFetcher f = local (\env -> env {seDocFetcher = Just f})

-- ============================================================================
-- Logging and Config
-- ============================================================================

-- | Get the logger from the environment
getLogger :: SynapseM Log.Logger
getLogger = asks seLogger
