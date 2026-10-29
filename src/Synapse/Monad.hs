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
  , seHost
  , sePort
  , initEnv
  , initEnvAt
  , defaultEnv
  , withRequestContext
  , withViewKey
  , viewQualified
  , SchemaSource(..)
  , withSchemaSource
  , isCached

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

    -- * Logging
  , getLogger
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.Map.Strict as Map
import Synapse.Schema.Types
import Synapse.Backend.Discovery (Backend(..))
import Synapse.SchemaStore (Store, Plan)
import Synapse.Endpoint (Endpoint(..), plainAt)
import qualified Synapse.Log as Log
import qualified Katip

-- | Environment for Synapse operations
data SynapseEnv = SynapseEnv
  { seEndpoint :: !Endpoint                   -- ^ Where the service is reached (SCAFFOLD-21: ws or wss, its path)
  , seBackend :: !Text                        -- ^ Backend name (first CLI argument)
  , seCache   :: !(IORef (HashMap PluginHash PluginSchema))  -- ^ Schema cache
  , seVisited :: !(IORef (HashSet PluginHash))               -- ^ Cycle detection
  , seLogger  :: !Log.Logger                  -- ^ Structured logger
  , seToken   :: !(Maybe Text)                -- ^ JWT sent as Cookie: access_token=<jwt>
  , seCookies :: ![(Text, Text)]              -- ^ SAFE-S04/REQ-5: extra cookies for WS upgrade
  , seHeaders :: ![(Text, Text)]              -- ^ SAFE-S04/REQ-5: extra headers for WS upgrade
  , seViewKey :: !(Maybe Text)                -- ^ AUTH-8: the caller's view (its hash, else its kind); qualifies every cache key
  , seSchemaSource :: !SchemaSource           -- ^ SYNAPSE-28: where a schema comes from (the server, or the cache alone)
  , seFetched :: !(IORef (Map.Map Path PluginSchema))  -- ^ SYNAPSE-28: each path fetched once a run
  , sePlan    :: !(Maybe Plan)                -- ^ SYNAPSE-28: the call's plan, when the cache had one
  }

-- | SYNAPSE-28: where this run's schemas come from.
data SchemaSource
  = FromServer (Maybe Store)   -- ^ fetched, and kept in the store when there is one
  | FromCache Store            -- ^ the store alone: what it lacks is 'StaleSchema', never a fetch

-- | SYNAPSE-28: the run sends first, from the cache.
isCached :: SynapseEnv -> Bool
isCached env = case seSchemaSource env of
  FromCache _ -> True
  FromServer _ -> False

withSchemaSource :: SchemaSource -> Maybe Plan -> SynapseEnv -> SynapseEnv
withSchemaSource src plan env = env { seSchemaSource = src, sePlan = plan }

-- | Errors that can occur during Synapse operations
data SynapseError
  = NavError NavError
  | TransportError Text                    -- Keep for compatibility
  | TransportErrorContext TransportContext -- NEW: with context
  | ParseError Text
  | ValidationError Text
  | BackendError BackendErrorType [Backend]
  | StaleSchema Text
    -- ^ SYNAPSE-28: the cache misled the call (a method or flag it does not
    -- know, or the server refused what it said): forget it, ask the server,
    -- run the call again
  | CacheMiss Text
    -- ^ SYNAPSE-28: the cache has not got what the call needs (a schema it
    -- never fetched): ask the server, and keep what it says
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

-- | The service's host.
seHost :: SynapseEnv -> Text
seHost = epHost . seEndpoint

-- | The service's port.
sePort :: SynapseEnv -> Int
sePort = epPort . seEndpoint

-- | Initialize environment with given host/port/backend/logger/token, for a
-- service at plain @ws://HOST:PORT/@ ('initEnvAt' for any address).
-- Extra cookies/headers default to empty; use 'withRequestContext' to add
-- them after construction (SAFE-S04/REQ-5).
initEnv :: Text -> Int -> Text -> Log.Logger -> Maybe Text -> IO SynapseEnv
initEnv host port = initEnvAt (plainAt host port)

-- | SCAFFOLD-21: 'initEnv' for a service at any address (@wss://…/ws@ too).
initEnvAt :: Endpoint -> Text -> Log.Logger -> Maybe Text -> IO SynapseEnv
initEnvAt ep backend logger token = do
  cache <- newIORef HM.empty
  visited <- newIORef HS.empty
  fetched <- newIORef Map.empty
  pure SynapseEnv
    { seEndpoint = ep
    , seBackend = backend
    , seCache   = cache
    , seVisited = visited
    , seLogger  = logger
    , seToken   = token
    , seCookies = []
    , seHeaders = []
    , seViewKey = Nothing
    , seSchemaSource = FromServer Nothing
    , seFetched = fetched
    , sePlan = Nothing
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
  key <- asks (\e -> viewQualified (seViewKey e) hash)
  cache <- liftIO $ readIORef cacheRef
  pure $ HM.lookup key cache

-- | Insert a schema into the cache
insertCache :: PluginHash -> PluginSchema -> SynapseM ()
insertCache hash schema = do
  cacheRef <- asks seCache
  key <- asks (\e -> viewQualified (seViewKey e) hash)
  liftIO $ modifyIORef' cacheRef (HM.insert key schema)

-- | AUTH-8: a cache key under the caller's view — @VIEW/HASH@, or the hash
-- alone on a service with no view (as before AUTH). Two callers' views of one
-- service never share an entry, even where the service gave a schema one hash in
-- both.
viewQualified :: Maybe Text -> Text -> Text
viewQualified Nothing hash = hash
viewQualified (Just v) hash
  | T.null v = hash
  | otherwise = v <> "/" <> hash

-- | AUTH-8: the view this env's calls see (from @_info@).
withViewKey :: Maybe Text -> SynapseEnv -> SynapseEnv
withViewKey v env = env { seViewKey = v }

-- ============================================================================
-- Logging and Config
-- ============================================================================

-- | Get the logger from the environment
getLogger :: SynapseM Log.Logger
getLogger = asks seLogger
