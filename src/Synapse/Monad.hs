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

    -- * Errors
  , SynapseError(..)
  , BackendErrorType(..)
  , TransportContext(..)
  , TransportErrorCategory(..)
  , throwNav
  , throwTransport
  , throwTransportWith
  , categorizeTransportError
  , throwParse
  , throwBackend

    -- * Cycle Detection
  , checkCycle
  , withFreshVisited

    -- * Cache Operations
  , lookupCache
  , insertCache
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

import Synapse.Schema.Types
import Synapse.Backend.Discovery (Backend(..))

-- | Environment for Synapse operations
data SynapseEnv = SynapseEnv
  { seHost    :: !Text                        -- ^ Hub host
  , sePort    :: !Int                         -- ^ Hub port
  , seBackend :: !Text                        -- ^ Backend name (first CLI argument)
  , seCache   :: !(IORef (HashMap PluginHash PluginSchema))  -- ^ Schema cache
  , seVisited :: !(IORef (HashSet PluginHash))               -- ^ Cycle detection
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

-- | Run a SynapseM action with default host/port and specified backend
runSynapseM' :: Text -> SynapseM a -> IO (Either SynapseError a)
runSynapseM' backend action = do
  env <- defaultEnv backend
  runSynapseM env action

-- | Initialize environment with given host/port/backend
initEnv :: Text -> Int -> Text -> IO SynapseEnv
initEnv host port backend = do
  cache <- newIORef HM.empty
  visited <- newIORef HS.empty
  pure SynapseEnv
    { seHost = host
    , sePort = port
    , seBackend = backend
    , seCache = cache
    , seVisited = visited
    }

-- | Default environment (localhost:4444, requires backend)
defaultEnv :: Text -> IO SynapseEnv
defaultEnv = initEnv "127.0.0.1" 4444

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

-- | Categorize transport errors from exception messages
categorizeTransportError :: Text -> TransportErrorCategory
categorizeTransportError msg
  | "refused" `T.isInfixOf` T.toLower msg = ConnectionRefused
  | "timeout" `T.isInfixOf` T.toLower msg = ConnectionTimeout
  | "protocol" `T.isInfixOf` T.toLower msg = ProtocolError
  | otherwise = UnknownTransportError

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
