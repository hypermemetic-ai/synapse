{-# LANGUAGE OverloadedStrings #-}

-- | Structured logging for Synapse using katip
--
-- Provides filterable, subsystem-tagged logging with severity levels:
-- - ErrorS: Errors that need attention
-- - WarningS: Warnings about potential issues
-- - InfoS: High-level operations (connections, discoveries)
-- - DebugS: Detailed flow (schema fetches, cache hits/misses)
--
-- Default log level: ErrorS (only errors shown)
module Synapse.Log
  ( -- * Types
    Severity(..)
  , Subsystem(..)
  , Logger

    -- * Logger Creation
  , makeLogger
  , closeLogger

    -- * Logging Functions
  , logError
  , logWarn
  , logInfo
  , logDebug
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Katip
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (stdout)

-- | Subsystems for filtering (maps to katip namespace)
data Subsystem
  = SubsystemDiscovery
  | SubsystemTransport
  | SubsystemRPC
  | SubsystemSchema
  | SubsystemCache
  | SubsystemNavigation
  | SubsystemRendering
  | SubsystemCLI
  | SubsystemBidir
  deriving (Show, Eq)

-- | Logger handle
data Logger = Logger
  { logEnv :: LogEnv
  , logContext :: LogContexts
  , logNamespace :: Namespace
  }

-- | Convert subsystem to katip namespace
subsystemToNamespace :: Subsystem -> Namespace
subsystemToNamespace SubsystemDiscovery  = "discovery"
subsystemToNamespace SubsystemTransport  = "transport"
subsystemToNamespace SubsystemRPC        = "rpc"
subsystemToNamespace SubsystemSchema     = "schema"
subsystemToNamespace SubsystemCache      = "cache"
subsystemToNamespace SubsystemNavigation = "navigation"
subsystemToNamespace SubsystemRendering  = "rendering"
subsystemToNamespace SubsystemCLI        = "cli"
subsystemToNamespace SubsystemBidir      = "bidir"

-- | Create a logger with specified minimum severity
-- Default: ErrorS (only show errors)
makeLogger :: Severity -> IO Logger
makeLogger minSeverity = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout (permitItem minSeverity) V2
  env <- registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "synapse" "production"
  pure Logger
    { logEnv = env
    , logContext = mempty
    , logNamespace = "synapse"
    }

-- | Close logger and flush buffers
closeLogger :: Logger -> IO ()
closeLogger logger = void $ closeScribes (logEnv logger)

-- | Log an error message
logError :: MonadIO m => Logger -> Subsystem -> Text -> m ()
logError logger subsys msg = liftIO $
  runKatipContextT (logEnv logger) (logContext logger) (logNamespace logger <> subsystemToNamespace subsys) $
    logFM ErrorS (logStr msg)

-- | Log a warning message
logWarn :: MonadIO m => Logger -> Subsystem -> Text -> m ()
logWarn logger subsys msg = liftIO $
  runKatipContextT (logEnv logger) (logContext logger) (logNamespace logger <> subsystemToNamespace subsys) $
    logFM WarningS (logStr msg)

-- | Log an info message
logInfo :: MonadIO m => Logger -> Subsystem -> Text -> m ()
logInfo logger subsys msg = liftIO $
  runKatipContextT (logEnv logger) (logContext logger) (logNamespace logger <> subsystemToNamespace subsys) $
    logFM InfoS (logStr msg)

-- | Log a debug message
logDebug :: MonadIO m => Logger -> Subsystem -> Text -> m ()
logDebug logger subsys msg = liftIO $
  runKatipContextT (logEnv logger) (logContext logger) (logNamespace logger <> subsystemToNamespace subsys) $
    logFM DebugS (logStr msg)
