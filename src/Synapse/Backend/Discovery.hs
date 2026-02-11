{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Backend Discovery Abstraction
--
-- This module provides an abstraction layer for discovering Plexus RPC servers.
-- Currently implements a stub that returns hardcoded localhost:4444, but the
-- interface is designed to support future dynamic discovery via a directory
-- service.
--
-- = Future Vision
--
-- @
-- ┌─────────────┐     ┌──────────────────┐     ┌─────────────────┐
-- │   synapse   │────▶│  Hub Directory   │────▶│  Backend Hubs   │
-- │   (CLI)     │     │  (port 4444)     │     │  (discovered)   │
-- └─────────────┘     └──────────────────┘     └─────────────────┘
--                             │
--                             ▼
--                     Returns list of:
--                     - Hub name
--                     - Connection info (host:port or https URL)
--                     - Description
--                     - Schema hash
-- @
--
-- = Usage
--
-- @
-- main :: IO ()
-- main = do
--   let discovery = stubDiscovery
--   backends <- discoverBackends discovery
--   -- Use backends to build CLI subcommands
-- @
module Synapse.Backend.Discovery
  ( -- * Backend Type
    Backend(..)

    -- * Discovery Interface
  , BackendDiscovery(..)

    -- * Implementations
  , stubDiscovery
  , registryDiscovery

    -- * Backend Discovery
  , getBackendAt

    -- * Health Checks
  , pingBackend
  , pingBackends
  ) where

import Data.Aeson (ToJSON, FromJSON, (.:), (.:?), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.Exception (catch, SomeException)
import Data.Either (isRight)
import Control.Concurrent.Async (mapConcurrently, race, async, wait)
import Control.Concurrent (threadDelay)
import Data.IORef (newIORef, writeIORef, readIORef)
import Control.Monad (void)
import Plexus.Client (SubstrateConfig(..))
import qualified Plexus.Transport as ST
import Plexus.Types (PlexusStreamItem(..), TransportError(..))

-- ============================================================================
-- Backend Type
-- ============================================================================

-- | A discovered backend hub
data Backend = Backend
  { backendName        :: Text       -- ^ e.g., "plexus"
  , backendDescription :: Text       -- ^ Human-readable description
  , backendHost        :: Text       -- ^ Host to connect to
  , backendPort        :: Int        -- ^ Port
  , backendVersion     :: Maybe Text -- ^ Version if known
  , backendReachable   :: Maybe Bool -- ^ Whether backend is reachable (Nothing = not checked)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- ============================================================================
-- Discovery Interface
-- ============================================================================

-- | Backend discovery interface
--
-- This is a record-of-functions pattern that allows swapping discovery
-- implementations without changing calling code. In the future, this could
-- become a typeclass if we need more sophisticated dispatch.
data BackendDiscovery = BackendDiscovery
  { discoverBackends :: IO [Backend]
      -- ^ Query for all available backends
  , getBackendInfo   :: Text -> IO (Maybe Backend)
      -- ^ Get info for a specific backend by name
  }

-- ============================================================================
-- Stub Implementation
-- ============================================================================

-- | Stub discovery that returns empty list
--
-- Used as fallback when connection fails completely.
stubDiscovery :: BackendDiscovery
stubDiscovery = BackendDiscovery
  { discoverBackends = pure []
  , getBackendInfo   = \_ -> pure Nothing
  }

-- ============================================================================
-- Registry Types
-- ============================================================================

-- | Registry backend info (from registry activation)
data RegistryBackendInfo = RegistryBackendInfo
  { rbiName        :: Text
  , rbiHost        :: Text
  , rbiPort        :: Int
  , rbiProtocol    :: Text
  , rbiDescription :: Maybe Text
  , rbiIsActive    :: Bool
  }
  deriving (Show, Generic)

instance FromJSON RegistryBackendInfo where
  parseJSON = withObject "RegistryBackendInfo" $ \o -> do
    name <- o .: "name"
    host <- o .: "host"
    port <- o .: "port"
    protocol <- o .: "protocol"
    desc <- o .:? "description"
    active <- o .: "is_active"
    pure $ RegistryBackendInfo name host port protocol desc active

-- | Registry event wrapper
data RegistryEvent
  = BackendsEvent { backends :: [RegistryBackendInfo] }
  | BackendEvent { backend :: Maybe RegistryBackendInfo }
  deriving (Show, Generic)

instance FromJSON RegistryEvent where
  parseJSON = withObject "RegistryEvent" $ \o -> do
    eventType <- o .: "type"  -- Rust uses #[serde(tag = "type")]
    case eventType :: Text of
      "backends" -> BackendsEvent <$> o .: "backends"
      "backend"  -> BackendEvent <$> o .: "backend"
      _          -> fail $ "Unknown registry event type: " ++ T.unpack eventType

-- | Backend info response from _info endpoint
data BackendInfoResponse = BackendInfoResponse
  { birBackend :: Text
  }
  deriving (Show, Generic)

instance FromJSON BackendInfoResponse where
  parseJSON = withObject "BackendInfoResponse" $ \o -> do
    backendName <- o .: "backend"
    pure $ BackendInfoResponse backendName

-- ============================================================================
-- Registry-based Discovery
-- ============================================================================

-- | Query the registry activation for available backends
--
-- Connects to the registry service and queries for registered backends.
-- Falls back to stubDiscovery if registry is unavailable.
registryDiscovery :: Text -> Int -> BackendDiscovery
registryDiscovery registryHost registryPort = BackendDiscovery
  { discoverBackends = do
      -- First, discover the backend name using _info
      maybeBackendName <- discoverBackendName registryHost registryPort
      case maybeBackendName of
        Just discoveredName -> do
          -- Create a Backend entry for the discovered backend
          let discoveredBackend = Backend
                { backendName        = discoveredName
                , backendDescription = "Backend discovered via _info"
                , backendHost        = registryHost
                , backendPort        = registryPort
                , backendVersion     = Nothing
                , backendReachable   = Nothing
                }

          -- Try to get additional backends from registry (if it exists)
          registryBackends <- queryRegistry registryHost registryPort discoveredName

          -- Only include discovered backend if not already in registry
          -- (prefer registry entry since it has more details)
          let matchesDiscovered b = backendName b == discoveredName
                                 && backendHost b == registryHost
                                 && backendPort b == registryPort
              isDuplicate = any matchesDiscovered registryBackends
              backends = if isDuplicate
                          then registryBackends
                          else [discoveredBackend] ++ registryBackends

          pure backends
        Nothing ->
          -- Connection failed completely
          discoverBackends stubDiscovery
  , getBackendInfo   = queryBackend registryHost registryPort
  }

-- | Get the backend name at a given host/port via _info
-- Returns whatever backend is running there (e.g., "registry-hub", "plexus")
-- If that backend has a registry plugin, it can be used to discover other backends
getBackendAt :: Text -> Int -> IO (Maybe Text)
getBackendAt = discoverBackendName

-- | Discover the backend name by calling _info
discoverBackendName :: Text -> Int -> IO (Maybe Text)
discoverBackendName host port = do
  let cfg = SubstrateConfig
        { substrateHost = T.unpack host
        , substratePort = port
        , substratePath = "/"
        , substrateBackend = "" -- Not used for _info
        }
  result <- ST.rpcCallWith cfg "_info" Aeson.Null
    `catch` \(_e :: SomeException) -> pure (Left $ NetworkError "Connection failed")

  case result of
    Left _err -> pure Nothing
    Right items -> do
      -- Extract the backend name from subscription response
      case [Aeson.fromJSON content | StreamData _ _ _ content <- items] of
        (Aeson.Success (Aeson.Object obj):_) | Just (Aeson.String name) <- KM.lookup "backend" obj ->
          pure (Just name)
        _ -> pure Nothing

-- | Query the registry for all backends
queryRegistry :: Text -> Int -> Text -> IO [Backend]
queryRegistry host port backendName = do
  -- Try registry query first
  queryRegistryImpl host port backendName `catch` \(_e :: SomeException) ->
    -- Return empty list on error - caller will handle fallback
    pure []

-- | Implementation of registry query
queryRegistryImpl :: Text -> Int -> Text -> IO [Backend]
queryRegistryImpl host port backendName = do
  let cfg = SubstrateConfig
        { substrateHost = T.unpack host
        , substratePort = port
        , substratePath = "/"
        , substrateBackend = backendName
        }
  -- Call registry.list through the backend
  result <- ST.invokeMethod cfg ["registry"] "list" (Aeson.object [])
  case result of
    Left _err -> pure []
    Right items -> do
      -- Extract content from stream items
      let contents = [c | StreamData _ _ _ c <- items]
      let events = [e | Aeson.Success e <- map Aeson.fromJSON contents]
      let backendInfos = concat [bs | BackendsEvent bs <- events]
      pure $ map convertBackend backendInfos

-- | Query for a specific backend by name
queryBackend :: Text -> Int -> Text -> IO (Maybe Backend)
queryBackend host port name = do
  -- First discover the backend name
  maybeBackendName <- discoverBackendName host port
  case maybeBackendName of
    Just discoveredName -> do
      -- Check if the requested name matches the discovered backend
      if name == discoveredName
        then pure $ Just Backend
          { backendName        = discoveredName
          , backendDescription = "Backend discovered via _info"
          , backendHost        = host
          , backendPort        = port
          , backendVersion     = Nothing
          , backendReachable   = Nothing
          }
        else do
          -- Try to find it in the registry
          result <- catch
            (queryBackendImpl host port discoveredName name)
            (\(_e :: SomeException) -> pure Nothing)
          pure result
    Nothing -> getBackendInfo stubDiscovery name

-- | Implementation of backend get query
queryBackendImpl :: Text -> Int -> Text -> Text -> IO (Maybe Backend)
queryBackendImpl host port backendName name = do
  let cfg = SubstrateConfig
        { substrateHost = T.unpack host
        , substratePort = port
        , substratePath = "/"
        , substrateBackend = backendName
        }
      params = Aeson.object ["name" Aeson..= name]
  result <- ST.invokeMethod cfg ["registry"] "get" params
  case result of
    Left _err -> pure Nothing
    Right items -> do
      -- Extract content from stream items
      let contents = [c | StreamData _ _ _ c <- items]
      let events = [e | Aeson.Success e <- map Aeson.fromJSON contents]
      case [b | BackendEvent (Just b) <- events] of
        (info:_) -> pure $ Just (convertBackend info)
        []       -> pure Nothing

-- | Convert registry backend info to discovery backend
convertBackend :: RegistryBackendInfo -> Backend
convertBackend info = Backend
  { backendName        = rbiName info
  , backendDescription = maybe "" id (rbiDescription info)
  , backendHost        = rbiHost info
  , backendPort        = rbiPort info
  , backendVersion     = Nothing
  , backendReachable   = Nothing -- Will be checked separately
  }

-- | Ping a backend to check if it's reachable (with 300ms timeout)
-- 300ms allows for WebSocket connection establishment overhead (~150-200ms)
-- plus actual RPC round-trip time
pingBackend :: Backend -> IO Backend
pingBackend backend = do
  let cfg = SubstrateConfig
        { substrateHost = T.unpack (backendHost backend)
        , substratePort = backendPort backend
        , substratePath = "/"
        , substrateBackend = ""  -- _info has no namespace prefix
        }
      timeout = threadDelay 300000 >> pure (Left $ ConnectionTimeout (backendHost backend) (backendPort backend))
      rpcCall = ST.rpcCallWith cfg "_info" Aeson.Null
        `catch` \(_e :: SomeException) -> pure (Left $ NetworkError "Connection failed")

  -- Race the RPC call against a 300ms timeout
  result <- race timeout rpcCall
  let reachable = case result of
        Left _          -> False  -- Timeout
        Right (Right _) -> True   -- Success
        Right (Left _)  -> False  -- RPC error

  pure $ backend { backendReachable = Just reachable }

-- | Ping all backends in parallel
pingBackends :: [Backend] -> IO [Backend]
pingBackends backends = mapConcurrently pingBackend backends
