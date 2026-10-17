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
  , registryDiscoveryAt

    -- * SCAFFOLD-21: an address that is not plain ws://HOST:PORT/
  , endpointFor

    -- * Backend Discovery
  , getBackendAt
  , getBackendAtWith
  , getBackendAtEp
  , probeBackendAtWith
  , probeBackendAtEp
  , probedInfo
  , probedInfoAt
  , probeFailureAt

    -- * Health Checks
  , pingBackend
  , pingBackends
  , pingBackendsVia

    -- * Registry Registration
  , registerWithRegistry

    -- * Registry Endpoint
  , registryUrlEnvVar
  , defaultRegistryHost
  , defaultRegistryPort
  , registryEndpointFromEnv
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
import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)
import Plexus.Auth (BackendInfo, parseBackendInfo)
import Control.Monad (void)
import Synapse.Endpoint (Endpoint(..), plainAt, substrateConfig, addressKey)
import qualified Network.WebSockets as WS
import qualified Plexus.Transport as ST
import Plexus.Types (PlexusStreamItem(..), TransportError(..))
import qualified Synapse.Log as Log
import qualified System.IO as IO
import qualified Data.Text.Read as TR
import System.Environment (lookupEnv)

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
registryDiscovery registryHost registryPort = registryDiscoveryAt (plainAt registryHost registryPort)

-- | SCAFFOLD-21: 'registryDiscovery' through a connection point at any
-- address (a hub behind TLS, on a path). The backends a registry lists
-- are plain @ws://HOST:PORT/@, as it records them; the connection point
-- itself is reached as it was named ('endpointFor').
registryDiscoveryAt :: Endpoint -> BackendDiscovery
registryDiscoveryAt ep = BackendDiscovery
  { discoverBackends = do
      -- First, discover the backend name using _info
      maybeBackendName <- either (const Nothing) Just <$> probeBackendAtEp [] ep
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
          registryBackends <- queryRegistry ep discoveredName

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
  , getBackendInfo   = queryBackend ep
  }
  where
    registryHost = epHost ep
    registryPort = epPort ep

-- | SCAFFOLD-21: how to reach a backend a discovery answered — the
-- connection point as it was named (its scheme, path, CAs) when the
-- backend is at its host and port, else plain @ws://HOST:PORT/@.
endpointFor :: Endpoint -> Backend -> Endpoint
endpointFor ep b
  | backendHost b == epHost ep && backendPort b == epPort ep = ep
  | otherwise = plainAt (backendHost b) (backendPort b)

-- | Get the backend name at a given host/port via _info
-- Returns whatever backend is running there (e.g., "registry-hub", "plexus")
-- If that backend has a registry plugin, it can be used to discover other backends
getBackendAt :: Text -> Int -> IO (Maybe Text)
getBackendAt host port = getBackendAtEp [] (plainAt host port)

-- | IDENTITY-16: 'getBackendAt', its _info probe carrying the caller's
-- upgrade headers (the token's cookie, --cookie, --header; see
-- 'Synapse.Transport.mergeUpgradeHeaders'). A backend that refuses
-- anonymous upgrades answers the probe only with them.
getBackendAtWith :: WS.Headers -> Text -> Int -> IO (Maybe Text)
getBackendAtWith headers host port = getBackendAtEp headers (plainAt host port)

-- | SCAFFOLD-21: 'getBackendAtWith' at any address.
getBackendAtEp :: WS.Headers -> Endpoint -> IO (Maybe Text)
getBackendAtEp headers ep = either (const Nothing) Just <$> probeBackendAtEp headers ep

-- | IDENTITY-43: the _info probe, saying why when it fails — so a caller
-- can tell the upgrade's 401 (its session ended) from a backend that is
-- down.
probeBackendAtWith :: WS.Headers -> Text -> Int -> IO (Either Text Text)
probeBackendAtWith headers host port = probeBackendAtEp headers (plainAt host port)

-- | SCAFFOLD-21: 'probeBackendAtWith' at any address. Behind TLS the
-- probe waits longer (a handshake across the internet, not loopback).
probeBackendAtEp :: WS.Headers -> Endpoint -> IO (Either Text Text)
probeBackendAtEp headers ep = do
  let cfg = substrateConfig ep "" headers   -- the backend is not used for _info
      secs = probeSecs ep
      timeout = do
        threadDelay (secs * 1000000)
        pure (Left $ ConnectionTimeout (epHost ep) (epPort ep))
      rpcCall = do
        ST.rpcCallWith cfg "_info" Aeson.Null
          `catch` \(e :: SomeException) ->
            pure (Left $ NetworkError ("Connection failed: " <> T.pack (show e)))

  -- Race the RPC call against the timeout
  raceResult <- race timeout rpcCall

  let failed why = do
        -- SCAFFOLD-21: kept, so a caller can say why (a certificate refused)
        modifyIORef' failMemo (Map.insert (addressKey ep) why)
        pure (Left why)
  case raceResult of
    Left _err ->
      failed "the _info probe timed out"
    Right (Left err) ->
      failed (T.pack (show err))
    Right (Right items) -> do
      -- Extract the backend name from subscription response
      case [Aeson.fromJSON content | StreamData _ _ _ content <- items] of
        (Aeson.Success v@(Aeson.Object obj):_) | Just (Aeson.String name) <- KM.lookup "backend" obj -> do
          -- AUTH-8: kept for the rest of the process — the view and the
          -- auth block this connection's credential was answered with
          mapM_ (\i -> modifyIORef' infoMemo (Map.insert (addressKey ep) i)) (parseBackendInfo v)
          pure (Right name)
        _ ->
          failed "the _info probe answered no backend name"

-- | How long the _info probe waits: 2 s on a plain socket (loopback, as
-- ever), 10 s behind TLS.
probeSecs :: Endpoint -> Int
probeSecs ep = if epTls ep then 10 else 2

-- | AUTH-8: what the last successful _info probe at HOST:PORT answered in
-- this process (its view and auth block), when it was probed.
probedInfo :: Text -> Int -> IO (Maybe BackendInfo)
probedInfo host port = probedInfoAt (plainAt host port)

-- | SCAFFOLD-21: 'probedInfo' at any address.
probedInfoAt :: Endpoint -> IO (Maybe BackendInfo)
probedInfoAt ep = Map.lookup (addressKey ep) <$> readIORef infoMemo

-- | SCAFFOLD-21: why the last _info probe at this address failed, in this
-- process (a TLS handshake's refusal, say), if it did.
probeFailureAt :: Endpoint -> IO (Maybe Text)
probeFailureAt ep = Map.lookup (addressKey ep) <$> readIORef failMemo

{-# NOINLINE failMemo #-}
failMemo :: IORef (Map.Map Text Text)
failMemo = unsafePerformIO (newIORef Map.empty)

{-# NOINLINE infoMemo #-}
infoMemo :: IORef (Map.Map Text BackendInfo)
infoMemo = unsafePerformIO (newIORef Map.empty)

-- | Query the registry for all backends
queryRegistry :: Endpoint -> Text -> IO [Backend]
queryRegistry ep backendName = do
  -- Try registry query first
  queryRegistryImpl ep backendName `catch` \(_e :: SomeException) ->
    -- Return empty list on error - caller will handle fallback
    pure []

-- | Implementation of registry query
queryRegistryImpl :: Endpoint -> Text -> IO [Backend]
queryRegistryImpl ep backendName = do
  let cfg = substrateConfig ep backendName []
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
queryBackend :: Endpoint -> Text -> IO (Maybe Backend)
queryBackend ep name = do
  -- First discover the backend name
  maybeBackendName <- getBackendAtEp [] ep
  case maybeBackendName of
    Just discoveredName -> do
      -- Check if the requested name matches the discovered backend
      if name == discoveredName
        then pure $ Just Backend
          { backendName        = discoveredName
          , backendDescription = "Backend discovered via _info"
          , backendHost        = epHost ep
          , backendPort        = epPort ep
          , backendVersion     = Nothing
          , backendReachable   = Nothing
          }
        else do
          -- Try to find it in the registry
          result <- catch
            (queryBackendImpl ep discoveredName name)
            (\(_e :: SomeException) -> pure Nothing)
          pure result
    Nothing -> getBackendInfo stubDiscovery name

-- | Implementation of backend get query
queryBackendImpl :: Endpoint -> Text -> Text -> IO (Maybe Backend)
queryBackendImpl ep backendName name = do
  let cfg = substrateConfig ep backendName []
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
pingBackend = pingBackendAt Nothing

-- | SCAFFOLD-21: ping at the backend's own address — the connection point
-- as it was named when the backend is there ('endpointFor'), which behind
-- TLS is given 5 s rather than 300 ms.
pingBackendAt :: Maybe Endpoint -> Backend -> IO Backend
pingBackendAt via backend = do
  let ep = maybe (plainAt (backendHost backend) (backendPort backend)) (`endpointFor` backend) via
      cfg = substrateConfig ep "" []  -- _info has no namespace prefix
      micros = if epTls ep then 5000000 else 300000
      timeout = threadDelay micros >> pure (Left $ ConnectionTimeout (backendHost backend) (backendPort backend))
      rpcCall = ST.rpcCallWith cfg "_info" Aeson.Null
        `catch` \(_e :: SomeException) -> pure (Left $ NetworkError "Connection failed")

  -- Race the RPC call against the timeout
  result <- race timeout rpcCall
  let reachable = case result of
        Left _          -> False  -- Timeout
        Right (Right _) -> True   -- Success
        Right (Left _)  -> False  -- RPC error

  pure $ backend { backendReachable = Just reachable }

-- | Ping all backends in parallel
pingBackends :: [Backend] -> IO [Backend]
pingBackends backends = mapConcurrently pingBackend backends

-- | SCAFFOLD-21: 'pingBackends', the connection point reached as named.
pingBackendsVia :: Endpoint -> [Backend] -> IO [Backend]
pingBackendsVia ep = mapConcurrently (pingBackendAt (Just ep))

-- ============================================================================
-- Registry Registration
-- ============================================================================

-- | Register a backend with the registry, if the registry is reachable.
--
-- Called when synapse connects to a backend at a non-default location.
-- Silently does nothing if the registry is unreachable or the call fails.
registerWithRegistry
  :: Text   -- ^ Registry host
  -> Int    -- ^ Registry port
  -> Text   -- ^ Registry backend name (discovered via _info at registry port)
  -> Text   -- ^ Backend name to register
  -> Text   -- ^ Backend host
  -> Int    -- ^ Backend port
  -> IO ()
registerWithRegistry registryHost registryPort registryBackend name host port = do
  let cfg = substrateConfig (plainAt registryHost registryPort) registryBackend []
      params = Aeson.object
        [ "name"     Aeson..= name
        , "host"     Aeson..= host
        , "port"     Aeson..= port
        , "protocol" Aeson..= ("ws" :: Text)
        ]
  -- Fire and forget — don't block the CLI on registry registration
  void $ async $ do
    _ <- ST.invokeMethod cfg ["registry"] "register" params
      `catch` \(_e :: SomeException) -> pure (Left $ NetworkError "Registry unavailable")
    pure ()

-- ============================================================================
-- Registry Endpoint
-- ============================================================================
--
-- SYNAPSE-7: the endpoint helpers synapse-cc 0.3.4 imports
-- (src/SynapseCC/RegistryResolve.hs). They were written with Z2H-6 on
-- the feature/Z2H-6-registry branch (e176373), which was never merged to
-- main, so synapse-cc's main did not build against synapse's. These are
-- that commit's definitions, unchanged and additive: nothing in synapse
-- calls them yet, so no behavior here moves.

-- | Environment variable naming the registry endpoint, shared with
-- plexus-transport's boot-time self-registration.
registryUrlEnvVar :: String
registryUrlEnvVar = "PLEXUS_REGISTRY_URL"

defaultRegistryHost :: Text
defaultRegistryHost = "127.0.0.1"

defaultRegistryPort :: Int
defaultRegistryPort = 4444

-- | Resolve the registry endpoint from @PLEXUS_REGISTRY_URL@
-- (e.g. @ws:\/\/127.0.0.1:4444@ or @host:port@), falling back to
-- @127.0.0.1:4444@. Malformed values fall back to the default rather
-- than failing the invocation.
registryEndpointFromEnv :: IO (Text, Int)
registryEndpointFromEnv = do
  mUrl <- lookupEnv registryUrlEnvVar
  pure $ case mUrl of
    Nothing -> (defaultRegistryHost, defaultRegistryPort)
    Just raw -> parseRegistryUrl (T.strip (T.pack raw))

-- | Parse @[ws://]host[:port]@.
parseRegistryUrl :: Text -> (Text, Int)
parseRegistryUrl raw
  | T.null raw = (defaultRegistryHost, defaultRegistryPort)
  | otherwise =
      let noScheme = case T.breakOn "://" raw of
            (_, rest) | not (T.null rest) -> T.drop 3 rest
            _                             -> raw
          noPath = T.takeWhile (/= '/') noScheme
          (host, portTxt) = case T.breakOnEnd ":" noPath of
            (h, p) | not (T.null h) -> (T.dropEnd 1 h, p)
            _                       -> (noPath, "")
      in case TR.decimal portTxt of
           Right (p, "") | not (T.null host) -> (host, p)
           _ | T.null noPath -> (defaultRegistryHost, defaultRegistryPort)
             | otherwise     -> (noPath, defaultRegistryPort)
