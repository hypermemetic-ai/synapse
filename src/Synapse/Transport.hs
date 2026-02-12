{-# LANGUAGE ScopedTypeVariables #-}

-- | Transport layer for Synapse
--
-- Bridges the low-level Substrate transport to the SynapseM monad.
module Synapse.Transport
  ( -- * Schema Fetching
    fetchSchema
  , fetchSchemaAt
  , fetchMethodSchema

    -- * Method Invocation (collected)
  , invoke
  , invokeRaw

    -- * Method Invocation (streaming)
  , invokeStreaming
  , invokeStreamingWithBidir

    -- * Bidirectional Response
  , sendResponse
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T

import Plexus.Client (SubstrateConfig(..))
import qualified Plexus.Transport as ST
import qualified Plexus.Types as PT

import Synapse.Schema.Types
import Synapse.Monad

-- | Fetch the root schema
fetchSchema :: SynapseM PluginSchema
fetchSchema = fetchSchemaAt []

-- | Convert plexus-protocol TransportError to error message
transportErrorToText :: PT.TransportError -> Text
transportErrorToText (PT.ConnectionRefused host port) =
  "Connection refused to " <> host <> ":" <> T.pack (show port)
transportErrorToText (PT.ConnectionTimeout host port) =
  "Connection timeout to " <> host <> ":" <> T.pack (show port)
transportErrorToText (PT.ProtocolError msg) = "Protocol error: " <> msg
transportErrorToText (PT.NetworkError msg) = "Network error: " <> msg

-- | Convert plexus-protocol TransportError to TransportCategory
transportErrorToCategory :: PT.TransportError -> TransportErrorCategory
transportErrorToCategory (PT.ConnectionRefused _ _) = ConnectionRefused
transportErrorToCategory (PT.ConnectionTimeout _ _) = ConnectionTimeout
transportErrorToCategory (PT.ProtocolError _) = ProtocolError
transportErrorToCategory (PT.NetworkError _) = UnknownTransportError

-- | Fetch schema at a specific path
fetchSchemaAt :: Path -> SynapseM PluginSchema
fetchSchemaAt path = do
  cfg <- getConfig
  result <- liftIO $ ST.fetchSchemaAt cfg path
  case result of
    Left err -> throwNav $ FetchError (transportErrorToText err) path
    Right schema -> pure schema

-- | Fetch a specific method's schema (more efficient than full plugin schema)
-- Uses the parameter-based query: plugin.schema with {"method": "name"}
fetchMethodSchema :: Path -> Text -> SynapseM MethodSchema
fetchMethodSchema path methodName = do
  cfg <- getConfig
  result <- liftIO $ ST.fetchMethodSchemaAt cfg path methodName
  case result of
    Left err -> throwNav $ FetchError (transportErrorToText err) path
    Right schema -> pure schema

-- | Invoke a method and return stream items
invoke :: Path -> Text -> Value -> SynapseM [HubStreamItem]
invoke namespacePath method params = do
  cfg <- getConfig
  result <- liftIO $ ST.invokeMethod cfg namespacePath method params
  case result of
    Left transportErr -> do
      -- Build context from typed transport error and environment
      backend <- asks seBackend
      let path = namespacePath ++ [method]
      let ctx = TransportContext
            { tcMessage  = transportErrorToText transportErr
            , tcHost     = getHost transportErr cfg
            , tcPort     = getPort transportErr cfg
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = transportErrorToCategory transportErr
            }
      throwTransportWith ctx
    Right items -> pure items
  where
    getHost (PT.ConnectionRefused h _) _ = h
    getHost (PT.ConnectionTimeout h _) _ = h
    getHost _ c = T.pack $ substrateHost c
    getPort (PT.ConnectionRefused _ p) _ = p
    getPort (PT.ConnectionTimeout _ p) _ = p
    getPort _ c = substratePort c

-- | Invoke with raw method path
invokeRaw :: Text -> Value -> SynapseM [HubStreamItem]
invokeRaw method params = do
  cfg <- getConfig
  result <- liftIO $ ST.invokeRaw cfg method params
  case result of
    Left transportErr -> do
      -- Build context from typed transport error and environment
      backend <- asks seBackend
      let path = T.splitOn "." method
      let ctx = TransportContext
            { tcMessage  = transportErrorToText transportErr
            , tcHost     = getHost transportErr cfg
            , tcPort     = getPort transportErr cfg
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = transportErrorToCategory transportErr
            }
      throwTransportWith ctx
    Right items -> pure items
  where
    getHost (PT.ConnectionRefused h _) _ = h
    getHost (PT.ConnectionTimeout h _) _ = h
    getHost _ c = T.pack $ substrateHost c
    getPort (PT.ConnectionRefused _ p) _ = p
    getPort (PT.ConnectionTimeout _ p) _ = p
    getPort _ c = substratePort c

-- | Invoke a method with streaming output - calls callback for each item
invokeStreaming :: Path -> Text -> Value -> (HubStreamItem -> IO ()) -> SynapseM ()
invokeStreaming namespacePath method params onItem = do
  cfg <- getConfig
  result <- liftIO $ ST.invokeMethodStreaming cfg namespacePath method params onItem
  case result of
    Left transportErr -> do
      -- Build context from typed transport error and environment
      backend <- asks seBackend
      let path = namespacePath ++ [method]
      let ctx = TransportContext
            { tcMessage  = transportErrorToText transportErr
            , tcHost     = getHost transportErr cfg
            , tcPort     = getPort transportErr cfg
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = transportErrorToCategory transportErr
            }
      throwTransportWith ctx
    Right () -> pure ()
  where
    getHost (PT.ConnectionRefused h _) _ = h
    getHost (PT.ConnectionTimeout h _) _ = h
    getHost _ c = T.pack $ substrateHost c
    getPort (PT.ConnectionRefused _ p) _ = p
    getPort (PT.ConnectionTimeout _ p) _ = p
    getPort _ c = substratePort c

-- | Invoke a method with streaming output and bidirectional request handling
-- When a StreamRequest is received, the bidirectional handler is called and
-- the response is sent back via {backend}.respond
invokeStreamingWithBidir
  :: Path
  -> Text
  -> Value
  -> (HubStreamItem -> IO ())           -- ^ Handler for non-request items
  -> (Text -> StandardRequest -> IO StandardResponse)  -- ^ Handler for bidirectional requests
  -> SynapseM ()
invokeStreamingWithBidir namespacePath method params onItem onBidirRequest = do
  cfg <- getConfig
  let wrappedHandler item = case item of
        PT.StreamRequest _ _ reqId reqData _timeout -> do
          -- Handle the bidirectional request
          response <- onBidirRequest reqId reqData
          -- Send the response back via {backend}.respond
          _ <- ST.sendBidirectionalResponse cfg reqId response
          -- Don't forward StreamRequest to the regular handler
          pure ()
        _ -> onItem item
  result <- liftIO $ ST.invokeMethodStreaming cfg namespacePath method params wrappedHandler
  case result of
    Left transportErr -> do
      backend <- asks seBackend
      let path = namespacePath ++ [method]
      let ctx = TransportContext
            { tcMessage  = transportErrorToText transportErr
            , tcHost     = getHost transportErr cfg
            , tcPort     = getPort transportErr cfg
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = transportErrorToCategory transportErr
            }
      throwTransportWith ctx
    Right () -> pure ()
  where
    getHost (PT.ConnectionRefused h _) _ = h
    getHost (PT.ConnectionTimeout h _) _ = h
    getHost _ c = T.pack $ substrateHost c
    getPort (PT.ConnectionRefused _ p) _ = p
    getPort (PT.ConnectionTimeout _ p) _ = p
    getPort _ c = substratePort c

-- | Send a response for a bidirectional request
sendResponse :: Text -> StandardResponse -> SynapseM ()
sendResponse requestId response = do
  cfg <- getConfig
  result <- liftIO $ ST.sendBidirectionalResponse cfg requestId response
  case result of
    Left transportErr -> do
      backend <- asks seBackend
      let ctx = TransportContext
            { tcMessage  = transportErrorToText transportErr
            , tcHost     = T.pack $ substrateHost cfg
            , tcPort     = substratePort cfg
            , tcBackend  = backend
            , tcPath     = ["respond"]
            , tcCategory = transportErrorToCategory transportErr
            }
      throwTransportWith ctx
    Right () -> pure ()

-- | Get SubstrateConfig from environment
getConfig :: SynapseM SubstrateConfig
getConfig = do
  host <- asks seHost
  port <- asks sePort
  backend <- asks seBackend
  pure $ SubstrateConfig
    { substrateHost = T.unpack host
    , substratePort = port
    , substratePath = "/"
    , substrateBackend = backend
    }
