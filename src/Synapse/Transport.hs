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
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T

import Plexus.Client (SubstrateConfig(..))
import qualified Plexus.Transport as ST

import Synapse.Schema.Types
import Synapse.Monad

-- | Fetch the root schema
fetchSchema :: SynapseM PluginSchema
fetchSchema = fetchSchemaAt []

-- | Fetch schema at a specific path
fetchSchemaAt :: Path -> SynapseM PluginSchema
fetchSchemaAt path = do
  cfg <- getConfig
  result <- liftIO $ ST.fetchSchemaAt cfg path
  case result of
    Left err -> throwNav $ FetchError err path
    Right schema -> pure schema

-- | Fetch a specific method's schema (more efficient than full plugin schema)
-- Uses the parameter-based query: plugin.schema with {"method": "name"}
fetchMethodSchema :: Path -> Text -> SynapseM MethodSchema
fetchMethodSchema path methodName = do
  cfg <- getConfig
  result <- liftIO $ ST.fetchMethodSchemaAt cfg path methodName
  case result of
    Left err -> throwNav $ FetchError err path
    Right schema -> pure schema

-- | Invoke a method and return stream items
invoke :: Path -> Text -> Value -> SynapseM [HubStreamItem]
invoke namespacePath method params = do
  cfg <- getConfig
  result <- liftIO $ ST.invokeMethod cfg namespacePath method params
  case result of
    Left err -> do
      -- Build context from environment
      host <- asks seHost
      port <- asks sePort
      backend <- asks seBackend
      let path = namespacePath ++ [method]
      let ctx = TransportContext
            { tcMessage  = err
            , tcHost     = host
            , tcPort     = port
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = categorizeTransportError err
            }
      throwTransportWith ctx
    Right items -> pure items

-- | Invoke with raw method path
invokeRaw :: Text -> Value -> SynapseM [HubStreamItem]
invokeRaw method params = do
  cfg <- getConfig
  result <- liftIO $ ST.invokeRaw cfg method params
  case result of
    Left err -> do
      -- Build context from environment
      host <- asks seHost
      port <- asks sePort
      backend <- asks seBackend
      let path = T.splitOn "." method
      let ctx = TransportContext
            { tcMessage  = err
            , tcHost     = host
            , tcPort     = port
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = categorizeTransportError err
            }
      throwTransportWith ctx
    Right items -> pure items

-- | Invoke a method with streaming output - calls callback for each item
invokeStreaming :: Path -> Text -> Value -> (HubStreamItem -> IO ()) -> SynapseM ()
invokeStreaming namespacePath method params onItem = do
  cfg <- getConfig
  result <- liftIO $ ST.invokeMethodStreaming cfg namespacePath method params onItem
  case result of
    Left err -> do
      -- Build context from environment
      host <- asks seHost
      port <- asks sePort
      backend <- asks seBackend
      let path = namespacePath ++ [method]
      let ctx = TransportContext
            { tcMessage  = err
            , tcHost     = host
            , tcPort     = port
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = categorizeTransportError err
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
