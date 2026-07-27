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
  , invokeStreamingWithBidirCancellable

    -- * Bidirectional Response
  , sendResponse

    -- * Substrate Config Access (SYN-2)
  , getConfig
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Plexus.Client (SubstrateConfig(..), CancelTurn(..), cookieHeader)
import qualified Plexus.Transport as ST
import qualified Plexus.Types as PT

import Synapse.Schema.Types
import Synapse.Monad
import qualified Synapse.Log as Log

-- | Fetch the root schema
fetchSchema :: SynapseM PluginSchema
fetchSchema = do
  logger <- getLogger
  Log.logInfo logger Log.SubsystemSchema "Fetching root schema"
  fetchSchemaAt []

-- | Convert plexus-protocol TransportError to error message.
--
-- SAFE-5 + REQ-5: detect embedded JSON-RPC semantic error codes in the
-- protocol-error message and rewrite them with meaningful prefixes.
-- The wire format wraps RpcError as @"Subscription error: RpcErrorObj
-- {errCode = N, errMessage = \\"...\\", errData = ...}"@; we parse N
-- to render semantic prefixes for the four documented codes:
--
-- * @-32001@ Authentication required (REQ-5 hint added downstream)
-- * @-32602@ Invalid parameters
-- * @-32601@ Method not found
-- * @-32000@ Execution error
transportErrorToText :: PT.TransportError -> Text
transportErrorToText (PT.ConnectionRefused host port) =
  "Connection refused to " <> host <> ":" <> T.pack (show port)
transportErrorToText (PT.ConnectionTimeout host port) =
  "Connection timeout to " <> host <> ":" <> T.pack (show port)
transportErrorToText (PT.ProtocolError msg) = renderProtocolError msg
transportErrorToText (PT.NetworkError msg) = "Network error: " <> msg

-- | Inspect a protocol-error message for an embedded JSON-RPC error code
-- and rewrite with a semantic prefix. Falls back to the original message
-- when no recognized code is found.
renderProtocolError :: Text -> Text
renderProtocolError msg =
  case extractErrCode msg of
    Just (-32001) -> "Authentication required: " <> extractErrMessage msg
                  <> "\nHint: pass --token <jwt>, set SYNAPSE_TOKEN, or store an access_token ref in ~/.plexus/<backend>/defaults.json (see `synapse _self`)"
    Just (-32602) -> "Invalid parameters: " <> extractErrMessage msg
                  <> "\nHint: run with --help for the method to see expected params"
    Just (-32601) -> "Method not found: " <> extractErrMessage msg
                  <> "\nHint: run 'synapse <backend>' to list available methods"
    Just (-32000) -> "Execution error: " <> extractErrMessage msg
    _             -> "Protocol error: " <> msg

-- | Pull the @errCode = N@ integer out of a 'show'-ed RpcErrorObj.
extractErrCode :: Text -> Maybe Int
extractErrCode msg =
  case T.breakOn "errCode = " msg of
    (_, rest) | not (T.null rest) ->
      let after = T.drop (T.length "errCode = ") rest
          digits = T.takeWhile (\c -> c == '-' || (c >= '0' && c <= '9')) after
      in case T.unpack digits of
           "" -> Nothing
           d  -> Just (read d)
    _ -> Nothing

-- | Pull the @errMessage = "..."@ string out of a 'show'-ed RpcErrorObj.
-- Returns the empty string if no message can be parsed.
extractErrMessage :: Text -> Text
extractErrMessage msg =
  case T.breakOn "errMessage = \"" msg of
    (_, rest) | not (T.null rest) ->
      let after = T.drop (T.length "errMessage = \"") rest
      in T.takeWhile (/= '"') after
    _ -> ""

-- | Convert plexus-protocol TransportError to TransportCategory
transportErrorToCategory :: PT.TransportError -> TransportErrorCategory
transportErrorToCategory (PT.ConnectionRefused _ _) = ConnectionRefused
transportErrorToCategory (PT.ConnectionTimeout _ _) = ConnectionTimeout
transportErrorToCategory (PT.ProtocolError _) = ProtocolError
transportErrorToCategory (PT.NetworkError _) = UnknownTransportError

-- | Fetch schema at a specific path
fetchSchemaAt :: Path -> SynapseM PluginSchema
fetchSchemaAt path = do
  logger <- getLogger
  cfg <- getConfig
  Log.logDebug logger Log.SubsystemSchema $
    "Fetching schema at path: " <> T.pack (show path)
  result <- liftIO $ ST.fetchSchemaAt cfg path
  case result of
    Left err -> do
      Log.logInfo logger Log.SubsystemSchema $
        "Schema fetch failed: " <> T.pack (show path) <> " - " <> transportErrorToText err
      throwNav $ FetchError (transportErrorToText err) path
    Right schema -> do
      Log.logDebug logger Log.SubsystemSchema $
        "Schema fetch succeeded: " <> T.pack (show path) <> " (" <> psNamespace schema <> ")"
      pure schema

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
  logger <- getLogger
  cfg <- getConfig
  backend <- asks seBackend
  let fullPath = namespacePath ++ [method]
  Log.logInfo logger Log.SubsystemRPC $
    "Invoking RPC method: " <> T.intercalate "." fullPath <> " on " <> backend
  Log.logDebug logger Log.SubsystemRPC $
    "RPC request params: " <> T.pack (show params)
  result <- liftIO $ ST.invokeMethod cfg namespacePath method params
  case result of
    Left transportErr -> do
      -- Build context from typed transport error and environment
      let path = namespacePath ++ [method]
      let ctx = TransportContext
            { tcMessage  = transportErrorToText transportErr
            , tcHost     = getHost transportErr cfg
            , tcPort     = getPort transportErr cfg
            , tcBackend  = backend
            , tcPath     = path
            , tcCategory = transportErrorToCategory transportErr
            }
      Log.logInfo logger Log.SubsystemRPC $
        "RPC invocation failed: " <> T.intercalate "." fullPath <> " - " <> transportErrorToText transportErr
      throwTransportWith ctx
    Right items -> do
      Log.logDebug logger Log.SubsystemRPC $
        "RPC invocation succeeded: " <> T.intercalate "." fullPath <> " (" <> T.pack (show (length items)) <> " items)"
      pure items
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
-- the response is sent back via {backend}.respond (if the handler returns Just).
-- If the handler returns Nothing (BidirRespond mode), no response is sent immediately.
invokeStreamingWithBidir
  :: Path
  -> Text
  -> Value
  -> (HubStreamItem -> IO ())                              -- ^ Handler for non-request items
  -> (Text -> PT.Request Value -> IO (Maybe (PT.Response Value)))  -- ^ Handler for bidirectional requests
  -> SynapseM ()
invokeStreamingWithBidir = invokeStreamingWithBidirCancellable (\_ -> pure ())

-- | PLX-123 / M3·D — 'invokeStreamingWithBidir' plus the cancel handle.
--
-- The extra argument is called once, as soon as the substrate confirms the
-- subscription and before the first item is pulled, with an action that
-- cancels __this__ invocation. See 'Plexus.Client.substrateRpcCancellable' for
-- the channel decision (RFC 002 §10.2 leaves it to the transport) and for the
-- precise limits of what it asserts.
invokeStreamingWithBidirCancellable
  :: (CancelTurn -> IO ())
  -> Path
  -> Text
  -> Value
  -> (HubStreamItem -> IO ())
  -> (Text -> PT.Request Value -> IO (Maybe (PT.Response Value)))
  -> SynapseM ()
invokeStreamingWithBidirCancellable onCancel namespacePath method params onItem onBidirRequest = do
  cfg <- getConfig
  let wrappedHandler item = case item of
        PT.StreamRequest _ _ reqId reqData _timeout -> do
          -- Handle the bidirectional request
          mResponse <- onBidirRequest reqId reqData
          -- Send the response back via {backend}.respond (if present)
          case mResponse of
            Just response -> do
              _ <- ST.sendBidirectionalResponse cfg reqId response
              pure ()
            Nothing -> pure ()  -- agent will respond via separate call
        _ -> onItem item
  result <- liftIO $ ST.invokeMethodStreamingCancellable onCancel cfg namespacePath method params wrappedHandler
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
sendResponse :: Text -> PT.Response Value -> SynapseM ()
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
  host    <- asks seHost
  port    <- asks sePort
  backend <- asks seBackend
  mToken  <- asks seToken
  cks     <- asks seCookies
  hdrs    <- asks seHeaders
  pure $ SubstrateConfig
    { substrateHost    = T.unpack host
    , substratePort    = port
    , substratePath    = "/"
    , substrateBackend = backend
    -- SAFE-S04: merge token Cookie + arbitrary --cookie/--header upgrade context.
    , substrateHeaders = mergeUpgradeHeaders mToken cks hdrs
    }

-- | Build the WS upgrade Headers list from optional token + extra cookies + extra headers.
-- Cookies are concatenated into a single Cookie header as @key=value; key=value@.
-- Token (when present) becomes a @access_token=...@ cookie entry merged with the rest.
mergeUpgradeHeaders :: Maybe Text -> [(Text, Text)] -> [(Text, Text)] -> WS.Headers
mergeUpgradeHeaders mToken extraCookies extraHeaders =
  let allCookies = case mToken of
        Just tok -> ("access_token", tok) : extraCookies
        Nothing  -> extraCookies
      cookieHdr = if null allCookies
        then []
        else
          let kv (k, v) = T.unpack k <> "=" <> T.unpack v
              joined = BS8.pack $ List.intercalate "; " (map kv allCookies)
          in [(CI.mk "Cookie", joined)]
      otherHdrs = [ (CI.mk (BS8.pack (T.unpack k)), BS8.pack (T.unpack v))
                  | (k, v) <- extraHeaders
                  ]
  in cookieHdr ++ otherHdrs
