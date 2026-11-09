{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Transport layer for Synapse
--
-- Bridges the low-level Substrate transport to the SynapseM monad.
module Synapse.Transport
  ( -- * Schema Fetching
    fetchSchema
  , fetchSlotSchemaAt
  , fetchSchemaAt
  , fetchServiceIR
  , rootSchemaKnown
  , fetchMethodSchema

    -- * Method Invocation (collected)
  , invoke
  , invokeRaw

    -- * Method Invocation (streaming)
  , invokeStreaming
  , invokeStreamingCall
  , invokeWithInput

    -- * Upgrade headers (IDENTITY-16: discovery probes carry them too)
  , mergeUpgradeHeaders
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Data.IORef (newIORef, readIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import qualified Synapse.SchemaStore as Store
import Control.Monad.Reader (asks)
import Data.Aeson (Value(..), Result(..), fromJSON, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.CaseInsensitive as CI
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import Plexus.Client (SubstrateConfig(..), cookieHeader, hostHeader)
import qualified Plexus.Transport as ST
import qualified Plexus.Types as PT

import Synapse.Schema.Types
import Synapse.Monad
import Synapse.IR.Types (IR(..), currentIrVersion)
import Synapse.Endpoint (substrateConfig)
import qualified Synapse.Log as Log
import qualified Synapse.Reconnect as R
import qualified Synapse.Input as In
import Control.Exception (throwIO)
import Control.Monad (forM_)
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, stderr)

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
transportErrorToText (PT.ConnectionLost msg _) = "Connection lost: " <> msg

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
transportErrorToCategory (PT.ConnectionLost _ _) = UnknownTransportError

-- | Fetch schema at a specific path
--
-- SYNAPSE-28: each path once a run (navigation, the deprecation check and
-- the IR all ask for the method's activation); from the cache alone when
-- the run sends first ('FromCache' — what it lacks is 'StaleSchema'), and
-- kept in the cache when it is fetched.
fetchSchemaAt :: Path -> SynapseM PluginSchema
fetchSchemaAt path = do
  memo <- asks seFetched
  known <- liftIO $ Map.lookup path <$> readIORef memo
  case known of
    Just schema -> pure schema
    Nothing -> do
      src <- asks seSchemaSource
      schema <- case src of
        FromCache st -> do
          cached <- liftIO $ Store.readSchema st path
          maybe (throwError (CacheMiss ("no cached schema at " <> showPath path))) pure cached
        FromServer st -> do
          schema <- fetchSchemaFromServer path
          liftIO $ mapM_ (\s -> Store.writeSchema s path schema) st
          pure schema
      liftIO $ modifyIORef' memo (Map.insert path schema)
      pure schema

-- | SYNAPSE-41: the schema of a name in a slot (a dynamic child's key, a
--   peer's name) — @Nothing@ when the service says there is nothing there
--   (activation or method not found, a slot's @not_found@). Every other
--   failure propagates as 'fetchSchemaAt' raises it: an outage, a refusal,
--   a sign-in, an unreadable answer, and a cache miss (which sends the call
--   to the server, SYNAPSE-28). Memoized and cached as 'fetchSchemaAt'.
fetchSlotSchemaAt :: Path -> SynapseM (Maybe PluginSchema)
fetchSlotSchemaAt path = do
  memo <- asks seFetched
  known <- liftIO $ Map.lookup path <$> readIORef memo
  case known of
    Just schema -> pure (Just schema)
    Nothing -> do
      src <- asks seSchemaSource
      found <- case src of
        FromCache st -> do
          cached <- liftIO $ Store.readSchema st path
          maybe (throwError (CacheMiss ("no cached schema at " <> showPath path))) (pure . Just) cached
        FromServer st -> do
          logger <- getLogger
          cfg <- getConfig
          result <- liftIO $ R.retrying (whereOf cfg) $ ST.fetchSchemaAtIfAny cfg path
          case result of
            Left err -> do
              Log.logInfo logger Log.SubsystemSchema $
                "Slot schema fetch failed: " <> showPath path <> " - " <> transportErrorToText err
              throwNav $ FetchError (transportErrorToText err) path
            Right Nothing -> do
              Log.logDebug logger Log.SubsystemSchema $ "Nothing in the slot at " <> showPath path
              pure Nothing
            Right (Just schema) -> do
              liftIO $ mapM_ (\s -> Store.writeSchema s path schema) st
              pure (Just schema)
      forM_ found $ \schema -> liftIO $ modifyIORef' memo (Map.insert path schema)
      pure found

-- | RETURNS-40: the service's own IR of the walk from @path@ — one call,
--   @<backend>.ir@ with the path as its namespace (none: the whole
--   service). @Just (Right ir)@ when it wrote one of this synapse's IR
--   version; @Just (Left why)@ when it refused (a namespace it has not, a
--   schema of its own it could not read — a title collision is no refusal:
--   its walk refuses that activation into the IR, as this one's does);
--   @Nothing@ when it has no @ir@ (every
--   service on the published 0.7.0 line, whose answer is a call failure) or
--   answered anything else — the caller walks the raw schemas then.
fetchServiceIR :: Path -> SynapseM (Maybe (Either Text IR))
fetchServiceIR path = do
  logger <- getLogger
  cfg <- getConfig
  let params = if null path then object [] else object ["namespaces" .= [T.intercalate "." path]]
  -- asked once, never ridden out (R.retrying): a service that cannot be
  -- reached now is the walk's to wait for, which reads what it can from the
  -- run's memo and the cache and rides out the rest
  result <- liftIO $ ST.invokeMethod cfg [] "ir" params
  let answer = case result of
        Left _ -> Nothing
        Right items -> case [ d | PT.StreamData _ _ _ d <- items ] of
          (Object o : _)
            | Just ok <- KM.lookup "ok" o, Success ir <- fromJSON ok, irVersion ir == currentIrVersion -> Just (Right ir)
            | Just (Object e) <- KM.lookup "err" o -> Just (Left (case KM.lookup "message" e of
                Just (String m) -> m
                _ -> "the service wrote no IR"))
          _ -> Nothing
  Log.logDebug logger Log.SubsystemSchema $ "The service's ir at " <> showPath path <> ": "
    <> maybe "none (walking the schemas)" (either ("refused: " <>) (const "used")) answer
  pure answer

-- | RETURNS-40 review #5: the service's own schema, as cheaply as this run
--   has it — the run's memo, else the schema cache on disk, else fetched
--   once, never ridden out (a service that cannot be reached now is the
--   walk's to wait for), and memoized: a walk from the root reads it from
--   there. 'Nothing' when it cannot be had.
rootSchemaKnown :: SynapseM (Maybe PluginSchema)
rootSchemaKnown = do
  memo <- asks seFetched
  known <- liftIO $ Map.lookup [] <$> readIORef memo
  case known of
    Just s -> pure (Just s)
    Nothing -> do
      src <- asks seSchemaSource
      stored <- case src of
        FromServer (Just st) -> liftIO (Store.readSchema st [])
        FromCache st -> liftIO (Store.readSchema st [])
        FromServer Nothing -> pure Nothing
      case stored of
        Just s -> pure (Just s)
        Nothing -> do
          cfg <- getConfig
          fetched <- liftIO (ST.fetchSchemaAt cfg [])
          case fetched of
            Left _ -> pure Nothing
            Right s -> do
              liftIO $ modifyIORef' memo (Map.insert [] s)
              case src of
                FromServer (Just st) -> liftIO (Store.writeSchema st [] s)
                _ -> pure ()
              pure (Just s)

-- | A path as a message names it.
showPath :: Path -> Text
showPath [] = "the root"
showPath p = T.intercalate "." p

fetchSchemaFromServer :: Path -> SynapseM PluginSchema
fetchSchemaFromServer path = do
  logger <- getLogger
  cfg <- getConfig
  Log.logDebug logger Log.SubsystemSchema $
    "Fetching schema at path: " <> T.pack (show path)
  result <- liftIO $ R.retrying (whereOf cfg) $ ST.fetchSchemaAt cfg path
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
  result <- liftIO $ R.retrying (whereOf cfg) $ ST.fetchMethodSchemaAt cfg path methodName
  case result of
    Left err -> throwNav $ FetchError (transportErrorToText err) path
    Right schema -> pure schema

-- | Invoke a method and return stream items
invoke :: Path -> Text -> Value -> SynapseM [ServiceStreamItem]
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
invokeRaw :: Text -> Value -> SynapseM [ServiceStreamItem]
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
invokeStreaming :: Path -> Text -> Value -> (ServiceStreamItem -> IO ()) -> SynapseM ()
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

-- | Invoke a method, each item to the handler as it comes, through an
-- outage as its shape says (SYNAPSE-20).
invokeStreamingCall
  :: R.CallShape                                           -- ^ SYNAPSE-20: how it rides out an outage
  -> IO (Maybe WS.Headers)                                 -- ^ SYNAPSE-20: sign in again; the upgrade's new headers
  -> (R.Resumed -> IO ())                                  -- ^ told when a stream resumed after a gap
  -> Path
  -> Text
  -> Value
  -> (ServiceStreamItem -> IO ())                          -- ^ each item
  -> SynapseM ()
invokeStreamingCall shape refreshAuth onResume namespacePath method params onItem = do
  cfg <- getConfig
  cfgRef <- liftIO $ newIORef cfg
  let attempt ps subscribed h = do
        c <- readIORef cfgRef
        ST.invokeMethodStreamingWith c subscribed namespacePath method ps h
      reauth = refreshAuth >>= \case
        Just hs -> True <$ modifyIORef' cfgRef (\c -> c { substrateHeaders = hs })
        Nothing -> pure False
  -- SYNAPSE-20: through an outage — a read sent again, a stream resumed
  -- (with since when it takes one), a write that went out never repeated
  result <- liftIO $
    R.resilientCall (whereOf cfg) shape attempt reauth params onResume onItem
      >>= R.reportEnding (whereOf cfg)
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

-- | BIDIR-18: a method with an input half, its input fed from the source
-- (spec/10) as the callee's credit allows. Never sent twice: a connection
-- lost once the call went out is said on stderr with what was sent and
-- acknowledged, and the run exits 69 (a call that never left is tried
-- again in the reconnect window, as any). The printer is told whether the
-- input is still untouched (no credit came, so nothing was read): only
-- then may it hold an item back to run the call again. At the end, the
-- tally on stderr; a typed close exits by its kind ('In.closeExit'), a
-- line that is not an item 65.
invokeWithInput
  :: R.CallShape
  -> IO (Maybe WS.Headers)
  -> In.ItemKind -> Int -> Bool -> In.InputSource
  -> Path
  -> Text
  -> Value
  -> (IO Bool -> PT.PlexusStreamItem -> IO ())
  -> SynapseM ()
invokeWithInput shape0 refreshAuth kind chunk progress src namespacePath method params onItem = do
  cfg <- getConfig
  cfgRef <- liftIO $ newIORef cfg
  refusal <- liftIO $ newIORef Nothing
  let shape = shape0 { R.csRead = False, R.csSince = False }
      whereTo = whereOf cfg
      reauth = refreshAuth >>= \case
        Just hs -> True <$ modifyIORef' cfgRef (\c -> c { substrateHeaders = hs })
        Nothing -> pure False
      say t = TIO.hPutStrLn stderr ("synapse: " <> t) >> hFlush stderr
      tally t = TIO.hPutStrLn stderr (In.renderTally t) >> hFlush stderr
      run (half, untouched) = do
        let item i = do
              case In.inputRefusal i of
                Just r -> modifyIORef' refusal (maybe (Just r) Just)
                Nothing -> pure ()
              onItem untouched i
            attempt ps subscribed h = do
              c <- readIORef cfgRef
              ST.invokeMethodDuplex c half subscribed namespacePath method ps h
        R.resilientCall whereTo shape attempt reauth params (const (pure ())) item
  (r, fed, t) <- liftIO $ In.withInputFeed kind chunk progress src run
  ending <- liftIO $ either throwIO pure r
  refused <- liftIO $ readIORef refusal
  let lost why = liftIO $ do
        say $ "connection to " <> whereTo <> " lost during the call (" <> why
          <> ") — its input is not sent again (spec/10: no resume)"
        tally t
        exitWith (ExitFailure R.exitUnreachable)
  case (fed, ending) of
    (In.FedBadLine n why, _) -> liftIO $ do
      say $ "input line " <> T.pack (show n) <> " is not an item: " <> why
        <> " — no frame carried it; the call was ended"
      tally t
      exitWith (ExitFailure 65)
    (In.FedFailed why, _) -> liftIO $ do
      say $ "the input could not be read (" <> why <> ") — the call was ended"
      tally t
      exitWith (ExitFailure 74)
    (_, R.WriteUnknown why) -> lost why
    (_, R.Failed e@(PT.ConnectionLost why True))
      | R.isOutage e -> lost why
    (_, R.GaveUp window why)
      | In.tFrames t > 0 || In.tEnded t -> lost why
      | otherwise -> liftIO (R.reportEnding whereTo (R.GaveUp window why)) >> pure ()
    (_, R.OutputClosed) -> liftIO (R.reportEnding whereTo R.OutputClosed) >> pure ()
    (_, R.Failed e) -> do
      liftIO (tally t)
      backend <- asks seBackend
      throwTransportWith TransportContext
        { tcMessage  = transportErrorToText e
        , tcHost     = T.pack (substrateHost cfg)
        , tcPort     = substratePort cfg
        , tcBackend  = backend
        , tcPath     = namespacePath ++ [method]
        , tcCategory = transportErrorToCategory e
        }
    (_, R.Finished) -> liftIO $ do
      tally t
      forM_ (In.closeExit refused (In.tClosed t)) $ \(code, why) -> do
        say why
        exitWith (ExitFailure code)

-- | SYNAPSE-20: @host:port@, for what synapse says about an outage
-- (SCAFFOLD-21: the URL, for an address behind TLS or on a path).
whereOf :: SubstrateConfig -> Text
whereOf c = case (substrateTls c, substratePath c) of
  (Nothing, "/") -> T.pack (substrateHost c) <> ":" <> T.pack (show (substratePort c))
  (tls, path) -> maybe "ws" (const "wss") tls <> "://" <> T.pack (hostHeader c) <> T.pack path

-- | Get SubstrateConfig from environment
getConfig :: SynapseM SubstrateConfig
getConfig = do
  ep      <- asks seEndpoint
  backend <- asks seBackend
  mToken  <- asks seToken
  cks     <- asks seCookies
  hdrs    <- asks seHeaders
  -- SAFE-S04: merge token Cookie + arbitrary --cookie/--header upgrade context.
  -- SCAFFOLD-21: the endpoint's scheme (ws / wss) and path come along.
  pure $ substrateConfig ep backend (mergeUpgradeHeaders mToken cks hdrs)

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
