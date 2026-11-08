{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | AUTH-8: a Plexus service in miniature, speaking the wire synapse speaks
-- (JSON-RPC over a WebSocket; @_info@, @<backend>.call@, subscription
-- notifications ending in @done@), answering AUTH-2..5's shapes as the
-- tickets write them. It listens on 127.0.0.1, a port the OS picks.
--
-- A 'Scenario' says which service it is: today's (no view, no auth activation),
-- an anonymous caller's, or a signed-in caller's. The person flow's
-- browser is played by the service itself: answering @auth.login {flow:
-- {browser: {redirect}}}@ it also, a moment later, sends the GET the
-- provider's redirect would — to @redirect?code=…&state=…@.
module MockService
  ( Scenario(..)
  , ViewMode(..)
  , todaysService
  , withMockService
  , withEphemeralService
  , withMockServiceOn
  , serveMockService
  , mockCode
  , mockState
  , catalogService
  , schemaCalls
    -- * SYNAPSE-28: what a run cost the service
  , connectionsAccepted
  , infoCalls
  , unsubscribes
  , renamedGet
    -- * SYNAPSE-CC-8: a service serving schemas a test gives it
  , schemaOverride
    -- * SYNAPSE-40: a dial that takes its time, and what is left open
  , upgradeDelay
  , connectionsOpen
    -- * SYNAPSE-26: what a closed stdout cost the service
  , callsMade
  , floodsEnded
  , dropNext
    -- * RETURNS-36: a service on the published 0.7.0 line
  , served070
    -- * RETURNS-40: a service that writes its own IR
  , irAnswer
  , irAsked
    -- * ASK-3: methods that ask (ASK-1's needs_input)
  , askCalls
  , resetAsks
    -- * BIDIR-18: methods with an input half
  , InputPlan(..)
  , defaultInputPlan
  , inputPlan
  , inputSchemas
  , Received(..)
  , inputReceived
  , inputViolations
  , resetInput
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Data.Maybe (isJust)
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forever, void)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Bits (shiftL, (.|.))
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import qualified Network.WebSockets as WS
import qualified Data.Vector as V
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)

import Plexus07 (withContract)

data ViewMode = NoView | AnonymousView | CallerView
  deriving (Eq, Show)

data Scenario = Scenario
  { scView          :: ViewMode
  , scAuth          :: Bool   -- ^ has the auth activation (auth.login, auth.whoami, …)
  , scSessionLogin  :: Bool   -- ^ has IDENTITY-42's session.login (today's menger)
  , scBrowserState  :: Text   -- ^ the state the browser comes back with (mockState: the right one)
  , scCatalog       :: Bool   -- ^ SYNAPSE-17: serve menger's tickets and deployments as they were (test/fixtures)
  } deriving (Eq, Show)

todaysService :: Scenario
todaysService = Scenario NoView False True mockState False

-- | SYNAPSE-17: a service with a catalog to search — menger's tickets (17 of
-- its methods) and deployments (3), as its schema had them on 2026-10-15;
-- in a caller's view, tickets.delete (admin only) is not in it.
catalogService :: ViewMode -> Scenario
catalogService v = Scenario v False False mockState True

-- | How many schema calls the mock services have answered (this process).
{-# NOINLINE schemaCalls #-}
schemaCalls :: IORef Int
schemaCalls = unsafePerformIO (newIORef 0)

-- | SYNAPSE-28: WebSocket connections the mock services have accepted (this process).
{-# NOINLINE connectionsAccepted #-}
connectionsAccepted :: IORef Int
connectionsAccepted = unsafePerformIO (newIORef 0)

-- | SYNAPSE-28: _info probes answered.
{-# NOINLINE infoCalls #-}
infoCalls :: IORef Int
infoCalls = unsafePerformIO (newIORef 0)

-- | SYNAPSE-28: @menger.call_unsub@ requests, by subscription.
{-# NOINLINE unsubscribes #-}
unsubscribes :: IORef [Value]
unsubscribes = unsafePerformIO (newIORef [])

-- | SYNAPSE-28: the service renamed @tickets.get@ to @tickets.fetch@ (its schema
-- says @fetch@, and @get@ is not found).
{-# NOINLINE renamedGet #-}
renamedGet :: IORef Bool
renamedGet = unsafePerformIO (newIORef False)

-- | SYNAPSE-CC-8: when set, @<ns>.schema@ (@menger.schema@ for the root,
-- keyed @""@) answers these schemas, by dotted path, before anything else.
{-# NOINLINE schemaOverride #-}
schemaOverride :: IORef (Maybe [(Text, Value)])
schemaOverride = unsafePerformIO (newIORef Nothing)

-- | SYNAPSE-40: microseconds each service waits before it answers a
-- WebSocket upgrade (a dial under way that long).
{-# NOINLINE upgradeDelay #-}
upgradeDelay :: IORef Int
upgradeDelay = unsafePerformIO (newIORef 0)

-- | SYNAPSE-40: WebSocket connections open at the services now.
{-# NOINLINE connectionsOpen #-}
connectionsOpen :: IORef Int
connectionsOpen = unsafePerformIO (newIORef 0)

-- | SYNAPSE-26: the method of every @menger.call@ the services took, in order.
{-# NOINLINE callsMade #-}
callsMade :: IORef [Text]
callsMade = unsafePerformIO (newIORef [])

-- | SYNAPSE-26: @tickets.flood@ streams that ended — unsubscribed, or their
-- connection gone.
{-# NOINLINE floodsEnded #-}
floodsEnded :: IORef Int
floodsEnded = unsafePerformIO (newIORef 0)

-- | SYNAPSE-26: methods whose next call is taken and then answered by
-- closing the connection (each name once).
{-# NOINLINE dropNext #-}
dropNext :: IORef [Text]
dropNext = unsafePerformIO (newIORef [])

-- | RETURNS-36: when set, the services answer as one on the published 0.7.0
-- line does — every schema as written (a method's @streaming@, no
-- @cardinality@), every error frame spec/05's fields alone (@message@,
-- @code@, @error_kind@ and a refusal's fields, no typed @error@).
{-# NOINLINE served070 #-}
served070 :: IORef Bool
served070 = unsafePerformIO (newIORef False)

-- | RETURNS-40: when set, the service writes its own IR: @menger.ir@
-- answers this (@{"ok": IR}@ or @{"err": …}@) as plexus 0.8's does. Unset,
-- it has no @ir@ — every 0.7.0 service — and the call fails as an unknown
-- method does.
{-# NOINLINE irAnswer #-}
irAnswer :: IORef (Maybe Value)
irAnswer = unsafePerformIO (newIORef Nothing)

-- | RETURNS-40: the params of every @menger.ir@ the services took, in order.
{-# NOINLINE irAsked #-}
irAsked :: IORef [Value]
irAsked = unsafePerformIO (newIORef [])

-- | BIDIR-18: how the services answer a call's input half.
data InputPlan = InputPlan
  { planWindow    :: Int
    -- ^ the first credit, and the window: past half of it consumed, an
    -- @ack@ and a @credit@ for what was consumed
  , planStingy    :: Bool
    -- ^ grant the first window and nothing more, never ack
  , planRefuse    :: Maybe (Int, Text)
    -- ^ at this many frames, refuse with this @error_kind@ (naming the frame)
  , planClose     :: Maybe (Int, Text)
    -- ^ at this many frames, @input_closed {reason}@, then an error item
  , planDropAfter :: Maybe Int
    -- ^ at this many frames, close the socket
  } deriving (Eq, Show)

defaultInputPlan :: InputPlan
defaultInputPlan = InputPlan 64 False Nothing Nothing Nothing

{-# NOINLINE inputPlan #-}
inputPlan :: IORef InputPlan
inputPlan = unsafePerformIO (newIORef defaultInputPlan)

-- | One input frame as a service took it.
data Received
  = ReceivedJson Word64 [Value]                 -- ^ @menger.input {seq, items}@
  | ReceivedBinary Word64 Word64 BS.ByteString  -- ^ a binary frame: subscription, seq, payload
  | ReceivedEnd Word64                          -- ^ @menger.end {seq}@
  deriving (Eq, Show)

-- | BIDIR-18: every input frame the services took, in order.
{-# NOINLINE inputReceived #-}
inputReceived :: IORef [Received]
inputReceived = unsafePerformIO (newIORef [])

-- | BIDIR-18: what a client did that spec/10 forbids (past credit, out of
-- sequence, a malformed binary frame), each said.
{-# NOINLINE inputViolations #-}
inputViolations :: IORef [Text]
inputViolations = unsafePerformIO (newIORef [])

resetInput :: InputPlan -> IO ()
resetInput plan = do
  writeIORef inputPlan plan
  writeIORef inputReceived []
  writeIORef inputViolations []

-- | BIDIR-18: a root with a @stream@ activation beside @tickets@, whose
-- methods take a stream in — for 'schemaOverride'.
inputSchemas :: [(Text, Value)]
inputSchemas =
  [ ("", object
      [ "namespace" .= ("menger" :: Text), "version" .= ("1.0.0" :: Text), "description" .= ("Central routing" :: Text)
      , "hash" .= ("r-input" :: Text)
      , "children" .=
          [ object ["namespace" .= ("tickets" :: Text), "description" .= ("Plans as data" :: Text), "hash" .= ("t0" :: Text)]
          , object ["namespace" .= ("stream" :: Text), "description" .= ("Streams in" :: Text), "hash" .= ("s0" :: Text)] ] ])
  , ("stream", object
      [ "namespace" .= ("stream" :: Text), "version" .= ("0.1.0" :: Text), "hash" .= ("s0" :: Text)
      , "description" .= ("Streams in" :: Text)
      , "methods" .=
          [ m "lines" "A log, a line an item" (object ["type" .= ("string" :: Text)]) Nothing
          , m "ingest" "Items in, an object each" item Nothing
          , m "audio" "Raw bytes in" bytesSchema Nothing
          , m "tagged" "Lines in, beside a parameter named input" (object ["type" .= ("string" :: Text)])
              (Just (object ["type" .= ("object" :: Text), "required" .= (["input"] :: [Text]), "properties" .= object
                  ["input" .= object ["type" .= ("string" :: Text), "description" .= ("A tag" :: Text)]]]))
          ] ]) ]
  where
    m :: Text -> Text -> Value -> Maybe Value -> Value
    m n d inp ps = object $
      [ "name" .= (n :: Text), "description" .= (d :: Text), "hash" .= (n <> "0")
      , "returns" .= object [], "input" .= inp ] ++ [ "params" .= p | Just p <- [ps] ]
    item = object
      [ "type" .= ("object" :: Text), "title" .= ("Item" :: Text), "required" .= (["at", "pad"] :: [Text])
      , "properties" .= object ["at" .= object ["type" .= ("integer" :: Text)], "pad" .= object ["type" .= ("string" :: Text)]] ]
    bytesSchema = object
      [ "type" .= ("string" :: Text), "contentEncoding" .= ("base64" :: Text)
      , "contentMediaType" .= ("application/octet-stream" :: Text), "x-plexus-binary" .= True ]

-- | One call's input half at the service.
data InputCall = InputCall
  { icEmit     :: Value -> IO ()
  , icMeta     :: Value
  , icMethod   :: Text
  , icNext     :: IORef Word64   -- ^ the next frame expected
  , icItems    :: IORef Int      -- ^ items taken
  , icGranted  :: IORef Int      -- ^ items granted in all
  , icUngiven  :: IORef Int      -- ^ items consumed and not yet granted back
  , icTag      :: Maybe Text     -- ^ the call's own param named input, if it had one
  , icOver     :: IORef Bool     -- ^ the half is closed (refused, closed)
  }

mockCode, mockState :: Text
mockCode = "c_19ab"
mockState = "s_7f2c"

-- | Run an action against a service on a fresh port; the port is passed in.
withMockService :: Scenario -> (Int -> IO a) -> IO a
withMockService sc act = bracket open (N.close . fst) (\(_, port) -> act port)
  where
    open = do
      sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
      N.setSocketOption sock N.ReuseAddr 1
      N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
      N.listen sock 16
      port <- fromIntegral <$> N.socketPort sock
      void $ forkIO $ acceptLoop (const (pure ())) sc sock
      pure (sock, port)

-- | SYNAPSE-40: today's service on a port already chosen (one a stopped
-- service had: a server come back).
withMockServiceOn :: Int -> IO a -> IO a
withMockServiceOn port act = bracket open N.close (const act)
  where
    open = do
      sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
      N.setSocketOption sock N.ReuseAddr 1
      N.bind sock (N.SockAddrInet (fromIntegral port) (N.tupleToHostAddress (127, 0, 0, 1)))
      N.listen sock 16
      void $ forkIO $ acceptLoop (const (pure ())) todaysService sock
      pure sock

-- | SYNAPSE-40: a service that stops as a process does. When the action
-- ends, its listener closes and so does every connection it accepted (a
-- FIN, as the kernel sends for a process that exits), so a client still
-- holding one sees its server go.
withEphemeralService :: Scenario -> (Int -> IO a) -> IO a
withEphemeralService sc act = do
  accepted <- newIORef []
  bracket (open accepted) (stop accepted) (\(_, port) -> act port)
  where
    open accepted = do
      sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
      N.setSocketOption sock N.ReuseAddr 1
      N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
      N.listen sock 16
      port <- fromIntegral <$> N.socketPort sock
      void $ forkIO $ acceptLoop (\c -> atomicModifyIORef' accepted (\cs -> (c : cs, ()))) sc sock
      pure (sock, port)
    stop accepted (sock, _) = do
      N.close sock
      cs <- readIORef accepted
      mapM_ (\c -> void (try (N.shutdown c N.ShutdownBoth) :: IO (Either SomeException ()))) cs

-- | Serve until killed, printing the port (for transcripts by hand).
serveMockService :: Scenario -> IO ()
serveMockService sc = withMockService sc $ \port -> do
  putStrLn (show port)
  hFlush stdout
  forever (threadDelay 1000000)

acceptLoop :: (N.Socket -> IO ()) -> Scenario -> N.Socket -> IO ()
acceptLoop onAccept sc sock = do
  r <- try (N.accept sock)
  case r of
    Left (_ :: SomeException) -> pure ()   -- the service was closed
    Right (conn, _) -> do
      onAccept conn
      void $ forkIO $ do
        _ <- try (session conn) :: IO (Either SomeException ())
        N.close conn
      acceptLoop onAccept sc sock
  where
    session conn = do
      pending <- WS.makePendingConnection conn WS.defaultConnectionOptions
      -- the credential the upgrade carried, if any
      let bearer = lookup "Authorization" (WS.requestHeaders (WS.pendingRequest pending))
      readIORef upgradeDelay >>= \d -> if d > 0 then threadDelay d else pure ()
      c <- WS.acceptRequest pending
      atomicModifyIORef' connectionsAccepted (\n -> (n + 1, ()))
      atomicModifyIORef' connectionsOpen (\n -> (n + 1, ()))
      next <- newIORef (100 :: Int)
      inputs <- newIORef Map.empty
      flip finally (atomicModifyIORef' connectionsOpen (\n -> (n - 1, ()))) $ forever $ do
        dm <- WS.receiveDataMessage c
        case dm of
          WS.Binary bytes -> binaryInput inputs (LBS.toStrict bytes)
          WS.Text msg _ -> case decode msg of
            Just (Object o)
              | str "method" o `elem` ["menger.input", "menger.end"] -> jsonInput inputs o
              | otherwise -> handle sc c next inputs (isBearer bearer) o
            _ -> pure ()
    isBearer = maybe False (BS8.isPrefixOf "Bearer ")

handle :: Scenario -> WS.Connection -> IORef Int -> IORef (Map.Map Int InputCall) -> Bool -> Object -> IO ()
handle sc c next inputs signedIn o = do
  let rid = KM.lookup "id" o
      method = str "method" o
      params = case KM.lookup "params" o of { Just (Object p) -> p; _ -> KM.empty }
  -- SYNAPSE-28: an unsubscribe is answered true, and noted
  if method == "menger.call_unsub"
    then do
      atomicModifyIORef' unsubscribes (\us -> (maybe Null id (KM.lookup "params" o) : us, ()))
      send (object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= True])
    else handleCall sc c next inputs signedIn o rid method params
  where
    send :: Value -> IO ()
    send = WS.sendTextData c . encode

handleCall :: Scenario -> WS.Connection -> IORef Int -> IORef (Map.Map Int InputCall) -> Bool -> Object -> Maybe Value -> Text -> Object -> IO ()
handleCall sc c next inputs signedIn _o rid method params = do
  sub <- atomicModifyIORef' next (\n -> (n + 1, n))
  send (object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= sub])
  let emit item = send (object
        [ "jsonrpc" .= ("2.0" :: Text), "method" .= ("subscription" :: Text)
        , "params" .= object ["subscription" .= sub, "result" .= item] ])
      meta = object ["provenance" .= (["menger"] :: [Text]), "plexus_hash" .= ("h0" :: Text), "timestamp" .= (0 :: Int)]
      -- RETURNS-36: a schema as plexus 0.7 states it (its methods' cardinality and returns)
      dat :: Text -> Value -> IO ()
      dat ct v = do
        as070 <- readIORef served070
        emit (object ["type" .= ("data" :: Text), "metadata" .= meta, "content_type" .= ct
                     , "content" .= (if ".schema" `T.isSuffixOf` ct && not as070 then withContract v else v)])
      -- RETURNS-36: plexus 0.7's error frame — the typed RemoteError the
      -- message names, and AUTH-2's refusal fields beside it
      err :: Text -> [(K.Key, Value)] -> IO ()
      err m extra = do
        as070 <- readIORef served070
        -- as plexus-core writes it: message, code and spec/05's fields
        -- always; 0.8's typed error beside them (the 0.7.0 line has none)
        emit (Object (KM.fromList (
          [("type", "error"), ("metadata", meta), ("message", String m), ("code", String (code070 m extra)), ("recoverable", Bool False)]
          <> [ ("error", remoteError m extra) | not as070 ] <> extra)))
      done = emit (object ["type" .= ("done" :: Text), "metadata" .= meta])
  case method of
    "_info" -> do
      atomicModifyIORef' infoCalls (\n -> (n + 1, ()))
      dat "_info" (infoFor sc)
      done
    "menger.call" -> do
      let inner = str "method" params
          args = case KM.lookup "params" params of { Just (Object a) -> a; _ -> KM.empty }
      renamed <- readIORef renamedGet
      atomicModifyIORef' callsMade (\cs -> (cs ++ [inner], ()))
      dropIt <- atomicModifyIORef' dropNext (\ds -> if inner `elem` ds then (filter (/= inner) ds, True) else (ds, False))
      if dropIt then ioError (userError "dropped") else pure ()
      case inner of
        -- SYNAPSE-26: a write answered in two parts, a moment apart, so a
        -- reader that closes after the first has closed before the second
        "tickets.amend" -> void $ forkIO $ do
          dat "tickets.amend" (object ["type" .= ("amended" :: Text), "id" .= str "id" args, "part" .= (1 :: Int)])
          threadDelay 300000
          dat "tickets.amend" (object ["type" .= ("amended" :: Text), "id" .= str "id" args, "part" .= (2 :: Int)])
          done
        -- SYNAPSE-26: a stream that ticks every 100 ms until it is
        -- unsubscribed or its connection goes
        "tickets.flood" -> void $ forkIO $ do
          u0 <- length <$> readIORef unsubscribes
          let tick (n :: Int) = do
                u <- length <$> readIORef unsubscribes
                if u > u0 then pure () else do
                  ok <- try (dat "tickets.flood" (object ["type" .= ("tick" :: Text), "n" .= n]))
                  case ok of
                    Left (_ :: SomeException) -> pure ()
                    Right () -> threadDelay 100000 >> tick (n + 1)
          tick 0
          atomicModifyIORef' floodsEnded (\n -> (n + 1, ()))
        -- SYNAPSE-28: a stream that never ends by itself (until unsubscribed)
        "tickets.watch" -> dat "tickets.watch" (object ["type" .= ("watching" :: Text)])
        -- BIDIR-18: a method with an input half: credit at once, then the
        -- frames as they come (jsonInput, binaryInput)
        _ | inner `elem` ["stream.lines", "stream.ingest", "stream.audio", "stream.tagged"] -> do
            plan <- readIORef inputPlan
            ic <- InputCall emit meta inner <$> newIORef 0 <*> newIORef 0 <*> newIORef (planWindow plan)
                    <*> newIORef 0 <*> pure (case KM.lookup "input" args of { Just (String t) -> Just t; _ -> Nothing })
                    <*> newIORef False
            atomicModifyIORef' inputs (\m -> (Map.insert sub ic m, ()))
            emit (object ["type" .= ("credit" :: Text), "n" .= planWindow plan])
        "menger.ir" -> do
          atomicModifyIORef' irAsked (\as -> (as ++ [maybe (Object KM.empty) id (KM.lookup "params" params)], ()))
          answer <- readIORef irAnswer
          case answer of
            Just v -> dat "menger.ir" v >> done
            Nothing -> err "Method not found: menger.ir" [] >> done
        "tickets.get" | renamed -> err "Method not found: tickets.get" [] >> done
        "tickets.fetch" | renamed -> callMethod sc signedIn "tickets.get" args dat err >> done
        "tickets.schema" | renamed, scView sc /= AnonymousView, not (scCatalog sc) ->
          dat "tickets.schema" (renameGet (ticketsSchema sc)) >> done
        _ -> callMethod sc signedIn inner args dat err >> done
    _ -> err ("Invalid method format: " <> method) [] >> done
  where
    send :: Value -> IO ()
    send = WS.sendTextData c . encode

-- | BIDIR-18: a JSON input frame or end, for the call it names.
jsonInput :: IORef (Map.Map Int InputCall) -> Object -> IO ()
jsonInput inputs o = do
  let params = case KM.lookup "params" o of { Just (Object p) -> p; _ -> KM.empty }
      num k = case KM.lookup k params of { Just (Number n) -> Just (round n); _ -> Nothing }
  case (num "subscription", num "seq") of
    (Just sub, Just sq) -> do
      calls <- readIORef inputs
      case Map.lookup sub calls of
        Nothing -> violation ("a frame for no call: " <> T.pack (show sub))
        Just ic
          | str "method" o == "menger.end" -> do
              atomicModifyIORef' inputReceived (\rs -> (rs ++ [ReceivedEnd (fromIntegral sq)], ()))
              expected <- readIORef (icNext ic)
              if fromIntegral sq /= expected then violation ("end seq " <> T.pack (show sq) <> ", frames " <> T.pack (show expected)) else pure ()
              finish ic
          | otherwise -> do
              let items = case KM.lookup "items" params of { Just (Array a) -> V.toList a; _ -> [] }
              atomicModifyIORef' inputReceived (\rs -> (rs ++ [ReceivedJson (fromIntegral sq) items], ()))
              taken ic (fromIntegral sq) (length items)
    _ -> violation "an input frame without subscription or seq"

-- | BIDIR-18: a binary input frame: @[0x01][subscription u64][seq u64][payload]@.
binaryInput :: IORef (Map.Map Int InputCall) -> BS.ByteString -> IO ()
binaryInput inputs bytes
  | BS.length bytes < 17 || BS.head bytes /= 0x01 = violation "a malformed binary frame"
  | otherwise = do
      let word b = foldl (\acc x -> (acc `shiftL` 8) .|. fromIntegral x) (0 :: Word64) (BS.unpack b)
          sub = word (BS.take 8 (BS.drop 1 bytes))
          sq = word (BS.take 8 (BS.drop 9 bytes))
          payload = BS.drop 17 bytes
      atomicModifyIORef' inputReceived (\rs -> (rs ++ [ReceivedBinary sub sq payload], ()))
      calls <- readIORef inputs
      case Map.lookup (fromIntegral sub) calls of
        Nothing -> violation ("a binary frame for no call: " <> T.pack (show sub))
        Just ic -> taken ic sq 1

violation :: Text -> IO ()
violation v = atomicModifyIORef' inputViolations (\vs -> (vs ++ [v], ()))

-- | Frame @sq@ of @n@ items taken: checked against the sequence and the
-- credit, then acked and credited as the plan says.
taken :: InputCall -> Word64 -> Int -> IO ()
taken ic sq n = do
  over <- readIORef (icOver ic)
  if over then pure () else do
    plan <- readIORef inputPlan
    expected <- readIORef (icNext ic)
    if sq /= expected then violation ("out of sequence: " <> T.pack (show sq) <> ", expected " <> T.pack (show expected)) else pure ()
    writeIORef (icNext ic) (sq + 1)
    total <- atomicModifyIORef' (icItems ic) (\k -> (k + n, k + n))
    granted <- readIORef (icGranted ic)
    if total > granted then violation ("past credit: " <> T.pack (show total) <> " of " <> T.pack (show granted)) else pure ()
    let frames = fromIntegral (sq + 1)
        errorItem kind = icEmit ic (object
          [ "type" .= ("error" :: Text), "metadata" .= icMeta ic, "message" .= ("input refused: " <> kind)
          , "code" .= ("-32602" :: Text), "error_kind" .= kind, "recoverable" .= False, "seq" .= sq ])
    case (planDropAfter plan, planRefuse plan, planClose plan) of
      (Just k, _, _) | frames >= k -> ioError (userError "dropped mid-input")
      (_, Just (k, kind), _) | frames >= k -> do
        writeIORef (icOver ic) True
        icEmit ic (object ["type" .= ("input_closed" :: Text), "reason" .= kind])
        errorItem kind
      (_, _, Just (k, reason)) | frames >= k -> do
        writeIORef (icOver ic) True
        icEmit ic (object ["type" .= ("input_closed" :: Text), "reason" .= reason, "ack" .= sq])
        icEmit ic (object
          [ "type" .= ("error" :: Text), "metadata" .= icMeta ic, "message" .= ("the call was asked again: " <> reason)
          , "code" .= ("-32001" :: Text), "error_kind" .= ("unauthenticated" :: Text), "recoverable" .= False ])
      _ | planStingy plan -> pure ()
        | otherwise -> do
            ungiven <- atomicModifyIORef' (icUngiven ic) (\u -> (u + n, u + n))
            if ungiven * 2 >= planWindow plan
              then do
                writeIORef (icUngiven ic) 0
                modifyIORef' (icGranted ic) (+ ungiven)
                icEmit ic (object ["type" .= ("ack" :: Text), "seq" .= sq])
                icEmit ic (object ["type" .= ("credit" :: Text), "n" .= ungiven])
              else pure ()

-- | The input ended: the last ack, what was taken, done.
finish :: InputCall -> IO ()
finish ic = do
  n <- readIORef (icItems ic)
  next <- readIORef (icNext ic)
  if next > 0 then icEmit ic (object ["type" .= ("ack" :: Text), "seq" .= (next - 1)]) else pure ()
  icEmit ic (object
    [ "type" .= ("data" :: Text), "metadata" .= icMeta ic, "content_type" .= icMethod ic
    , "content" .= object (["type" .= ("taken" :: Text), "items" .= n, "frames" .= next] ++ [ "tag" .= t | Just t <- [icTag ic] ]) ])
  icEmit ic (object ["type" .= ("done" :: Text), "metadata" .= icMeta ic])

-- | RETURNS-36: the plexus 0.7 'RemoteError' an error the mock answers is:
-- a refusal (AUTH-2's fields) a missing grant, a "Method not found: a.m" or
-- "Activation not found: a" one of those, anything else an execution failure.
remoteError :: Text -> [(K.Key, Value)] -> Value
remoteError m extra
  | lookup "error_kind" extra == Just (String "forbidden") = object
      [ "kind" .= ("forbidden" :: Text), "reason" .= ("missing_grant" :: Text)
      , "grant" .= field "grant", "scope" .= field "scope"
      , "roles" .= maybe (toJSON ([] :: [Text])) id (lookup "roles" extra)
      , "held_on" .= maybe (toJSON ([] :: [Text])) id (lookup "held_on" extra) ]
  | Just rest <- T.stripPrefix "Method not found: " m
  , (a, dm) <- T.breakOnEnd "." rest, not (T.null a) = object
      [ "kind" .= ("method_not_found" :: Text), "activation" .= T.dropEnd 1 a, "method" .= dm ]
  | Just a <- T.stripPrefix "Activation not found: " m = object
      [ "kind" .= ("activation_not_found" :: Text), "activation" .= a ]
  | otherwise = object [ "kind" .= ("execution" :: Text), "message" .= m ]
  where
    field k = maybe (String "") id (lookup k extra)

-- | The JSON-RPC code a service puts on an error frame (spec/05).
code070 :: Text -> [(K.Key, Value)] -> Text
code070 m extra
  | lookup "error_kind" extra == Just (String "forbidden") = "-32003"
  | "not found" `T.isInfixOf` m = "-32601"
  | otherwise = "-32000"

-- | ASK-3: every call of a method that asks, its params as they came
-- (this process), oldest first.
{-# NOINLINE askCalls #-}
askCalls :: IORef [(Text, Object)]
askCalls = unsafePerformIO (newIORef [])

-- | ASK-3: the last token each method that asks proposed, and how many it has.
{-# NOINLINE askTokens #-}
askTokens :: IORef (Map.Map Text (Text, Int))
askTokens = unsafePerformIO (newIORef Map.empty)

resetAsks :: IO ()
resetAsks = writeIORef askCalls [] >> writeIORef askTokens Map.empty

-- | ASK-1's methods that ask, as plexus-core answers them: a call with no
-- valid confirm changes nothing and yields needs_input; the same call with
-- the last token proposed does it. @tickets.archive@ is a confirmation (no
-- fields) — its filter @moving@ moves the plan once (the first answer gets
-- a fresh proposal) and @none@ matches nothing (answered directly);
-- @tickets.purge@ needs @children@ (options, a default); @tickets.close@
-- needs @reason@ (no default).
askMethod :: Text -> Object -> (Text -> Value -> IO ()) -> IO ()
askMethod inner args dat = do
  atomicModifyIORef' askCalls (\cs -> (cs ++ [(inner, args)], ()))
  tokens <- readIORef askTokens
  let given = case KM.lookup "confirm" args of { Just (String t) -> Just t; _ -> Nothing }
      current = Map.lookup inner tokens
      valid = isJust given && fmap fst current == given
      issued = maybe 0 snd current
      moves = inner == "tickets.archive" && str "filter" args == "moving" && issued < 2
      propose summary effects fields = do
        let n = issued + 1
            token = "v1.1794100000." <> T.drop (T.length "tickets.") inner <> "-" <> T.pack (show n)
        atomicModifyIORef' askTokens (\m -> (Map.insert inner (token, n) m, ()))
        dat inner (object
          [ "type" .= ("needs_input" :: Text), "summary" .= (summary :: Text), "effects" .= (effects :: [Value])
          , "fields" .= (fields :: [Value]), "token" .= token, "expires" .= ("2026-11-07T12:00:00Z" :: Text) ])
  case inner of
    "tickets.archive"
      | str "filter" args == "none" -> dat inner (object ["type" .= ("archived" :: Text), "count" .= (0 :: Int)])
      | valid && not moves -> mapM_ (\i -> dat inner (object ["type" .= ("archived" :: Text), "id" .= (i :: Text)])) ids
      | otherwise -> propose ("archive " <> T.intercalate ", " ids <> " (" <> T.pack (show (length ids)) <> " not done)")
          (map String ids) []
      where ids = ["MENGER-4", "MENGER-5", "MENGER-6"] ++ [ "MENGER-7" | issued >= 1, str "filter" args == "moving" ]
    "tickets.purge"
      | valid -> dat inner (object ["type" .= ("purged" :: Text), "ids" .= KM.lookup "ids" args, "children" .= KM.lookup "children" args])
      | otherwise -> propose "KILN-2 has 4 children" [String "KILN-3", String "KILN-4", String "KILN-5", String "KILN-6"]
          [object ["name" .= ("children" :: Text), "options" .= (["reparent", "delete"] :: [Text]), "default" .= ("reparent" :: Text), "help" .= ("what moves" :: Text)]]
    _ -- tickets.close
      | valid -> dat inner (object ["type" .= ("closed" :: Text), "id" .= str "id" args, "reason" .= KM.lookup "reason" args])
      | otherwise -> propose ("close " <> str "id" args) []
          [object ["name" .= ("reason" :: Text), "help" .= ("why it closes" :: Text)]]

infoFor :: Scenario -> Value
infoFor sc = case scView sc of
  NoView -> object ["backend" .= ("menger" :: Text), "auth_capabilities" .= object ["mechanisms" .= [object ["kind" .= ("anonymous" :: Text)]]]]
  AnonymousView -> object
    [ "backend" .= ("menger" :: Text), "view" .= ("anonymous" :: Text)
    , "auth" .= object
        [ "methods" .= (["auth.providers", "auth.login", "auth.signup"] :: [Text])
        , "resource_metadata" .= ("http://127.0.0.1:4456/.well-known/oauth-protected-resource" :: Text) ] ]
  CallerView -> object ["backend" .= ("menger" :: Text), "view" .= ("caller" :: Text), "view_hash" .= ("v-9a77" :: Text)]

callMethod :: Scenario -> Bool -> Text -> Object
           -> (Text -> Value -> IO ()) -> (Text -> [(K.Key, Value)] -> IO ()) -> IO ()
callMethod sc signedIn inner args dat err = do
 if ".schema" `T.isSuffixOf` inner then atomicModifyIORef' schemaCalls (\n -> (n + 1, ())) else pure ()
 override <- readIORef schemaOverride
 let ns = if inner == "menger.schema" then "" else T.dropEnd (T.length ".schema") inner
 case override >>= lookup ns of
  Just v | ".schema" `T.isSuffixOf` inner -> dat inner v
  _ -> callMethod' sc signedIn inner args dat err

callMethod' :: Scenario -> Bool -> Text -> Object
            -> (Text -> Value -> IO ()) -> (Text -> [(K.Key, Value)] -> IO ()) -> IO ()
callMethod' sc _signedIn inner args dat err =
 case inner of
  "menger.schema" -> do
    -- RETURNS-40: a service that writes its IR lists `ir` among its own methods
    answer <- readIORef irAnswer
    dat "menger.schema" (maybe id (const withIr) answer (rootSchema sc))
  "tickets.schema" | scCatalog sc -> do
      Just (Object o) <- decodeFileStrict "test/fixtures/find-tickets.json"
      dat "tickets.schema" (Object (catalogView sc o))
  "deployments.schema" | scCatalog sc -> do
      Just v <- decodeFileStrict "test/fixtures/find-deployments.json"
      dat "deployments.schema" (v :: Value)
  "auth.schema" | scAuth sc -> dat "auth.schema" authSchema
  "tickets.schema" | scView sc /= AnonymousView -> dat "tickets.schema" (ticketsSchema sc)
  "auth.whoami" | scAuth sc -> dat "auth.whoami" (object
      [ "type" .= ("whoami" :: Text), "principal" .= ("key:ac79e0f3b1c24d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6" :: Text)
      , "name" .= ("worker-1" :: Text), "agent" .= ("worker-1" :: Text)
      , "memberships" .= [object ["role" .= ("editor" :: Text), "scope" .= ("MENGER" :: Text)]]
      , "ceiling" .= (["editor@MENGER-12"] :: [Text]) ])
  "tickets.get" -> dat "tickets.get" (object ["type" .= ("ticket" :: Text), "id" .= ("MENGER-12" :: Text)])
  _ | inner `elem` ["tickets.archive", "tickets.purge", "tickets.close"] -> askMethod inner args dat
  "tickets.set_status"
    | scView sc == CallerView -> err "needs tickets.set_status on IDENTITY"
        [ ("error_kind", "forbidden"), ("grant", "tickets.set_status"), ("scope", "IDENTITY")
        , ("roles", toJSON (["editor", "admin"] :: [Text])), ("held_on", toJSON (["MENGER"] :: [Text])) ]
    | otherwise -> dat "tickets.set_status" (object ["type" .= ("status_changed" :: Text)])
  "auth.login" | scAuth sc -> case KM.lookup "flow" args of
      Just (Object f)
        | Just (Object b) <- KM.lookup "browser" f -> do
            let redirect = str "redirect" b
            dat "auth.login" (object
              [ "type" .= ("login_url" :: Text)
              , "url" .= ("http://127.0.0.1:4456/login?cli=1&state=" <> mockState)
              , "state" .= mockState ])
            -- the provider's redirect, a moment later
            void $ forkIO $ threadDelay 300000 >> browserReturns redirect (scBrowserState sc)
        | Just (Object cd) <- KM.lookup "code" f ->
            if str "code" cd == mockCode && str "state" cd == mockState
              then dat "auth.login" (object
                [ "type" .= ("session" :: Text), "token" .= ("mgs_person" :: Text)
                , "principal" .= ("oidc:workos/user_01M4QXZ7ABCDEF" :: Text), "name" .= ("Ada Lovelace" :: Text)
                , "expires_at" .= (1791663600 :: Int)
                , "memberships" .= [object ["role" .= ("admin" :: Text), "scope" .= ("*" :: Text)]] ])
              else err "the code is not good for this state" []
        | Just (Object s) <- KM.lookup "session" f -> dat "auth.login" (object
            [ "type" .= ("session" :: Text), "token" .= ("mgs_agent_auth" :: Text)
            , "principal" .= ("key:79b5" :: Text), "agent" .= ("worker-1" :: Text)
            , "expires_at" .= (1791663600 :: Int)
            , "ceiling" .= maybe (toJSON ([] :: [Text])) id (KM.lookup "ceiling" s) ])
      _ -> err "auth.login: no flow" []
  "session.login" | scSessionLogin sc -> dat "session.login" (object
      [ "type" .= ("login" :: Text), "token" .= ("mgs_agent_session" :: Text)
      , "principal" .= ("key:79b5" :: Text), "agent" .= ("worker-1" :: Text)
      , "expires_at" .= (1791663600 :: Int)
      , "scope" .= maybe (toJSON ([] :: [Text])) id (KM.lookup "scope" args) ])
  other -> err ("Activation not found: " <> T.takeWhile (/= '.') other) []

-- | The browser coming back to synapse's loopback, as the provider's
-- redirect sends it.
browserReturns :: Text -> Text -> IO ()
browserReturns redirect state = do
  let rest = maybe redirect id (T.stripPrefix "http://127.0.0.1:" redirect)
      (portT, path) = T.breakOn "/" rest
      port = read (T.unpack portT) :: Int
  sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
  N.connect sock (N.SockAddrInet (fromIntegral port) (N.tupleToHostAddress (127, 0, 0, 1)))
  -- a favicon first, as browsers do: the loopback must not take it for an answer
  NB.sendAll sock "GET /favicon.ico HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
  _ <- NB.recv sock 4096
  N.close sock
  sock' <- N.socket N.AF_INET N.Stream N.defaultProtocol
  N.connect sock' (N.SockAddrInet (fromIntegral port) (N.tupleToHostAddress (127, 0, 0, 1)))
  NB.sendAll sock' $ TE.encodeUtf8 $
    "GET " <> path <> "?code=" <> mockCode <> "&state=" <> state <> " HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
  _ <- NB.recv sock' 4096
  N.close sock'

-- | RETURNS-40: a root schema with the `ir` method beside its own.
withIr :: Value -> Value
withIr (Object o) = Object (KM.insert "methods" (Array (V.snoc ms irMethod)) o)
  where
    ms = case KM.lookup "methods" o of { Just (Array a) -> a; _ -> mempty }
    irMethod = object
      [ "name" .= ("ir" :: Text), "description" .= ("The service's IR" :: Text), "hash" .= ("ir0" :: Text)
      , "cardinality" .= ("one" :: Text), "returns" .= object [] ]
withIr v = v

rootSchema :: Scenario -> Value
rootSchema sc = object $
  [ "namespace" .= ("menger" :: Text), "version" .= ("1.0.0" :: Text)
  , "description" .= ("Central routing and introspection" :: Text)
  , "hash" .= (case scView sc of { AnonymousView -> "v-anon-3c1f"; CallerView | scCatalog sc -> "rc-v9a77"; _ | scCatalog sc -> "rc0"; _ -> "r0" } :: Text)
  , "children" .= children ]
  where
    children = case scView sc of
      _ | scCatalog sc ->
        [ object ["namespace" .= ("tickets" :: Text), "description" .= ("Plans as data" :: Text), "hash" .= ("t0" :: Text)]
        , object ["namespace" .= ("deployments" :: Text), "description" .= ("Deployments" :: Text), "hash" .= ("d0" :: Text)] ]
      AnonymousView -> [object ["namespace" .= ("auth" :: Text), "methods" .= [
        object ["name" .= ("providers" :: Text), "public" .= True],
        object ["name" .= ("login" :: Text), "public" .= True],
        object ["name" .= ("signup" :: Text), "public" .= True]]]]
      _ -> [ object ["namespace" .= ("tickets" :: Text), "description" .= ("Plans as data" :: Text), "hash" .= ("t0" :: Text)] ]
        <> [ object ["namespace" .= ("auth" :: Text), "description" .= ("Sign in, who am I, what does this need" :: Text), "hash" .= ("a0" :: Text)] | scAuth sc ]

authSchema :: Value
authSchema = object
  [ "namespace" .= ("auth" :: Text), "version" .= ("0.1.0" :: Text), "hash" .= ("a0" :: Text)
  , "description" .= ("Sign in, who am I, what does this need" :: Text)
  , "methods" .= [ m "providers" "The ways in this server offers"
                 , m "login" "Sign in \x2014 a person through the browser, an agent with its key"
                 , m "signup" "Create an account, or ask for access" ] ]
  where m n d = object ["name" .= (n :: Text), "description" .= (d :: Text), "hash" .= (n <> "0"), "public" .= True, "returns" .= object []]

ticketsSchema :: Scenario -> Value
ticketsSchema sc = object $
  [ "namespace" .= ("tickets" :: Text), "version" .= ("0.5.0" :: Text), "hash" .= ("t0" :: Text)
  , "description" .= ("Plans as data" :: Text)
  , "methods" .= (map method
      [ ("get", "One ticket by id", ["MENGER"], Just "id")
      , ("list", "Tickets, filtered and paged", ["MENGER"], Nothing)
      , ("set_status", "Change a ticket's status", ["MENGER-12"], Just "id") ]
      ++ askingMethods) ]
  <> [ "view" .= ("caller" :: Text) | caller ]
  where
    -- ASK-3: methods that ask (asks: true); archive and purge declare
    -- confirm as plexus-core 0.10 does, close leaves it to synapse
    askingMethods =
      [ asking "archive" "Archive the tickets a filter matches" "many"
          [("filter", object ["type" .= ("string" :: Text), "description" .= ("Which tickets" :: Text)])] ["filter"] True
      , asking "purge" "Delete tickets for good" "one"
          [ ("ids", object ["type" .= ("array" :: Text), "items" .= object ["type" .= ("string" :: Text)], "description" .= ("Tickets to delete" :: Text)])
          , ("children", object ["type" .= ("string" :: Text), "enum" .= (["reparent", "delete"] :: [Text]), "description" .= ("What moves" :: Text)]) ] ["ids"] True
      , asking "close" "Close a ticket" "one"
          [ ("id", object ["type" .= ("string" :: Text), "description" .= ("Ticket id" :: Text)])
          , ("reason", object ["type" .= ("string" :: Text), "description" .= ("Why" :: Text)]) ] ["id"] False ]
    asking n d card props req declaresConfirm = object
      [ "name" .= (n :: Text), "description" .= (d :: Text), "hash" .= (n <> "0"), "returns" .= object []
      , "cardinality" .= (card :: Text), "asks" .= True
      , "params" .= object
          [ "type" .= ("object" :: Text), "required" .= (req :: [Text])
          , "properties" .= object ([ (K.fromText k, v) | (k, v) <- props ]
              ++ [ "confirm" .= object ["type" .= ("string" :: Text), "description" .= ("ConfirmToken: the token of a proposal" :: Text)] | declaresConfirm ]) ] ]
    caller = scView sc == CallerView
    method (n, d, held, target) = object $
      [ "name" .= (n :: Text), "description" .= (d :: Text), "hash" .= (n <> "0"), "returns" .= object []
      , "params" .= object ["type" .= ("object" :: Text), "required" .= (["id"] :: [Text]),
          "properties" .= object ["id" .= object ["type" .= ("string" :: Text), "description" .= ("Ticket id, e.g. MNG-2" :: Text)]]] ]
      <> (if caller then [ "grant" .= ("tickets." <> n), "held_on" .= (held :: [Text]) ] <> maybe [] (\t -> ["target" .= (t :: Text)]) target else [])

-- | SYNAPSE-28: the schema after @get@ was renamed @fetch@ (a new hash).
renameGet :: Value -> Value
renameGet (Object o) = Object (KM.insert "hash" (String "t1") (maybe o (\v -> KM.insert "methods" (ms v) o) (KM.lookup "methods" o)))
  where
    ms (Array a) = Array (V.map m a)
    ms v = v
    m (Object mo) | KM.lookup "name" mo == Just (String "get") =
      Object (KM.insert "name" (String "fetch") (KM.insert "hash" (String "fetch0") mo))
    m v = v
renameGet v = v

-- | The tickets schema as the caller's view shows it: without what only an
-- admin holds (tickets.delete).
catalogView :: Scenario -> Object -> Object
catalogView sc o
  | scView sc /= CallerView = o
  | otherwise = KM.insert "view" (String "caller") (maybe o (\ms -> KM.insert "methods" (dropDelete ms) o) (KM.lookup "methods" o))
  where
    dropDelete (Array ms) = Array (V.filter (\case { Object m -> KM.lookup "name" m /= Just (String "delete"); _ -> True }) ms)
    dropDelete v = v

str :: Text -> Object -> Text
str k o = case KM.lookup (K.fromText k) o of
  Just (String s) -> s
  _ -> ""
