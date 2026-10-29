{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | AUTH-8: a Plexus hub in miniature, speaking the wire synapse speaks
-- (JSON-RPC over a WebSocket; @_info@, @<backend>.call@, subscription
-- notifications ending in @done@), answering AUTH-2..5's shapes as the
-- tickets write them. It listens on 127.0.0.1, a port the OS picks.
--
-- A 'Scenario' says which hub it is: today's (no view, no auth activation),
-- an anonymous caller's, or a signed-in caller's. The person flow's
-- browser is played by the hub itself: answering @auth.login {flow:
-- {browser: {redirect}}}@ it also, a moment later, sends the GET the
-- provider's redirect would — to @redirect?code=…&state=…@.
module MockHub
  ( Scenario(..)
  , ViewMode(..)
  , todaysHub
  , withMockHub
  , serveMockHub
  , mockCode
  , mockState
  , catalogHub
  , schemaCalls
    -- * SYNAPSE-28: what a run cost the hub
  , connectionsAccepted
  , infoCalls
  , unsubscribes
  , renamedGet
    -- * SYNAPSE-CC-8: a service serving schemas a test gives it
  , schemaOverride
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar, tryTakeMVar)
import System.Timeout (timeout)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever, void)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
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

data ViewMode = NoView | AnonymousView | CallerView
  deriving (Eq, Show)

data Scenario = Scenario
  { scView          :: ViewMode
  , scAuth          :: Bool   -- ^ has the auth activation (auth.login, auth.whoami, …)
  , scSessionLogin  :: Bool   -- ^ has IDENTITY-42's session.login (today's menger)
  , scBrowserState  :: Text   -- ^ the state the browser comes back with (mockState: the right one)
  , scCatalog       :: Bool   -- ^ SYNAPSE-17: serve menger's tickets and deployments as they were (test/fixtures)
  } deriving (Eq, Show)

todaysHub :: Scenario
todaysHub = Scenario NoView False True mockState False

-- | SYNAPSE-17: a hub with a catalog to search — menger's tickets (17 of
-- its methods) and deployments (3), as its schema had them on 2026-10-15;
-- in a caller's view, tickets.delete (admin only) is not in it.
catalogHub :: ViewMode -> Scenario
catalogHub v = Scenario v False False mockState True

-- | How many schema calls the mock hubs have answered (this process).
{-# NOINLINE schemaCalls #-}
schemaCalls :: IORef Int
schemaCalls = unsafePerformIO (newIORef 0)

-- | SYNAPSE-28: WebSocket connections the mock hubs have accepted (this process).
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

-- | SYNAPSE-28: the hub renamed @tickets.get@ to @tickets.fetch@ (its schema
-- says @fetch@, and @get@ is not found).
{-# NOINLINE renamedGet #-}
renamedGet :: IORef Bool
renamedGet = unsafePerformIO (newIORef False)

-- | SYNAPSE-CC-8: when set, @<ns>.schema@ (@menger.schema@ for the root,
-- keyed @""@) answers these schemas, by dotted path, before anything else.
{-# NOINLINE schemaOverride #-}
schemaOverride :: IORef (Maybe [(Text, Value)])
schemaOverride = unsafePerformIO (newIORef Nothing)

mockCode, mockState :: Text
mockCode = "c_19ab"
mockState = "s_7f2c"

-- | Run an action against a hub on a fresh port; the port is passed in.
withMockHub :: Scenario -> (Int -> IO a) -> IO a
withMockHub sc act = bracket open (N.close . fst) (\(_, port) -> act port)
  where
    open = do
      sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
      N.setSocketOption sock N.ReuseAddr 1
      N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
      N.listen sock 16
      port <- fromIntegral <$> N.socketPort sock
      void $ forkIO $ acceptLoop sc sock
      pure (sock, port)

-- | Serve until killed, printing the port (for transcripts by hand).
serveMockHub :: Scenario -> IO ()
serveMockHub sc = withMockHub sc $ \port -> do
  putStrLn (show port)
  hFlush stdout
  forever (threadDelay 1000000)

acceptLoop :: Scenario -> N.Socket -> IO ()
acceptLoop sc sock = do
  r <- try (N.accept sock)
  case r of
    Left (_ :: SomeException) -> pure ()   -- the hub was closed
    Right (conn, _) -> do
      void $ forkIO $ do
        _ <- try (session conn) :: IO (Either SomeException ())
        N.close conn
      acceptLoop sc sock
  where
    session conn = do
      pending <- WS.makePendingConnection conn WS.defaultConnectionOptions
      -- the credential the upgrade carried, if any
      let bearer = lookup "Authorization" (WS.requestHeaders (WS.pendingRequest pending))
      c <- WS.acceptRequest pending
      atomicModifyIORef' connectionsAccepted (\n -> (n + 1, ()))
      next <- newIORef (100 :: Int)
      forever $ do
        msg <- WS.receiveData c
        case decode msg of
          Just (Object o) -> handle sc c next (isBearer bearer) o
          _ -> pure ()
    isBearer = maybe False (BS8.isPrefixOf "Bearer ")

handle :: Scenario -> WS.Connection -> IORef Int -> Bool -> Object -> IO ()
handle sc c next signedIn o = do
  let rid = KM.lookup "id" o
      method = str "method" o
      params = case KM.lookup "params" o of { Just (Object p) -> p; _ -> KM.empty }
  -- SYNAPSE-28: an unsubscribe is answered true, and noted
  if method == "menger.call_unsub"
    then do
      atomicModifyIORef' unsubscribes (\us -> (maybe Null id (KM.lookup "params" o) : us, ()))
      send (object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= True])
    else handleCall sc c next signedIn o rid method params
  where
    send :: Value -> IO ()
    send = WS.sendTextData c . encode

handleCall :: Scenario -> WS.Connection -> IORef Int -> Bool -> Object -> Maybe Value -> Text -> Object -> IO ()
handleCall sc c next signedIn _o rid method params = do
  sub <- atomicModifyIORef' next (\n -> (n + 1, n))
  send (object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= sub])
  let emit item = send (object
        [ "jsonrpc" .= ("2.0" :: Text), "method" .= ("subscription" :: Text)
        , "params" .= object ["subscription" .= sub, "result" .= item] ])
      meta = object ["provenance" .= (["menger"] :: [Text]), "plexus_hash" .= ("h0" :: Text), "timestamp" .= (0 :: Int)]
      dat :: Text -> Value -> IO ()
      dat ct v = emit (object ["type" .= ("data" :: Text), "metadata" .= meta, "content_type" .= ct, "content" .= v])
      err :: Text -> [(K.Key, Value)] -> IO ()
      err m extra = emit (Object (KM.fromList (
        [("type", "error"), ("metadata", meta), ("message", String m), ("recoverable", Bool False)] <> extra)))
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
      case inner of
        -- waits for its answer off the connection's loop, as a real hub
        -- runs calls side by side (SYNAPSE-28: the answer may come on this
        -- very connection)
        "tickets.ask" -> void $ forkIO (askBack emit meta dat err >> done)
        -- SYNAPSE-28: a stream that never ends by itself (until unsubscribed)
        "tickets.watch" -> dat "tickets.watch" (object ["type" .= ("watching" :: Text)])
        "tickets.get" | renamed -> err "Method not found: tickets.get" [] >> done
        "tickets.fetch" | renamed -> callMethod sc signedIn "tickets.get" args dat err >> done
        "tickets.schema" | renamed, scView sc /= AnonymousView, not (scCatalog sc) ->
          dat "tickets.schema" (renameGet (ticketsSchema sc)) >> done
        _ -> callMethod sc signedIn inner args dat err >> done
    -- SCAFFOLD-21: a bidirectional answer, on a connection of its own
    "menger.respond" -> do
      void (tryPutMVar responses (KM.lookup "response_data" params))
      done
    _ -> err ("Invalid method format: " <> method) [] >> done
  where
    send :: Value -> IO ()
    send = WS.sendTextData c . encode

-- | SCAFFOLD-21: the answers to 'askBack's requests (@menger.respond@'s
-- @response_data@), as they arrive on any connection.
{-# NOINLINE responses #-}
responses :: MVar (Maybe Value)
responses = unsafePerformIO newEmptyMVar

-- | A bidirectional method (@tickets.ask@): it asks the caller to confirm
-- (a @request@ item), waits up to 5 s for @menger.respond@, and answers
-- with what came back.
askBack :: (Value -> IO ()) -> Value -> (Text -> Value -> IO ()) -> (Text -> [(K.Key, Value)] -> IO ()) -> IO ()
askBack emit meta dat err = do
  _ <- tryTakeMVar responses
  emit (object
    [ "type" .= ("request" :: Text), "metadata" .= meta, "request_id" .= ("req-1" :: Text)
    , "request" .= object ["type" .= ("confirm" :: Text), "message" .= ("go on?" :: Text)] ])
  got <- timeout 5000000 (takeMVar responses)
  case got of
    Just (Just r) -> dat "tickets.ask" (object ["type" .= ("answered" :: Text), "response" .= r])
    _ -> err "no response to req-1" []

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
  "menger.schema" -> dat "menger.schema" (rootSchema sc)
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
  where m n d = object ["name" .= (n :: Text), "description" .= (d :: Text), "hash" .= (n <> "0"), "public" .= True]

ticketsSchema :: Scenario -> Value
ticketsSchema sc = object $
  [ "namespace" .= ("tickets" :: Text), "version" .= ("0.5.0" :: Text), "hash" .= ("t0" :: Text)
  , "description" .= ("Plans as data" :: Text)
  , "methods" .= map method
      [ ("get", "One ticket by id", ["MENGER"], Just "id")
      , ("list", "Tickets, filtered and paged", ["MENGER"], Nothing)
      , ("set_status", "Change a ticket's status", ["MENGER-12"], Just "id") ] ]
  <> [ "view" .= ("caller" :: Text) | caller ]
  where
    caller = scView sc == CallerView
    method (n, d, held, target) = object $
      [ "name" .= (n :: Text), "description" .= (d :: Text), "hash" .= (n <> "0")
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
