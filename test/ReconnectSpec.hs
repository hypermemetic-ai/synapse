{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SYNAPSE-20: a lost connection is noticed, and synapse rides out the
-- outage. Against a flaky service in miniature (each call answered as the test
-- says: answered, dropped mid-stream, dropped before any answer, or taken
-- and then silent), on ports of its own:
--
-- * the client fails a call whose connection closes or goes silent —
--   'PT.ConnectionLost' — instead of waiting forever (APP-41's hang);
-- * a read is sent again, a stream resumes (with @since@ when it takes
--   one), a write that went out is never repeated, one that never left is;
-- * the window, its spelling, and a method's shape from its schema;
-- * SYNAPSE-26: an item the printer could not print (stdout closed) ends
--   the call — never a retry — while the socket's own broken pipe is still
--   an outage.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (forever, void)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.Text (Text)
import qualified Network.Socket as N
import qualified Network.WebSockets as WS
import System.Environment (setEnv, unsetEnv)
import GHC.Clock (getMonotonicTime)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import System.IO (stdout)
import System.Timeout (timeout)
import Test.Hspec

import Plexus.Client (SubstrateConfig(..))
import Plexus.Schema.Recursive (MethodSchema(..), MethodRole(..))
import qualified Plexus.Transport as ST
import qualified Plexus.Types as PT
import Synapse.Reconnect
import qualified Synapse.Hold as Hold
import System.Directory (doesFileExist, getTemporaryDirectory)
import System.FilePath ((</>))
import System.Posix.Process (getProcessID)

-- ---------------------------------------------------------------------------
-- The flaky service

-- | How the service answers one call.
data Mode
  = Answer   -- ^ confirmed, one item echoing the params, done
  | DropMid  -- ^ confirmed, one item, then the connection closed
  | DropNow  -- ^ the connection closed before any answer
  | Silent   -- ^ confirmed, then nothing — not even a pong

data Service = Service
  { servicePort  :: Int
  , serviceModes :: IORef [Mode]     -- ^ one per call, the last repeated
  , serviceCalls :: IORef [Value]    -- ^ the params of every call, in order
  }

withService :: [Mode] -> (Service -> IO a) -> IO a
withService modes act = do
  ms <- newIORef modes
  calls <- newIORef []
  bracket (open ms calls) (N.close . fst) (\(_, h) -> act h)
  where
    open ms calls = do
      sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
      N.setSocketOption sock N.ReuseAddr 1
      N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
      N.listen sock 16
      port <- fromIntegral <$> N.socketPort sock
      void $ forkIO $ acceptLoop sock ms calls
      pure (sock, Service port ms calls)

acceptLoop :: N.Socket -> IORef [Mode] -> IORef [Value] -> IO ()
acceptLoop sock ms calls = do
  r <- try (N.accept sock)
  case r of
    Left (_ :: SomeException) -> pure ()
    Right (conn, _) -> do
      void $ forkIO $ do
        _ <- try (session conn) :: IO (Either SomeException ())
        N.close conn
      acceptLoop sock ms calls
  where
    session conn = do
      pending <- WS.makePendingConnection conn WS.defaultConnectionOptions
      c <- WS.acceptRequest pending
      forever $ do
        msg <- WS.receiveData c
        case decode msg of
          Just (Object o) -> handle c o
          _ -> pure ()
    handle c o = do
      let rid = KM.lookup "id" o
          params = case KM.lookup "params" o of { Just (Object p) -> p; _ -> KM.empty }
          inner = maybe Null id (KM.lookup "params" params)
          send = WS.sendTextData c . encode
          confirm = send (object ["jsonrpc" .= ("2.0" :: Text), "id" .= rid, "result" .= (7 :: Int)])
          emit item = send (object
            [ "jsonrpc" .= ("2.0" :: Text), "method" .= ("subscription" :: Text)
            , "params" .= object ["subscription" .= (7 :: Int), "result" .= item] ])
          meta = object ["provenance" .= (["service"] :: [Text]), "plexus_hash" .= ("h0" :: Text), "timestamp" .= (0 :: Int)]
          dat = emit (object ["type" .= ("data" :: Text), "metadata" .= meta, "content_type" .= ("t.echo" :: Text), "content" .= inner])
          done = emit (object ["type" .= ("done" :: Text), "metadata" .= meta])
      modifyIORef' calls (++ [inner])
      mode <- atomicModifyIORef' ms $ \case
        [m] -> ([m], m)
        (m : rest) -> (rest, m)
        [] -> ([], Answer)
      case mode of
        Answer -> confirm >> dat >> done
        DropMid -> confirm >> dat >> threadDelay 50000 >> ioError (userError "dropped")
        DropNow -> ioError (userError "dropped")
        Silent -> confirm >> forever (threadDelay 1000000)

cfgFor :: Int -> SubstrateConfig
cfgFor port = SubstrateConfig "127.0.0.1" port "/" "t" [] Nothing

-- | One try, as synapse makes it.
attemptAt :: Int -> Value -> IO () -> (PT.PlexusStreamItem -> IO ()) -> IO (Either PT.TransportError ())
attemptAt port ps subscribed onItem =
  ST.invokeMethodStreamingWith (cfgFor port) subscribed ["t"] "m" ps onItem

noReauth :: IO Bool
noReauth = pure False

-- | A port nothing listens on.
deadPort :: IO Int
deadPort = do
  sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
  N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
  port <- fromIntegral <$> N.socketPort sock
  N.close sock
  pure port

isData :: PT.PlexusStreamItem -> Bool
isData PT.StreamData{} = True
isData _ = False

readShape, writeShape, sinceShape :: CallShape
readShape = CallShape False True False
writeShape = CallShape False False False
sinceShape = CallShape True True True

method :: Text -> Maybe Text -> Maybe Value -> MethodSchema
method name grp params = MethodSchema
  { methodName = name, methodDescription = "", methodHash = "h", methodParams = params
  , methodReturns = Nothing, methodStreaming = False, methodBidirectional = False
  , methodRequestType = Nothing, methodResponseType = Nothing, methodDeprecation = Nothing
  , methodParamSchemas = Nothing, methodRole = MethodRoleRpc, methodLongDescription = Nothing
  , methodGrant = Nothing, methodTarget = Nothing, methodHeldOn = Nothing, methodPublic = False
  , methodGroup = grp, methodExamples = [], methodInput = Nothing }

-- | Never hang a test: a call that waits forever fails it.
within :: Int -> IO a -> IO a
within secs act = timeout (secs * 1000000) act >>= maybe (expectationFailure "hung" >> error "hung") pure

main :: IO ()
main = hspec $ do
  describe "the window" $ do
    it "reads its spellings" $ do
      map parseDuration ["10m", "90s", "1h", "1m30s", "600", "0", "", "ten", "5x"]
        `shouldBe` [Just 600, Just 90, Just 3600, Just 90, Just 600, Just 0, Nothing, Nothing, Nothing]
    it "says a duration short" $
      map renderDuration [0, 45, 130, 600, 3600] `shouldBe` ["0s", "45s", "2m10s", "10m", "1h"]

  describe "a method's shape" $ do
    it "is a read when its group says so, a write when it says write" $ do
      csRead (callShape (method "get" (Just "read") Nothing)) `shouldBe` True
      csRead (callShape (method "get" (Just "write") Nothing)) `shouldBe` False
    it "falls back on a read's name, and anything else is a write" $ do
      csRead (callShape (method "list" (Just "inputs") Nothing)) `shouldBe` True
      csRead (callShape (method "answer" (Just "inputs") Nothing)) `shouldBe` False
      csRead (callShape (method "post" Nothing Nothing)) `shouldBe` False
    it "is never a read when bidirectional" $
      csRead (callShape ((method "get" (Just "read") Nothing) { methodBidirectional = True })) `shouldBe` False
    it "takes since when its params have one" $ do
      let ps = object ["type" .= ("object" :: Text), "properties" .= object ["since" .= object [], "id" .= object []]]
      csSince (callShape (method "await_reply" (Just "read") (Just ps))) `shouldBe` True
      csSince (callShape (method "get" (Just "read") Nothing)) `shouldBe` False

  describe "what an outage is" $ do
    let upgrade code = PT.ProtocolError ("Connection failed: MalformedResponse (ResponseHead {responseCode = "
          <> code <> ", responseMessage = \"x\", responseHeaders = []}) \"Wrong response status or message.\"")
    it "is a proxy in front answering 502 / 503 / 504 while the service behind it is away (SCAFFOLD-21)" $ do
      map (isOutage . upgrade) ["502", "503", "504"] `shouldBe` [True, True, True]
      isOutageText "responseCode = 502, responseMessage = \"Bad Gateway\"" `shouldBe` True
    it "is not a refused credential, a wrong path, a misdirected request" $
      map (isOutage . upgrade) ["401", "403", "404", "421"] `shouldBe` [False, False, False, False]
    it "is the server's own 429, as before" $
      isOutage (upgrade "429") `shouldBe` True
    it "is a socket's own broken pipe (a peer gone away), as before (SYNAPSE-26)" $
      isOutage (PT.NetworkError "Network.Socket.sendBuf: resource vanished (Broken pipe)") `shouldBe` True

  outageSpec
  -- SYNAPSE-28: the same outages, every call through a connection holder
  -- (in this process, on a socket of the test's own): what the run sees
  -- of an outage is what it saw dialing itself
  describe "through the connection holder" $ beforeAll_ throughHolder $ do
    it "is the holder that answers" $
      withService [Answer] $ \h -> do
        sock <- holderSocket
        acc <- newIORef (0 :: Int)
        Hold.holderCall sock (cfgFor (servicePort h)) (pure ()) "t.call" (object ["method" .= ("t.m" :: Text)])
          (\i -> if isData i then modifyIORef' acc (+ 1) else pure ())
        readIORef acc `shouldReturn` 1
    outageSpec

-- | SYNAPSE-20's outages, as a run meets them.
outageSpec :: Spec
outageSpec = do
  describe "the client notices a lost connection" $ do
    it "fails a stream whose connection closes, instead of waiting forever" $
      withService [DropMid] $ \h -> do
        r <- within 10 $ attemptAt (servicePort h) (object []) (pure ()) (\_ -> pure ())
        r `shouldSatisfy` \case
          Left (PT.ConnectionLost _ True) -> True
          _ -> False
    it "fails a call dropped before any answer, as sent" $
      withService [DropNow] $ \h -> do
        r <- within 10 $ attemptAt (servicePort h) (object []) (pure ()) (\_ -> pure ())
        r `shouldSatisfy` \case
          Left (PT.ConnectionLost _ True) -> True
          _ -> False
    it "notices a silent peer by its keepalive" $
      withService [Silent] $ \h -> do
        setEnv "PLEXUS_KEEPALIVE" "1,1"
        r <- within 10 $ attemptAt (servicePort h) (object []) (pure ()) (\_ -> pure ())
        unsetEnv "PLEXUS_KEEPALIVE"
        case r of
          Left (PT.ConnectionLost why True) -> why `shouldBe` "no answer to a keepalive ping in 1 s"
          other -> expectationFailure (show other)

  describe "synapse rides out an outage" $ do
    it "sends a read again, and says the call resumed when part had arrived" $
      withService [DropMid, Answer] $ \h -> do
        setReconnectWindow 10
        items <- newIORef (0 :: Int)
        resumed <- newIORef []
        e <- within 20 $ resilientCall "service" readShape (attemptAt (servicePort h)) noReauth (object ["id" .= ("A-1" :: Text)])
               (\r -> modifyIORef' resumed (r :)) (\i -> if isData i then modifyIORef' items (+ 1) else pure ())
        e `shouldBe` Finished
        readIORef items `shouldReturn` 2
        length <$> readIORef resumed `shouldReturn` 1
        length <$> readIORef (serviceCalls h) `shouldReturn` 2
    it "resumes a stream with since set to when it began" $
      withService [DropMid, Answer] $ \h -> do
        setReconnectWindow 10
        resumed <- newIORef []
        e <- within 20 $ resilientCall "service" sinceShape (attemptAt (servicePort h)) noReauth (object ["id" .= ("A-1" :: Text), "since" .= Null])
               (\r -> modifyIORef' resumed (r :)) (\_ -> pure ())
        e `shouldBe` Finished
        [first, second] <- readIORef (serviceCalls h)
        first `shouldBe` object ["id" .= ("A-1" :: Text), "since" .= Null]
        case second of
          Object o -> KM.lookup "since" o `shouldSatisfy` \case { Just (Number _) -> True; _ -> False }
          _ -> expectationFailure "params"
        map rsSince <$> readIORef resumed >>= \ss -> ss `shouldSatisfy` all (/= Nothing)
    it "keeps a since the caller gave" $
      withService [DropMid, Answer] $ \h -> do
        setReconnectWindow 10
        _ <- within 20 $ resilientCall "service" sinceShape (attemptAt (servicePort h)) noReauth (object ["since" .= (42 :: Int)])
               (\_ -> pure ()) (\_ -> pure ())
        readIORef (serviceCalls h) `shouldReturn` [object ["since" .= (42 :: Int)], object ["since" .= (42 :: Int)]]
    it "never sends a write again once it went out" $
      withService [DropNow, Answer] $ \h -> do
        setReconnectWindow 10
        e <- within 20 $ resilientCall "service" writeShape (attemptAt (servicePort h)) noReauth (object []) (\_ -> pure ()) (\_ -> pure ())
        e `shouldSatisfy` \case { WriteUnknown _ -> True; _ -> False }
        length <$> readIORef (serviceCalls h) `shouldReturn` 1
    it "sends a write that never left once the backend is back" $ do
      port <- deadPort
      setReconnectWindow 20
      -- the backend comes up on that port 2 s later
      r <- newEmptyIORef
      void $ forkIO $ do
        threadDelay 2000000
        withServiceAt port [Answer] $ \h -> do
          threadDelay 8000000
          readIORef (serviceCalls h) >>= writeIORef r . Just . length
      e <- within 20 $ resilientCall "service" writeShape (attemptAt port) noReauth (object []) (\_ -> pure ()) (\_ -> pure ())
      e `shouldBe` Finished
    it "gives up when the window runs out" $ do
      port <- deadPort
      setReconnectWindow 2
      e <- within 20 $ resilientCall "service" readShape (attemptAt port) noReauth (object []) (\_ -> pure ()) (\_ -> pure ())
      e `shouldSatisfy` \case { GaveUp 2 _ -> True; _ -> False }
    it "with no window, reports the loss as before" $
      withService [DropMid] $ \h -> do
        setReconnectWindow 0
        e <- within 20 $ resilientCall "service" readShape (attemptAt (servicePort h)) noReauth (object []) (\_ -> pure ()) (\_ -> pure ())
        e `shouldSatisfy` \case { Failed (PT.ConnectionLost _ True) -> True; _ -> False }

  describe "a closed stdout is not an outage (SYNAPSE-26)" $ do
    it "ends a write whose answer cannot be printed, sent once, with no pause" $
      withService [Answer] $ \h -> do
        setReconnectWindow 10
        t0 <- getMonotonicTime
        e <- within 20 $ resilientCall "service" writeShape (attemptAt (servicePort h)) noReauth (object []) (\_ -> pure ()) (\_ -> throwIO stdoutClosed)
        t1 <- getMonotonicTime
        length <$> readIORef (serviceCalls h) `shouldReturn` 1
        e `shouldNotBe` Finished
        e `shouldSatisfy` \case { GaveUp{} -> False; WriteUnknown{} -> False; _ -> True }
        (t1 - t0) `shouldSatisfy` (< 1)
    it "ends a read whose item cannot be printed, sent once, even with no window" $
      withService [Answer] $ \h -> do
        setReconnectWindow 0
        e <- within 20 $ resilientCall "service" readShape (attemptAt (servicePort h)) noReauth (object []) (\_ -> pure ()) (\_ -> throwIO stdoutClosed)
        length <$> readIORef (serviceCalls h) `shouldReturn` 1
        show e `shouldNotContain` "Transport"
        e `shouldSatisfy` \case { Failed{} -> False; _ -> True }
    it "with stdout open, a read cut mid-answer is sent again and printed whole" $
      withService [DropMid, Answer] $ \h -> do
        setReconnectWindow 10
        printed <- newIORef (0 :: Int)
        e <- within 20 $ resilientCall "service" readShape (attemptAt (servicePort h)) noReauth (object [])
               (\_ -> pure ()) (\i -> if isData i then modifyIORef' printed (+ 1) else pure ())
        e `shouldBe` Finished
        readIORef printed `shouldReturn` 2
        length <$> readIORef (serviceCalls h) `shouldReturn` 2
  where
    -- what a write to stdout raises once its reader has gone (GHC ignores
    -- SIGPIPE, so EPIPE comes back as this)
    stdoutClosed = IOError (Just stdout) ResourceVanished "hPutChar" "Broken pipe" (Just 32) Nothing
    newEmptyIORef :: IO (IORef (Maybe Int))
    newEmptyIORef = newIORef Nothing

-- | SYNAPSE-28: the holder's socket for these tests.
holderSocket :: IO FilePath
holderSocket = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  pure (tmp </> ("synapse-reconnect-test-" <> show pid <> ".sock"))

-- | Start a holder in this process and route every call through it.
throughHolder :: IO ()
throughHolder = do
  sock <- holderSocket
  _ <- forkIO $ Hold.runHolder (Hold.HoldOpts sock 600) `catch'` ()
  let wait (0 :: Int) = pure ()
      wait n = doesFileExist sock >>= \ok -> if ok then pure () else threadDelay 20000 >> wait (n - 1)
  wait 250
  _ <- Hold.installRouter Hold.RouterOpts { Hold.roHold = Just (const (pure (Just sock))), Hold.roHoldFor = 600 }
  pure ()
  where
    catch' act v = void (try act :: IO (Either SomeException ())) >> pure v

-- | A service on a port already chosen.
withServiceAt :: Int -> [Mode] -> (Service -> IO a) -> IO a
withServiceAt port modes act = do
  ms <- newIORef modes
  calls <- newIORef []
  bracket (open ms calls) N.close (\_ -> act (Service port ms calls))
  where
    open ms calls = do
      sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
      N.setSocketOption sock N.ReuseAddr 1
      N.bind sock (N.SockAddrInet (fromIntegral port) (N.tupleToHostAddress (127, 0, 0, 1)))
      N.listen sock 16
      void $ forkIO $ acceptLoop sock ms calls
      pure sock
