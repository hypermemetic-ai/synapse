{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SYNAPSE-20: a lost connection is noticed, and synapse rides out the
-- outage. Against a flaky hub in miniature (each call answered as the test
-- says: answered, dropped mid-stream, dropped before any answer, or taken
-- and then silent), on ports of its own:
--
-- * the client fails a call whose connection closes or goes silent —
--   'PT.ConnectionLost' — instead of waiting forever (APP-41's hang);
-- * a read is sent again, a stream resumes (with @since@ when it takes
--   one), a write that went out is never repeated, one that never left is;
-- * the window, its spelling, and a method's shape from its schema.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever, void)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.Text (Text)
import qualified Network.Socket as N
import qualified Network.WebSockets as WS
import System.Environment (setEnv, unsetEnv)
import System.Timeout (timeout)
import Test.Hspec

import Plexus.Client (SubstrateConfig(..))
import Plexus.Schema.Recursive (MethodSchema(..), MethodRole(..))
import qualified Plexus.Transport as ST
import qualified Plexus.Types as PT
import Synapse.Reconnect

-- ---------------------------------------------------------------------------
-- The flaky hub

-- | How the hub answers one call.
data Mode
  = Answer   -- ^ confirmed, one item echoing the params, done
  | DropMid  -- ^ confirmed, one item, then the connection closed
  | DropNow  -- ^ the connection closed before any answer
  | Silent   -- ^ confirmed, then nothing — not even a pong

data Hub = Hub
  { hubPort  :: Int
  , hubModes :: IORef [Mode]     -- ^ one per call, the last repeated
  , hubCalls :: IORef [Value]    -- ^ the params of every call, in order
  }

withHub :: [Mode] -> (Hub -> IO a) -> IO a
withHub modes act = do
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
      pure (sock, Hub port ms calls)

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
          meta = object ["provenance" .= (["hub"] :: [Text]), "plexus_hash" .= ("h0" :: Text), "timestamp" .= (0 :: Int)]
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
cfgFor port = SubstrateConfig "127.0.0.1" port "/" "t" []

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
  , methodGroup = grp, methodExamples = [] }

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

  describe "the client notices a lost connection" $ do
    it "fails a stream whose connection closes, instead of waiting forever" $
      withHub [DropMid] $ \h -> do
        r <- within 10 $ attemptAt (hubPort h) (object []) (pure ()) (\_ -> pure ())
        r `shouldSatisfy` \case
          Left (PT.ConnectionLost _ True) -> True
          _ -> False
    it "fails a call dropped before any answer, as sent" $
      withHub [DropNow] $ \h -> do
        r <- within 10 $ attemptAt (hubPort h) (object []) (pure ()) (\_ -> pure ())
        r `shouldSatisfy` \case
          Left (PT.ConnectionLost _ True) -> True
          _ -> False
    it "notices a silent peer by its keepalive" $
      withHub [Silent] $ \h -> do
        setEnv "PLEXUS_KEEPALIVE" "1,1"
        r <- within 10 $ attemptAt (hubPort h) (object []) (pure ()) (\_ -> pure ())
        unsetEnv "PLEXUS_KEEPALIVE"
        case r of
          Left (PT.ConnectionLost why True) -> why `shouldBe` "no answer to a keepalive ping in 1 s"
          other -> expectationFailure (show other)

  describe "synapse rides out an outage" $ do
    it "sends a read again, and says the call resumed when part had arrived" $
      withHub [DropMid, Answer] $ \h -> do
        setReconnectWindow 10
        items <- newIORef (0 :: Int)
        resumed <- newIORef []
        e <- within 20 $ resilientCall "hub" readShape (attemptAt (hubPort h)) noReauth (object ["id" .= ("A-1" :: Text)])
               (\r -> modifyIORef' resumed (r :)) (\i -> if isData i then modifyIORef' items (+ 1) else pure ())
        e `shouldBe` Finished
        readIORef items `shouldReturn` 2
        length <$> readIORef resumed `shouldReturn` 1
        length <$> readIORef (hubCalls h) `shouldReturn` 2
    it "resumes a stream with since set to when it began" $
      withHub [DropMid, Answer] $ \h -> do
        setReconnectWindow 10
        resumed <- newIORef []
        e <- within 20 $ resilientCall "hub" sinceShape (attemptAt (hubPort h)) noReauth (object ["id" .= ("A-1" :: Text), "since" .= Null])
               (\r -> modifyIORef' resumed (r :)) (\_ -> pure ())
        e `shouldBe` Finished
        [first, second] <- readIORef (hubCalls h)
        first `shouldBe` object ["id" .= ("A-1" :: Text), "since" .= Null]
        case second of
          Object o -> KM.lookup "since" o `shouldSatisfy` \case { Just (Number _) -> True; _ -> False }
          _ -> expectationFailure "params"
        map rsSince <$> readIORef resumed >>= \ss -> ss `shouldSatisfy` all (/= Nothing)
    it "keeps a since the caller gave" $
      withHub [DropMid, Answer] $ \h -> do
        setReconnectWindow 10
        _ <- within 20 $ resilientCall "hub" sinceShape (attemptAt (hubPort h)) noReauth (object ["since" .= (42 :: Int)])
               (\_ -> pure ()) (\_ -> pure ())
        readIORef (hubCalls h) `shouldReturn` [object ["since" .= (42 :: Int)], object ["since" .= (42 :: Int)]]
    it "never sends a write again once it went out" $
      withHub [DropNow, Answer] $ \h -> do
        setReconnectWindow 10
        e <- within 20 $ resilientCall "hub" writeShape (attemptAt (hubPort h)) noReauth (object []) (\_ -> pure ()) (\_ -> pure ())
        e `shouldSatisfy` \case { WriteUnknown _ -> True; _ -> False }
        length <$> readIORef (hubCalls h) `shouldReturn` 1
    it "sends a write that never left once the backend is back" $ do
      port <- deadPort
      setReconnectWindow 20
      -- the backend comes up on that port 2 s later
      r <- newEmptyIORef
      void $ forkIO $ do
        threadDelay 2000000
        withHubAt port [Answer] $ \h -> do
          threadDelay 8000000
          readIORef (hubCalls h) >>= writeIORef r . Just . length
      e <- within 20 $ resilientCall "hub" writeShape (attemptAt port) noReauth (object []) (\_ -> pure ()) (\_ -> pure ())
      e `shouldBe` Finished
    it "gives up when the window runs out" $ do
      port <- deadPort
      setReconnectWindow 2
      e <- within 20 $ resilientCall "hub" readShape (attemptAt port) noReauth (object []) (\_ -> pure ()) (\_ -> pure ())
      e `shouldSatisfy` \case { GaveUp 2 _ -> True; _ -> False }
    it "with no window, reports the loss as before" $
      withHub [DropMid] $ \h -> do
        setReconnectWindow 0
        e <- within 20 $ resilientCall "hub" readShape (attemptAt (hubPort h)) noReauth (object []) (\_ -> pure ()) (\_ -> pure ())
        e `shouldSatisfy` \case { Failed (PT.ConnectionLost _ True) -> True; _ -> False }
  where
    newEmptyIORef :: IO (IORef (Maybe Int))
    newEmptyIORef = newIORef Nothing

-- | A hub on a port already chosen.
withHubAt :: Int -> [Mode] -> (Hub -> IO a) -> IO a
withHubAt port modes act = do
  ms <- newIORef modes
  calls <- newIORef []
  bracket (open ms calls) N.close (\_ -> act (Hub port ms calls))
  where
    open ms calls = do
      sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
      N.setSocketOption sock N.ReuseAddr 1
      N.bind sock (N.SockAddrInet (fromIntegral port) (N.tupleToHostAddress (127, 0, 0, 1)))
      N.listen sock 16
      void $ forkIO $ acceptLoop sock ms calls
      pure sock
