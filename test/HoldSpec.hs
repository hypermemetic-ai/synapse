{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SYNAPSE-28: a call costs one round trip and the server's time.
--
-- * The multiplexer: every call of a process on one connection, in
--   sequence and twenty at once; a stream it abandons is unsubscribed.
-- * The holder: runs hand it their requests; however many, the hub sees
--   one connection; a run that goes away mid-stream ends its call; the
--   holder exits when idle and removes its socket.
-- * The CLI, end to end against the mock hub (the built synapse, with a
--   home, a cache and a runtime directory of the test's own): a cold call
--   answers and fills the cache, on one connection; a warm call opens no
--   connection, probes nothing and fetches no schema; @--no-hold@ opens
--   exactly one; a method the hub renamed falls back to the probe and the
--   fetch, then answers; a method the hub no longer has is refused after
--   the fallback, as it always was.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently, async, cancel)
import Control.Exception (SomeException, try, fromException)
import Control.Monad (forM_, replicateM_, void)
import Data.Maybe (isJust)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.Text as T
import System.Directory (doesFileExist, createDirectoryIfMissing, findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Hspec

import Plexus.Client (SubstrateConfig(..), LostConnection(..), defaultConfig)
import Plexus.Types (PlexusStreamItem(..))
import qualified Synapse.Hold as Hold

import MockHub

main :: IO ()
main = hspec $ do
  describe "the multiplexer" $ do
    it "carries every call of a process on one connection, in sequence and twenty at once" $
      withMockHub todaysHub $ \port -> do
        before <- readIORef connectionsAccepted
        mux <- Hold.newMux
        let cfg = hubAt port
        replicateM_ 5 $ do
          items <- collect (Hold.muxCall mux cfg) "menger.call" (getTicket "MENGER-12")
          items `shouldSatisfy` any isTicket
        rs <- mapConcurrently (\_ -> collect (Hold.muxCall mux cfg) "menger.call" (getTicket "MENGER-12")) [1 .. 20 :: Int]
        all (any isTicket) rs `shouldBe` True
        after <- readIORef connectionsAccepted
        (after - before) `shouldBe` 1
        Hold.muxConnections mux `shouldReturn` 1

    it "asks a schema once however many wait for it, and gives each the answer" $
      withMockHub todaysHub $ \port -> do
        mux <- Hold.newMux
        s0 <- readIORef schemaCalls
        rs <- mapConcurrently (\_ -> collect (Hold.muxCall mux (hubAt port)) "menger.call"
                (Object (KM.fromList [("method", String "tickets.schema")]))) [1 .. 20 :: Int]
        s1 <- readIORef schemaCalls
        (s1 - s0) `shouldSatisfy` (< 20)
        all (== head rs) rs `shouldBe` True
        all (any isSchema) rs `shouldBe` True

    it "unsubscribes a stream it abandons" $
      withMockHub todaysHub $ \port -> do
        writeIORef unsubscribes []
        mux <- Hold.newMux
        got <- newEmptyFlag
        a <- async $ Hold.muxCall mux (hubAt port) (pure ()) "menger.call" (watch) (\_ -> setFlag got)
        waitFlag got
        cancel a
        threadDelay 200000
        us <- readIORef unsubscribes
        length us `shouldBe` 1

  describe "the holder" $ do
    it "serves many runs' requests on one connection" $
      withSystemTempDirectory "hold" $ \dir -> withMockHub todaysHub $ \port -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.HoldOpts sock 30) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        before <- readIORef connectionsAccepted
        let call = collect (Hold.holderCall sock (hubAt port)) "menger.call" (getTicket "MENGER-12")
        replicateM_ 3 (call >>= (`shouldSatisfy` any isTicket))
        rs <- mapConcurrently (const call) [1 .. 20 :: Int]
        all (any isTicket) rs `shouldBe` True
        after <- readIORef connectionsAccepted
        (after - before) `shouldBe` 1

    it "relays each item as the hub sent it, and the end" $
      withSystemTempDirectory "hold" $ \dir -> withMockHub todaysHub $ \port -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.HoldOpts sock 30) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        direct <- Hold.newMux
        viaMux <- collect (Hold.muxCall direct (hubAt port)) "menger.call" (getTicket "MENGER-12")
        viaHolder <- collect (Hold.holderCall sock (hubAt port)) "menger.call" (getTicket "MENGER-12")
        viaHolder `shouldBe` viaMux

    it "ends the call of a run that goes away mid-stream" $
      withSystemTempDirectory "hold" $ \dir -> withMockHub todaysHub $ \port -> do
        writeIORef unsubscribes []
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.HoldOpts sock 30) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        got <- newEmptyFlag
        a <- async $ Hold.holderCall sock (hubAt port) (pure ()) "menger.call" watch (\_ -> setFlag got)
        waitFlag got
        cancel a
        waitFor ((== 1) . length <$> readIORef unsubscribes)

    it "says a dial it could not make as connect does, so the run reads an outage" $
      withSystemTempDirectory "hold" $ \dir -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.HoldOpts sock 30) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        r <- try (collect (Hold.holderCall sock (hubAt 1)) "menger.call" (getTicket "X"))
        case r of
          Left (e :: SomeException) -> do
            show e `shouldContain` "Connection failed"
            (fromException e :: Maybe LostConnection) `shouldSatisfy` (\x -> case x of { Nothing -> True; Just _ -> False })
          Right _ -> expectationFailure "a call to a closed port answered"

    it "carries a bidirectional request and its answer" $
      withSystemTempDirectory "hold" $ \dir -> withMockHub todaysHub $ \port -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.HoldOpts sock 30) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        answered <- newIORef Nothing
        let onItem i = case i of
              StreamRequest {} -> void $ collect (Hold.holderCall sock (hubAt port)) "menger.respond"
                (Object (KM.fromList [ ("request_id", String "req-1")
                                     , ("response_data", Object (KM.fromList [("type", String "confirmed"), ("value", Bool True)])) ]))
              StreamData _ _ "tickets.ask" v -> writeIORef answered (Just v)
              _ -> pure ()
        Hold.holderCall sock (hubAt port) (pure ()) "menger.call" (Object (KM.fromList [("method", String "tickets.ask")])) onItem
        readIORef answered >>= (`shouldSatisfy` isJust)

    it "exits when idle, and takes its socket with it" $
      withSystemTempDirectory "hold" $ \dir -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.HoldOpts sock 1) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        waitFor (not <$> doesFileExist sock)

  describe "a socket per address and agent" $ do
    it "names the address, the agent and the version" $ do
      p <- Hold.socketPathFor "3.16.0" "wss_app.menger.sh_443_ws" "builder"
      p `shouldContain` "wss_app.menger.sh_443_ws@builder-3.16.0.sock"
      q <- Hold.socketPathFor "3.16.0" "wss_app.menger.sh_443_ws" "pipeline"
      q `shouldNotBe` p
    it "stays short enough for a unix socket" $ do
      p <- Hold.socketPathFor "3.16.0" (replicate 120 'h') "builder"
      length p `shouldSatisfy` (< 108)

  describe "the CLI, sending first" $ do
    synapse <- runIO (findExecutable "synapse")
    case synapse of
      Nothing -> it "(the synapse executable is not on PATH)" $ pendingWith "cabal test puts it there"
      Just exe -> cliSpec exe

cliSpec :: FilePath -> Spec
cliSpec exe = do
  it "is this build" $ do
    (_, out, _) <- readCreateProcessWithExitCode (proc exe ["--version"]) ""
    out `shouldStartWith` "3.17"

  it "answers cold, fills the cache, and opens one connection for the probe, the schema and the call" $
    withRun $ \run _ -> do
      c0 <- readIORef connectionsAccepted
      (code, out, err) <- run ["tickets", "get", "--id", "MENGER-12"]
      (code, err) `shouldBe` (ExitSuccess, "")
      out `shouldContain` "MENGER-12"
      c1 <- readIORef connectionsAccepted
      (c1 - c0) `shouldBe` 1

  it "answers warm with no new connection, no probe and no schema fetch" $
    withRun $ \run _ -> do
      _ <- run ["tickets", "get", "--id", "MENGER-12"]
      c0 <- readIORef connectionsAccepted
      i0 <- readIORef infoCalls
      s0 <- readIORef schemaCalls
      forM_ [1 .. 3 :: Int] $ \_ -> do
        (code, out, _) <- run ["tickets", "get", "--id", "MENGER-12"]
        code `shouldBe` ExitSuccess
        out `shouldContain` "MENGER-12"
      c1 <- readIORef connectionsAccepted
      i1 <- readIORef infoCalls
      s1 <- readIORef schemaCalls
      (c1 - c0, i1 - i0, s1 - s0) `shouldBe` (0, 0, 0)

  it "with --no-hold, opens exactly one connection a run" $
    withRun $ \run runWith -> do
      _ <- run ["tickets", "get", "--id", "MENGER-12"]
      c0 <- readIORef connectionsAccepted
      (code, out, _) <- runWith ["--no-hold"] ["tickets", "get", "--id", "MENGER-12"]
      code `shouldBe` ExitSuccess
      out `shouldContain` "MENGER-12"
      c1 <- readIORef connectionsAccepted
      (c1 - c0) `shouldBe` 1

  it "falls back to the probe and the fetch for a method the hub renamed, then answers" $
    withRun $ \run _ -> do
      _ <- run ["tickets", "get", "--id", "MENGER-12"]
      writeIORef renamedGet True
      s0 <- readIORef schemaCalls
      (code, out, err) <- run ["tickets", "fetch", "--id", "MENGER-12"]
      writeIORef renamedGet False
      (code, err) `shouldBe` (ExitSuccess, "")
      out `shouldContain` "MENGER-12"
      s1 <- readIORef schemaCalls
      (s1 - s0) `shouldSatisfy` (> 0)

  it "asks the server when it refuses a method the cache still has, and says it is gone" $
    withRun $ \run _ -> do
      _ <- run ["tickets", "get", "--id", "MENGER-12"]
      writeIORef renamedGet True
      (code, out, err) <- run ["tickets", "get", "--id", "MENGER-12"]
      writeIORef renamedGet False
      code `shouldBe` ExitFailure 1
      err `shouldContain` "Command not found: 'get'"
      out `shouldNotContain` "Method not found"
      -- and the call after it is warm again, for what the hub has now
      writeIORef renamedGet True
      (code', out', _) <- run ["tickets", "fetch", "--id", "MENGER-12"]
      writeIORef renamedGet False
      code' `shouldBe` ExitSuccess
      out' `shouldContain` "MENGER-12"
  it "says a refusal once and exits 77, asking nothing more (a write is never sent twice for it)" $
    withRunOn (Scenario CallerView False True mockState False) $ \run _ -> do
      _ <- run ["tickets", "get", "--id", "MENGER-12"]
      (code0, _, _) <- run ["tickets", "set_status", "--id", "IDENTITY-1"]
      code0 `shouldBe` ExitFailure 77
      s0 <- readIORef schemaCalls
      i0 <- readIORef infoCalls
      (code, _, err) <- run ["tickets", "set_status", "--id", "IDENTITY-1"]
      code `shouldBe` ExitFailure 77
      err `shouldContain` "refused: needs tickets.set_status on IDENTITY"
      s1 <- readIORef schemaCalls
      i1 <- readIORef infoCalls
      (s1 - s0, i1 - i0) `shouldBe` (0, 0)
  where
    withRun = withRunOn todaysHub
    -- a run of the built synapse against a fresh mock hub, with a home,
    -- cache and runtime directory of its own (no keychain, a short hold)
    withRunOn sc act = withSystemTempDirectory "synapse28" $ \dir -> withMockHub sc $ \port -> do
      forM_ ["home", "cache", "run"] $ \d -> createDirectoryIfMissing True (dir </> d)
      env0 <- getEnvironment
      let env = [ (k, v) | (k, v) <- env0, k `notElem` ["HOME", "XDG_CACHE_HOME", "XDG_RUNTIME_DIR", "SYNAPSE_AS", "PLEXUS_SESSION"] ]
              ++ [ ("HOME", dir </> "home"), ("XDG_CACHE_HOME", dir </> "cache"), ("XDG_RUNTIME_DIR", dir </> "run")
                 , ("PLEXUS_KEYCHAIN", "off"), ("SYNAPSE_HOLD_FOR", "3s") ]
          run' extra args = readCreateProcessWithExitCode
            (proc exe (extra ++ ["--url", "ws://127.0.0.1:" <> show port <> "/ws", "menger"] ++ args)) { env = Just env } ""
      writeIORef renamedGet False
      act (run' []) run'

-- ---------------------------------------------------------------------------

hubAt :: Int -> SubstrateConfig
hubAt port = (defaultConfig "menger") { substratePort = port, substratePath = "/ws" }

getTicket :: T.Text -> Value
getTicket i = Object (KM.fromList [("method", String "tickets.get"), ("params", Object (KM.fromList [("id", String i)]))])

watch :: Value
watch = Object (KM.fromList [("method", String "tickets.watch")])

collect :: (IO () -> T.Text -> Value -> (PlexusStreamItem -> IO ()) -> IO ()) -> T.Text -> Value -> IO [PlexusStreamItem]
collect route method params = do
  acc <- newIORef []
  route (pure ()) method params (\i -> modifyIORef' acc (i :))
  reverse <$> readIORef acc

isSchema :: PlexusStreamItem -> Bool
isSchema (StreamData _ _ "tickets.schema" _) = True
isSchema _ = False

isTicket :: PlexusStreamItem -> Bool
isTicket (StreamData _ _ "tickets.get" _) = True
isTicket _ = False

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll act h = try act >>= either h pure

waitFor :: IO Bool -> IO ()
waitFor cond = go (100 :: Int)
  where
    go 0 = expectationFailure "timed out waiting"
    go n = cond >>= \ok -> if ok then pure () else threadDelay 50000 >> go (n - 1)

newtype Flag = Flag (IORef Bool)

newEmptyFlag :: IO Flag
newEmptyFlag = Flag <$> newIORef False

setFlag :: Flag -> IO ()
setFlag (Flag r) = writeIORef r True

waitFlag :: Flag -> IO ()
waitFlag (Flag r) = waitFor (readIORef r)
