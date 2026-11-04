{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SYNAPSE-28: a call costs one round trip and the server's time.
--
-- * The multiplexer: every call of a process on one connection, in
--   sequence and twenty at once; a stream it abandons is unsubscribed.
-- * The holder: runs hand it their requests; however many, the service sees
--   one connection; a run that goes away mid-stream ends its call; the
--   holder exits when idle and removes its socket.
-- * The CLI, end to end against the mock service (the built synapse, with a
--   home, a cache and a runtime directory of the test's own): a cold call
--   answers and fills the cache, on one connection; a warm call opens no
--   connection, probes nothing and fetches no schema; @--no-hold@ opens
--   exactly one; a method the service renamed falls back to the probe and the
--   fetch, then answers; a method the service no longer has is refused after
--   the fallback, as it always was.
-- * SYNAPSE-40, the built synapse again: one holder process for the user,
--   however many addresses and agents call, a connection for each (address,
--   agent), never one shared by two agents; a connection whose server
--   stopped is gone within 10 s while the rest are kept; the cap closes the
--   longest idle first; and a loop of calls against short-lived servers
--   (@SYNAPSE40_LOOP@, 500) runs through one holder that holds nothing 10 s
--   after the last server stops, its memory flat. What the holder holds is
--   asked of it on its socket (@{"status":true}@); every holder a test
--   started is stopped by pid when the test ends.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (mapConcurrently, async, cancel)
import Control.Exception (SomeException, bracket, finally, try, fromException)
import Control.Monad (filterM, forM, forM_, forever, replicateM_, void, when)
import Data.Char (isDigit)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import Data.Maybe (isJust)
import Data.Aeson (Value(..), decodeStrict)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Clock (getMonotonicTime)
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import System.Directory (doesDirectoryExist, doesFileExist, createDirectoryIfMissing, findExecutable, getSymbolicLinkTarget, listDirectory)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (signalProcess, sigKILL, sigTERM)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Hspec

import Plexus.Client (SubstrateConfig(..), LostConnection(..), defaultConfig)
import Plexus.Input (InputHalf(..), InputPort(..), InputSignal(..))
import Plexus.Types (PlexusStreamItem(..))
import qualified Synapse.Hold as Hold

import MockService

main :: IO ()
main = hspec $ do
  describe "the multiplexer" $ do
    it "carries every call of a process on one connection, in sequence and twenty at once" $
      withMockService todaysService $ \port -> do
        before <- readIORef connectionsAccepted
        mux <- Hold.newMux
        let cfg = serviceAt port
        replicateM_ 5 $ do
          items <- collect (Hold.muxCall mux cfg) "menger.call" (getTicket "MENGER-12")
          items `shouldSatisfy` any isTicket
        rs <- mapConcurrently (\_ -> collect (Hold.muxCall mux cfg) "menger.call" (getTicket "MENGER-12")) [1 .. 20 :: Int]
        all (any isTicket) rs `shouldBe` True
        after <- readIORef connectionsAccepted
        (after - before) `shouldBe` 1
        Hold.muxConnections mux `shouldReturn` 1

    it "asks a schema once however many wait for it, and gives each the answer" $
      withMockService todaysService $ \port -> do
        mux <- Hold.newMux
        s0 <- readIORef schemaCalls
        rs <- mapConcurrently (\_ -> collect (Hold.muxCall mux (serviceAt port)) "menger.call"
                (Object (KM.fromList [("method", String "tickets.schema")]))) [1 .. 20 :: Int]
        s1 <- readIORef schemaCalls
        (s1 - s0) `shouldSatisfy` (< 20)
        all (== head rs) rs `shouldBe` True
        all (any isSchema) rs `shouldBe` True

    it "unsubscribes a stream it abandons" $
      withMockService todaysService $ \port -> do
        writeIORef unsubscribes []
        mux <- Hold.newMux
        got <- newEmptyFlag
        a <- async $ Hold.muxCall mux (serviceAt port) (pure ()) "menger.call" (watch) (\_ -> setFlag got)
        waitFlag got
        cancel a
        threadDelay 200000
        us <- readIORef unsubscribes
        length us `shouldBe` 1

  describe "the holder" $ do
    it "serves many runs' requests on one connection" $
      withSystemTempDirectory "hold" $ \dir -> withMockService todaysService $ \port -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.holdOpts sock 30) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        before <- readIORef connectionsAccepted
        let call = collect (Hold.holderCall sock "a1" (serviceAt port)) "menger.call" (getTicket "MENGER-12")
        replicateM_ 3 (call >>= (`shouldSatisfy` any isTicket))
        rs <- mapConcurrently (const call) [1 .. 20 :: Int]
        all (any isTicket) rs `shouldBe` True
        after <- readIORef connectionsAccepted
        (after - before) `shouldBe` 1

    it "relays each item as the service sent it, and the end" $
      withSystemTempDirectory "hold" $ \dir -> withMockService todaysService $ \port -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.holdOpts sock 30) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        direct <- Hold.newMux
        viaMux <- collect (Hold.muxCall direct (serviceAt port)) "menger.call" (getTicket "MENGER-12")
        viaHolder <- collect (Hold.holderCall sock "a1" (serviceAt port)) "menger.call" (getTicket "MENGER-12")
        viaHolder `shouldBe` viaMux

    it "ends the call of a run that goes away mid-stream" $
      withSystemTempDirectory "hold" $ \dir -> withMockService todaysService $ \port -> do
        writeIORef unsubscribes []
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.holdOpts sock 30) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        got <- newEmptyFlag
        a <- async $ Hold.holderCall sock "a1" (serviceAt port) (pure ()) "menger.call" watch (\_ -> setFlag got)
        waitFlag got
        cancel a
        waitFor ((== 1) . length <$> readIORef unsubscribes)

    it "says a dial it could not make as connect does, so the run reads an outage" $
      withSystemTempDirectory "hold" $ \dir -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.holdOpts sock 30) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        r <- try (collect (Hold.holderCall sock "a1" (serviceAt 1)) "menger.call" (getTicket "X"))
        case r of
          Left (e :: SomeException) -> do
            show e `shouldContain` "Connection failed"
            (fromException e :: Maybe LostConnection) `shouldSatisfy` (\x -> case x of { Nothing -> True; Just _ -> False })
          Right _ -> expectationFailure "a call to a closed port answered"

    it "carries a bidirectional request and its answer" $
      withSystemTempDirectory "hold" $ \dir -> withMockService todaysService $ \port -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.holdOpts sock 30) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        answered <- newIORef Nothing
        let onItem i = case i of
              StreamRequest {} -> void $ collect (Hold.holderCall sock "a1" (serviceAt port)) "menger.respond"
                (Object (KM.fromList [ ("request_id", String "req-1")
                                     , ("response_data", Object (KM.fromList [("type", String "confirmed"), ("value", Bool True)])) ]))
              StreamData _ _ "tickets.ask" v -> writeIORef answered (Just v)
              _ -> pure ()
        Hold.holderCall sock "a1" (serviceAt port) (pure ()) "menger.call" (Object (KM.fromList [("method", String "tickets.ask")])) onItem
        readIORef answered >>= (`shouldSatisfy` isJust)

    it "exits when it has held nothing for --hold-for, and takes its socket with it" $
      withSystemTempDirectory "hold" $ \dir -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.holdOpts sock 1) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        waitFor (not <$> doesFileExist sock)

    it "closes a connection idle for --hold-for, then exits" $
      withSystemTempDirectory "hold" $ \dir -> withMockService todaysService $ \port -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.holdOpts sock 1) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        collect (Hold.holderCall sock "a1" (serviceAt port)) "menger.call" (getTicket "MENGER-12") >>= (`shouldSatisfy` any isTicket)
        waitFor (not <$> doesFileExist sock)

  describe "the holder, one for every address and agent (SYNAPSE-40)" $ do
    it "keeps a connection per agent at one address, never one for two" $
      withHolder 30 0 $ \sock -> withMockService todaysService $ \port -> do
        c0 <- readIORef connectionsAccepted
        forM_ ["a1", "a2", "a1", "a2"] $ \a ->
          collect (Hold.holderCall sock a (serviceAt port)) "menger.call" (getTicket "MENGER-12") >>= (`shouldSatisfy` any isTicket)
        c1 <- readIORef connectionsAccepted
        (c1 - c0) `shouldBe` 2
        fmap (sort . heldOf) <$> Hold.holderStatus sock `shouldReturn` Just [(port, "a1"), (port, "a2")]

    it "keeps a connection per credential too, as a re-login makes one" $
      withHolder 30 0 $ \sock -> withMockService todaysService $ \port -> do
        c0 <- readIORef connectionsAccepted
        let as tok = (serviceAt port) { substrateHeaders = [("Authorization", "Bearer " <> tok)] }
        forM_ ["t1", "t2", "t1"] $ \tok ->
          collect (Hold.holderCall sock "a1" (as tok)) "menger.call" (getTicket "MENGER-12") >>= (`shouldSatisfy` any isTicket)
        c1 <- readIORef connectionsAccepted
        (c1 - c0) `shouldBe` 2

    it "lets a connection go within two seconds of its server stopping, and keeps the others" $
      withHolder 30 0 $ \sock -> withMockService todaysService $ \live -> do
        let call port = collect (Hold.holderCall sock "a1" (serviceAt port)) "menger.call" (getTicket "MENGER-12")
        call live >>= (`shouldSatisfy` any isTicket)
        gone <- withEphemeralService todaysService $ \port -> do
          call port >>= (`shouldSatisfy` any isTicket)
          fmap (sort . map fst . heldOf) <$> Hold.holderStatus sock `shouldReturn` Just (sort [live, port])
          pure port
        t0 <- getMonotonicTime
        waitFor (maybe False ((== [live]) . map fst . heldOf) <$> Hold.holderStatus sock)
        t1 <- getMonotonicTime
        (t1 - t0) `shouldSatisfy` (< 2.5)
        gone `shouldNotBe` live

    it "dials again, on the next call, an address whose server came back" $
      withHolder 30 0 $ \sock -> do
        port <- withEphemeralService todaysService $ \p -> do
          collect (Hold.holderCall sock "a1" (serviceAt p)) "menger.call" (getTicket "MENGER-12") >>= (`shouldSatisfy` any isTicket)
          pure p
        waitFor (maybe False (null . heldOf) <$> Hold.holderStatus sock)
        withMockServiceOn port $
          collect (Hold.holderCall sock "a1" (serviceAt port)) "menger.call" (getTicket "MENGER-12") >>= (`shouldSatisfy` any isTicket)

    it "closes the connection idle longest when it holds its cap, never one carrying a call" $
      withHolder 30 2 $ \sock -> withServices 4 $ \ps -> case ps of
        [s1, s2, s3, s4] -> do
          let call p = collect (Hold.holderCall sock "a1" (serviceAt p)) "menger.call" (getTicket "MENGER-12") >>= (`shouldSatisfy` any isTicket)
              held = fmap (sort . map fst . heldOf) <$> Hold.holderStatus sock
          -- s1 carries a stream throughout: never the one closed
          got <- newEmptyFlag
          w <- async $ Hold.holderCall sock "a1" (serviceAt s1) (pure ()) "menger.call" watch (\_ -> setFlag got)
          waitFlag got
          call s2
          call s3
          held `shouldReturn` Just (sort [s1, s3])
          call s4
          held `shouldReturn` Just (sort [s1, s4])
          cancel w
        _ -> expectationFailure "four services"

    it "leaves nothing open for a run that went away while its connection was being made" $
      withHolder 30 0 $ \sock -> withMockService todaysService $ \port -> do
        o0 <- readIORef connectionsOpen
        writeIORef upgradeDelay 1000000
        a <- async $ collect (Hold.holderCall sock "a1" (serviceAt port)) "menger.call" (getTicket "MENGER-12")
        threadDelay 300000   -- the dial is under way
        cancel a
        writeIORef upgradeDelay 0
        -- the dial ends a moment on: closed at once, nothing open, nothing held
        threadDelay 1000000
        waitFor ((== o0) <$> readIORef connectionsOpen)
        fmap heldOf <$> Hold.holderStatus sock `shouldReturn` Just []

    it "keeps a connection only as long as its call's --hold-for says" $
      withHolder 60 0 $ \sock -> withMockService todaysService $ \port -> do
        collect (Hold.holderCallAs sock "a1" (Just 1) (const (pure ())) (serviceAt port)) "menger.call" (getTicket "MENGER-12")
          >>= (`shouldSatisfy` any isTicket)
        fmap heldOf <$> Hold.holderStatus sock `shouldReturn` Just [(port, "a1")]
        waitFor (maybe False (null . heldOf) <$> Hold.holderStatus sock)

    it "exits when it holds nothing however often its status is asked" $
      withSystemTempDirectory "hold" $ \dir -> do
        let sock = dir </> "h.sock"
        _ <- forkIO $ Hold.runHolder (Hold.holdOpts sock 1) `catchAll` const (pure ())
        waitFor (doesFileExist sock)
        poller <- async $ forever $ Hold.holderStatus sock >> threadDelay 200000
        waitFor (not <$> doesFileExist sock)
        cancel poller

    it "says what it holds, never a credential" $
      withHolder 30 0 $ \sock -> withMockService todaysService $ \port -> do
        let cfg = (serviceAt port) { substrateHeaders = [("Authorization", "Bearer mgs_s3cret")] }
        collect (Hold.holderCall sock "a1" cfg) "menger.call" (getTicket "MENGER-12") >>= (`shouldSatisfy` any isTicket)
        st <- Hold.holderStatus sock
        fmap heldOf st `shouldBe` Just [(port, "a1")]
        show st `shouldNotContain` "s3cret"

  -- BIDIR-18: a call's input half rides the call's connection — through
  -- the holder (protocol 4: "sig", "in", "bin", "end") and on a run's own
  describe "a call with an input half" $ do
    let carries name route = it ("carries text and binary input frames " <> name) $
          withMockService todaysService $ \port -> do
            writeIORef schemaOverride (Just inputSchemas)
            resetInput defaultInputPlan
            (textItems, textSigs) <- duplexCall (route port) "stream.lines" $ \p -> do
              ipItems p 0 [String "one", String "two"]
              ipItems p 1 [String "three"]
              ipEnd p 2
            any isTaken textItems `shouldBe` True
            textSigs `shouldSatisfy` any (\case { InputCredit 64 -> True; _ -> False })
            textSigs `shouldSatisfy` any (\case { InputAck 1 -> True; _ -> False })
            rs <- readIORef inputReceived
            rs `shouldBe` [ReceivedJson 0 [String "one", String "two"], ReceivedJson 1 [String "three"], ReceivedEnd 2]
            resetInput defaultInputPlan
            (binItems, _) <- duplexCall (route port) "stream.audio" $ \p -> do
              ipBinary p 0 (BS.pack [1, 2, 3])
              ipBinary p 1 (BS.replicate 640 9)
              ipEnd p 2
            any isTaken binItems `shouldBe` True
            rs' <- readIORef inputReceived
            [ (sq, BS.length b) | ReceivedBinary _ sq b <- rs' ] `shouldBe` [(0, 3), (1, 640)]
            [ sq | ReceivedEnd sq <- rs' ] `shouldBe` [2]
            readIORef inputViolations `shouldReturn` []
            writeIORef schemaOverride Nothing
    -- review of BIDIR-18: the holder reads a binary frame's line strictly
    -- and refuses one past 16 MiB, ending the call with why
    forM_ [ ("b 0 20000000", "past the"), ("b 0x5", "cannot read"), ("b 0 5 6", "cannot read") ] $ \(line, says) ->
      it ("refuses the binary frame line " <> show line <> ", ending the call with an error") $
        withHolder 30 0 $ \sock -> withMockService todaysService $ \port -> do
          writeIORef schemaOverride (Just inputSchemas)
          resetInput defaultInputPlan
          s <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
          N.connect s (N.SockAddrUnix sock)
          NB.sendAll s $ BS8.pack $ "{\"agent\":\"a1\",\"input\":true,\"host\":\"127.0.0.1\",\"port\":" <> show port
            <> ",\"path\":\"/ws\",\"tls\":false,\"ca\":[],\"headers\":[],\"method\":\"menger.call\",\"params\":{\"method\":\"stream.audio\"}}\n"
          let readUntil acc = do
                chunk <- NB.recv s 65536
                let acc' = acc <> chunk
                if BS.null chunk || any (BS8.isInfixOf "\"t\":\"error\"") (BS8.lines acc') || any (BS8.isInfixOf "\"t\":\"end\"") (BS8.lines acc')
                  then pure acc' else readUntil acc'
              readSub acc = do
                chunk <- NB.recv s 65536
                let acc' = acc <> chunk
                if BS.null chunk || BS8.isInfixOf "\"t\":\"sub\"" acc' then pure acc' else readSub acc'
          _ <- readSub ""
          NB.sendAll s (BS8.pack (line <> "\n"))
          out <- readUntil ""
          N.close s
          let errs = [ l | l <- BS8.lines out, BS8.isInfixOf "\"t\":\"error\"" l ]
          map BS8.unpack errs `shouldSatisfy` any (says `isInfixOf`)
          rs <- readIORef inputReceived
          [ r | r@ReceivedBinary {} <- rs ] `shouldBe` []
          writeIORef schemaOverride Nothing
    carries "through the holder" $ \port half -> \onSub m ps onItem -> withHolderOnce $ \sock ->
      Hold.holderCallDuplex sock "a1" Nothing (const (pure ())) (serviceAt port) half onSub m ps onItem
    carries "on the run's own connection (--no-hold)" $ \port half -> \onSub m ps onItem -> do
      mux <- Hold.newMux
      Hold.muxCallDuplex mux (serviceAt port) half onSub m ps onItem

  describe "one socket for the user" $ do
    -- RETURNS-36: 4.0 changes the items the holder relays (plexus 0.8's
    -- typed error and progress), so its holder is protocol 3, apart from
    -- 3.17.x's (2)
    it "is holder-4.sock in the runtime directory, whatever the address or agent" $ do
      p <- Hold.holderSocketPath
      takeFileName p `shouldBe` "holder-4.sock"
      Hold.holdProtocol `shouldBe` 4

  describe "the CLI, sending first" $ do
    synapse <- runIO (findExecutable "synapse")
    case synapse of
      Nothing -> it "(the synapse executable is not on PATH)" $ pendingWith "cabal test puts it there"
      Just exe -> cliSpec exe

  describe "one holder for the user (SYNAPSE-40)" $ do
    synapse <- runIO (findExecutable "synapse")
    case synapse of
      Nothing -> it "(the synapse executable is not on PATH)" $ pendingWith "cabal test puts it there"
      Just exe -> oneHolderSpec exe

cliSpec :: FilePath -> Spec
cliSpec exe = do
  it "is this build" $ do
    (_, out, _) <- readCreateProcessWithExitCode (proc exe ["--version"]) ""
    out `shouldStartWith` "4.0"

  -- re-review #5: help names the holder's socket as it is
  it "help names holder-4.sock" $ do
    (_, out, err) <- readCreateProcessWithExitCode (proc exe ["--help"]) ""
    (out <> err) `shouldContain` "holder-4.sock"
    (out <> err) `shouldNotContain` "holder-3.sock"

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

  it "falls back to the probe and the fetch for a method the service renamed, then answers" $
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
      -- and the call after it is warm again, for what the service has now
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
    withRun = withRunOn todaysService
    -- a run of the built synapse against a fresh mock service, with a home,
    -- cache and runtime directory of its own (no keychain, a short hold)
    withRunOn sc act = withSystemTempDirectory "synapse28" $ \dir -> withMockService sc $ \port -> do
      forM_ ["home", "cache", "run"] $ \d -> createDirectoryIfMissing True (dir </> d)
      env0 <- getEnvironment
      let env = [ (k, v) | (k, v) <- env0, k `notElem` ["HOME", "XDG_CACHE_HOME", "XDG_RUNTIME_DIR", "SYNAPSE_AS", "PLEXUS_SESSION"] ]
              ++ [ ("HOME", dir </> "home"), ("XDG_CACHE_HOME", dir </> "cache"), ("XDG_RUNTIME_DIR", dir </> "run")
                 , ("PLEXUS_KEYCHAIN", "off"), ("SYNAPSE_HOLD_FOR", "3s") ]
          run' extra args = readCreateProcessWithExitCode
            (proc exe (extra ++ ["--url", "ws://127.0.0.1:" <> show port <> "/ws", "menger"] ++ args)) { env = Just env } ""
      writeIORef renamedGet False
      act (run' []) run' `finally` stopHolders dir

-- | SYNAPSE-40: the built synapse, every run as the user it is, through one
-- holder (each test a home and runtime directory of its own, the holder told
-- to wait 60 s, so what it lets go it lets go by itself).
oneHolderSpec :: FilePath -> Spec
oneHolderSpec exe = do
  it "runs one holder process however many addresses and agents call, with a connection for each" $
    withHome [] $ \dir env -> withServices 3 $ \ps -> do
      forM_ ps $ \p -> forM_ ["a1", "a2"] $ \a -> do
        (code, out, _) <- runIn exe env ["--as", a] p
        code `shouldBe` ExitSuccess
        out `shouldContain` "MENGER-12"
      length <$> holdersUnder dir `shouldReturn` 1
      st <- holderStatusIn dir
      fmap (sort . heldOf) st `shouldBe` Right (sort [ (p, a) | p <- ps, a <- ["a1", "a2"] ])

  it "never lets two agents share a connection to one address" $
    withHome [] $ \dir env -> withMockService todaysService $ \p -> do
      c0 <- readIORef connectionsAccepted
      forM_ ["a1", "a2", "a1", "a2"] $ \a -> do
        (code, _, _) <- runIn exe env ["--as", a] p
        code `shouldBe` ExitSuccess
      c1 <- readIORef connectionsAccepted
      (c1 - c0) `shouldBe` 2
      st <- holderStatusIn dir
      fmap (sort . heldOf) st `shouldBe` Right [(p, "a1"), (p, "a2")]

  it "lets a connection go within 10 s of its server stopping, and keeps the others" $
    withHome [] $ \dir env -> withMockService todaysService $ \live -> do
      (code0, _, _) <- runIn exe env [] live
      code0 `shouldBe` ExitSuccess
      forM_ [1 .. 3 :: Int] $ \_ -> withEphemeralService todaysService $ \p -> do
        (code, out, _) <- runIn exe env [] p
        code `shouldBe` ExitSuccess
        out `shouldContain` "MENGER-12"
      -- all three have stopped: within 10 s the holder holds the live one alone
      st <- waitStatus 10 (\v -> map fst (heldOf v) == [live]) dir
      fmap (map fst . heldOf) st `shouldBe` Right [live]
      length <$> holdersUnder dir `shouldReturn` 1

  it "holds no more than its cap, closing the longest idle first" $
    withHome [("SYNAPSE_HOLD_MAX", "3")] $ \dir env -> withServices 6 $ \ps -> case ps of
      [s1, s2, s3, s4, s5, s6] -> do
        let call p = runIn exe env [] p >>= \(code, _, _) -> code `shouldBe` ExitSuccess
            held = fmap (sort . map fst . heldOf) <$> holderStatusIn dir
        mapM_ call [s1, s2, s3, s4, s5]
        held `shouldReturn` Right (sort [s3, s4, s5])
        call s3   -- s3 is the one used last now
        call s6   -- and s4 the one idle longest
        held `shouldReturn` Right (sort [s3, s5, s6])
      _ -> expectationFailure "six services"

  it "puts its own holder in the place of an older one, never of a newer one" $ do
    (_, v, _) <- readCreateProcessWithExitCode (proc exe ["--version"]) ""
    let ours = takeWhile (/= '\n') v
        sockIn dir = dir </> "run" </> "synapse" </> ("holder-" <> show Hold.holdProtocol <> ".sock")
        versionIn dir = fmap (\st -> case st of { Object o | Just (String x) <- KM.lookup "version" o -> T.unpack x; _ -> "" }) <$> holderStatusIn dir
    -- an older holder (3.16.0, here in this process) is drained and replaced by the run's
    withHome [] $ \dir env -> withMockService todaysService $ \port -> do
      _ <- forkIO $ Hold.runHolder (Hold.holdOpts (sockIn dir) 30) { Hold.hoVersion = "3.16.0" } `catchAll` const (pure ())
      waitFor (doesFileExist (sockIn dir))
      (code, out, _) <- runIn exe env [] port
      (code, "MENGER-12" `isInfixOf` out) `shouldBe` (ExitSuccess, True)
      versionIn dir `shouldReturn` Right ours
      hs <- holdersUnder dir
      length hs `shouldBe` 1
      -- and the next run keeps it: no turns taken
      (code', _, _) <- runIn exe env [] port
      code' `shouldBe` ExitSuccess
      holdersUnder dir `shouldReturn` hs
    -- a newer one (99.0.0) serves the run and stays
    withHome [] $ \dir env -> withMockService todaysService $ \port -> do
      _ <- forkIO $ Hold.runHolder (Hold.holdOpts (sockIn dir) 30) { Hold.hoVersion = "99.0.0" } `catchAll` const (pure ())
      waitFor (doesFileExist (sockIn dir))
      (code, out, _) <- runIn exe env [] port
      (code, "MENGER-12" `isInfixOf` out) `shouldBe` (ExitSuccess, True)
      versionIn dir `shouldReturn` Right "99.0.0"
      holdersUnder dir `shouldReturn` []

  -- RETURNS-36: a 3.x holder (protocol 2, holder-2.sock) is neither used nor
  -- replaced by 4.0: the run holds on a socket of its own protocol, and the
  -- 3.x holder is left as it was, for the 3.x runs
  it "never reuses nor replaces a 3.x holder (protocol 2)" $
    withHome [] $ \dir env -> withMockService todaysService $ \port -> do
      let old = dir </> "run" </> "synapse" </> "holder-2.sock"
      _ <- forkIO $ Hold.runHolder (Hold.holdOpts old 30) { Hold.hoVersion = "3.17.4" } `catchAll` const (pure ())
      waitFor (doesFileExist old)
      (code, out, _) <- runIn exe env [] port
      (code, "MENGER-12" `isInfixOf` out) `shouldBe` (ExitSuccess, True)
      let version = fmap (\st -> case st of { Object o | Just (String x) <- KM.lookup "version" o -> T.unpack x; _ -> "" })
      version <$> askStatus old `shouldReturn` Right "3.17.4"
      fmap heldOf <$> askStatus old `shouldReturn` Right []
      doesFileExist (dir </> "run" </> "synapse" </> ("holder-" <> show Hold.holdProtocol <> ".sock")) `shouldReturn` True

  it "starts its holder in /, without the run's keepalive or secrets" $
    withHome [("PLEXUS_KEEPALIVE", "off"), ("SYNAPSE40_MARK", "kept")] $ \dir env -> withMockService todaysService $ \port -> do
      (code, _, _) <- runIn exe env [] port
      code `shouldBe` ExitSuccess
      hs <- holdersUnder dir
      case hs of
        [h] -> do
          environ <- map BS8.unpack . BS.split 0 <$> BS.readFile ("/proc" </> show h </> "environ")
          getSymbolicLinkTarget ("/proc" </> show h </> "cwd") `shouldReturn` "/"
          any ("PLEXUS_KEEPALIVE=" `isPrefixOf`) environ `shouldBe` False
          ("SYNAPSE40_MARK=kept" `elem` environ) `shouldBe` True
        other -> expectationFailure ("holders: " <> show other)

  it "keeps a live connection for about 100 kB (what its cap is set by)" $ do
    n <- maybe 100 read <$> lookupEnv "SYNAPSE40_HELD"
    withHome [("SYNAPSE_HOLD_MAX", "0"), ("SYNAPSE_HOLD_FOR", "600s")] $ \dir env -> withServices (n + 3) $ \ps -> do
      let call p = runIn exe env [] p >>= \(code, _, _) -> code `shouldBe` ExitSuccess
          (warm, rest) = splitAt 3 ps
      mapM_ call warm
      hs <- holdersUnder dir
      case hs of
        [h] -> do
          r0 <- rssKb h
          mapM_ call rest
          r1 <- rssKb h
          st <- holderStatusIn dir
          fmap (length . heldOf) st `shouldBe` Right (n + 3)
          let per = (r1 - r0) `div` n
          putStrLn $ "      " <> show (n + 3) <> " connections held: " <> show r0 <> " kB with 3, "
            <> show r1 <> " kB with " <> show (n + 3) <> ": about " <> show per <> " kB each"
          per `shouldSatisfy` (< 200)
        other -> expectationFailure ("holders: " <> show other)

  it "runs a loop of calls against short-lived servers through one holder, which holds nothing 10 s on, its memory flat" $ do
    n <- maybe 500 read <$> lookupEnv "SYNAPSE40_LOOP"
    withHome [] $ \dir env -> do
      seen <- newIORef []
      samples <- newIORef []
      t0 <- getMonotonicTime
      forM_ [1 .. n :: Int] $ \i -> do
        withEphemeralService todaysService $ \p -> do
          (code, out, err) <- runIn exe env [] p
          when (code /= ExitSuccess || not ("MENGER-12" `isInfixOf` out)) $
            expectationFailure ("call " <> show i <> ": " <> show code <> " " <> err)
        hs <- holdersUnder dir
        modifyIORef' seen (nub . (hs ++))
        when (i `mod` max 1 (n `div` 10) == 0) $ case hs of
          [h] -> do
            rss <- rssKb h
            st <- holderStatusIn dir
            modifyIORef' samples (++ [(i, rss, either (const (-1)) (length . heldOf) st)])
          _ -> pure ()
      t1 <- getMonotonicTime
      pids <- readIORef seen
      length pids `shouldBe` 1
      st <- waitStatus 10 (null . heldOf) dir
      fmap heldOf st `shouldBe` Right []
      ss <- readIORef samples
      putStrLn $ "      " <> show n <> " calls against " <> show n <> " servers in "
        <> show (round (t1 - t0) :: Int) <> " s; holder " <> show pids
        <> "; (call, RSS kB, connections held): " <> show ss
      case ss of
        (_ : (_, early, _) : _) -> do
          let (_, late, _) = last ss
          -- flat: what it had grown to by a fifth of the way, within 4 MB, at the end
          late `shouldSatisfy` (<= early + 4096)
        _ -> pure ()
      -- never more held than the servers that stopped in its last second or so
      maximum (0 : [ h | (_, _, h) <- ss ]) `shouldSatisfy` (<= 20)

-- | SYNAPSE-40: a home, a cache and a runtime directory of the test's own,
-- and the environment a run is given there (no keychain, a holder that
-- waits 60 s unless told otherwise); every holder a run started there is
-- stopped by pid when the action ends.
withHome :: [(String, String)] -> (FilePath -> [(String, String)] -> IO a) -> IO a
withHome extra act = withSystemTempDirectory "synapse40" $ \dir -> do
  forM_ ["home", "cache", "run"] $ \d -> createDirectoryIfMissing True (dir </> d)
  env0 <- getEnvironment
  let ours = ["HOME", "XDG_CACHE_HOME", "XDG_RUNTIME_DIR", "SYNAPSE_AS", "PLEXUS_SESSION"
             , "SYNAPSE_NO_HOLD", "SYNAPSE_HOLD_FOR", "SYNAPSE_HOLD_MAX"]
      env = [ (k, v) | (k, v) <- env0, k `notElem` ours ]
         ++ [ ("HOME", dir </> "home"), ("XDG_CACHE_HOME", dir </> "cache"), ("XDG_RUNTIME_DIR", dir </> "run")
            , ("PLEXUS_KEYCHAIN", "off") ]
         ++ [ ("SYNAPSE_HOLD_FOR", "60s") | "SYNAPSE_HOLD_FOR" `notElem` map fst extra ]
         ++ extra
  act dir env `finally` stopHolders dir

-- | One run of the built synapse: @tickets get@ at the service on the port.
runIn :: FilePath -> [(String, String)] -> [String] -> Int -> IO (ExitCode, String, String)
runIn exe env extra port = readCreateProcessWithExitCode
  (proc exe (extra ++ ["--url", "ws://127.0.0.1:" <> show port <> "/ws", "menger", "tickets", "get", "--id", "MENGER-12"]))
    { env = Just env } ""

-- | Services on fresh ports, all up for the action.
withServices :: Int -> ([Int] -> IO a) -> IO a
withServices 0 act = act []
withServices k act = withMockService todaysService $ \p -> withServices (k - 1) (act . (p :))

-- | The holder processes (pids) whose socket lies under the directory.
holdersUnder :: FilePath -> IO [Int]
holdersUnder dir = do
  pids <- filter (all isDigit) <$> listDirectory "/proc"
  fmap concat $ forM pids $ \p -> do
    r <- try (BS.readFile ("/proc" </> p </> "cmdline"))
    pure $ case r of
      Left (_ :: SomeException) -> []
      Right bs ->
        let args = map BS8.unpack (BS.split 0 bs)
        in [ read p | "_hold" `elem` args, any (dir `isInfixOf`) args ]

-- | Stop every holder under the directory, by pid: SIGTERM, then SIGKILL
-- for any still there 3 s on.
stopHolders :: FilePath -> IO ()
stopHolders dir = do
  ps <- holdersUnder dir
  forM_ ps $ \p -> void (try (signalProcess sigTERM (fromIntegral p)) :: IO (Either SomeException ()))
  let alive p = doesDirectoryExist ("/proc" </> show p)
      wait (0 :: Int) = pure ()
      wait k = do
        left <- filterM alive ps
        if null left then pure () else threadDelay 50000 >> wait (k - 1)
  wait 60
  left <- holdersUnder dir
  forM_ left $ \p -> void (try (signalProcess sigKILL (fromIntegral p)) :: IO (Either SomeException ()))

-- | A process's resident memory, in kB.
rssKb :: Int -> IO Int
rssKb pid = do
  s <- BS8.unpack <$> BS.readFile ("/proc" </> show pid </> "status")
  pure $ case [ read (words l !! 1) | l <- lines s, "VmRSS:" `isPrefixOf` l ] of
    (kb : _) -> kb
    [] -> 0

-- | What the holder under the directory says it holds: its one socket,
-- asked for its status.
holderStatusIn :: FilePath -> IO (Either String Value)
holderStatusIn dir = do
  let sdir = dir </> "run" </> "synapse"
  r <- try (listDirectory sdir)
  case filter (".sock" `isSuffixOf`) (either (\(_ :: SomeException) -> []) id r) of
    [one] -> askStatus (sdir </> one)
    other -> pure (Left ("not one holder's socket: " <> show other))

-- | The holder's status: @{"status":true}@ sent, a line of JSON back,
-- @{"t":"status", "held": [{"host", "port", "agent", …}, …], …}@.
askStatus :: FilePath -> IO (Either String Value)
askStatus path = do
  r <- try $ bracket (N.socket N.AF_UNIX N.Stream N.defaultProtocol) N.close $ \s -> do
    N.connect s (N.SockAddrUnix path)
    NB.sendAll s "{\"status\":true}\n"
    let go acc = do
          chunk <- NB.recv s 65536
          if BS.null chunk || BS8.elem '\n' chunk then pure (acc <> chunk) else go (acc <> chunk)
    BS8.takeWhile (/= '\n') <$> go BS.empty
  pure $ case r of
    Left (e :: SomeException) -> Left (show e)
    Right line -> case decodeStrict line of
      Just v@(Object o) | KM.lookup "t" o == Just (String "status") -> Right v
      _ -> Left ("not a status: " <> BS8.unpack line)

-- | Ask until the status satisfies the test, or the seconds run out; the
-- last answer.
waitStatus :: Double -> (Value -> Bool) -> FilePath -> IO (Either String Value)
waitStatus secs ok dir = do
  t0 <- getMonotonicTime
  let go = do
        st <- holderStatusIn dir
        now <- getMonotonicTime
        case st of
          Right v | ok v -> pure st
          _ | now - t0 >= secs -> pure st
            | otherwise -> threadDelay 200000 >> go
  go

-- | The connections a status says are held: (port, agent) each.
heldOf :: Value -> [(Int, T.Text)]
heldOf (Object o) | Just (Array hs) <- KM.lookup "held" o =
  [ (round n, a) | Object h <- V.toList hs
                 , Just (Number n) <- [KM.lookup "port" h], Just (String a) <- [KM.lookup "agent" h] ]
heldOf _ = []

-- | A holder in this process for the action: its socket, waiting the
-- seconds, keeping at most the cap (0: none).
withHolder :: Double -> Int -> (FilePath -> IO a) -> IO a
withHolder secs cap act = withSystemTempDirectory "hold" $ \dir -> do
  let sock = dir </> "h.sock"
  _ <- forkIO $ Hold.runHolder (Hold.holdOpts sock secs) { Hold.hoCap = cap } `catchAll` const (pure ())
  waitFor (doesFileExist sock)
  act sock

-- | BIDIR-18: one call with an input half on the route: once the port is
-- handed over, @feed@ sends its frames; the items and the signals, as they
-- came.
duplexCall :: (Maybe InputHalf -> IO () -> T.Text -> Value -> (PlexusStreamItem -> IO ()) -> IO ())
           -> T.Text -> (InputPort -> IO ()) -> IO ([PlexusStreamItem], [InputSignal])
duplexCall route inner feed = do
  items <- newIORef []
  sigs <- newIORef []
  let half = InputHalf
        { ihPort = \p -> void (forkIO (threadDelay 50000 >> feed p))
        , ihSignal = \g -> modifyIORef' sigs (++ [g]) }
  route (Just half) (pure ()) "menger.call" (Object (KM.fromList [("method", String inner)])) (\i -> modifyIORef' items (++ [i]))
  (,) <$> readIORef items <*> readIORef sigs

isTaken :: PlexusStreamItem -> Bool
isTaken (StreamData _ _ _ (Object o)) = KM.lookup "type" o == Just (String "taken")
isTaken _ = False

-- | A holder in this process for one call.
withHolderOnce :: (FilePath -> IO a) -> IO a
withHolderOnce = withHolder 30 0

-- ---------------------------------------------------------------------------

serviceAt :: Int -> SubstrateConfig
serviceAt port = (defaultConfig "menger") { substratePort = port, substratePath = "/ws" }

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
