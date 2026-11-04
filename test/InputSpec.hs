{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | BIDIR-18: synapse calls a method with an input half (plexus-wire
-- spec/10). The built synapse, end to end against the mock service's
-- @stream@ activation (a home, a cache and a runtime directory of the
-- test's own), the frames counted where they land:
--
-- * lines as string items, NDJSON as object items, a bad line named by its
--   number and never carried;
-- * stdin not read past the credit (a service granting 4 and never acking),
--   and synapse's memory flat while the writer runs ahead;
-- * bytes as binary frames — header, payload, @--chunk@, the call's seq;
-- * a disconnect said on stderr with the counts, exit 69, the call never
--   made again;
-- * the typed closes' exits: 70, 65, 77;
-- * a parameter named @input@ keeps its flag (the stream is
--   @--input-stream@); no stream given is refused before anything is sent;
--   @-n@ prints the open frame and the first input frame.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, finally, try)
import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.IORef
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, findExecutable, listDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hClose, hSetBinaryMode, hSetBuffering, BufferMode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals (signalProcess, sigKILL, sigTERM)
import System.Process
import Test.Hspec

import MockService

main :: IO ()
main = hspec $ do
  synapse <- runIO (findExecutable "synapse")
  case synapse of
    Nothing -> it "(the synapse executable is not on PATH)" $ pendingWith "cabal test puts it there"
    Just exe -> inputSpec exe

inputSpec :: FilePath -> Spec
inputSpec exe = describe "a call with an input half (BIDIR-18)" $ do
  forM_ [(["--no-hold"], "on its own socket"), ([], "through the holder")] $ \(how, label) -> describe label $ do
    it "sends lines as string items, one a line, an empty one too" $ withRun $ \run dir -> do
      writeFile (dir </> "log.txt") "first\n\nthird\n"
      (code, out, err) <- run how ["stream", "lines", "--input", "@" <> dir </> "log.txt"] ""
      code `shouldBe` ExitSuccess
      out `shouldContain` "items: 3"
      err `shouldContain` "input: sent 3 items in 1 frame"
      itemsReceived `shouldReturn` [String "first", String "", String "third"]
      ends `shouldReturn` [1]
      readIORef inputViolations `shouldReturn` []
      -- through the holder: a holder process under this test's runtime directory
      holders <- holdersUnder dir
      (if null how then length holders `shouldBe` 1 else holders `shouldBe` [])

    it "sends NDJSON as object items, in order, under credit (10,000 from stdin)" $ withRun $ \run _ -> do
      let items = [ object ["at" .= i, "pad" .= ("p" <> show i)] | i <- [1 .. 10000 :: Int] ]
      (code, out, err) <- run how ["stream", "ingest", "--input", "-"] (unlines (map (LBS8.unpack . encode) items))
      code `shouldBe` ExitSuccess
      out `shouldContain` "items: 10000"
      err `shouldContain` "input: sent 10,000 items"
      err `shouldContain` "0 unacked"
      itemsReceived `shouldReturn` items
      readIORef inputViolations `shouldReturn` []

    it "sends bytes as binary frames: the header, --chunk bytes each, seq counting every frame" $ withRun $ \run dir -> do
      let bytes = BS.pack [0 .. 22]
      BS.writeFile (dir </> "audio.raw") bytes
      (code, _, err) <- run how ["stream", "audio", "--input", "@" <> dir </> "audio.raw", "--chunk", "5"] ""
      (code, err) `shouldSatisfy` ((== ExitSuccess) . fst)
      rs <- readIORef inputReceived
      let bins = [ (sub, sq, p) | ReceivedBinary sub sq p <- rs ]
      map (\(_, sq, _) -> sq) bins `shouldBe` [0 .. 4]
      map (\(_, _, p) -> BS.length p) bins `shouldBe` [5, 5, 5, 5, 3]
      BS.concat [ p | (_, _, p) <- bins ] `shouldBe` bytes
      length (filter (\(sub, _, _) -> sub == firstSub bins) bins) `shouldBe` 5
      [ sq | ReceivedEnd sq <- rs ] `shouldBe` [5]
      readIORef inputViolations `shouldReturn` []

  it "names a line that is not JSON by its number; no frame carries it, and the call ends (65)" $ withRun $ \run dir -> do
    writeFile (dir </> "bad.jsonl") $ unlines
      [ "{\"at\":1,\"pad\":\"a\"}", "{\"at\":2,\"pad\":\"b\"}", "{\"at\":3,", "{\"at\":4,\"pad\":\"d\"}" ]
    (code, _, err) <- run ["--no-hold"] ["stream", "ingest", "--input", "@" <> dir </> "bad.jsonl"] ""
    code `shouldBe` ExitFailure 65
    err `shouldContain` "input line 3 is not an item"
    got <- itemsReceived
    got `shouldNotSatisfy` any (\v -> v == object ["at" .= (4 :: Int), "pad" .= ("d" :: Text)])
    length got `shouldSatisfy` (<= 2)
    ends `shouldReturn` []

  it "reads no more of stdin than the credit allows, its memory flat (a service granting 4, never acking)" $
    withRunPlan defaultInputPlan { planWindow = 4, planStingy = True } $ \_ _ env port -> do
      written <- newIORef (0 :: Int)
      (Just hin, Just hout, Just herr, ph) <- createProcess (proc exe (["--no-hold", "--url", "ws://127.0.0.1:" <> show port <> "/ws", "menger", "stream", "lines", "--input", "-"]))
        { env = Just env, std_in = CreatePipe, std_err = CreatePipe, std_out = CreatePipe }
      hSetBinaryMode hin True
      hSetBuffering hin NoBuffering
      let line = BS8.pack (replicate 1023 'x' <> "\n")
      _ <- forkIO $ (let go = BS.hPut hin line >> modifyIORef' written (+ 1) >> go in go)
                      `catchAny` \_ -> pure ()
      Just pid <- getPid ph
      threadDelay 1500000
      alive <- getProcessExitCode ph
      case alive of
        Just c -> do
          e <- BS.hGetContents herr
          expectationFailure ("synapse ended (" <> show c <> "): " <> BS8.unpack e)
        Nothing -> pure ()
      rss1 <- rssKb (show pid)
      threadDelay 3000000
      rss2 <- rssKb (show pid)
      n <- readIORef written
      got <- itemsReceived
      signalProcess sigTERM pid `catchAny` \_ -> pure ()
      _ <- waitForProcess ph
      mapM_ (\h -> hClose h `catchAny` \_ -> pure ()) [herr, hout]
      putStrLn ("      " <> show n <> " lines of 1 KiB written in 4.5 s; synapse RSS " <> show rss1 <> " kB at 1.5 s, " <> show rss2 <> " kB at 4.5 s")
      length got `shouldBe` 4
      -- 4 items, the read buffer synapse holds (32 KiB), and the pipe's
      -- 64 KiB: nothing like the writer's endless stream
      n `shouldSatisfy` (< 4 + 32 + 64 + 16)
      abs (rss2 - rss1) `shouldSatisfy` (<= 1024)
      readIORef inputViolations `shouldReturn` []

  it "says a disconnect on stderr with what was sent and acked, exits 69, and never makes the call again" $
    withRunPlan defaultInputPlan { planDropAfter = Just 3 } $ \run dir _ _ -> do
      writeFile (dir </> "many.jsonl") $ unlines [ "{\"at\":" <> show i <> ",\"pad\":\"x\"}" | i <- [1 .. 5000 :: Int] ]
      writeIORef callsMade []
      (code, _, err) <- run ["--no-hold"] ["stream", "ingest", "--input", "@" <> dir </> "many.jsonl"] ""
      code `shouldBe` ExitFailure 69
      err `shouldContain` "lost during the call"
      err `shouldContain` "not sent again"
      err `shouldContain` "input: sent "
      err `shouldContain` "unacked"
      calls <- readIORef callsMade
      length (filter (== "stream.ingest") calls) `shouldBe` 1

  forM_ [ ("past_credit", 70, "a bug in synapse"), ("invalid_input", 65, "refused the input (invalid_input) at frame 0")
        , ("too_much_input", 65, "too_much_input"), ("out_of_sequence", 65, "out_of_sequence") ] $ \(kind, exit, says) ->
    it ("exits " <> show exit <> " when the service closes the input with " <> T.unpack kind) $
      withRunPlan defaultInputPlan { planRefuse = Just (1, kind) } $ \run dir _ _ -> do
        writeFile (dir </> "two.txt") "a\nb\n"
        (code, _, err) <- run ["--no-hold"] ["stream", "lines", "--input", "@" <> dir </> "two.txt"] ""
        code `shouldBe` ExitFailure exit
        err `shouldContain` says
        err `shouldContain` "input: sent"

  it "exits 77 when the service closes the input for a revoked sign-in" $
    withRunPlan defaultInputPlan { planClose = Just (1, "revoked") } $ \run dir _ _ -> do
      writeFile (dir </> "two.txt") "a\nb\n"
      (code, _, err) <- run ["--no-hold"] ["stream", "lines", "--input", "@" <> dir </> "two.txt"] ""
      code `shouldBe` ExitFailure 77
      err `shouldContain` "revoked"

  it "keeps --input for a parameter named input; the stream is --input-stream" $ withRun $ \run dir -> do
    writeFile (dir </> "log.txt") "one\ntwo\n"
    (code, out, _) <- run ["--no-hold"] ["stream", "tagged", "--input", "t1", "--input-stream", "@" <> dir </> "log.txt"] ""
    code `shouldBe` ExitSuccess
    out `shouldContain` "tag: t1"
    itemsReceived `shouldReturn` [String "one", String "two"]

  it "refuses a call with no stream named, before anything is sent, naming the flag" $ withRun $ \run _ -> do
    writeIORef callsMade []
    (code, _, err) <- run ["--no-hold"] ["stream", "lines"] ""
    code `shouldNotBe` ExitSuccess
    err `shouldContain` "--input -"
    calls <- readIORef callsMade
    filter (\c -> "stream." `T.isPrefixOf` c && not (".schema" `T.isSuffixOf` c)) calls `shouldBe` []

  it "-n prints the open frame and the first input frame, and sends nothing" $ withRun $ \run dir -> do
    writeFile (dir </> "log.txt") "hello\nworld\n"
    writeIORef callsMade []
    (code, out, _) <- run ["--no-hold", "-n"] ["stream", "lines", "--input", "@" <> dir </> "log.txt"] ""
    code `shouldBe` ExitSuccess
    case lines out of
      [open, first] -> do
        open `shouldContain` "\"method\":\"menger.call\""
        first `shouldContain` "\"method\":\"menger.input\""
        first `shouldContain` "\"items\":[\"hello\"]"
      other -> expectationFailure ("two lines, not " <> show other)
    BS.writeFile (dir </> "a.raw") (BS.replicate 700 7)
    (_, out', _) <- run ["--no-hold", "-n"] ["stream", "audio", "--input", "@" <> dir </> "a.raw", "--chunk", "640"] ""
    out' `shouldContain` "binary: 01 0000000000000000 0000000000000000 + 640 bytes"
    calls <- readIORef callsMade
    filter (\c -> "stream." `T.isPrefixOf` c && not (".schema" `T.isSuffixOf` c)) calls `shouldBe` []

  it "--help says the input: its item type, binary or not, --input and --chunk" $ withRun $ \run _ -> do
    (_, out, _) <- run ["--no-hold"] ["stream", "audio", "--help"] ""
    out `shouldContain` "--input <-|@FILE>"
    out `shouldContain` "bytes items"
    out `shouldContain` "binary frame"
    out `shouldContain` "[--chunk <bytes>]"
    (_, out2, _) <- run ["--no-hold"] ["stream", "ingest", "--help"] ""
    out2 `shouldContain` "Item items"
    out2 `shouldContain` "NDJSON"
    (_, found, _) <- run ["--no-hold"] ["_find", "raw", "bytes"] ""
    found `shouldContain` "takes a stream in: --input -|@FILE"
  where
    withRun act = withRunPlan defaultInputPlan (\run dir _ _ -> act run dir)
    withRunPlan plan act = withSystemTempDirectory "b18" $ \dir -> withMockService todaysService $ \port -> do
      forM_ ["home", "cache", "run"] $ \d -> createDirectoryIfMissing True (dir </> d)
      env0 <- getEnvironment
      let env = [ (k, v) | (k, v) <- env0, k `notElem` ["HOME", "XDG_CACHE_HOME", "XDG_RUNTIME_DIR", "SYNAPSE_AS", "PLEXUS_SESSION", "SYNAPSE_NO_HOLD"] ]
              ++ [ ("HOME", dir </> "home"), ("XDG_CACHE_HOME", dir </> "cache"), ("XDG_RUNTIME_DIR", dir </> "run")
                 , ("PLEXUS_KEYCHAIN", "off"), ("SYNAPSE_HOLD_FOR", "5s"), ("SYNAPSE_RECONNECT_FOR", "3s") ]
          url = ["--url", "ws://127.0.0.1:" <> show port <> "/ws"]
          run' globals args input = readCreateProcessWithExitCode
            (proc exe (globals ++ url ++ case args of
                         ("_find" : rest) -> "_find" : "menger" : rest
                         _ -> "menger" : args)) { env = Just env } input
      writeIORef schemaOverride (Just inputSchemas)
      resetInput plan
      (act run' dir env port `finally` (writeIORef schemaOverride Nothing >> stopHolders dir))

-- | The items every JSON frame carried, in order.
itemsReceived :: IO [Value]
itemsReceived = do
  rs <- readIORef inputReceived
  pure (concat [ is | ReceivedJson _ is <- rs ])

-- | The seq of every @end@.
ends :: IO [Integer]
ends = do
  rs <- readIORef inputReceived
  pure [ toInteger sq | ReceivedEnd sq <- rs ]

firstSub :: [(a, b, c)] -> a
firstSub ((s, _, _) : _) = s
firstSub [] = error "no binary frame"

-- | A process's resident memory, in KiB (VmRSS).
rssKb :: String -> IO Int
rssKb pid = do
  st <- BS8.readFile ("/proc" </> pid </> "status")
  case [ l | l <- BS8.lines st, "VmRSS:" `BS8.isPrefixOf` l ] of
    (l : _) -> pure (read (BS8.unpack (BS8.words l !! 1)))
    [] -> fail ("no VmRSS for " <> pid <> ": " <> BS8.unpack st)

-- | The holder processes whose socket lies under the directory.
holdersUnder :: FilePath -> IO [String]
holdersUnder dir = do
  pids <- filter (all (`elem` ['0' .. '9'])) <$> listDirectory "/proc"
  fmap concat $ mapM (\p -> do
    r <- try (BS.readFile ("/proc" </> p </> "cmdline")) :: IO (Either SomeException BS.ByteString)
    pure [ p | Right bs <- [r], let args = map BS8.unpack (BS.split 0 bs), "_hold" `elem` args, any (dir `isInfixOf`) args ]) pids

-- | Stop every holder whose socket lies under the directory.
stopHolders :: FilePath -> IO ()
stopHolders dir = do
  pids <- filter (all (`elem` ['0' .. '9'])) <$> listDirectory "/proc"
  forM_ pids $ \p -> do
    r <- try (BS.readFile ("/proc" </> p </> "cmdline")) :: IO (Either SomeException BS.ByteString)
    case r of
      Right bs | let args = map BS8.unpack (BS.split 0 bs), "_hold" `elem` args, any (dir `isInfixOf`) args ->
        signalProcess sigKILL (read p) `catchAny` \_ -> pure ()
      _ -> pure ()

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny act h = try act >>= either h pure
