{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SYNAPSE-39: @synapse -o FILE@ writes the call's answer to FILE, whole,
-- and nothing to stdout. The built synapse, end to end against the mock
-- service (a home, a cache and a runtime directory of the test's own):
--
-- * an answer in FILE is byte for byte what stdout would have had (plain
--   and @-j@), and stdout is empty;
-- * a call that fails leaves no FILE (an old one is gone too) and no
--   temporary file beside it, and exits with its own code (a lost write
--   still 75);
-- * a stream's items are in FILE as they come;
-- * FILE is 0600; @-o -@ is stdout;
-- * a FILE that cannot be made (no such directory, a directory) is said at
--   once, and the call is not sent;
-- * the review's (SYNAPSE-39): a failed write to FILE exits 74 (forced with
--   @ulimit -f@), a refusal under @-j@ is said on stderr, a two-way
--   request's line goes to stderr and the answer to FILE, a SIGTERM leaves
--   no temporary file.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, listDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hClose, hGetLine, hPutStrLn, hFlush)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
import System.Process
import System.Timeout (timeout)
import Test.Hspec

import MockService

main :: IO ()
main = hspec $ do
  synapse <- runIO (findExecutable "synapse")
  case synapse of
    Nothing -> it "(the synapse executable is not on PATH)" $ pendingWith "cabal test puts it there"
    Just exe -> outputSpec exe

outputSpec :: FilePath -> Spec
outputSpec exe = describe "synapse -o FILE (SYNAPSE-39)" $ do
  it "is this build" $ do
    (_, out, _) <- readProcessWithExitCode exe ["--version"] ""
    out `shouldStartWith` "3.17.3"

  forM_ [ ([], "", ["tickets", "get", "--id", "MENGER-12"])
        , (["-j"], " with -j", ["tickets", "get", "--id", "MENGER-12"])
        , ([], ", a write's answer in two parts", ["tickets", "amend", "--id", "MENGER-12", "--body", "the step"])
        , (["-j"], ", a write's answer in two parts with -j", ["tickets", "amend", "--id", "MENGER-12", "--body", "the step"]) ]
    $ \(globals, how, call) ->
   it ("an answer in FILE is what stdout would have had" <> how) $
    withRun $ \dir spawn -> do
      warm spawn
      (code0, onStdout, err0) <- reading (spawn globals) call
      (code0, err0) `shouldBe` (ExitSuccess, "")
      onStdout `shouldNotBe` ""
      let file = dir </> "answer.out"
      (code, out, err) <- reading (spawn (globals ++ ["-o", file])) call
      (code, out, err) `shouldBe` (ExitSuccess, "", "")
      inFile <- readFile file
      inFile `shouldBe` onStdout
      leftovers dir "answer.out" >>= (`shouldBe` [])

  it "--output is -o" $
    withRun $ \dir spawn -> do
      warm spawn
      let file = dir </> "answer.out"
      (code, out, _) <- reading (spawn ["--output", file]) ["tickets", "get", "--id", "MENGER-12"]
      (code, out) `shouldBe` (ExitSuccess, "")
      readFile file >>= (`shouldContain` "MENGER-12")

  it "FILE is 0600" $
    withRun $ \dir spawn -> do
      warm spawn
      let file = dir </> "answer.out"
      (code, _, _) <- reading (spawn ["-o", file]) ["tickets", "get", "--id", "MENGER-12"]
      code `shouldBe` ExitSuccess
      modeOf file >>= (`shouldBe` 0o600)

  it "-o - is stdout" $
    withRun $ \dir spawn -> do
      warm spawn
      (_, onStdout, _) <- reading (spawn []) ["tickets", "get", "--id", "MENGER-12"]
      (code, out, err) <- reading (spawn ["-o", "-"]) ["tickets", "get", "--id", "MENGER-12"]
      (code, out, err) `shouldBe` (ExitSuccess, onStdout, "")
      doesFileExist (dir </> "-") >>= (`shouldBe` False)

  it "a write lost on the socket: exit 75, sent once, no FILE and nothing beside it" $
    withRun $ \dir spawn -> do
      warm spawn
      let file = dir </> "answer.out"
      writeIORef callsMade []
      writeIORef dropNext ["tickets.amend"]
      (code, out, err) <- reading (spawn ["-o", file]) ["tickets", "amend", "--id", "MENGER-12", "--body", "the step"]
      (code, out) `shouldBe` (ExitFailure 75, "")
      err `shouldContain` "the write may have happened"
      amends <- length . filter (== "tickets.amend") <$> readIORef callsMade
      amends `shouldBe` 1
      doesFileExist file >>= (`shouldBe` False)
      leftovers dir "answer.out" >>= (`shouldBe` [])

  it "a call that fails takes an old FILE with it: no stale answer is left to read" $
    withRun $ \dir spawn -> do
      warm spawn
      let file = dir </> "answer.out"
      writeFile file "an answer from before\n"
      writeIORef dropNext ["tickets.amend"]
      (code, _, _) <- reading (spawn ["-o", file]) ["tickets", "amend", "--id", "MENGER-12", "--body", "the step"]
      code `shouldBe` ExitFailure 75
      doesFileExist file >>= (`shouldBe` False)
      leftovers dir "answer.out" >>= (`shouldBe` [])

  it "a call refused before it is sent (an unknown flag) leaves no FILE" $
    withRun $ \dir spawn -> do
      warm spawn
      let file = dir </> "answer.out"
      (code, out, _) <- reading (spawn ["-o", file]) ["tickets", "get", "--id", "MENGER-12", "--nonesuch", "x"]
      code `shouldNotBe` ExitSuccess
      out `shouldBe` ""
      doesFileExist file >>= (`shouldBe` False)
      leftovers dir "answer.out" >>= (`shouldBe` [])

  forM_ [([], ""), (["-j"], " with -j")] $ \(globals, how) ->
   it ("a stream's items are in FILE as they come, each line whole" <> how) $
    withRun $ \dir spawn -> do
      warm spawn
      let file = dir </> "ticks.out"
      (Just inp, Just out, Just err, ph) <- createProcess (spawn (globals ++ ["-o", file]) ["tickets", "flood"])
      hClose inp
      -- three ticks while the stream still runs
      got <- waitUpTo 10 $ do
        e <- doesFileExist file
        if not e then pure False else (>= 3) . length . lines <$> readFile' file
      running <- getProcessExitCode ph
      terminateProcess ph
      _ <- waitForProcess ph
      o <- BS.hGetContents out
      _ <- BS.hGetContents err
      (got, running) `shouldBe` (True, Nothing)
      BS.length o `shouldBe` 0
      body <- readFile' file
      -- what was printed is in FILE whole: it ends with its line's end
      last body `shouldBe` '\n'
      if null globals
        then body `shouldContain` "type: tick"
        else forM_ (lines body) $ \l -> (decodeStrict (BS8.pack l) :: Maybe Value) `shouldSatisfy` (/= Nothing)
      modeOf file >>= (`shouldBe` 0o600)
      leftovers dir "ticks.out" >>= (`shouldBe` [])

  it "a FILE that cannot be made is said at once, and the call is not sent" $
    withRun $ \dir spawn -> do
      warm spawn
      writeIORef callsMade []
      let file = dir </> "no-such-dir" </> "answer.out"
      (code, out, err) <- reading (spawn ["-o", file]) ["tickets", "amend", "--id", "MENGER-12", "--body", "the step"]
      (code, out) `shouldBe` (ExitFailure 73, "")
      err `shouldContain` file
      threadDelay 300000
      amends <- length . filter (== "tickets.amend") <$> readIORef callsMade
      amends `shouldBe` 0

  it "a directory as FILE is said at once, and the call is not sent" $
    withRun $ \dir spawn -> do
      warm spawn
      writeIORef callsMade []
      (code, out, err) <- reading (spawn ["-o", dir]) ["tickets", "amend", "--id", "MENGER-12", "--body", "the step"]
      (code, out) `shouldBe` (ExitFailure 73, "")
      err `shouldContain` "is a directory"
      threadDelay 300000
      amends <- length . filter (== "tickets.amend") <$> readIORef callsMade
      amends `shouldBe` 0
      listDirectory dir >>= (`shouldBe` ["cache", "home", "run"]) . sort

  it "a write to FILE that fails (a file size limit) exits 74, said once, the call unretried" $
    withRun $ \dir spawn -> do
      warm spawn
      writeIORef callsMade []
      let file = dir </> "ticks.out"
          limited args = let p = spawn ["-j", "-o", file] args in case cmdspec p of
            RawCommand exe' as -> p { cmdspec = RawCommand "/bin/sh" (["-c", "trap '' XFSZ; ulimit -f 1; exec \"$0\" \"$@\"", exe'] ++ as) }
            ShellCommand _ -> p
      (code, out, err) <- reading limited ["tickets", "flood"]
      (code, out) `shouldBe` (ExitFailure 74, "")
      err `shouldContain` ("cannot write " <> file)
      err `shouldNotContain` "hFlush"
      err `shouldNotContain` "reconnect"
      floods <- length . filter (== "tickets.flood") <$> readIORef callsMade
      floods `shouldBe` 1
      leftovers dir "ticks.out" >>= (`shouldBe` [])

  it "a refusal under -j -o is said on stderr: exit 77, no FILE" $
    withRunS (Scenario CallerView False True mockState False) $ \dir spawn -> do
      warm spawn
      let file = dir </> "answer.out"
      (code, out, err) <- reading (spawn ["-j", "-o", file]) ["tickets", "set_status", "--id", "IDENTITY-3"]
      (code, out) `shouldBe` (ExitFailure 77, "")
      err `shouldContain` "tickets.set_status"
      doesFileExist file >>= (`shouldBe` False)
      leftovers dir "answer.out" >>= (`shouldBe` [])

  it "a two-way request's line goes to stderr, its answer to FILE" $
    withRun $ \dir spawn -> do
      warm spawn
      let file = dir </> "answer.out"
      (Just inp, Just out, Just err, ph) <- createProcess (spawn ["--bidir-mode", "json", "-o", file] ["tickets", "ask"])
      let untilRequest = do
            l <- hGetLine err
            if "bidir_request" `isInfixOf` l then pure l else untilRequest
      req <- timeout 10000000 untilRequest
      req `shouldSatisfy` (/= Nothing)
      hPutStrLn inp "{\"type\":\"confirmed\",\"value\":true}"
      hFlush inp
      code <- timeout 10000000 (waitForProcess ph)
      code `shouldBe` Just ExitSuccess
      o <- BS.hGetContents out
      BS.length o `shouldBe` 0
      body <- readFile' file
      body `shouldContain` "answered"
      body `shouldNotContain` "bidir_request"

  forM_ [("a SIGTERM", ExitFailure 143)] $ \(how, want) ->
   it (how <> " during an answer leaves no FILE and no temporary file") $
    withRun $ \dir spawn -> do
      warm spawn
      let file = dir </> "answer.out"
      -- tickets.watch, unary here: a call the service never ends
      (Just inp, Just out, Just err, ph) <- createProcess (spawn ["-o", file] ["tickets", "watch"])
      hClose inp
      begun <- waitUpTo 10 (not . null <$> leftovers dir "answer.out")
      threadDelay 500000
      terminateProcess ph
      code <- timeout 5000000 (waitForProcess ph)
      _ <- BS.hGetContents out
      _ <- BS.hGetContents err
      (begun, code) `shouldBe` (True, Just want)
      doesFileExist file >>= (`shouldBe` False)
      leftovers dir "answer.out" >>= (`shouldBe` [])
  where
    -- a run of the built synapse against a fresh mock service serving a
    -- tickets schema with a read (get), a write (amend) and an endless
    -- stream (flood); @spawn globals args@
    withRun = withRunS todaysService
    withRunS sc act = withSystemTempDirectory "synapse39" $ \dir -> withMockService sc $ \port -> do
      forM_ ["home", "cache", "run"] $ \d -> createDirectoryIfMissing True (dir </> d)
      env0 <- getEnvironment
      let env = [ (k, v) | (k, v) <- env0, k `notElem` ["HOME", "XDG_CACHE_HOME", "XDG_RUNTIME_DIR", "SYNAPSE_AS", "PLEXUS_SESSION", "SYNAPSE_RECONNECT_FOR"] ]
              ++ [ ("HOME", dir </> "home"), ("XDG_CACHE_HOME", dir </> "cache"), ("XDG_RUNTIME_DIR", dir </> "run")
                 , ("PLEXUS_KEYCHAIN", "off"), ("SYNAPSE_HOLD_FOR", "3s") ]
          spawn globals args = (proc exe (globals ++ ["--url", "ws://127.0.0.1:" <> show port <> "/ws", "menger"] ++ args))
            { env = Just env, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
      writeIORef schemaOverride (Just [("tickets", ticketsWithWrites)])
      writeIORef dropNext []
      r <- try (act dir spawn)
      writeIORef schemaOverride Nothing
      writeIORef dropNext []
      either (\(e :: SomeException) -> ioError (userError (show e))) pure r

    -- the schema in the cache first, so the run under test is the call alone
    warm spawn = do
      (code, _, _) <- reading (spawn []) ["tickets", "get", "--id", "MENGER-12"]
      code `shouldBe` ExitSuccess

-- | What is beside FILE in its directory but FILE itself: a temporary file
-- left behind.
leftovers :: FilePath -> FilePath -> IO [FilePath]
leftovers dir name = filter (\f -> name `isPrefixOf` f && f /= name) <$> listDirectory dir

modeOf :: FilePath -> IO Int
modeOf f = (\s -> fromIntegral (fileMode s .&. 0o777)) <$> getFileStatus f

readFile' :: FilePath -> IO String
readFile' f = BS8.unpack <$> BS.readFile f

-- | Run with stdout read to its end.
reading :: ([String] -> CreateProcess) -> [String] -> IO (ExitCode, String, String)
reading spawn args = do
  (Just inp, Just out, Just err, ph) <- createProcess (spawn args)
  hClose inp
  c <- timeout 30000000 (waitForProcess ph)
  case c of
    Nothing -> do
      terminateProcess ph
      _ <- waitForProcess ph
      e <- BS.hGetContents err
      expectationFailure ("hung; stderr: " <> take 600 (BS8.unpack e))
      error "hung"
    Just code -> do
      o <- BS.hGetContents out
      e <- BS.hGetContents err
      pure (code, BS8.unpack o, BS8.unpack e)

waitUpTo :: Double -> IO Bool -> IO Bool
waitUpTo secs cond = go (round (secs * 20) :: Int)
  where
    go 0 = cond
    go n = cond >>= \ok -> if ok then pure True else threadDelay 50000 >> go (n - 1)

-- | menger's tickets in miniature: a read, a write and an endless stream.
ticketsWithWrites :: Value
ticketsWithWrites = object
  [ "namespace" .= ("tickets" :: Text), "version" .= ("0.5.0" :: Text), "hash" .= ("t0" :: Text)
  , "description" .= ("Plans as data" :: Text)
  , "methods" .=
      [ m "get" "One ticket by id" (Just "read") False [("id", True)]
      , m "amend" "Replace a ticket's body" (Just "write") False [("id", True), ("body", False)]
      , m "flood" "Ticks, every 100 ms, until unsubscribed" (Just "read") True []
      , m "set_status" "Change a ticket's status" (Just "write") False [("id", True)]
      , m "ask" "Ask the caller to confirm" (Just "write") False []
        -- the service's tickets.watch, said here to be unary: one item, and no end
      , m "watch" "One item, and no end" (Just "read") False [] ] ]
  where
    m :: Text -> Text -> Maybe Text -> Bool -> [(Text, Bool)] -> Value
    m n d g s ps = object $
      [ "name" .= n, "description" .= d, "hash" .= (n <> "0"), "streaming" .= s
      , "params" .= object
          [ "type" .= ("object" :: Text)
          , "required" .= [ p | (p, True) <- ps ]
          , "properties" .= object [ Key.fromText p .= object ["type" .= ("string" :: Text)] | (p, _) <- ps ] ] ]
      <> maybe [] (\g' -> ["group" .= g']) g
