{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SYNAPSE-26: a closed stdout is not an outage. The built synapse, end to
-- end against the mock service (a home, a cache and a runtime directory of
-- the test's own), its stdout a pipe whose reader closes early, as
-- @… | head -1@ does:
--
-- * a write's answer printed to a closed pipe: the service took the write
--   once, synapse exited 141 within 2 s and said nothing of a reconnect;
-- * a stream into a reader that closed after one line: 141 within 2 s, the
--   stream unsubscribed (or its connection closed), subscribed once;
-- * the socket dropping with stdout open: as before — a read sent again
--   and answered, a write that went out never repeated (exit 75).
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import GHC.Clock (getMonotonicTime)
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (Handle, hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import System.Timeout (timeout)
import Control.Concurrent (threadDelay)
import Test.Hspec

import MockService

main :: IO ()
main = hspec $ do
  synapse <- runIO (findExecutable "synapse")
  case synapse of
    Nothing -> it "(the synapse executable is not on PATH)" $ pendingWith "cabal test puts it there"
    Just exe -> pipeSpec exe

pipeSpec :: FilePath -> Spec
pipeSpec exe = describe "a closed stdout (SYNAPSE-26)" $ do
  it "is this build" $ do
    (_, out, _) <- readProcessWithExitCode exe ["--version"] ""
    out `shouldStartWith` "3.17"

  it "a_closed_stdout_ends_a_write_once" $
    withRun $ \spawn -> do
      warm spawn
      writeIORef callsMade []
      (code, secs, err) <- closingAfter (BS.hGetSome `flip` 1) spawn
        ["tickets", "amend", "--id", "MENGER-12", "--body", "the step"]
      threadDelay 500000
      amends <- length . filter (== "tickets.amend") <$> readIORef callsMade
      (code, amends) `shouldBe` (ExitFailure 141, 1)
      secs `shouldSatisfy` (< 2)
      err `shouldNotContain` "reconnect"
      err `shouldNotContain` "lost"

  it "a_closed_stdout_ends_a_stream" $
    withRun $ \spawn -> do
      warm spawn
      writeIORef callsMade []
      ended0 <- readIORef floodsEnded
      (code, secs, err) <- closingAfter BS8.hGetLine spawn ["tickets", "flood"]
      code `shouldBe` ExitFailure 141
      secs `shouldSatisfy` (< 2)
      err `shouldNotContain` "reconnect"
      -- the service saw the stream end, and it was subscribed once
      waitUpTo 2 ((> ended0) <$> readIORef floodsEnded) >>= (`shouldBe` True)
      floods <- length . filter (== "tickets.flood") <$> readIORef callsMade
      floods `shouldBe` 1

  it "a_lost_socket_with_stdout_open_still_reconnects" $
    withRun $ \spawn -> do
      warm spawn
      writeIORef callsMade []
      writeIORef dropNext ["tickets.get"]
      (code, out, err) <- reading spawn ["tickets", "get", "--id", "MENGER-12"]
      code `shouldBe` ExitSuccess
      out `shouldContain` "MENGER-12"
      err `shouldContain` "lost"
      err `shouldContain` "reconnected"
      gets <- length . filter (== "tickets.get") <$> readIORef callsMade
      gets `shouldBe` 2

  it "a lost socket on a write that went out, stdout open, is still exit 75, sent once" $
    withRun $ \spawn -> do
      warm spawn
      writeIORef callsMade []
      writeIORef dropNext ["tickets.amend"]
      (code, _, err) <- reading spawn ["tickets", "amend", "--id", "MENGER-12", "--body", "the step"]
      code `shouldBe` ExitFailure 75
      err `shouldContain` "the write may have happened"
      amends <- length . filter (== "tickets.amend") <$> readIORef callsMade
      amends `shouldBe` 1
  where
    -- a run of the built synapse against a fresh mock service serving a
    -- tickets schema with a write (amend) and an endless stream (flood)
    withRun act = withSystemTempDirectory "synapse26" $ \dir -> withMockService todaysService $ \port -> do
      forM_ ["home", "cache", "run"] $ \d -> createDirectoryIfMissing True (dir </> d)
      env0 <- getEnvironment
      let env = [ (k, v) | (k, v) <- env0, k `notElem` ["HOME", "XDG_CACHE_HOME", "XDG_RUNTIME_DIR", "SYNAPSE_AS", "PLEXUS_SESSION", "SYNAPSE_RECONNECT_FOR"] ]
              ++ [ ("HOME", dir </> "home"), ("XDG_CACHE_HOME", dir </> "cache"), ("XDG_RUNTIME_DIR", dir </> "run")
                 , ("PLEXUS_KEYCHAIN", "off"), ("SYNAPSE_HOLD_FOR", "3s") ]
          spawn args = (proc exe (["--url", "ws://127.0.0.1:" <> show port <> "/ws", "menger"] ++ args))
            { env = Just env, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
      writeIORef schemaOverride (Just [("tickets", ticketsWithWrites)])
      writeIORef dropNext []
      r <- try (act spawn)
      writeIORef schemaOverride Nothing
      writeIORef dropNext []
      either (\(e :: SomeException) -> ioError (userError (show e))) pure r

    -- the schema in the cache first, so the run under test is the call alone
    warm spawn = do
      (code, _, _) <- reading spawn ["tickets", "get", "--id", "MENGER-12"]
      code `shouldBe` ExitSuccess

-- | Run, read the stdout until the reader has what it wants, close it, and
-- wait (at most 5 s) for the exit: the code, the seconds from the close to
-- the exit, and stderr.
closingAfter :: (Handle -> IO BS.ByteString) -> ([String] -> CreateProcess) -> [String] -> IO (ExitCode, Double, String)
closingAfter readSome spawn args = do
  (Just inp, Just out, Just err, ph) <- createProcess (spawn args)
  hClose inp
  got <- timeout 10000000 (readSome out)
  got `shouldSatisfy` maybe False (not . BS.null)
  hClose out
  t0 <- getMonotonicTime
  code <- timeout 5000000 (waitForProcess ph)
  t1 <- getMonotonicTime
  case code of
    Nothing -> do
      terminateProcess ph
      _ <- waitForProcess ph
      e <- BS.hGetContents err
      expectationFailure ("still running 5 s after its stdout closed; stderr: " <> take 600 (BS8.unpack e))
      error "unreachable"
    Just c -> do
      e <- BS.hGetContents err
      pure (c, t1 - t0, BS8.unpack e)

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
      , m "flood" "Ticks, every 100 ms, until unsubscribed" (Just "read") True [] ] ]
  where
    m :: Text -> Text -> Maybe Text -> Bool -> [(Text, Bool)] -> Value
    m n d g s ps = object $
      [ "name" .= n, "description" .= d, "hash" .= (n <> "0"), "streaming" .= s
      , "params" .= object
          [ "type" .= ("object" :: Text)
          , "required" .= [ p | (p, True) <- ps ]
          , "properties" .= object [ (fromText' p) .= object ["type" .= ("string" :: Text)] | (p, _) <- ps ] ] ]
      <> maybe [] (\g' -> ["group" .= g']) g
    fromText' = Key.fromText
