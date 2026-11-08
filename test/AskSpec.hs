{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | ASK-3: synapse answers a proposal (ASK-1's @needs_input@) — the built
-- synapse against the mock service, whose @tickets.archive@ (a
-- confirmation), @tickets.purge@ (a field with options and a default) and
-- @tickets.close@ (a field with none) ask.
--
-- A terminal is a pty on stdin (stdout a pipe, so the prompts go to
-- stderr); no terminal is a closed pipe. Every call the mock takes is
-- kept ('askCalls'), so each test checks what the answer call carried:
-- the same params, each field under its name, and @confirm@ — and that
-- nothing else was called.
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.List (isPrefixOf)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (Handle, hClose, hPutStr, hFlush)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.IO (fdToHandle)
import System.Posix.Terminal (openPseudoTerminal)
import System.Process
import System.Timeout (timeout)
import Test.Hspec

import MockService

main :: IO ()
main = hspec $ do
  synapse <- runIO (findExecutable "synapse")
  case synapse of
    Nothing -> it "(the synapse executable is not on PATH)" $ pendingWith "cabal test puts it there"
    Just exe -> askSpec exe

askSpec :: FilePath -> Spec
askSpec exe = do
  describe "on a terminal" $ do
    it "a confirmation: the summary and [y/N]; y makes the same call with confirm" $
      withRun $ \run -> do
        (code, out, err) <- run Tty "y\n" [] ["tickets", "archive", "--filter", "label stale"]
        code `shouldBe` ExitSuccess
        err `shouldContain` "  \194\183 MENGER-4"
        err `shouldContain` "? archive MENGER-4, MENGER-5, MENGER-6 (3 not done)  [y/N] "
        out `shouldContain` "MENGER-6"
        calls <- readIORef askCalls
        calls `shouldBe`
          [ ("tickets.archive", KM.fromList [("filter", "label stale")])
          , ("tickets.archive", KM.fromList [("filter", "label stale"), ("confirm", "v1.1794100000.archive-1")]) ]

    forM_ [("n", "n\n"), ("an empty line", "\n"), ("the end of the input", "")] $ \(how, typed) ->
      it ("no (" <> how <> "): nothing done, exit 0, no second call") $
        withRun $ \run -> do
          (code, out, err) <- run Tty typed [] ["tickets", "archive", "--filter", "label stale"]
          (code, out) `shouldBe` (ExitSuccess, "")
          err `shouldContain` "nothing done"
          map fst <$> readIORef askCalls `shouldReturn` ["tickets.archive"]

    it "a field with options: a menu, its default shown; the choice goes under the field's name" $
      withRun $ \run -> do
        (code, _, err) <- run Tty "2\ny\n" [] ["tickets", "purge", "--ids", "KILN-2"]
        code `shouldBe` ExitSuccess
        err `shouldContain` "KILN-2 has 4 children"
        err `shouldContain` "    1) reparent (default)"
        err `shouldContain` "    2) delete"
        err `shouldContain` "  children [1]: "
        err `shouldContain` "? go ahead with children = delete  [y/N] "
        calls <- readIORef askCalls
        map snd calls `shouldBe`
          [ KM.fromList [("ids", toJSON ["KILN-2" :: Text])]
          , KM.fromList [("ids", toJSON ["KILN-2" :: Text]), ("children", "delete"), ("confirm", "v1.1794100000.purge-1")] ]

    it "a field's default is taken on an empty answer" $
      withRun $ \run -> do
        (code, _, _) <- run Tty "\ny\n" [] ["tickets", "purge", "--ids", "KILN-2"]
        code `shouldBe` ExitSuccess
        calls <- readIORef askCalls
        (KM.lookup "children" . snd) (last calls) `shouldBe` Just "reparent"

    it "a field with no default is asked until it has a value" $
      withRun $ \run -> do
        (code, out, err) <- run Tty "\nduplicate\ny\n" [] ["tickets", "close", "--id", "KILN-9"]
        code `shouldBe` ExitSuccess
        err `shouldContain` "reason has no default"
        out `shouldContain` "duplicate"
        calls <- readIORef askCalls
        snd (last calls) `shouldBe` KM.fromList [("id", "KILN-9"), ("reason", "duplicate"), ("confirm", "v1.1794100000.close-1")]

    it "a plan that moved is shown and asked again" $
      withRun $ \run -> do
        (code, out, err) <- run Tty "y\ny\n" [] ["tickets", "archive", "--filter", "moving"]
        code `shouldBe` ExitSuccess
        err `shouldContain` "the plan moved"
        err `shouldContain` "? archive MENGER-4, MENGER-5, MENGER-6, MENGER-7 (4 not done)  [y/N] "
        out `shouldContain` "MENGER-7"
        map (KM.lookup "confirm" . snd) <$> readIORef askCalls
          `shouldReturn` [Nothing, Just "v1.1794100000.archive-1", Just "v1.1794100000.archive-2"]

    it "a call that changes nothing answers directly" $
      withRun $ \run -> do
        (code, out, err) <- run Tty "" [] ["tickets", "archive", "--filter", "none"]
        (code, err) `shouldBe` (ExitSuccess, "")
        out `shouldContain` "archived"

  describe "with no terminal" $ do
    it "prints the proposal, the command that confirms it on stderr, and exits 10" $
      withRun $ \run -> do
        (code, out, err) <- run NoTty "" [] ["tickets", "archive", "--filter", "label stale"]
        code `shouldBe` ExitFailure 10
        out `shouldContain` "needs input: archive MENGER-4, MENGER-5, MENGER-6 (3 not done)"
        out `shouldContain` "  \194\183 MENGER-5"
        out `shouldContain` "expires 2026-11-07T12:00:00Z"
        err `shouldContain` "nothing done"
        err `shouldContain` "menger tickets archive --filter 'label stale' --confirm v1.1794100000.archive-1"
        length <$> readIORef askCalls `shouldReturn` 1

    it "-j: the item is the line on stdout" $
      withRun $ \run -> do
        (code, out, _) <- run NoTty "" ["-j"] ["tickets", "archive", "--filter", "label stale"]
        code `shouldBe` ExitFailure 10
        let items = [ v | l <- lines out, Just (v :: Value) <- [decodeStrict (BS8.pack l)] ]
        [ c | Object o <- items, Just (Object c) <- [KM.lookup "content" o], KM.lookup "type" c == Just "needs_input" ]
          `shouldSatisfy` ((== 1) . length)

    it "a field: its default in the command, or ??? where it has none" $
      withRun $ \run -> do
        (c1, _, e1) <- run NoTty "" [] ["tickets", "purge", "--ids", "KILN-2"]
        c1 `shouldBe` ExitFailure 10
        e1 `shouldContain` "--confirm v1.1794100000.purge-1 --children reparent"
        (c2, o2, e2) <- run NoTty "" [] ["tickets", "close", "--id", "KILN-9"]
        c2 `shouldBe` ExitFailure 10
        o2 `shouldContain` "--reason (no default)"
        e2 `shouldContain` "--confirm v1.1794100000.close-1 --reason ???"

    it "the command it prints does what the proposal said" $
      withRun $ \run -> do
        (_, _, err) <- run NoTty "" [] ["tickets", "archive", "--filter", "label stale"]
        let cmd = readyLine err
        (code, out, _) <- run (Shell cmd) "" [] []
        code `shouldBe` ExitSuccess
        out `shouldContain` "MENGER-4"
        map (KM.lookup "confirm" . snd) <$> readIORef askCalls
          `shouldReturn` [Nothing, Just "v1.1794100000.archive-1"]

    it "params given with -p come back whole in -p, with confirm" $
      withRun $ \run -> do
        (code, _, err) <- run NoTty "" ["-p", "{\"filter\":\"label stale\"}"] ["tickets", "archive"]
        code `shouldBe` ExitFailure 10
        err `shouldContain` "-p '{\"confirm\":\"v1.1794100000.archive-1\",\"filter\":\"label stale\"}'"
        let cmd = readyLine err
        (code', out, _) <- run (Shell cmd) "" [] []
        code' `shouldBe` ExitSuccess
        out `shouldContain` "MENGER-6"

  describe "--yes" $ do
    it "confirms a confirmation: the same call with the token" $
      withRun $ \run -> do
        (code, out, err) <- run NoTty "" ["--yes"] ["tickets", "archive", "--filter", "label stale"]
        code `shouldBe` ExitSuccess
        err `shouldContain` "--yes: confirming archive MENGER-4"
        out `shouldContain` "MENGER-6"
        map (KM.lookup "confirm" . snd) <$> readIORef askCalls
          `shouldReturn` [Nothing, Just "v1.1794100000.archive-1"]

    it "sends each field's default" $
      withRun $ \run -> do
        (code, _, _) <- run NoTty "" ["--yes"] ["tickets", "purge", "--ids", "KILN-2"]
        code `shouldBe` ExitSuccess
        calls <- readIORef askCalls
        (KM.lookup "children" . snd) (last calls) `shouldBe` Just "reparent"

    it "refuses, naming the field, when one has no default: nothing done, exit 10" $
      withRun $ \run -> do
        (code, _, err) <- run Tty "" ["--yes"] ["tickets", "close", "--id", "KILN-9"]
        code `shouldBe` ExitFailure 10
        err `shouldContain` "--yes cannot confirm: the field reason has no default"
        err `shouldContain` "--reason ???"
        length <$> readIORef askCalls `shouldReturn` 1

  describe "what a method that asks says of itself" $ do
    forM_ ["archive", "close"] $ \m ->
      it ("--help lists --confirm TOKEN and says it may ask (" <> m <> ")") $
        withRun $ \run -> do
          (code, out, _) <- run NoTty "" [] ["tickets", m, "--help"]
          code `shouldBe` ExitSuccess
          out `shouldContain` "[--confirm TOKEN]"
          out `shouldContain` "It may ask for confirmation"

    it "a method that does not ask says nothing of it" $
      withRun $ \run -> do
        (_, out, _) <- run NoTty "" [] ["tickets", "get", "--help"]
        out `shouldNotContain` "--confirm"
        out `shouldNotContain` "may ask"

    it "the IR it writes (-i) carries mdAsks and the confirm param" $
      withRun $ \run -> do
        (_, out, _) <- run NoTty "" ["-i"] ["tickets"]
        Just (Object ir) <- pure (decodeStrict (BS8.pack out))
        Just (Object methods) <- pure (KM.lookup "irMethods" ir)
        let asks name = case KM.lookup name methods of
              Just (Object md) -> (KM.lookup "mdAsks" md, confirmIn md)
              _ -> (Nothing, False)
            confirmIn md = case KM.lookup "mdParams" md of
              Just (Array ps) -> any (\p -> case p of { Object o -> KM.lookup "pdName" o == Just "confirm"; _ -> False }) ps
              _ -> False
        asks "tickets.archive" `shouldBe` (Just (Bool True), True)
        asks "tickets.close" `shouldBe` (Just (Bool True), True)
        asks "tickets.get" `shouldBe` (Just (Bool False), False)
  where
    withRun act = withSystemTempDirectory "ask3" $ \dir -> withMockService todaysService $ \port -> do
      forM_ ["home", "cache", "run"] $ \d -> createDirectoryIfMissing True (dir </> d)
      env0 <- getEnvironment
      resetAsks
      let env = [ (k, v) | (k, v) <- env0, k `notElem` ["HOME", "XDG_CACHE_HOME", "XDG_RUNTIME_DIR", "SYNAPSE_AS", "PLEXUS_SESSION", "SYNAPSE_RECONNECT_FOR"] ]
              ++ [ ("HOME", dir </> "home"), ("XDG_CACHE_HOME", dir </> "cache"), ("XDG_RUNTIME_DIR", dir </> "run")
                 , ("PLEXUS_KEYCHAIN", "off"), ("SYNAPSE_HOLD_FOR", "3s") ]
          url = "ws://127.0.0.1:" <> show port <> "/ws"
          run how typed globals args = do
            let p = case how of
                  Shell cmd -> proc "sh" ["-c", cmd]
                  _ -> proc exe (globals ++ ["--url", url, "menger"] ++ args)
            runOne how typed p { env = Just env }
      act run

data How = Tty | NoTty | Shell String

-- | The command stderr says confirms the proposal (the line under
-- "to confirm, run:"), as the shell would be given it. Bytes as they are
-- (the line is ASCII; the run's em dash is on the line before it).
readyLine :: String -> String
readyLine err = case [ l | l <- lines err, "  synapse " `isPrefixOf` l ] of
  l : _ -> dropWhile (== ' ') l
  [] -> error ("no ready command in: " <> err)

-- | Run with stdin a pty (what is typed written to it at once) or a closed
-- pipe, stdout and stderr read to their ends.
runOne :: How -> String -> CreateProcess -> IO (ExitCode, String, String)
runOne how typed p = do
  (stdinOf, master) <- case how of
    Tty -> do
      (m, s) <- openPseudoTerminal
      sh <- fdToHandle s
      mh <- fdToHandle m
      pure (UseHandle sh, Just mh)
    _ -> pure (CreatePipe, Nothing)
  (inp, Just out, Just err, ph) <- createProcess p { std_in = stdinOf, std_out = CreatePipe, std_err = CreatePipe }
  maybe (pure ()) hClose inp
  forM_ master $ \mh -> do
    hPutStr mh typed
    hFlush mh
    -- the end of the input, as a person's ^D, once the lines are read
    if null typed then hPutStr mh "\EOT" >> hFlush mh else pure ()
  c <- timeout 30000000 (waitForProcess ph)
  o <- BS.hGetContents out
  e <- BS.hGetContents err
  forM_ master closeQuietly
  case c of
    Nothing -> do
      terminateProcess ph
      expectationFailure ("hung; stderr: " <> take 800 (BS8.unpack e))
      error "hung"
    Just code -> pure (code, BS8.unpack o, BS8.unpack e)
  where
    closeQuietly :: Handle -> IO ()
    closeQuietly h = () <$ (try (hClose h) :: IO (Either SomeException ()))
