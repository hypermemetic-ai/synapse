{-# LANGUAGE ScopedTypeVariables #-}

-- | SYNAPSE-39: @synapse -o FILE@ — the call's answer written to a file,
-- whole, never to a pipe.
--
-- The run's stdout itself is pointed at a temporary file beside FILE (made
-- 0600 and exclusive, as 'openBinaryTempFile' makes it), so every printer
-- writes there unchanged and in the same form it would have had on a pipe
-- (@-j@ lines with @-j@, the rendered answer without). When the run ends
-- with 0 the temporary file is renamed over FILE: a reader never sees half
-- an answer. When it ends with anything else, the temporary file is
-- removed and so is an older FILE — no stale answer is left to be read as
-- this one — and the run's own code stands (75, 77, 69…).
--
-- A stream ('streaming', told when the call turns out to be one) has no
-- end to wait for: its temporary file is renamed to FILE at once, and each
-- item is in FILE when it is printed (stdout is line-buffered and every
-- printer flushes). A stream that fails keeps what it wrote; one that wrote
-- nothing leaves no FILE.
--
-- A FILE that cannot be made (no such directory, a directory) is said at
-- once and the run exits 'exitCannotCreate' before anything is sent. A write to it that fails
-- later (a full disk) is no outage and no answer of the service's: the
-- printer's failure ends the call (SYNAPSE-26's marking in
-- 'Synapse.Reconnect'), and the run exits 'exitCannotWrite', saying so —
-- at once, past the runtime's last flush, which would fail on what is left
-- in stdout's buffer and make the exit 1.
--
-- What must be seen while the call waits goes to stderr under @-o@, not
-- FILE: a two-way request's prompt ('promptHandle'), and under @-j@ an error
-- or a refusal — FILE goes with the failed run, so its line alone would be
-- lost. A SIGTERM or SIGHUP ends the run as a failed one (143, 129): the
-- temporary file is removed. A SIGKILL can leave it behind.
module Synapse.Output
  ( withOutputFile
  , streaming
  , active
  , promptHandle
  , exitCannotCreate
  , exitCannotWrite
  ) where

import Control.Exception (SomeException, IOException, catch, fromException, throwIO, try)
import Control.Monad (when)
import Data.IORef
import GHC.IO.Exception (IOException(..))
import GHC.IO.Handle (hDuplicateTo)
import System.Directory (removeFile, renameFile, doesFileExist)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath (takeDirectory, takeFileName)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (setFileMode, fileSize, getFileStatus)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler(..), installHandler, sigHUP, sigTERM)
import System.Directory (doesDirectoryExist)

import Synapse.Reconnect (isStdoutFailure)

-- | sysexits' EX_CANTCREAT: FILE could not be made.
exitCannotCreate :: Int
exitCannotCreate = 73

-- | sysexits' EX_IOERR: a write to FILE failed.
exitCannotWrite :: Int
exitCannotWrite = 74

data Out = Out
  { oFile      :: FilePath
  , oTmp       :: FilePath
  , oStreaming :: IORef Bool
  }

{-# NOINLINE current #-}
current :: IORef (Maybe Out)
current = unsafePerformIO (newIORef Nothing)

-- | Run the program with its stdout in FILE (@Nothing@ or @-@: stdout as
-- it is).
withOutputFile :: Maybe FilePath -> IO a -> IO a
withOutputFile Nothing act = act
withOutputFile (Just "-") act = act
withOutputFile (Just file) act = do
  let dir = case takeDirectory file of { "" -> "."; d -> d }
  isDir <- doesDirectoryExist file
  opened <- if isDir then pure (Left "is a directory") else
    either (Left . reason) Right <$> try (openBinaryTempFile dir (takeFileName file <> ".tmp"))
  case opened of
    Left why -> do
      hPutStrLn stderr ("synapse: cannot write " <> file <> ": " <> why)
      exitWith (ExitFailure exitCannotCreate)
    Right (tmp, h) -> do
      setFileMode tmp 0o600
      enc <- hGetEncoding stdout
      hFlush stdout
      hDuplicateTo h stdout
      hClose h
      -- the form a pipe would have had: the same encoding, and each line
      -- out as it is printed
      maybe (hSetBinaryMode stdout True) (hSetEncoding stdout) enc
      hSetBuffering stdout LineBuffering
      st <- newIORef False
      let out = Out file tmp st
      writeIORef current (Just out)
      -- a run told to stop is a failed one: no FILE, no temporary file
      let stopped n = Catch $ do
            finish out (ExitFailure n) `catch` \(_ :: SomeException) -> pure ()
            hFlush stderr `catch` \(_ :: SomeException) -> pure ()
            exitImmediately (ExitFailure n)
      _ <- installHandler sigTERM (stopped 143) Nothing
      _ <- installHandler sigHUP (stopped 129) Nothing
      r <- try act
      let code = case r of
            Right _ -> ExitSuccess
            Left e | Just (c :: ExitCode) <- fromException e -> c
                   | otherwise -> ExitFailure 1
      flushed <- try (hFlush stdout)
      case flushed of
        Left (e :: IOException) -> do
          finish out (ExitFailure exitCannotWrite)
          cannotWrite file e
        Right () -> finish out code
      case r of
        Right a -> pure a
        Left e
          | Just (ioe :: IOException) <- fromException e, isStdoutFailure e -> cannotWrite file ioe
          | otherwise -> throwIO e

-- | The run's ending, in FILE: renamed into place, or taken away.
finish :: Out -> ExitCode -> IO ()
finish out code = do
  s <- readIORef (oStreaming out)
  if s
    then when (code /= ExitSuccess) $ do
      -- a stream keeps what came; one that brought nothing leaves no FILE
      size <- (fileSize <$> getFileStatus (oFile out)) `catch` \(_ :: SomeException) -> pure 1
      when (size == 0) $ quietly (removeFile (oFile out))
    else if code == ExitSuccess
      then renameFile (oTmp out) (oFile out) `catch` \(e :: IOException) -> do
        quietly (removeFile (oTmp out))
        cannotWrite (oFile out) e
      else do
        quietly (removeFile (oTmp out))
        exists <- doesFileExist (oFile out)
        when exists $ quietly (removeFile (oFile out))

-- | Whether @-o FILE@ holds stdout.
active :: IO Bool
active = maybe False (const True) <$> readIORef current

-- | Where a prompt goes that must be seen while the call waits: stderr
-- under @-o@ (stdout is FILE), else stdout.
promptHandle :: IO Handle
promptHandle = do
  a <- active
  pure (if a then stderr else stdout)

-- | The call is a stream: FILE is its temporary file from now on, each
-- item in it as it is printed.
streaming :: IO ()
streaming = do
  m <- readIORef current
  case m of
    Nothing -> pure ()
    Just out -> do
      s <- readIORef (oStreaming out)
      when (not s) $ do
        hFlush stdout
        renameFile (oTmp out) (oFile out)
        writeIORef (oStreaming out) True

cannotWrite :: FilePath -> IOException -> IO a
cannotWrite file e = do
  hPutStrLn stderr ("synapse: cannot write " <> file <> ": " <> reason e)
  hFlush stderr `catch` \(_ :: SomeException) -> pure ()
  exitImmediately (ExitFailure exitCannotWrite)
  exitWith (ExitFailure exitCannotWrite)

reason :: IOException -> String
reason e = case ioe_description e of
  "" -> show (ioe_type e)
  d -> d

quietly :: IO () -> IO ()
quietly a = a `catch` \(_ :: SomeException) -> pure ()
