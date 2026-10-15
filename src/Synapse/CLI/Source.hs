{-# LANGUAGE RecordWildCards #-}
-- | SYNAPSE-10: a parameter's value from a file or stdin.
--
-- A flag's value is one shell argument, and Linux caps an argument at 128 KB
-- (@MAX_ARG_STRLEN@), so @--body "$(cat body.md)"@ fails before synapse
-- starts once the body is large. Here a value names its source instead and
-- synapse reads it after it has started, so the only limit left is the
-- backend's own:
--
-- @
-- --body @body.md      the file's contents (UTF-8), as they are
-- --body -             standard input, read once however many values ask
-- --body @@literal     a value that really begins with @ (one @ is dropped)
-- -p @params.json      the whole params object from a file; -p - from stdin
-- @
--
-- The rule applies to every flag value of every backend, after the flags are
-- paired and before any other transformation, so @--body=\@body.md@ works
-- too and a path parameter read from a file is still expanded afterwards.
-- A file that cannot be read is a refusal naming the flag, the path and the
-- escape — never a value sent as the literal text @\@body.md@.
module Synapse.CLI.Source
  ( SourceEnv(..)
  , SourceError(..)
  , mkSourceEnv
  , expandSources
  , expandValue
  , stdinWasRead
  , renderSourceError
  ) where

import Control.Exception (IOException, try)
import System.IO.Error (ioeGetErrorString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import System.FilePath (isRelative, (</>))
import System.IO (Handle, stdin)

-- | What a source needs: where relative paths resolve, what @~/@ means, how
-- stdin is read (a test hands in its own), and stdin's contents once read.
data SourceEnv = SourceEnv
  { seCwd   :: FilePath
  , seHome  :: Maybe FilePath
  , seStdin :: IO BS.ByteString
  , seCache :: IORef (Maybe Text)
  }

-- | A source that could not be read: the flag it was for (empty for @-p@),
-- the path as written, and the system's reason.
data SourceError = SourceUnreadable
  { seKey    :: Text
  , seSource :: Text
  , seWhy    :: Text
  }
  deriving (Show, Eq)

mkSourceEnv :: FilePath -> Maybe FilePath -> IO SourceEnv
mkSourceEnv cwd home = do
  cache <- newIORef Nothing
  pure SourceEnv { seCwd = cwd, seHome = home, seStdin = readToEnd stdin, seCache = cache }

-- | All of a handle, without closing it: 'BS.getContents' semi-closes
-- stdin, and the bidirectional mode's terminal check later throws on a
-- closed handle.
readToEnd :: Handle -> IO BS.ByteString
readToEnd h = go []
  where
    go acc = do
      chunk <- BS.hGetSome h 65536
      if BS.null chunk then pure (BS.concat (reverse acc)) else go (chunk : acc)

-- | Whether some value has taken stdin: the bidirectional JSON mode reads it
-- too, and the two cannot share.
stdinWasRead :: SourceEnv -> IO Bool
stdinWasRead env = maybe False (const True) <$> readIORef (seCache env)

-- | Every flag value through 'expandValue'; the first failure wins.
expandSources :: SourceEnv -> [(Text, Text)] -> IO (Either SourceError [(Text, Text)])
expandSources env = go []
  where
    go acc [] = pure (Right (reverse acc))
    go acc ((k, v) : rest) = do
      r <- expandValue env k v
      case r of
        Left e -> pure (Left e)
        Right v' -> go ((k, v') : acc) rest

-- | One value: @-@ is stdin, @\@\@x@ is the literal @\@x@, @\@path@ is that
-- file, anything else is itself.
expandValue :: SourceEnv -> Text -> Text -> IO (Either SourceError Text)
expandValue env@SourceEnv{..} key val
  | val == "-" = Right <$> readStdinOnce env
  | "@@" `T.isPrefixOf` val = pure (Right (T.drop 1 val))
  | Just path <- T.stripPrefix "@" val, not (T.null path) = do
      r <- try (BS.readFile (resolve path)) :: IO (Either IOException BS.ByteString)
      pure $ case r of
        Left e -> Left SourceUnreadable { seKey = key, seSource = val, seWhy = T.pack (ioeGetErrorString e) }
        Right bytes -> Right (decode bytes)
  | otherwise = pure (Right val)
  where
    resolve p = case (T.unpack p, seHome) of
      ('~' : '/' : rest, Just home) -> home </> rest
      (fp, _) | isRelative fp -> seCwd </> fp
              | otherwise -> fp

readStdinOnce :: SourceEnv -> IO Text
readStdinOnce SourceEnv{..} = do
  cached <- readIORef seCache
  case cached of
    Just t -> pure t
    Nothing -> do
      t <- decode <$> seStdin
      writeIORef seCache (Just t)
      pure t

decode :: BS.ByteString -> Text
decode = TE.decodeUtf8With TE.lenientDecode

-- | The refusal, with the escape for a value that really begins with @.
renderSourceError :: SourceError -> Text
renderSourceError SourceUnreadable{..} =
  flag <> seSource <> ": cannot read " <> T.drop 1 seSource <> " (" <> seWhy <> "). "
    <> "A value that begins with @ is read from that file; write @@" <> T.drop 1 seSource
    <> " to send the text " <> seSource <> " itself."
  where
    flag = if T.null seKey then "-p " else "--" <> T.replace "_" "-" seKey <> " "
