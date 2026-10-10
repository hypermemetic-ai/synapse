{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Notes (SYNAPSE-11): a thought written down before it is a ticket —
-- dated, never titled, kept in one place.
--
-- __Where.__ One directory for every note, whatever backend or directory
-- synapse is run from: @$SYNAPSE_NOTES_DIR@, else @~\/.plexus\/notes@ —
-- beside synapse's per-backend state (@~\/.plexus\/\<backend\>@). Made when
-- the first note is written.
--
-- __What.__ A note is one markdown file, @\<id\>.md@: a frontmatter that
-- holds its @date@ (UTC) — and, once it is upgraded, the @ticket@ it
-- became, the @backend@ that holds it and when (@upgraded@) — then the
-- text as written. Nothing titles it. Its id is the moment it was taken,
-- @YYYYMMDD-HHMMSS@ (UTC), with @-2@, @-3@… for a second note in the same
-- second; the file is plain, so @grep@, an editor and a sync tool all work
-- on it.
--
-- __Writes__ are atomic and never overwrite another note: the file is
-- written beside its name and linked into place, which fails if the name
-- is taken (then the next suffix is tried). An upgrade rewrites its note
-- the same way, renamed over itself.
module Synapse.Note
  ( -- * Notes
    Note(..)
  , Upgrade(..)
    -- * The directory
  , notesDir
  , noteFile
    -- * The file
  , renderNote
  , parseNote
  , noteIdAt
    -- * Reading and writing
  , writeNewNote
  , readNotes
  , findNote
  , markUpgraded
    -- * As a ticket
  , firstLine
  , titleOf
  , ticketBody
  , maxTitle
  , localStamp
  ) where

import Control.Exception (IOException, try)
import qualified Control.Exception as E
import Data.Char (isAlphaNum, isSpace)
import Data.List (sortBy)
import Data.Ord (Down(..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone, utcToZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , getHomeDirectory
  , listDirectory
  , removeFile
  , renameFile
  )
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, dropExtension, (</>))
import System.IO (IOMode(..), hSetEncoding, utf8, withFile)
import System.IO.Error (isAlreadyExistsError)
import System.Posix.Files (createLink)

-- | A note: its id (the file's name), when it was taken, its text, and —
-- once upgraded — the ticket it became.
data Note = Note
  { noteId      :: Text
  , noteDate    :: UTCTime
  , noteText    :: Text
  , noteUpgrade :: Maybe Upgrade
  } deriving (Eq, Show)

-- | Where a note went: the ticket's id, the backend that holds it, when.
data Upgrade = Upgrade
  { upTicket  :: Text
  , upBackend :: Text
  , upAt      :: UTCTime
  } deriving (Eq, Show)

-- ============================================================================
-- The directory
-- ============================================================================

-- | The one place notes live: @$SYNAPSE_NOTES_DIR@ when set and not empty,
-- else @~\/.plexus\/notes@.
notesDir :: IO FilePath
notesDir = do
  override <- lookupEnv "SYNAPSE_NOTES_DIR"
  case override of
    Just dir | not (null dir) -> pure dir
    _ -> do
      home <- getHomeDirectory
      pure (home </> ".plexus" </> "notes")

-- | A note's file in @dir@.
noteFile :: FilePath -> Text -> FilePath
noteFile dir nid = dir </> (T.unpack nid <> ".md")

-- ============================================================================
-- The file
-- ============================================================================

-- | The id a note taken at @t@ has before any suffix: @YYYYMMDD-HHMMSS@, UTC.
noteIdAt :: UTCTime -> Text
noteIdAt = T.pack . formatTime defaultTimeLocale "%Y%m%d-%H%M%S"

stamp :: UTCTime -> Text
stamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | A note as its file holds it. Its id is the file's name, not a field.
renderNote :: Note -> Text
renderNote n = T.unlines (["---", "date: " <> stamp (noteDate n)] <> upgraded <> ["---"]) <> noteText n <> "\n"
  where
    upgraded = case noteUpgrade n of
      Nothing -> []
      Just u ->
        [ "ticket: " <> upTicket u
        , "backend: " <> upBackend u
        , "upgraded: " <> stamp (upAt u)
        ]

-- | A note from its id and its file's text: a frontmatter with a @date@,
-- then the text (its last newline dropped). An @upgraded@ note carries all
-- three of @ticket@, @backend@ and @upgraded@. Other keys are ignored, so
-- a hand edit that adds one does not lose the note.
parseNote :: Text -> Text -> Either Text Note
parseNote nid raw = do
  rest <- maybe (Left "it does not open with a --- frontmatter") Right (T.stripPrefix "---\n" (T.replace "\r\n" "\n" raw))
  let (front, after) = T.breakOn "\n---\n" ("\n" <> rest)
  body <- case T.stripPrefix "\n---\n" after of
    Just b -> Right b
    Nothing -> case T.stripSuffix "\n---" (T.stripEnd ("\n" <> rest)) of
      Just _ -> Right ""
      Nothing -> Left "its frontmatter never closes"
  let fields = [ (T.strip k, T.strip (T.drop 1 v))
               | line <- T.lines front, not (T.null (T.strip line))
               , let (k, v) = T.breakOn ":" line, not (T.null v) ]
      field k = lookup k fields
      time k = field k >>= iso8601ParseM . T.unpack
  date <- maybe (Left "it has no date") Right (time "date")
  upgrade <- case (field "ticket", field "backend", time "upgraded") of
    (Nothing, _, _) -> Right Nothing
    (Just t, Just b, Just at) -> Right (Just (Upgrade t b at))
    (Just _, _, _) -> Left "it names a ticket without its backend and when"
  pure Note { noteId = nid, noteDate = date, noteText = dropLastNewline body, noteUpgrade = upgrade }
  where
    dropLastNewline t = maybe t id (T.stripSuffix "\n" t)

-- ============================================================================
-- Reading and writing
-- ============================================================================

-- | Writes a new note taken at @t@ into @dir@ (made if missing), its text
-- with trailing whitespace dropped. The id is @noteIdAt t@, or the first of
-- @-2@, @-3@… that no note in @dir@ holds yet. Atomic, and never over
-- another note.
writeNewNote :: FilePath -> UTCTime -> Text -> IO Note
writeNewNote dir t text = do
  createDirectoryIfMissing True dir
  go (1 :: Int)
  where
    base = noteIdAt t
    go k = do
      let nid = if k == 1 then base else base <> "-" <> T.pack (show k)
          note = Note { noteId = nid, noteDate = t, noteText = T.dropWhileEnd isSpace text, noteUpgrade = Nothing }
          tmp = dir </> ("." <> T.unpack nid <> ".md.tmp")
      writeUtf8 tmp (renderNote note)
      linked <- try (createLink tmp (noteFile dir nid))
      removeFile tmp
      case linked of
        Right () -> pure note
        Left (e :: IOException)
          | isAlreadyExistsError e && k < 1000 -> go (k + 1)
          | otherwise -> E.throwIO e

-- | Every note in @dir@, newest first (a missing directory holds none), and
-- the files that could not be read as notes, each with why.
readNotes :: FilePath -> IO ([Note], [(FilePath, Text)])
readNotes dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure ([], [])
    else do
      names <- listDirectory dir
      let files = [ f | f <- names, takeExtension f == ".md", take 1 f /= "." ]
      parsed <- mapM read1 files
      let notes = [ n | Right n <- parsed ]
          bad = [ e | Left e <- parsed ]
      -- Newest first; in one second, the later suffix first (-10 after -9).
      pure (sortBy (comparing (\n -> (Down (noteDate n), Down (T.length (noteId n)), Down (noteId n)))) notes, bad)
  where
    read1 f = do
      raw <- readUtf8 (dir </> f)
      pure $ case parseNote (T.pack (dropExtension f)) raw of
        Right n -> Right n
        Left why -> Left (dir </> f, why)

-- | The note an id names: the note with exactly that id, else the one note
-- whose id begins with it. Otherwise the reason — no note, or which ones
-- it could mean.
findNote :: [Note] -> Text -> Either Text Note
findNote notes asked
  | T.null wanted = Left "name a note: its id, or the start of one"
  | Just n <- lookup wanted [ (noteId n, n) | n <- notes ] = Right n
  | otherwise = case filter ((wanted `T.isPrefixOf`) . noteId) notes of
      [n] -> Right n
      [] -> Left ("no note " <> wanted)
      many -> Left (wanted <> " could be any of " <> T.intercalate ", " (map noteId many))
  where
    wanted = T.strip asked

-- | The note, upgraded: its file rewritten with the ticket it became — in
-- place, atomically (written beside it, renamed over it).
markUpgraded :: FilePath -> Note -> Upgrade -> IO Note
markUpgraded dir n u = do
  let upgraded = n { noteUpgrade = Just u }
      tmp = dir </> ("." <> T.unpack (noteId n) <> ".md.tmp")
  writeUtf8 tmp (renderNote upgraded)
  renameFile tmp (noteFile dir (noteId n))
  pure upgraded

writeUtf8 :: FilePath -> Text -> IO ()
writeUtf8 path text = withFile path WriteMode $ \h -> hSetEncoding h utf8 >> TIO.hPutStr h text

readUtf8 :: FilePath -> IO Text
readUtf8 path = withFile path ReadMode $ \h -> hSetEncoding h utf8 >> (T.copy <$> TIO.hGetContents h)

-- ============================================================================
-- As a ticket
-- ============================================================================

-- | The longest title a note gives a ticket.
maxTitle :: Int
maxTitle = 80

-- | A note's first line with words — a letter or a digit; a rule (@---@)
-- or a line of marks has none — its markdown marks (a heading's @#@, a
-- quote's @>@, a list's bullet, @**@) dropped and its spaces collapsed.
firstLine :: Text -> Text
firstLine text = case filter (T.any isAlphaNum) (map clean (T.lines text)) of
  (l : _) -> l
  [] -> ""
  where
    clean = T.unwords . T.words . T.replace "**" "" . unmark . T.strip
    unmark l = case T.uncons l of
      Just (c, rest) | c `elem` ("#>" :: String) -> unmark (T.stripStart (T.dropWhile (== c) rest))
      Just (c, rest) | c `elem` ("-*+" :: String), maybe True (isSpace . fst) (T.uncons rest) -> T.stripStart rest
      _ -> l

-- | A moment as the reader's clock shows it, its zone named: what a
-- person reads (a file keeps UTC).
localStamp :: TimeZone -> UTCTime -> Text
localStamp tz = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M %Z" . utcToZonedTime tz

-- | The title a note gives the ticket it becomes: its first line, at most
-- 'maxTitle' characters — cut at a word, with an ellipsis, when longer —
-- or, for a note with no words, when it was taken (in @tz@).
titleOf :: TimeZone -> Note -> Text
titleOf tz n
  | T.null line = "Note of " <> localStamp tz (noteDate n)
  | T.length line <= maxTitle = line
  | otherwise =
      let cut = T.take (maxTitle - 1) line
          atWord = T.dropWhileEnd (not . isSpace) cut
          kept = if T.length atWord >= maxTitle `div` 2 then T.stripEnd atWord else cut
      in kept <> "…"
  where
    line = firstLine (noteText n)

-- | The body a note gives the ticket it becomes: its text, then a line
-- saying when it was noted (in @tz@) and which note it was.
ticketBody :: TimeZone -> Note -> Text
ticketBody tz n =
  noteText n <> "\n\nNoted " <> localStamp tz (noteDate n) <> " — synapse note " <> noteId n <> "."

