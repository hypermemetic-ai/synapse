{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | The @synapse _note …@ command tree (SYNAPSE-11) over 'Synapse.Note':
--
-- @
-- add [TEXT…]                    -- the words, or stdin with none (or "-")
-- list [--all] [--json]          -- newest first; --all adds the upgraded
-- show ID [--json]
-- upgrade ID --epic E [--title T] [--kind K] [--parent P] [--backend B]
-- path                           -- where notes live
-- @
--
-- Everything but @upgrade@ is local: no backend is asked anything. An
-- upgrade creates a ticket through a 'CreateTicket' the executable hands
-- in — @tickets.create@ on the backend, reached as any synapse call is —
-- so the command, and its tests, never open a socket themselves.
module Synapse.Note.Command
  ( NoteCommand(..)
  , UpgradeOpts(..)
  , CreateTicket
  , noteCommandInfo
  , parseNoteCommand
  , runNoteCommand
  , runNoteCommandIn
  , createParams
  ) where

import Control.Monad (forM_, when)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, getTimeZone, utcToZonedTime)
import Options.Applicative
import System.Directory (getHomeDirectory)
import System.Exit (ExitCode(..))
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdin)

import Synapse.Note

data NoteCommand
  = NoteAdd [Text]
  | NoteList Bool Bool          -- ^ --all, --json
  | NoteShow Text Bool          -- ^ id, --json
  | NoteUpgrade Text UpgradeOpts
  | NotePath
  deriving (Eq, Show)

data UpgradeOpts = UpgradeOpts
  { uoEpic    :: Text
  , uoTitle   :: Maybe Text
  , uoKind    :: Maybe Text
  , uoParent  :: Maybe Text
  , uoBackend :: Text
  } deriving (Eq, Show)

-- | Create a ticket on a backend: the backend's name and @tickets.create@'s
-- params in; the new ticket's id out, or why there is none (the backend's
-- own words, or why it could not be reached).
type CreateTicket = Text -> Value -> IO (Either Text Text)

-- ============================================================================
-- Parser
-- ============================================================================

noteCommandInfo :: ParserInfo NoteCommand
noteCommandInfo = info (parseNoteCommand <**> helper)
  ( fullDesc
 <> progDesc "Notes: dated, untitled, kept in one place ($SYNAPSE_NOTES_DIR, else ~/.plexus/notes); a note can become a ticket"
 <> header "synapse _note - write a note now, make it a ticket later"
  )

parseNoteCommand :: Parser NoteCommand
parseNoteCommand = hsubparser
  ( command "add"     (info addCmd     (progDesc "Write a note: the words given, or stdin when there are none (or -)"))
 <> command "list"    (info listCmd    (progDesc "The notes not yet upgraded, newest first"))
 <> command "show"    (info showCmd    (progDesc "One note: its date, its ticket if upgraded, its text"))
 <> command "upgrade" (info upgradeCmd (progDesc "Make a note a ticket (tickets.create on the backend), and keep the note pointing at it"))
 <> command "path"    (info (pure NotePath) (progDesc "Print where notes live"))
  )
  where
    addCmd = NoteAdd <$> many (argument (T.pack <$> str) (metavar "TEXT..."))
    listCmd = NoteList
      <$> switch (long "all" <> help "Include the notes already upgraded, with their tickets")
      <*> switch (long "json" <> help "One JSON array")
    showCmd = NoteShow <$> noteIdArg <*> switch (long "json" <> help "The note as JSON")
    upgradeCmd = NoteUpgrade <$> noteIdArg <*> upgradeOpts
    noteIdArg = argument (T.pack <$> str) (metavar "ID" <> help "A note's id, or the start of exactly one")
    upgradeOpts = UpgradeOpts
      <$> strOption (long "epic" <> metavar "EPIC" <> help "The ticket's shortcode, e.g. MENGER")
      <*> optional (strOption (long "title" <> metavar "TITLE" <> help "The ticket's title (default: the note's first line)"))
      <*> optional (strOption (long "kind" <> metavar "KIND" <> help "The ticket's kind (default: the backend's)"))
      <*> optional (strOption (long "parent" <> metavar "ID" <> help "The ticket's parent (default: the backend's)"))
      <*> strOption (long "backend" <> metavar "NAME" <> value "menger" <> showDefault
                     <> help "The backend that holds tickets, reached as any synapse call is (-H/-P before _note)")

-- ============================================================================
-- Running
-- ============================================================================

-- | Runs a command against the notes directory ('notesDir').
runNoteCommand :: CreateTicket -> NoteCommand -> IO ExitCode
runNoteCommand create cmd = do
  dir <- notesDir
  runNoteCommandIn dir create cmd

-- | Runs a command against the notes in @dir@.
runNoteCommandIn :: FilePath -> CreateTicket -> NoteCommand -> IO ExitCode
runNoteCommandIn dir create = \case
  NotePath -> TIO.putStrLn (T.pack dir) >> pure ExitSuccess

  NoteAdd ws -> do
    text <- if null ws || ws == ["-"]
      then do
        tty <- hIsTerminalDevice stdin
        if tty then pure "" else TIO.getContents
      else pure (T.unwords ws)
    if T.null (T.strip text)
      then failWith "nothing to note: give the text (synapse _note add WORDS…), or pipe it in"
      else do
        now <- getCurrentTime
        note <- writeNewNote dir now text
        shown <- tilde (noteFile dir (noteId note))
        TIO.putStrLn ("noted " <> noteId note <> "  " <> shown)
        pure ExitSuccess

  NoteList everything json -> do
    notes <- readAll
    let listed = if everything then notes else filter ((== Nothing) . noteUpgrade) notes
    if json
      then LBS.putStrLn (A.encode (map noteJson listed))
      else do
        forM_ listed $ \n -> do
          tz <- getTimeZone (noteDate n)
          TIO.putStrLn (listLine tz n)
        when (null listed) $ hPutStrLn stderr ("no notes" <> (if everything then "" else " waiting") <> " in " <> dir)
    pure ExitSuccess

  NoteShow asked json -> withNote asked $ \n -> do
    if json
      then LBS.putStrLn (A.encode (noteJson n))
      else do
        tz <- getTimeZone (noteDate n)
        TIO.putStrLn (noteId n <> " · " <> localStamp tz (noteDate n))
        forM_ (noteUpgrade n) $ \u -> do
          utz <- getTimeZone (upAt u)
          TIO.putStrLn ("→ " <> upTicket u <> " (" <> upBackend u <> ", " <> localStamp utz (upAt u) <> ")")
        TIO.putStrLn ""
        TIO.putStrLn (noteText n)
    pure ExitSuccess

  NoteUpgrade asked opts -> withNote asked $ \n -> case noteUpgrade n of
    Just u -> failWith ("note " <> noteId n <> " is already a ticket: " <> upTicket u <> " (" <> upBackend u <> ")")
    Nothing -> do
      tz <- getTimeZone (noteDate n)
      made <- create (uoBackend opts) (createParams tz n opts)
      case made of
        Left why -> failWith ("note " <> noteId n <> " was not upgraded: " <> why)
        Right ticket -> do
          now <- getCurrentTime
          _ <- markUpgraded dir n (Upgrade ticket (uoBackend opts) now)
          TIO.putStrLn ("upgraded " <> noteId n <> " → " <> ticket <> "  (" <> uoBackend opts <> " tickets.create)")
          pure ExitSuccess
  where
    readAll = do
      (notes, bad) <- readNotes dir
      forM_ bad $ \(path, why) -> hPutStrLn stderr ("warning: " <> path <> " is not a note: " <> T.unpack why)
      pure notes
    withNote asked k = do
      notes <- readAll
      either failWith k (findNote notes asked)
    failWith why = TIO.hPutStrLn stderr ("error: " <> why) >> pure (ExitFailure 1)
    listLine tz n =
      let line = firstLine (noteText n)
          short = if T.length line > 70 then T.take 69 line <> "…" else line
          pointer = maybe "" (\u -> "  → " <> upTicket u) (noteUpgrade n)
      in noteId n <> "  " <> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (utcToZonedTime tz (noteDate n))) <> "  " <> short <> pointer

-- | @tickets.create@'s params for a note: its shortcode, its title (given,
-- else the note's — 'titleOf'), its body ('ticketBody', its date in @tz@),
-- and a kind and a parent only when given.
createParams :: TimeZone -> Note -> UpgradeOpts -> Value
createParams tz n opts = object $
  [ "epic" .= uoEpic opts
  , "title" .= maybe (titleOf tz n) id (nonBlank =<< uoTitle opts)
  , "body" .= ticketBody tz n
  ]
  <> catMaybes [ ("kind" .=) <$> (nonBlank =<< uoKind opts), ("parent" .=) <$> (nonBlank =<< uoParent opts) ]
  where
    nonBlank t = let s = T.strip t in if T.null s then Nothing else Just s

noteJson :: Note -> Value
noteJson n = object $
  [ "id" .= noteId n, "date" .= noteDate n, "text" .= noteText n ]
  <> maybe [] (\u -> [ "ticket" .= upTicket u, "backend" .= upBackend u, "upgraded" .= upAt u ]) (noteUpgrade n)

-- | A path with the home directory written as @~@.
tilde :: FilePath -> IO Text
tilde path = do
  home <- getHomeDirectory
  let p = T.pack path
  pure $ maybe p ("~" <>) (T.stripPrefix (T.pack home) p)

