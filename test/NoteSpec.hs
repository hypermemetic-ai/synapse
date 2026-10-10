{-# LANGUAGE OverloadedStrings #-}

-- | SYNAPSE-11: notes — the file a note is, its id and its neighbours in
-- the same second, the order they are listed in, finding one by a prefix,
-- the title and body it gives a ticket, and every way an upgrade can go,
-- the backend a stub. Each test runs in a directory of its own; nothing
-- touches ~/.plexus or a socket.
module Main (main) where

import Control.Monad (forM_, replicateM_)
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, addUTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Time.LocalTime (utc)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Synapse.Note
import Synapse.Note.Command

at :: String -> UTCTime
at s = maybe (error ("bad time " <> s)) id (iso8601ParseM s)

t0 :: UTCTime
t0 = at "2026-10-10T15:03:12Z"

note :: Text -> Note
note text = Note { noteId = "20261010-150312", noteDate = t0, noteText = text, noteUpgrade = Nothing }

withNotes :: (FilePath -> IO a) -> IO a
withNotes k = withSystemTempDirectory "synapse-notes" $ \tmp -> k (tmp </> "notes")

-- | A backend stub: records what it was asked, answers what it is told.
stub :: Either Text Text -> IO (IORef [(Text, Value)], CreateTicket)
stub answer = do
  asked <- newIORef []
  pure (asked, \backend params -> modifyIORef asked (<> [(backend, params)]) >> pure answer)

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (K.fromText k) o
field _ _ = Nothing

opts :: UpgradeOpts
opts = UpgradeOpts { uoEpic = "MENGER", uoTitle = Nothing, uoKind = Nothing, uoParent = Nothing, uoBackend = "menger" }

main :: IO ()
main = hspec $ do
  describe "the file" $ do
    it "holds the date and the text, and nothing that titles it" $ do
      renderNote (note "the palette should remember its query")
        `shouldBe` "---\ndate: 2026-10-10T15:03:12Z\n---\nthe palette should remember its query\n"

    it "reads back what it wrote, upgraded or not" $ do
      let plain = note "line one\n\n---\nnot a frontmatter: a rule\nlast"
          upgraded = plain { noteUpgrade = Just (Upgrade "MENGER-41" "menger" (at "2026-10-10T16:00:00Z")) }
      forM_ [plain, upgraded, note ""] $ \n ->
        parseNote (noteId n) (renderNote n) `shouldBe` Right n

    it "takes CRLF, ignores keys it does not know, and refuses what is not a note" $ do
      parseNote "x" "---\r\ndate: 2026-10-10T15:03:12Z\r\nmood: fine\r\n---\r\nhello\r\n"
        `shouldBe` Right Note { noteId = "x", noteDate = t0, noteText = "hello", noteUpgrade = Nothing }
      parseNote "x" "hello" `shouldBe` Left "it does not open with a --- frontmatter"
      parseNote "x" "---\nmood: fine\n---\nhello\n" `shouldBe` Left "it has no date"
      parseNote "x" "---\ndate: 2026-10-10T15:03:12Z\nhello\n" `shouldBe` Left "its frontmatter never closes"
      parseNote "x" "---\ndate: 2026-10-10T15:03:12Z\nticket: MENGER-41\n---\nhello\n"
        `shouldBe` Left "it names a ticket without its backend and when"

    it "is named for the moment it was taken, in UTC" $
      noteIdAt t0 `shouldBe` "20261010-150312"

  describe "writing" $ do
    it "makes the directory, writes one file, leaves no temporary behind, drops trailing space" $ withNotes $ \dir -> do
      n <- writeNewNote dir t0 "a thought  \n\n"
      noteId n `shouldBe` "20261010-150312"
      noteText n `shouldBe` "a thought"
      listDirectory dir `shouldReturn` ["20261010-150312.md"]
      TIO.readFile (noteFile dir (noteId n)) `shouldReturn` renderNote n

    it "gives a second note in the same second -2, a third -3 — never over another" $ withNotes $ \dir -> do
      ids <- mapM (\k -> noteId <$> writeNewNote dir t0 (T.pack (show k))) [1 :: Int .. 3]
      ids `shouldBe` ["20261010-150312", "20261010-150312-2", "20261010-150312-3"]
      (notes, _) <- readNotes dir
      map noteText notes `shouldBe` ["3", "2", "1"]

  describe "reading" $ do
    it "lists newest first — and, in one second, -10 after -9" $ withNotes $ \dir -> do
      _ <- writeNewNote dir (addUTCTime (-60) t0) "older"
      replicateM_ 11 (writeNewNote dir t0 "same second")
      (notes, bad) <- readNotes dir
      bad `shouldBe` []
      map noteId (take 3 notes) `shouldBe` ["20261010-150312-11", "20261010-150312-10", "20261010-150312-9"]
      noteText (last notes) `shouldBe` "older"

    it "holds none in a directory that is not there, and says which files are not notes" $ withNotes $ \dir -> do
      readNotes dir >>= \(ns, bad) -> (ns, bad) `shouldBe` ([], [])
      createDirectoryIfMissing True dir
      TIO.writeFile (dir </> "stray.md") "no frontmatter"
      TIO.writeFile (dir </> ".hidden.md") "ignored"
      TIO.writeFile (dir </> "readme.txt") "ignored"
      _ <- writeNewNote dir t0 "a note"
      (notes, bad) <- readNotes dir
      map noteText notes `shouldBe` ["a note"]
      map fst bad `shouldSatisfy` \ps -> length ps == 1 && "stray.md" `isSuffixOf` head ps

    it "finds a note by its id or the start of exactly one" $ do
      let a = (note "a") { noteId = "20261010-150312" }
          b = (note "b") { noteId = "20261010-150312-2" }
          c = (note "c") { noteId = "20261011-090000" }
          ns = [a, b, c]
      findNote ns "20261010-150312" `shouldBe` Right a
      findNote ns "20261011" `shouldBe` Right c
      findNote ns " 20261010-150312-2 " `shouldBe` Right b
      findNote ns "2026101" `shouldBe` Left "2026101 could be any of 20261010-150312, 20261010-150312-2, 20261011-090000"
      findNote ns "1999" `shouldBe` Left "no note 1999"
      findNote ns "  " `shouldBe` Left "name a note: its id, or the start of one"

  describe "as a ticket" $ do
    it "is titled by its first line, its markdown marks dropped" $ do
      titleOf utc (note "\n\n## watch drops **events**\nmore") `shouldBe` "watch drops events"
      titleOf utc (note "- a bullet") `shouldBe` "a bullet"
      titleOf utc (note "> a quote") `shouldBe` "a quote"
      titleOf utc (note "-5 degrees outside") `shouldBe` "-5 degrees outside"

    it "cuts a long first line at a word, within 80 characters" $ do
      let long = T.unwords (replicate 30 "word")
          t = titleOf utc (note long)
      T.length t `shouldSatisfy` (<= maxTitle)
      t `shouldSatisfy` T.isSuffixOf "word…"
      titleOf utc (note (T.replicate 100 "x")) `shouldBe` T.replicate 79 "x" <> "…"

    it "is titled by its date when it has no words — a rule is not words" $ do
      titleOf utc (note "  \n---\n") `shouldBe` "Note of 2026-10-10 15:03 UTC"
      titleOf utc (note "---\n***\nthe first words") `shouldBe` "the first words"

    it "carries its text, then when it was noted and which note it was" $
      ticketBody utc (note "hello") `shouldBe` "hello\n\nNoted 2026-10-10 15:03 UTC — synapse note 20261010-150312."

    it "asks tickets.create for a kind and a parent only when given" $ do
      let p = createParams utc (note "hello") opts
      field "epic" p `shouldBe` Just "MENGER"
      field "title" p `shouldBe` Just "hello"
      field "kind" p `shouldBe` Nothing
      field "parent" p `shouldBe` Nothing
      let q = createParams utc (note "hello") opts { uoTitle = Just " Given ", uoKind = Just "gap", uoParent = Just "MENGER-2" }
      (field "title" q, field "kind" q, field "parent" q) `shouldBe` (Just "Given", Just "gap", Just "MENGER-2")
      field "title" (createParams utc (note "hello") opts { uoTitle = Just "  " }) `shouldBe` Just "hello"

  describe "the command" $ do
    it "adds a note from its words" $ withNotes $ \dir -> do
      (_, create) <- stub (Left "unused")
      runNoteCommandIn dir create (NoteAdd ["remember", "the", "query"]) `shouldReturn` ExitSuccess
      (notes, _) <- readNotes dir
      map noteText notes `shouldBe` ["remember the query"]

    it "upgrades a note: the backend asked once, the note pointing at its ticket, out of the list" $ withNotes $ \dir -> do
      n <- writeNewNote dir t0 "## watch drops events\ndetails"
      (asked, create) <- stub (Right "MENGER-41")
      runNoteCommandIn dir create (NoteUpgrade "20261010" opts { uoParent = Just "MENGER-2" }) `shouldReturn` ExitSuccess
      [(backend, params)] <- readIORef asked
      backend `shouldBe` "menger"
      field "title" params `shouldBe` Just "watch drops events"
      field "parent" params `shouldBe` Just "MENGER-2"
      field "body" params `shouldSatisfy` maybe False (\(String b) -> "## watch drops events\ndetails\n\nNoted " `T.isPrefixOf` b)
      (notes, _) <- readNotes dir
      map (fmap upTicket . noteUpgrade) notes `shouldBe` [Just "MENGER-41"]
      map noteText notes `shouldBe` [noteText n]
      runNoteCommandIn dir create (NoteList False False) `shouldReturn` ExitSuccess

    it "leaves a note as it was when the backend refuses" $ withNotes $ \dir -> do
      n <- writeNewNote dir t0 "hello"
      before <- TIO.readFile (noteFile dir (noteId n))
      (_, create) <- stub (Left "invalid epic 'BAD EPIC'")
      runNoteCommandIn dir create (NoteUpgrade (noteId n) opts { uoEpic = "BAD EPIC" }) `shouldReturn` ExitFailure 1
      TIO.readFile (noteFile dir (noteId n)) `shouldReturn` before

    it "refuses a note already upgraded without asking the backend, and an id that names none" $ withNotes $ \dir -> do
      n <- writeNewNote dir t0 "hello"
      _ <- markUpgraded dir n (Upgrade "MENGER-41" "menger" t0)
      (asked, create) <- stub (Right "MENGER-42")
      runNoteCommandIn dir create (NoteUpgrade (noteId n) opts) `shouldReturn` ExitFailure 1
      runNoteCommandIn dir create (NoteUpgrade "1999" opts) `shouldReturn` ExitFailure 1
      runNoteCommandIn dir create (NoteShow "1999" False) `shouldReturn` ExitFailure 1
      readIORef asked `shouldReturn` []
      listDirectory dir >>= \fs -> sort fs `shouldBe` [T.unpack (noteId n) <> ".md"]

    it "prints where notes live without making the directory" $ withNotes $ \dir -> do
      (_, create) <- stub (Left "unused")
      runNoteCommandIn dir create NotePath `shouldReturn` ExitSuccess
      doesDirectoryExist dir `shouldReturn` False
