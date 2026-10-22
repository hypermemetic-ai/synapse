{-# LANGUAGE OverloadedStrings #-}

-- | SYNAPSE-17: @synapse _find@.
--
-- * the matcher: exact, prefix, a typo at both thresholds, a snake_case
--   subsequence, every word required, and the ranking;
-- * the renders, against the mock hub's catalog (menger's tickets and
--   deployments as its schema had them on 2026-10-15, test/fixtures):
--   each of the ticket's examples piped (no colour codes) and on a
--   terminal (the matched characters bold);
-- * a caller's view: a method outside it is never a hit;
-- * the IR cache: a second search costs one schema call, the hash check.
--
-- @SYNAPSE_MOCK_HUB=catalog|catalog-caller@ serves the catalog hub instead
-- of running the tests (it prints its port), for transcripts by hand.
module Main (main) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Katip
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec

import Synapse.Algebra.Render (fitText, summarize)
import Synapse.Find
import Synapse.IR.Cache (CacheConfig(..), newIRCache)
import Synapse.IR.Types
import qualified Synapse.Log as Log
import Synapse.Monad (initEnv, runSynapseM, withViewKey)

import MockHub

main :: IO ()
main = do
  serve <- lookupEnv "SYNAPSE_MOCK_HUB"
  dump <- lookupEnv "SYNAPSE_FIND_DUMP"
  case (serve, dump) of
    (Just "catalog", _) -> serveMockHub (catalogHub NoView)
    (Just "catalog-caller", _) -> serveMockHub (catalogHub CallerView)
    (_, Just _) -> dumpAll
    _ -> hspec spec

-- ============================================================================
-- Fixtures
-- ============================================================================

-- | The catalog as a view of the mock hub has it (no cache).
catalog :: ViewMode -> IO IR
catalog v = withMockHub (catalogHub v) $ \port -> do
  logger <- Log.makeLogger Katip.ErrorS
  env0 <- initEnv "127.0.0.1" port "menger" logger Nothing
  let env = withViewKey (if v == CallerView then Just "v-9a77" else Nothing) env0
  r <- runSynapseM env (loadCatalog Nothing)
  either (\e -> fail (show e)) pure r

opts :: [Text] -> FindOpts
opts ws = FindOpts "menger" ws defaultLimit False False

-- | _find menger WORDS, as synapse -P 4455 says it, piped or on a terminal.
page :: IR -> Bool -> FindOpts -> Text
page ir bold fo = anText (answer "synapse -P 4455" "menger" bold fo ir)

stripEsc :: Text -> Text
stripEsc = T.replace "\ESC[1m" "" . T.replace "\ESC[22m" ""

-- | The ticket's examples: how each is typed, its golden file, the opts,
-- and a stretch of its terminal page that must be bold.
examples :: [(String, FilePath, FindOpts, Text)]
examples =
  [ ("snooze", "snooze", opts ["snooze"], "tickets \ESC[1msnooze\ESC[22m")
  , ("comit", "comit", opts ["comit"], "link_\ESC[1mcommit\ESC[22ms")
  , ("queue urgency", "queue-urgency", opts ["queue", "urgency"], "--\ESC[1murgency\ESC[22m <string>")
  , ("tickets setpar", "tickets-setpar", opts ["tickets", "setpar"], "\ESC[1mset\ESC[22m_\ESC[1mpar\ESC[22ment")
  , ("--params filter", "params-filter", (opts ["filter"]) { foParams = True }, "[--\ESC[1mfilter\ESC[22m <string>]")
  , ("reschedule", "reschedule", opts ["reschedule"], "")
    -- a quoted example is never cut inside: 'category in (active, review)'
    -- does not fit, so the line ends before it
  , ("watch filter", "watch-filter", opts ["watch", "filter"], "--\ESC[1mfilter\ESC[22m <string>")
  ]

golden :: FilePath -> FilePath
golden name = "test/fixtures/find-golden" </> name <> ".txt"

-- | Write the goldens again (SYNAPSE_FIND_DUMP=1), from the pages as they
-- render now: read them before committing.
dumpAll :: IO ()
dumpAll = do
  ir <- catalog NoView
  createDirectoryIfMissing True "test/fixtures/find-golden"
  mapM_ (\(_, file, fo, _) -> TIO.writeFile (golden file) (page ir False fo)) examples
  LBS.writeFile (golden "rationale-json") (LBS.unlines (map encode (anJson (answer "synapse -P 4455" "menger" False (opts ["rationale"]) ir))))

-- | A method as the IR has it, for the ranking's synthetic catalog.
method :: Text -> Text -> Text -> MethodDef
method ns name desc = MethodDef
  { mdName = name, mdFullPath = ns <> "." <> name, mdNamespace = ns
  , mdDescription = Just desc, mdStreaming = False, mdParams = []
  , mdReturns = RefUnknown, mdBidirType = Nothing, mdBidirResponseType = Nothing
  , mdBidirResponseSchema = Nothing, mdRole = MethodRoleRpc
  , mdGrant = Nothing, mdTarget = Nothing, mdLongDescription = Nothing
  }

synthetic :: IR
synthetic = emptyIR { irMethods = Map.fromList [ (mdFullPath m, m) | m <- ms ] }
  where
    ms =
      [ method "tickets" "find" "One by id"
      , method "x" "find" "One by id, elsewhere"
      , method "tickets" "find_all" "Every one"
      , method "tickets" "fi_nder" "A finder"
      , method "tickets" "fine" "A fine"
      , method "tickets" "lookup" "Find things by name"
      , method "tickets" "other" "Nothing to see"
      ]

kinds :: Text -> Text -> Maybe MatchKind
kinds w n = wmKind <$> matchName w n

-- ============================================================================
-- The spec
-- ============================================================================

spec :: Spec
spec = do
  describe "the matcher" $ do
    it "an exact name" $
      kinds "snooze" "snooze" `shouldBe` Just Exact
    it "a prefix of the name, a segment, a segment's start" $ do
      kinds "snooze" "snoozed" `shouldBe` Just Prefix
      kinds "commits" "link_commits" `shouldBe` Just SegmentExact
      kinds "commit" "link_commits" `shouldBe` Just SegmentPrefix
    it "a typo of 4-7 letters: one edit, said as the name it meant" $ do
      fmap (\m -> (wmKind m, wmTaken m)) (matchName "comit" "link_commits") `shouldBe` Just (Typo 1, "commit")
      kinds "snoze" "snooze" `shouldBe` Just (Typo 1)
      kinds "unlnik" "unlink_commits" `shouldBe` Nothing   -- two edits at 6 letters
    it "a typo beyond 7 letters: two edits" $ do
      kinds "ratoinale" "rationale" `shouldBe` Just (Typo 2)
      kinds "rationle" "rationale" `shouldBe` Just (Typo 1)
      kinds "rtoinael" "rationale" `shouldBe` Nothing
    it "no typo under 4 letters" $ do
      typoLimit 3 `shouldBe` 0
      (typoLimit 4, typoLimit 7, typoLimit 8) `shouldBe` (1, 1, 2)
      kinds "snz" "snooze" `shouldBe` Nothing
    it "a snake_case subsequence: prefixes of its segments, in order" $ do
      matchName "setpar" "set_parent" `shouldBe` Just (WordMatch Subsequence "set_parent" [(0, 3), (4, 3)])
      kinds "lc" "link_commits" `shouldBe` Just Subsequence
      kinds "parset" "set_parent" `shouldBe` Nothing
    it "a word of a description, or its start" $ do
      fmap wmSpans (matchDescription "snooze" "Include the snoozed ones") `shouldBe` Just [(12, 6)]
      matchDescription "zz" "zz top" `shouldBe` Nothing
    it "every word must match" $ do
      ir <- catalog NoView
      let hits ws = search OnlyAll (catalogEntries [] ir) ws
          where' h = (entryLocation (hitEntry h), pdName <$> enParam (hitEntry h))
      map where' (hits ["queue", "urgency"]) `shouldBe` [("tickets queue", Just "urgency")]
      hits ["queue", "zzzz"] `shouldBe` []
    it "ranks exact > prefix > subsequence / typo > description; ties by the shorter path, then alphabetically" $ do
      let ranked = map (entryLocation . hitEntry) (search OnlyAll (catalogEntries [] synthetic) ["find"])
      ranked `shouldBe` ["x find", "tickets find", "tickets find_all", "tickets fi_nder", "tickets fine", "tickets lookup"]

  describe "a description cut to the line (fitText): never inside a quote" $ do
    let watchFilter = "A filter, e.g. 'category in (active, review)' (optional): adds Entered / Left for each ticket a write moves into or out of it, or Recount past 256"
    it "ends before a quote that does not fit, its brackets untouched" $ do
      fitText 36 (summarize watchFilter) `shouldBe` "A filter, e.g.\x2026"
      fitText 50 (summarize watchFilter) `shouldBe` "A filter, e.g. 'category in (active, review)'"
    it "a quote that fits stays whole" $
      fitText 40 "A filter, e.g. 'not category done' (optional)" `shouldBe` "A filter, e.g. 'not category done'"
    it "a backtick span is a quote too; an apostrophe is not" $ do
      fitText 26 "Run `set_status (x, y)` with the ticket's id (optional)" `shouldBe` "Run `set_status (x, y)`\x2026"
      fitText 30 "The ticket's own words (optional) and more words" `shouldBe` "The ticket's own words and\x2026"
    it "summarize does not stop inside a quote" $
      summarize "Run `a. b` then stop. More" `shouldBe` "Run `a. b` then stop"

  describe "the pages, piped and on a terminal (the mock hub's catalog)" $ do
    mapM_ (\(typed, file, fo, boldBit) -> do
      it ("_find menger " <> typed <> ", piped: as pinned, no colour codes") $ do
        ir <- catalog NoView
        want <- TIO.readFile (golden file)
        let got = page ir False fo
        got `shouldBe` want
        T.any (== '\ESC') got `shouldBe` False
      it ("_find menger " <> typed <> ", on a terminal: the same, the matched characters bold") $ do
        ir <- catalog NoView
        want <- TIO.readFile (golden file)
        let got = page ir True fo
        stripEsc got `shouldBe` want
        if T.null boldBit then pure () else T.isInfixOf boldBit got `shouldBe` True
      ) examples

    it "-j: one JSON object per hit, best first" $ do
      ir <- catalog NoView
      want <- LBS.readFile (golden "rationale-json")
      LBS.unlines (map encode (anJson (answer "synapse -P 4455" "menger" False (opts ["rationale"]) ir))) `shouldBe` want

    it "--limit: at most N, and the footer says how many more" $ do
      ir <- catalog NoView
      let t = page ir False ((opts ["filter"]) { foParams = True, foLimit = 3 })
      T.lines t `shouldContain` ["6 more: '--limit 9' shows them all."]
      length (filter ("  tickets " `T.isPrefixOf`) (T.lines t)) `shouldBe` 3

    it "--methods: only methods" $ do
      ir <- catalog NoView
      let t = page ir False ((opts ["commit"]) { foMethods = True })
      T.lines t `shouldContain` ["METHODS"]
      T.lines t `shouldNotContain` ["PARAMETERS"]

    it "a description's match is MENTIONED IN, the word shown" $ do
      ir <- catalog NoView
      let t = page ir False (opts ["rationale"])
      T.lines t `shouldContain` ["MENTIONED IN"]
      T.isInfixOf "rationale" (T.unlines (dropWhile (/= "MENTIONED IN") (T.lines t))) `shouldBe` True

  describe "under login (a caller's view)" $ do
    it "a method outside the caller's view is never a hit" $ do
      outside <- catalog NoView
      inside <- catalog CallerView
      let paths ir = [ mdFullPath (enMethod (hitEntry h)) | h <- search OnlyAll (catalogEntries [] ir) ["delete"] ]
      paths outside `shouldContain` ["tickets.delete"]
      paths inside `shouldNotContain` ["tickets.delete"]
      anJson (answer "synapse" "menger" False (opts ["delete"]) inside) `shouldSatisfy` all (not . T.isInfixOf "tickets.delete" . T.pack . LBS.unpack . encode)

  describe "the schema, from the IR cache when its hash is current" $ do
    it "a second search costs one schema call: the hash check" $ do
      tmp <- getTemporaryDirectory
      let dir = tmp </> "synapse-find-test-cache"
      exists <- doesDirectoryExist dir
      if exists then removeDirectoryRecursive dir else pure ()
      cache <- newIRCache (CacheConfig True 100 True dir)
      withMockHub (catalogHub NoView) $ \port -> do
        logger <- Log.makeLogger Katip.ErrorS
        env <- initEnv "127.0.0.1" port "menger" logger Nothing
        before <- readIORef schemaCalls
        Right first <- runSynapseM env (loadCatalog (Just cache))
        middle <- readIORef schemaCalls
        -- a fresh cache on the same directory: from disk, as the next process
        -- reads it (SYNAPSE-28: and a fresh env — a run fetches each path once)
        cache' <- newIRCache (CacheConfig True 100 True dir)
        env' <- initEnv "127.0.0.1" port "menger" logger Nothing
        Right second <- runSynapseM env' (loadCatalog (Just cache'))
        after <- readIORef schemaCalls
        (middle - before) `shouldSatisfy` (>= 3)
        (after - middle) `shouldBe` 1
        Map.keys (irMethods second) `shouldBe` Map.keys (irMethods first)
        page second False (opts ["comit"]) `shouldBe` page first False (opts ["comit"])
