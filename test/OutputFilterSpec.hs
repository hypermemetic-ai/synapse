{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Unit tests for OUTPUT-2: stream-shaping flags.
--
-- Covers:
--
-- * Filter grammar (parse pass and parse fail with column pointer)
-- * Predicate evaluation (==, !=, &&, ||, !, parens, missing field)
-- * Field projection (object, non-object, missing field -> null)
-- * ShapedJson renderer behavior:
--     - --select projection
--     - --filter pass / drop
--     - --head limit
--     - --tail bounded ring
--     - --count aggregate
--     - non-data items pass through unchanged
module Main where

import Control.Exception (bracket)
import Data.Aeson (Value(..), decode, object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.FilePath ((</>))
import System.IO (IOMode(..), hClose, hFlush, openFile, stdout)
import System.IO.Temp (getCanonicalTemporaryDirectory)

import Test.Hspec

import Synapse.Schema.Types
  ( HubStreamItem
  , pattern HubData
  , pattern HubProgress
  , pattern HubError
  , pattern HubDone
  )
import Plexus.Types (Provenance(..))

import Synapse.Output.Filter
  ( FieldName(..)
  , ShapeOpts(..)
  , defaultShapeOpts
  , Predicate(..)
  , Cmp(..)
  , CmpOp(..)
  , Literal(..)
  , parseFilter
  , evalPredicate
  , projectFields
  , shapedJsonRenderer
  , ParseFilterError(..)
  )

import Synapse.Output.Renderer (StreamRenderer(..), feedItem, finalize)

main :: IO ()
main = hspec spec

-- ============================================================================
-- Helpers
-- ============================================================================

-- | A minimal provenance for synthetic stream items.
prov :: Provenance
prov = Provenance []

-- | Build a HubData item from a JSON object content.
mkData :: Value -> HubStreamItem
mkData v = HubData "test-hash" prov "test/content" v

-- | A HubProgress sample (non-data item).
mkProgress :: Text -> HubStreamItem
mkProgress msg = HubProgress "test-hash" prov msg Nothing

-- | A HubDone sample (non-data item).
mkDone :: HubStreamItem
mkDone = HubDone "test-hash" prov

-- | Capture stdout while @act@ runs, returning its result and the captured
-- bytes as a list of NDJSON lines (terminating empty entries dropped).
captureStdoutLines :: IO a -> IO (a, [Value])
captureStdoutLines act = do
  tmpRoot <- getCanonicalTemporaryDirectory
  let capPath = tmpRoot </> "synapse-output-filter-spec-stdout.txt"
  hFlush stdout
  origStdout <- hDuplicate stdout
  r <- bracket
    (openFile capPath WriteMode)
    (\h -> do
        hFlush stdout
        hDuplicateTo origStdout stdout
        hClose origStdout
        hClose h)
    (\h -> do
        hDuplicateTo h stdout
        act)
  raw <- BS.readFile capPath
  let lbs = LBS.fromStrict raw
      lns = filter (not . LBS.null) (LBS.split '\n' lbs)
      parsed = map (fromJust . decode) lns
  pure (r, parsed)

-- | Drive a list of items through a renderer and capture the resulting
-- stdout as decoded JSON values.
runRenderer :: ShapeOpts -> [HubStreamItem] -> IO [Value]
runRenderer opts items = do
  ((), out) <- captureStdoutLines $ do
    rndr <- shapedJsonRenderer opts
    mapM_ (feedItem rndr) items
    finalize rndr
  pure out

-- ============================================================================
-- Spec
-- ============================================================================

spec :: Spec
spec = do
  describe "filter grammar parser" $ do
    it "parses a simple equality" $
      parseFilter "foo == \"bar\"" `shouldBe`
        Right (PCmp (Cmp ["foo"] OpEq (LitString "bar")))

    it "parses a not-equal with bare keyword" $
      parseFilter "has_changes != true" `shouldBe`
        Right (PCmp (Cmp ["has_changes"] OpNeq (LitBool True)))

    it "parses dotted field paths" $
      parseFilter "a.b.c == 1" `shouldBe`
        Right (PCmp (Cmp ["a","b","c"] OpEq (LitNumber 1)))

    it "parses && with both sides" $
      parseFilter "a == 1 && b == 2" `shouldBe`
        Right (PAnd
                (PCmp (Cmp ["a"] OpEq (LitNumber 1)))
                (PCmp (Cmp ["b"] OpEq (LitNumber 2))))

    it "parses || and respects precedence below &&" $
      parseFilter "a == 1 && b == 2 || c == 3" `shouldBe`
        Right (POr
                (PAnd
                  (PCmp (Cmp ["a"] OpEq (LitNumber 1)))
                  (PCmp (Cmp ["b"] OpEq (LitNumber 2))))
                (PCmp (Cmp ["c"] OpEq (LitNumber 3))))

    it "parses parens to override precedence" $
      parseFilter "a == 1 && (b == 2 || c == 3)" `shouldBe`
        Right (PAnd
                (PCmp (Cmp ["a"] OpEq (LitNumber 1)))
                (POr
                  (PCmp (Cmp ["b"] OpEq (LitNumber 2)))
                  (PCmp (Cmp ["c"] OpEq (LitNumber 3)))))

    it "parses ! negation" $
      parseFilter "!foo == \"x\"" `shouldBe`
        Right (PNot (PCmp (Cmp ["foo"] OpEq (LitString "x"))))

    it "parses null literal" $
      parseFilter "x == null" `shouldBe`
        Right (PCmp (Cmp ["x"] OpEq LitNull))

    it "rejects an empty input with a column-1 error" $
      case parseFilter "" of
        Left e  -> pfeColumn e `shouldBe` 1
        Right _ -> expectationFailure "expected parse failure"

    it "rejects 'invalid syntax here' with column-pointer error" $
      case parseFilter "invalid syntax here" of
        Left e  -> pfeColumn e `shouldSatisfy` (>= 1)
        Right _ -> expectationFailure "expected parse failure"

    it "rejects an unknown operator (=)" $
      case parseFilter "foo = 1" of
        Left e  -> pfeColumn e `shouldSatisfy` (>= 5)
        Right _ -> expectationFailure "expected parse failure for '='"

    it "rejects trailing junk" $
      case parseFilter "foo == 1 extra" of
        Left _  -> pure ()
        Right _ -> expectationFailure "expected trailing-junk failure"

  describe "predicate evaluation" $ do
    let v = object [ "foo" .= ("bar" :: Text)
                   , "n"   .= (3 :: Int)
                   , "ok"  .= True
                   , "nested" .= object [ "k" .= ("v" :: Text) ]
                   ]

    it "true on string equality" $
      evalPredicate (right (parseFilter "foo == \"bar\"")) v `shouldBe` True

    it "false on string mismatch" $
      evalPredicate (right (parseFilter "foo == \"baz\"")) v `shouldBe` False

    it "true on != for mismatched literals" $
      evalPredicate (right (parseFilter "foo != \"baz\"")) v `shouldBe` True

    it "true on number equality" $
      evalPredicate (right (parseFilter "n == 3")) v `shouldBe` True

    it "true on bool equality" $
      evalPredicate (right (parseFilter "ok == true")) v `shouldBe` True

    it "evaluates dotted paths" $
      evalPredicate (right (parseFilter "nested.k == \"v\"")) v `shouldBe` True

    it "treats missing fields as null (== \"x\" is false)" $
      evalPredicate (right (parseFilter "missing == \"x\"")) v `shouldBe` False

    it "treats missing fields as null (== null is true)" $
      evalPredicate (right (parseFilter "missing == null")) v `shouldBe` True

    it "respects && short-circuit" $
      evalPredicate (right (parseFilter "foo == \"bar\" && n == 3")) v `shouldBe` True

    it "respects || short-circuit" $
      evalPredicate (right (parseFilter "foo == \"WRONG\" || n == 3")) v `shouldBe` True

    it "negation with !" $
      evalPredicate (right (parseFilter "!foo == \"WRONG\"")) v `shouldBe` True

  describe "field projection" $ do
    let obj = object [ "a" .= (1 :: Int), "b" .= ("hi" :: Text), "c" .= True ]

    it "projects a subset of fields" $
      projectFields [FieldName "a", FieldName "c"] obj
        `shouldBe` object [ "a" .= (1 :: Int), "c" .= True ]

    it "fills missing fields with null" $
      projectFields [FieldName "a", FieldName "z"] obj
        `shouldBe` object [ "a" .= (1 :: Int), "z" .= Null ]

    it "leaves non-object content unchanged" $
      projectFields [FieldName "a"] (String "scalar")
        `shouldBe` String "scalar"

    it "passes through array content unchanged" $
      projectFields [FieldName "a"] (Array V.empty)
        `shouldBe` Array V.empty

    it "no-op when select list is empty" $
      projectFields [] obj `shouldBe` obj

  describe "ShapedJson renderer: behavior table" $ do
    let mk i = mkData $ object [ "id" .= (i :: Int), "name" .= (T.pack ("n" <> show i)) ]
        items = [mk 1, mk 2, mk 3, mk 4, mk 5]

    it "select projection emits one object per data line with only requested fields" $ do
      let opts = defaultShapeOpts { soSelect = [FieldName "id"] }
      out <- runRenderer opts items
      let contents = map extractContent out
      contents `shouldBe`
        [ object [ "id" .= (1 :: Int) ]
        , object [ "id" .= (2 :: Int) ]
        , object [ "id" .= (3 :: Int) ]
        , object [ "id" .= (4 :: Int) ]
        , object [ "id" .= (5 :: Int) ]
        ]

    it "filter drops items where predicate is false" $ do
      let p = right (parseFilter "id == 3")
          opts = defaultShapeOpts { soFilter = Just p }
      out <- runRenderer opts items
      length out `shouldBe` 1
      extractContent (head out) `shouldBe`
        object [ "id" .= (3 :: Int), "name" .= ("n3" :: Text) ]

    it "head N keeps the first N data items only" $ do
      let opts = defaultShapeOpts { soHead = Just 3 }
      out <- runRenderer opts items
      length out `shouldBe` 3
      map (extractContentField "id") out `shouldBe`
        [ Number 1, Number 2, Number 3 ]

    it "head 0 emits no data items" $ do
      let opts = defaultShapeOpts { soHead = Just 0 }
      out <- runRenderer opts items
      out `shouldBe` []

    it "tail N keeps the last N data items, in arrival order" $ do
      let opts = defaultShapeOpts { soTail = Just 2 }
      out <- runRenderer opts items
      map (extractContentField "id") out `shouldBe`
        [ Number 4, Number 5 ]

    it "count emits no data lines and one {\"count\": N} aggregate" $ do
      let opts = defaultShapeOpts { soCount = True }
      out <- runRenderer opts items
      out `shouldBe`
        [ object [ "count" .= (5 :: Int) ] ]

    it "count respects filter (counts only items that pass)" $ do
      let p = right (parseFilter "id == 2 || id == 4")
          opts = defaultShapeOpts { soFilter = Just p, soCount = True }
      out <- runRenderer opts items
      out `shouldBe`
        [ object [ "count" .= (2 :: Int) ] ]

    it "filter then projection (filter -> select)" $ do
      let p = right (parseFilter "id == 4")
          opts = defaultShapeOpts
                   { soFilter = Just p
                   , soSelect = [FieldName "name"]
                   }
      out <- runRenderer opts items
      length out `shouldBe` 1
      extractContent (head out) `shouldBe` object [ "name" .= ("n4" :: Text) ]

    it "non-data items (HubProgress, HubDone) pass through unchanged" $ do
      let opts = defaultShapeOpts { soHead = Just 1 }
          mixed = [mkProgress "starting", mk 1, mkProgress "halfway", mk 2, mkDone]
      out <- runRenderer opts mixed
      -- Expect: progress, data id=1, progress, done. The second data item
      -- is dropped because --head 1 was set.
      length out `shouldBe` 4
      -- Sanity: types of items in order
      map (extractField "type") out `shouldBe`
        [ String "progress"
        , String "data"
        , String "progress"
        , String "done"
        ]

  describe "ShapedJson renderer: --tail edge cases" $ do
    it "tail N with fewer items emits all items in arrival order" $ do
      let mk i = mkData $ object [ "id" .= (i :: Int) ]
          opts = defaultShapeOpts { soTail = Just 10 }
      out <- runRenderer opts [mk 1, mk 2]
      map (extractContentField "id") out `shouldBe` [ Number 1, Number 2 ]

    it "tail buffer respects soTailCap (truncates older items)" $ do
      let mk i = mkData $ object [ "id" .= (i :: Int) ]
          opts = defaultShapeOpts { soTail = Just 100, soTailCap = 3 }
          items = [ mk i | i <- [1..10] ]
      out <- runRenderer opts items
      -- Cap is 3 so only the last three (ids 8, 9, 10) survive.
      map (extractContentField "id") out `shouldBe`
        [ Number 8, Number 9, Number 10 ]

-- ============================================================================
-- Test helpers
-- ============================================================================

right :: Show e => Either e a -> a
right (Right a) = a
right (Left e)  = error $ "expected Right, got Left: " <> show e

-- | Pull the @content@ field out of a @HubData@-shaped JSON value.
-- For @--count@ output (a plain @{"count": N}@ object), returns the
-- whole value.
extractContent :: Value -> Value
extractContent v = case v of
  Object o -> case KM.lookup "content" o of
    Just c  -> c
    Nothing -> Object o
  _ -> v

-- | Pull a field from a @HubStreamItem@-shaped JSON value.
extractField :: Text -> Value -> Value
extractField k v = case v of
  Object o -> maybe Null id (KM.lookup (K.fromText k) o)
  _        -> v

-- | For @HubData@-shaped JSON, fetch a field nested inside @content@.
extractContentField :: Text -> Value -> Value
extractContentField k v = extractField k (extractContent v)
