{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for path and parameter normalization
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

-- Copy of parsePathAndParams for testing
-- We can't import from Main, so we replicate the logic
parsePathAndParams :: [Text] -> ([Text], [(Text, Text)], Bool)
parsePathAndParams = go [] [] False
  where
    go path params helpReq [] = (reverse path, reverse params, helpReq)
    go path params helpReq (x:xs)
      -- --help flag: mark help requested, don't add as param
      | x == "--help" || x == "-h" =
          go path params True xs
      -- --key value pair (value must not start with --)
      | Just key <- T.stripPrefix "--" x
      , not (T.null key)
      , (val:rest) <- xs
      , not (T.isPrefixOf "--" val) =
          let normalizedKey = T.replace "-" "_" key  -- Normalize kebab-case to snake_case
          in go path ((normalizedKey, val) : params) helpReq rest
      -- --key with no value or next arg is another flag (boolean flag)
      | Just key <- T.stripPrefix "--" x
      , not (T.null key) =
          let normalizedKey = T.replace "-" "_" key  -- Normalize kebab-case to snake_case
          in go path ((normalizedKey, "") : params) helpReq xs
      -- Regular path segment - split on dots to support plexus.cone.chat syntax
      | otherwise =
          let segments = map (T.replace "-" "_") $ filter (not . T.null) $ T.splitOn "." x
          in go (reverse segments ++ path) params helpReq xs

-- Copy of parseRawPathAndParams (the legacy _self variant) for testing.
-- Identical flag handling, but positionals are kept as whole raw tokens:
-- no dot-split and no dash-replace (defect C1). Keep in sync with
-- app/Main.hs.
parseRawPathAndParams :: [Text] -> ([Text], [(Text, Text)], Bool)
parseRawPathAndParams = go [] [] False
  where
    go path params helpReq [] = (reverse path, reverse params, helpReq)
    go path params helpReq (x:xs)
      | x == "--help" || x == "-h" =
          go path params True xs
      | Just key <- T.stripPrefix "--" x
      , not (T.null key)
      , (val:rest) <- xs
      , not (T.isPrefixOf "--" val) =
          let normalizedKey = T.replace "-" "_" key
          in go path ((normalizedKey, val) : params) helpReq rest
      | Just key <- T.stripPrefix "--" x
      , not (T.null key) =
          let normalizedKey = T.replace "-" "_" key
          in go path ((normalizedKey, "") : params) helpReq xs
      | otherwise =
          go (x : path) params helpReq xs

main :: IO ()
main = hspec $ do
  describe "parsePathAndParams - hyphen normalization" $ do

    it "normalizes hyphens in simple path segments" $ do
      let input = ["changelog", "queue-add", "--help"]
      let (path, params, helpReq) = parsePathAndParams input
      path `shouldBe` ["changelog", "queue_add"]
      params `shouldBe` []
      helpReq `shouldBe` True

    it "normalizes hyphens in dot-notation paths" $ do
      let input = ["plexus.cone.queue-add", "--name", "test"]
      let (path, params, helpReq) = parsePathAndParams input
      path `shouldBe` ["plexus", "cone", "queue_add"]
      params `shouldBe` [("name", "test")]
      helpReq `shouldBe` False

    it "normalizes hyphens in parameter names" $ do
      let input = ["cone", "create", "--working-dir", "/tmp", "--dry-run"]
      let (path, params, helpReq) = parsePathAndParams input
      path `shouldBe` ["cone", "create"]
      params `shouldBe` [("working_dir", "/tmp"), ("dry_run", "")]
      helpReq `shouldBe` False

    it "handles mixed hyphens in path and params" $ do
      let input = ["cone", "queue-list", "--max-count", "10", "--sort-by", "date"]
      let (path, params, helpReq) = parsePathAndParams input
      path `shouldBe` ["cone", "queue_list"]
      params `shouldBe` [("max_count", "10"), ("sort_by", "date")]
      helpReq `shouldBe` False

    it "normalizes complex dot-notation with hyphens" $ do
      let input = ["plexus.arbor.tree-create", "--tree-id", "abc-123"]
      let (path, params, helpReq) = parsePathAndParams input
      path `shouldBe` ["plexus", "arbor", "tree_create"]
      params `shouldBe` [("tree_id", "abc-123")]  -- Value NOT normalized, only key
      helpReq `shouldBe` False

    it "handles underscore paths unchanged" $ do
      let input = ["changelog", "queue_add", "--description", "test"]
      let (path, params, helpReq) = parsePathAndParams input
      path `shouldBe` ["changelog", "queue_add"]
      params `shouldBe` [("description", "test")]
      helpReq `shouldBe` False

    it "normalizes nested parameter paths" $ do
      let input = ["cone", "chat", "--identifier.type", "by-name", "--identifier.name", "test"]
      let (path, params, helpReq) = parsePathAndParams input
      path `shouldBe` ["cone", "chat"]
      params `shouldBe` [("identifier.type", "by-name"), ("identifier.name", "test")]
      -- Note: parameter suffixes (after .) are NOT normalized in parsePathAndParams
      -- They're handled later in the parsing pipeline
      helpReq `shouldBe` False

    it "normalizes _self template commands" $ do
      let input = ["_self", "template", "show", "cone.queue-add"]
      let (path, params, helpReq) = parsePathAndParams input
      -- Note: "cone.queue-add" splits on dot and normalizes hyphens
      path `shouldBe` ["_self", "template", "show", "cone", "queue_add"]
      params `shouldBe` []
      helpReq `shouldBe` False

    it "handles repeated array parameters with hyphens" $ do
      let input = ["queue-add", "--tags", "backend", "--tags", "critical", "--max-items", "5"]
      let (path, params, helpReq) = parsePathAndParams input
      path `shouldBe` ["queue_add"]
      params `shouldBe` [("tags", "backend"), ("tags", "critical"), ("max_items", "5")]
      helpReq `shouldBe` False

  describe "parsePathAndParams - edge cases" $ do

    it "handles empty input" $ do
      let input = []
      let (path, params, helpReq) = parsePathAndParams input
      path `shouldBe` []
      params `shouldBe` []
      helpReq `shouldBe` False

    it "preserves hyphens in parameter values" $ do
      let input = ["cone", "create", "--name", "my-test-cone"]
      let (path, params, helpReq) = parsePathAndParams input
      path `shouldBe` ["cone", "create"]
      params `shouldBe` [("name", "my-test-cone")]  -- Value preserved
      helpReq `shouldBe` False

    it "handles boolean flags with hyphens" $ do
      let input = ["test", "--verbose", "--dry-run", "--force"]
      let (path, params, helpReq) = parsePathAndParams input
      path `shouldBe` ["test"]
      params `shouldBe` [("verbose", ""), ("dry_run", ""), ("force", "")]
      helpReq `shouldBe` False

  describe "parseRawPathAndParams - legacy _self (defect C1: no dot-split)" $ do

    it "keeps an IPv4 host as a single token (the debug-mangling bug)" $ do
      -- The plain parser would split 127.0.0.1 into ["127","0","0","1"].
      let input = ["debug", "127.0.0.1", "4444", "foo"]
      let (path, params, helpReq) = parseRawPathAndParams input
      path `shouldBe` ["debug", "127.0.0.1", "4444", "foo"]
      params `shouldBe` []
      helpReq `shouldBe` False

    it "does NOT dash-replace positional tokens" $ do
      let input = ["debug", "my-host.local", "4444", "my-backend"]
      let (path, _, _) = parseRawPathAndParams input
      path `shouldBe` ["debug", "my-host.local", "4444", "my-backend"]

    it "still parses --key value flags normally" $ do
      let input = ["template", "show", "--name", "cone-thing"]
      let (path, params, helpReq) = parseRawPathAndParams input
      path `shouldBe` ["template", "show"]
      params `shouldBe` [("name", "cone-thing")]
      helpReq `shouldBe` False

    it "contrast: the dot-splitting parser mangles the same IPv4" $ do
      -- Documents WHY parseRawPathAndParams exists.
      let (path, _, _) = parsePathAndParams ["debug", "127.0.0.1", "4444", "foo"]
      path `shouldBe` ["debug", "127", "0", "0", "1", "4444", "foo"]
