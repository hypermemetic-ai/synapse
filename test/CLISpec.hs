{-# LANGUAGE OverloadedStrings #-}

-- | CLI integration tests
--
-- Pattern: args `has` ["expected", "substrings"]
module Main where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcess)
import Test.Hspec

main :: IO ()
main = hspec $ do

  describe "navigation" $ do
    it "root"              $ ["--help"]                              `has` ["synapse", "backend"]
    it "substrate"            $ ["substrate"]                              `has` ["echo", "solar", "health"]
    it "echo"              $ ["substrate", "echo"]                      `has` ["echo", "Echo messages back"]
    it "solar"             $ ["substrate", "solar"]                     `has` ["solar", "Solar system model"]
    it "health"            $ ["substrate", "health"]                    `has` ["health", "health and uptime"]
    it "solar earth"       $ ["substrate", "solar", "earth"]            `has` ["earth", "planet"]
    it "solar earth luna"  $ ["substrate", "solar", "earth", "luna"]    `has` ["luna", "Moon"]

  describe "method help" $ do
    it "echo once"    $ ["substrate", "echo", "once"]    `has` ["once", "--message", "required"]
    it "echo echo"    $ ["substrate", "echo", "echo"]    `has` ["--message", "--count"]
    -- health.check has no required params, so it auto-invokes
    it "health check" $ ["substrate", "health", "check"] `has` ["healthy"]

  describe "invocation" $ do
    it "echo once"     $ call ["substrate", "echo", "once"] (msg "test")       `has` ["test"]
    it "echo count"    $ call ["substrate", "echo", "echo"] (msgN "hi" 2)      `has` ["hi", "2"]
    it "health"        $ callRaw ["substrate", "health", "check"] "{}"         `has` ["healthy"]
    it "solar observe" $ callRaw ["substrate", "solar", "observe"] "{}"        `has` ["sol", "planet_count"]
    it "luna info"     $ callRaw ["substrate", "solar", "earth", "luna", "info"] "{}" `has` ["luna"]

-- ============================================================================
-- Harness
-- ============================================================================

synapse :: FilePath
synapse = "dist-newstyle/build/aarch64-osx/ghc-9.6.7/plexus-synapse-0.3.0.0/x/synapse/build/synapse/synapse"

-- | Assert synapse output contains all substrings
has :: [String] -> [Text] -> Expectation
has = checkOutput synapse

-- | Generic output checker
checkOutput :: FilePath -> [String] -> [Text] -> Expectation
checkOutput bin args expected = do
  out <- T.pack <$> readProcess bin args ""
  mapM_ (assertContains out) expected

assertContains :: Text -> Text -> Expectation
assertContains haystack needle =
  T.toLower haystack `shouldSatisfy` T.isInfixOf (T.toLower needle)

-- | Build invocation args (options before backend due to noIntersperse)
call :: [String] -> String -> [String]
call path params = ["-p", params] ++ path

-- | Build invocation args with --raw (options before backend)
callRaw :: [String] -> String -> [String]
callRaw path params = ["--raw", "-p", params] ++ path

-- | JSON builders
msg :: String -> String
msg m = "{\"message\":\"" <> m <> "\"}"

msgN :: String -> Int -> String
msgN m n = "{\"message\":\"" <> m <> "\",\"count\":" <> show n <> "}"
