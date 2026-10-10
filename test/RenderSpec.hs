{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | SYNAPSE-12: a backend's help, laid out — no runs of blank lines, the
-- sections under their headers, parameters a step in under their method,
-- required before optional, wrapped text in its column, nothing wider
-- than the width, and a parameter's type as its schema says it.
module Main (main) where

import Data.Aeson (Value)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec

import Plexus.Schema.Recursive (ChildSummary(..), MethodRole(..), MethodSchema(..), PluginSchema(..))
import Synapse.Algebra.Render (renderSchema)

method :: Text -> Text -> Maybe Value -> MethodSchema
method name desc params = MethodSchema
  { methodName = name, methodDescription = desc, methodHash = "h", methodParams = params
  , methodReturns = Nothing, methodStreaming = True, methodBidirectional = False
  , methodRequestType = Nothing, methodResponseType = Nothing, methodDeprecation = Nothing
  , methodParamSchemas = Nothing, methodRole = MethodRoleRpc }

hub :: PluginSchema
hub = PluginSchema
  { psNamespace = "menger", psVersion = "1.0.0"
  , psDescription = "Central routing and introspection"
  , psLongDescription = Nothing, psHash = "h0"
  , psMethods =
      [ method "find" "Find tickets whose id or title contains the query (case-insensitive), optionally under a filter; summary rows, paged with after + limit" (Just findParams)
      , method "health" "Heartbeat" Nothing
      , method "link_commits" "Link commits to a ticket" (Just linkParams)
      ]
  , psChildren = Just
      [ ChildSummary "tickets" "Plans as data — create, query, and watch the work DAG" "h1"
      , ChildSummary "comments" "Comments on the app and on ticket bodies — add, list (filtered), resolve, reopen, watch" "h2"
      ]
  , psDeprecation = Nothing
  }

findParams, linkParams :: Value
findParams = [aesonQQ|{
  "type": "object",
  "properties": {
    "after": {"type": ["string", "null"], "description": "Cursor: start strictly after this ticket id, e.g. MNG-9 (optional)"},
    "limit": {"type": ["integer", "null"], "format": "uint64", "description": "At most this many tickets (default 50); the page event carries the next cursor (optional)"},
    "query": {"type": "string", "description": "Substring of the ticket id or title, case-insensitive (ASCII), trimmed; empty matches every ticket"}
  },
  "required": ["query"]
}|]
linkParams = [aesonQQ|{
  "type": "object",
  "properties": {
    "commits": {"type": "array", "items": {"type": "string"}, "description": "Commit references"},
    "value": {"$ref": "#/$defs/InputValue", "description": "Typed"},
    "id": {"type": "string", "description": "Ticket id"}
  },
  "required": ["id", "commits"]
}|]

out :: Text
out = renderSchema hub

ls :: [Text]
ls = T.lines out

lineWith :: Text -> Text
lineWith needle = fromJust (lookupLine needle)
  where lookupLine n = case filter (T.isInfixOf n) ls of { (l : _) -> Just l; [] -> Nothing }

indexOf :: Text -> Int
indexOf needle = length (takeWhile (not . T.isInfixOf needle) ls)

col :: Text -> Text -> Int
col needle l = T.length (fst (T.breakOn needle l))

main :: IO ()
main = do
  TIO.putStrLn out
  hspec $ describe "renderSchema (SYNAPSE-12)" $ do
    it "never leaves two blank lines in a row" $
      ("\n\n\n" `T.isInfixOf` out) `shouldBe` False

    it "is the backend's name, version and description, then its sections" $ do
      take 2 ls `shouldBe` ["menger v1.0.0", "  Central routing and introspection"]
      filter (`elem` ["activations", "methods"]) ls `shouldBe` ["activations", "methods"]

    it "starts every description in one column, across both sections" $ do
      let cols = [col "Plans as data" (lineWith "Plans as data"), col "Heartbeat" (lineWith "Heartbeat"), col "Find tickets" (lineWith "Find tickets")]
      cols `shouldSatisfy` (\cs -> and (zipWith (==) cs (drop 1 cs)))

    it "keeps a wrapped description in its column" $ do
      let i = indexOf "Find tickets"
          next = ls !! (i + 1)
      T.length (T.takeWhile (== ' ') next) `shouldBe` col "Find tickets" (ls !! i)

    it "puts parameters a step in under their method, required before optional" $ do
      let q = indexOf "--query"
          a = indexOf "[--after"
          l = indexOf "[--limit"
      (q < a, a < l) `shouldBe` (True, True)
      map (T.length . T.takeWhile (== ' ') . lineWith) ["--query", "[--after", "[--limit"] `shouldBe` [6, 6, 6]

    it "aligns parameter descriptions and their wrapped lines" $ do
      let c = col "Substring" (lineWith "--query")
      col "Cursor" (lineWith "[--after") `shouldBe` c
      let i = indexOf "[--limit"
      T.length (T.takeWhile (== ' ') (ls !! (i + 1))) `shouldBe` c

    it "names a parameter's type as its schema does" $ do
      lineWith "--after" `shouldSatisfy` T.isInfixOf "[--after <string>]"
      lineWith "--limit" `shouldSatisfy` T.isInfixOf "[--limit <integer>]"
      lineWith "--commits" `shouldSatisfy` T.isInfixOf "--commits <string[]>"
      lineWith "--value" `shouldSatisfy` T.isInfixOf "[--value <InputValue>]"

    it "fits in 80 columns" $
      filter ((> 80) . T.length) ls `shouldBe` []

    it "carries no synapse banner or option list" $
      ("Usage:" `isInfixOf` T.unpack out, "Meta-commands" `isInfixOf` T.unpack out) `shouldBe` (False, False)
