{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | SYNAPSE-13: a backend's help in the gh / cargo shape.
--
-- A listing (@synapse … menger tickets@) is a usage line, then one line per
-- activation and per method — its name and summary, no flags — the methods
-- in their groups, and how to see a method's options. A method's page
-- (@… tickets set_status --help@) is the description, the long description,
-- a usage line naming the required flags, aligned options with their types
-- and allowed values, examples as command lines (the schema's, else one
-- synthesised), and, in a caller's view, what it requires. A schema from a
-- hub that says none of the new fields still renders sensibly.
module Main (main) where

import Data.Aeson (Value)
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec

import Plexus.Schema.Recursive (ChildSummary(..), MethodExample(..), MethodRole(..), MethodSchema(..), PluginSchema(..))
import Synapse.Algebra.Render (Listing(..), renderListing, renderSchema, defaultStyle, summarize, fitText)
import Synapse.CLI.Help (MethodPage(..), renderMethodPage, renderExample, shellQuote)
import Synapse.IR.Builder (extractMethodDef)
import Synapse.IR.Types

method :: Text -> Text -> Maybe Value -> MethodSchema
method name desc params = MethodSchema
  { methodName = name, methodDescription = desc, methodHash = "h", methodParams = params
  , methodReturns = Nothing, methodStreaming = True, methodBidirectional = False
  , methodRequestType = Nothing, methodResponseType = Nothing, methodDeprecation = Nothing
  , methodParamSchemas = Nothing, methodRole = MethodRoleRpc
  , methodLongDescription = Nothing, methodGrant = Nothing, methodTarget = Nothing, methodHeldOn = Nothing, methodPublic = False
  , methodGroup = Nothing, methodExamples = [] }

grouped :: Text -> MethodSchema -> MethodSchema
grouped g m = m { methodGroup = Just g }

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

-- | A hub with children and ungrouped methods (an older hub's shape).
hub :: PluginSchema
hub = PluginSchema
  { psNamespace = "menger", psVersion = "1.0.0"
  , psDescription = "Central routing and introspection"
  , psLongDescription = Nothing, psHash = "h0"
  , psMethods =
      [ method "hash" "Get plexus configuration hash (from the recursive schema) This hash changes whenever any method or child plugin changes" Nothing
      , method "call" "Route a call to a registered activation" (Just findParams)
      ]
  , psChildren = Just
      [ ChildSummary "tickets" "Plans as data — create, query, and watch the work DAG" "h1"
      , ChildSummary "comments" "Comments on the app and on ticket bodies — add, list (filtered), resolve, reopen, watch" "h2"
      ]
  , psDeprecation = Nothing
  , psRoles = Nothing, psView = Nothing
  }

-- | A leaf whose methods say their groups (declaration order: read, write,
-- inputs), one saying none.
tickets :: PluginSchema
tickets = PluginSchema
  { psNamespace = "tickets", psVersion = "0.5.0"
  , psDescription = "Plans as data — create, query, and watch the work DAG"
  , psLongDescription = Nothing, psHash = "t0"
  , psMethods =
      [ grouped "read" (method "list" "List tickets, optionally filtered by epic, status, parent and/or a filter; paged with after + limit" (Just findParams))
      , grouped "write" (method "set_status" "Set a ticket's status (not a doc's: a doc has no lifecycle)" (Just setStatusParams))
      , grouped "read" (method "get" "Get one ticket by id, or by path address SHORTCODE/path (e.g. PLEXUS/_MENGER/board)" Nothing)
      , grouped "inputs" (method "answer" "Answer an input: record a value (checked against its kind and options) with a rationale" Nothing)
      , grouped "write" (method "create" "Create a ticket; allocates the next EPIC-N id" Nothing)
      , method "schema" "Get plugin or method schema. Pass {\"method\": \"name\"} for a specific method." Nothing
      ]
  , psChildren = Nothing
  , psDeprecation = Nothing
  , psRoles = Nothing, psView = Nothing
  }

findParams, setStatusParams, answerParams :: Value
findParams = [aesonQQ|{
  "type": "object",
  "properties": {
    "after": {"type": ["string", "null"], "description": "Cursor: start strictly after this ticket id, e.g. MNG-9 (optional)"},
    "limit": {"type": ["integer", "null"], "format": "uint64", "description": "At most this many tickets (default 50); the page event carries the next cursor (optional)"},
    "query": {"type": "string", "description": "Substring of the ticket id or title, case-insensitive (ASCII), trimmed; empty matches every ticket"}
  },
  "required": ["query"]
}|]
setStatusParams = [aesonQQ|{
  "type": "object",
  "properties": {
    "id": {"type": "string", "description": "Ticket id, e.g. MNG-2"},
    "status": {"type": "string", "description": "New status (case-insensitive; `active` is Coding)",
               "x-plexus-values": ["Pending", "Ready", "Coding", "In Review", "Blocked", "Complete"]},
    "note": {"type": ["string", "null"], "description": "Why (optional)"}
  },
  "required": ["id", "status"]
}|]
answerParams = [aesonQQ|{
  "type": "object",
  "properties": {
    "ticket": {"type": "string", "description": "Ticket id, e.g. INPUT-9"},
    "value": {"type": "object", "description": "Typed by the input's kind"},
    "commits": {"type": ["array", "null"], "items": {"type": "string"}, "description": "Commits"},
    "rationale": {"type": ["string", "null"], "description": "Why (optional)"}
  },
  "required": ["ticket", "value"]
}|]

-- | set_status as a hub with SYNAPSE-13's fields serves it.
setStatus :: MethodSchema
setStatus = (method "set_status" "Set a ticket's status (not a doc's: a doc has no lifecycle)" (Just setStatusParams))
  { methodLongDescription = Just "Yields exactly one event: `StatusChanged`, `NotFound`, or `Error`.\n\nA doc has no lifecycle."
  , methodGrant = Just "tickets.set_status", methodTarget = Just "id", methodHeldOn = Just ["MENGER"]
  , methodGroup = Just "write"
  , methodExamples =
      [ MethodExample [aesonQQ|{"id": "MENGER-40", "status": "Coding"}|] (Just "start work on MENGER-40")
      , MethodExample [aesonQQ|{"id": "MENGER-40", "status": "In Review"}|] Nothing
      ]
  }

pageOf :: Bool -> MethodSchema -> (MethodDef, Text)
pageOf caller ms =
  let (types, md) = extractMethodDef "tickets" "tickets" ms
      ir = emptyIR { irMethods = Map.singleton (mdFullPath md) md, irTypes = types }
      page = MethodPage
        { mpProgram = "synapse -P 4455", mpTarget = "menger tickets " <> methodName ms
        , mpSchema = Just ms, mpCaller = caller }
  in (md, renderMethodPage page ir md)

-- | An older hub's set_status: no long description, examples, values,
-- grant or group — and only the IR (no schema) at hand.
oldPage :: Text
oldPage =
  let old = (method "set_status" "Set a ticket's status" (Just [aesonQQ|{
              "type": "object",
              "properties": {"id": {"type": "string", "description": "Ticket id, e.g. MNG-2"},
                             "status": {"type": "string", "description": "New status"}},
              "required": ["id", "status"]}|]))
      (types, md) = extractMethodDef "tickets" "tickets" old
      ir = emptyIR { irMethods = Map.singleton (mdFullPath md) md, irTypes = types }
      page = MethodPage { mpProgram = "synapse", mpTarget = "menger tickets set_status", mpSchema = Nothing, mpCaller = False }
  in renderMethodPage page ir md

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

linesOf :: Text -> [Text]
linesOf = T.lines

lineWith :: [Text] -> Text -> Text
lineWith ls needle = fromJust (lookupLine needle)
  where lookupLine n = case filter (T.isInfixOf n) ls of { (l : _) -> Just l; [] -> Nothing }

indexOf :: [Text] -> Text -> Int
indexOf ls needle = length (takeWhile (not . T.isInfixOf needle) ls)

col :: Text -> Text -> Int
col needle l = T.length (fst (T.breakOn needle l))

main :: IO ()
main = do
  let listing = renderListing defaultStyle (Listing "synapse -P 4455 menger tickets" Nothing) tickets
      hubListing = renderListing defaultStyle (Listing "synapse -P 4455 menger" Nothing) hub
      (_, page) = pageOf False setStatus
      (_, callerPage) = pageOf True setStatus
  mapM_ TIO.putStrLn [listing, hubListing, page, callerPage, oldPage]
  hspec $ do
    describe "a listing (gh's shape)" $ do
      let ls = linesOf listing
      it "is the name, version and description, a usage line, then its sections" $ do
        take 2 ls `shouldBe` ["tickets v0.5.0", "  Plans as data \x2014 create, query, and watch the work DAG"]
        lineWith ls "USAGE" `shouldBe` "USAGE"
        ls !! (indexOf ls "USAGE" + 1) `shouldBe` "  synapse -P 4455 menger tickets <method> [flags]"

      it "groups the methods in the order their groups first appear, the ungrouped last" $
        filter (\l -> not (T.null l) && T.toUpper l == l && not (" " `T.isPrefixOf` l)) ls
          `shouldBe` ["USAGE", "READ", "WRITE", "INPUTS", "OTHER"]

      it "lists each group alphabetically" $ do
        let at n = indexOf ls ("  " <> n <> " ")
        (at "get" < at "list", at "create" < at "set_status") `shouldBe` (True, True)
        (indexOf ls "READ" < at "get", at "list" < indexOf ls "WRITE") `shouldBe` (True, True)

      it "is one line per method — its summary, no flags" $ do
        lineWith ls "  create " `shouldSatisfy` T.isSuffixOf "Create a ticket"
        lineWith ls "  answer " `shouldSatisfy` T.isSuffixOf "Answer an input"
        lineWith ls "  schema " `shouldSatisfy` T.isSuffixOf "Get plugin or method schema"
        filter (T.isInfixOf "--") (takeWhile (not . T.isPrefixOf "Run ") ls) `shouldBe` []
        length (filter (T.isPrefixOf "  ") ls) `shouldBe` 2 + length (psMethods tickets)  -- the description, the usage, a line each

      it "starts every summary in one column" $ do
        let cols = [col "Create a ticket" (lineWith ls "  create "), col "Answer" (lineWith ls "  answer "), col "Set a ticket" (lineWith ls "  set_status ")]
        cols `shouldSatisfy` (\cs -> and (zipWith (==) cs (drop 1 cs)))

      it "ends with how to see a method's options" $
        last ls `shouldBe` "Run 'synapse -P 4455 menger tickets <method> --help' for its options."

      it "fits in 80 columns and never leaves two blank lines in a row" $ do
        filter ((> 80) . T.length) ls `shouldBe` []
        ("\n\n\n" `T.isInfixOf` listing) `shouldBe` False

    describe "a listing of a hub with no groups (an older hub)" $ do
      let ls = linesOf hubListing
      it "lists activations, then methods, alphabetically" $ do
        filter (`elem` ["ACTIVATIONS", "METHODS"]) ls `shouldBe` ["ACTIVATIONS", "METHODS"]
        (indexOf ls "  comments " < indexOf ls "  tickets ", indexOf ls "  call " < indexOf ls "  hash ") `shouldBe` (True, True)
      it "says how to reach both" $ do
        ls `shouldContain` ["  synapse -P 4455 menger <activation> <method> [flags]", "  synapse -P 4455 menger <method> [flags]"]
        ls `shouldContain` ["Run 'synapse -P 4455 menger <activation>' for its methods.", "Run 'synapse -P 4455 menger <method> --help' for its options."]
      it "keeps a long summary to its line" $ do
        lineWith ls "  comments " `shouldSatisfy` T.isSuffixOf "Comments on the app and on ticket bodies"
        filter ((> 80) . T.length) ls `shouldBe` []

    describe "a listing in a caller's view (AUTH-8)" $ do
      let caller = PluginSchema
            { psNamespace = "tickets", psVersion = "0.5.0", psDescription = "Plans as data"
            , psLongDescription = Nothing, psHash = "c", psChildren = Nothing, psDeprecation = Nothing
            , psRoles = Nothing, psView = Nothing
            , psMethods = [ setStatus, grouped "read" (method "get" "One ticket by id" Nothing) { methodGrant = Just "tickets.get", methodHeldOn = Just ["MENGER", "PLEXUS"] } ] }
          ls = linesOf (renderListing defaultStyle (Listing "synapse menger tickets" (Just "you are worker-1 \x00B7 editor on MENGER")) caller)
      it "keeps who the caller is under the description" $
        take 3 ls `shouldBe` ["tickets v0.5.0", "  Plans as data", "  you are worker-1 \x00B7 editor on MENGER"]
      it "ends each method's one line with its grant and where it is held, aligned" $ do
        let g = lineWith ls "  get "
            s = lineWith ls "  set_status "
        T.stripEnd g `shouldSatisfy` T.isSuffixOf "tickets.get         MENGER, PLEXUS"
        T.stripEnd s `shouldSatisfy` T.isSuffixOf "tickets.set_status  MENGER"
        col "tickets." g `shouldBe` col "tickets." s
      it "shows no grant column outside a caller's view" $
        renderListing defaultStyle (Listing "synapse menger tickets" Nothing) caller { psMethods = [setStatus { methodHeldOn = Nothing }] }
          `shouldSatisfy` (not . T.isInfixOf "tickets.set_status")

    describe "a summary" $ do
      it "is the first sentence, outside brackets, not stopping at e.g." $ do
        summarize "Create a ticket; allocates the next id" `shouldBe` "Create a ticket"
        summarize "Answer an input: record a value" `shouldBe` "Answer an input"
        summarize "Get plugin or method schema. Pass {\"method\": \"name\"}." `shouldBe` "Get plugin or method schema"
        summarize "Set a ticket's status (not a doc's: a doc has no lifecycle)" `shouldBe` "Set a ticket's status (not a doc's: a doc has no lifecycle)"
        summarize "Get one ticket, e.g. MNG-2. More" `shouldBe` "Get one ticket, e.g. MNG-2"
        summarize "Heartbeat \x2014 version, unix time" `shouldBe` "Heartbeat \x2014 version, unix time"
      it "fits its column: without parentheticals, at a dash, else cut with an ellipsis" $ do
        fitText 40 "Get one ticket by id, or by path address SHORTCODE/path (e.g. PLEXUS/_MENGER/board)"
          `shouldBe` "Get one ticket by id, or by path\x2026"
        fitText 60 "Comments on the app (and bodies) \x2014 add, list (filtered), resolve, reopen, watch"
          `shouldBe` "Comments on the app \x2014 add, list, resolve, reopen, watch"
        fitText 30 "Each shortcode's last sequence \x2014 where each one ends" `shouldBe` "Each shortcode's last sequence"
        T.length (fitText 20 "a very long single sentence without any stops in it at all") `shouldSatisfy` (<= 20)

    describe "a method's page (cargo's shape)" $ do
      let ls = linesOf page
          opts = drop (indexOf ls "Options:") ls
      it "is the description, then the long description in its paragraphs" $ do
        head ls `shouldBe` "Set a ticket's status (not a doc's: a doc has no lifecycle)"
        take 4 (drop 1 ls) `shouldBe` ["", "Yields exactly one event: `StatusChanged`, `NotFound`, or `Error`.", "", "A doc has no lifecycle."]
      it "has a usage line naming the required flags, then [OPTIONS]" $
        lineWith ls "Usage:" `shouldBe` "Usage: synapse -P 4455 menger tickets set_status --id <ID> --status <STATUS> [OPTIONS]"
      it "aligns the options, required first, optional in brackets, each with its type" $ do
        let i = indexOf ls "Options:"
        take 3 (map (T.takeWhile (/= '>')) (filter ("  -" `T.isPrefixOf`) (drop i ls) ++ filter ("  [" `T.isPrefixOf`) (drop i ls)))
          `shouldBe` ["  --id <string", "  --status <string", "  [--note <string"]
        let cols = [col "Ticket id" (lineWith opts "--id <"), col "New status" (lineWith opts "--status <"), col "Why" (lineWith opts "[--note <")]
        cols `shouldSatisfy` (\cs -> and (zipWith (==) cs (drop 1 cs)))
      it "says the values a param takes, under its description" $ do
        let i = indexOf opts "--status <"
        opts !! (i + 1) `shouldSatisfy` T.isInfixOf "[values: Pending, Ready, Coding, In Review, Blocked,"
        T.length (T.takeWhile (== ' ') (opts !! (i + 1))) `shouldBe` col "New status" (opts !! i)
      it "renders the schema's examples as command lines under their notes" $ do
        lineWith ls "Examples:" `shouldBe` "Examples:"
        ls `shouldContain`
          [ "  # start work on MENGER-40"
          , "  synapse -P 4455 menger tickets set_status --id MENGER-40 --status Coding" ]
        lineWith ls "In Review'" `shouldBe` "  synapse -P 4455 menger tickets set_status --id MENGER-40 --status 'In Review'"
      it "says nothing of grants outside a caller's view" $
        page `shouldSatisfy` (not . T.isInfixOf "Requires:")
      it "says what it requires in a caller's view" $
        last (linesOf callerPage) `shouldBe` "Requires: tickets.set_status on the scope of --id (you hold it on MENGER)"
      it "fits in 80 columns, its command lines (usage, examples) aside" $
        filter (\l -> T.length l > 80 && not ("  synapse " `T.isPrefixOf` l) && not ("Usage: " `T.isPrefixOf` l)) ls `shouldBe` []

    describe "a method's page from an older hub (none of the new fields)" $ do
      let ls = linesOf oldPage
      it "still has its usage line and options" $ do
        lineWith ls "Usage:" `shouldBe` "Usage: synapse menger tickets set_status --id <ID> --status <STATUS>"
        lineWith (drop (indexOf ls "Options:") ls) "--status <" `shouldSatisfy` T.isInfixOf "New status"
      it "synthesises one example from the required options (an e.g. where it gives one)" $ do
        lineWith ls "Example:" `shouldBe` "Example:"
        lineWith ls "  synapse " `shouldBe` "  synapse menger tickets set_status --id MNG-2 --status <STATUS>"
        oldPage `shouldSatisfy` (not . T.isInfixOf "values:")

    describe "an example as a command line" $ do
      let (md, _) = pageOf False ((method "answer" "Answer" (Just answerParams)))
          run = renderExample "synapse -P 4455" "menger tickets answer" md
      it "puts required params first, an object's fields dotted with its type first" $
        run [aesonQQ|{"rationale": "holds are real", "value": {"type": "choice", "option": "archive"}, "ticket": "R-13"}|]
          `shouldBe` "synapse -P 4455 menger tickets answer --ticket R-13 --value.type choice --value.option archive --rationale 'holds are real'"
      it "joins a list with commas, an element holding one in double quotes (SYNAPSE-2)" $ do
        run [aesonQQ|{"ticket": "R-13", "commits": ["a1b2c3d", "x@4f7114a"]}|]
          `shouldBe` "synapse -P 4455 menger tickets answer --ticket R-13 --commits a1b2c3d,x@4f7114a"
        run [aesonQQ|{"ticket": "R-13", "commits": ["a,b", "c"]}|]
          `shouldBe` "synapse -P 4455 menger tickets answer --ticket R-13 --commits '\"a,b\",c'"
      it "sends a list inside an object whole, with -p before the backend (SYNAPSE-9)" $
        run [aesonQQ|{"ticket": "R-13", "value": {"type": "multi", "options": ["unit", "wire"]}}|]
          `shouldBe` "synapse -P 4455 -p '{\"ticket\":\"R-13\",\"value\":{\"options\":[\"unit\",\"wire\"],\"type\":\"multi\"}}' menger tickets answer"
      it "quotes what the shell would split" $ do
        map shellQuote ["MENGER-40", "In Review", "it's", "#/ticket/D-2", "<ID>", "$HOME", ""]
          `shouldBe` ["MENGER-40", "'In Review'", "\"it's\"", "'#/ticket/D-2'", "<ID>", "'$HOME'", "''"]

    describe "renderSchema (the listing, reached by its namespace)" $
      it "is the listing with synapse <namespace> as its usage" $
        renderSchema tickets `shouldSatisfy` T.isInfixOf "  synapse tickets <method> [flags]"
