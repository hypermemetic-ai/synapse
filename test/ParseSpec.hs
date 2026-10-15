{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for CLI parameter parsing
module Main where

import Data.Aeson (Value(..), object)
import qualified Data.Aeson.Key as K
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Hspec

import Synapse.CLI.Help (renderParamHelp)
import Synapse.CLI.Parse
import Synapse.CLI.Transform (TransformEnv(..), envExpansion, injectBooleanDefaults)
import Synapse.IR.Types

main :: IO ()
main = hspec $ do
  describe "envExpansion (SYNAPSE-6): $VAR is substituted in path parameters only" $ do
    let env = TransformEnv
          { teCwd = "/work", teHome = Just "/home/alice"
          , teEnv = [("HOME", "/home/alice"), ("PROJECT", "/home/alice/proj")] }
        expand k v = envExpansion env (k, v)

    it "a ticket body keeps $HOME literally" $
      expand "body" "costs $HOME and $PROJECT" `shouldReturn` ("body", "costs $HOME and $PROJECT")

    it "a title, a message, any non-path value keeps it too" $ do
      expand "title" "$HOME" `shouldReturn` ("title", "$HOME")
      expand "message" "Hello $PROJECT" `shouldReturn` ("message", "Hello $PROJECT")

    it "a path parameter still expands" $
      expand "path" "$PROJECT/src" `shouldReturn` ("path", "/home/alice/proj/src")

    it "an unset variable in a path parameter is left as written" $
      expand "working_dir" "$NOPE/x" `shouldReturn` ("working_dir", "$NOPE/x")

  describe "groupByPrefix" $ do
    it "groups dotted keys by prefix" $ do
      let input = [("identifier.type", "by_name"), ("identifier.name", "foo"), ("prompt", "hi")]
      let expected = Map.fromList
            [ ("identifier", [("type", "by_name"), ("name", "foo")])
            , ("prompt", [("", "hi")])
            ]
      groupByPrefix input `shouldBe` expected

    it "collects repeated keys for arrays" $ do
      let input = [("tags", "backend"), ("tags", "critical"), ("tags", "urgent")]
      let expected = Map.fromList
            [ ("tags", [("", "backend"), ("", "critical"), ("", "urgent")])
            ]
      groupByPrefix input `shouldBe` expected

  describe "buildParamValue with arrays" $ do
    let emptyIR = IR
          { irVersion = "2.0"
          , irHash = Nothing
          , irTypes = Map.empty
          , irMethods = Map.empty
          , irPlugins = Map.empty
          }

    it "builds string array from repeated values" $ do
      let param = ParamDef
            { pdName = "tags"
            , pdType = RefArray (RefPrimitive "string" Nothing)
            , pdDescription = Nothing
            , pdRequired = True
            , pdDefault = Nothing
            , pdDeprecation = Nothing
            }
      let kvs = [("", "backend"), ("", "critical"), ("", "urgent")]
      let expected = Right $ Array $ V.fromList [String "backend", String "critical", String "urgent"]
      buildParamValue emptyIR param kvs `shouldBe` expected

    it "builds integer array from repeated values" $ do
      let param = ParamDef
            { pdName = "ids"
            , pdType = RefArray (RefPrimitive "integer" Nothing)
            , pdDescription = Nothing
            , pdRequired = True
            , pdDefault = Nothing
            , pdDeprecation = Nothing
            }
      let kvs = [("", "1"), ("", "2"), ("", "3")]
      let expected = Right $ Array $ V.fromList [Number 1, Number 2, Number 3]
      buildParamValue emptyIR param kvs `shouldBe` expected

    it "fails when array has no values" $ do
      let param = ParamDef
            { pdName = "tags"
            , pdType = RefArray (RefPrimitive "string" Nothing)
            , pdDescription = Nothing
            , pdRequired = True
            , pdDefault = Nothing
            , pdDeprecation = Nothing
            }
      let kvs = []
      buildParamValue emptyIR param kvs `shouldSatisfy` isLeft

  -- SYNAPSE-2: each flag value of a list of scalars is a comma-separated
  -- record with double-quote quoting. One test per row of the ticket's table.
  describe "buildParamValue with comma-separated list values" $ do
    let strings = listParam "tags" (RefArray (RefPrimitive "string" Nothing))
        ints = listParam "ids" (RefArray (RefPrimitive "integer" Nothing))
        one p val = buildParamValue listIR p [("", val)]

    describe "string list" $ do
      it "a,b,c,e -> a b c e" $
        one strings "a,b,c,e" `shouldBe` strs ["a", "b", "c", "e"]
      it "a -> a (unchanged)" $
        one strings "a" `shouldBe` strs ["a"]
      it "\"a,b,c,d\",\"e,f,g\" -> two elements (a quoted field keeps its commas)" $
        one strings "\"a,b,c,d\",\"e,f,g\"" `shouldBe` strs ["a,b,c,d", "e,f,g"]
      it "\"say \"\"hi\"\"\",b -> say \"hi\" and b (a doubled quote is one quote)" $
        one strings "\"say \"\"hi\"\"\",b" `shouldBe` strs ["say \"hi\"", "b"]
      it "a, b ,c -> a b c (space around an unquoted field is dropped)" $
        one strings "a, b ,c" `shouldBe` strs ["a", "b", "c"]
      it "a,,b, -> a b (an empty unquoted field is dropped)" $
        one strings "a,,b," `shouldBe` strs ["a", "b"]
      it "\"\",a -> the empty string and a" $
        one strings "\"\",a" `shouldBe` strs ["", "a"]
      it "\"a,b is an error naming the parameter and the value" $
        one strings "\"a,b" `shouldBe`
          Left (MalformedListValue "tags" "\"a,b" "a quoted element has no closing quote")
      it "\"a\"x,b is an error naming the parameter and the value" $
        one strings "\"a\"x,b" `shouldBe`
          Left (MalformedListValue "tags" "\"a\"x,b" "a closing quote must be followed by a comma or the end")

    describe "integer list (elements typed as before)" $ do
      it "1,2,3 -> 1 2 3" $
        one ints "1,2,3" `shouldBe` nums [1, 2, 3]
      it "1 -> 1 (unchanged)" $
        one ints "1" `shouldBe` nums [1]
      it "\"1,2\",\"3\" -> two elements (a quoted field keeps its commas)" $
        one ints "\"1,2\",\"3\"" `shouldBe` arr [String "1,2", Number 3]
      it "\"4\"\"2\",5 -> 4\"2 and 5 (a doubled quote is one quote)" $
        one ints "\"4\"\"2\",5" `shouldBe` arr [String "4\"2", Number 5]
      it "1, 2 ,3 -> 1 2 3 (space around an unquoted field is dropped)" $
        one ints "1, 2 ,3" `shouldBe` nums [1, 2, 3]
      it "1,,2, -> 1 2 (an empty unquoted field is dropped)" $
        one ints "1,,2," `shouldBe` nums [1, 2]
      it "\"\",1 -> the empty string and 1" $
        one ints "\"\",1" `shouldBe` arr [String "", Number 1]
      it "\"1,2 is an error naming the parameter and the value" $
        one ints "\"1,2" `shouldBe`
          Left (MalformedListValue "ids" "\"1,2" "a quoted element has no closing quote")
      it "\"1\"x,2 is an error naming the parameter and the value" $
        one ints "\"1\"x,2" `shouldBe`
          Left (MalformedListValue "ids" "\"1\"x,2" "a closing quote must be followed by a comma or the end")

    describe "which lists split" $ do
      it "an optional list splits like a list" $ do
        let blockedBy = listParam "blocked_by" (RefOptional (RefArray (RefPrimitive "string" Nothing)))
        one blockedBy "MNG-1,MNG-2" `shouldBe` strs ["MNG-1", "MNG-2"]
        one blockedBy "\"a,b\",\"c\"" `shouldBe` strs ["a,b", "c"]

      it "a list of a named scalar type splits" $
        one (listParam "refs" (RefArray (named "TicketId"))) "MNG-1,MNG-2"
          `shouldBe` strs ["MNG-1", "MNG-2"]

      it "a list of a string enum splits" $
        one (listParam "statuses" (RefArray (named "Status"))) "open,closed"
          `shouldBe` strs ["open", "closed"]

      it "a list of objects is not split: each flag value is one element" $
        one (listParam "links" (RefArray (named "Link"))) "{\"id\":\"a\",\"kind\":\"b\"}"
          `shouldBe` arr [String "{\"id\":\"a\",\"kind\":\"b\"}"]

      it "a list of untyped values is not split" $
        one (listParam "items" (RefArray RefAny)) "a,b" `shouldBe` strs ["a,b"]

      it "a list of lists is not split" $
        one (listParam "rows" (RefArray (RefArray (RefPrimitive "string" Nothing)))) "a,b"
          `shouldBe` strs ["a,b"]

      it "a non-list parameter keeps its commas" $
        buildParamValue listIR (listParam "title" (RefPrimitive "string" Nothing)) [("", "a, b")]
          `shouldBe` Right (String "a, b")

    describe "repeated flags" $ do
      it "--x a,b --x c -> a b c, in order" $
        buildParamValue listIR strings [("", "a,b"), ("", "c")] `shouldBe` strs ["a", "b", "c"]

      it "mix plain, comma-separated and quoted values, in order" $
        buildParamValue listIR strings [("", "a"), ("", "b, c"), ("", "\"d,e\",f")]
          `shouldBe` strs ["a", "b", "c", "d,e", "f"]

      it "one malformed value fails the whole parameter" $
        buildParamValue listIR strings [("", "a"), ("", "\"b")]
          `shouldBe` Left (MalformedListValue "tags" "\"b" "a quoted element has no closing quote")

    describe "parseParams" $ do
      let method = methodWith
            [ listParam "blocked_by" (RefOptional (RefArray (RefPrimitive "string" Nothing)))
            , listParam "title" (RefPrimitive "string" Nothing)
            ]
      it "splits the list and leaves other parameters alone" $
        parseParams listIR method [("blocked_by", "MNG-1,MNG-2"), ("title", "t, u")]
          `shouldBe` Right (object
            [ (K.fromText "blocked_by", Array (V.fromList [String "MNG-1", String "MNG-2"]))
            , (K.fromText "title", String "t, u")
            ])
      it "a malformed list value fails the parse, so nothing is sent" $
        parseParams listIR method [("blocked_by", "\"a,b"), ("title", "t")]
          `shouldBe` Left [MalformedListValue "blocked_by" "\"a,b" "a quoted element has no closing quote"]

  describe "help for list parameters" $ do
    it "a list of scalars names the comma form" $
      renderParamHelp listIR (listParam "blocked_by" (RefOptional (RefArray (RefPrimitive "string" Nothing))))
        `shouldSatisfy` elem "    Repeat the flag or use commas: --blocked-by a,b"
    it "a list of objects only repeats" $
      renderParamHelp listIR (listParam "links" (RefArray (named "Link")))
        `shouldSatisfy` elem "    Repeat the flag for each element"

  -- SYNAPSE-4: a flag that takes a value takes the next argument whatever it
  -- begins with, and --key=value is a flag and its value. One group per row
  -- of the ticket's table, run through both passes and parseParams.
  describe "flag values that begin with dashes" $ do
    describe "a parameter that takes a value takes the next argument, whatever it begins with" $ do
      it "--body '---\\nid: X\\n---' (frontmatter)" $
        invoke ["--body", frontmatter] `shouldBe` sends [("body", String frontmatter)]
      it "--body --force" $
        invoke ["--body", "--force"] `shouldBe` sends [("body", "--force")]
      it "--offset -5" $
        invoke ["--offset", "-5"] `shouldBe` sends [("offset", Number (-5))]
      it "--body -- (exactly two dashes)" $
        invoke ["--body", "--"] `shouldBe` sends [("body", "--")]
      it "--body '' (empty)" $
        invoke ["--body", ""] `shouldBe` sends [("body", "")]
      it "the flag after such a value is still a flag" $
        invoke ["--body", "---", "--id", "MNG-1"] `shouldBe` sends [("body", "---"), ("id", "MNG-1")]

    describe "--key=value" $ do
      it "--body='---\\nid: X\\n---'" $
        invoke ["--body=" <> frontmatter] `shouldBe` sends [("body", String frontmatter)]
      it "--body= (empty)" $
        invoke ["--body="] `shouldBe` sends [("body", "")]
      it "--body=a=b (the value keeps every = after the first)" $
        invoke ["--body=a=b"] `shouldBe` sends [("body", "a=b")]
      it "--body=--force" $
        invoke ["--body=--force"] `shouldBe` sends [("body", "--force")]
      it "--id=MNG-1 --offset=-5" $
        invoke ["--id=MNG-1", "--offset=-5"] `shouldBe` sends [("id", "MNG-1"), ("offset", Number (-5))]

    describe "a boolean, bare" $ do
      it "--summary --limit 3 -> summary true, limit 3" $
        invoke ["--summary", "--limit", "3"] `shouldBe` sends [("summary", Bool True), ("limit", Number 3)]
      it "--limit 3 --summary (last) -> summary true" $
        invoke ["--limit", "3", "--summary"] `shouldBe` sends [("summary", Bool True), ("limit", Number 3)]

    describe "a boolean with an explicit value" $ do
      it "--summary true --limit 3" $
        invoke ["--summary", "true", "--limit", "3"] `shouldBe` sends [("summary", Bool True), ("limit", Number 3)]
      it "--summary false" $
        invoke ["--summary", "false"] `shouldBe` sends [("summary", Bool False)]
      it "--summary=false" $
        invoke ["--summary=false"] `shouldBe` sends [("summary", Bool False)]
      it "--summary 0 (a spelling already read as a boolean)" $
        invoke ["--summary", "0"] `shouldBe` sends [("summary", Bool False)]
      it "--summary yes: yes is not its value, and no flag takes it" $
        invoke ["--summary", "yes"] `shouldBe` Left [UnexpectedArgument "yes"]

    describe "a list parameter (SYNAPSE-2, unchanged)" $ do
      it "--commits a,b --commits c" $
        invoke ["--commits", "a,b", "--commits", "c"] `shouldBe` sends [("commits", strings ["a", "b", "c"])]
      it "--commits=a,b" $
        invoke ["--commits=a,b"] `shouldBe` sends [("commits", strings ["a", "b"])]
      it "--blocked-by=MNG-1,MNG-2 --blocked-by MNG-3" $
        invoke ["--blocked-by=MNG-1,MNG-2", "--blocked-by", "MNG-3"]
          `shouldBe` sends [("blocked_by", strings ["MNG-1", "MNG-2", "MNG-3"])]

    describe "a dotted key" $ do
      it "--identifier.type by_name --identifier.name foo (unchanged)" $
        invoke ["--identifier.type", "by_name", "--identifier.name", "foo"]
          `shouldBe` sends [("identifier", object [("type", "by_name"), ("name", "foo")])]
      it "--identifier.name takes a value that begins with dashes" $
        invoke ["--identifier.type", "by_name", "--identifier.name", "--x"]
          `shouldBe` sends [("identifier", object [("type", "by_name"), ("name", "--x")])]
      it "a bare boolean field does not swallow the next flag" $
        invoke ["--options.verbose", "--limit", "3"]
          `shouldBe` sends [("options", object [("verbose", Bool True), ("label", Null)]), ("limit", Number 3)]

    describe "a flag that names no parameter" $ do
      it "--no-such x: Unknown parameter, named as typed" $
        invoke ["--no-such", "x"] `shouldBe` Left [UnknownParam "no-such" []]
      it "--no-such=---x: named without its value" $
        invoke ["--no-such=---x"] `shouldBe` Left [UnknownParam "no-such" []]
      it "--bodyy x: still suggests the parameter" $
        invoke ["--bodyy", "x"] `shouldBe` Left [UnknownParam "bodyy" ["body"]]
      it "takes the next argument unless it begins with --, as before" $
        invoke ["--no-such", "--body", "b"] `shouldBe` Left [UnknownParam "no-such" []]

    describe "keys are normalized, values never" $ do
      it "--blocked-by MNG-1 -> blocked_by" $
        invoke ["--blocked-by", "MNG-1"] `shouldBe` sends [("blocked_by", strings ["MNG-1"])]
      it "--title --a-b-c keeps its dashes" $
        invoke ["--title", "--a-b-c"] `shouldBe` sends [("title", "--a-b-c")]
      it "--title=--a-b-c keeps its dashes" $
        invoke ["--title=--a-b-c"] `shouldBe` sends [("title", "--a-b-c")]

    describe "parsePathAndParams (before the method is known)" $ do
      it "finds the path when a value begins with dashes" $ do
        let (path, _, flags, helpReq) = parsePathAndParams ["tickets", "amend", "--body", frontmatter, "--id", "MNG-1"]
        (path, flags, helpReq) `shouldBe` (["tickets", "amend"], ["--body", frontmatter, "--id", "MNG-1"], False)
      it "a value of exactly -- is not a path segment" $
        parsePathAndParams ["tickets", "amend", "--body", "--"]
          `shouldBe` (["tickets", "amend"], [("body", "--")], ["--body", "--"], False)
      it "--key=value takes nothing after it" $
        parsePathAndParams ["--id=MNG-1", "tickets", "get"]
          `shouldBe` (["tickets", "get"], [("id", "MNG-1")], ["--id=MNG-1"], False)
      it "--help is still help, even after a flag" $
        parsePathAndParams ["tickets", "amend", "--body", "--help"]
          `shouldBe` (["tickets", "amend"], [("body", "")], ["--body"], True)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

-- ============================================================================
-- Fixtures for list values
-- ============================================================================

-- | A required parameter of the given type
listParam :: Text -> TypeRef -> ParamDef
listParam name ty = ParamDef
  { pdName = name
  , pdType = ty
  , pdDescription = Nothing
  , pdRequired = True
  , pdDefault = Nothing
  , pdDeprecation = Nothing
  , pdSource = Nothing
  , pdValues = Nothing
  }

-- | A method with the given parameters
methodWith :: [ParamDef] -> MethodDef
methodWith params = MethodDef
  { mdName = "create"
  , mdFullPath = "t.create"
  , mdNamespace = "t"
  , mdDescription = Nothing
  , mdStreaming = False
  , mdParams = params
  , mdReturns = RefAny
  , mdBidirType = Nothing
  , mdBidirResponseType = Nothing
  , mdBidirResponseSchema = Nothing
  , mdRole = MethodRoleRpc
  , mdGrant = Nothing
  , mdTarget = Nothing
  , mdLongDescription = Nothing
  }

-- | A reference to a type in 'listIR'
named :: Text -> TypeRef
named = RefNamed . QualifiedName "t"

-- | An IR with a named scalar, a string enum and an object type
listIR :: IR
listIR = emptyIR
  { irTypes = Map.fromList
      [ ("t.TicketId", TypeDef "TicketId" "t" Nothing (KindPrimitive "string" Nothing))
      , ("t.Status", TypeDef "Status" "t" Nothing (KindStringEnum ["open", "closed"]))
      , ("t.Link", TypeDef "Link" "t" Nothing (KindStruct
          [ FieldDef "id" (RefPrimitive "string" Nothing) Nothing True Nothing
          , FieldDef "kind" (RefPrimitive "string" Nothing) Nothing True Nothing
          ]))
      ]
  }

arr :: [Value] -> Either ParseError Value
arr = Right . Array . V.fromList

strs :: [Text] -> Either ParseError Value
strs = arr . map String

nums :: [Integer] -> Either ParseError Value
nums = arr . map (Number . fromInteger)

-- ============================================================================
-- Fixtures for flag values
-- ============================================================================

-- | A method's flag arguments as synapse reads them once the method is known:
-- both passes, then 'parseParams' (no backend needed)
invoke :: [Text] -> Either [ParseError] Value
invoke args =
  let (_, _, flags, _) = parsePathAndParams ("tickets" : "amend" : args)
  in pairFlags dashIR dashMethod flags
       >>= parseParams dashIR dashMethod . injectBooleanDefaults dashIR dashMethod

-- | The object 'parseParams' builds from these fields
sends :: [(Text, Value)] -> Either [ParseError] Value
sends fields = Right $ object [(K.fromText k, v) | (k, v) <- fields]

strings :: [Text] -> Value
strings = Array . V.fromList . map String

frontmatter :: Text
frontmatter = "---\nid: X\n---\nbody"

-- | An optional parameter of the given type
optParam :: Text -> TypeRef -> ParamDef
optParam name ty = (listParam name ty) { pdRequired = False }

-- | menger's amend/list/link_commits parameters, plus a dotted enum and struct
dashMethod :: MethodDef
dashMethod = methodWith
  [ optParam "body" (RefOptional (RefPrimitive "string" Nothing))
  , optParam "id" (RefPrimitive "string" Nothing)
  , optParam "title" (RefOptional (RefPrimitive "string" Nothing))
  , optParam "summary" (RefOptional (RefPrimitive "boolean" Nothing))
  , optParam "limit" (RefOptional (RefPrimitive "integer" (Just "uint32")))
  , optParam "offset" (RefPrimitive "integer" Nothing)
  , optParam "commits" (RefArray (RefPrimitive "string" Nothing))
  , optParam "blocked_by" (RefOptional (RefArray (RefPrimitive "string" Nothing)))
  , optParam "identifier" (named "Identifier")
  , optParam "options" (named "Options")
  ]

-- | An IR with a discriminated enum and a struct with a boolean field
dashIR :: IR
dashIR = emptyIR
  { irTypes = Map.fromList
      [ ("t.Identifier", TypeDef "Identifier" "t" Nothing (KindEnum "type"
          [ VariantDef "by_name" Nothing [FieldDef "name" (RefPrimitive "string" Nothing) Nothing True Nothing]
          , VariantDef "by_id" Nothing [FieldDef "id" (RefPrimitive "string" Nothing) Nothing True Nothing]
          ]))
      , ("t.Options", TypeDef "Options" "t" Nothing (KindStruct
          [ FieldDef "verbose" (RefPrimitive "boolean" Nothing) Nothing False Nothing
          , FieldDef "label" (RefPrimitive "string" Nothing) Nothing False Nothing
          ]))
      ]
  }
