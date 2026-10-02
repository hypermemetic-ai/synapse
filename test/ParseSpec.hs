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
import Synapse.IR.Types

main :: IO ()
main = hspec $ do
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
