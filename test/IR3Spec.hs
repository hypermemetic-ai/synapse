{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | IR 3.0 (RETURNS-13): a method's contract — item, error and progress
--   types and its cardinality — read from its plexus 0.7 schema, never
--   guessed; no two shapes under one title.
--
--   * The golden test builds IR from RETURNS-7's conformance fixture
--     (@../plexus-macros/fixtures/conformance/expected/schema.json@, by
--     sibling path) with 'buildIRFromSchema' and compares it, byte for byte,
--     with the checked-in @expected/ir.json@ beside it. Run with
--     @SYNAPSE_GOLDEN_ACCEPT=1@ to (re)write that file instead.
--   * Then one test per row of RETURNS-13's Required behavior table.
module Main where

import Control.Monad (forM_, unless)
import Data.Aeson (Value(..), eitherDecode, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (Config(..), Indent(..), defConfig, encodePretty')
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.ByteString.Lazy as LBS
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec

import Plexus.Schema.Recursive (MethodSchema(..), PluginSchema(..))
import Synapse.IR.Builder
  ( IRBuildError(..)
  , TypeSource(..)
  , buildIRFromSchema
  , renderIRBuildError
  )
import Synapse.IR.Types

-- ============================================================================
-- The fixture
-- ============================================================================

fixtureDir :: FilePath
fixtureDir = ".." </> "plexus-macros" </> "fixtures" </> "conformance" </> "expected"

schemaPath, irPath :: FilePath
schemaPath = fixtureDir </> "schema.json"
irPath = fixtureDir </> "ir.json"

-- | The fixture's IR: the activation at path @conformance@ — what
--   @synapse -i <backend> conformance@ builds when a backend serves it there
--   (less 'irMetadata', the toolchain and the clock).
fixtureIR :: PluginSchema -> Either IRBuildError IR
fixtureIR = buildIRFromSchema "conformance" ["conformance"]

-- | The bytes @ir.json@ holds: keys sorted, two-space indent, one trailing
--   newline.
renderIR :: IR -> LBS.ByteString
renderIR ir = encodePretty' defConfig { confIndent = Spaces 2, confCompare = compare } ir <> "\n"

main :: IO ()
main = do
  schemaBytes <- LBS.readFile schemaPath
  schema <- either (fail . ((schemaPath <> ": ") <>)) pure (eitherDecode schemaBytes)
  ir <- either (fail . T.unpack . renderIRBuildError) pure (fixtureIR schema)
  accept <- lookupEnv "SYNAPSE_GOLDEN_ACCEPT"
  case accept of
    Just "1" -> do
      LBS.writeFile irPath (renderIR ir)
      putStrLn $ "wrote " <> irPath
    _ -> pure ()
  golden <- LBS.readFile irPath
  hspec $ do
    goldenSpec ir golden
    tableSpec ir

-- ============================================================================
-- Golden
-- ============================================================================

goldenSpec :: IR -> LBS.ByteString -> Spec
goldenSpec ir golden = describe "golden: the conformance fixture's IR" $ do
  it "equals expected/ir.json byte for byte" $
    unless (renderIR ir == golden) $ expectationFailure $
      "IR built from " <> schemaPath <> " differs from " <> irPath
        <> " (regenerate with SYNAPSE_GOLDEN_ACCEPT=1 and read the diff)"

  it "expected/ir.json decodes back to the same IR" $
    eitherDecode golden `shouldBe` Right ir

  it "is IR 3.0" $
    irVersion ir `shouldBe` "3.0"

  it "has every method of the schema, keyed by full path" $
    Map.keys (irMethods ir) `shouldBe`
      [ "conformance.count", "conformance.load", "conformance.lookup", "conformance.ping"
      , "conformance.scan", "conformance.schema", "conformance.warm" ]

  it "carries the schema's own hash for the plugin" $
    fmap phiHash (Map.lookup "conformance" =<< irPluginHashes ir) `shouldBe` Just "60d7e85ced34c0c7"

  it "no MethodDef carries mdStreaming" $
    forM_ (Map.elems (irMethods ir)) $ \md -> case Aeson.toJSON md of
      Object o -> KM.member "mdStreaming" o `shouldBe` False
      other -> expectationFailure ("not an object: " <> show other)

  it "no RefNamed dangles" $
    danglingRefs ir `shouldBe` []

  it "reports synapse's own version" $
    synapseVersion `shouldBe` "3.15.0"

-- ============================================================================
-- The Required behavior table, one row at a time
-- ============================================================================

tableSpec :: IR -> Spec
tableSpec fixture = describe "IR 3.0 Required behavior" $ do

  describe "cardinality: one / many → mdCardinality, never inferred" $ do
    it "reads the fixture's cardinalities as they are" $ do
      let card m = mdCardinality <$> Map.lookup ("conformance." <> m) (irMethods fixture)
      map card ["ping", "lookup", "schema"] `shouldBe` replicate 3 (Just CardinalityOne)
      map card ["count", "scan", "load", "warm"] `shouldBe` replicate 4 (Just CardinalityMany)

    it "a many-variant item type of a one method stays one (no variant counting)" $ do
      md <- single [aesonQQ|{
        "name": "status", "description": "", "hash": "h",
        "returns": { "title": "Status", "oneOf": [
          { "type": "object", "properties": { "type": { "type": "string", "const": "a" } }, "required": ["type"] },
          { "type": "object", "properties": { "type": { "type": "string", "const": "b" } }, "required": ["type"] },
          { "type": "object", "properties": { "type": { "type": "string", "const": "c" } }, "required": ["type"] } ] },
        "cardinality": "one" }|]
      mdCardinality md `shouldBe` CardinalityOne

    it "a struct item type of a many method stays many" $ do
      md <- single (withReturns "many" pongSchema)
      mdCardinality md `shouldBe` CardinalityMany

  describe "returns (any schema form) → a RefNamed with a TypeDef, or a primitive / array / optional ref" $ do
    it "a struct: the fixture's ping returns conformance.Pong, a struct" $ do
      fmap mdReturns (Map.lookup "conformance.ping" (irMethods fixture))
        `shouldBe` Just (named "conformance" "Pong")
      kindOf fixture "conformance.Pong" `shouldBe` Just (KindStruct
        [ FieldDef "message" (RefPrimitive "string" Nothing) Nothing True Nothing ])

    it "a tagged enum" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Shape", "oneOf": [
          { "type": "object", "properties": { "r": { "type": "number" }, "type": { "type": "string", "const": "circle" } }, "required": ["type", "r"] },
          { "type": "object", "properties": { "type": { "type": "string", "const": "dot" } }, "required": ["type"] } ] }|]]
      returnsOf ir `shouldBe` named "t" "Shape"
      kindOf ir "t.Shape" `shouldBe` Just (KindEnum "type"
        [ VariantDef "circle" Nothing [FieldDef "r" (RefPrimitive "number" Nothing) Nothing True Nothing]
        , VariantDef "dot" Nothing [] ])

    it "a string enum" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Level", "type": "string", "enum": ["low", "high"] }|]]
      returnsOf ir `shouldBe` named "t" "Level"
      kindOf ir "t.Level" `shouldBe` Just (KindStringEnum ["low", "high"])

    it "a string enum written as consts" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Level", "oneOf": [
          { "type": "string", "const": "low", "description": "Low." },
          { "type": "string", "const": "high", "description": "High." } ] }|]]
      kindOf ir "t.Level" `shouldBe` Just (KindStringEnum ["low", "high"])

    it "an alias: a titled $ref" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Wrapper", "$ref": "#/$defs/Pong",
          "$defs": { "Pong": { "type": "object", "properties": { "message": { "type": "string" } }, "required": ["message"] } } }|]]
      returnsOf ir `shouldBe` named "t" "Wrapper"
      kindOf ir "t.Wrapper" `shouldBe` Just (KindAlias (named "t" "Pong"))
      fmap isStruct (kindOf ir "t.Pong") `shouldBe` Just True

    it "a primitive, whatever schemars titles it" $ do
      ir <- build [ withReturns "one" [aesonQQ|{ "title": "String", "type": "string" }|]
                  , withReturnsNamed "n" "one" [aesonQQ|{ "title": "uint32", "type": "integer", "format": "uint32", "minimum": 0 }|] ]
      returnsOf ir `shouldBe` RefPrimitive "string" Nothing
      fmap mdReturns (Map.lookup "t.n" (irMethods ir)) `shouldBe` Just (RefPrimitive "integer" (Just "uint32"))
      Map.keys (irTypes ir) `shouldBe` []

    it "an array of a named type" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Array_of_Pong", "type": "array", "items": { "$ref": "#/$defs/Pong" },
          "$defs": { "Pong": { "type": "object", "properties": { "message": { "type": "string" } }, "required": ["message"] } } }|]]
      returnsOf ir `shouldBe` RefArray (named "t" "Pong")
      fmap isStruct (kindOf ir "t.Pong") `shouldBe` Just True

    it "an optional named type" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Nullable_Pong", "anyOf": [ { "$ref": "#/$defs/Pong" }, { "type": "null" } ],
          "$defs": { "Pong": { "type": "object", "properties": { "message": { "type": "string" } }, "required": ["message"] } } }|]]
      returnsOf ir `shouldBe` RefOptional (named "t" "Pong")
      fmap isStruct (kindOf ir "t.Pong") `shouldBe` Just True

    it "any value ({})" $ do
      ir <- build [withReturns "one" [aesonQQ|{}|]]
      returnsOf ir `shouldBe` RefAny
      Map.keys (irTypes ir) `shouldBe` []

    it "the fixture's schema method: SchemaResult, an untagged union, is a named alias to any; its members are TypeDefs" $ do
      fmap mdReturns (Map.lookup "conformance.schema" (irMethods fixture))
        `shouldBe` Just (named "conformance" "SchemaResult")
      kindOf fixture "conformance.SchemaResult" `shouldBe` Just (KindAlias RefAny)
      fmap isStruct (kindOf fixture "conformance.PluginSchema") `shouldBe` Just True
      fmap isStruct (kindOf fixture "conformance.MethodSchema") `shouldBe` Just True

    it "an untagged union is a named alias to any, never a partial enum" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Either", "anyOf": [ { "$ref": "#/$defs/A" }, { "$ref": "#/$defs/B" } ],
          "$defs": { "A": { "type": "object", "properties": { "a": { "type": "string" } }, "required": ["a"] },
                     "B": { "type": "object", "properties": { "b": { "type": "string" } }, "required": ["b"] } } }|]]
      returnsOf ir `shouldBe` named "t" "Either"
      kindOf ir "t.Either" `shouldBe` Just (KindAlias RefAny)
      danglingRefs ir `shouldBe` []

  describe "error present → mdError = Just ref with TypeDef; absent → Nothing" $ do
    it "lookup's error is conformance.LookupError, a tagged enum" $ do
      fmap mdError (Map.lookup "conformance.lookup" (irMethods fixture))
        `shouldBe` Just (Just (named "conformance" "LookupError"))
      kindOf fixture "conformance.LookupError" `shouldBe` Just (KindEnum "type"
        [ VariantDef "not_found" Nothing [FieldDef "id" (RefPrimitive "string" Nothing) Nothing True Nothing] ])

    it "scan's and load's errors have TypeDefs" $ do
      fmap mdError (Map.lookup "conformance.scan" (irMethods fixture)) `shouldBe` Just (Just (named "conformance" "ScanError"))
      fmap mdError (Map.lookup "conformance.load" (irMethods fixture)) `shouldBe` Just (Just (named "conformance" "LoadError"))
      Map.member "conformance.ScanError" (irTypes fixture) `shouldBe` True
      Map.member "conformance.LoadError" (irTypes fixture) `shouldBe` True

    it "an infallible method has none" $
      forM_ ["ping", "count", "warm", "schema"] $ \m ->
        fmap mdError (Map.lookup ("conformance." <> m) (irMethods fixture)) `shouldBe` Just Nothing

  describe "progress present → mdProgress = Just ref with TypeDef; absent → Nothing" $ do
    it "load's progress is conformance.LoadStep, a tagged enum" $ do
      fmap mdProgress (Map.lookup "conformance.load" (irMethods fixture))
        `shouldBe` Just (Just (named "conformance" "LoadStep"))
      kindOf fixture "conformance.LoadStep" `shouldBe` Just (KindEnum "type"
        [ VariantDef "batch" Nothing
            [ FieldDef "done" (RefPrimitive "integer" (Just "uint32")) Nothing True Nothing
            , FieldDef "of" (RefPrimitive "integer" (Just "uint32")) Nothing True Nothing ] ])

    it "warm's progress is conformance.Note, the default" $ do
      fmap mdProgress (Map.lookup "conformance.warm" (irMethods fixture))
        `shouldBe` Just (Just (named "conformance" "Note"))
      fmap isStruct (kindOf fixture "conformance.Note") `shouldBe` Just True

    it "a method that reports no progress has none" $
      forM_ ["ping", "lookup", "count", "scan", "schema"] $ \m ->
        fmap mdProgress (Map.lookup ("conformance." <> m) (irMethods fixture)) `shouldBe` Just Nothing

  describe "tagged enum with any tag name → keDiscriminator = the property whose const every variant carries" $ do
    it "plexus-core's MethodRole (in the fixture's SchemaResult) is tagged by kind" $
      kindOf fixture "conformance.MethodRole" `shouldBe` Just (KindEnum "kind"
        [ VariantDef "rpc" Nothing []
        , VariantDef "static_child" Nothing []
        , VariantDef "dynamic_child" Nothing
            [ FieldDef "list_method" (RefOptional (RefPrimitive "string" Nothing)) Nothing False Nothing
            , FieldDef "search_method" (RefOptional (RefPrimitive "string" Nothing)) Nothing False Nothing ] ])

    it "a tag called kind" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [
          { "type": "object", "properties": { "kind": { "type": "string", "const": "created" }, "id": { "type": "string" } }, "required": ["kind", "id"] },
          { "type": "object", "properties": { "kind": { "type": "string", "const": "deleted" } }, "required": ["kind"] } ] }|]]
      kindOf ir "t.Event" `shouldBe` Just (KindEnum "kind"
        [ VariantDef "created" Nothing [FieldDef "id" (RefPrimitive "string" Nothing) Nothing True Nothing]
        , VariantDef "deleted" Nothing [] ])

    it "an adjacently tagged enum: the tag, with its content a field" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Msg", "oneOf": [
          { "type": "object", "properties": { "t": { "type": "string", "const": "text" }, "c": { "type": "string" } }, "required": ["t", "c"] },
          { "type": "object", "properties": { "t": { "type": "string", "const": "count" }, "c": { "type": "integer" } }, "required": ["t", "c"] } ] }|]]
      kindOf ir "t.Msg" `shouldBe` Just (KindEnum "t"
        [ VariantDef "text" Nothing [FieldDef "c" (RefPrimitive "string" Nothing) Nothing True Nothing]
        , VariantDef "count" Nothing [FieldDef "c" (RefPrimitive "integer" Nothing) Nothing True Nothing] ])

    it "variants by $ref" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [ { "$ref": "#/$defs/Up" }, { "$ref": "#/$defs/Down" } ],
          "$defs": {
            "Up": { "type": "object", "properties": { "op": { "type": "string", "const": "up" }, "by": { "type": "integer" } }, "required": ["op", "by"] },
            "Down": { "type": "object", "properties": { "op": { "type": "string", "const": "down" } }, "required": ["op"] } } }|]]
      kindOf ir "t.Event" `shouldBe` Just (KindEnum "op"
        [ VariantDef "up" Nothing [FieldDef "by" (RefPrimitive "integer" Nothing) Nothing True Nothing]
        , VariantDef "down" Nothing [] ])

  describe "newtype variant over a struct → variant fields = the struct's fields (from $defs)" $ do
    it "the 2020-12 form: $ref beside the tag" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [
          { "type": "object", "properties": { "type": { "type": "string", "const": "moved" } }, "$ref": "#/$defs/Move", "required": ["type"] },
          { "type": "object", "properties": { "type": { "type": "string", "const": "stopped" } }, "required": ["type"] } ],
          "$defs": { "Move": { "type": "object", "properties": { "x": { "type": "integer" }, "note": { "type": ["string", "null"] } }, "required": ["x"] } } }|]]
      kindOf ir "t.Event" `shouldBe` Just (KindEnum "type"
        [ VariantDef "moved" Nothing
            [ FieldDef "note" (RefOptional (RefPrimitive "string" Nothing)) Nothing False Nothing
            , FieldDef "x" (RefPrimitive "integer" Nothing) Nothing True Nothing ]
        , VariantDef "stopped" Nothing [] ])

    it "the allOf form" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [
          { "type": "object", "properties": { "type": { "type": "string", "const": "moved" } }, "allOf": [ { "$ref": "#/$defs/Move" } ], "required": ["type"] } ],
          "$defs": { "Move": { "type": "object", "properties": { "x": { "type": "integer" } }, "required": ["x"] } } }|]]
      kindOf ir "t.Event" `shouldBe` Just (KindEnum "type"
        [ VariantDef "moved" Nothing [FieldDef "x" (RefPrimitive "integer" Nothing) Nothing True Nothing] ])

    it "a newtype variant over something with no fields is not a partial enum" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [
          { "type": "object", "properties": { "type": { "type": "string", "const": "named" } }, "$ref": "#/$defs/Name", "required": ["type"] } ],
          "$defs": { "Name": { "type": "string" } } }|]]
      kindOf ir "t.Event" `shouldBe` Just (KindAlias RefAny)

  describe "field not in required → fdRequired = False" $ do
    it "in a struct: Note's percentage" $
      kindOf fixture "conformance.Note" `shouldBe` Just (KindStruct
        [ FieldDef "message" (RefPrimitive "string" Nothing) (Just "What is happening, for a human.") True Nothing
        , FieldDef "percentage" (RefOptional (RefPrimitive "number" (Just "float")))
            (Just "Completion, `0.0`–`100.0`, when the method can say.") False Nothing ])

    it "in a variant" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [
          { "type": "object", "properties": { "type": { "type": "string", "const": "seen" }, "at": { "type": "integer" }, "by": { "type": "string" } }, "required": ["type", "at"] } ] }|]]
      kindOf ir "t.Event" `shouldBe` Just (KindEnum "type"
        [ VariantDef "seen" Nothing
            [ FieldDef "at" (RefPrimitive "integer" Nothing) Nothing True Nothing
            , FieldDef "by" (RefPrimitive "string" Nothing) Nothing False Nothing ] ])

  describe "same title, same shape, two methods → one TypeDef" $ do
    it "two methods returning Pong, one of them through $defs" $ do
      ir <- build
        [ withReturnsNamed "a" "one" pongSchema
        , withReturnsNamed "b" "many" [aesonQQ|{ "title": "Array_of_Pong", "type": "array", "items": { "$ref": "#/$defs/Pong" },
            "$defs": { "Pong": { "description": "What ping answers.", "type": "object", "properties": { "message": { "type": "string" } }, "required": ["message"] } } }|] ]
      Map.keys (irTypes ir) `shouldBe` ["t.Pong"]
      fmap mdReturns (Map.lookup "t.a" (irMethods ir)) `shouldBe` Just (named "t" "Pong")
      fmap mdReturns (Map.lookup "t.b" (irMethods ir)) `shouldBe` Just (RefArray (named "t" "Pong"))

  describe "same title, different shapes, one namespace → build fails naming the two methods and the title" $ do
    it "fails" $ do
      let result = buildIn "t"
            [ withReturnsNamed "first" "one" pongSchema
            , withReturnsNamed "second" "one" [aesonQQ|{ "title": "Pong", "type": "object", "properties": { "reply": { "type": "integer" } }, "required": ["reply"] }|] ]
      result `shouldBe` Left TitleCollision
        { tcNamespace = "t"
        , tcTitle = "Pong"
        , tcFirst = TypeSource "first" "returns"
        , tcSecond = TypeSource "second" "returns" }
      case result of
        Left err -> do
          let msg = T.unpack (renderIRBuildError err)
          msg `shouldSatisfy` isInfixOf "\"Pong\""
          msg `shouldSatisfy` isInfixOf "method first (returns)"
          msg `shouldSatisfy` isInfixOf "method second (returns)"
        Right _ -> pure ()

    it "fails across a method's error and another's params $defs" $ do
      let result = buildIn "t"
            [ [aesonQQ|{ "name": "put", "description": "", "hash": "h", "cardinality": "one", "returns": {},
                "params": { "type": "object", "properties": { "why": { "$ref": "#/$defs/Why" } }, "required": ["why"],
                  "$defs": { "Why": { "type": "string", "enum": ["a", "b"] } } } }|]
            , [aesonQQ|{ "name": "get", "description": "", "hash": "h", "cardinality": "one", "returns": {},
                "error": { "title": "Why", "type": "string", "enum": ["c"] } }|] ]
      result `shouldBe` Left TitleCollision
        { tcNamespace = "t"
        , tcTitle = "Why"
        , tcFirst = TypeSource "put" "params"
        , tcSecond = TypeSource "get" "error" }

    it "the same title in two namespaces is two types" $ do
      a <- either (fail . T.unpack . renderIRBuildError) pure (buildIn "a" [withReturns "one" pongSchema])
      b <- either (fail . T.unpack . renderIRBuildError) pure
        (buildIn "b" [withReturns "one" [aesonQQ|{ "title": "Pong", "type": "object", "properties": { "reply": { "type": "integer" } }, "required": ["reply"] }|]])
      Map.keys (irTypes (mergeIR a b)) `shouldBe` ["a.Pong", "b.Pong"]

-- ============================================================================
-- Helpers
-- ============================================================================

pongSchema :: Value
pongSchema = [aesonQQ|{ "$schema": "https://json-schema.org/draft/2020-12/schema", "title": "Pong", "description": "What ping answers.",
  "type": "object", "properties": { "message": { "type": "string" } }, "required": ["message"] }|]

-- | A method @m@ with this cardinality and @returns@.
withReturns :: Text -> Value -> Value
withReturns = withReturnsNamed "m"

withReturnsNamed :: Text -> Text -> Value -> Value
withReturnsNamed name card returns = object
  [ "name" .= name, "description" .= ("" :: Text), "hash" .= ("h" :: Text)
  , "returns" .= returns, "cardinality" .= card ]

-- | A plugin in namespace @ns@ holding these 0.7 method schemas, as IR.
buildIn :: Text -> [Value] -> Either IRBuildError IR
buildIn ns methods = case traverse Aeson.fromJSON methods of
  Aeson.Error e -> error ("bad MethodSchema: " <> e)
  Aeson.Success ms -> buildIRFromSchema "test" [ns] PluginSchema
    { psNamespace = ns, psVersion = "1", psDescription = "", psLongDescription = Nothing
    , psSelfHash = Nothing, psHash = "h", psMethods = ms, psChildren = Nothing
    , psDeprecation = Nothing, psRequest = Nothing }

build :: [Value] -> IO IR
build = either (fail . T.unpack . renderIRBuildError) pure . buildIn "t"

single :: Value -> IO MethodDef
single m = do
  ir <- build [m]
  case Map.elems (irMethods ir) of
    [md] -> pure md
    mds -> fail ("expected one method, got " <> show (length mds))

returnsOf :: IR -> TypeRef
returnsOf ir = maybe RefUnknown mdReturns (Map.lookup "t.m" (irMethods ir))

named :: Text -> Text -> TypeRef
named ns n = RefNamed (QualifiedName ns n)

-- | A type's kind, descriptions aside (of its fields and variants too).
kindOf :: IR -> Text -> Maybe TypeKind
kindOf ir name = undocumented . tdKind <$> Map.lookup name (irTypes ir)
  where
    undocumented = \case
      KindStruct fs -> KindStruct (map field fs)
      KindEnum d vs -> KindEnum d [ v { vdDescription = Nothing, vdFields = map field (vdFields v) } | v <- vs ]
      k -> k
    field f
      | name == "conformance.Note" = f  -- this row checks the field docs too
      | otherwise = f { fdDescription = Nothing }

isStruct :: TypeKind -> Bool
isStruct KindStruct{} = True
isStruct _ = False

-- | Every RefNamed in the IR's methods and types with no TypeDef.
danglingRefs :: IR -> [Text]
danglingRefs ir =
  [ n | r <- concatMap methodRefs (Map.elems (irMethods ir)) ++ concatMap typeRefs (Map.elems (irTypes ir))
      , n <- names r, not (Map.member n (irTypes ir)) ]
  where
    methodRefs md = mdReturns md : maybe [] pure (mdError md) ++ maybe [] pure (mdProgress md) ++ map pdType (mdParams md)
    typeRefs td = case tdKind td of
      KindStruct fs -> map fdType fs
      KindEnum _ vs -> map fdType (concatMap vdFields vs)
      KindAlias t -> [t]
      _ -> []
    names = \case
      RefNamed qn -> [qualifiedNameFull qn]
      RefArray t -> names t
      RefOptional t -> names t
      _ -> []
