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
--   * RETURNS-40: the IR of a whole service, built from every schema the
--     walk reads ('buildIRFromSchemas') — the fixture
--     @../plexus-macros/fixtures/ir-service/expected/@: @schemas.json@ in,
--     @ir.json@ (the service), @ir.alpha.json@ (its @alpha@ subtree) and
--     @clash.ir.json@ (a title collision refuses its activation) and
--     @clash.one.error.txt@ (the one-plugin entry still fails) out. The service writes
--     its own IR (@<service>.ir@, plexus-core), held equal to these files.
--     @SYNAPSE_GOLDEN_ACCEPT=1@ rewrites them too.
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
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec

import Plexus.Schema.Recursive (MethodSchema(..), PluginSchema(..))
import Synapse.IR.Builder
  ( IRBuildError(..)
  , TypeSource(..)
  , buildIRFromSchema
  , buildIRFromSchemas
  , renderIRBuildError
  )
import Synapse.IR.Types

-- ============================================================================
-- The fixture
-- ============================================================================

-- | The fixture's directory: by sibling path, or wherever
--   @CONFORMANCE_FIXTURE_DIR@ says (a copy, to check an ir.json before it
--   is committed beside the fixture).
fixtureDirIO :: IO FilePath
fixtureDirIO = maybe (".." </> "plexus-macros" </> "fixtures" </> "conformance" </> "expected") id
  <$> lookupEnv "CONFORMANCE_FIXTURE_DIR"

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
  fixtureDir <- fixtureDirIO
  let schemaPath = fixtureDir </> "schema.json"
      irPath = fixtureDir </> "ir.json"
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
  service <- serviceGoldens (accept == Just "1")
  hard <- hardGoldens (accept == Just "1")
  hspec $ do
    goldenSpec schemaPath irPath ir golden
    tableSpec ir
    service
    hard

-- | RETURNS-40's review: the reviewer's hard shapes (generics, recursion,
--   every serde enum form, a dotted $defs name, flattening, every number's
--   default, one title with two docs in two namespaces), served by
--   plexus-core's tests/ir_serving.rs and kept beside it
--   (@../plexus-core/tests/fixtures/ir-hard/@, or @IR_HARD_FIXTURE_DIR@):
--   @schemas.json@ in; @ir.json@, @ir.alpha.json@ and @ir.beta-inner.json@
--   out — the IR plexus-core's service writes is held to them.
hardGoldens :: Bool -> IO Spec
hardGoldens acceptIt = do
  dir <- maybe (".." </> "plexus-core" </> "tests" </> "fixtures" </> "ir-hard") id <$> lookupEnv "IR_HARD_FIXTURE_DIR"
  schemas <- readSchemas (dir </> "schemas.json")
  let builds =
        [ ("ir.json", buildIRFromSchemas "xsvc" [[]] schemas)
        , ("ir.alpha.json", buildIRFromSchemas "xsvc" [["alpha"]] schemas)
        , ("ir.beta-inner.json", buildIRFromSchemas "xsvc" [["beta"], ["alpha", "inner"]] schemas)
        ]
  irs <- traverse (\(file, built) -> (,) file <$> either (fail . T.unpack . renderIRBuildError) pure built) builds
  if acceptIt then mapM_ (\(file, ir) -> LBS.writeFile (dir </> file) (renderIR ir) >> putStrLn ("wrote " <> dir </> file)) irs else pure ()
  goldens <- traverse (\(file, ir) -> (\g -> (file, ir, g)) <$> LBS.readFile (dir </> file)) irs
  pure $ describe "golden: the reviewer's hard shapes (RETURNS-40)" $
    forM_ goldens $ \(file, ir, g) -> do
      it (file <> " byte for byte") $
        unless (renderIR ir == g) $ expectationFailure ("IR built from " <> dir </> "schemas.json differs from " <> file)
      it (file <> ": no RefNamed dangles") $
        danglingRefs ir `shouldBe` []

-- ============================================================================
-- RETURNS-40: a whole service's IR
-- ============================================================================

-- | The service fixture's directory: by sibling path, or wherever
--   @IR_SERVICE_FIXTURE_DIR@ says.
serviceDirIO :: IO FilePath
serviceDirIO = maybe (".." </> "plexus-macros" </> "fixtures" </> "ir-service" </> "expected") id
  <$> lookupEnv "IR_SERVICE_FIXTURE_DIR"

-- | Schemas by dotted path (@""@ the root), as the fixture keeps them.
readSchemas :: FilePath -> IO (Map.Map [Text] PluginSchema)
readSchemas path = do
  bytes <- LBS.readFile path
  byName <- either (fail . ((path <> ": ") <>)) pure (eitherDecode bytes) :: IO (Map.Map Text PluginSchema)
  pure (Map.mapKeys (\k -> if T.null k then [] else T.splitOn "." k) byName)

-- | Each golden of the service fixture: built, written when accepting, and
--   compared byte for byte.
serviceGoldens :: Bool -> IO Spec
serviceGoldens acceptIt = do
  dir <- serviceDirIO
  schemas <- readSchemas (dir </> "schemas.json")
  clash <- readSchemas (dir </> "clash.schemas.json")
  let builds =
        [ ("ir.json", "the whole service", buildIRFromSchemas "irsvc" [[]] schemas)
        , ("ir.alpha.json", "its alpha subtree", buildIRFromSchemas "irsvc" [["alpha"]] schemas)
        , ("clash.ir.json", "the clash service", buildIRFromSchemas "clashsvc" [[]] clash)
        ]
      -- RETURNS-36 review #9: the walk refuses the colliding activation and
      -- goes on; the one-plugin entry still fails
      clashOne = case Map.lookup ["clash"] clash of
        Just s -> either (\e -> LBS.fromStrict (TE.encodeUtf8 (renderIRBuildError e)) <> "\n") (const "built") (buildIRFromSchema "clashsvc" ["clash"] s)
        Nothing -> "no clash schema"
  irs <- traverse (\(file, what, built) -> do
           ir <- either (fail . T.unpack . renderIRBuildError) pure built
           pure (file, what, ir)) builds
  if acceptIt
    then do
      mapM_ (\(file, _, ir) -> LBS.writeFile (dir </> file) (renderIR ir) >> putStrLn ("wrote " <> dir </> file)) irs
      LBS.writeFile (dir </> "clash.one.error.txt") clashOne
    else pure ()
  goldens <- traverse (\(file, what, ir) -> (\g -> (file, what, ir, g)) <$> LBS.readFile (dir </> file)) irs
  clashGolden <- LBS.readFile (dir </> "clash.one.error.txt")
  pure $ describe "golden: the ir-service fixture's IR (RETURNS-40)" $ do
    forM_ goldens $ \(file, what, ir, g) -> do
      it (what <> " equals " <> file <> " byte for byte") $
        unless (renderIR ir == g) $ expectationFailure $
          "IR built from " <> dir </> "schemas.json" <> " differs from " <> file
            <> " (regenerate with SYNAPSE_GOLDEN_ACCEPT=1 and read the diff)"
      -- (Not "decodes back to the same IR": the root's call.params has
      -- "default": null, which the IR writes as pdDefault null — the same
      -- null an absent default writes — and reads back as no default.)
      it (what <> ": " <> file <> " decodes") $
        (eitherDecode g :: Either String IR) `shouldSatisfy` either (const False) (const True)
      it (what <> ": no RefNamed dangles") $
        danglingRefs ir `shouldBe` []
    it "the whole service carries the reader's refusals" $
      case [ ir | (f, _, ir, _) <- goldens, f == "ir.json" ] of
        (ir : _) -> null (irRefusals ir) `shouldBe` False
        [] -> expectationFailure "no ir.json"
    it "the clash service's walk refuses clash (keyword title, both sites named) and builds the rest" $
      case [ ir | (f, _, ir, _) <- goldens, f == "clash.ir.json" ] of
        (ir : _) -> do
          [ rfSite r | r <- irRefusals ir, rfKeyword r == "title" ] `shouldBe` ["clash"]
          [ m | m <- Map.keys (irMethods ir), "clash." `T.isPrefixOf` m ] `shouldBe` []
          Map.member "" (irPlugins ir) `shouldBe` True
        [] -> expectationFailure "no clash.ir.json"
    it "the clash plugin alone (buildIRFromSchema) still fails, as clash.one.error.txt says" $
      clashOne `shouldBe` clashGolden

-- ============================================================================
-- Golden
-- ============================================================================

goldenSpec :: FilePath -> FilePath -> IR -> LBS.ByteString -> Spec
goldenSpec schemaPath irPath ir golden = describe "golden: the conformance fixture's IR" $ do
  it "equals expected/ir.json byte for byte" $
    unless (renderIR ir == golden) $ expectationFailure $
      "IR built from " <> schemaPath <> " differs from " <> irPath
        <> " (regenerate with SYNAPSE_GOLDEN_ACCEPT=1 and read the diff)"

  it "expected/ir.json decodes back to the same IR" $
    eitherDecode golden `shouldBe` Right ir

  it "is IR 3.1 (ASK-1: 3.0 without the bidir keys)" $
    irVersion ir `shouldBe` "3.1"

  it "has every method of the schema, keyed by full path" $
    Map.keys (irMethods ir) `shouldBe`
      [ "conformance.count", "conformance.load", "conformance.lookup", "conformance.ping"
      , "conformance.scan", "conformance.schema", "conformance.warm" ]

  it "carries the schema's own hash for the plugin" $ do
    -- the fixture's hash moves with every change to the fixture (RETURNS-10);
    -- read it from schema.json rather than pin one
    schema <- either fail pure . eitherDecode =<< LBS.readFile schemaPath
    fmap phiHash (Map.lookup "conformance" =<< irPluginHashes ir) `shouldBe` Just (psHash schema)

  it "no MethodDef carries mdStreaming" $
    forM_ (Map.elems (irMethods ir)) $ \md -> case Aeson.toJSON md of
      Object o -> KM.member "mdStreaming" o `shouldBe` False
      other -> expectationFailure ("not an object: " <> show other)

  it "no RefNamed dangles" $
    danglingRefs ir `shouldBe` []

  it "reports synapse's own version" $
    synapseVersion `shouldBe` "4.0.0"

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
      kindOf ir "t.Shape" `shouldBe` Just (taggedEnum "type"
        [ variant "circle" Nothing [FieldDef "r" (RefPrimitive "number" Nothing) Nothing True Nothing]
        , variant "dot" Nothing [] ])

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

    it "any value ({}): the JSON type (IR 2.1's RefJson)" $ do
      ir <- build [withReturns "one" [aesonQQ|{}|]]
      returnsOf ir `shouldBe` RefJson
      Map.keys (irTypes ir) `shouldBe` []

    it "the fixture's schema method: SchemaResult, an untagged union of its members (IR 2.1's untagged form); its members are TypeDefs" $ do
      fmap mdReturns (Map.lookup "conformance.schema" (irMethods fixture))
        `shouldBe` Just (named "conformance" "SchemaResult")
      kindOf fixture "conformance.SchemaResult" `shouldBe` Just (untaggedEnum
        [ ("PluginSchema", named "conformance" "PluginSchema"), ("MethodSchema", named "conformance" "MethodSchema") ])
      fmap isStruct (kindOf fixture "conformance.PluginSchema") `shouldBe` Just True
      fmap isStruct (kindOf fixture "conformance.MethodSchema") `shouldBe` Just True

    it "an untagged union is IR 2.1's untagged enum, each variant its type, never a partial enum" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Either", "anyOf": [ { "$ref": "#/$defs/A" }, { "$ref": "#/$defs/B" } ],
          "$defs": { "A": { "type": "object", "properties": { "a": { "type": "string" } }, "required": ["a"] },
                     "B": { "type": "object", "properties": { "b": { "type": "string" } }, "required": ["b"] } } }|]]
      returnsOf ir `shouldBe` named "t" "Either"
      kindOf ir "t.Either" `shouldBe` Just (untaggedEnum [("A", named "t" "A"), ("B", named "t" "B")])
      danglingRefs ir `shouldBe` []

  describe "error present → mdError = Just ref with TypeDef; absent → Nothing" $ do
    it "lookup's error is conformance.LookupError, a tagged enum" $ do
      fmap mdError (Map.lookup "conformance.lookup" (irMethods fixture))
        `shouldBe` Just (Just (named "conformance" "LookupError"))
      kindOf fixture "conformance.LookupError" `shouldBe` Just (taggedEnum "type"
        [ variant "not_found" Nothing [FieldDef "id" (RefPrimitive "string" Nothing) Nothing True Nothing] ])

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
      kindOf fixture "conformance.LoadStep" `shouldBe` Just (taggedEnum "type"
        [ variant "batch" Nothing
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
      kindOf fixture "conformance.MethodRole" `shouldBe` Just (taggedEnum "kind"
        [ variant "rpc" Nothing []
        , variant "static_child" Nothing []
        , variant "dynamic_child" Nothing
            [ FieldDef "list_method" (RefOptional (RefPrimitive "string" Nothing)) Nothing False Nothing
            , FieldDef "search_method" (RefOptional (RefPrimitive "string" Nothing)) Nothing False Nothing ] ])

    it "a tag called kind" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [
          { "type": "object", "properties": { "kind": { "type": "string", "const": "created" }, "id": { "type": "string" } }, "required": ["kind", "id"] },
          { "type": "object", "properties": { "kind": { "type": "string", "const": "deleted" } }, "required": ["kind"] } ] }|]]
      kindOf ir "t.Event" `shouldBe` Just (taggedEnum "kind"
        [ variant "created" Nothing [FieldDef "id" (RefPrimitive "string" Nothing) Nothing True Nothing]
        , variant "deleted" Nothing [] ])

    it "an adjacently tagged enum: the tag, with its content a field" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Msg", "oneOf": [
          { "type": "object", "properties": { "t": { "type": "string", "const": "text" }, "c": { "type": "string" } }, "required": ["t", "c"] },
          { "type": "object", "properties": { "t": { "type": "string", "const": "count" }, "c": { "type": "integer" } }, "required": ["t", "c"] } ] }|]]
      kindOf ir "t.Msg" `shouldBe` Just (taggedEnum "t"
        [ variant "text" Nothing [FieldDef "c" (RefPrimitive "string" Nothing) Nothing True Nothing]
        , variant "count" Nothing [FieldDef "c" (RefPrimitive "integer" Nothing) Nothing True Nothing] ])

    it "variants by $ref" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [ { "$ref": "#/$defs/Up" }, { "$ref": "#/$defs/Down" } ],
          "$defs": {
            "Up": { "type": "object", "properties": { "op": { "type": "string", "const": "up" }, "by": { "type": "integer" } }, "required": ["op", "by"] },
            "Down": { "type": "object", "properties": { "op": { "type": "string", "const": "down" } }, "required": ["op"] } } }|]]
      kindOf ir "t.Event" `shouldBe` Just (taggedEnum "op"
        [ variant "up" Nothing [FieldDef "by" (RefPrimitive "integer" Nothing) Nothing True Nothing]
        , variant "down" Nothing [] ])

  describe "newtype variant over a struct → variant fields = the struct's fields (from $defs)" $ do
    it "the 2020-12 form: $ref beside the tag" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [
          { "type": "object", "properties": { "type": { "type": "string", "const": "moved" } }, "$ref": "#/$defs/Move", "required": ["type"] },
          { "type": "object", "properties": { "type": { "type": "string", "const": "stopped" } }, "required": ["type"] } ],
          "$defs": { "Move": { "type": "object", "properties": { "x": { "type": "integer" }, "note": { "type": ["string", "null"] } }, "required": ["x"] } } }|]]
      kindOf ir "t.Event" `shouldBe` Just (taggedEnum "type"
        [ variant "moved" Nothing
            [ FieldDef "note" (RefOptional (RefPrimitive "string" Nothing)) Nothing False Nothing
            , FieldDef "x" (RefPrimitive "integer" Nothing) Nothing True Nothing ]
        , variant "stopped" Nothing [] ])

    it "the allOf form" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [
          { "type": "object", "properties": { "type": { "type": "string", "const": "moved" } }, "allOf": [ { "$ref": "#/$defs/Move" } ], "required": ["type"] } ],
          "$defs": { "Move": { "type": "object", "properties": { "x": { "type": "integer" } }, "required": ["x"] } } }|]]
      kindOf ir "t.Event" `shouldBe` Just (taggedEnum "type"
        [ variant "moved" Nothing [FieldDef "x" (RefPrimitive "integer" Nothing) Nothing True Nothing] ])

    it "a newtype variant over something with no fields is not a silent partial enum: the site is refused (IR 2.1)" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [
          { "type": "object", "properties": { "type": { "type": "string", "const": "named" } }, "$ref": "#/$defs/Name", "required": ["type"] } ],
          "$defs": { "Name": { "type": "string" } } }|]]
      map rfSite (irRefusals ir) `shouldBe` ["t.Event.named"]

  describe "field not in required → fdRequired = False" $ do
    it "in a struct: Note's percentage" $
      kindOf fixture "conformance.Note" `shouldBe` Just (KindStruct
        [ FieldDef "message" (RefPrimitive "string" Nothing) (Just "What is happening, for a human.") True Nothing
        , FieldDef "percentage" (RefOptional (RefPrimitive "number" (Just "float")))
            (Just "Completion, `0.0`–`100.0`, when the method can say.") False Nothing ])

    it "in a variant" $ do
      ir <- build [withReturns "one" [aesonQQ|{ "title": "Event", "oneOf": [
          { "type": "object", "properties": { "type": { "type": "string", "const": "seen" }, "at": { "type": "integer" }, "by": { "type": "string" } }, "required": ["type", "at"] } ] }|]]
      kindOf ir "t.Event" `shouldBe` Just (taggedEnum "type"
        [ variant "seen" Nothing
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
    , psDeprecation = Nothing, psRequest = Nothing, psRoles = Nothing, psView = Nothing }

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

-- | An internally tagged enum's kind (IR 2.1's 'KindEnum' carries its
--   representation; a tagged variant has no 'vdRef').
taggedEnum :: Text -> [VariantDef] -> TypeKind
taggedEnum tag vs = KindEnum tag vs Internal

-- | A tagged variant: its name, description and fields.
variant :: Text -> Maybe Text -> [FieldDef] -> VariantDef
variant name desc fields = VariantDef name desc fields Nothing

-- | IR 2.1's untagged union: each variant named by its type, its 'vdRef'.
untaggedEnum :: [(Text, TypeRef)] -> TypeKind
untaggedEnum vs = KindEnum "" [ VariantDef n Nothing [] (Just t) | (n, t) <- vs ] Untagged

-- | A type's kind, descriptions aside (of its fields and variants too).
kindOf :: IR -> Text -> Maybe TypeKind
kindOf ir name = undocumented . tdKind <$> Map.lookup name (irTypes ir)
  where
    undocumented = \case
      KindStruct fs -> KindStruct (map field fs)
      KindEnum d vs r -> KindEnum d [ v { vdDescription = Nothing, vdFields = map field (vdFields v) } | v <- vs ] r
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
      KindEnum _ vs _ -> map fdType (concatMap vdFields vs) ++ [ t | Just t <- map vdRef vs ]
      KindAlias t -> [t]
      _ -> []
    -- as irTypes keys a type: the root's are ".Name" (qualifiedNameFull
    -- would say "Name")
    names = \case
      RefNamed qn -> [qnNamespace qn <> "." <> qnLocalName qn]
      RefArray t -> names t
      RefOptional t -> names t
      _ -> []
