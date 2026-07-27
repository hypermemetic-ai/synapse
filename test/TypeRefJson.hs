{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | PLX-157 — @typeref-json@, which until now was a registered test suite
-- containing __zero assertions__.
--
-- = Why this file existed, and why it was worse than nothing
--
-- It printed the JSON encodings of 'TypeRef', 'QualifiedName' and 'FieldDef'
-- and always exited 0. It was __passing by construction__: no input could make
-- it fail, so it contributed a green tick to every count while proving
-- nothing. PLX-124 named it as a deletion candidate and correctly declined to
-- take the decision.
--
-- = The decision (PLX-157): assertions, not deletion
--
-- Deleting it was the wrong call, because __what it was printing is a live
-- cross-language contract and nothing else pins it__. @synapse --emit-ir@
-- serialises the IR with exactly these derived instances
-- (@app\/Main.hs@: @buildIR ... >>= liftIO . LBS.putStrLn . encode@), and
-- hub-codegen parses that JSON from Rust and TypeScript. The
-- @deriving anyclass ToJSON@ on 'TypeRef' means the sum-type tag lives under
-- @"tag"@ with the payload under @"contents"@, and the @FieldDef@ record keeps
-- its Haskell field prefixes (@fdName@, @fdType@, ...) on the wire. Both are
-- consequences of a @deriving@ clause rather than a stated decision, which is
-- precisely the kind of thing that changes by accident: adding
-- @genericToJSON@ options, or a @sumEncoding@, would silently rename every key
-- a generator reads.
--
-- So the file keeps its subject and gains the thing it lacked. Every encoding
-- it used to print is now pinned as a value, and the suite can fail.
--
-- The pins are on decoded 'Value's, not on byte strings: key ORDER is not part
-- of the contract (a JSON object is unordered and aeson does not promise an
-- order), whereas key NAMES and the tag\/contents shape are exactly what a
-- foreign generator binds to.
module Main (main) where

import Data.Aeson (Value (..), encode, decode, eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BSL
import Test.Hspec

import Synapse.IR.Types

-- | Parse a literal expectation, failing loudly if the literal itself is bad.
json :: BSL.ByteString -> Value
json s = case eitherDecode s of
  Left e -> error ("bad expectation literal in TypeRefJson: " <> e)
  Right v -> v

-- | What a foreign generator would actually receive.
wire :: TypeRef -> Value
wire r = case decode (encode r) of
  Nothing -> error "encoded TypeRef did not round-trip through decode"
  Just v -> v

qnCone :: QualifiedName
qnCone = QualifiedName {qnNamespace = "cone", qnLocalName = "UUID"}

qnGlobal :: QualifiedName
qnGlobal = QualifiedName {qnNamespace = "", qnLocalName = "GlobalType"}

qnArbor :: QualifiedName
qnArbor = QualifiedName {qnNamespace = "arbor", qnLocalName = "UUID"}

sampleField :: FieldDef
sampleField =
  FieldDef
    { fdName = "tree_id"
    , fdType = RefNamed qnArbor
    , fdDescription = Just "Tree identifier"
    , fdRequired = True
    , fdDefault = Nothing
    }

main :: IO ()
main = hspec $ do
  describe "TypeRef — the tag/contents encoding --emit-ir puts on the wire" $ do
    it "RefNamed carries a namespaced QualifiedName under contents" $
      wire (RefNamed qnCone)
        `shouldBe` json
          "{\"tag\":\"RefNamed\",\
          \\"contents\":{\"qnNamespace\":\"cone\",\"qnLocalName\":\"UUID\"}}"

    it "an EMPTY namespace is emitted, not omitted" $
      -- A generator distinguishes a global type from a namespaced one by this
      -- field being present and empty. Dropping it (e.g. via omitNothingFields
      -- on a Maybe) would make the two indistinguishable.
      wire (RefNamed qnGlobal)
        `shouldBe` json
          "{\"tag\":\"RefNamed\",\
          \\"contents\":{\"qnNamespace\":\"\",\"qnLocalName\":\"GlobalType\"}}"

    it "nesting is by contents, all the way down" $
      wire (RefOptional (RefArray (RefNamed qnArbor)))
        `shouldBe` json
          "{\"tag\":\"RefOptional\",\
          \\"contents\":{\"tag\":\"RefArray\",\
          \\"contents\":{\"tag\":\"RefNamed\",\
          \\"contents\":{\"qnNamespace\":\"arbor\",\"qnLocalName\":\"UUID\"}}}}"

    it "every TypeRef round-trips through its own codec" $ do
      let refs =
            [ RefNamed qnCone
            , RefNamed qnGlobal
            , RefArray (RefNamed qnArbor)
            , RefOptional (RefArray (RefNamed qnArbor))
            ]
      mapM_ (\r -> decode (encode r) `shouldBe` Just r) refs

  describe "QualifiedName — the dotted rendering generators key on" $ do
    it "a namespaced name renders dotted" $
      qualifiedNameFull qnCone `shouldBe` "cone.UUID"

    it "an unnamespaced name renders BARE — no leading dot" $
      -- ".GlobalType" would be a different identifier in every target
      -- language, and this is the only place the distinction is enforced.
      qualifiedNameFull qnGlobal `shouldBe` "GlobalType"

  describe "FieldDef — the record keys hub-codegen binds to" $ do
    it "keeps its Haskell field prefixes on the wire" $
      decode (encode sampleField)
        `shouldBe` Just
          ( json
              "{\"fdName\":\"tree_id\",\
              \\"fdType\":{\"tag\":\"RefNamed\",\
              \\"contents\":{\"qnNamespace\":\"arbor\",\"qnLocalName\":\"UUID\"}},\
              \\"fdDescription\":\"Tree identifier\",\
              \\"fdRequired\":true,\
              \\"fdDefault\":null}"
          )

    it "an absent default is null, not an omitted key" $ do
      -- A generator that reads fdDefault must be able to tell "no default"
      -- from "key missing because the encoder changed".
      let v = decode (encode sampleField) :: Maybe Value
      case v of
        Just val -> hasKey "fdDefault" val `shouldBe` True
        Nothing -> expectationFailure "FieldDef did not encode to an object"

    it "round-trips" $
      decode (encode sampleField) `shouldBe` Just sampleField
  where
    hasKey k v = case v of
      Object o -> KM.member k o
      _ -> False
