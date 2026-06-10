{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | R-3 (ROLES+AP merged wave, trak facet 9406a11e): synapse decodes and
--   renders the R-2 credential/posture wire metadata.
--
--   Covers:
--
--   1. Decode-with-fields: the literal Rust-derived wire JSON (see below)
--      decodes into 'MethodSchema' with @auth_posture@, @public@, and the
--      full @requires_credential@ (kind + scopes + site_hint) populated.
--   2. Decode-without-fields: the pinned pre-R-2 wire snapshot decodes
--      with the additive defaults (Nothing / False) — decode never fails
--      on old backends.
--   3. Builder projection: 'extractMethodDef' carries all three fields
--      onto 'MethodDef' ('mdAuthPosture', 'mdPublic',
--      'mdRequiresCredential').
--   4. Render round-trip: synapse's IR JSON emits the fields under the
--      EXACT wire names pinned by plexus-core R-2 — @auth_posture@,
--      @public@, @requires_credential@ — with values 'Value'-equal to the
--      fixture's subtrees, omitted when absent (no nulls), and the result
--      decodes back losslessly.
--   5. Human-readable output: 'formatAuthLine' annotation (consumed by
--      the schema tree view and @renderMethodHelp@); absent metadata
--      produces no annotation (byte-identical legacy output).
--
--   == Fixture provenance
--
--   The three fixtures below are byte-for-byte the JSON emitted by
--   plexus-core's serde derives on branch @feature/R-5-scope-gate@
--   (contains R-2 commit @80eaba7@), captured by running a dump test in
--   @plexus-core/tests@ that mirrors the R-2 wire snapshot tests
--   (@r2_populated_fields_roundtrip@, @r2_pre_r2_snapshot_decodes_with_defaults@,
--   @cred_core_3_required_credential_roundtrip@):
--
--   @
--   GATED  {"name":"send","description":"sends","hash":"h_send","streaming":false,"bidirectional":false,"http_method":"POST","role":{"kind":"rpc"},"requires_credential":{"kind":{"kind":"oauth_access"},"scopes":["cone.send_message","cone.admin"],"site_hint":{"site":"header","name":"authorization"}},"auth_posture":"required"}
--   PUBLIC {"name":"login","description":"logs in","hash":"h_login","streaming":false,"bidirectional":false,"http_method":"POST","role":{"kind":"rpc"},"auth_posture":"mixed","public":true}
--   OLD    {"name":"ping","description":"pong","hash":"h_ping","streaming":false,"bidirectional":false,"http_method":"POST","role":{"kind":"rpc"}}
--   @
module Main where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.=), eitherDecode, encode, object, decode)
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import Plexus.Schema.Recursive
  ( AttachmentSite(..)
  , AuthPosture(..)
  , CredentialKind(..)
  , MethodSchema(..)
  , RequiredCredential(..)
  )
import Synapse.Algebra.Render (formatAuthLine)
import Synapse.CLI.Help (renderMethodHelp)
import Synapse.IR.Types (MethodDef(..), emptyIR)
import Synapse.IR.Builder (extractMethodDef)

-- ============================================================================
-- Literal Rust-derived fixtures (see module header for provenance)
-- ============================================================================

-- | GATED: full 'RequiredCredential' (kind + scopes + site_hint) plus a
--   declared posture. Not public, so no @public@ key on the wire.
fixtureGatedJson :: Value
fixtureGatedJson = [aesonQQ|
{"name":"send","description":"sends","hash":"h_send","streaming":false,"bidirectional":false,"http_method":"POST","role":{"kind":"rpc"},"requires_credential":{"kind":{"kind":"oauth_access"},"scopes":["cone.send_message","cone.admin"],"site_hint":{"site":"header","name":"authorization"}},"auth_posture":"required"}
|]

-- | PUBLIC: explicitly public method with a declared posture; no
--   @requires_credential@ (mutually exclusive with @public@ by
--   construction on the producer side).
fixturePublicJson :: Value
fixturePublicJson = [aesonQQ|
{"name":"login","description":"logs in","hash":"h_login","streaming":false,"bidirectional":false,"http_method":"POST","role":{"kind":"rpc"},"auth_posture":"mixed","public":true}
|]

-- | OLD: the pinned pre-R-2 wire shape — byte-for-byte the
--   @r2_pre_r2_snapshot_decodes_with_defaults@ literal from
--   plexus-core's wire snapshot tests. No R-2 keys at all.
fixtureOldWireJson :: Value
fixtureOldWireJson = [aesonQQ|
{"name":"ping","description":"pong","hash":"h_ping","streaming":false,"bidirectional":false,"http_method":"POST","role":{"kind":"rpc"}}
|]

-- | The expected decode of the GATED fixture's @requires_credential@.
expectedGatedRequirement :: RequiredCredential
expectedGatedRequirement = RequiredCredential
  { rcKind     = Just CredKindOauthAccess
  , rcScopes   = ["cone.send_message", "cone.admin"]
  , rcSiteHint = Just (AttachHeader "authorization")
  }

-- | Decode a fixture or fail the test.
decodeSchema :: Value -> IO MethodSchema
decodeSchema v = case Aeson.fromJSON v of
  Aeson.Error e   -> expectationFailure ("fixture decode failed: " <> e)
                       >> fail "unreachable"
  Aeson.Success m -> pure m

-- | Project a fixture through the IR builder.
buildMethodDef :: Value -> IO MethodDef
buildMethodDef v = do
  m <- decodeSchema v
  let (_, mdef) = extractMethodDef "cone" "cone" m
  pure mdef

-- | Encode to a JSON object's KeyMap or fail.
encodedObject :: Aeson.ToJSON a => a -> IO (KM.KeyMap Value)
encodedObject x = case Aeson.toJSON x of
  Object o -> pure o
  other    -> expectationFailure ("expected JSON object, got: " <> show other)
                >> fail "unreachable"

main :: IO ()
main = hspec $ do

  describe "R-3: MethodSchema decodes the R-2 wire fields" $ do

    it "decode-with-fields: GATED fixture populates posture + full requires_credential" $ do
      m <- decodeSchema fixtureGatedJson
      methodName m               `shouldBe` "send"
      methodAuthPosture m        `shouldBe` Just AuthPostureRequired
      methodPublic m             `shouldBe` False
      methodRequiresCredential m `shouldBe` Just expectedGatedRequirement

    it "decode-with-fields: PUBLIC fixture populates public + posture, no requirement" $ do
      m <- decodeSchema fixturePublicJson
      methodName m               `shouldBe` "login"
      methodAuthPosture m        `shouldBe` Just AuthPostureMixed
      methodPublic m             `shouldBe` True
      methodRequiresCredential m `shouldBe` Nothing

    it "decode-without-fields: pinned pre-R-2 snapshot decodes with defaults" $ do
      m <- decodeSchema fixtureOldWireJson
      methodName m               `shouldBe` "ping"
      methodAuthPosture m        `shouldBe` Nothing
      methodPublic m             `shouldBe` False
      methodRequiresCredential m `shouldBe` Nothing

    it "all four posture wire names decode (snake_case, matching the Rust enum)" $ do
      let postures = [ ("required", AuthPostureRequired)
                     , ("optional", AuthPostureOptional)
                     , ("mixed",    AuthPostureMixed)
                     , ("none",     AuthPostureNone)
                     ] :: [(Text, AuthPosture)]
      mapM_ (\(wire, expected) -> do
        case Aeson.fromJSON (String wire) of
          Aeson.Error e   -> expectationFailure ("posture decode failed: " <> e)
          Aeson.Success p -> p `shouldBe` expected
        Aeson.toJSON expected `shouldBe` String wire)
        postures

    it "MethodSchema wire round-trip is lossless for all three fixtures" $ do
      mapM_ (\fixture -> do
        origin <- decodeSchema fixture
        case decode (encode origin) :: Maybe MethodSchema of
          Nothing      -> expectationFailure "re-decode after encode failed"
          Just decoded -> decoded `shouldBe` origin)
        [fixtureGatedJson, fixturePublicJson, fixtureOldWireJson]

    it "MethodSchema re-encode preserves the exact wire names and omission rules" $ do
      gated <- decodeSchema fixtureGatedJson >>= encodedObject
      KM.lookup "auth_posture" gated `shouldBe` Just (String "required")
      KM.lookup "requires_credential" gated
        `shouldBe` KM.lookup "requires_credential"
                     (case fixtureGatedJson of Object o -> o; _ -> KM.empty)
      KM.member "public" gated `shouldBe` False

      public <- decodeSchema fixturePublicJson >>= encodedObject
      KM.lookup "auth_posture" public `shouldBe` Just (String "mixed")
      KM.lookup "public" public       `shouldBe` Just (Bool True)
      KM.member "requires_credential" public `shouldBe` False

      old <- decodeSchema fixtureOldWireJson >>= encodedObject
      KM.member "auth_posture" old         `shouldBe` False
      KM.member "public" old               `shouldBe` False
      KM.member "requires_credential" old  `shouldBe` False

  describe "R-3: Builder projection carries the fields onto MethodDef" $ do

    it "GATED fixture: mdAuthPosture / mdRequiresCredential populated, mdPublic False" $ do
      mdef <- buildMethodDef fixtureGatedJson
      mdName mdef               `shouldBe` "send"
      mdAuthPosture mdef        `shouldBe` Just AuthPostureRequired
      mdPublic mdef             `shouldBe` False
      mdRequiresCredential mdef `shouldBe` Just expectedGatedRequirement

    it "PUBLIC fixture: mdPublic True, posture carried, no requirement" $ do
      mdef <- buildMethodDef fixturePublicJson
      mdAuthPosture mdef        `shouldBe` Just AuthPostureMixed
      mdPublic mdef             `shouldBe` True
      mdRequiresCredential mdef `shouldBe` Nothing

    it "OLD fixture: defaults carried verbatim (Nothing / False)" $ do
      mdef <- buildMethodDef fixtureOldWireJson
      mdAuthPosture mdef        `shouldBe` Nothing
      mdPublic mdef             `shouldBe` False
      mdRequiresCredential mdef `shouldBe` Nothing

  describe "R-3: IR JSON render (hub-codegen's input) uses the exact wire names" $ do

    it "GATED: auth_posture + requires_credential emitted, Value-equal to the fixture subtrees; public omitted" $ do
      mdef <- buildMethodDef fixtureGatedJson
      o <- encodedObject mdef
      KM.lookup "auth_posture" o `shouldBe` Just (String "required")
      KM.lookup "requires_credential" o
        `shouldBe` KM.lookup "requires_credential"
                     (case fixtureGatedJson of Object fo -> fo; _ -> KM.empty)
      KM.member "public" o `shouldBe` False

    it "PUBLIC: public + auth_posture emitted; requires_credential omitted (no null)" $ do
      mdef <- buildMethodDef fixturePublicJson
      o <- encodedObject mdef
      KM.lookup "public" o       `shouldBe` Just (Bool True)
      KM.lookup "auth_posture" o `shouldBe` Just (String "mixed")
      KM.member "requires_credential" o `shouldBe` False

    it "OLD: none of the three wire keys appear (additive — old IR shape intact)" $ do
      mdef <- buildMethodDef fixtureOldWireJson
      o <- encodedObject mdef
      KM.member "auth_posture" o         `shouldBe` False
      KM.member "public" o               `shouldBe` False
      KM.member "requires_credential" o  `shouldBe` False

    it "MethodDef IR JSON round-trips losslessly for all three fixtures" $ do
      mapM_ (\fixture -> do
        mdef <- buildMethodDef fixture
        case eitherDecode (encode mdef) :: Either String MethodDef of
          Left err      -> expectationFailure ("re-decode failed: " <> err)
          Right decoded -> decoded `shouldBe` mdef)
        [fixtureGatedJson, fixturePublicJson, fixtureOldWireJson]

    it "pre-R-3 IR JSON (no new keys) decodes with defaults — decode never fails on old caches" $ do
      let preR3Json = object
            [ "mdName"        .= ("ping" :: Text)
            , "mdFullPath"    .= ("util.ping" :: Text)
            , "mdNamespace"   .= ("util" :: Text)
            , "mdDescription" .= Aeson.Null
            , "mdStreaming"   .= False
            , "mdParams"      .= ([] :: [Value])
            , "mdReturns"     .= object [ "tag" .= ("RefAny" :: Text) ]
            ]
      case Aeson.fromJSON preR3Json :: Aeson.Result MethodDef of
        Aeson.Error e      -> expectationFailure ("decode failed: " <> e)
        Aeson.Success mdef -> do
          mdAuthPosture mdef        `shouldBe` Nothing
          mdPublic mdef             `shouldBe` False
          mdRequiresCredential mdef `shouldBe` Nothing

    it "legacy mdRequiresCredential key (AUTHZ-CRED-IR-1 IR JSON) still decodes" $ do
      let legacyJson = object
            [ "mdName"        .= ("send" :: Text)
            , "mdFullPath"    .= ("cone.send" :: Text)
            , "mdNamespace"   .= ("cone" :: Text)
            , "mdDescription" .= Aeson.Null
            , "mdStreaming"   .= False
            , "mdParams"      .= ([] :: [Value])
            , "mdReturns"     .= object [ "tag" .= ("RefAny" :: Text) ]
            , "mdRequiresCredential" .= expectedGatedRequirement
            ]
      case Aeson.fromJSON legacyJson :: Aeson.Result MethodDef of
        Aeson.Error e      -> expectationFailure ("decode failed: " <> e)
        Aeson.Success mdef ->
          mdRequiresCredential mdef `shouldBe` Just expectedGatedRequirement

  describe "R-3: human-readable schema output" $ do

    it "formatAuthLine renders the GATED combination" $
      formatAuthLine False (Just expectedGatedRequirement) (Just AuthPostureRequired)
        `shouldBe` Just "(auth: requires credential kind=oauth_access scopes=cone.send_message,cone.admin site=header:authorization; posture: required)"

    it "formatAuthLine renders the PUBLIC combination" $
      formatAuthLine True Nothing (Just AuthPostureMixed)
        `shouldBe` Just "(auth: public; posture: mixed)"

    it "formatAuthLine is Nothing when no metadata is present (legacy output byte-identical)" $
      formatAuthLine False Nothing Nothing `shouldBe` Nothing

    it "renderMethodHelp surfaces the auth annotation for a public method" $ do
      mdef <- buildMethodDef fixturePublicJson
      let help = renderMethodHelp emptyIR mdef
      help `shouldSatisfy` T.isInfixOf "(auth: public; posture: mixed)"

    it "renderMethodHelp output is annotation-free for a pre-R-2 method" $ do
      mdef <- buildMethodDef fixtureOldWireJson
      let help = renderMethodHelp emptyIR mdef
      help `shouldSatisfy` (not . T.isInfixOf "(auth:")
