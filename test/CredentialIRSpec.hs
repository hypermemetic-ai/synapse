{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | AUTHZ-CRED-IR-1: tests for the credential metadata projection on
--   'Plexus.Schema.Recursive.MethodSchema' and 'Synapse.IR.Types.MethodDef'.
--
--   Acceptance criteria covered (per AUTHZ-CRED-IR-1):
--
--   1. A schema with one method whose return type contains one
--      @Credential\<T\>@ field decodes into a 'MethodDef' whose
--      'mdCredentials' is a single-entry list with the correct
--      'cfdFieldPath', 'cfdVariantTag' (Nothing), and 'cfdMetadata'.
--   2. A schema with multiple credentials per method decodes into a
--      'MethodDef' with one entry per credential, in declaration order.
--   3. A schema for an enum return type with a credential on one variant
--      decodes with @cfdVariantTag = Just \<variant\>@.
--   4. A schema for a method tagged @public@ (no @requires_credential@
--      key in the wire JSON) decodes with @mdRequiresCredential = Nothing@.
--   5. A schema for a method with a scope decodes with
--      @mdRequiresCredential = Just RequiredCredential@ whose scope set
--      matches the method's scope.
--   6. A schema produced before AUTHZ-CRED-CORE-3 lands (no @credentials@
--      or @requires_credential@ fields present in the JSON) decodes with
--      @mdCredentials = []@ and @mdRequiresCredential = Nothing@.
--   7. A round-trip decode-then-encode of any schema produced by
--      AUTHZ-CRED-CORE-3's test fixtures yields JSON that decodes to an
--      identical 'MethodDef'.
--   10. A golden-file test bridging a fixture JSON from plexus-core's
--       schema producer to the expected 'MethodDef' value passes.
--
--   The golden-file fixtures below are byte-for-byte the JSON
--   'plexus-core/src/plexus/schema.rs' emits for the corresponding test
--   cases (verified by running the dump test in plexus-core; see the
--   AUTHZ-CRED-IR-1 run-notes).
module Main where

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.=), eitherDecode, encode, object, decode)
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Hspec

import Plexus.Schema.Recursive
  ( AttachmentSite(..)
  , CredentialFieldDecl(..)
  , CredentialIssuer(..)
  , CredentialKind(..)
  , CredentialMetadata(..)
  , MethodRole(..)
  , MethodSchema(..)
  , PluginSchema(..)
  , RequiredCredential(..)
  )
import Synapse.IR.Types
  ( MethodDef(..)
  , TypeRef(..)
  )
import Synapse.IR.Builder (extractMethodDef)

-- ============================================================================
-- Golden-file fixtures: byte-for-byte JSON from plexus-core's serde derives
-- ============================================================================

-- | AC #1, AC #5, AC #7: a MethodSchema whose return type carries a single
--   @Credential\<T\>@ field on a flat struct, plus a scope-derived
--   @requires_credential@ filter.
--
--   This is byte-for-byte the JSON 'plexus-core' emits via
--   @serde_json::to_string_pretty(&method)@ for a method built with:
--
--   @
--   MethodSchema::new("login", "logs in", "h_login")
--     .with_credentials(vec![CredentialFieldDecl::from_marker(
--       &CredentialFieldMarker::new(None, "session", Bearer, Header(authorization),
--                                    Some("Bearer "), [cone.send_message],
--                                    Some(auth.refresh), Some(auth.logout)),
--       &[], sample_issuer())])
--     .with_requires_credential(
--       RequiredCredential::from_method_scope(Scope::new("cone.send_message")))
--   @
fixtureSingleCredentialJson :: Value
fixtureSingleCredentialJson = [aesonQQ|
{
  "name": "login",
  "description": "logs in",
  "hash": "h_login",
  "streaming": false,
  "bidirectional": false,
  "http_method": "POST",
  "role": { "kind": "rpc" },
  "credentials": [
    {
      "field_path": ["session"],
      "metadata": {
        "kind": { "kind": "bearer" },
        "attach_as": { "site": "header", "name": "authorization" },
        "scheme": "Bearer ",
        "scopes": ["cone.send_message"],
        "expires_at": null,
        "refresh_via": "auth.refresh",
        "revoke_via": "auth.logout",
        "issuer": {
          "origin": "ws://localhost:4444",
          "method": "auth.login"
        },
        "sensitive": true
      }
    }
  ],
  "requires_credential": {
    "scopes": ["cone.send_message"]
  }
}
|]

-- | AC #2: a MethodSchema whose return type carries two credentials
--   (OAuth-style access + refresh). Field declaration order is preserved.
fixtureMultipleCredentialsJson :: Value
fixtureMultipleCredentialsJson = [aesonQQ|
{
  "name": "oauth_login",
  "description": "OAuth login",
  "hash": "h_oauth",
  "streaming": false,
  "bidirectional": false,
  "http_method": "POST",
  "role": { "kind": "rpc" },
  "credentials": [
    {
      "field_path": ["access"],
      "metadata": {
        "kind": { "kind": "oauth_access" },
        "attach_as": { "site": "header", "name": "authorization" },
        "scheme": "Bearer ",
        "scopes": ["cone.send"],
        "refresh_via": "auth.refresh",
        "issuer": {
          "origin": "ws://localhost:4444",
          "method": "auth.login"
        },
        "sensitive": true
      }
    },
    {
      "field_path": ["refresh"],
      "metadata": {
        "kind": { "kind": "oauth_refresh" },
        "attach_as": { "site": "header", "name": "authorization" },
        "scopes": ["auth.refresh"],
        "issuer": {
          "origin": "ws://localhost:4444",
          "method": "auth.login"
        },
        "sensitive": true
      }
    }
  ]
}
|]

-- | AC #3: enum-return-type schema with the credential pinned to a
--   single variant.
fixtureEnumVariantCredentialJson :: Value
fixtureEnumVariantCredentialJson = [aesonQQ|
{
  "name": "login",
  "description": "logs in",
  "hash": "h_login",
  "streaming": false,
  "bidirectional": false,
  "http_method": "POST",
  "role": { "kind": "rpc" },
  "credentials": [
    {
      "field_path": ["session"],
      "variant_tag": "Issued",
      "metadata": {
        "kind": { "kind": "bearer" },
        "attach_as": { "site": "header", "name": "authorization" },
        "scheme": "Bearer ",
        "scopes": ["cone.send_message"],
        "issuer": {
          "origin": "ws://localhost:4444",
          "method": "auth.login"
        },
        "sensitive": true
      }
    }
  ]
}
|]

-- | AC #4: a "public" method has no @requires_credential@ key on the wire.
fixturePublicMethodJson :: Value
fixturePublicMethodJson = [aesonQQ|
{
  "name": "ping",
  "description": "health check",
  "hash": "h_ping",
  "streaming": false,
  "bidirectional": false,
  "http_method": "POST",
  "role": { "kind": "rpc" }
}
|]

-- | AC #6: a pre-AUTHZ-CRED-CORE-3 method JSON with no @credentials@ or
--   @requires_credential@ keys. Same shape as legacy IR fixtures.
fixtureLegacyMethodJson :: Value
fixtureLegacyMethodJson = [aesonQQ|
{
  "name": "echo",
  "description": "echoes input",
  "hash": "h_echo"
}
|]

-- | AC #6: a pre-AUTHZ-CRED-CORE-3 PluginSchema with multiple methods, none
--   of which carry credential metadata. Verifies decode at the plugin level.
fixtureLegacyPluginJson :: Value
fixtureLegacyPluginJson = [aesonQQ|
{
  "namespace": "legacy",
  "version": "1.0.0",
  "description": "no credentials",
  "hash": "h1",
  "methods": [
    { "name": "alpha", "description": "first", "hash": "ah" },
    { "name": "beta",  "description": "second", "hash": "bh" }
  ]
}
|]

-- ============================================================================
-- Main test driver
-- ============================================================================

main :: IO ()
main = hspec $ do
  describe "AUTHZ-CRED-IR-1: MethodSchema credential decode" $ do

    it "AC #1: single Credential<T> field decodes with correct path and metadata" $ do
      case Aeson.fromJSON fixtureSingleCredentialJson :: Aeson.Result MethodSchema of
        Aeson.Error e   -> expectationFailure $ "decode failed: " <> e
        Aeson.Success m -> do
          length (methodCredentials m) `shouldBe` 1
          let [cfd] = methodCredentials m
          cfdFieldPath cfd  `shouldBe` ["session"]
          cfdVariantTag cfd `shouldBe` Nothing
          cmKind (cfdMetadata cfd) `shouldBe` CredKindBearer
          cmAttachAs (cfdMetadata cfd)
            `shouldBe` AttachHeader { attachHeaderName = "authorization" }
          cmScheme (cfdMetadata cfd) `shouldBe` Just "Bearer "
          cmScopes (cfdMetadata cfd) `shouldBe` ["cone.send_message"]
          cmRefreshVia (cfdMetadata cfd) `shouldBe` Just "auth.refresh"
          cmRevokeVia (cfdMetadata cfd)  `shouldBe` Just "auth.logout"
          credIssuerOrigin (cmIssuer (cfdMetadata cfd))
            `shouldBe` "ws://localhost:4444"
          credIssuerMethod (cmIssuer (cfdMetadata cfd))
            `shouldBe` "auth.login"
          cmSensitive (cfdMetadata cfd) `shouldBe` True

    it "AC #2: multiple credentials preserve declaration order" $ do
      case Aeson.fromJSON fixtureMultipleCredentialsJson :: Aeson.Result MethodSchema of
        Aeson.Error e   -> expectationFailure $ "decode failed: " <> e
        Aeson.Success m -> do
          length (methodCredentials m) `shouldBe` 2
          let [c1, c2] = methodCredentials m
          cfdFieldPath c1 `shouldBe` ["access"]
          cmKind (cfdMetadata c1) `shouldBe` CredKindOauthAccess
          cfdFieldPath c2 `shouldBe` ["refresh"]
          cmKind (cfdMetadata c2) `shouldBe` CredKindOauthRefresh

    it "AC #3: enum variant credential carries cfdVariantTag = Just" $ do
      case Aeson.fromJSON fixtureEnumVariantCredentialJson :: Aeson.Result MethodSchema of
        Aeson.Error e   -> expectationFailure $ "decode failed: " <> e
        Aeson.Success m -> do
          let [cfd] = methodCredentials m
          cfdVariantTag cfd `shouldBe` Just "Issued"
          cfdFieldPath cfd  `shouldBe` ["session"]

    it "AC #4: public method decodes with methodRequiresCredential = Nothing" $ do
      case Aeson.fromJSON fixturePublicMethodJson :: Aeson.Result MethodSchema of
        Aeson.Error e   -> expectationFailure $ "decode failed: " <> e
        Aeson.Success m -> do
          methodCredentials m        `shouldBe` []
          methodRequiresCredential m `shouldBe` Nothing

    it "AC #5: scoped method decodes with methodRequiresCredential populated" $ do
      case Aeson.fromJSON fixtureSingleCredentialJson :: Aeson.Result MethodSchema of
        Aeson.Error e   -> expectationFailure $ "decode failed: " <> e
        Aeson.Success m -> case methodRequiresCredential m of
          Nothing  -> expectationFailure "expected Just RequiredCredential"
          Just rc  -> do
            rcKind rc     `shouldBe` Nothing
            rcScopes rc   `shouldBe` ["cone.send_message"]
            rcSiteHint rc `shouldBe` Nothing

    it "AC #6: legacy MethodSchema (no credentials/requires_credential keys) decodes with empty defaults" $ do
      case Aeson.fromJSON fixtureLegacyMethodJson :: Aeson.Result MethodSchema of
        Aeson.Error e   -> expectationFailure $ "decode failed: " <> e
        Aeson.Success m -> do
          methodName m               `shouldBe` "echo"
          methodCredentials m        `shouldBe` []
          methodRequiresCredential m `shouldBe` Nothing

    it "AC #6 (plugin level): legacy PluginSchema decodes with empty defaults on every method" $ do
      case Aeson.fromJSON fixtureLegacyPluginJson :: Aeson.Result PluginSchema of
        Aeson.Error e   -> expectationFailure $ "decode failed: " <> e
        Aeson.Success p -> do
          length (psMethods p) `shouldBe` 2
          mapM_ (\m -> do
                   methodCredentials m        `shouldBe` []
                   methodRequiresCredential m `shouldBe` Nothing)
                (psMethods p)

    it "AC #7: round-trip MethodSchema with credentials populated" $ do
      case Aeson.fromJSON fixtureSingleCredentialJson :: Aeson.Result MethodSchema of
        Aeson.Error e        -> expectationFailure $ "decode failed: " <> e
        Aeson.Success origin -> do
          let reEncoded = encode origin
          case decode reEncoded :: Maybe MethodSchema of
            Nothing      -> expectationFailure "re-decode after encode failed"
            Just decoded -> decoded `shouldBe` origin

    it "AC #7 (variant): round-trip multiple credentials" $ do
      case Aeson.fromJSON fixtureMultipleCredentialsJson :: Aeson.Result MethodSchema of
        Aeson.Error e        -> expectationFailure $ "decode failed: " <> e
        Aeson.Success origin -> do
          let reEncoded = encode origin
          case decode reEncoded :: Maybe MethodSchema of
            Nothing      -> expectationFailure "re-decode after encode failed"
            Just decoded -> decoded `shouldBe` origin

    it "AC #7 (variant): round-trip enum variant credential" $ do
      case Aeson.fromJSON fixtureEnumVariantCredentialJson :: Aeson.Result MethodSchema of
        Aeson.Error e        -> expectationFailure $ "decode failed: " <> e
        Aeson.Success origin -> do
          let reEncoded = encode origin
          case decode reEncoded :: Maybe MethodSchema of
            Nothing      -> expectationFailure "re-decode after encode failed"
            Just decoded -> decoded `shouldBe` origin

  describe "AUTHZ-CRED-IR-1: MethodDef carries credentials through the IR builder" $ do

    it "extractMethodDef lifts methodCredentials onto mdCredentials" $ do
      case Aeson.fromJSON fixtureSingleCredentialJson :: Aeson.Result MethodSchema of
        Aeson.Error e   -> expectationFailure $ "decode failed: " <> e
        Aeson.Success m -> do
          let (_, mdef) = extractMethodDef "auth" "auth" m
          mdName mdef          `shouldBe` "login"
          length (mdCredentials mdef) `shouldBe` 1
          let [cfd] = mdCredentials mdef
          cfdFieldPath cfd  `shouldBe` ["session"]
          cmKind (cfdMetadata cfd) `shouldBe` CredKindBearer
          mdRequiresCredential mdef
            `shouldBe` Just RequiredCredential
                              { rcKind     = Nothing
                              , rcScopes   = ["cone.send_message"]
                              , rcSiteHint = Nothing
                              }

    it "extractMethodDef on a legacy method emits empty defaults" $ do
      case Aeson.fromJSON fixtureLegacyMethodJson :: Aeson.Result MethodSchema of
        Aeson.Error e   -> expectationFailure $ "decode failed: " <> e
        Aeson.Success m -> do
          let (_, mdef) = extractMethodDef "echo" "echo" m
          mdCredentials mdef        `shouldBe` []
          mdRequiresCredential mdef `shouldBe` Nothing

    it "AC #7 (IR-level): round-trip MethodDef through synapse's IR JSON" $ do
      case Aeson.fromJSON fixtureSingleCredentialJson :: Aeson.Result MethodSchema of
        Aeson.Error e   -> expectationFailure $ "decode failed: " <> e
        Aeson.Success m -> do
          let (_, mdef) = extractMethodDef "auth" "auth" m
              reEncoded = encode mdef
          case eitherDecode reEncoded :: Either String MethodDef of
            Left err      -> expectationFailure $ "re-decode failed: " <> err
            Right decoded -> do
              mdCredentials decoded        `shouldBe` mdCredentials mdef
              mdRequiresCredential decoded `shouldBe` mdRequiresCredential mdef
              mdName decoded               `shouldBe` mdName mdef

    it "pre-AUTHZ-CRED-IR-1 IR JSON (no mdCredentials/mdRequiresCredential keys) decodes with defaults" $ do
      -- This is the IR JSON shape produced by the pre-CRED-IR-1 ToJSON
      -- MethodDef instance (no mdCredentials, no mdRequiresCredential keys).
      let preIrJson = object
            [ "mdName"        .= ("ping" :: Text)
            , "mdFullPath"    .= ("util.ping" :: Text)
            , "mdNamespace"   .= ("util" :: Text)
            , "mdDescription" .= Aeson.Null
            , "mdStreaming"   .= False
            , "mdParams"      .= ([] :: [Value])
            , "mdReturns"     .= object [ "tag" .= ("RefAny" :: Text) ]
            ]
      case Aeson.fromJSON preIrJson :: Aeson.Result MethodDef of
        Aeson.Error e      -> expectationFailure $ "decode failed: " <> e
        Aeson.Success mdef -> do
          mdCredentials mdef        `shouldBe` []
          mdRequiresCredential mdef `shouldBe` Nothing

  describe "AUTHZ-CRED-IR-1: tagged enum wire shapes" $ do

    -- These tests guard against silent variant-tag drift between the Rust
    -- serde representation and the Haskell decoder. The producer side uses
    -- `#[serde(tag = "kind"/"site", rename_all = "snake_case")]`, so each
    -- variant has a specific JSON spelling.

    it "CredentialKind decodes every kind variant" $ do
      let j k = object [ "kind" .= (k :: Text) ]
          parse v = case Aeson.fromJSON v :: Aeson.Result CredentialKind of
            Aeson.Error e   -> error e
            Aeson.Success x -> x
      parse (j "bearer")        `shouldBe` CredKindBearer
      parse (j "cookie")        `shouldBe` CredKindCookie
      parse (j "oauth_access")  `shouldBe` CredKindOauthAccess
      parse (j "oauth_refresh") `shouldBe` CredKindOauthRefresh
      parse (j "oidc_id")       `shouldBe` CredKindOidcId
      parse (j "aws_sts")       `shouldBe` CredKindAwsSts
      parse (j "macaroon")      `shouldBe` CredKindMacaroon

    it "CredentialKind::Other decodes name field" $ do
      let j = object [ "kind" .= ("other" :: Text), "name" .= ("my_kind" :: Text) ]
      case Aeson.fromJSON j :: Aeson.Result CredentialKind of
        Aeson.Error e   -> expectationFailure e
        Aeson.Success x -> x `shouldBe` CredKindOther "my_kind"

    it "AttachmentSite decodes every variant" $ do
      let parse v = case Aeson.fromJSON v :: Aeson.Result AttachmentSite of
            Aeson.Error e   -> error e
            Aeson.Success x -> x
      parse (object [ "site" .= ("header" :: Text), "name" .= ("authorization" :: Text) ])
        `shouldBe` AttachHeader "authorization"
      parse (object [ "site" .= ("cookie" :: Text), "name" .= ("plexus_session" :: Text) ])
        `shouldBe` AttachCookie "plexus_session"
      parse (object [ "site" .= ("first_frame" :: Text)
                    , "setup_method" .= ("auth.setup" :: Text)
                    , "param" .= ("token" :: Text) ])
        `shouldBe` AttachFirstFrame "auth.setup" "token"
      parse (object [ "site" .= ("in_rpc_param" :: Text), "param" .= ("auth_token" :: Text) ])
        `shouldBe` AttachInRpcParam "auth_token"

    it "RequiredCredential serializes to compact wire shape (scopes only)" $ do
      let req = RequiredCredential
                  { rcKind     = Nothing
                  , rcScopes   = ["cone.send"]
                  , rcSiteHint = Nothing
                  }
      case Aeson.toJSON req of
        Object o -> do
          KM.lookup "scopes" o `shouldBe` Just (Aeson.toJSON ["cone.send" :: Text])
          -- kind and site_hint are omitted when Nothing.
          KM.lookup "kind"      o `shouldBe` Nothing
          KM.lookup "site_hint" o `shouldBe` Nothing
        v -> expectationFailure $ "expected object, got: " <> show v

    it "RequiredCredential round-trip preserves all fields" $ do
      let req = RequiredCredential
                  { rcKind     = Just CredKindOauthRefresh
                  , rcScopes   = ["auth.refresh"]
                  , rcSiteHint = Just (AttachHeader "authorization")
                  }
      case decode (encode req) :: Maybe RequiredCredential of
        Nothing      -> expectationFailure "re-decode failed"
        Just decoded -> decoded `shouldBe` req
