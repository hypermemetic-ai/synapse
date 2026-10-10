{-# LANGUAGE OverloadedStrings #-}

-- | IR-12 regression tests: 'MethodRole' round-trips through the
--   Plexus RPC wire format and into synapse's IR output.
--
--   Acceptance criteria covered:
--
--   * A JSON @MethodSchema@ with
--     @role: {"kind": "dynamic_child", "list_method": "planet_names",
--     "search_method": null}@ decodes to 'MethodRoleDynamicChild' with
--     the matching fields.
--
--   * A pre-IR @MethodSchema@ (no @role@ key) decodes to
--     'MethodRoleRpc' — the wire-level back-compat guarantee for
--     pre-IR servers.
--
--   * A synthesised 'MethodDef' whose 'mdRole' is 'MethodRoleDynamicChild'
--     round-trips through JSON and emits the exact @kind@/@list_method@
--     shape consumed by hub-codegen (whose Rust @MethodRole@ enum uses
--     @#[serde(tag = \"kind\", rename_all = \"snake_case\")]@).
module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson (Value(..), (.=), eitherDecode, encode, object)
import Test.Hspec

import Plexus.Schema.Recursive
  ( MethodRole(..)
  , MethodSchema(..)
  )

import qualified Data.Map.Strict as Map
import Synapse.IR.Builder (extractMethodDef)
import Synapse.IR.Types
  ( IR(..)
  , emptyIR
  , MethodDef(..)
  , TypeRef(..)
  )

-- | Baseline 'MethodDef' used by the round-trip tests.
baseMethodDef :: MethodDef
baseMethodDef = MethodDef
  { mdName                = "planet"
  , mdFullPath            = "solar.planet"
  , mdNamespace           = "solar"
  , mdDescription         = Just "Fetch a planet child by name."
  , mdStreaming           = False
  , mdParams              = []
  , mdReturns             = RefAny
  , mdBidirType           = Nothing
  , mdBidirResponseType   = Nothing
  , mdBidirResponseSchema = Nothing
  , mdRole                = MethodRoleRpc
  , mdGrant               = Nothing
  , mdTarget              = Nothing
  , mdLongDescription     = Nothing
  }

-- | Build a JSON @MethodSchema@ with an optional @role@ key. Passing
--   'Nothing' produces the pre-IR shape (no @role@ key at all).
methodSchemaJson :: Maybe Value -> Value
methodSchemaJson mRole =
  let base =
        [ "name"        .= ("planet" :: String)
        , "description" .= ("Fetch a planet child by name." :: String)
        , "hash"        .= ("h-planet" :: String)
        ]
  in object $ case mRole of
       Nothing   -> base
       Just role -> ("role" .= role) : base

-- | A hand-written pre-IR-12 MethodDef JSON blob: same field names as
--   the generic derivation produces, minus @mdRole@. Exercising the
--   back-compat path of synapse's manual @FromJSON MethodDef@ instance.
preIrMethodDefJson :: Value
preIrMethodDefJson = object
  [ "mdName"        .= ("planet" :: String)
  , "mdFullPath"    .= ("solar.planet" :: String)
  , "mdNamespace"   .= ("solar" :: String)
  , "mdDescription" .= Aeson.Null
  , "mdStreaming"   .= False
  , "mdParams"      .= ([] :: [Value])
  , "mdReturns"     .= object [ "tag" .= ("RefAny" :: String) ]
  ]

main :: IO ()
main = hspec $ do
  describe "IR-12: MethodRole wire round-trip via MethodSchema" $ do
    it "decodes DynamicChild with list_method and a null search_method" $ do
      let j = methodSchemaJson $ Just $ object
            [ "kind"          .= ("dynamic_child" :: String)
            , "list_method"   .= ("planet_names" :: String)
            , "search_method" .= Aeson.Null
            ]
      case Aeson.fromJSON j :: Aeson.Result MethodSchema of
        Aeson.Error e -> expectationFailure $ "decode failed: " <> e
        Aeson.Success ms ->
          methodRole ms `shouldBe`
            MethodRoleDynamicChild
              { listMethod   = Just "planet_names"
              , searchMethod = Nothing
              }

    it "decodes StaticChild" $ do
      let j = methodSchemaJson $ Just $ object
            [ "kind" .= ("static_child" :: String) ]
      case Aeson.fromJSON j :: Aeson.Result MethodSchema of
        Aeson.Error e -> expectationFailure $ "decode failed: " <> e
        Aeson.Success ms -> methodRole ms `shouldBe` MethodRoleStaticChild

    it "decodes explicit Rpc variant" $ do
      let j = methodSchemaJson $ Just $ object [ "kind" .= ("rpc" :: String) ]
      case Aeson.fromJSON j :: Aeson.Result MethodSchema of
        Aeson.Error e -> expectationFailure $ "decode failed: " <> e
        Aeson.Success ms -> methodRole ms `shouldBe` MethodRoleRpc

    it "defaults to Rpc when role is absent (pre-IR wire shape)" $ do
      case Aeson.fromJSON (methodSchemaJson Nothing) :: Aeson.Result MethodSchema of
        Aeson.Error e -> expectationFailure $ "decode failed: " <> e
        Aeson.Success ms -> methodRole ms `shouldBe` MethodRoleRpc

    it "serialises DynamicChild back to the exact Rust-compatible tag shape" $ do
      let role = MethodRoleDynamicChild
            { listMethod   = Just "planet_names"
            , searchMethod = Nothing
            }
          encoded = Aeson.toJSON role
      case encoded of
        Object o -> do
          KM.lookup "kind" o
            `shouldBe` Just (String "dynamic_child")
          KM.lookup "list_method" o
            `shouldBe` Just (String "planet_names")
          -- Matches the Rust #[serde(skip_serializing_if = "Option::is_none")]
          -- behaviour on search_method, so the encoded object never
          -- carries an explicit null for the absent field.
          KM.lookup "search_method" o `shouldBe` Nothing
        _ -> expectationFailure $
          "expected JSON Object, got: " <> show encoded

  describe "IR-12: MethodDef mdRole is emitted in synapse's IR JSON" $ do
    it "includes mdRole in the serialised IR method" $ do
      let md = baseMethodDef
            { mdRole = MethodRoleDynamicChild
                { listMethod   = Just "planet_names"
                , searchMethod = Nothing
                }
            }
          encoded = Aeson.toJSON md
      case encoded of
        Object o ->
          KM.lookup "mdRole" o `shouldBe` Just
            (object
              [ "kind"        .= ("dynamic_child" :: String)
              , "list_method" .= ("planet_names"  :: String)
              ])
        _ -> expectationFailure $
          "expected JSON Object, got: " <> show encoded

    it "round-trips DynamicChild through encode >>> decode" $ do
      let original = baseMethodDef
            { mdRole = MethodRoleDynamicChild
                { listMethod   = Just "planet_names"
                , searchMethod = Just "search_planets"
                }
            }
      case eitherDecode (encode original) :: Either String MethodDef of
        Left e -> expectationFailure $ "decode failed: " <> e
        Right md -> mdRole md `shouldBe` mdRole original

    it "pre-IR-12 MethodDef JSON (no mdRole) decodes with default Rpc" $ do
      -- Matches hub-codegen's Rust-side #[serde(default)] posture.
      case eitherDecode (encode preIrMethodDefJson) :: Either String MethodDef of
        Left e -> expectationFailure $ "decode failed: " <> e
        Right md -> mdRole md `shouldBe` MethodRoleRpc

  -- AUTH-9: what a method requires reaches hub-codegen through the IR.
  describe "AUTH-9: grant, target, long_description and roles in the IR" $ do
    let schemaWith extra = object $
          [ "name" .= ("set_status" :: String)
          , "description" .= ("Change a ticket's status" :: String)
          , "hash" .= ("h" :: String)
          ] <> extra
        lifted j = case Aeson.fromJSON j :: Aeson.Result MethodSchema of
          Aeson.Error e -> error e
          Aeson.Success ms -> snd (extractMethodDef "tickets" "tickets" ms)
        keysOf v = case v of
          Object o -> o
          _ -> KM.empty

    it "lifts the schema's grant, target and long_description onto the method" $ do
      let md = lifted $ schemaWith
            [ "grant" .= ("tickets.set_status" :: String)
            , "target" .= ("id" :: String)
            , "long_description" .= ("Moves it through Pending → Ready." :: String)
            ]
          o = keysOf (Aeson.toJSON md)
      (mdGrant md, mdTarget md) `shouldBe` (Just "tickets.set_status", Just "id")
      KM.lookup "mdGrant" o `shouldBe` Just (String "tickets.set_status")
      KM.lookup "mdTarget" o `shouldBe` Just (String "id")
      KM.lookup "mdLongDescription" o `shouldBe` Just (String "Moves it through Pending → Ready.")
      fmap mdTarget (eitherDecode (encode md)) `shouldBe` Right (Just "id")

    it "a pre-AUTH schema's method carries none of the keys" $ do
      let o = keysOf (Aeson.toJSON (lifted (schemaWith [])))
      map (`KM.member` o) ["mdGrant", "mdTarget", "mdLongDescription"] `shouldBe` [False, False, False]

    it "irPluginRoles is emitted only when some plugin has roles" $ do
      KM.member "irPluginRoles" (keysOf (Aeson.toJSON emptyIR)) `shouldBe` False
      let roles = Map.singleton "tickets" (Map.singleton "editor" ["tickets.set_status"])
          withRoles = emptyIR { irPluginRoles = Just roles }
      KM.lookup "irPluginRoles" (keysOf (Aeson.toJSON withRoles)) `shouldBe` Just (Aeson.toJSON roles)
      fmap irPluginRoles (eitherDecode (encode withRoles)) `shouldBe` Right (Just roles)
