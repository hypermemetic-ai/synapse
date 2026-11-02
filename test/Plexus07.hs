{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | RETURNS-36: the test fixtures captured from 0.6-shaped services (menger's
-- schemas of 2026-10-15 and 2026-10-29, the mock service's own), as plexus
-- 0.7 states them.
--
-- plexus 0.7 (RETURNS-1) states a method's cardinality where 0.6 had a
-- streaming flag, and synapse reads 0.7 only. The fixtures stay as they
-- were captured; 'asPlexus07' says each method's @cardinality@ —
-- @"many"@ where it said @streaming: true@, else @"one"@ — and drops the
-- flag. Nothing else moves: a method with no @returns@ stays without one
-- (and is no 0.7 schema).
module Plexus07
  ( asPlexus07
  , methodAs07
  , withContract
  , methodWithContract
  , renameParamsDef
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

-- | A plugin schema — or a fixture's rows (@[{"path", "schema"}]@) — with
--   every method as 0.7 states it, its children's too.
asPlexus07 :: Value -> Value
asPlexus07 = \case
  Object o -> Object (KM.mapWithKey key o)
  Array a -> Array (fmap asPlexus07 a)
  v -> v
  where
    key k v = case (k, v) of
      ("methods", Array ms) -> Array (fmap methodAs07 ms)
      ("schema", _) -> asPlexus07 v
      ("children", Array cs) -> Array (fmap asPlexus07 cs)
      _ -> v

-- | One method schema as 0.7 states it.
methodAs07 :: Value -> Value
methodAs07 = \case
  Object m | not (KM.member "cardinality" m) ->
    let card = if KM.lookup "streaming" m == Just (Bool True) then "many" else "one"
    in Object (KM.insert "cardinality" (String card) (KM.delete "streaming" m))
  Object m -> Object (KM.delete "streaming" m)
  v -> v

-- | 'asPlexus07', and each method that states no @returns@ (a fixture
--   written for a test of something else: auth's fields, help) given
--   @returns: {}@ — any JSON — as a 0.7 schema must state one.
withContract :: Value -> Value
withContract = \case
  Object o -> Object (KM.mapWithKey key o)
  Array a -> Array (fmap withContract a)
  v -> v
  where
    key k v = case (k, v) of
      ("methods", Array ms) -> Array (fmap methodWithContract ms)
      ("schema", _) -> withContract v
      ("children", Array cs) -> Array (fmap withContract cs)
      _ -> v

-- | One method as 'withContract' states it.
methodWithContract :: Value -> Value
methodWithContract v = case methodAs07 v of
  Object m | not (KM.member "returns" m) -> Object (KM.insert "returns" (Object KM.empty) m)
  m -> m

-- | In every method's params of a plugin schema, the @$defs@ entry @from@
--   renamed @to@, and every @$ref@ to it.
renameParamsDef :: Text -> Text -> Value -> Value
renameParamsDef from to = \case
  Object o -> Object (KM.mapWithKey (\k v -> if k == "methods" then onMethods v else v) o)
  v -> v
  where
    onMethods (Array ms) = Array (fmap onMethod ms)
    onMethods v = v
    onMethod (Object m) = Object (KM.mapWithKey (\k v -> if k == "params" then rename v else v) m)
    onMethod v = v
    fromRef = String ("#/$defs/" <> from)
    rename = \case
      Object o -> Object (KM.fromList [ (key k, rename v) | (k, v) <- KM.toList o ])
      Array a -> Array (fmap rename a)
      v | v == fromRef -> String ("#/$defs/" <> to)
      v -> v
    key k = if k == K.fromText from then K.fromText to else k
