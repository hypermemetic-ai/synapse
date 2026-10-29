{-# LANGUAGE OverloadedStrings #-}

-- | Example value generation for CLI templates
--
-- Generates realistic example values for different TypeRefs
-- to populate CLI command templates.
module Synapse.Self.Examples
  ( generateExampleValue
  , generateExampleParam
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Synapse.IR.Types

-- | Generate a realistic example value for a TypeRef
-- Returns the value in a format suitable for CLI usage
generateExampleValue :: IR -> TypeRef -> Text
generateExampleValue ir typeRef = case typeRef of
  -- Primitives with format hints
  RefPrimitive "string" (Just "uuid") ->
    "550e8400-e29b-41d4-a716-446655440000"
  RefPrimitive "string" (Just "date-time") ->
    "2026-01-21T10:30:00Z"
  RefPrimitive "string" _ ->
    "example"
  RefPrimitive "integer" _ -> "42"
  RefPrimitive "number" _ -> "3.14"
  RefPrimitive "boolean" _ -> "true"

  -- Named types - look up in IR
  RefNamed qn -> case lookupTypeDef ir qn of
    Just typeDef -> generateNamedTypeExample ir typeDef
    Nothing -> "{}"  -- Unknown type

  -- Collections and modifiers
  RefArray inner ->
    "'[" <> generateExampleValue ir inner <> "]'"
  RefOptional inner ->
    generateExampleValue ir inner
  RefJson -> "{}"
  RefUnknown -> "<value>"
  RefMap _ -> "{}"
  RefTuple ts -> "'[" <> T.intercalate "," (map (generateExampleValue ir) ts) <> "]'"
  RefRefined inner _ -> generateExampleValue ir inner
  RefChild _ -> "<value>"

-- | Generate example for a named type definition
generateNamedTypeExample :: IR -> TypeDef -> Text
generateNamedTypeExample ir typeDef = case tdKind typeDef of
  -- String enums - show first value
  KindStringEnum values -> case values of
    (v:_) -> "\"" <> v <> "\""
    [] -> "\"value\""

  -- Discriminated unions - show first variant as JSON
  KindEnum disc variants _ -> case variants of
    (v:_) -> formatVariantExample ir disc v
    [] -> "{}"

  -- Structs - show empty object with hint
  KindStruct _ -> "{}"

  -- Aliases - resolve to target
  KindAlias target -> generateExampleValue ir target

  -- Primitives
  KindPrimitive ptype fmt -> generateExampleValue ir (RefPrimitive ptype fmt)

-- | Format a discriminated union variant as JSON
formatVariantExample :: IR -> Text -> VariantDef -> Text
formatVariantExample ir disc variant =
  let typeField = "\\\"" <> disc <> "\\\":\\\"" <> vdName variant <> "\\\""
      fieldExamples = map (formatFieldExample ir) (vdFields variant)
      allFields = typeField : fieldExamples
  in "'{" <> T.intercalate "," allFields <> "}'"

-- | Format a field as a JSON key-value pair
formatFieldExample :: IR -> FieldDef -> Text
formatFieldExample ir field =
  "\\\"" <> fdName field <> "\\\":" <> simpleExample (fdType field)
  where
    simpleExample :: TypeRef -> Text
    simpleExample = \case
      RefPrimitive "string" _ -> "\\\"value\\\""
      RefPrimitive "integer" _ -> "42"
      RefPrimitive "boolean" _ -> "true"
      RefPrimitive "number" _ -> "3.14"
      RefNamed qn -> case lookupTypeDef ir qn of
        Just td -> case tdKind td of
          KindStringEnum (v:_) -> "\\\"" <> v <> "\\\""
          _ -> "{}"
        Nothing -> "{}"
      _ -> "{}"

-- | Generate a CLI parameter flag with example value
generateExampleParam :: IR -> ParamDef -> Text
generateExampleParam ir param =
  "--" <> pdName param <> " " <> generateExampleValue ir (pdType param)

-- | Look up a type definition in the IR by qualified name
lookupTypeDef :: IR -> QualifiedName -> Maybe TypeDef
lookupTypeDef ir qn =
  Map.lookup (qualifiedNameFull qn) (irTypes ir)
