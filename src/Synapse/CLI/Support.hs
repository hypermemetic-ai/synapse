{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | CLI representability checks for IR types
--
-- Determines whether types from the IR can be represented as CLI flags.
-- This enables:
--
-- 1. Showing appropriate help (simple flags vs JSON input required)
-- 2. Tab completion for types that can be enumerated
-- 3. Validation before attempting CLI invocation
--
-- = Representability Rules
--
-- A type is CLI-representable if it can be expressed as --flag value pairs:
--
-- - Primitives: string, integer, number, boolean (always representable)
-- - UUID/formatted strings: representable as strings
-- - Optional T: representable if T is representable (flag is omittable)
-- - Arrays: representable via repeated flags (--key val1 --key val2 --key val3)
-- - Structs: representable if all required fields are representable
-- - Enums (discriminated): representable via --field.type variant --field.x ...
-- - String enums: representable (finite set of string values)
-- - Any/Unknown: NOT representable (need JSON input)
--
-- = Example
--
-- @
-- data SupportLevel
--   = FullSupport        -- All params work as CLI flags
--   | PartialSupport     -- Some params need JSON
--   | NoSupport          -- Method requires JSON input
-- @
module Synapse.CLI.Support
  ( -- * Support Levels
    SupportLevel(..)
  , SupportReason(..)

    -- * Method Support
  , methodSupport
  , methodSupportDetails

    -- * Type Checks
  , canCLIRepresent
  , canCLIRepresentVariant
  , canCLIRepresentField

    -- * Utilities
  , unsupportedParams
  , requiredJsonParams
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Synapse.IR.Types

-- ============================================================================
-- Support Levels
-- ============================================================================

-- | Level of CLI support for a method
data SupportLevel
  = FullSupport
    -- ^ All parameters can be expressed as CLI flags
  | PartialSupport [Text]
    -- ^ Some parameters require JSON input (listed by name)
  | NoSupport [Text]
    -- ^ Method requires JSON input for critical parameters
  deriving stock (Show, Eq)

-- | Reason why a type is not CLI representable
data SupportReason
  = ReasonArray
    -- ^ Arrays need JSON or special handling
  | ReasonNested
    -- ^ Deeply nested structure
  | ReasonAny
    -- ^ Type is dynamic (any)
  | ReasonUnknown
    -- ^ Type information missing
  | ReasonComplexUnion
    -- ^ Union with non-representable variants
  deriving stock (Show, Eq)

-- ============================================================================
-- Method Support
-- ============================================================================

-- | Check the CLI support level for a method
methodSupport :: IR -> MethodDef -> SupportLevel
methodSupport ir method =
  let unsupported = unsupportedParams ir method
      required = filter (isRequired method) unsupported
  in case (null unsupported, null required) of
    (True, _)     -> FullSupport
    (False, True) -> PartialSupport unsupported
    (False, False) -> NoSupport required
  where
    isRequired m name = any (\p -> pdName p == name && pdRequired p) (mdParams m)

-- | Get detailed support information for a method
methodSupportDetails :: IR -> MethodDef -> [(Text, Either SupportReason ())]
methodSupportDetails ir method =
  [ (pdName p, checkParam p)
  | p <- mdParams method
  ]
  where
    checkParam p = case canCLIRepresentWithReason ir (pdType p) of
      Left reason -> Left reason
      Right () -> Right ()

-- ============================================================================
-- Type Representability
-- ============================================================================

-- | Check if a type can be represented as CLI flags
canCLIRepresent :: IR -> TypeRef -> Bool
canCLIRepresent ir typeRef = case canCLIRepresentWithReason ir typeRef of
  Right () -> True
  Left _ -> False

-- | Check representability with reason
canCLIRepresentWithReason :: IR -> TypeRef -> Either SupportReason ()
canCLIRepresentWithReason ir = \case
  -- Primitives are always representable
  RefPrimitive _ _ -> Right ()

  -- Optional types: representable if inner type is
  RefOptional inner -> canCLIRepresentWithReason ir inner

  -- Arrays are representable via repeated flags if the element type is representable
  RefArray innerType -> canCLIRepresentWithReason ir innerType

  -- Any/Unknown require JSON
  RefJson -> Left ReasonAny
  RefUnknown -> Left ReasonUnknown
  RefMap _ -> Left ReasonAny
  RefTuple _ -> Left ReasonNested
  RefRefined inner _ -> canCLIRepresentWithReason ir inner
  RefChild _ -> Left ReasonUnknown

  -- Named types: look up and check
  RefNamed qn -> canCLIRepresentNamed ir (qualifiedNameFull qn)

-- | Check if a named type is CLI representable
canCLIRepresentNamed :: IR -> Text -> Either SupportReason ()
canCLIRepresentNamed ir name =
  case Map.lookup name (irTypes ir) of
    Nothing -> Left ReasonUnknown
    Just typeDef -> canCLIRepresentTypeDef ir typeDef

-- | Check if a type definition is CLI representable
canCLIRepresentTypeDef :: IR -> TypeDef -> Either SupportReason ()
canCLIRepresentTypeDef ir TypeDef{..} = case tdKind of
  -- Primitives are representable
  KindPrimitive _ _ -> Right ()

  -- Alias: check the target
  KindAlias target -> canCLIRepresentWithReason ir target

  -- Structs: all fields must be representable
  KindStruct fields ->
    let checks = map (canCLIRepresentField ir) fields
    in case filter isLeftE checks of
      [] -> Right ()
      _ -> Left ReasonNested

  -- Enums: check based on discriminator type
  -- SYNAPSE-CC-8: an external or untagged union is read whole now; it was
  -- an empty enum before, and representable as one
  KindEnum _ _ repr | repr /= Internal -> Right ()
  KindEnum discriminator variants _
    -- String enums (discriminator is "value") are representable
    | discriminator == "value" -> Right ()
    -- Discriminated unions: all variants must be representable
    | otherwise ->
        let checks = map (canCLIRepresentVariantWithReason ir) variants
        in case filter isLeftE checks of
          [] -> Right ()
          _ -> Left ReasonComplexUnion

  -- String enums are representable (simple string literals)
  KindStringEnum _ -> Right ()

isLeftE :: Either a b -> Bool
isLeftE (Left _) = True
isLeftE (Right _) = False

-- | Check if a variant is CLI representable
canCLIRepresentVariant :: IR -> VariantDef -> Bool
canCLIRepresentVariant ir VariantDef{..} =
  all (canCLIRepresentField' ir) vdFields

-- | Check if a variant is representable (with reason)
canCLIRepresentVariantWithReason :: IR -> VariantDef -> Either SupportReason ()
canCLIRepresentVariantWithReason ir VariantDef{..} =
  let checks = map (canCLIRepresentField ir) vdFields
  in case filter isLeftE checks of
    [] -> Right ()
    _ -> Left ReasonNested

-- | Check if a field is CLI representable
canCLIRepresentField :: IR -> FieldDef -> Either SupportReason ()
canCLIRepresentField ir FieldDef{..} =
  canCLIRepresentWithReason ir fdType

-- | Check if a field is representable (Bool version)
canCLIRepresentField' :: IR -> FieldDef -> Bool
canCLIRepresentField' ir field = case canCLIRepresentField ir field of
  Right () -> True
  Left _ -> False

-- ============================================================================
-- Utilities
-- ============================================================================

-- | Get list of parameters that cannot be represented as CLI flags
unsupportedParams :: IR -> MethodDef -> [Text]
unsupportedParams ir method =
  [ pdName p
  | p <- mdParams method
  , not (canCLIRepresent ir (pdType p))
  ]

-- | Get parameters that require JSON input (non-representable + required)
requiredJsonParams :: IR -> MethodDef -> [Text]
requiredJsonParams ir method =
  [ pdName p
  | p <- mdParams method
  , pdRequired p
  , not (canCLIRepresent ir (pdType p))
  ]

-- ============================================================================
-- Depth Checking (for recursive/deeply nested types)
-- ============================================================================

-- | Maximum nesting depth for CLI representability
maxNestingDepth :: Int
maxNestingDepth = 3

-- | Check if a type is representable within a depth limit
canCLIRepresentWithDepth :: IR -> Int -> TypeRef -> Bool
canCLIRepresentWithDepth _ depth _
  | depth <= 0 = False
canCLIRepresentWithDepth ir depth typeRef = case typeRef of
  RefPrimitive _ _ -> True
  RefOptional inner -> canCLIRepresentWithDepth ir depth inner
  RefArray innerType -> canCLIRepresentWithDepth ir depth innerType
  RefJson -> False
  RefUnknown -> False
  RefMap _ -> False
  RefTuple _ -> False
  RefRefined inner _ -> canCLIRepresentWithDepth ir depth inner
  RefChild _ -> False
  RefNamed qn -> canCLIRepresentNamedWithDepth ir (depth - 1) (qualifiedNameFull qn)

-- | Check named type with depth
canCLIRepresentNamedWithDepth :: IR -> Int -> Text -> Bool
canCLIRepresentNamedWithDepth ir depth name
  | depth <= 0 = False
  | otherwise = case Map.lookup name (irTypes ir) of
      Nothing -> False
      Just TypeDef{..} -> case tdKind of
        KindPrimitive _ _ -> True
        KindAlias target -> canCLIRepresentWithDepth ir depth target
        KindStruct fields -> all (canCLIRepresentFieldWithDepth ir (depth - 1)) fields
        KindEnum "value" _ _ -> True  -- String enum
        KindEnum _ _ repr | repr /= Internal -> True
        KindEnum _ variants _ -> all (canCLIRepresentVariantWithDepth ir (depth - 1)) variants

-- | Check field with depth
canCLIRepresentFieldWithDepth :: IR -> Int -> FieldDef -> Bool
canCLIRepresentFieldWithDepth ir depth FieldDef{..} =
  canCLIRepresentWithDepth ir depth fdType

-- | Check variant with depth
canCLIRepresentVariantWithDepth :: IR -> Int -> VariantDef -> Bool
canCLIRepresentVariantWithDepth ir depth VariantDef{..} =
  all (canCLIRepresentFieldWithDepth ir depth) vdFields
