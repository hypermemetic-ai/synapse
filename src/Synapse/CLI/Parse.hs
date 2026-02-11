{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | IR-driven parameter parsing for CLI
--
-- Transforms flat --key value pairs into properly nested JSON using IR type information.
--
-- = Problem
--
-- CLI flags are flat: @--identifier.type by_name --identifier.name foo@
-- Server expects nested: @{"identifier": {"type": "by_name", "name": "foo"}}@
--
-- = Solution
--
-- Use IR type definitions to understand the structure:
--
-- @
-- IR lookup: identifier -> RefNamed "ConeIdentifier"
-- IR lookup: ConeIdentifier -> KindEnum "type" [VariantDef "by_name" [...], ...]
-- Result: {"identifier": {"type": "by_name", "name": "foo"}}
-- @
--
-- = Design
--
-- Dotted keys are grouped by their first segment:
--
-- @
-- [("identifier.type", "by_name"), ("identifier.name", "foo"), ("prompt", "hi")]
-- -> Map.fromList [("identifier", [("type", "by_name"), ("name", "foo")]),
--                  ("prompt", [("", "hi")])]
-- @
--
-- Each group is then built according to its parameter's TypeRef.
--
-- = Arrays
--
-- Repeated flags are collected into arrays:
--
-- @
-- --tags backend --tags critical --tags urgent
-- -> [("tags", [("", "backend"), ("", "critical"), ("", "urgent")])]
-- -> {"tags": ["backend", "critical", "urgent"]}
-- @
module Synapse.CLI.Parse
  ( -- * Parsing
    parseParams
  , ParseError(..)

    -- * Helpers (exposed for testing)
  , groupByPrefix
  , buildParamValue
  ) where

import Data.Aeson (Value(..), object)
import qualified Data.Aeson.Key as K
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (Scientific)
import qualified Data.Vector as V
import Text.Read (readMaybe)

import Synapse.IR.Types
import Synapse.CLI.Similarity (suggestCorrections)

-- ============================================================================
-- Error Types
-- ============================================================================

-- | Errors that can occur during parameter parsing
data ParseError
  = UnknownParam Text [Text]
      -- ^ Flag doesn't match any known parameter (param name, suggestions)
  | MissingRequired Text
      -- ^ Required parameter was not provided
  | InvalidValue Text Text
      -- ^ Invalid value for parameter (param name, reason)
  | AmbiguousVariant Text [Text]
      -- ^ Multiple variants could match (discriminator, variant names)
  | MissingDiscriminator Text Text
      -- ^ Enum type missing discriminator value (param name, discriminator field)
  | UnknownVariant Text Text [Text] [Text]
      -- ^ Unknown variant name (param name, provided value, valid variants, suggestions)
  | TypeNotFound Text
      -- ^ Referenced type not found in IR
  deriving (Show, Eq)

-- ============================================================================
-- Main Parsing Function
-- ============================================================================

-- | Parse flat key-value pairs into nested JSON using IR structure
--
-- Example:
-- @
-- parseParams ir method [("identifier.type", "by_name"), ("identifier.name", "foo"), ("prompt", "hi")]
-- = Right (object [("identifier", object [("type", "by_name"), ("name", "foo")]),
--                  ("prompt", "hi")])
-- @
parseParams :: IR -> MethodDef -> [(Text, Text)] -> Either [ParseError] Value
parseParams ir method kvPairs = do
  -- Group keys by their top-level prefix
  let grouped = groupByPrefix kvPairs

  -- Build each parameter value
  let paramDefs = mdParams method
  let paramMap = Map.fromList [(pdName p, p) | p <- paramDefs]

  -- Process each group and collect results/errors
  let results = map (processGroup ir paramMap) (Map.toList grouped)

  -- Check for missing required params
  let providedParams = Map.keys grouped
  let missingRequired =
        [ MissingRequired (pdName p)
        | p <- paramDefs
        , pdRequired p
        , pdName p `notElem` providedParams
        , pdDefault p == Nothing  -- Has no default value
        ]

  -- Collect all errors and results
  let (errors, values) = partitionResults results
  let allErrors = errors ++ missingRequired

  if null allErrors
    then Right $ object values
    else Left allErrors

-- | Process a single parameter group
processGroup :: IR -> Map Text ParamDef -> (Text, [(Text, Text)]) -> Either ParseError (K.Key, Value)
processGroup ir paramMap (paramName, subKeys) =
  case Map.lookup paramName paramMap of
    Nothing ->
      let validParams = Map.keys paramMap
          suggestions = suggestCorrections paramName validParams
      in Left $ UnknownParam paramName suggestions
    Just paramDef -> do
      val <- buildParamValue ir paramDef subKeys
      Right (K.fromText paramName, val)

-- | Partition results into errors and successes
partitionResults :: [Either ParseError (K.Key, Value)] -> ([ParseError], [(K.Key, Value)])
partitionResults = foldr partition ([], [])
  where
    partition (Left e) (errs, vals) = (e : errs, vals)
    partition (Right v) (errs, vals) = (errs, v : vals)

-- ============================================================================
-- Key Grouping
-- ============================================================================

-- | Group dotted keys by their first segment
--
-- Keys without dots get empty suffix:
-- @
-- [("identifier.type", "by_name"), ("identifier.name", "foo"), ("prompt", "hi")]
-- -> Map.fromList [("identifier", [("type", "by_name"), ("name", "foo")]),
--                  ("prompt", [("", "hi")])]
-- @
groupByPrefix :: [(Text, Text)] -> Map Text [(Text, Text)]
groupByPrefix = foldr addPair Map.empty
  where
    addPair (key, val) acc =
      let (prefix, suffix) = splitKey key
      in Map.insertWith (++) prefix [(suffix, val)] acc

    splitKey key = case T.breakOn "." key of
      (pre, rest)
        | T.null rest -> (pre, "")  -- No dot, empty suffix
        | otherwise -> (pre, T.drop 1 rest)  -- Drop the dot

-- ============================================================================
-- Value Building
-- ============================================================================

-- | Build value for a single param using its TypeRef
buildParamValue :: IR -> ParamDef -> [(Text, Text)] -> Either ParseError Value
buildParamValue ir param kvs = case pdType param of
  RefPrimitive typ mFmt ->
    -- Simple value, expect single kv with empty suffix
    case lookup "" kvs of
      Just val -> Right $ inferTypedValue typ mFmt val
      Nothing -> Left $ InvalidValue (pdName param) "expected simple value"

  RefNamed qn ->
    let typeName = qualifiedNameFull qn
    in case Map.lookup typeName (irTypes ir) of
      Just typeDef -> buildFromTypeDef ir (pdName param) typeDef kvs
      Nothing ->
        -- Type not in IR, treat as simple value
        case lookup "" kvs of
          Just val -> Right $ String val
          Nothing -> Left $ TypeNotFound typeName

  RefOptional inner ->
    -- For optional, if we have values, parse the inner type
    if null kvs
      then Right Null
      else buildParamValue ir param{pdType = inner} kvs

  RefArray innerType ->
    -- Collect all repeated --key value entries
    let values = [val | ("", val) <- kvs]
    in if null values
       then Left $ InvalidValue (pdName param) "array requires at least one value"
       else Right $ Array $ V.fromList $ map (buildTypedValue ir innerType) values

  RefAny ->
    -- Dynamic type, try to parse as JSON or use as string
    case lookup "" kvs of
      Just val -> Right $ inferValue val
      Nothing -> Left $ InvalidValue (pdName param) "dynamic type needs value"

  RefUnknown ->
    -- Unknown type, best effort
    case lookup "" kvs of
      Just val -> Right $ inferValue val
      Nothing -> Left $ InvalidValue (pdName param) "unknown type needs value"

-- | Build value from a TypeDef
buildFromTypeDef :: IR -> Text -> TypeDef -> [(Text, Text)] -> Either ParseError Value
buildFromTypeDef ir paramName TypeDef{..} kvs = case tdKind of
  KindEnum discriminator variants ->
    -- Find which variant based on discriminator value
    case lookup discriminator kvs of
      Just variantName ->
        case find (\v -> vdName v == variantName) variants of
          Just variant -> buildVariant ir paramName discriminator variant kvs
          Nothing ->
            let validVariants = map vdName variants
                suggestions = suggestCorrections variantName validVariants
            in Left $ UnknownVariant paramName variantName validVariants suggestions
      Nothing -> Left $ MissingDiscriminator paramName discriminator

  KindStruct fields ->
    -- Build object from fields
    buildStruct ir paramName fields kvs

  KindAlias target ->
    -- Follow the alias
    buildParamValue ir ParamDef{pdName = paramName, pdType = target, pdDescription = Nothing, pdRequired = True, pdDefault = Nothing} kvs

  KindStringEnum allowedValues ->
    -- Expect single string value from allowed set
    case lookup "" kvs of
      Just val
        | val `elem` allowedValues -> Right $ String val
        | otherwise -> Left $ InvalidValue paramName $
            "must be one of: " <> T.intercalate ", " allowedValues
      Nothing -> Left $ InvalidValue paramName "expected enum value"

  KindPrimitive typ mFmt ->
    -- Expect single value
    case lookup "" kvs of
      Just val -> Right $ inferTypedValue typ mFmt val
      Nothing -> Left $ InvalidValue paramName "expected primitive value"

-- | Build a variant value (discriminator + variant fields)
buildVariant :: IR -> Text -> Text -> VariantDef -> [(Text, Text)] -> Either ParseError Value
buildVariant ir paramName discriminator VariantDef{..} kvs =
  let discPair = (K.fromText discriminator, String vdName)
  in case buildFieldPairs ir paramName vdFields kvs of
       Right fieldPairs -> Right $ object (discPair : fieldPairs)
       Left err -> Left err

-- | Build field pairs from a list of field definitions
buildFieldPairs :: IR -> Text -> [FieldDef] -> [(Text, Text)] -> Either ParseError [(K.Key, Value)]
buildFieldPairs ir paramName fields kvs = do
  let results = map (buildFieldPair ir paramName kvs) fields
  let (errors, pairs) = partitionResults results
  if null errors
    then Right pairs
    else Left (head errors)  -- Return first error

-- | Build a single field pair
buildFieldPair :: IR -> Text -> [(Text, Text)] -> FieldDef -> Either ParseError (K.Key, Value)
buildFieldPair ir paramName kvs FieldDef{..} =
  case lookup fdName kvs of
    Just val -> Right (K.fromText fdName, buildTypedValue ir fdType val)
    Nothing
      | fdRequired -> Left $ MissingRequired (paramName <> "." <> fdName)
      | Just def <- fdDefault -> Right (K.fromText fdName, def)
      | otherwise -> Right (K.fromText fdName, Null)

-- | Build a struct value from fields
buildStruct :: IR -> Text -> [FieldDef] -> [(Text, Text)] -> Either ParseError Value
buildStruct ir paramName fields kvs = do
  pairs <- buildFieldPairs ir paramName fields kvs
  Right $ object pairs

-- | Build a typed value from a TypeRef
buildTypedValue :: IR -> TypeRef -> Text -> Value
buildTypedValue ir ref val = case ref of
  RefPrimitive typ mFmt -> inferTypedValue typ mFmt val
  RefNamed qn ->
    let typeName = qualifiedNameFull qn
    in case Map.lookup typeName (irTypes ir) of
      Just TypeDef{tdKind = KindPrimitive typ mFmt} -> inferTypedValue typ mFmt val
      _ -> String val
  RefOptional inner -> buildTypedValue ir inner val
  RefArray _ -> inferValue val  -- Best effort for arrays
  RefAny -> inferValue val
  RefUnknown -> String val

-- ============================================================================
-- Value Inference
-- ============================================================================

-- | Infer JSON value from string using type hints
inferTypedValue :: Text -> Maybe Text -> Text -> Value
inferTypedValue typ mFmt val = case typ of
  "string" -> String val
  "integer" -> case readMaybe (T.unpack val) :: Maybe Integer of
    Just n -> Number (fromInteger n)
    Nothing -> String val
  "number" -> case readMaybe (T.unpack val) :: Maybe Double of
    Just n -> Number (realToFrac n)
    Nothing -> String val
  "boolean" -> case T.toLower val of
    "true" -> Bool True
    "false" -> Bool False
    "1" -> Bool True
    "0" -> Bool False
    _ -> String val
  _ -> String val  -- Default to string for unknown types

-- | Infer JSON value from string without type hints
inferValue :: Text -> Value
inferValue t
  | t == "true" = Bool True
  | t == "false" = Bool False
  | t == "null" = Null
  | Just n <- readMaybe (T.unpack t) :: Maybe Integer = Number (fromInteger n)
  | Just n <- readMaybe (T.unpack t) :: Maybe Double = Number (realToFrac n)
  | otherwise = String t

-- | Parse a string as JSON
parseJsonValue :: Text -> Either ParseError Value
parseJsonValue t =
  -- Try to parse as JSON, fall back to string
  Right $ inferValue t
