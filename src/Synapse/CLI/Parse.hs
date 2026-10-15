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
--
-- For a list of scalars, each flag value is also a comma-separated record
-- (see 'splitListValue'):
--
-- @
-- --tags backend,critical --tags '"a,b"'
-- -> {"tags": ["backend", "critical", "a,b"]}
-- @
--
-- = Command line
--
-- The pairs come from the command line in two passes. 'parsePathAndParams'
-- runs before the method is known and finds the path; 'pairFlags' runs once
-- it is, and uses each parameter's type to decide whether a flag takes the
-- argument after it, so a value may begin with dashes:
--
-- @
-- --summary --body "--- id: X" --limit=3      (summary a boolean)
-- -> [("summary", "true"), ("body", "--- id: X"), ("limit", "3")]
-- @
module Synapse.CLI.Parse
  ( -- * Command line
    parsePathAndParams
  , pairFlags

    -- * Parsing
  , parseParams
  , ParseError(..)

    -- * List values
  , isScalarElement
  , splitListValue

    -- * Helpers (exposed for testing)
  , groupByPrefix
  , buildParamValue
  ) where

import Control.Monad (foldM)
import Data.Aeson (Value(..), object)
import qualified Data.Aeson.Key as K
import Data.Either (partitionEithers)
import Data.List (find)
import Data.Maybe (fromMaybe)
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
  | MalformedListValue Text Text Text
      -- ^ List flag value with broken quoting (param name, flag value, reason)
  | UnexpectedArgument Text
      -- ^ Argument that no flag takes as its value
  deriving (Show, Eq)

-- ============================================================================
-- Command Line
-- ============================================================================

-- | Whether a flag written without @=@ takes the argument after it
data FlagArity
  = TakesNext        -- ^ Always, whatever it begins with: the parameter takes a value
  | TakesBoolean     -- ^ Only when it reads as a boolean: the parameter is a boolean
  | TakesUnlessFlag  -- ^ Unless it begins with @--@ (and is not @--@): no parameter is known

-- | Split the arguments after the backend into path segments, (key, value)
-- pairs, the flag arguments the pairs came from, and whether help was asked
-- for.
--
-- This runs before the method is known, so a flag takes the next argument
-- unless that begins with @--@ and is not exactly @--@. That is enough to find
-- the path. Once the method is known, 'pairFlags' pairs the flag arguments
-- again by type; the pairs returned here serve only where there is no method
-- schema (@_self@, @respond@, a method missing from the IR).
--
-- Example: ["echo", "once", "--message", "hello", "--count", "3"]
--       -> (["echo", "once"], [("message", "hello"), ("count", "3")],
--           ["--message", "hello", "--count", "3"], False)
-- Example: ["echo", "once", "--help"]
--       -> (["echo", "once"], [], [], True)
parsePathAndParams :: [Text] -> ([Text], [(Text, Text)], [Text], Bool)
parsePathAndParams = go [] [] [] False
  where
    go path params flags helpReq [] = (reverse path, reverse params, reverse flags, helpReq)
    go path params flags helpReq (x:xs)
      -- --help flag: mark help requested, don't add as param
      | x == "--help" || x == "-h" =
          go path params flags True xs
      -- --key value, --key=value, or a bare --key
      | Just flag <- T.stripPrefix "--" x
      , not (T.null flag) =
          let (key, val, n) = takeFlag (const TakesUnlessFlag) flag xs
              (taken, rest) = splitAt n xs
          in go path ((normalizeKey key, fromMaybe "" val) : params)
                (reverse (x : taken) ++ flags) helpReq rest
      -- Regular path segment - split on dots to support Plexus RPC path syntax (e.g., cone.chat)
      | otherwise =
          let segments = map (T.replace "-" "_") $ filter (not . T.null) $ T.splitOn "." x
          in go (reverse segments ++ path) params flags helpReq xs

-- | Pair a method's flag arguments (from 'parsePathAndParams') by the types
-- of its parameters:
--
-- * @--key=value@: everything after the first @=@, empty or not
-- * a boolean: @true@ when bare; it takes the next argument only when that
--   reads as a boolean (@true@, @false@, @1@, @0@), so @--summary --limit 3@
--   is two flags
-- * any other parameter: the next argument, whatever it begins with
--   (@---@, @--force@, @-5@, @--@)
-- * a flag that names no parameter: as in 'parsePathAndParams'; its key
--   stays as typed, so 'UnknownParam' names it so
--
-- Keys are normalized (kebab-case to snake_case); values never are. An
-- argument that no flag takes is an 'UnexpectedArgument'.
pairFlags :: IR -> MethodDef -> [Text] -> Either [ParseError] [(Text, Text)]
pairFlags ir method args = case partitionEithers (go args) of
  ([], pairs) -> Right pairs
  (errs, _) -> Left errs
  where
    go [] = []
    go (x:xs) = case T.stripPrefix "--" x of
      Just flag | not (T.null flag) ->
        let (typed, val, n) = takeFlag arity flag xs
            key = normalizeKey typed
            pair = case arity key of
              TakesNext -> (key, fromMaybe "" val)
              TakesBoolean -> (key, fromMaybe "true" val)
              TakesUnlessFlag -> (typed, fromMaybe "" val)
        in Right pair : go (drop n xs)
      _ -> Left (UnexpectedArgument x) : go xs

    arity = flagArity ir method

-- | Read one flag (the text after @--@) against the arguments after it:
-- its key as typed, its value ('Nothing' when bare), and how many of those
-- arguments it took (0 or 1).
takeFlag :: (Text -> FlagArity) -> Text -> [Text] -> (Text, Maybe Text, Int)
takeFlag arity flag next
  | (key, eqValue) <- T.breakOn "=" flag, not (T.null eqValue) =
      (key, Just (T.drop 1 eqValue), 0)
  | otherwise = case (arity (normalizeKey flag), next) of
      (TakesNext, val : _) -> (flag, Just val, 1)
      (TakesBoolean, val : _) | isBooleanWord val -> (flag, Just val, 1)
      (TakesUnlessFlag, val : _) | val == "--" || not ("--" `T.isPrefixOf` val) -> (flag, Just val, 1)
      _ -> (flag, Nothing, 0)
  where
    -- The spellings 'inferTypedValue' reads as a boolean
    isBooleanWord val = T.toLower val `elem` ["true", "false", "1", "0"]

-- | How the flag with this (normalized) key takes its value: by the type of
-- the parameter it names, or of the field a dotted key names
flagArity :: IR -> MethodDef -> Text -> FlagArity
flagArity ir method key = case T.splitOn "." key of
  name : fields
    | Just param <- find ((== name) . pdName) (mdParams method) ->
        case foldM (fieldType ir) (pdType param) fields of
          Just ty | isBooleanRef ir ty -> TakesBoolean
          _ -> TakesNext
  _ -> TakesUnlessFlag

-- | The type of a field of a struct, or of any variant of an enum
fieldType :: IR -> TypeRef -> Text -> Maybe TypeRef
fieldType ir ref name = case ref of
  RefOptional inner -> fieldType ir inner name
  RefNamed qn -> case tdKind <$> Map.lookup (qualifiedNameFull qn) (irTypes ir) of
    Just (KindStruct fields) -> fdType <$> find ((== name) . fdName) fields
    Just (KindEnum _ variants) -> fdType <$> find ((== name) . fdName) (concatMap vdFields variants)
    Just (KindAlias target) -> fieldType ir target name
    _ -> Nothing
  _ -> Nothing

-- | Is this a boolean (optional, or a named type that is one)?
isBooleanRef :: IR -> TypeRef -> Bool
isBooleanRef ir = \case
  RefPrimitive typ _ -> typ == "boolean"
  RefOptional inner -> isBooleanRef ir inner
  RefNamed qn -> case tdKind <$> Map.lookup (qualifiedNameFull qn) (irTypes ir) of
    Just (KindPrimitive typ _) -> typ == "boolean"
    Just (KindAlias target) -> isBooleanRef ir target
    _ -> False
  _ -> False

-- | Normalize a flag key from kebab-case to snake_case
normalizeKey :: Text -> Text
normalizeKey = T.replace "-" "_"

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
    -- Collect all repeated --key value entries; for a list of scalars each
    -- entry is itself a comma-separated record (objects/lists/any stay whole)
    let rawValues = [val | ("", val) <- kvs]
        elements val
          | isScalarElement ir innerType =
              either (Left . MalformedListValue (pdName param) val) Right (splitListValue val)
          | otherwise = Right [val]
    in if null rawValues
       then Left $ InvalidValue (pdName param) "array requires at least one value"
       else do
         values <- concat <$> traverse elements rawValues
         Right $ Array $ V.fromList $ map (buildTypedValue ir innerType) values

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
    buildParamValue ir ParamDef{pdName = paramName, pdType = target, pdDescription = Nothing, pdRequired = True, pdDefault = Nothing, pdDeprecation = Nothing, pdSource = Nothing, pdValues = Nothing} kvs

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
-- List Values
-- ============================================================================

-- | Is this list element type a scalar (string, integer, number, boolean, or
-- a named type that is one of those)? Only lists of scalars split their flag
-- values on commas; objects, lists and untyped values are passed whole.
isScalarElement :: IR -> TypeRef -> Bool
isScalarElement ir = \case
  RefPrimitive typ _ -> typ `elem` scalarTypes
  RefNamed qn -> case Map.lookup (qualifiedNameFull qn) (irTypes ir) of
    Just TypeDef{tdKind = KindPrimitive typ _} -> typ `elem` scalarTypes
    Just TypeDef{tdKind = KindStringEnum _} -> True
    Just TypeDef{tdKind = KindAlias target} -> isScalarElement ir target
    _ -> False
  RefOptional inner -> isScalarElement ir inner
  RefArray _ -> False
  RefAny -> False
  RefUnknown -> False
  where
    scalarTypes = ["string", "integer", "number", "boolean"]

-- | Split one flag value of a scalar list into its elements.
--
-- The value is one comma-separated record. Unquoted fields are trimmed and
-- dropped when empty. A field starting with @"@ runs to its closing quote,
-- keeps its commas and spaces, and reads @""@ inside as one quote.
--
-- @
-- a,b,c           -> ["a", "b", "c"]
-- a, b ,c         -> ["a", "b", "c"]
-- a,,b,           -> ["a", "b"]
-- "a,b","c"       -> ["a,b", "c"]
-- "say ""hi""",b  -> ["say \"hi\"", "b"]
-- "",a            -> ["", "a"]
-- "a,b  or "a"x,b -> Left reason
-- @
splitListValue :: Text -> Either Text [Text]
splitListValue = field
  where
    field t = case T.uncons (T.stripStart t) of
      Just ('"', rest) -> quoted "" rest
      _ ->
        let (raw, rest) = T.breakOn "," t
            val = T.strip raw
        in ([val | not (T.null val)] ++) <$> afterComma rest

    -- Inside quotes: a doubled quote is one quote, a single one closes the field
    quoted acc t = case T.breakOn "\"" t of
      (_, "") -> Left "a quoted element has no closing quote"
      (chunk, close) -> case T.uncons (T.drop 1 close) of
        Just ('"', more) -> quoted (acc <> chunk <> "\"") more
        _ -> case T.uncons (T.stripStart (T.drop 1 close)) of
          Nothing -> Right [acc <> chunk]
          Just (',', more) -> ((acc <> chunk) :) <$> field more
          Just _ -> Left "a closing quote must be followed by a comma or the end"

    afterComma rest = case T.uncons rest of
      Nothing -> Right []
      Just (_, more) -> field more  -- drop the comma

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
