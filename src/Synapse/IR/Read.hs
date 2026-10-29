{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnboxedTuples #-}

-- | SYNAPSE-CC-8 (IR 2.1): the schema reader. Every JSON Schema the IR
-- builder meets in a type position is read whole here, or refused by name.
--
-- The reader is pure: @'SchemaRead' a@ is a value and the refusals met on the
-- way. There is no 'Data.Maybe.mapMaybe' that can drop a union member: a
-- union is a string enum, a tagged enum (tagged by whichever property every
-- member gives a string @const@ — RETURNS-13's @discriminatorOf@), an
-- externally tagged enum, an untagged union of named types, or a nullable;
-- anything else is a 'Refusal' naming the type, the keyword and the first
-- member it could not read, and the type is left out.
--
-- A schema in a type position uses only the closed keyword list
-- ('knownKeywords'); any other keyword is refused by name. A dynamic value
-- (a @serde_json::Value@) is the JSON type, 'RefJson' — @true@, @{}@,
-- annotations only, @type@ naming all six JSON types, or a @$ref@ to @Json@;
-- it is refused. Maps, tuples and bounds the format does not imply are
-- carried ('RefMap', 'RefTuple', 'RefRefined').
module Synapse.IR.Read
  ( -- * The reader
    SchemaRead
  , runRead
  , refuse
    -- * Documents
  , Doc
  , docOf
  , sharedDoc
  , readDefs
  , readRoot
  , RootRead(..)
    -- * Type positions
  , readType
    -- * Sites
  , siteParam
  , siteReturns
  , siteInput
    -- * The vocabulary
  , knownKeywords
  , isJsonSchema
  ) where

import Control.Monad (forM, when, zipWithM)
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub, nubBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Scientific (Scientific, toBoundedInteger)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Synapse.IR.Types

-- ============================================================================
-- The reader
-- ============================================================================

-- | What a read carries along: the refusals met (in reverse), and the named
--   roots already read (a namespace's methods mostly return one event
--   union: it is read once, then found again when its schema is the same).
data ReadState = ReadState
  { rsRefusals :: ![Refusal]
  , rsRoots    :: !(Map Text (Value, Maybe TypeKind))
  }

-- | A value, and the refusals met reading it.
newtype SchemaRead a = SchemaRead { unRead :: ReadState -> (# a, ReadState #) }

instance Functor SchemaRead where
  fmap f (SchemaRead g) = SchemaRead $ \s -> case g s of (# a, s' #) -> (# f a, s' #)

instance Applicative SchemaRead where
  pure a = SchemaRead $ \s -> (# a, s #)
  SchemaRead f <*> SchemaRead g = SchemaRead $ \s -> case f s of
    (# h, s1 #) -> case g s1 of
      (# a, s2 #) -> (# h a, s2 #)

instance Monad SchemaRead where
  SchemaRead g >>= k = SchemaRead $ \s -> case g s of (# a, s1 #) -> unRead (k a) s1

-- | The value and every refusal, in the order met.
runRead :: SchemaRead a -> (a, [Refusal])
runRead (SchemaRead g) = case g (ReadState [] Map.empty) of
  (# a, s #) -> (a, reverse (rsRefusals s))

-- | Name a site the IR cannot type.
refuse :: Text -> Text -> Maybe Int -> Text -> SchemaRead ()
refuse site kw variant detail = SchemaRead $ \s ->
  (# (), s { rsRefusals = Refusal site kw variant detail : rsRefusals s } #)

-- | A named root read before. One name in one namespace is one type: the
--   first schema read under it is the type, as the first of a name in
--   @$defs@ is (and as IR 2.0 kept it), so a later root of the same name is
--   not read again (comparing the schemas cost more than the read).
rootSeen :: Text -> Value -> SchemaRead (Maybe (Maybe TypeKind))
rootSeen site _ = SchemaRead $ \s -> case Map.lookup site (rsRoots s) of
  Just (_, k) -> (# Just k, s #)
  _ -> (# Nothing, s #)

rootRead :: Text -> Value -> Maybe TypeKind -> SchemaRead ()
rootRead site v k = SchemaRead $ \s -> (# (), s { rsRoots = Map.insert site (v, k) (rsRoots s) } #)

-- | Refuse, and stand 'RefUnknown' where the type would be.
unknownAt :: Text -> Text -> Maybe Int -> Text -> SchemaRead TypeRef
unknownAt site kw variant detail = RefUnknown <$ refuse site kw variant detail

-- ============================================================================
-- Documents
-- ============================================================================

-- | One JSON Schema document (a method's @params@, @returns@ or @input@):
--   the namespace its types land in, and its @$defs@ by name.
data Doc = Doc
  { docNamespace :: Text
  , docDefs      :: KM.KeyMap Value
  }

docOf :: Text -> KM.KeyMap Value -> Doc
docOf ns o = Doc ns $ case KM.lookup "$defs" o of
  Just (Object d) -> d
  _ -> case KM.lookup "definitions" o of
    Just (Object d) -> d
    _ -> KM.empty

-- | One namespace's documents as one: every @$defs@ entry of them, the
--   first of each name winning (as the types the builder kept always did).
--   Read once, a namespace's definitions are read once, not once a method.
sharedDoc :: Text -> [KM.KeyMap Value] -> Doc
sharedDoc ns docs = Doc ns (foldl (\acc o -> KM.union acc (docDefs (docOf ns o))) KM.empty docs)

-- | The definition a @$ref@ names, in this document.
lookupDef :: Doc -> Text -> Maybe Value
lookupDef doc ref = KM.lookup (K.fromText (refName ref)) (docDefs doc)

refName :: Text -> Text
refName ref = case T.splitOn "/" ref of
  parts@(_:_) -> last parts
  [] -> ref

qualified :: Doc -> Text -> TypeRef
qualified doc ref = RefNamed QualifiedName { qnNamespace = docNamespace doc, qnLocalName = refName ref }

typeSite :: Doc -> Text -> Text
typeSite doc name = docNamespace doc <> "." <> name

siteParam :: Text -> Text -> Text
siteParam path name = path <> "(" <> name <> ")"

siteReturns :: Text -> Text
siteReturns path = path <> " returns"

siteInput :: Text -> Text
siteInput path = path <> " input"

-- ============================================================================
-- The vocabulary
-- ============================================================================

-- | The keywords a schema in a type position may use (SYNAPSE-CC-8's closed
--   list). Any other is refused by name.
knownKeywords :: Set.Set Text
knownKeywords = Set.fromList
  [ "type", "$ref", "properties", "required", "additionalProperties", "items", "prefixItems"
  , "oneOf", "anyOf", "enum", "const", "format"
  , "minimum", "maximum", "exclusiveMinimum", "exclusiveMaximum"
  , "minLength", "maxLength", "pattern", "minItems", "maxItems"
  , "default", "description", "title", "examples", "deprecated"
  , "$schema", "$defs", "definitions"
  , "x-plexus-source", "x-plexus-values"
  ]

-- | Refuse each keyword outside the list; 'True' when there were none.
keywordsOk :: Text -> KM.KeyMap Value -> SchemaRead Bool
keywordsOk site o
  | KM.foldrWithKey (\k _ acc -> Set.member (K.toText k) knownKeywords && acc) True o = pure True
  | otherwise = do
      let bad = [ k | k <- map K.toText (KM.keys o), not (Set.member k knownKeywords) ]
      mapM_ (\k -> refuse site k Nothing ("`" <> k <> "` is not a keyword synapse reads")) bad
      pure False

-- | The JSON type (a @serde_json::Value@), as a schema says it: annotations
--   only (@{}@, a description, a default), or @type@ naming all six JSON
--   types (@["null", "boolean", "number", "string", "array", "object"]@, in
--   any order) and nothing else that types. @true@ says it too, and a
--   @$ref@ to a definition named @Json@ ('isJsonRef').
isJsonSchema :: KM.KeyMap Value -> Bool
isJsonSchema o = not (any (`KM.member` o) structural) || allSix
  where
    structural = ["type", "$ref", "properties", "additionalProperties", "items", "prefixItems", "oneOf", "anyOf", "enum", "const"]
    allSix = case KM.lookup "type" o of
      Just (Array ts) ->
        Set.fromList [ t | String t <- V.toList ts ] == jsonTypes
          && not (any (`KM.member` o) (filter (/= "type") structural))
      _ -> False

jsonTypes :: Set.Set Text
jsonTypes = Set.fromList ["null", "boolean", "number", "string", "array", "object"]

jsonValue :: Value -> Bool
jsonValue = \case
  Bool True -> True
  Object o -> isJsonSchema o
  _ -> False

-- | A @$ref@ to the definition named @Json@: the JSON type.
isJsonRef :: Text -> Bool
isJsonRef ref = refName ref == "Json"

-- ============================================================================
-- Type positions
-- ============================================================================

-- | The type of a schema in a type position (a field, a param, an element),
--   at a site.
readType :: Doc -> Text -> Value -> SchemaRead TypeRef
readType doc site = \case
  Bool True -> pure RefJson
  Object o -> do
    ok <- keywordsOk site o
    if not ok then pure RefUnknown
      else if isJsonSchema o then pure RefJson
      else readTypeObject doc site o
  _ -> unknownAt site "type" Nothing "not a schema"

readTypeObject :: Doc -> Text -> KM.KeyMap Value -> SchemaRead TypeRef
readTypeObject doc site o
  | Just (String ref) <- KM.lookup "$ref" o = pure (if isJsonRef ref then RefJson else qualified doc ref)
  | Just (kw, members) <- unionOf o = readInlineUnion doc site kw members
  | Just (Array ts) <- KM.lookup "type" o =
      let types = [ t | String t <- V.toList ts ]
          nonNull = filter (/= "null") types
          hasNull = "null" `elem` types
      in case nonNull of
           [t] -> do
             base <- readTyped doc (if hasNull then site <> "?" else site) t o
             pure (if hasNull then RefOptional base else base)
           [] -> pure (RefPrimitive "null" Nothing)
           _ -> unknownAt site "type" Nothing "a type list that is neither one type (with null or without) nor the JSON type (all six)"
  | Just (String t) <- KM.lookup "type" o = readTyped doc site t o
  | KM.member "enum" o || KM.member "const" o = pure (RefPrimitive (literalType o) Nothing)
  | KM.member "items" o || KM.member "prefixItems" o = readTyped doc site "array" o
  | otherwise = readTyped doc site "object" o

-- | The JSON type of an enum's or const's values (a string, for a string enum).
literalType :: KM.KeyMap Value -> Text
literalType o = case concat [ maybe [] V.toList (arr (KM.lookup "enum" o)), maybe [] pure (KM.lookup "const" o) ] of
  (Number _ : _) -> "number"
  (Bool _ : _) -> "boolean"
  _ -> "string"
  where arr (Just (Array a)) = Just a
        arr _ = Nothing

-- | A schema of one JSON type.
readTyped :: Doc -> Text -> Text -> KM.KeyMap Value -> SchemaRead TypeRef
readTyped doc site t o = case t of
  "array"
    | Just (Array prefix) <- KM.lookup "prefixItems" o -> readTuple doc site (V.toList prefix) o
    | Just items <- KM.lookup "items" o -> do
        inner <- readType doc (site <> "[]") items
        refined site (RefArray inner) (itemBounds o)
    | otherwise -> RefArray <$> unknownAt (site <> "[]") "items" Nothing "an array with no items"
  "object"
    | KM.member "properties" o ->
        unknownAt site "properties" Nothing "an object with properties in a type position has no name: give it one in $defs"
    | Just ap <- KM.lookup "additionalProperties" o, ap /= Bool False ->
        RefMap <$> readType doc (site <> "{}") ap
    | otherwise -> unknownAt site "type" Nothing "an object with neither properties nor additionalProperties"
  _ -> do
    let base = RefPrimitive t (formatOf o)
    refined site base (scalarBounds (formatOf o) o)

-- | @prefixItems@ with @minItems = maxItems = n@: a tuple of n.
readTuple :: Doc -> Text -> [Value] -> KM.KeyMap Value -> SchemaRead TypeRef
readTuple doc site prefix o
  | intAt "minItems" o == Just n, intAt "maxItems" o == Just n, noExtra =
      RefTuple <$> zipWithM (\i v -> readType doc (site <> "." <> T.pack (show i)) v) [0 :: Int ..] prefix
  | otherwise = unknownAt site "prefixItems" Nothing
      "prefixItems is read only as a tuple: minItems = maxItems = its length, no further items"
  where
    n = length prefix
    noExtra = case KM.lookup "items" o of
      Nothing -> True
      Just (Bool False) -> True
      _ -> False

formatOf :: KM.KeyMap Value -> Maybe Text
formatOf o = case KM.lookup "format" o of
  Just (String s) -> Just s
  _ -> Nothing

intAt :: Text -> KM.KeyMap Value -> Maybe Int
intAt k o = case KM.lookup (K.fromText k) o of
  Just (Number n) -> toBoundedInteger n
  _ -> Nothing

numAt :: Text -> KM.KeyMap Value -> Maybe Scientific
numAt k o = case KM.lookup (K.fromText k) o of
  Just (Number n) -> Just n
  _ -> Nothing

-- | Wrap a ref in its bounds, when it has any.
refined :: Text -> TypeRef -> Refinements -> SchemaRead TypeRef
refined _ t r
  | isNoRefinements r = pure t
  | otherwise = pure (RefRefined t r)

-- | An array's bounds (a tuple's are its length).
itemBounds :: KM.KeyMap Value -> Refinements
itemBounds o = noRefinements
  { rnMinItems = intAt "minItems" o
  , rnMaxItems = intAt "maxItems" o
  }

-- | A scalar's bounds, less those its format already says.
scalarBounds :: Maybe Text -> KM.KeyMap Value -> Refinements
scalarBounds fmt o
  | not (any (`KM.member` o) ["minimum", "maximum", "exclusiveMinimum", "exclusiveMaximum", "minLength", "maxLength", "pattern"]) = noRefinements
  | otherwise = Refinements
  { rnMinimum = numAt "minimum" o `unlessEq` impliedMin
  , rnMaximum = numAt "maximum" o `unlessEq` impliedMax
  , rnExclusiveMinimum = numAt "exclusiveMinimum" o
  , rnExclusiveMaximum = numAt "exclusiveMaximum" o
  , rnMinLength = intAt "minLength" o
  , rnMaxLength = intAt "maxLength" o
  , rnPattern = case KM.lookup "pattern" o of { Just (String p) -> Just p; _ -> Nothing }
  , rnMinItems = Nothing
  , rnMaxItems = Nothing
  }
  where
    (impliedMin, impliedMax) = formatBounds fmt
    unlessEq (Just v) (Just implied) | v == implied = Nothing
    unlessEq v _ = v

-- | The bounds an integer format implies (schemars writes them out).
formatBounds :: Maybe Text -> (Maybe Scientific, Maybe Scientific)
formatBounds = \case
  Just "uint8"  -> (Just 0, Just 255)
  Just "uint16" -> (Just 0, Just 65535)
  Just "uint32" -> (Just 0, Just 4294967295)
  Just "uint64" -> (Just 0, Just 18446744073709551615)
  Just "uint128" -> (Just 0, Nothing)
  Just "uint"   -> (Just 0, Nothing)
  Just "int8"   -> (Just (-128), Just 127)
  Just "int16"  -> (Just (-32768), Just 32767)
  Just "int32"  -> (Just (-2147483648), Just 2147483647)
  Just "int64"  -> (Just (-9223372036854775808), Just 9223372036854775807)
  _ -> (Nothing, Nothing)

-- ============================================================================
-- Unions
-- ============================================================================

-- | A schema's union: its keyword and members.
unionOf :: KM.KeyMap Value -> Maybe (Text, [Value])
unionOf o = case (KM.lookup "oneOf" o, KM.lookup "anyOf" o) of
  (Just (Array ms), _) -> Just ("oneOf", V.toList ms)
  (_, Just (Array ms)) -> Just ("anyOf", V.toList ms)
  _ -> Nothing

-- | A union in a type position: only a nullable (or a string enum, which a
--   ref carries as a string) has a form there; any other union has no name.
readInlineUnion :: Doc -> Text -> Text -> [Value] -> SchemaRead TypeRef
readInlineUnion doc site kw members
  | Just inner <- nullableMember members = RefOptional <$> readType doc (site <> "?") inner
  | Just _ <- stringEnumValues members = pure (RefPrimitive "string" Nothing)
  | otherwise = unknownAt site kw Nothing
      "a union in a type position has no name: give it one in $defs"

-- | @[X, {"type": "null"}]@ (either order) is a nullable @X@.
nullableMember :: [Value] -> Maybe Value
nullableMember [a, b]
  | isNullSchema a = Just b
  | isNullSchema b = Just a
nullableMember _ = Nothing

isNullSchema :: Value -> Bool
isNullSchema (Object o) = KM.lookup "type" o == Just (String "null")
isNullSchema _ = False

-- | Every member a string literal (@{"const": "a"}@ or @{"enum": ["a", …]}@,
--   of type string): the literals, in schema order.
stringEnumValues :: [Value] -> Maybe [Text]
stringEnumValues [] = Nothing
stringEnumValues members = concat <$> traverse stringLiterals members

stringLiterals :: Value -> Maybe [Text]
stringLiterals (Object m)
  | stringTyped, Just (String c) <- KM.lookup "const" m = Just [c]
  | stringTyped, Just (Array vs) <- KM.lookup "enum" m, not (V.null vs) = traverse asString (V.toList vs)
  where
    stringTyped = case KM.lookup "type" m of
      Nothing -> True
      Just (String "string") -> True
      _ -> False
    asString (String s) = Just s
    asString _ = Nothing
stringLiterals _ = Nothing

-- | A named union's kind, read whole or refused.
readUnionKind :: Doc -> Text -> Text -> [Value] -> SchemaRead (Maybe TypeKind)
readUnionKind doc site kw members
  | Just inner <- nullableMember members = Just . KindAlias . RefOptional <$> readType doc (site <> "?") inner
  | Just values <- stringEnumValues members = pure (Just (KindStringEnum values))
  | Just shapes <- traverse (variantShape doc maxDepth) members
  , Just tag <- discriminatorOf shapes = Just <$> taggedEnum doc site tag shapes
  | Just vs <- traverse externalMember members = Just <$> externalEnum doc site vs
  | Just refs <- traverse refOnly members, not (null refs) = pure $ Just KindEnum
      { keDiscriminator = ""
      , keRepr = Untagged
      , keVariants =
          [ VariantDef { vdName = refName r, vdDescription = d, vdFields = [], vdRef = Just (qualified doc r) }
          | (r, d) <- refs ]
      }
  | otherwise = do
      let (i, why) = firstUnreadable doc members
      Nothing <$ refuse site kw (Just i) why

-- | The first member a union could not be read past, and why.
firstUnreadable :: Doc -> [Value] -> (Int, Text)
firstUnreadable doc members = case members of
  [] -> (0, "an empty union")
  (m0 : _)
    | isJust (stringLiterals m0) -> pick (isJust . stringLiterals) "is not a string, as the members before it are"
    | Just s0 <- variantShape doc maxDepth m0 ->
        let tags = map fst (variantConsts s0)
            fits m = case variantShape doc maxDepth m of
              Just s -> any (`elem` tags) (map fst (variantConsts s))
              Nothing -> False
        in pick fits "shares no string-const property with the others and is not a string"
    | otherwise -> (0, "variant 0 is neither a string, an object nor a $ref to one")
  where
    pick ok why = case [ i | (i, m) <- zip [0 :: Int ..] members, not (ok m) ] of
      (i : _) -> (i, "variant " <> T.pack (show i) <> " " <> why)
      [] -> (0, "no string-const property tells the variants apart")

-- | @{"$ref": X}@ (a description beside it) as a member: X and the description.
refOnly :: Value -> Maybe (Text, Maybe Text)
refOnly (Object m)
  | Just (String r) <- KM.lookup "$ref" m
  , all (`elem` ["$ref", "description", "title"]) (map K.toText (KM.keys m)) =
      Just (r, descriptionOf (Object m))
refOnly _ = Nothing

-- ============================================================================
-- Tagged enums (RETURNS-13's rules, ported from 873e791)
-- ============================================================================

-- | One member of a union, its references resolved.
data VariantShape = VariantShape
  { vsProps    :: [(Text, Value)]   -- ^ Its own properties, the tag among them
  , vsRequired :: [Text]
  , vsWrapped  :: Maybe Value       -- ^ The struct a newtype variant wraps (a @$ref@ beside the tag)
  , vsDesc     :: Maybe Text
  }

maxDepth :: Int
maxDepth = 8

-- | A union member as a variant: an object schema, or a @$ref@ to one.
variantShape :: Doc -> Int -> Value -> Maybe VariantShape
variantShape doc depth member@(Object m)
  | Just (String ref) <- KM.lookup "$ref" m
  , not (KM.member "properties" m)
  , depth > 0 = do
      target <- lookupDef doc ref
      shape <- variantShape doc (depth - 1) target
      pure shape { vsDesc = descriptionOf member <|>. vsDesc shape }
  | Just (Object props) <- KM.lookup "properties" m = Just VariantShape
      { vsProps    = [ (K.toText k, v) | (k, v) <- KM.toList props ]
      , vsRequired = requiredOf m
      , vsWrapped  = KM.lookup "$ref" m
      , vsDesc     = descriptionOf member
      }
variantShape _ _ _ = Nothing

(<|>.) :: Maybe a -> Maybe a -> Maybe a
(<|>.) (Just a) _ = Just a
(<|>.) Nothing b = b

-- | The string @const@s a variant gives its own properties (a one-value
--   @enum@ counts, as draft-07 tooling writes it).
variantConsts :: VariantShape -> [(Text, Text)]
variantConsts s =
  [ (k, c)
  | (k, Object p) <- vsProps s
  , Just c <- [constOf p]
  ]
  where
    constOf p = case (KM.lookup "const" p, KM.lookup "enum" p) of
      (Just (String c), _) -> Just c
      (_, Just (Array vs)) | [String c] <- V.toList vs -> Just c
      _ -> Nothing

-- | The tag: the property every variant gives a @const@, no two variants
--   the same one. Should several qualify, the one first in the first
--   variant's @required@ (schemars lists the tag first).
discriminatorOf :: [VariantShape] -> Maybe Text
discriminatorOf [] = Nothing
discriminatorOf shapes@(first : _) =
  case sortOn rank candidates of
    [] -> Nothing
    (k : _) -> Just k
  where
    consts = map variantConsts shapes
    candidates =
      [ k
      | (k, _) <- variantConsts first
      , Just values <- [traverse (lookup k) consts]
      , length (nub values) == length values
      ]
    rank k = fromMaybe maxBound (lookup k (zip (vsRequired first) [0 :: Int ..]))

taggedEnum :: Doc -> Text -> Text -> [VariantShape] -> SchemaRead TypeKind
taggedEnum doc site tag shapes = do
  variants <- forM shapes $ \s -> do
    let name = fromMaybe "" (lookup tag (variantConsts s))
        vsite = site <> "." <> name
    own <- fieldsOf doc vsite (vsRequired s) [ kv | kv@(k, _) <- vsProps s, k /= tag ]
    wrapped <- case vsWrapped s of
      Nothing -> pure []
      Just w -> wrappedFields doc vsite w
    pure VariantDef
      { vdName = name
      , vdDescription = vsDesc s
      , vdFields = nubBy (\a b -> fdName a == fdName b) (own ++ wrapped)
      , vdRef = Nothing
      }
  pure KindEnum { keDiscriminator = tag, keVariants = variants, keRepr = Internal }

-- | The fields of the struct a newtype variant wraps, flattened (RETURNS-13's
--   @vsWrapped@); refused when it wraps no struct.
wrappedFields :: Doc -> Text -> Value -> SchemaRead [FieldDef]
wrappedFields doc site = go maxDepth
  where
    go depth (String ref)
      | depth > 0, Just (Object t) <- lookupDef doc ref = case () of
          _ | Just (String r') <- KM.lookup "$ref" t, not (KM.member "properties" t) -> go (depth - 1) (String r')
            | KM.member "properties" t -> fieldsOf doc site (requiredOf t) (propsOf t)
            | KM.lookup "type" t == Just (String "object") -> pure []
            | otherwise -> notStruct
    go _ _ = notStruct
    notStruct = [] <$ refuse site "$ref" Nothing "a newtype variant wraps no struct whose fields it could carry"

-- | An externally tagged member: one property, required, and not a const.
externalMember :: Value -> Maybe (Text, Value, Maybe Text)
externalMember (Object m)
  | Just (Object props) <- KM.lookup "properties" m
  , [(k, v)] <- KM.toList props
  , requiredOf m == [K.toText k]
  , not (isConstSchema v) = Just (K.toText k, v, descriptionOf (Object m))
externalMember _ = Nothing

isConstSchema :: Value -> Bool
isConstSchema (Object p) = KM.member "const" p
isConstSchema _ = False

externalEnum :: Doc -> Text -> [(Text, Value, Maybe Text)] -> SchemaRead TypeKind
externalEnum doc site members = do
  variants <- forM members $ \(name, v, desc) -> do
    let vsite = site <> "." <> name
    case v of
      Object inner | KM.member "properties" inner, isNothing (KM.lookup "$ref" inner) -> do
        _ <- keywordsOk vsite inner
        fields <- fieldsOf doc vsite (requiredOf inner) (propsOf inner)
        pure VariantDef { vdName = name, vdDescription = desc, vdFields = fields, vdRef = Nothing }
      _ -> do
        t <- readType doc vsite v
        pure VariantDef { vdName = name, vdDescription = desc, vdFields = [], vdRef = Just t }
  pure KindEnum { keDiscriminator = "", keVariants = variants, keRepr = External }

-- ============================================================================
-- Structs and fields
-- ============================================================================

propsOf :: KM.KeyMap Value -> [(Text, Value)]
propsOf o = case KM.lookup "properties" o of
  Just (Object p) -> [ (K.toText k, v) | (k, v) <- KM.toList p ]
  _ -> []

requiredOf :: KM.KeyMap Value -> [Text]
requiredOf o = case KM.lookup "required" o of
  Just (Array arr) -> [ t | String t <- V.toList arr ]
  _ -> []

fieldsOf :: Doc -> Text -> [Text] -> [(Text, Value)] -> SchemaRead [FieldDef]
fieldsOf doc site required props = forM props $ \(k, v) -> do
  t <- readType doc (site <> "." <> k) v
  pure FieldDef
    { fdName = k
    , fdType = t
    , fdDescription = descriptionOf v
    , fdRequired = k `elem` required
    , fdDefault = defaultOf v
    }

descriptionOf :: Value -> Maybe Text
descriptionOf (Object o) = case KM.lookup "description" o of
  Just (String s) -> Just s
  _ -> Nothing
descriptionOf _ = Nothing

defaultOf :: Value -> Maybe Value
defaultOf (Object o) = KM.lookup "default" o
defaultOf _ = Nothing

-- ============================================================================
-- Named types
-- ============================================================================

-- | Every @$defs@ entry of a document, as types of its namespace. A type the
--   reader refuses is left out (its uses then name no type, and the builder
--   turns them to 'RefUnknown').
readDefs :: Doc -> SchemaRead (Map Text TypeDef)
readDefs doc = do
  -- the definition named Json, when it is the JSON type, is 'RefJson'
  -- wherever it is named: it is no type of the namespace's own
  defs <- forM [ kv | kv@(k, v) <- KM.toList (docDefs doc), not (K.toText k == "Json" && jsonValue v) ] $ \(k, v) -> do
    let name = K.toText k
    fmap (\td -> (tdFullName td, td)) <$> readNamed doc name v
  pure (Map.fromList [ d | Just d <- defs ])

-- | One named type: its kind, read whole, or 'Nothing' and a refusal.
readNamed :: Doc -> Text -> Value -> SchemaRead (Maybe TypeDef)
readNamed doc name v = fmap (TypeDef name (docNamespace doc) (descriptionOf v)) <$> readKind doc (typeSite doc name) v

readKind :: Doc -> Text -> Value -> SchemaRead (Maybe TypeKind)
readKind doc site = \case
  Bool True -> pure (Just (KindAlias RefJson))
  Object o -> do
    ok <- keywordsOk site o
    if not ok then pure Nothing
      else if isJsonSchema o then pure (Just (KindAlias RefJson))
      else readKindObject doc site o
  _ -> Nothing <$ refuse site "type" Nothing "not a schema"

readKindObject :: Doc -> Text -> KM.KeyMap Value -> SchemaRead (Maybe TypeKind)
readKindObject doc site o
  | Just (kw, members) <- unionOf o =
      if KM.member "properties" o
        then Nothing <$ refuse site kw Nothing "a struct with a union flattened into it has no IR form"
        else readUnionKind doc site kw members
  | Just (Array values) <- KM.lookup "enum" o =
      case traverse asString (V.toList values) of
        Just vs | not (null vs) -> pure (Just (KindStringEnum vs))
        _ -> Nothing <$ refuse site "enum" Nothing "an enum of values that are not all strings"
  | KM.member "properties" o = do
      when (maybe False (\ap -> ap /= Bool False) (KM.lookup "additionalProperties" o)) $
        refuse site "additionalProperties" Nothing "a map beside properties (a flattened map) has no IR form"
      fields <- fieldsOf doc site (requiredOf o) (propsOf o)
      wrapped <- case KM.lookup "$ref" o of
        Nothing -> pure []
        Just w -> wrappedFields doc site w
      pure (Just (KindStruct (nubBy (\a b -> fdName a == fdName b) (fields ++ wrapped))))
  | Just (String ref) <- KM.lookup "$ref" o = pure (Just (KindAlias (if isJsonRef ref then RefJson else qualified doc ref)))
  | Just (String t) <- KM.lookup "type" o, t `notElem` ["array", "object"] =
      let r = scalarBounds (formatOf o) o
      in pure $ Just $ if isNoRefinements r
           then KindPrimitive t (formatOf o)
           else KindAlias (RefRefined (RefPrimitive t (formatOf o)) r)
  | KM.member "const" o, not (KM.member "type" o) || KM.lookup "type" o == Just (String "string")
  , Just vs <- stringLiterals (Object o) = pure (Just (KindStringEnum vs))
  | otherwise = do
      t <- readTypeObject doc site o
      pure $ case t of
        RefUnknown -> Nothing
        _ -> Just (KindAlias t)
  where
    asString (String s) = Just s
    asString _ = Nothing

-- ============================================================================
-- A document's root
-- ============================================================================

-- | What a document's root (a method's @returns@ or @input@) reads as.
data RootRead = RootRead
  { rrTypes     :: Map Text TypeDef   -- ^ its @$defs@, and the root's own type when it is named
  , rrRef       :: TypeRef            -- ^ the root as a ref
  , rrStreaming :: Bool               -- ^ a union of more than one non-error variant
  }

-- | A document's root: a titled (or else @fallback@-named) struct, enum or
--   union is a type of its own; anything else is the plain ref.
readRoot :: Maybe Doc -> Text -> Text -> Text -> Value -> SchemaRead RootRead
readRoot shared namespace site fallback = \case
  Object o -> do
    let doc = maybe (docOf namespace o) id shared
        name = case KM.lookup "title" o of
          Just (String t) | not (T.null t) -> t
          _ -> fallback
        named = isJust (unionOf o) && isNothing (nullableMember (maybe [] snd (unionOf o)))
             || KM.member "properties" o
             || KM.member "enum" o
    defs <- maybe (readDefs doc) (const (pure Map.empty)) shared
    if named
      then do
        let body = Object (KM.delete "$defs" (KM.delete "definitions" o))
            tsite = typeSite doc name
        seen <- rootSeen tsite body
        kind <- case seen of
          Just k -> pure k
          Nothing -> do
            k <- readKind doc tsite body
            rootRead tsite body k
            pure k
        let ref = RefNamed QualifiedName { qnNamespace = namespace, qnLocalName = name }
        case kind of
          Just k -> do
            let td = TypeDef name namespace (descriptionOf (Object o)) k
            pure RootRead
              { rrTypes = Map.insert (tdFullName td) td defs
              , rrRef = ref
              , rrStreaming = streams k
              }
          Nothing -> pure RootRead { rrTypes = defs, rrRef = RefUnknown, rrStreaming = False }
      else do
        ref <- readType doc site (Object (KM.delete "$defs" (KM.delete "definitions" o)))
        pure RootRead { rrTypes = defs, rrRef = ref, rrStreaming = False }
  other -> do
    ref <- readType (Doc namespace KM.empty) site other
    pure RootRead { rrTypes = Map.empty, rrRef = ref, rrStreaming = False }
  where
    streams = \case
      KindEnum _ vs Internal -> length (filter ((/= "error") . vdName) vs) > 1
      _ -> False
