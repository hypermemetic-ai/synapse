{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | IR Builder - walks schema tree and constructs the IR
--
-- IR 3.0 (plexus 0.7). For every method it reads, from the schema, never
-- guessing:
--
-- 1. its item type (@returns@), error type (@error@) and progress type
--    (@progress@), each a 'TypeRef' — a 'RefNamed' with a 'TypeDef', or a
--    primitive / array / optional / any ref;
-- 2. its cardinality (@cardinality@), as is;
-- 3. a 'TypeDef' for every named type in those schemas and in its params:
--    a struct, a tagged enum (whatever its tag is called; a newtype variant
--    over a struct carries the struct's fields), a string enum, or an alias.
--
-- One title in one namespace is one type: two methods whose schemas give the
-- same title the same shape share one 'TypeDef'; two shapes under one title
-- fail the build ('TitleCollision'), naming both methods. Identical types in
-- different namespaces are then deduplicated (prefer the parent namespace,
-- then the shortest).
module Synapse.IR.Builder
  ( -- * Building IR
    buildIR
  , buildIRFromSchema

    -- * Errors
  , IRBuildError(..)
  , TypeSource(..)
  , renderIRBuildError

    -- * Extraction (for testing)
  , extractTypesFromSchema
  , extractMethodDef
  , schemaToTypeRef
  ) where

import Control.Applicative ((<|>))
import Control.Monad (foldM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (minimumBy, nub, nubBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Synapse.Schema.Types
import Synapse.Schema.Functor (SchemaF(..))
import Synapse.Algebra.Walk (walkSchemaPar)
import Synapse.Monad
import Synapse.IR.Types hiding (QualifiedName(..), qualifiedNameFull)
import Synapse.IR.Types (QualifiedName(..), qualifiedNameFull, synapseVersion)
import Plexus.Schema.Recursive (DeprecationInfo, ParamSchema(..))

-- ============================================================================
-- Errors
-- ============================================================================

-- | Why no IR can be built from a schema.
data IRBuildError
  = TitleCollision
      { tcNamespace :: Text        -- ^ The namespace both types are in
      , tcTitle     :: Text        -- ^ The title they share
      , tcFirst     :: TypeSource  -- ^ Where the first shape was read
      , tcSecond    :: TypeSource  -- ^ Where the second, different, shape was read
      }
    -- ^ Two different shapes carry one title in one namespace. One title in
    --   one namespace is one Rust type, so this is a conflict the
    --   activation's author resolves; the IR never picks one.
  deriving stock (Show, Eq)

-- | Where a 'TypeDef' was read: a method, and which of its schemas.
data TypeSource = TypeSource
  { tsMethod :: Text  -- ^ The method's name
  , tsSchema :: Text  -- ^ @"params"@, @"returns"@, @"error"@ or @"progress"@
  }
  deriving stock (Show, Eq)

-- | One line, naming the title and both methods.
renderIRBuildError :: IRBuildError -> Text
renderIRBuildError TitleCollision{..} =
  "IR: two different types are titled \"" <> tcTitle <> "\" in namespace "
    <> renderNamespace tcNamespace <> ": " <> renderSource tcFirst
    <> " and " <> renderSource tcSecond
    <> ". One title in one namespace is one type; rename one of them."
  where
    renderNamespace ns
      | T.null ns = "(root)"
      | otherwise = "\"" <> ns <> "\""
    renderSource TypeSource{..} = "method " <> tsMethod <> " (" <> tsSchema <> ")"

-- | Raise an 'IRBuildError' in 'SynapseM'.
liftBuild :: Either IRBuildError a -> SynapseM a
liftBuild = either (throwError . ValidationError . renderIRBuildError) pure

-- ============================================================================
-- Building IR
-- ============================================================================

-- | Parse a generator info string "tool:version" into GeneratorInfo
-- Returns Nothing if the format is invalid
parseGeneratorInfo :: Text -> Maybe GeneratorInfo
parseGeneratorInfo s = case T.splitOn ":" s of
  [tool, version] | not (T.null tool) && not (T.null version) ->
    Just $ GeneratorInfo tool version
  _ -> Nothing

-- | Extract V2 hash information from a plugin schema
-- Since Plexus currently only provides a composite 'hash' field,
-- we use it for all three hash fields (backward compatible V1 mode)
extractPluginHashInfo :: PluginSchema -> PluginHashInfo
extractPluginHashInfo schema = PluginHashInfo
  { phiHash = psHash schema
  , phiSelfHash = psHash schema      -- V1 fallback: use composite hash
  , phiChildrenHash = psHash schema  -- V1 fallback: use composite hash
  }

-- | Build IR by walking the schema tree from a given path
-- After walking, deduplicate types that have identical structure
-- Accepts generator info strings in "tool:version" format
-- Uses parallel schema fetching for better performance
--
-- Fails with a 'ValidationError' carrying 'renderIRBuildError' when the
-- schema puts two shapes under one title in one namespace.
buildIR :: [Text] -> Path -> SynapseM IR
buildIR generatorInfoStrs path = do
  backend <- asks seBackend
  raw <- walkSchemaPar irAlgebra path

  -- Parse generator info and add synapse itself
  let parsedGens = mapMaybe parseGeneratorInfo generatorInfoStrs
      allGens = parsedGens ++ [GeneratorInfo "synapse" synapseVersion]

  -- Get current timestamp in ISO 8601 format
  currentTime <- liftIO getCurrentTime
  let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" currentTime

  -- Create generation metadata
  let metadata = GenerationMetadata
        { gmGenerators = allGens
        , gmTimestamp = timestamp
        , gmIrVersion = irVersion emptyIR
        }

  pure $ deduplicateTypes raw
    { irBackend = backend
    , irMetadata = Just metadata
    }

-- | Build IR from one plugin schema, with no backend: what 'buildIR' builds
--   for a walk that starts at @path@ and finds this schema there, children
--   aside (they are summaries; nothing fetches them). 'irMetadata' is
--   'Nothing' — the toolchain and the clock are 'buildIR''s to record — so
--   the result is a pure function of its arguments (the conformance
--   fixture's golden @ir.json@ is built with it).
buildIRFromSchema
  :: Text          -- ^ Backend name ('irBackend')
  -> Path          -- ^ The path the schema sits at; its namespace is the path joined with "."
  -> PluginSchema
  -> Either IRBuildError IR
buildIRFromSchema backend path schema = do
  raw <- pluginIR path schema []
  pure $ (deduplicateTypes raw)
    { irBackend = backend
    , irMetadata = Nothing
    }

-- | Algebra for building IR from schema tree
--
-- At each node:
-- - Extract types from all methods' params and returns
-- - Build method definitions
-- - Merge with child results
irAlgebra :: SchemaF IR -> SynapseM IR
irAlgebra (PluginF schema path childIRs) = liftBuild (pluginIR path schema childIRs)

irAlgebra (MethodF method namespace path) = liftBuild $ do
  -- Single method node (shouldn't happen in normal walk, but handle it)
  let fullPath = T.intercalate "." path
  (types, mdef) <- extractMethodDef namespace fullPath method
  pure $ IR
    { irVersion = irVersion emptyIR  -- Use version from emptyIR
    , irBackend = irBackend emptyIR  -- Will be set by buildIR
    , irHash = Nothing  -- Methods don't carry hash
    , irMetadata = Nothing  -- Will be set by buildIR
    , irTypes = types
    , irMethods = Map.singleton fullPath mdef
    , irPlugins = Map.singleton namespace [methodName method]
    , irPluginHashes = Nothing  -- Methods don't have plugin-level hashes
    , irPluginRequests = Nothing  -- Methods don't carry request schemas
    }

-- | One plugin node: its own types and methods, merged with its children's.
pluginIR :: Path -> PluginSchema -> [IR] -> Either IRBuildError IR
pluginIR path schema childIRs = do
  -- Use full path as namespace to avoid collisions
  -- e.g., "hyperforge.workspace.repos" instead of just "repos"
  let namespace = T.intercalate "." path
      pathPrefix = namespace  -- Same as namespace

  -- Extract types and methods from this plugin
  (localTypes, localMethods) <- extractFromPlugin namespace pathPrefix schema

  -- Merge with children
  let childIR = foldr mergeIR emptyIR childIRs
  let pluginMethods = map mdName (Map.elems localMethods)

  -- Use this plugin's hash if at root (path is empty)
  let thisHash = if null path
                 then Just (psHash schema)
                 else irHash childIR

  -- Extract V2 hash information for this plugin
  let hashInfo = extractPluginHashInfo schema
      childHashes = fromMaybe Map.empty (irPluginHashes childIR)
      allHashes = Map.insert namespace hashInfo childHashes

  -- REQ-5: capture per-plugin PlexusRequest schema (psRequest from wire schema)
  let childRequests = fromMaybe Map.empty (irPluginRequests childIR)
      allRequests = case psRequest schema of
        Just reqV -> Map.insert namespace reqV childRequests
        Nothing   -> childRequests

  pure $ IR
    { irVersion = irVersion emptyIR  -- Use version from emptyIR
    , irBackend = irBackend emptyIR  -- Will be set by buildIR
    , irHash = thisHash
    , irMetadata = irMetadata childIR  -- Preserve metadata from child
    , irTypes = Map.union localTypes (irTypes childIR)  -- Namespaces differ, so keys never meet
    , irMethods = Map.union localMethods (irMethods childIR)
    , irPlugins = Map.insert namespace pluginMethods (irPlugins childIR)
    , irPluginHashes = Just allHashes  -- V2: Store hash info per plugin
    , irPluginRequests = if Map.null allRequests then Nothing else Just allRequests
    }

-- ============================================================================
-- Type Deduplication
-- ============================================================================

-- | Deduplicate types by content hash
--
-- When multiple namespaces define identical types (e.g., solar.SolarEvent and
-- jupiter.SolarEvent), we deduplicate them by keeping one canonical version.
--
-- Strategy:
-- 1. Hash each TypeDef by its structure (name + kind, ignoring namespace)
-- 2. Group duplicates
-- 3. Pick canonical: prefer parent namespace, then shortest namespace
-- 4. Update all RefNamed references to point to canonical qualified name
deduplicateTypes :: IR -> IR
deduplicateTypes ir =
  let -- Group types by their content hash
      typesByHash = Map.fromListWith (++)
        [ (hashTypeStructure td, [(fullName, td)])
        | (fullName, td) <- Map.toList (irTypes ir)
        ]

      -- Pick canonical version for each group
      canonical = Map.fromList
        [ (hash, selectCanonical group)
        | (hash, group) <- Map.toList typesByHash
        ]

      -- Build redirect map: old qualified name -> canonical qualified name
      redirects = Map.fromList
        [ (oldName, canonicalName)
        | group <- Map.elems typesByHash
        , let canonicalName = fst (selectCanonical group)
        , (oldName, _) <- group
        , oldName /= canonicalName
        ]

      -- Keep only canonical types and update their internal type references
      canonicalPairs = [canon | canon <- Map.elems canonical]
      dedupedTypesWithUpdatedRefs = Map.fromList
        [ (qualName, updateTypeDefRefs redirects td)
        | (qualName, td) <- canonicalPairs
        ]

      -- Update all method references
      dedupedMethods = Map.map (updateMethodRefs redirects) (irMethods ir)

  in ir { irTypes = dedupedTypesWithUpdatedRefs, irMethods = dedupedMethods }

-- | Hash a TypeDef by its structure (ignoring namespace and description)
-- Types are considered identical if they have the same name and kind
-- We normalize TypeRefs (strip namespaces) before hashing to detect structural identity
hashTypeStructure :: TypeDef -> Text
hashTypeStructure TypeDef{..} =
  tdName <> "::" <> T.pack (show (normalizeTypeKind tdKind))
  where
    -- Normalize a TypeKind by stripping namespaces from all RefNamed types
    normalizeTypeKind :: TypeKind -> TypeKind
    normalizeTypeKind = \case
      KindStruct fields -> KindStruct (map normalizeField fields)
      KindEnum disc variants -> KindEnum disc (map normalizeVariant variants)
      KindStringEnum vals -> KindStringEnum vals
      KindAlias target -> KindAlias (normalizeTypeRef target)
      KindPrimitive t f -> KindPrimitive t f

    normalizeField :: FieldDef -> FieldDef
    normalizeField fd = fd { fdType = normalizeTypeRef (fdType fd) }

    normalizeVariant :: VariantDef -> VariantDef
    normalizeVariant vd = vd { vdFields = map normalizeField (vdFields vd) }

    normalizeTypeRef :: TypeRef -> TypeRef
    normalizeTypeRef = \case
      RefNamed qn ->
        -- Strip namespace: keep only local name
        RefNamed qn { qnNamespace = "" }
      RefArray inner -> RefArray (normalizeTypeRef inner)
      RefOptional inner -> RefOptional (normalizeTypeRef inner)
      other -> other

-- | Select the canonical version from a group of duplicate types
-- Strategy: prefer parent namespace, then shortest namespace
selectCanonical :: [(Text, TypeDef)] -> (Text, TypeDef)
selectCanonical dups =
  case dups of
    [] -> error "selectCanonical: empty list"
    [single] -> single
    multiple -> minimumBy compareNamespacePreference multiple
  where
    -- Compare two (fullName, typedef) pairs
    compareNamespacePreference (_, td1) (_, td2) =
      let ns1 = tdNamespace td1
          ns2 = tdNamespace td2
          -- Check if one is parent of the other
          isParent n1 n2 = n2 `T.isPrefixOf` n1 && T.length n1 > T.length n2
      in case (isParent ns1 ns2, isParent ns2 ns1) of
           (True, False) -> GT  -- ns2 is parent of ns1, prefer ns2
           (False, True) -> LT  -- ns1 is parent of ns2, prefer ns1
           _ -> compare (T.length ns1) (T.length ns2)  -- Fallback: shortest wins

-- | Parse a qualified name from a full name string (e.g., "cone.UUID" -> QualifiedName "cone" "UUID")
parseQualifiedName :: Text -> Maybe QualifiedName
parseQualifiedName t =
  case T.breakOnEnd "." t of
    ("", _) -> Nothing  -- No dot found
    (ns, local) | T.null local -> Nothing  -- Ends with dot
                | otherwise -> Just QualifiedName
                    { qnNamespace = T.dropEnd 1 ns  -- Remove trailing dot
                    , qnLocalName = local
                    }

-- | Update all RefNamed references in a method using redirect map
updateMethodRefs :: Map Text Text -> MethodDef -> MethodDef
updateMethodRefs redirects md = md
  { mdParams = map (updateParamRefs redirects) (mdParams md)
  , mdReturns = updateTypeRef redirects (mdReturns md)
  , mdError = fmap (updateTypeRef redirects) (mdError md)
  , mdProgress = fmap (updateTypeRef redirects) (mdProgress md)
  , mdBidirType = fmap (updateTypeRef redirects) (mdBidirType md)
  , mdBidirResponseType = mdBidirResponseType md  -- Preserve as-is
  , mdBidirResponseSchema = mdBidirResponseSchema md  -- Preserve as-is
  -- mdRole: left unchanged via record-update syntax
  }

-- | Update type references in a parameter
updateParamRefs :: Map Text Text -> ParamDef -> ParamDef
updateParamRefs redirects pd = pd
  { pdType = updateTypeRef redirects (pdType pd)
  }

-- | Recursively update a TypeRef to use canonical names
updateTypeRef :: Map Text Text -> TypeRef -> TypeRef
updateTypeRef redirects = \case
  RefNamed qn ->
    let fullName = qualifiedNameFull qn
        canonicalName = Map.findWithDefault fullName fullName redirects
    in case parseQualifiedName canonicalName of
         Just qn' -> RefNamed qn'
         Nothing -> RefNamed qn  -- Fallback if parse fails
  RefArray inner ->
    RefArray (updateTypeRef redirects inner)
  RefOptional inner ->
    RefOptional (updateTypeRef redirects inner)
  other -> other

-- | Update all type references in a TypeDef
updateTypeDefRefs :: Map Text Text -> TypeDef -> TypeDef
updateTypeDefRefs redirects td = td { tdKind = updateTypeKind (tdKind td) }
  where
    updateTypeKind :: TypeKind -> TypeKind
    updateTypeKind = \case
      KindStruct fields ->
        KindStruct (map updateField fields)
      KindEnum disc variants ->
        KindEnum disc (map updateVariant variants)
      KindAlias target ->
        KindAlias (updateTypeRef redirects target)
      KindStringEnum vals ->
        KindStringEnum vals
      KindPrimitive t f ->
        KindPrimitive t f

    updateField :: FieldDef -> FieldDef
    updateField fd = fd { fdType = updateTypeRef redirects (fdType fd) }

    updateVariant :: VariantDef -> VariantDef
    updateVariant vd = vd { vdFields = map updateField (vdFields vd) }

-- ============================================================================
-- Extraction from Plugin
-- ============================================================================

-- | Types read so far in one namespace, each with where it was first read.
type Sourced = Map Text (TypeDef, TypeSource)

-- | Add one type. The same title with the same shape is one type (the first
--   reading is kept); the same title with another shape is a
--   'TitleCollision'.
insertSourced :: Sourced -> (Text, (TypeDef, TypeSource)) -> Either IRBuildError Sourced
insertSourced acc (key, new@(td, src)) = case Map.lookup key acc of
  Nothing -> Right (Map.insert key new acc)
  Just (td0, src0)
    | sameShape td0 td -> Right acc
    | otherwise -> Left TitleCollision
        { tcNamespace = tdNamespace td
        , tcTitle     = tdName td
        , tcFirst     = src0
        , tcSecond    = src
        }

-- | Two readings of a title are one type when they agree on everything but
--   their documentation.
sameShape :: TypeDef -> TypeDef -> Bool
sameShape a b = undocumented a == undocumented b
  where
    undocumented td = td { tdDescription = Nothing, tdKind = kind (tdKind td) }
    kind = \case
      KindStruct fields -> KindStruct (map field fields)
      KindEnum disc variants -> KindEnum disc (map variant variants)
      other -> other
    field fd = fd { fdDescription = Nothing }
    variant vd = vd { vdDescription = Nothing, vdFields = map field (vdFields vd) }

-- | Extract all types and methods from a plugin schema
-- Types are namespace-qualified to avoid collisions (e.g., "cone.ListResult")
extractFromPlugin
  :: Text -> Text -> PluginSchema
  -> Either IRBuildError (Map Text TypeDef, Map Text MethodDef)
extractFromPlugin namespace pathPrefix schema = do
  results <- traverse (extractMethodSourced namespace pathPrefix) (psMethods schema)
  types <- foldM (\acc (s, _) -> foldM insertSourced acc (Map.toList s)) Map.empty results
  pure
    ( Map.map fst types
    , Map.fromList [ (mdFullPath m, m) | (_, m) <- results ]
    )

-- | Extract types and method def from a single method
--
-- Fails when two of the method's own schemas put two shapes under one title.
extractMethodDef :: Text -> Text -> MethodSchema -> Either IRBuildError (Map Text TypeDef, MethodDef)
extractMethodDef namespace pathPrefix method = do
  (types, mdef) <- extractMethodSourced namespace pathPrefix method
  pure (Map.map fst types, mdef)

extractMethodSourced :: Text -> Text -> MethodSchema -> Either IRBuildError (Sourced, MethodDef)
extractMethodSourced namespace pathPrefix method = do
  let name = methodName method
      fullPath = if T.null pathPrefix
                 then namespace <> "." <> name
                 else pathPrefix <> "." <> name

      -- Extract types from params (namespace-qualified).
      -- Also pass methodParamSchemas so per-param deprecation info
      -- (IR-14) flows from ParamSchema.paramDeprecation onto the IR
      -- ParamDef.pdDeprecation.
      (paramTypes, params) =
        extractParams namespace (methodParams method) (methodParamSchemas method)

      -- The contract (plexus 0.7): item, error and progress types, each its
      -- own JSON Schema document. Cardinality is read, not inferred.
      (returnTypes, returnRef) = extractContractType namespace (methodReturns method)
      errorPart    = extractContractType namespace <$> methodError method
      progressPart = extractContractType namespace <$> methodProgress method

      from schema types = [ (tdFullName td, (td, TypeSource name schema)) | td <- types ]

  types <- foldM insertSourced Map.empty $ concat
    [ from "params" paramTypes
    , from "returns" returnTypes
    , maybe [] (from "error" . fst) errorPart
    , maybe [] (from "progress" . fst) progressPart
    ]

  let -- Detect bidirectional type parameter T.
      --
      -- When the schema reports bidirectional: true we know the method uses a
      -- BidirChannel.  The 'request_type' field (if present) holds the JSON
      -- Schema for T.  We currently emit:
      --   - Nothing           → not bidirectional
      --   - Just RefAny       → bidirectional with T=Value (StandardBidirChannel,
      --                         the default case; request_type is the StandardRequest schema)
      --   - Just (RefNamed …) → bidirectional with a specific named T
      --                         (future: when request_type references a named type)
      --
      -- NOTE: The substrate schema as of this implementation always uses
      -- StandardBidirChannel (T=Value), so mdBidirType is always Nothing or
      -- Just RefAny.  A future change to MethodSchema / hub-macro that emits a
      -- structured "bidir_type" field (distinct from the full request_type schema)
      -- should be handled here.
      bidirTypeRef = inferBidirType method
      bidirResponseSchema = methodResponseType method

      mdef = MethodDef
        { mdName = name
        , mdFullPath = fullPath
        , mdNamespace = namespace
        , mdDescription = Just (methodDescription method)
        , mdParams = params
        , mdReturns = returnRef
        , mdError = snd <$> errorPart
        , mdProgress = snd <$> progressPart
        , mdCardinality = methodCardinality method
        , mdBidirType = bidirTypeRef
        , mdBidirResponseType = Nothing  -- TODO: could extract from response_type title
        , mdBidirResponseSchema = bidirResponseSchema
        , mdRole = methodRole method
          -- IR-12: lift MethodSchema.role onto the IR so hub-codegen's
          -- typed-handle codegen (IR-9) sees DynamicChild/StaticChild
          -- tags. plexus-protocol defaults this to MethodRoleRpc when
          -- the upstream schema omits the field, so pre-IR servers
          -- continue to work with no change in behaviour.
        }
  pure (types, mdef)

-- | Infer the bidirectional type parameter from a MethodSchema.
--
-- Returns:
--   Nothing    – method is not bidirectional
--   Just RefAny – method is bidirectional with default T=Value (StandardBidirChannel)
--   Just tr    – method is bidirectional with specific T type (future)
inferBidirType :: MethodSchema -> Maybe TypeRef
inferBidirType method
  | not (methodBidirectional method) = Nothing
  -- Method is bidirectional.  Inspect request_type to determine T.
  | otherwise = case methodRequestType method of
      Nothing ->
        -- bidirectional: true but no request_type schema → treat as T=Value
        Just RefAny
      Just _ ->
        -- request_type is present.  For now we always emit RefAny (T=Value)
        -- because the schema emits the full StandardRequest schema rather than
        -- a dedicated "bidir_type" field identifying T.
        --
        -- TODO: When the hub-macro is extended to emit a structured
        -- "bidir_type": { "$ref": "#/$defs/MyType" } field in the schema JSON,
        -- parse it here with schemaToTypeRef and return the resulting TypeRef.
        Just RefAny

-- ============================================================================
-- Parameter Extraction
-- ============================================================================

-- | Extract types and param defs from method params schema.
--
-- The third argument carries optional per-parameter metadata
-- ('ParamSchema') emitted by IR-5 producers. When present, each entry's
-- 'paramDeprecation' is lifted onto the resulting 'ParamDef.pdDeprecation'
-- by matching parameter name. Absent entries (pre-IR-5 producers, or
-- non-deprecated params) leave 'pdDeprecation' as 'Nothing'.
-- Types are namespace-qualified to avoid collisions.
extractParams
  :: Text
  -> Maybe Value
  -> Maybe [ParamSchema]
  -> ([TypeDef], [ParamDef])
extractParams _ Nothing _ = ([], [])
extractParams namespace (Just val) paramSchemas = case val of
  Object o -> extractParamsFromObject (docOf namespace val) o paramSchemas
  _ -> ([], [])

extractParamsFromObject
  :: Doc
  -> KM.KeyMap Value
  -> Maybe [ParamSchema]
  -> ([TypeDef], [ParamDef])
extractParamsFromObject doc o paramSchemas =
  let -- Extract $defs (namespace-qualified)
      defs = defsTypeDefs doc

      -- Extract properties
      props = case KM.lookup "properties" o of
        Just (Object p) -> KM.toList p
        _ -> []

      -- Get required list
      required = requiredOf o

      -- Look up per-parameter deprecation by name from the structured
      -- ParamSchema list (IR-5). 'Nothing' when either the list is absent
      -- or the named parameter isn't in it.
      lookupDeprecation :: Text -> Maybe DeprecationInfo
      lookupDeprecation pname = case paramSchemas of
        Nothing -> Nothing
        Just xs -> case filter ((== pname) . paramName) xs of
          (ps:_) -> paramDeprecation ps
          []     -> Nothing

      -- REQ-6/REQ-9: extract @x-plexus-source@ from each property's schema.
      -- Carries where the param value comes from (auth, cookie, header,
      -- query, derived, rpc) so downstream renderers and codegen can
      -- emit per-param annotations. Nothing means the schema had no
      -- extension — treated as RPC.
      extractSource :: Value -> Maybe Value
      extractSource propSchema = case propSchema of
        Object po -> KM.lookup "x-plexus-source" po
        _         -> Nothing

      -- Build param defs
      params =
        [ ParamDef
            { pdName = K.toText k
            , pdType = typeRefIn doc v
            , pdDescription = extractDescription v
            , pdRequired = K.toText k `elem` required
            , pdDefault = extractDefault v
            , pdDeprecation = lookupDeprecation (K.toText k)
            , pdSource = extractSource v
            }
        | (k, v) <- props
        ]
  in (defs, params)

-- ============================================================================
-- Contract Types: returns · error · progress
-- ============================================================================

-- | One of a method's contract schemas (its @returns@, @error@ or
--   @progress@) — a whole JSON Schema document — as the 'TypeDef's it
--   defines and the 'TypeRef' the method holds.
--
--   * A titled struct, tagged enum, string enum or untagged union is a
--     'TypeDef' under its title, and the ref is 'RefNamed' to it.
--   * A @$ref@ is a 'RefNamed' to its target; under a title of its own, an
--     alias 'TypeDef' by that title.
--   * Anything else — a primitive, an array, a nullable, @{}@ — is the
--     'TypeRef' 'schemaToTypeRef' gives it, whatever its title says
--     (schemars titles @String@ "String" and @Vec<T>@ "Array_of_T").
--
--   Every entry of the document's @$defs@ is a 'TypeDef' too, so no
--   'RefNamed' this returns dangles.
extractContractType :: Text -> Value -> ([TypeDef], TypeRef)
extractContractType namespace val =
  let doc = docOf namespace val
      (rootTypes, ref) = rootTypeRef doc val
  in (defsTypeDefs doc ++ rootTypes, ref)

rootTypeRef :: Doc -> Value -> ([TypeDef], TypeRef)
rootTypeRef doc val@(Object o)
  | Just title <- titleOf o, isNamedShape o =
      ( [TypeDef title (docNamespace doc) (extractDescription val) (inferTypeKind doc o)]
      , namedIn doc title )
  | Just (String ref) <- KM.lookup "$ref" o, not (KM.member "properties" o) =
      let target = refTypeRef doc ref
      in case titleOf o of
           Just title | typeRefName target /= Just (qualifiedNameFull (QualifiedName (docNamespace doc) title)) ->
             ( [TypeDef title (docNamespace doc) (extractDescription val) (KindAlias target)]
             , namedIn doc title )
           _ -> ([], target)
rootTypeRef doc val = ([], typeRefIn doc val)

-- | Does this schema describe a type a generator names: a struct, a tagged
--   or string enum, or an untagged union (not a nullable wrapper)?
isNamedShape :: KM.KeyMap Value -> Bool
isNamedShape o =
  KM.member "properties" o
    || KM.member "enum" o
    || maybe False (not . isJust . nullableMember) (unionMembers o)

-- ============================================================================
-- Schema Documents
-- ============================================================================

-- | The JSON Schema document a type is read from — one of a method's
--   params, returns, error or progress schemas. Its @$ref@s resolve against
--   its own @$defs@.
data Doc = Doc
  { docNamespace :: Text
  , docDefs      :: KM.KeyMap Value
  , docRoot      :: Maybe Text  -- ^ The root's title: what @"$ref": "#"@ names
  }

docOf :: Text -> Value -> Doc
docOf namespace (Object o) = Doc namespace (defsOf o) (titleOf o)
docOf namespace _ = Doc namespace KM.empty Nothing

-- | @$defs@, or draft-07's @definitions@.
defsOf :: KM.KeyMap Value -> KM.KeyMap Value
defsOf o = case KM.lookup "$defs" o of
  Just (Object d) -> d
  _ -> case KM.lookup "definitions" o of
    Just (Object d) -> d
    _ -> KM.empty

titleOf :: KM.KeyMap Value -> Maybe Text
titleOf o = case KM.lookup "title" o of
  Just (String t) | not (T.null t) -> Just t
  _ -> Nothing

requiredOf :: KM.KeyMap Value -> [Text]
requiredOf o = case KM.lookup "required" o of
  Just (Array arr) -> [t | String t <- V.toList arr]
  _ -> []

namedIn :: Doc -> Text -> TypeRef
namedIn doc name = RefNamed QualifiedName
  { qnNamespace = docNamespace doc
  , qnLocalName = name
  }

-- | A @$ref@ as a 'TypeRef'. @"#"@ is the document's root (by its title).
refTypeRef :: Doc -> Text -> TypeRef
refTypeRef doc "#" = maybe RefAny (namedIn doc) (docRoot doc)
refTypeRef doc ref = namedIn doc (extractRefName ref)

-- | The schema a @#/$defs/Name@ ref points at, in this document.
lookupDef :: Doc -> Text -> Maybe Value
lookupDef _ "#" = Nothing
lookupDef doc ref = KM.lookup (K.fromText (extractRefName ref)) (docDefs doc)

-- | Every @$defs@ entry of the document, as a 'TypeDef' named by its key.
defsTypeDefs :: Doc -> [TypeDef]
defsTypeDefs doc =
  [ TypeDef (K.toText k) (docNamespace doc) (extractDescription v) (inferTypeKind doc o)
  | (k, v@(Object o)) <- KM.toList (docDefs doc)
  ]

-- ============================================================================
-- Type Kinds
-- ============================================================================

-- | The kind of a named type, from its JSON Schema.
--
--   A union (@oneOf@ / @anyOf@) is a nullable alias, a string enum, or a
--   tagged enum whose tag is the property every variant carries a @const@
--   for. A union with no such tag (serde's untagged and externally tagged
--   enums), and a struct with an enum flattened into it, have no IR form:
--   they are an alias to 'RefAny', never a partial enum.
inferTypeKind :: Doc -> KM.KeyMap Value -> TypeKind
inferTypeKind doc o
  -- A struct with a flattened enum (properties beside a union): no IR form
  | Just _ <- unionMembers o, KM.member "properties" o = KindAlias RefAny

  -- oneOf / anyOf
  | Just members <- unionMembers o = unionKind doc members

  -- Check for enum (simple string enum)
  | Just (Array values) <- KM.lookup "enum" o =
      let stringValues = [ asText v | v <- V.toList values ]
      in KindStringEnum stringValues

  -- Check for object with properties (struct)
  | KM.member "properties" o =
      maybe (KindAlias RefAny) KindStruct (objectFields doc maxDepth o)

  -- A reference to another type
  | Just (String ref) <- KM.lookup "$ref" o = KindAlias (refTypeRef doc ref)

  -- Check for primitive types
  | Just typeVal <- KM.lookup "type" o =
      case typeVal of
        String t -> KindPrimitive t (extractFormat o)
        Array ts ->
          -- Nullable type like ["string", "null"]
          let nonNull = [t | String t <- V.toList ts, t /= "null"]
          in case nonNull of
               [t] -> KindPrimitive t (extractFormat o)
               _ -> KindPrimitive "any" Nothing
        _ -> KindPrimitive "any" Nothing

  -- Default to unknown
  | otherwise = KindPrimitive "any" Nothing

  where
    asText (String s) = s
    asText _ = "unknown"

-- | The members of a @oneOf@ (or else an @anyOf@).
unionMembers :: KM.KeyMap Value -> Maybe [Value]
unionMembers o = case KM.lookup "oneOf" o <|> KM.lookup "anyOf" o of
  Just (Array members) -> Just (V.toList members)
  _ -> Nothing

-- | The kind of a union.
unionKind :: Doc -> [Value] -> TypeKind
unionKind doc members
  | Just inner <- nullableMember members = KindAlias (RefOptional (typeRefIn doc inner))
  | Just values <- stringEnumValues members = KindStringEnum values
  | Just (disc, variants) <- taggedVariants doc members = KindEnum disc variants
  | otherwise = KindAlias RefAny

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
--   of type string): the literals.
stringEnumValues :: [Value] -> Maybe [Text]
stringEnumValues [] = Nothing
stringEnumValues members = concat <$> traverse literals members
  where
    literals (Object m)
      | stringTyped m, Just (String c) <- KM.lookup "const" m = Just [c]
      | stringTyped m, Just (Array vs) <- KM.lookup "enum" m
      , not (V.null vs) = traverse asString (V.toList vs)
    literals _ = Nothing
    stringTyped m = case KM.lookup "type" m of
      Nothing -> True
      Just (String "string") -> True
      _ -> False
    asString (String s) = Just s
    asString _ = Nothing

-- ============================================================================
-- Tagged Enums
-- ============================================================================

-- | One variant of a union, its references resolved.
data VariantShape = VariantShape
  { vsProps    :: [(Text, Value)]      -- ^ Its own properties, the tag among them
  , vsRequired :: [Text]               -- ^ Its @required@
  , vsWrapped  :: Maybe [FieldDef]     -- ^ The fields of the struct(s) it wraps (a newtype variant); 'Nothing' when it wraps something that has no fields to inline
  , vsDesc     :: Maybe Text
  }

-- | A tagged enum's tag and variants; 'Nothing' when the union is not one.
taggedVariants :: Doc -> [Value] -> Maybe (Text, [VariantDef])
taggedVariants doc members = do
  shapes <- traverse (variantShape doc maxDepth) members
  disc <- discriminatorOf shapes
  variants <- traverse (toVariant disc) shapes
  pure (disc, variants)
  where
    toVariant disc s = do
      name <- lookup disc (variantConsts s)
      wrapped <- vsWrapped s
      let own =
            [ FieldDef
                { fdName = k
                , fdType = typeRefIn doc v
                , fdDescription = extractDescription v
                , fdRequired = k `elem` vsRequired s
                , fdDefault = extractDefault v
                }
            | (k, v) <- vsProps s
            , k /= disc
            ]
      pure VariantDef
        { vdName = name
        , vdDescription = vsDesc s
        , vdFields = uniqueFields (own ++ wrapped)
        }

-- | A union member as a variant: an object schema, or a @$ref@ to one.
variantShape :: Doc -> Int -> Value -> Maybe VariantShape
variantShape doc depth member@(Object m)
  | Just (String ref) <- KM.lookup "$ref" m
  , not (KM.member "properties" m)
  , depth > 0 = do
      target <- lookupDef doc ref
      shape <- variantShape doc (depth - 1) target
      pure shape { vsDesc = extractDescription member <|> vsDesc shape }
  | Just (Object props) <- KM.lookup "properties" m = Just VariantShape
      { vsProps    = [ (K.toText k, v) | (k, v) <- KM.toList props ]
      , vsRequired = requiredOf m
      , vsWrapped  = wrappedFields doc depth m
      , vsDesc     = extractDescription member
      }
variantShape _ _ _ = Nothing

-- | The string @const@s a variant gives its own properties
--   (a one-value @enum@ counts, as draft-07 tooling writes it).
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

-- | The tag: the property every variant gives a @const@, with no two
--   variants giving it the same one. Should several qualify, the one that
--   comes first in the first variant's @required@ (schemars lists the tag
--   first).
discriminatorOf :: [VariantShape] -> Maybe Text
discriminatorOf [] = Nothing
discriminatorOf shapes@(first:_) =
  case sortOn rank candidates of
    [] -> Nothing
    (k:_) -> Just k
  where
    consts = map variantConsts shapes
    candidates =
      [ k
      | (k, _) <- variantConsts first
      , Just values <- [traverse (lookup k) consts]
      , length (nub values) == length values
      ]
    rank k = fromMaybe maxBound (lookup k (zip (vsRequired first) [0 :: Int ..]))

-- ============================================================================
-- Struct Fields
-- ============================================================================

-- | How deep a chain of @$ref@s is followed (a guard against cycles).
maxDepth :: Int
maxDepth = 8

-- | A struct's fields: its own properties (required as @required@ says),
--   then the fields of any struct it wraps by @$ref@ or @allOf@. 'Nothing'
--   when it wraps something with no fields to inline.
objectFields :: Doc -> Int -> KM.KeyMap Value -> Maybe [FieldDef]
objectFields doc depth o = do
  wrapped <- wrappedFields doc depth o
  let required = requiredOf o
      own =
        [ FieldDef
            { fdName = K.toText k
            , fdType = typeRefIn doc v
            , fdDescription = extractDescription v
            , fdRequired = K.toText k `elem` required
            , fdDefault = extractDefault v
            }
        | Just (Object props) <- [KM.lookup "properties" o]
        , (k, v) <- KM.toList props
        ]
  pure (uniqueFields (own ++ wrapped))

-- | The fields an object schema takes from the structs it wraps: its
--   @$ref@ (2020-12 writes a newtype variant's struct beside its own
--   properties) and each @allOf@ member.
wrappedFields :: Doc -> Int -> KM.KeyMap Value -> Maybe [FieldDef]
wrappedFields doc depth o = concat <$> sequence (refPart ++ allOfParts)
  where
    refPart =
      [ refFields ref
      | Just (String ref) <- [KM.lookup "$ref" o]
      ]
    allOfParts =
      [ memberFields doc (depth - 1) member
      | Just (Array members) <- [KM.lookup "allOf" o]
      , member <- V.toList members
      ]
    refFields ref
      | depth <= 0 = Nothing
      | otherwise = lookupDef doc ref >>= memberFields doc (depth - 1)

-- | The fields of a schema that is a struct: a @$ref@ to one, an object with
--   properties, or an empty object (@{"type": "object"}@, an empty struct).
memberFields :: Doc -> Int -> Value -> Maybe [FieldDef]
memberFields doc depth (Object m)
  | depth < 0 = Nothing
  | isJust (unionMembers m) = Nothing
  | Just (String ref) <- KM.lookup "$ref" m, not (KM.member "properties" m) =
      if depth <= 0 then Nothing else lookupDef doc ref >>= memberFields doc (depth - 1)
  | KM.member "properties" m = objectFields doc depth m
  | KM.lookup "type" m == Just (String "object")
  , not (KM.member "additionalProperties" m) = Just []
memberFields _ _ _ = Nothing

-- | Fields by name, the first of each.
uniqueFields :: [FieldDef] -> [FieldDef]
uniqueFields = nubBy (\a b -> fdName a == fdName b)

-- ============================================================================
-- Schema to TypeRef Conversion
-- ============================================================================

-- | Convert a JSON Schema value to a TypeRef
--
-- Distinguishes between:
-- - RefAny: Schema present but no type constraints (intentionally dynamic, e.g. serde_json::Value)
-- - RefUnknown: No schema at all (schema gap, should warn)
--
-- Type references via $ref are namespace-qualified (e.g., "cone.ConeInfo")
schemaToTypeRef :: Text -> Value -> TypeRef
schemaToTypeRef namespace = typeRefIn (Doc namespace KM.empty Nothing)

-- | 'schemaToTypeRef' within a document (so @"$ref": "#"@ resolves).
typeRefIn :: Doc -> Value -> TypeRef
typeRefIn doc (Object o)
  -- Check for $ref - namespace-qualify the reference
  | Just (String ref) <- KM.lookup "$ref" o = refTypeRef doc ref

  -- Check for array
  | Just (String "array") <- KM.lookup "type" o =
      case KM.lookup "items" o of
        Just items -> RefArray (typeRefIn doc items)
        Nothing -> RefArray RefAny  -- array without items = any[]

  -- Check for nullable
  | Just (Array types) <- KM.lookup "type" o =
      let nonNull = [t | t@(String s) <- V.toList types, s /= "null"]
          hasNull = any (\case String "null" -> True; _ -> False) (V.toList types)
      in case nonNull of
           [String "array"] ->
             -- Nullable array: type: ["array", "null"] with items
             let inner = case KM.lookup "items" o of
                           Just items -> RefArray (typeRefIn doc items)
                           Nothing -> RefArray RefAny
             in if hasNull then RefOptional inner else inner
           [String t] ->
             let base = RefPrimitive t (extractFormat o)
             in if hasNull then RefOptional base else base
           _ -> RefAny  -- Multiple non-null types = any

  -- anyOf [X, null] (schemars' Option<X>) is an optional X
  | Just members <- unionMembers o
  , Just inner <- nullableMember members = RefOptional (typeRefIn doc inner)

  -- Any other anyOf is an untagged union, which a ref cannot say more of
  -- than any
  | Just (Array _) <- KM.lookup "anyOf" o = RefAny

  -- allOf [X] (a described $ref, as draft-07 tooling writes it) is X
  | Just (Array members) <- KM.lookup "allOf" o
  , [single] <- V.toList members = typeRefIn doc single

  -- Check for primitive type
  | Just (String t) <- KM.lookup "type" o =
      RefPrimitive t (extractFormat o)

  -- Schema object present but no type constraint = intentionally dynamic (RefAny)
  -- This happens with serde_json::Value which emits {"description": "...", "default": null}
  | otherwise = RefAny

-- JSON Schema `true` is the "accept anything" schema - intentionally dynamic
-- This is used when schemars emits a field like `input: true` for serde_json::Value
typeRefIn _ (Bool True) = RefAny

-- Null, false, or non-JSON-Schema values = schema gap (should warn)
typeRefIn _ _ = RefUnknown

-- | Extract ref name from a $ref string like "#/$defs/Position"
extractRefName :: Text -> Text
extractRefName ref = case T.splitOn "/" ref of
  parts | not (null parts) -> last parts
  _ -> ref

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Extract description from a schema
extractDescription :: Value -> Maybe Text
extractDescription (Object o) = case KM.lookup "description" o of
  Just (String s) -> Just s
  _ -> Nothing
extractDescription _ = Nothing

-- | Extract default value from a schema
extractDefault :: Value -> Maybe Value
extractDefault (Object o) = KM.lookup "default" o
extractDefault _ = Nothing

-- | Extract format from a schema object
extractFormat :: KM.KeyMap Value -> Maybe Text
extractFormat o = case KM.lookup "format" o of
  Just (String s) -> Just s
  _ -> Nothing

-- | Extract types from a full schema (for standalone use)
-- Types are namespace-qualified
extractTypesFromSchema :: Text -> Value -> Map Text TypeDef
extractTypesFromSchema namespace val@(Object _) =
  Map.fromList [ (tdFullName td, td) | td <- defsTypeDefs (docOf namespace val) ]
extractTypesFromSchema _ _ = Map.empty
