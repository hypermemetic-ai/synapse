{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | IR Builder - walks schema tree and constructs the IR
--
-- Uses the existing schema walker with a custom algebra that:
-- 1. Reads types from $defs in methodParams, methodReturns and methodInput
--    — whole, or refused by name (SYNAPSE-CC-8: "Synapse.IR.Read")
-- 2. Deduplicates types by content hash, placing each shared type in the
--    namespace that refers to it most (SYNAPSE-CC-8)
-- 3. Infers streaming from return type structure
-- 4. Builds method definitions with type references; server-injected params
--    go to 'mdInjected', never 'mdParams'
module Synapse.IR.Builder
  ( -- * Building IR
    buildIR
  , buildIRRaw
  , finalizeIR

    -- * Extraction (for testing)
  , extractTypesFromSchema
  , extractMethodDef
  , extractMethodDefR
  , schemaToTypeRef
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (minimumBy, nub, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Synapse.Schema.Types
import Synapse.Schema.Functor (SchemaF(..))
import Synapse.Algebra.Walk (walkSchemaPar)
import Synapse.Monad
import Synapse.IR.Types hiding (QualifiedName(..), qualifiedNameFull)
import Synapse.IR.Types (QualifiedName(..), qualifiedNameFull, synapseVersion)
import Synapse.IR.Read
import Plexus.Schema.Recursive (DeprecationInfo, ParamSchema(..), paramValues)

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

  pure $ finalizeIR raw
    { irBackend = backend
    , irMetadata = Just metadata
    }

-- | The walk's IR before 'finalizeIR' (for the bench: its phases apart).
buildIRRaw :: Path -> SynapseM IR
buildIRRaw = walkSchemaPar irAlgebra

-- | SYNAPSE-CC-8: the walk's IR made whole — shared types deduplicated and
--   placed where they are referred to most, every ref that names no type
--   turned to 'RefUnknown' (and refused, unless the type it names was), the
--   refusals deduplicated and sorted by site.
finalizeIR :: IR -> IR
finalizeIR raw =
  let placed = deduplicateTypes raw
      refusedTypes = Set.fromList (map rfSite (irRefusals placed))
      (fixed, dangling) = unknownWhereUnnamed refusedTypes placed
  in fixed { irRefusals = sortOn rfSite (nub (irRefusals placed ++ dangling)) }

-- | Algebra for building IR from schema tree
--
-- At each node:
-- - Extract types from all methods' params and returns
-- - Build method definitions
-- - Merge with child results
irAlgebra :: SchemaF IR -> SynapseM IR
irAlgebra (PluginF schema path childIRs) = do
  -- Use full path as namespace to avoid collisions
  -- e.g., "hyperforge.workspace.repos" instead of just "repos"
  let namespace = T.intercalate "." path
      pathPrefix = namespace  -- Same as namespace

  -- Extract types and methods from this plugin
  let (localTypes, localMethods, localRefusals) = extractFromPlugin namespace pathPrefix schema

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

  -- REQ-5: capture per-plugin PlexusRequest schema (psRequest from wire schema).
  -- HEADERS-1 stub: plexus-protocol's PluginSchema does not yet expose
  -- psRequest (cross-repo patch pending). Treat as always-Nothing so the
  -- synapse binary builds; REQ-5 rendering becomes a no-op until the field
  -- lands in plexus-protocol.
  let _ = schema  -- suppress unused-binding warning when REQ-5 is stubbed
      childRequests = fromMaybe Map.empty (irPluginRequests childIR)
      allRequests = childRequests

  pure $ IR
    { irVersion = irVersion emptyIR  -- Use version from emptyIR
    , irBackend = irBackend emptyIR  -- Will be set by buildIR
    , irHash = thisHash
    , irMetadata = irMetadata childIR  -- Preserve metadata from child
    , irTypes = Map.union localTypes (irTypes childIR)  -- Local wins on conflict
    , irMethods = Map.union localMethods (irMethods childIR)
    , irPlugins = Map.insert namespace pluginMethods (irPlugins childIR)
    , irPluginHashes = Just allHashes  -- V2: Store hash info per plugin
    , irPluginRequests = if Map.null allRequests then Nothing else Just allRequests
      -- AUTH-9: this plugin's default roles, beside its children's.
    , irPluginRoles = case (psRoles schema, irPluginRoles childIR) of
        (Nothing, childRoles) -> childRoles
        (Just roles, childRoles) -> Just (Map.insert namespace roles (fromMaybe Map.empty childRoles))
    , irRefusals = localRefusals <> irRefusals childIR
    }

irAlgebra (MethodF method namespace path) = do
  -- Single method node (shouldn't happen in normal walk, but handle it)
  let fullPath = T.intercalate "." path
  let ((types, mdef), refusals) = extractMethodDefR namespace fullPath method
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
    , irPluginRoles = Nothing  -- Methods don't carry roles
    , irRefusals = refusals
    }

-- ============================================================================
-- Type Deduplication and Placement
-- ============================================================================

-- | Deduplicate types by content hash
--
-- When multiple namespaces define identical types (e.g., solar.SolarEvent and
-- jupiter.SolarEvent), we deduplicate them by keeping one canonical version.
--
-- Strategy:
-- 1. Hash each TypeDef by its structure (name + kind, ignoring namespace)
-- 2. Group duplicates
-- 3. Pick canonical: the namespace that refers to it most (SYNAPSE-CC-8)
-- 4. Update all RefNamed references to point to canonical qualified name
deduplicateTypes :: IR -> IR
deduplicateTypes ir =
  let -- Group types by their content hash
      typesByHash = Map.fromListWith (++)
        [ (hashTypeStructure td, [(fullName, td)])
        | (fullName, td) <- Map.toList (irTypes ir)
        ]

      -- only types with copies in several namespaces need their references counted
      contested = Set.fromList
        [ (tdNamespace td, tdName td) | group <- Map.elems typesByHash, length group > 1, (_, td) <- group ]
      counts = refCounts contested ir

      -- Pick canonical version for each group
      canonical = Map.map (selectCanonical counts) typesByHash

      -- Build redirect map: old qualified name -> canonical qualified name
      redirects = Map.fromList
        [ (oldName, canonicalName)
        | (hash, group) <- Map.toList typesByHash
        , let canonicalName = fst (canonical Map.! hash)
        , (oldName, _) <- group
        , oldName /= canonicalName
        ]

      -- Keep only canonical types and update their internal type references
      dedupedTypesWithUpdatedRefs = Map.fromList
        [ (qualName, updateTypeDefRefs redirects td)
        | (qualName, td) <- Map.elems canonical
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
    normalizeTypeKind = mapKindRefs normalizeTypeRef
    normalizeTypeRef = mapRef $ \case
      RefNamed qn -> RefNamed qn { qnNamespace = "" }
      other -> other

-- | SYNAPSE-CC-8: how often each type is named, by full name — in every
--   method's params, returns, input and bidirectional type, and every type's
--   fields, before deduplication. A namespace names only its own copy, so a
--   copy's count is how much its namespace refers to it.
refCounts :: Set.Set (Text, Text) -> IR -> Map (Text, Text) Int
refCounts contested ir
  | Set.null contested = Map.empty
  | otherwise = Map.fromListWith (+) [ (n, 1 :: Int) | n <- names, Set.member n contested ]
  where
    names = concatMap methodNames (Map.elems (irMethods ir))
         ++ concatMap (kindNames . tdKind) (Map.elems (irTypes ir))
    methodNames md = concatMap refKeys
      (map pdType (mdParams md) ++ [mdReturns md] ++ maybe [] pure (mdInput md) ++ maybe [] pure (mdBidirType md))
    kindNames = \case
      KindStruct fs -> concatMap (refKeys . fdType) fs
      KindEnum _ vs _ -> concatMap (\v -> concatMap (refKeys . fdType) (vdFields v) ++ maybe [] refKeys (vdRef v)) vs
      KindAlias t -> refKeys t
      KindStringEnum _ -> []
      KindPrimitive _ _ -> []
    refKeys = \case
      RefNamed qn -> [(qnNamespace qn, qnLocalName qn)]
      t -> concatMap refKeys (typeRefChildren t)

-- | A name as 'irTypes' keys it (the root's types are @.Name@).
qualifiedNameFull' :: QualifiedName -> Text
qualifiedNameFull' qn = qnNamespace qn <> "." <> qnLocalName qn

-- | Select the canonical version from a group of duplicate types
-- (SYNAPSE-CC-8): the copy in the namespace that refers to it most. A tie
-- goes to the namespace that is an ancestor of every other tied one, then
-- the shortest name, then alphabetical order. The protocol root @""@ is
-- never chosen when a generated namespace has a copy.
selectCanonical :: Map (Text, Text) Int -> [(Text, TypeDef)] -> (Text, TypeDef)
selectCanonical counts dups =
  case dups of
    [] -> error "selectCanonical: empty list"
    [single] -> single
    multiple ->
      let generated = filter ((/= "") . tdNamespace . snd) multiple
          candidates = if null generated then multiple else generated
          countOf (_, td) = Map.findWithDefault 0 (tdNamespace td, tdName td) counts
          best = maximum (map countOf candidates)
          tied = filter ((== best) . countOf) candidates
          nsOf = tdNamespace . snd
          ancestorOfAll c = all (\o -> nsOf o == nsOf c || (nsOf c <> ".") `T.isPrefixOf` nsOf o) tied
      in case filter ancestorOfAll tied of
           (c : _) -> c
           [] -> minimumBy (comparing (\c -> (T.length (nsOf c), nsOf c))) tied

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
  , mdBidirType = fmap (updateTypeRef redirects) (mdBidirType md)
  , mdInput = fmap (updateTypeRef redirects) (mdInput md)
  -- mdRole, mdBidirResponse*: left unchanged via record-update syntax
  }

-- | Update type references in a parameter
updateParamRefs :: Map Text Text -> ParamDef -> ParamDef
updateParamRefs redirects pd = pd
  { pdType = updateTypeRef redirects (pdType pd)
  }

-- | Recursively update a TypeRef to use canonical names
updateTypeRef :: Map Text Text -> TypeRef -> TypeRef
updateTypeRef redirects = mapRef $ \case
  RefNamed qn ->
    let fullName = qualifiedNameFull' qn
        canonicalName = Map.findWithDefault fullName fullName redirects
    in case parseQualifiedName canonicalName of
         Just qn' -> RefNamed qn'
         Nothing -> RefNamed qn  -- Fallback if parse fails
  other -> other

-- | Update all type references in a TypeDef
updateTypeDefRefs :: Map Text Text -> TypeDef -> TypeDef
updateTypeDefRefs redirects td = td { tdKind = mapKindRefs (updateTypeRef redirects) (tdKind td) }

-- | Apply a function to every ref, bottom up.
mapRef :: (TypeRef -> TypeRef) -> TypeRef -> TypeRef
mapRef f = go
  where
    go = \case
      RefArray t -> f (RefArray (go t))
      RefOptional t -> f (RefOptional (go t))
      RefMap t -> f (RefMap (go t))
      RefTuple ts -> f (RefTuple (map go ts))
      RefRefined t r -> f (RefRefined (go t) r)
      other -> f other

-- | Apply a ref transformation to every ref a kind holds.
mapKindRefs :: (TypeRef -> TypeRef) -> TypeKind -> TypeKind
mapKindRefs g = \case
  KindStruct fields -> KindStruct (map field fields)
  KindEnum disc variants repr -> KindEnum disc (map variant variants) repr
  KindAlias target -> KindAlias (g target)
  KindStringEnum vals -> KindStringEnum vals
  KindPrimitive t f -> KindPrimitive t f
  where
    field fd = fd { fdType = g (fdType fd) }
    variant vd = vd { vdFields = map field (vdFields vd), vdRef = fmap g (vdRef vd) }

-- ============================================================================
-- Refs that name no type
-- ============================================================================

-- | SYNAPSE-CC-8: every 'RefNamed' that names no type becomes 'RefUnknown'.
--   One that names a type the reader refused needs nothing more (the type's
--   refusal names it); any other is refused at its own site.
unknownWhereUnnamed :: Set.Set Text -> IR -> (IR, [Refusal])
unknownWhereUnnamed refused ir =
  let known = Map.keysSet (irTypes ir)
      fix site t = case t of
        RefNamed qn
          | Set.member (qualifiedNameFull' qn) known -> (t, [])
          | Set.member (qualifiedNameFull' qn) refused -> (RefUnknown, [])
          | otherwise -> (RefUnknown, [Refusal site "$ref" Nothing ("names " <> qualifiedNameFull' qn <> ", which no $defs defines")])
        RefArray i -> let (i', rs) = fix (site <> "[]") i in (RefArray i', rs)
        RefOptional i -> let (i', rs) = fix (site <> "?") i in (RefOptional i', rs)
        RefMap i -> let (i', rs) = fix (site <> "{}") i in (RefMap i', rs)
        RefRefined i r -> let (i', rs) = fix site i in (RefRefined i' r, rs)
        RefTuple ts ->
          let rs' = zipWith (\n x -> fix (site <> "." <> T.pack (show n)) x) [0 :: Int ..] ts
          in (RefTuple (map fst rs'), concatMap snd rs')
        _ -> (t, [])
      fixField site fd = let (t, rs) = fix (site <> "." <> fdName fd) (fdType fd) in (fd { fdType = t }, rs)
      fixKind site = \case
        KindStruct fs -> let r = map (fixField site) fs in (KindStruct (map fst r), concatMap snd r)
        KindEnum d vs repr ->
          let fixV v =
                let vsite = site <> "." <> vdName v
                    fr = map (fixField vsite) (vdFields v)
                    (ref', rr) = case vdRef v of
                      Nothing -> (Nothing, [])
                      Just x -> let (x', rs) = fix vsite x in (Just x', rs)
                in (v { vdFields = map fst fr, vdRef = ref' }, concatMap snd fr ++ rr)
              r = map fixV vs
          in (KindEnum d (map fst r) repr, concatMap snd r)
        KindAlias t -> let (t', rs) = fix site t in (KindAlias t', rs)
        k -> (k, [])
      fixType name td = let (k, rs) = fixKind name (tdKind td) in (td { tdKind = k }, rs)
      fixMethod md =
        let path = mdFullPath md
            ps = [ let (t, rs) = fix (siteParam path (pdName p)) (pdType p) in (p { pdType = t }, rs) | p <- mdParams md ]
            (ret, rr) = fix (siteReturns path) (mdReturns md)
            (inp, ri) = case mdInput md of
              Nothing -> (Nothing, [])
              Just x -> let (x', rs) = fix (siteInput path) x in (Just x', rs)
        in (md { mdParams = map fst ps, mdReturns = ret, mdInput = inp }, concatMap snd ps ++ rr ++ ri)
      types' = Map.mapWithKey fixType (irTypes ir)
      methods' = Map.map fixMethod (irMethods ir)
  in ( ir { irTypes = Map.map fst types', irMethods = Map.map fst methods' }
     , concatMap snd (Map.elems types') ++ concatMap snd (Map.elems methods') )

-- ============================================================================
-- Extraction from Plugin
-- ============================================================================

-- | Extract all types and methods from a plugin schema
-- Types are namespace-qualified to avoid collisions (e.g., "cone.ListResult")
extractFromPlugin :: Text -> Text -> PluginSchema -> (Map Text TypeDef, Map Text MethodDef, [Refusal])
extractFromPlugin namespace pathPrefix schema =
  let -- every method's documents share one namespace's $defs: read once
      docs = [ o | m <- psMethods schema, Just (Object o) <- [methodParams m, methodReturns m, methodInput m] ]
      shared = sharedDoc namespace docs
      ((defs, results), refusals) = runRead $ do
        ds <- readDefs shared
        rs <- traverse (methodDefRead (Just shared) namespace pathPrefix) (psMethods schema)
        pure (ds, rs)
      allTypes = Map.unions (defs : map fst results)
      allMethods = Map.fromList [ (mdFullPath m, m) | (_, m) <- results ]
  in (allTypes, allMethods, refusals)

-- | Extract types and method def from a single method (its refusals dropped:
--   'extractMethodDefR' keeps them).
extractMethodDef :: Text -> Text -> MethodSchema -> (Map Text TypeDef, MethodDef)
extractMethodDef namespace pathPrefix = fst . extractMethodDefR namespace pathPrefix

-- | Extract types and method def from a single method, and every site the
--   reader refused on the way.
extractMethodDefR :: Text -> Text -> MethodSchema -> ((Map Text TypeDef, MethodDef), [Refusal])
extractMethodDefR = extractMethodDefIn Nothing

-- | 'extractMethodDefR', its $defs read from a namespace's shared document
--   when one is given (and so not read here), else from its own documents.
extractMethodDefIn :: Maybe Doc -> Text -> Text -> MethodSchema -> ((Map Text TypeDef, MethodDef), [Refusal])
extractMethodDefIn shared namespace pathPrefix = runRead . methodDefRead shared namespace pathPrefix

methodDefRead :: Maybe Doc -> Text -> Text -> MethodSchema -> SchemaRead (Map Text TypeDef, MethodDef)
methodDefRead shared namespace pathPrefix method = do
  let name = methodName method
      fullPath = if T.null pathPrefix
                 then namespace <> "." <> name
                 else pathPrefix <> "." <> name

  -- Params: what a client sends, and what the server injects (SYNAPSE-CC-8).
  -- Per-param deprecation (IR-14) flows from methodParamSchemas.
  (paramTypes, params, injected) <-
    readParams shared namespace fullPath (methodParams method) (methodParamSchemas method)

  -- Returns: a child's is its client (SYNAPSE-CC-8's RefChild); an rpc
  -- method's is read whole, and its absence is refused.
  RootRead returnTypes returnRef streaming <- case methodRole method of
    MethodRoleRpc -> case methodReturns method of
      Just v -> readRoot shared namespace (siteReturns fullPath) (name <> "Result") v
      Nothing -> do
        refuse (siteReturns fullPath) "returns" Nothing "an rpc method with no returns schema"
        pure (RootRead Map.empty RefUnknown False)
    _child -> do
      defs <- case (shared, methodReturns method) of
        (Nothing, Just (Object o)) -> readDefs (docOf namespace o)
        _ -> pure Map.empty
      pure (RootRead defs (RefChild fullPath) False)

  -- Input: reserved for BIDIR-6; read when a server says it.
  inputRead <- traverse (readRoot shared namespace (siteInput fullPath) (name <> "Input")) (methodInput method)

  let allTypes = Map.unions [paramTypes, returnTypes, maybe Map.empty rrTypes inputRead]
  pure
    ( allTypes
    , MethodDef
        { mdName = name
        , mdFullPath = fullPath
        , mdNamespace = namespace
        , mdDescription = Just (methodDescription method)
        , mdStreaming = streaming
        , mdParams = params
        , mdReturns = returnRef
        , mdBidirType = inferBidirType method
        , mdBidirResponseType = Nothing  -- TODO: could extract from response_type title
        , mdBidirResponseSchema = methodResponseType method
        , mdRole = methodRole method
          -- IR-12: lift MethodSchema.role onto the IR so hub-codegen's
          -- typed-handle codegen (IR-9) sees DynamicChild/StaticChild tags.
        , mdGrant = methodGrant method
        , mdTarget = methodTarget method
        , mdLongDescription = methodLongDescription method
          -- AUTH-9: what the method requires and its doc comment, lifted
          -- for hub-codegen; Nothing from a pre-AUTH schema.
        , mdInjected = injected
        , mdInput = rrRef <$> inputRead
        }
    )

-- | Infer the bidirectional type parameter from a MethodSchema.
--
-- Returns:
--   Nothing    – method is not bidirectional
--   Just RefJson – method is bidirectional with default T=Value (StandardBidirChannel)
--
-- TODO: When the activation macro emits a structured "bidir_type", read it here.
inferBidirType :: MethodSchema -> Maybe TypeRef
inferBidirType method
  | not (methodBidirectional method) = Nothing
  | otherwise = Just RefJson  -- StandardBidirChannel's T = serde_json::Value

-- ============================================================================
-- Parameter Extraction
-- ============================================================================

-- | The params schema read: its types, the params a client sends, and the
--   params the server injects (@x-plexus-source@ other than rpc — they are
--   not read as types: the server supplies them).
readParams
  :: Maybe Doc
  -> Text
  -> Text
  -> Maybe Value
  -> Maybe [ParamSchema]
  -> SchemaRead (Map Text TypeDef, [ParamDef], [InjectedParam])
readParams shared namespace fullPath (Just (Object o)) paramSchemas = do
  let doc = maybe (docOf namespace o) id shared
      props = case KM.lookup "properties" o of
        Just (Object p) -> KM.toList p
        _ -> []
      required = case KM.lookup "required" o of
        Just (Array arr) -> [t | String t <- foldr (:) [] arr]
        _ -> []

      -- IR-14: per-parameter deprecation by name from the structured list.
      lookupDeprecation :: Text -> Maybe DeprecationInfo
      lookupDeprecation pname = case paramSchemas of
        Nothing -> Nothing
        Just xs -> case filter ((== pname) . paramName) xs of
          (ps:_) -> paramDeprecation ps
          []     -> Nothing

  defs <- maybe (readDefs doc) (const (pure Map.empty)) shared
  entries <- traverse (\(k, v) -> readParam doc (K.toText k) v) props
  let params = [ p { pdRequired = pdName p `elem` required, pdDeprecation = lookupDeprecation (pdName p) } | Left p <- entries ]
      injected = [ i | Right i <- entries ]
  pure (defs, params, injected)
  where
    readParam doc pname v = case sourceOf v of
      Just src | injectedSource src -> pure $ Right InjectedParam
        { ipName = pname, ipSource = src, ipDescription = descriptionOfValue v }
      src -> do
        t <- readType doc (siteParam fullPath pname) v
        pure $ Left ParamDef
          { pdName = pname
          , pdType = t
          , pdDescription = descriptionOfValue v
          , pdRequired = False
          , pdDefault = defaultOfValue v
          , pdDeprecation = Nothing
          , pdSource = src
          , pdValues = paramValues v
          }
readParams _ _ _ _ _ = pure (Map.empty, [], [])

-- | REQ-6/REQ-9: a param's @x-plexus-source@.
sourceOf :: Value -> Maybe Value
sourceOf (Object po) = KM.lookup "x-plexus-source" po
sourceOf _ = Nothing

-- | A source the server fills (as REQ-8's help reads it): an object whose
--   @from@ is anything but @rpc@.
injectedSource :: Value -> Bool
injectedSource (Object src) = KM.lookup "from" src /= Just (String "rpc")
injectedSource _ = False

descriptionOfValue :: Value -> Maybe Text
descriptionOfValue (Object o) = case KM.lookup "description" o of
  Just (String s) -> Just s
  _ -> Nothing
descriptionOfValue _ = Nothing

defaultOfValue :: Value -> Maybe Value
defaultOfValue (Object o) = KM.lookup "default" o
defaultOfValue _ = Nothing

-- ============================================================================
-- Standalone helpers
-- ============================================================================

-- | Convert a JSON Schema value to a TypeRef (its refusals dropped), with
--   refs qualified by the namespace.
schemaToTypeRef :: Text -> Value -> TypeRef
schemaToTypeRef namespace v = fst (runRead (readType (docOf namespace KM.empty) namespace v))

-- | Extract types from a full schema (for standalone use)
-- Types are namespace-qualified
extractTypesFromSchema :: Text -> Value -> Map Text TypeDef
extractTypesFromSchema namespace (Object o) = fst (runRead (readDefs (docOf namespace o)))
extractTypesFromSchema _ _ = Map.empty
