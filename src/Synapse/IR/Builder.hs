{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | IR Builder - walks schema tree and constructs the IR
--
-- IR 3.0 (plexus 0.7, RETURNS-13) over 2.1's reader (SYNAPSE-CC-8). The
-- schema walker runs a custom algebra that, for every method, reads from
-- the schema, never guessing:
--
-- 1. its item type (@returns@), error type (@error@) and progress type
--    (@progress@), each a 'TypeRef' — a 'RefNamed' with a 'TypeDef', or a
--    primitive / array / optional / JSON ref — and its params and @input@;
--    every type in them read whole, or refused by name ("Synapse.IR.Read");
-- 2. its cardinality (@cardinality@), as is ('mdCardinality');
-- 3. its params, server-injected ones going to 'mdInjected', never
--    'mdParams'.
--
-- One title in one namespace is one type: two methods whose schemas give a
-- name the same shape share one 'TypeDef'; two shapes under one name fail
-- the build ('TitleCollision'), naming both methods. Identical types in
-- different namespaces are then deduplicated, each placed in the namespace
-- that refers to it most (SYNAPSE-CC-8).
module Synapse.IR.Builder
  ( -- * Building IR
    buildIR
  , buildIRRaw
  , buildIRFromSchema
  , finalizeIR

    -- * Errors
  , IRBuildError(..)
  , TypeSource(..)
  , renderIRBuildError

    -- * Extraction (for testing)
  , extractTypesFromSchema
  , extractMethodDef
  , extractMethodDefR
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
-- Errors
-- ============================================================================

-- | Why no IR can be built from a schema.
data IRBuildError
  = TitleCollision
      { tcNamespace :: Text        -- ^ The namespace both types are in
      , tcTitle     :: Text        -- ^ The name they share
      , tcFirst     :: TypeSource  -- ^ Where the first shape was read
      , tcSecond    :: TypeSource  -- ^ Where the second, different, shape was read
      }
    -- ^ Two different shapes carry one name in one namespace (RETURNS-13).
    --   One name in one namespace is one Rust type, so this is a conflict
    --   the activation's author resolves; the IR never picks one.
  deriving stock (Show, Eq)

-- | Where a 'TypeDef' was read: a method, and which of its schemas.
data TypeSource = TypeSource
  { tsMethod :: Text  -- ^ The method's name
  , tsSchema :: Text  -- ^ @"params"@, @"returns"@, @"error"@, @"progress"@ or @"input"@
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

-- | Build IR from one plugin schema, with no backend: what 'buildIR' builds
--   for a walk that starts at @path@ and finds this schema there, children
--   aside (they are summaries; nothing fetches them). 'irMetadata' is
--   'Nothing' — the toolchain and the clock are 'buildIR''s to record — so
--   the result is a pure function of its arguments (RETURNS-13: the
--   conformance fixture's golden @ir.json@ is built with it, and RETURNS-40's
--   server-side IR is held equal to it).
buildIRFromSchema
  :: Text          -- ^ Backend name ('irBackend')
  -> Path          -- ^ The path the schema sits at; its namespace is the path joined with "."
  -> PluginSchema
  -> Either IRBuildError IR
buildIRFromSchema backend path schema = do
  raw <- pluginIR path schema []
  pure $ (finalizeIR raw)
    { irBackend = backend
    , irMetadata = Nothing
    }

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
irAlgebra (PluginF schema path childIRs) = liftBuild (pluginIR path schema childIRs)

irAlgebra (MethodF method namespace path) = liftBuild $ do
  -- Single method node (shouldn't happen in normal walk, but handle it)
  let fullPath = T.intercalate "." path
  ((types, mdef), refusals) <- extractMethodDefR namespace fullPath method
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

-- | One plugin node: its own types and methods, merged with its children's.
pluginIR :: Path -> PluginSchema -> [IR] -> Either IRBuildError IR
pluginIR path schema childIRs = do
  -- Use full path as namespace to avoid collisions
  -- e.g., "hyperforge.workspace.repos" instead of just "repos"
  let namespace = T.intercalate "." path
      pathPrefix = namespace  -- Same as namespace

  -- Extract types and methods from this plugin
  (localTypes, localMethods, localRefusals) <- extractFromPlugin namespace pathPrefix schema

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

  -- REQ-5: capture per-plugin PlexusRequest schema (psRequest from the wire
  -- schema, CONTEXT-5 — HEADERS-1's stub is gone now plexus-protocol reads it).
  let childRequests = fromMaybe Map.empty (irPluginRequests childIR)
      allRequests = case psRequest schema of
        Just reqV -> Map.insert namespace reqV childRequests
        Nothing   -> childRequests

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
      (map pdType (mdParams md) ++ [mdReturns md] ++ maybe [] pure (mdError md) ++ maybe [] pure (mdProgress md)
        ++ maybe [] pure (mdInput md) ++ maybe [] pure (mdBidirType md))
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
  , mdError = fmap (updateTypeRef redirects) (mdError md)
  , mdProgress = fmap (updateTypeRef redirects) (mdProgress md)
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
            optional site = \case
              Nothing -> (Nothing, [])
              Just x -> let (x', rs) = fix site x in (Just x', rs)
            (err, re) = optional (siteError path) (mdError md)
            (prog, rp) = optional (siteProgress path) (mdProgress md)
            (inp, ri) = optional (siteInput path) (mdInput md)
        in ( md { mdParams = map fst ps, mdReturns = ret, mdError = err, mdProgress = prog, mdInput = inp }
           , concatMap snd ps ++ rr ++ re ++ rp ++ ri )
      types' = Map.mapWithKey fixType (irTypes ir)
      methods' = Map.map fixMethod (irMethods ir)
  in ( ir { irTypes = Map.map fst types', irMethods = Map.map fst methods' }
     , concatMap snd (Map.elems types') ++ concatMap snd (Map.elems methods') )

-- ============================================================================
-- Extraction from Plugin
-- ============================================================================

-- | Extract all types and methods from a plugin schema
-- Types are namespace-qualified to avoid collisions (e.g., "cone.ListResult")
--
-- Fails when two of its schemas give one name two shapes ('TitleCollision').
extractFromPlugin :: Text -> Text -> PluginSchema -> Either IRBuildError (Map Text TypeDef, Map Text MethodDef, [Refusal])
extractFromPlugin namespace pathPrefix schema = do
  checkTitles namespace (psMethods schema)
  let -- every method's documents share one namespace's $defs: read once
      docs = [ o | m <- psMethods schema, (_, Object o) <- methodDocs m ]
      shared = sharedDoc namespace docs
      ((defs, results), refusals) = runRead $ do
        ds <- readDefs shared
        rs <- traverse (methodDefRead (Just shared) namespace pathPrefix) (psMethods schema)
        pure (ds, rs)
      allTypes = Map.unions (defs : map fst results)
      allMethods = Map.fromList [ (mdFullPath m, m) | (_, m) <- results ]
  pure (allTypes, allMethods, refusals)

-- | Extract types and method def from a single method (its refusals dropped:
--   'extractMethodDefR' keeps them).
--
-- Fails when two of the method's own schemas give one name two shapes.
extractMethodDef :: Text -> Text -> MethodSchema -> Either IRBuildError (Map Text TypeDef, MethodDef)
extractMethodDef namespace pathPrefix = fmap fst . extractMethodDefR namespace pathPrefix

-- | Extract types and method def from a single method, and every site the
--   reader refused on the way.
extractMethodDefR :: Text -> Text -> MethodSchema -> Either IRBuildError ((Map Text TypeDef, MethodDef), [Refusal])
extractMethodDefR namespace pathPrefix method = do
  checkTitles namespace [method]
  pure (extractMethodDefIn Nothing namespace pathPrefix method)

-- | 'extractMethodDefR', its $defs read from a namespace's shared document
--   when one is given (and so not read here), else from its own documents.
extractMethodDefIn :: Maybe Doc -> Text -> Text -> MethodSchema -> ((Map Text TypeDef, MethodDef), [Refusal])
extractMethodDefIn shared namespace pathPrefix = runRead . methodDefRead shared namespace pathPrefix

-- | A method's schema documents, each by the name 'TypeSource' gives it.
methodDocs :: MethodSchema -> [(Text, Value)]
methodDocs m =
  [ (k, v)
  | (k, Just v) <-
      [ ("params", methodParams m)
      , ("returns", Just (methodReturns m))
      , ("error", methodError m)
      , ("progress", methodProgress m)
      , ("input", methodInput m)
      ]
  ]

-- | The fallback name of a method's untitled named root, by schema.
rootFallback :: MethodSchema -> Text -> Text
rootFallback m = \case
  "returns" -> methodName m <> "Result"
  "error" -> methodName m <> "Error"
  "progress" -> methodName m <> "Progress"
  "input" -> methodName m <> "Input"
  other -> methodName m <> other

-- ============================================================================
-- One name, one type (RETURNS-13)
-- ============================================================================

-- | RETURNS-13: one name in one namespace is one type. Every name the
--   methods' schemas define — each @$defs@ entry, and each named root
--   ('rootNamed') — in the methods' order, then params, returns, error,
--   progress, input; where two give a name different bodies, both are read
--   (each in its own document) and their kinds compared, descriptions aside.
--   Two shapes are a 'TitleCollision' naming the first and the second; the
--   same shape is one type (the reader keeps the first).
--
--   The bodies are compared as JSON first, so a namespace whose methods
--   carry the same @$defs@ again and again (as schemars writes them) costs
--   one equality test a copy, not a read.
checkTitles :: Text -> [MethodSchema] -> Either IRBuildError ()
checkTitles namespace methods = mapM_ checkName (Map.toList byName)
  where
    byName :: Map Text [(Value, KM.KeyMap Value, TypeSource)]
    byName = Map.fromListWith (flip (++))
      [ (name, [(body, docObj, TypeSource (methodName m) which)])
      | m <- methods
      , (which, doc@(Object docObj)) <- methodDocs m
      , (name, body) <- defsOf docObj ++ rootOf m which doc
      ]
    defsOf docObj = case KM.lookup "$defs" docObj <|> KM.lookup "definitions" docObj of
      Just (Object d) -> [ (K.toText k, v) | (k, v) <- KM.toList d ]
      _ -> []
    rootOf m which doc
      | which == "params" = []
      | otherwise = maybe [] pure (rootNamed (rootFallback m which) doc)

    checkName (name, occurrences) = case occurrences of
      [] -> Right ()
      (first : rest) -> () <$ foldM (compareWith name) (first, Nothing) rest

    -- the first occurrence, and its kind once read
    compareWith name (first@(b0, d0, s0), k0) (b, d, s)
      | b == b0 = Right (first, k0)
      | otherwise =
          let kind0 = maybe (readKindIn namespace d0 name b0) id k0
              kind = readKindIn namespace d name b
          in if sameShape kind0 kind
               then Right (first, Just kind0)
               else Left TitleCollision
                 { tcNamespace = namespace, tcTitle = name, tcFirst = s0, tcSecond = s }

-- | Two readings of a name are one type when they agree on everything but
--   their documentation.
sameShape :: Maybe TypeKind -> Maybe TypeKind -> Bool
sameShape a b = fmap undocumented a == fmap undocumented b
  where
    undocumented = \case
      KindStruct fields -> KindStruct (map field fields)
      KindEnum disc variants repr -> KindEnum disc (map variant variants) repr
      other -> other
    field fd = fd { fdDescription = Nothing }
    variant vd = vd { vdDescription = Nothing, vdFields = map field (vdFields vd) }

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
  -- method's is read whole (plexus 0.7: every method states it).
  RootRead returnTypes returnRef <- case methodRole method of
    MethodRoleRpc ->
      readRoot shared namespace (siteReturns fullPath) (rootFallback method "returns") (methodReturns method)
    _child -> do
      defs <- case (shared, methodReturns method) of
        (Nothing, Object o) -> readDefs (docOf namespace o)
        _ -> pure Map.empty
      pure (RootRead defs (RefChild fullPath))

  -- RETURNS-13: the rest of the contract — the error type of a
  -- Result<T, E> item and the progress type P, each its own document.
  errorRead <- traverse (readRoot shared namespace (siteError fullPath) (rootFallback method "error")) (methodError method)
  progressRead <- traverse (readRoot shared namespace (siteProgress fullPath) (rootFallback method "progress")) (methodProgress method)

  -- Input: reserved for BIDIR-6; read when a server says it.
  inputRead <- traverse (readRoot shared namespace (siteInput fullPath) (rootFallback method "input")) (methodInput method)

  let allTypes = Map.unions
        [paramTypes, returnTypes, maybe Map.empty rrTypes errorRead, maybe Map.empty rrTypes progressRead, maybe Map.empty rrTypes inputRead]
  pure
    ( allTypes
    , MethodDef
        { mdName = name
        , mdFullPath = fullPath
        , mdNamespace = namespace
        , mdDescription = Just (methodDescription method)
        , mdParams = params
        , mdReturns = returnRef
        , mdError = rrRef <$> errorRead
        , mdProgress = rrRef <$> progressRead
        , mdCardinality = methodCardinality method
          -- RETURNS-13: the schema's cardinality, never inferred
        , mdBidirType = inferBidirType method
        , mdBidirResponseType = Nothing  -- TODO: could extract from response_type title
        , mdBidirResponseSchema = methodResponseType method
        , mdRole = methodRole method
          -- IR-12: lift MethodSchema.role onto the IR so service-codegen's
          -- typed-handle codegen (IR-9) sees DynamicChild/StaticChild tags.
        , mdGrant = methodGrant method
        , mdTarget = methodTarget method
        , mdLongDescription = methodLongDescription method
          -- AUTH-9: what the method requires and its doc comment, lifted
          -- for service-codegen; Nothing from a pre-AUTH schema.
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
