{-# LANGUAGE ScopedTypeVariables #-}

-- | Navigation as pointer-chasing over one document (PLX-121).
--
-- = What changed under this module
--
-- Legacy navigation ("Synapse.Algebra.Navigate") costs a @{ns}.schema@ round
-- trip at /every level/ of a drill-down, because a legacy @ChildSummary@ is
-- three fields and shallow by design. PLX-142 put the whole tree on the wire:
-- @{ns}.connectome@ returns a CONNECTOME RFC 002 document with Static and
-- Indexed children embedded, so descending them requires no round trip at all
-- (§5.1: "Descending MUST require no additional round trip").
--
-- This module is the consumer of that. One fetch, then local pointer-chasing,
-- with Dynamic edges fetched lazily and cached __by hash__.
--
-- = The three facts from PLX-142 this module is built around
--
-- [The root has no methods]
--   Substrate's root document declares @methods = []@ — the hub's own
--   @call@ \/ @hash@ \/ @schema@ are invisible in it. 'navigateFrom' therefore
--   never treats "the root lists no method named X" as "this backend has no
--   method named X"; a miss at the root is reported as a miss at a node that
--   is known to be an incomplete map ('CRootHasNoMethods').
--
-- [Two Dynamic edges advertise a legacy hash]
--   @health@ and @registry@ advertise 16 hex characters from the legacy
--   @DefaultHasher@ rollup, not a @CONNECTOME-HASH/1@ digest, because those
--   activations declare no @ActivationIr@. This module __never compares an
--   advertised hash against a node hash__ — not to validate a fetch, not to
--   detect a cycle, not to key the cache. The advertised hash is used as an
--   opaque key and classified ('Synapse.Wire.classifyHash') so the mixed case
--   is visible in the log rather than silently coerced.
--
-- [1.6 MB per fetch, recomposed per request]
--   So the whole-tree document is fetched __at most once per process__ and
--   held ('readRootDocument'). That is memoisation and is named as such: the
--   root has no /advertised/ hash for a consumer to key on before fetching, so
--   there is nothing to cache it by. Only Dynamic children — which do carry a
--   pre-fetch hash — get a hash-keyed cache, which is exactly what §5.1 says
--   that hash is for.
--
-- = What this module found and does NOT work around
--
-- On substrate today __no Dynamic edge is fetchable__ (see 'CUnfetchable').
-- The hub emits a Static edge exactly when it holds the child's document and a
-- Dynamic edge exactly when it does not, and @{ns}.connectome@'s @namespace@
-- parameter resolves only hub-level activations — so the three nested Dynamic
-- edges (@claudecode\/session@, @cone\/of@, @solar\/body@) have no wire route,
-- and the two hub-level ones (@health@, @registry@) have no document. §5.1
-- requires a Dynamic edge to be "sufficient to fetch and cache it lazily".
-- That is an IR-completeness finding against RFC 002, reported here as a typed
-- outcome and a log line, and it is not papered over with a fallback to
-- @{ns}.schema@.
module Synapse.Connectome.Nav
  ( -- * Views and errors
    CView (..)
  , CNavError (..)
  , renderCNavError

    -- * The document
  , rootDocument

    -- * Navigation
  , navigateC
  , navigateFromC

    -- * Whole-tree traversal (the c1 gate)
  , walkTree
  , WalkEntry (..)

    -- * Local lookups (no IO — these are the pointer-chases)
  , findEdge
  , findMethodC
  , edgeNamespace
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T

import Plexus.Client (SubstrateConfig(..))
import qualified Plexus.Connectome.Codec as CC
import qualified Plexus.Connectome.Types as C
import qualified Plexus.Transport as ST

import qualified Synapse.Backend.Discovery as Disc
import Synapse.Monad
import Synapse.Schema.Types (Path)
import Synapse.Transport (getConfig)
import Synapse.Wire

--------------------------------------------------------------------------------
-- Views
--------------------------------------------------------------------------------

-- | Where a navigation landed.
--
-- Deliberately minimal. It carries no parent activation: widening a view to
-- carry the parent it already had in scope is the fix for PLX-121's two
-- re-fetch scars, and those were split out as __PLX-143__. Doing it here would
-- take that node's work and make its "behaviour is preserved" test meaningless.
data CView
  = CViewActivation !C.Activation !Path
  | CViewMethod !C.Method !Path
  deriving stock (Eq, Show)

data CNavError
  = -- | No child edge and no method with this name at this path.
    CNotFound !Text !Path
  | -- | Landed on a method with path segments still to go.
    CMethodNotTerminal !Text !Path
  | -- | The same node hash was seen twice on one descent.
    CCycle !Text !Path
  | -- | A Dynamic edge the wire cannot serve. RFC §5.1 says the edge must be
    -- sufficient to fetch lazily; this one is not. Namespace, advertised hash,
    -- its shape, and the server's own words.
    CUnfetchable !Text !Text !HashKind !Text
  | -- | The document itself could not be fetched or decoded.
    CDocumentError !Text
  | -- | A miss at the root, annotated: substrate's root document declares zero
    -- methods, so "not found at the root" is weaker evidence than it looks.
    CRootHasNoMethods !Text
  deriving stock (Eq, Show)

renderCNavError :: CNavError -> Text
renderCNavError = \case
  CNotFound seg path ->
    "'" <> seg <> "' is not a child or a method at "
      <> renderPath path
  CMethodNotTerminal seg path ->
    "'" <> seg <> "' is a method at " <> renderPath path
      <> " but the path continues past it"
  CCycle h path ->
    "cycle: node hash " <> h <> " was already visited on the way to " <> renderPath path
  CUnfetchable ns h kind why ->
    T.intercalate "\n      "
      [ "the Dynamic edge '" <> ns <> "' cannot be fetched: " <> why
      , "advertised hash: " <> h <> " (" <> kindWord kind <> ")"
      , "RFC 002 §5.1 requires a Dynamic edge to carry the child's namespace and"
      , "advertised hash 'sufficient to fetch and cache it lazily'. This document"
      , "satisfies the letter and not the sufficiency clause. That is an"
      , "IR-completeness finding against the emitter, not a client defect."
      ]
  CDocumentError msg -> "connectome document unavailable: " <> msg
  CRootHasNoMethods seg ->
    "'" <> seg <> "' is not an activation on this backend.\n\
    \  (The root document declares ZERO methods — the hub's own call/hash/schema\n\
    \  are invisible in it — so this is a miss against an incomplete map.)"
  where
    kindWord ConnectomeHash = "CONNECTOME-HASH/1 shape"
    kindWord LegacyAdvertised = "legacy shape; not a content identity"

renderPath :: Path -> Text
renderPath [] = "the root"
renderPath p = T.intercalate "." p

--------------------------------------------------------------------------------
-- Fetching
--------------------------------------------------------------------------------

-- | The whole-tree document, fetched at most once per process.
--
-- Every subsequent navigation in the same process chases pointers inside the
-- value this returns. That, not a cache, is what deletes the per-level round
-- trip: the second navigation issues no fetch because the tree is already in
-- hand, and the log says so.
rootDocument :: SynapseM (Either CNavError C.Connectome)
rootDocument =
  readRootDocument >>= \case
    Just doc -> pure (Right doc)
    Nothing -> fetchDocument Nothing >>= \case
      Left err -> pure (Left err)
      Right doc -> do
        writeRootDocument doc
        -- PLX-157 (item 5) — the root document is the authoritative statement
        -- of who this backend is. Seed the identity cache from §3.3's
        -- `backend_name` so no later resolution falls back to an `_info`
        -- probe. A document that declares nothing (every backend before this
        -- ticket) leaves the cache untouched and the probe path unchanged.
        cfg <- getConfig
        case C.rfBackendName (C.cRootFacts doc) of
          Nothing -> pure ()
          Just bn ->
            liftIO $
              Disc.noteBackendNameFromRootFact
                (T.pack (substrateHost cfg))
                (substratePort cfg)
                (C.unBackendName bn)
        pure (Right doc)

-- | One document fetch, logged.
fetchDocument :: Maybe Text -> SynapseM (Either CNavError C.Connectome)
fetchDocument mNs = do
  cfg <- getConfig
  override <- getDocFetcher
  result <- case override of
    Just fetch -> liftIO (fetch mNs)
    Nothing ->
      liftIO $
        either (Left . T.pack . show) Right
          <$> ST.fetchConnectomeDocument cfg mNs
  case result of
    Left transportErr -> pure (Left (CDocumentError transportErr))
    Right raw -> case CC.decodeConnectome raw of
      Left decodeErr ->
        pure . Left . CDocumentError $
          "decode failed: " <> T.pack decodeErr
      Right doc -> do
        wireEvent
          WireFetch
            { weMethod = ST.connectomeMethodName cfg
            , weNamespace = mNs
            , weBytes = fromIntegral (BL.length (encode (raw :: Value)))
            , weDocHash = C.unHash (C.rfDocumentHash (C.cRootFacts doc))
            }
        pure (Right doc)

--------------------------------------------------------------------------------
-- Local lookups — the pointer-chases
--------------------------------------------------------------------------------

-- | The namespace an edge leads to, whatever kind it is.
edgeNamespace :: C.Edge -> Text
edgeNamespace = \case
  C.EStatic act -> C.unNamespace (C.aNamespace act)
  C.EDynamic dy -> C.unNamespace (C.dyNamespace dy)
  C.EIndexed ix -> C.unNamespace (C.ixNamespace ix)

findEdge :: Text -> C.Activation -> Maybe C.Edge
findEdge seg act = find ((== seg) . edgeNamespace) (C.aChildren act)

findMethodC :: Text -> C.Activation -> Maybe C.Method
findMethodC seg act =
  find ((== seg) . C.unMethodName . C.mName) (C.aMethods act)

--------------------------------------------------------------------------------
-- Navigation
--------------------------------------------------------------------------------

-- | Navigate from the root of the whole-tree document.
--
-- Exactly one document fetch for the static tree, per PLX-121's required
-- behaviour 1, and it is the /first/ navigation in the process that pays for
-- it.
navigateC :: Path -> SynapseM (Either CNavError CView)
navigateC target =
  rootDocument >>= \case
    Left err -> pure (Left err)
    Right doc -> do
      let root = C.cRoot doc
          rootNs = C.unNamespace (C.aNamespace root)
          hasChildNamed seg = any ((== seg) . edgeNamespace) (C.aChildren root)
          -- Same normalisation the legacy navigator does: `synapse substrate
          -- cone` and `synapse cone` mean the same thing, unless a child
          -- genuinely shares the root's namespace.
          normalized = case target of
            (seg : rest) | seg == rootNs, not (hasChildNamed seg) -> rest
            _ -> target
      navigateFromC root [] normalized []

-- | Descend from a given activation.
--
-- @visited@ is the path taken so far; @seen@ is the set of node hashes on this
-- descent, used for cycle detection. Cycle detection uses __node__ hashes only
-- and never an advertised edge hash, because those are two different
-- constructions and on this wire two of them are not even the same algorithm.
navigateFromC
  :: C.Activation
  -> Path
  -> Path
  -> [Text]
  -> SynapseM (Either CNavError CView)
navigateFromC act visited target seen = case target of
  [] -> pure (Right (CViewActivation act visited))
  (seg : rest) -> case findEdge seg act of
    Just edge -> descend edge seg rest
    Nothing -> case findMethodC seg act of
      Just m
        | null rest -> pure (Right (CViewMethod m (visited ++ [seg])))
        | otherwise -> pure (Left (CMethodNotTerminal seg visited))
      Nothing
        | null visited -> pure (Left (CRootHasNoMethods seg))
        | otherwise -> pure (Left (CNotFound seg visited))
  where
    here = visited

    descend edge seg rest = do
      let path = here ++ [seg]
      case edge of
        -- §5.1: embedded. No round trip. This is the deleted round-trip class.
        C.EStatic child -> do
          wireEvent
            WireLocal
              { wePath = renderPath path
              , weEdgeKind = "static"
              , weNodeHash = C.unHash (C.aHash child)
              }
          continue child path rest

        -- §5.1: the template subtree is embedded, so reaching the family's
        -- shape is local too. Descending to a specific INSTANCE is M3·C
        -- (PLX-122) and is blocked on RFC §10.1; this module stops at the
        -- template rather than composing a path template with an id.
        C.EIndexed ix -> do
          let child = C.ixTemplate ix
          wireEvent
            WireLocal
              { wePath = renderPath path
              , weEdgeKind = "indexed"
              , weNodeHash = C.unHash (C.aHash child)
              }
          continue child path rest

        -- §5.1: not embedded. Fetch lazily, keyed on the advertised hash.
        C.EDynamic dy -> do
          let key = C.unHash (C.dyAdvertisedHash dy)
              ns = C.unNamespace (C.dyNamespace dy)
              kind = classifyHash key
          lookupNode key >>= \case
            Just child -> do
              wireEvent WireCacheHit {weNamespace' = ns, weKey = key, weKind = kind}
              continue child path rest
            Nothing -> do
              wireEvent WireCacheMiss {weNamespace' = ns, weKey = key, weKind = kind}
              fetchDocument (Just ns) >>= \case
                Left (CDocumentError why) -> do
                  wireEvent
                    WireUnfetchable
                      { weNamespace' = ns
                      , weKey = key
                      , weKind = kind
                      , weReason = why
                      }
                  pure (Left (CUnfetchable ns key kind why))
                Left err -> pure (Left err)
                Right childDoc -> do
                  let child = C.cRoot childDoc
                  -- Keyed on the ADVERTISED hash, never on the fetched node's
                  -- own hash. Lookup and insert therefore take the same key
                  -- from the same place and a hit can never be unobservable.
                  insertNode key child
                  continue child path rest

    continue child path rest = do
      let nodeHash = C.unHash (C.aHash child)
      if nodeHash `elem` seen && not (T.null nodeHash)
        then pure (Left (CCycle nodeHash path))
        else navigateFromC child path rest (nodeHash : seen)

--------------------------------------------------------------------------------
-- Whole-tree traversal
--------------------------------------------------------------------------------

-- | One node reached by 'walkTree'.
data WalkEntry = WalkEntry
  { wePathSegs :: !Path
  , weEdge :: !Text
  -- ^ @root@, @static@, @indexed@ or @dynamic@.
  , weActivationHash :: !Text
  , weMethodCount :: !Int
  , weUnreachable :: !(Maybe CNavError)
  -- ^ Set when the edge could not be followed. The walk continues.
  }
  deriving stock (Eq, Show)

-- | Walk the whole tree, following every edge exactly once.
--
-- This is the c1 gate: a full-tree navigation that issues ONE document fetch
-- plus K Dynamic-edge fetches, where K is the number of Dynamic edges that the
-- wire can actually serve. Unfetchable edges are recorded and the walk carries
-- on, so the log is a census rather than a crash.
walkTree :: SynapseM (Either CNavError [WalkEntry])
walkTree =
  rootDocument >>= \case
    Left err -> pure (Left err)
    Right doc -> do
      let root = C.cRoot doc
      entries <- go root [] "root" []
      pure (Right entries)
  where
    go act path edgeKind seen = do
      let nodeHash = C.unHash (C.aHash act)
          entry =
            WalkEntry
              { wePathSegs = path
              , weEdge = edgeKind
              , weActivationHash = nodeHash
              , weMethodCount = length (C.aMethods act)
              , weUnreachable = Nothing
              }
      if nodeHash `elem` seen && not (T.null nodeHash)
        then pure [entry {weUnreachable = Just (CCycle nodeHash path)}]
        else do
          children <- concat <$> mapM (child path (nodeHash : seen)) (C.aChildren act)
          pure (entry : children)

    child path seen edge = case edge of
      C.EStatic act -> do
        let p = path ++ [C.unNamespace (C.aNamespace act)]
        wireEvent
          WireLocal
            {wePath = renderPath p, weEdgeKind = "static", weNodeHash = C.unHash (C.aHash act)}
        go act p "static" seen
      C.EIndexed ix -> do
        let p = path ++ [C.unNamespace (C.ixNamespace ix)]
            act = C.ixTemplate ix
        wireEvent
          WireLocal
            {wePath = renderPath p, weEdgeKind = "indexed", weNodeHash = C.unHash (C.aHash act)}
        go act p "indexed" seen
      C.EDynamic dy -> do
        let ns = C.unNamespace (C.dyNamespace dy)
            p = path ++ [ns]
            key = C.unHash (C.dyAdvertisedHash dy)
            kind = classifyHash key
        lookupNode key >>= \case
          Just act -> do
            wireEvent WireCacheHit {weNamespace' = ns, weKey = key, weKind = kind}
            go act p "dynamic" seen
          Nothing -> do
            wireEvent WireCacheMiss {weNamespace' = ns, weKey = key, weKind = kind}
            fetchDocument (Just ns) >>= \case
              Left (CDocumentError why) -> do
                wireEvent
                  WireUnfetchable
                    {weNamespace' = ns, weKey = key, weKind = kind, weReason = why}
                pure
                  [ WalkEntry
                      { wePathSegs = p
                      , weEdge = "dynamic"
                      , weActivationHash = key
                      , weMethodCount = 0
                      , weUnreachable = Just (CUnfetchable ns key kind why)
                      }
                  ]
              Left err ->
                pure
                  [ WalkEntry
                      { wePathSegs = p
                      , weEdge = "dynamic"
                      , weActivationHash = key
                      , weMethodCount = 0
                      , weUnreachable = Just err
                      }
                  ]
              Right childDoc -> do
                let act = C.cRoot childDoc
                insertNode key act
                go act p "dynamic" seen
