-- | The wire log (PLX-121).
--
-- = Why this module exists
--
-- PLX-121's exit gate is not "navigation looks local" — it is a /captured/ log
-- showing one document fetch plus K lazy fetches, with cache hits by hash on
-- repeat. "Prove it on the wire, not by inspection." A claim about round trips
-- that cannot be counted is not a measurement, so the counting lives in the
-- navigator itself and every event it records is emitted verbatim.
--
-- The log is deliberately dumb: an append-only 'IORef' of events plus a
-- renderer. It is not 'Synapse.Log' (Katip, severity-filtered, prose) because
-- the point here is a machine-checkable transcript, not diagnostics.
--
-- = The one thing this module is opinionated about
--
-- 'HashKind'. RFC §4.7 exists because "hashed differently" must be
-- distinguishable from "changed", and PLX-142 left a live instance of exactly
-- that: two Dynamic edges on substrate's root advertise a __16-hex legacy__
-- @DefaultHasher@ rollup rather than a @CONNECTOME-HASH/1@ digest, because
-- their activations declare no @ActivationIr@. A consumer that compares one of
-- those against a Connectome node hash is comparing two different
-- constructions and will conclude "changed" forever.
--
-- So the kind is classified once, at the edge, and carried into the log. The
-- navigator never compares an advertised hash to a node hash — see
-- "Synapse.Connectome.Nav".
module Synapse.Wire
  ( -- * Events
    WireEvent (..)
  , HashKind (..)
  , classifyHash

    -- * The log
  , WireLog
  , newWireLog
  , recordEvent
  , readWireLog
  , clearWireLog

    -- * Rendering
  , renderEvent
  , renderWireLog

    -- * Counting (what the acceptance criteria are stated in)
  , countFetches
  , countCacheHits
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T

-- | How an advertised hash was constructed, as far as a consumer can tell from
-- the string alone.
--
-- This is a /syntactic/ classification and is honest about being one: the
-- document does not label a Dynamic edge's hash with its construction (RFC
-- §4.7 puts @hash_algorithm@ on the root, and a Dynamic edge's hash is
-- asserted by a child whose root facts the parent never sees). 64 lowercase
-- hex characters is what @CONNECTOME-HASH/1@ (SHA-256) produces; 16 is what
-- the legacy @DefaultHasher@ rollup produces.
data HashKind
  = -- | 64 hex characters — consistent with @CONNECTOME-HASH/1@.
    ConnectomeHash
  | -- | Anything else. On substrate today this is the 16-hex legacy hub hash,
    -- which PLX-142 measured changing on every boot of byte-identical code.
    -- Usable as an in-process cache key; NOT a content identity, and never
    -- comparable with a Connectome node hash.
    LegacyAdvertised
  deriving stock (Eq, Show)

-- | Classify an advertised hash by shape.
classifyHash :: Text -> HashKind
classifyHash h
  | T.length h == 64 && T.all isLowerHex h = ConnectomeHash
  | otherwise = LegacyAdvertised
  where
    isLowerHex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

-- | One observable thing the navigator did.
data WireEvent
  = -- | A document actually crossed the wire.
    WireFetch
      { weMethod :: !Text
      -- ^ The wire method, e.g. @substrate.connectome@.
      , weNamespace :: !(Maybe Text)
      -- ^ The @namespace@ parameter — 'Nothing' for the whole-tree fetch.
      , weBytes :: !Int
      -- ^ Size of the document content as it crossed the wire, so the cost of
      -- a fetch is a number in the log rather than a memory. Measured by
      -- re-encoding the decoded value compactly, which is what the server
      -- sends: a raw @substrate.connectome@ notification frame measures
      -- 622,390 bytes and its @content@ 622,142, matching this field exactly.
      -- (PLX-142's "1.6 MB" is @to_string_pretty@ as written to disk by
      -- @examples\/fetch_connectome@, not the wire.)
      , weDocHash :: !Text
      -- ^ The document hash the server put on the root.
      }
  | -- | An edge was followed without a round trip.
    WireLocal
      { wePath :: !Text
      , weEdgeKind :: !Text
      -- ^ @static@ or @indexed@.
      , weNodeHash :: !Text
      }
  | -- | A Dynamic edge resolved out of the hash-keyed cache. No fetch.
    WireCacheHit
      { weNamespace' :: !Text
      , weKey :: !Text
      -- ^ The cache key, which is the advertised hash and nothing else.
      , weKind :: !HashKind
      }
  | -- | A Dynamic edge missed the cache and is about to be fetched.
    WireCacheMiss
      { weNamespace' :: !Text
      , weKey :: !Text
      , weKind :: !HashKind
      }
  | -- | A Dynamic edge that §5.1 says is "sufficient to fetch and cache
    -- lazily" and that this wire cannot in fact serve. A finding, recorded
    -- rather than worked around.
    WireUnfetchable
      { weNamespace' :: !Text
      , weKey :: !Text
      , weKind :: !HashKind
      , weReason :: !Text
      }
  deriving stock (Eq, Show)

-- | An append-only log. Stored reversed; 'readWireLog' un-reverses.
newtype WireLog = WireLog (IORef [WireEvent])

newWireLog :: IO WireLog
newWireLog = WireLog <$> newIORef []

recordEvent :: WireLog -> WireEvent -> IO ()
recordEvent (WireLog ref) ev = atomicModifyIORef' ref $ \evs -> (ev : evs, ())

readWireLog :: WireLog -> IO [WireEvent]
readWireLog (WireLog ref) = reverse <$> readIORef ref

clearWireLog :: WireLog -> IO ()
clearWireLog (WireLog ref) = atomicModifyIORef' ref $ const ([], ())

-- | How many round trips happened. This is the number c1 is stated in.
countFetches :: [WireEvent] -> Int
countFetches = length . filter isFetch
  where
    isFetch WireFetch {} = True
    isFetch _ = False

-- | How many Dynamic descents were answered from the hash-keyed cache. This is
-- the number c2 is stated in.
countCacheHits :: [WireEvent] -> Int
countCacheHits = length . filter isHit
  where
    isHit WireCacheHit {} = True
    isHit _ = False

renderEvent :: WireEvent -> Text
renderEvent = \case
  WireFetch {..} ->
    "FETCH        "
      <> weMethod
      <> maybe " {}" (\ns -> " {\"namespace\":\"" <> ns <> "\"}") weNamespace
      <> "  bytes=" <> T.pack (show weBytes)
      <> " doc_hash=" <> weDocHash
  WireLocal {..} ->
    "LOCAL        " <> wePath <> "  edge=" <> weEdgeKind <> " node_hash=" <> weNodeHash
  WireCacheHit {..} ->
    "CACHE-HIT    " <> weNamespace' <> "  key=" <> weKey <> " (" <> renderKind weKind <> ")"
  WireCacheMiss {..} ->
    "CACHE-MISS   " <> weNamespace' <> "  key=" <> weKey <> " (" <> renderKind weKind <> ")"
  WireUnfetchable {..} ->
    "UNFETCHABLE  " <> weNamespace' <> "  advertised=" <> weKey
      <> " (" <> renderKind weKind <> ") — " <> weReason
  where
    renderKind ConnectomeHash = "CONNECTOME-HASH/1 shape"
    renderKind LegacyAdvertised = "legacy shape, NOT a content identity"

renderWireLog :: [WireEvent] -> Text
renderWireLog evs =
  T.unlines $
    map renderEvent evs
      ++ [ ""
         , "wire summary: "
             <> T.pack (show (countFetches evs))
             <> " fetch(es), "
             <> T.pack (show (countCacheHits evs))
             <> " hash-keyed cache hit(s)"
         ]
