-- | Schema caching by content hash
--
-- Schemas are cached by their hash. Same hash = same content.
-- This enables safe caching: if the hash matches, use cached value.
--
-- = PLX-143: one key, taken from one place
--
-- This module used to look a schema up under the hash the __parent advertised__
-- for the child (@csHash@) and insert it under the hash the __fetched document
-- carried for itself__ (@psHash@). Two keys for one entry. When they agree the
-- asymmetry is invisible; when they disagree the cache misses forever, silently
-- and with no error, because nothing is ever stored under the key anybody looks
-- up. A cache whose hit is unobservable is worse than no cache: it is a cache
-- you will believe in.
--
-- The advertised hash is now the only key, for both halves. That is the same
-- shape as the Connectome node cache built in PLX-121
-- ('Synapse.Monad.lookupNode' / 'Synapse.Monad.insertNode'), and it is what
-- CONNECTOME RFC 002 §5.2 requires of a client: fold the hash a parent
-- advertises, do not recompute one of your own.
--
-- Two consequences are deliberate:
--
--   * __An empty advertised hash disables the cache__ rather than becoming a
--     key. @plexus-macros@ synthesizes @ChildSummary@ values with
--     @hash: String::new()@ for @#[child]@ methods when an activation does not
--     hand-write @plugin_children@
--     (@plexus-macros\/src\/codegen\/activation.rs:594-620@), and Rust
--     serializes that as a literal @\"hash\": \"\"@. Keying on @\"\"@ would
--     collide every identity-less child in the tree onto one entry and hand
--     back the wrong schema. No identity advertised means no caching — the
--     round trip is the honest answer, exactly as PLX-121 concluded for
--     enumerated Dynamic children.
--
--   * __A disagreement between the advertised and the fetched hash is logged__
--     instead of silently changing which key is written. It is a statement
--     about the wire, and it belongs in the wire's log.
module Synapse.Cache
  ( -- * Cache Operations (re-exported from Monad)
    lookupCache
  , insertCache

    -- * Cache-aware fetching
  , fetchCached
  ) where

import Control.Monad (when)
import qualified Data.Text as T

import Synapse.Schema.Types
import Synapse.Monad
import qualified Synapse.Log as Log

-- | Fetch a schema, using cache if available.
--
-- The key is @advertised@ — the hash the parent gave for this child — on BOTH
-- the lookup and the insert. The fetcher is called only on a miss.
fetchCached :: PluginHash -> SynapseM PluginSchema -> SynapseM PluginSchema
fetchCached advertised fetcher
  -- No content identity on the wire: fetch, do not pretend to cache.
  | T.null advertised = fetcher
  | otherwise = do
      cached <- lookupCache advertised
      case cached of
        Just schema -> pure schema
        Nothing -> do
          schema <- fetcher
          -- Observability, not correction: if the parent's advertisement and the
          -- child's own self-report disagree, say so. The key stays the
          -- advertised one either way (RFC 002 §5.2).
          when (psHash schema /= advertised) $ do
            logger <- getLogger
            Log.logWarn logger Log.SubsystemCache $
              "advertised child hash " <> advertised
                <> " disagrees with fetched schema hash " <> psHash schema
                <> " for namespace " <> psNamespace schema
                <> " — caching under the advertised hash (RFC 002 5.2)"
          insertCache advertised schema
          pure schema
