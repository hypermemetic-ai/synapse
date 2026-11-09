{-# LANGUAGE OverloadedStrings #-}

-- | Invocation-time deprecation warnings (IR-15).
--
-- IR-6 landed tree-rendering markers for deprecated activations and methods.
-- IR-15 closes the gap for users who never browse the tree — they run a
-- method directly (often from a script). This module emits a one-line
-- notice to 'stderr' at the moment of invocation.
--
-- = Contract
--
--  * Warning text is emitted to 'stderr' only. 'stdout' is untouched so
--    pipe consumers see the same bytes they saw before.
--  * The warning line format is built from IR-6's 'deprecationMarker' and
--    'formatDeprecationLine' helpers (re-exported from 'Synapse.Algebra.Render')
--    so tree rendering and invocation warnings match byte-for-byte after the
--    key/path prefix.
--  * A per-process 'IORef' dedupes warnings by key. Method-level keys are the
--    full dotted path (e.g. @"docs.move_doc"@); activation-level keys are the
--    activation's namespace (e.g. @"docs"@). Keyspaces are disjoint by
--    construction (methods always carry a dot; activation namespaces never
--    carry one in the key we emit below).
--  * A boolean "suppressed" flag short-circuits both emission functions. The
--    CLI wires this flag to the @--no-deprecation-warnings@ option.
--
-- = Implementation
--
-- The dedupe 'IORef' is a top-level, 'NOINLINE' global because "per session"
-- in the ticket means "per process lifetime". Embedding the set in
-- 'Synapse.Monad.SynapseEnv' would also work but would force every call site
-- that currently runs in 'IO' (e.g. stream callbacks) to thread the env around.
-- The IORef approach keeps the API callable from plain 'IO'.
module Synapse.Deprecation
  ( -- * Emission
    emitMethodWarning
  , emitActivationWarning

    -- * Testing Support
  , resetDeprecationStateForTesting
  , formatMethodWarningLine
  , formatActivationWarningLine
  ) where

import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stderr)
import System.IO.Unsafe (unsafePerformIO)

import Plexus.Schema.Recursive (DeprecationInfo)
import Synapse.Algebra.Render (deprecationMarker, formatDeprecationLine)

-- ============================================================================
-- Dedupe State
-- ============================================================================

-- | Process-wide set of keys already warned-about. Keys follow the scheme
--   documented at the top of this module (method full-path, or activation
--   namespace). The same 'IORef' serves both keyspaces; collisions are
--   prevented by the dotted-path vs. plain-namespace distinction.
--
--   NOINLINE is load-bearing: GHC must share a single 'IORef' across every
--   call site, otherwise each call would get its own empty set and dedupe
--   would silently break.
warnedKeys :: IORef (Set Text)
warnedKeys = unsafePerformIO (newIORef Set.empty)
{-# NOINLINE warnedKeys #-}

-- | Try to claim a key for first-time emission. Returns 'True' when the
--   caller is the first to claim it (warning should fire), 'False' when
--   the key was already present (caller must stay silent).
--
--   'atomicModifyIORef'' makes this safe under the IO concurrency we care
--   about here (streaming RPC callback threads). There is
--   no correctness problem if two threads lose the race and both see the
--   key as "already present" — the semantics we promise are "fires at
--   least once per session", and 'atomicModifyIORef'' gives us exactly
--   "fires exactly once per session" under any interleaving.
claimKey :: Text -> IO Bool
claimKey k = atomicModifyIORef' warnedKeys $ \s ->
  if Set.member k s
    then (s, False)
    else (Set.insert k s, True)

-- | Reset the dedupe set. Exported only for tests that drive multiple
--   simulated "sessions" inside a single 'hspec' process. Production code
--   should never call this.
resetDeprecationStateForTesting :: IO ()
resetDeprecationStateForTesting = writeIORef warnedKeys Set.empty

-- ============================================================================
-- Line Formatting
-- ============================================================================

-- | Format the stderr line for a deprecated method invocation. The visible
--   shape is:
--
--   @⚠ <activation>.<method> is DEPRECATED since <since>, removed in <removed_in> — <message>@
--
--   The trailing DEPRECATED clause is delegated to 'formatDeprecationLine'
--   so the wording matches IR-6's tree rendering verbatim.
formatMethodWarningLine :: Text -> DeprecationInfo -> Text
formatMethodWarningLine fullPath di =
  deprecationMarker
    <> " " <> fullPath
    <> " is " <> formatDeprecationLine di

-- | Format the stderr line for an invocation on a deprecated activation.
--   Shape:
--
--   @⚠ activation '<namespace>' is DEPRECATED since <since>, removed in <removed_in> — <message>@
--
--   Uses 'formatDeprecationLine' for the DEPRECATED clause.
formatActivationWarningLine :: Text -> DeprecationInfo -> Text
formatActivationWarningLine namespace di =
  deprecationMarker
    <> " activation '" <> namespace <> "' is "
    <> formatDeprecationLine di

-- ============================================================================
-- Emission
-- ============================================================================

-- | Emit a one-line stderr warning for a deprecated method invocation if
--   (a) warnings aren't globally suppressed, and (b) this method hasn't
--   already been warned about in the current session.
--
--   No-ops silently when the method isn't deprecated, so call sites can
--   hand over the raw 'Maybe DeprecationInfo' without pre-checking.
emitMethodWarning
  :: Bool                     -- ^ suppress all warnings (e.g. @--no-deprecation-warnings@)
  -> Text                     -- ^ full dotted method path (e.g. @"docs.move_doc"@)
  -> Maybe DeprecationInfo    -- ^ deprecation metadata from IR / MethodSchema
  -> IO ()
emitMethodWarning _        _        Nothing   = pure ()
emitMethodWarning True     _        _         = pure ()
emitMethodWarning False    fullPath (Just di) = do
  first <- claimKey fullPath
  when first $ do
    TIO.hPutStrLn stderr (formatMethodWarningLine fullPath di)
    hFlush stderr

-- | Emit a one-line stderr warning for invoking any method on a deprecated
--   activation. Dedupe key is the activation namespace so subsequent
--   invocations on the same activation stay silent.
--
--   No-ops when the activation isn't deprecated.
emitActivationWarning
  :: Bool                     -- ^ suppress all warnings
  -> Text                     -- ^ activation namespace (e.g. @"docs"@)
  -> Maybe DeprecationInfo    -- ^ deprecation metadata from the parent PluginSchema
  -> IO ()
emitActivationWarning _     _         Nothing   = pure ()
emitActivationWarning True  _         _         = pure ()
emitActivationWarning False namespace (Just di) = do
  -- Guard against accidentally colliding with the method keyspace.
  -- Method keys always contain at least one '.'; activation namespaces
  -- may also, but the 'activation:' prefix keeps the two families
  -- unambiguously separated regardless of dot count.
  let key = "activation:" <> namespace
  first <- claimKey key
  when first $ do
    TIO.hPutStrLn stderr (formatActivationWarningLine namespace di)
    hFlush stderr
