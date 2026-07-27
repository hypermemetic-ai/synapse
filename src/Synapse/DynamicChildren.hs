{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Dynamic-children consumer (SYN-2).
--
-- This module consumes the @MethodRole::DynamicChild@ metadata that
-- @plexus-macros@ emits via @#[plexus_macros::child(list = ..., search =
-- ...)]@ and exposed through @MethodSchema.methodRole@.
--
-- For every method whose role is @MethodRoleDynamicChild { listMethod =
-- Just fn, searchMethod = ms }@, synapse can call the sibling RPC
-- methods (registered at the wire level as ordinary @<hub>.<fn>@) to
-- enumerate or filter the dynamic children that the gate accepts.
--
-- = Design
--
-- The module factors cleanly into three layers so the renderer (no IO),
-- the live-substrate caller (IO + timeout), and the tests
-- (mockable invoker) can each consume the piece they need.
--
-- 1. Pure helpers ('isDynamicChildGate', 'dynamicChildGate',
--    'dynamicChildGates', 'gateParamName') — extract the
--    @DynamicChild@ shape from a 'MethodSchema' / 'PluginSchema'.
--    No IO. Used by the renderer and by tests.
--
-- 2. Invocation helpers ('listChildrenWith', 'searchChildrenWith') —
--    take an injectable @Invoker@ so tests can mock without going
--    over the wire. The live caller is 'listChildren' /
--    'searchChildren' which wires the substrate transport through
--    'mkSubstrateInvoker'.
--
-- = The cache that used to be here (PLX-121)
--
-- SYN-2 gave this module a per-session cache keyed by
-- @(namespace, listMethodName)@ with a 5-second TTL. That key is
-- mutable and the entry was invalidated by a clock, which is the one
-- caching shape a content-addressed protocol must not have — and it
-- has been __deleted, not patched__.
--
-- Two reasons, and the second is the load-bearing one:
--
-- * It never worked. @renderDynamicChildrenBlock@ built a FRESH cache
--   on every call ('newCache' at the call site), and synapse is a
--   one-shot CLI, so no lookup ever saw an entry written by an earlier
--   one. The TTL protected nothing because nothing survived to expire.
--
-- * It cannot be replaced by a hash-keyed cache, and this is a
--   __finding, not an omission__. What it cached was the RESULT of
--   invoking a list method — a set of live instance names. The
--   Connectome carries no such thing. RFC §5.1's Indexed edge carries
--   an enumeration method's NAME, not its output, and substrate
--   declares no Indexed edge at all (the legacy @#[child(list = ..)]@
--   path emits a Dynamic edge, so @list_method@ / @search_method@ /
--   @id_field@ / @path_template@ never reach the document). There is
--   no advertised hash for a list of instances to key on, so the
--   honest move is to remove the mutable-keyed cache and take the
--   round trip, not to invent an identity for a runtime query.
--
-- The hash-keyed cache PLX-121 did build is for Connectome NODES and
-- lives in "Synapse.Connectome.Nav" / 'Synapse.Monad.lookupNode'.
--
-- = Risk mitigations (per SYN-2 risk table)
--
-- * /Timeout/: every wire call is wrapped in 'race' against
--   'defaultInvocationTimeoutMicros' (2s). On timeout the helper
--   returns the empty list — the caller renders "no completions"
--   without crashing or hanging the CLI.
--
-- * /Errors/: an invoker that returns 'Left' (transport error, method
--   not found, malformed response) is treated as "no completions" —
--   the caller logs a warning via the provided logger and the user
--   sees an empty result instead of a crash.
module Synapse.DynamicChildren
  ( -- * Schema queries (pure)
    DynamicChildGate(..)
  , isDynamicChildGate
  , dynamicChildGate
  , dynamicChildGates
  , gateParamName

    -- * Invocation (injectable)
  , Invoker
  , listChildrenWith
  , searchChildrenWith

    -- * Invocation (live substrate)
  , listChildren
  , searchChildren
  , mkSubstrateInvoker

    -- * Configuration
  , defaultInvocationTimeoutMicros
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Exception (SomeException, catch)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.IO (stderr)

import Plexus.Client (SubstrateConfig)
import qualified Plexus.Transport as ST
import Plexus.Types (PlexusStreamItem(..))
import Plexus.Schema.Recursive
  ( MethodRole(..)
  , MethodSchema(..)
  , PluginSchema(..)
  )

-- ============================================================================
-- Pure helpers
-- ============================================================================

-- | A method that gates a dynamic child. Reified from a 'MethodSchema'
--   whose 'methodRole' is 'MethodRoleDynamicChild'. Stored in pure form
--   so the renderer (no IO) and the IO layer can share the same view.
data DynamicChildGate = DynamicChildGate
  { gateMethodName   :: !Text
    -- ^ The gate's own method name on the wire (e.g. @"body"@).
  , gateListMethod   :: !(Maybe Text)
    -- ^ The sibling list method, if any (e.g. @"list_planets"@). When
    --   @Nothing@, the gate exists but the activation author did not
    --   opt-in to listing.
  , gateSearchMethod :: !(Maybe Text)
    -- ^ The sibling search method, if any.
  , gateParam        :: !Text
    -- ^ Display name for the placeholder slot. Best-effort: pulled
    --   from the method's parameter schema, falling back to @"name"@.
  }
  deriving stock (Show, Eq)

-- | Is this method a dynamic-child gate? (Any 'MethodRoleDynamicChild'
--   counts, even one without @list_method@ — the gate placeholder is
--   still rendered, but tab-completion will yield no completions.)
isDynamicChildGate :: MethodSchema -> Bool
isDynamicChildGate m = case methodRole m of
  MethodRoleDynamicChild {} -> True
  _                         -> False

-- | Project a 'MethodSchema' into a 'DynamicChildGate' iff its role is
--   'MethodRoleDynamicChild'. Returns 'Nothing' for ordinary RPC and
--   for 'MethodRoleStaticChild' methods.
dynamicChildGate :: MethodSchema -> Maybe DynamicChildGate
dynamicChildGate m = case methodRole m of
  MethodRoleDynamicChild { listMethod = lm, searchMethod = sm } ->
    Just DynamicChildGate
      { gateMethodName   = methodName m
      , gateListMethod   = lm
      , gateSearchMethod = sm
      , gateParam        = gateParamName m
      }
  _ -> Nothing

-- | All dynamic-child gates on a 'PluginSchema', in declaration order.
dynamicChildGates :: PluginSchema -> [DynamicChildGate]
dynamicChildGates ps = mapMaybe dynamicChildGate (psMethods ps)

-- | Best-effort parameter name for the gate placeholder. We scan the
--   method's JSON Schema 'methodParams' for a single @properties@ key
--   and use it. Falls back to @"name"@ when the schema is absent or
--   ambiguous.
--
--   We deliberately don't crash on weird schemas — the placeholder is
--   a UI convenience, not a semantic guarantee.
gateParamName :: MethodSchema -> Text
gateParamName m = case methodParams m of
  Just (Object o) -> case KM.lookup "properties" o of
    Just (Object props) -> case KM.keys props of
      [k] -> K.toText k
      ks  -> case filter (/= "_self") (map K.toText ks) of
        [k] -> k
        _   -> "name"
    _ -> "name"
  _ -> "name"

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Default per-call invocation timeout in microseconds. Past this we
--   give up and return "no completions" so the user is not blocked by
--   a slow or unreachable hub.
defaultInvocationTimeoutMicros :: Int
defaultInvocationTimeoutMicros = 2 * 1000 * 1000   -- 2 seconds

-- ============================================================================
-- Invocation (injectable)
-- ============================================================================

-- | An RPC invoker. Takes a namespace path, a method name, and JSON
--   params; returns 'Left' on any failure (transport, malformed
--   stream, JSON-RPC error) and 'Right' on success.
--
--   The choice of 'Either' here (vs throwing) is deliberate: the
--   consumer in SYN-2 wants to *swallow* errors gracefully and render
--   "no completions" rather than crash the CLI. Lifting that into the
--   types lets the test harness exercise both branches without
--   touching exception machinery.
type Invoker = [Text] -> Text -> Value -> IO (Either Text [PlexusStreamItem])

-- | Enumerate dynamic children by calling @<ns>.<listMethodName>@ via
--   the supplied invoker.
--
--   Behavior on edge cases (per SYN-2 risk table):
--
--   * /Empty stream/: returns @[]@ — gracefully empty.
--   * /Invoker error/: logs a warning via 'logWarn' and returns @[]@.
--     The call site renders "no completions".
--   * /Timeout/: races the call against
--     'defaultInvocationTimeoutMicros'; on timeout returns @[]@ and
--     logs a warning.
listChildrenWith
  :: (Text -> IO ())         -- ^ Warn logger (stderr writer in production)
  -> Invoker
  -> [Text]                  -- ^ Namespace path (e.g. @["solar"]@)
  -> Text                    -- ^ List method name (e.g. @"list_planets"@)
  -> IO [Text]
listChildrenWith logWarn invoker nsPath fn =
  invokeForNames logWarn fn $ invoker nsPath fn (object [])

-- | Filter dynamic children by calling
--   @<ns>.<searchMethodName>@ with a @query@ parameter. Not cached —
--   search results are prefix-specific so caching them by prefix would
--   bloat memory for marginal benefit. Tab-completion calls land here
--   at typing speed; 'defaultInvocationTimeoutMicros' still applies.
searchChildrenWith
  :: (Text -> IO ())
  -> Invoker
  -> [Text]
  -> Text              -- ^ Search method name
  -> Text              -- ^ Query prefix
  -> IO [Text]
searchChildrenWith logWarn invoker nsPath fn query =
  invokeForNames logWarn fn $
    invoker nsPath fn (object ["query" .= query])

-- | Shared driver for 'listChildrenWith' / 'searchChildrenWith'. Wraps
--   the underlying IO in:
--
--   1. A timeout race ('defaultInvocationTimeoutMicros').
--   2. An exception catch (defensive — the invoker is supposed to
--      return 'Left' on its own errors, but we don't want a runtime
--      panic in transport code to crash the CLI).
--
--   On any failure we log a warning and return @[]@.
invokeForNames
  :: (Text -> IO ())
  -> Text                                   -- ^ Method name (for log msg)
  -> IO (Either Text [PlexusStreamItem])
  -> IO [Text]
invokeForNames logWarn fn action = do
  let timer = threadDelay defaultInvocationTimeoutMicros >> pure Nothing
      run   = (Just <$> action)
                `catch` \(e :: SomeException) ->
                  pure (Just (Left (T.pack (show e))))
  result <- race timer run
  case result of
    Left _ -> do
      logWarn $ "dynamic-children call to " <> fn
        <> " timed out after "
        <> T.pack (show (defaultInvocationTimeoutMicros `div` 1000))
        <> "ms — falling back to no completions"
      pure []
    Right Nothing ->
      -- 'race' returned the 'run' branch with @Nothing@. By
      -- construction 'run' always wraps in 'Just'; this case is
      -- unreachable but kept exhaustive.
      pure []
    Right (Just (Left err)) -> do
      logWarn $ "dynamic-children call to " <> fn
        <> " failed: " <> err
      pure []
    Right (Just (Right items)) ->
      pure (extractNames items)

-- | Pull string names out of the substrate stream. We accept several
--   common shapes:
--
--   * A bare string per 'StreamData' frame — the canonical shape for
--     a method returning @impl Stream<Item = String>@.
--   * A JSON array of strings inside one 'StreamData' frame.
--   * An object with a @"name"@ field — tolerant fallback for hubs
--     that return richer records.
--
--   Any frame that doesn't match one of these is silently skipped;
--   we'd rather show a partial completion list than crash.
extractNames :: [PlexusStreamItem] -> [Text]
extractNames items = concatMap fromItem items
  where
    fromItem (StreamData _ _ _ content) = fromValue content
    fromItem _                          = []

    fromValue (String t) = [t]
    fromValue (Array xs) = concatMap fromValue (V.toList xs)
    fromValue (Object o) = case KM.lookup "name" o of
      Just (String t) -> [t]
      _               -> []
    fromValue _          = []

-- ============================================================================
-- Invocation (live substrate)
-- ============================================================================

-- | Wire 'Invoker' that goes over the configured substrate transport.
--   This is the production caller; tests use 'listChildrenWith' /
--   'searchChildrenWith' with a mock invoker instead.
mkSubstrateInvoker :: SubstrateConfig -> Invoker
mkSubstrateInvoker cfg nsPath method params = do
  result <- (Right <$> ST.invokeMethod cfg nsPath method params)
              `catch` \(e :: SomeException) ->
                pure (Left (T.pack (show e)))
  case result of
    Right (Right items) -> pure (Right items)
    Right (Left err)    -> pure (Left (T.pack (show err)))
    Left exMsg          -> pure (Left ("transport panic: " <> exMsg))

-- | Live wrapper around 'listChildrenWith'.
listChildren
  :: SubstrateConfig
  -> [Text]
  -> Text
  -> IO [Text]
listChildren cfg nsPath fn =
  listChildrenWith stderrWarn (mkSubstrateInvoker cfg) nsPath fn

-- | Live wrapper around 'searchChildrenWith'.
searchChildren
  :: SubstrateConfig
  -> [Text]
  -> Text
  -> Text
  -> IO [Text]
searchChildren cfg nsPath fn query =
  searchChildrenWith stderrWarn (mkSubstrateInvoker cfg) nsPath fn query

-- ============================================================================
-- Internal helpers
-- ============================================================================

-- | The production warning sink. Writes to stderr without colour or
--   structure; the existing 'Synapse.Log' is Katip-based and would
--   bring 'SynapseM' into scope, which we explicitly avoid in this
--   module so it can be called from pure-IO contexts (renderer,
--   tests).
stderrWarn :: Text -> IO ()
stderrWarn msg = TIO.hPutStrLn stderr $ "[WARN] " <> msg
