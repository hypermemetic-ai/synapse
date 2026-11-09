-- | Navigation algebra (paramorphism)
--
-- = Why Paramorphism
--
-- Navigation requires inspecting child namespaces to match path segments.
-- A catamorphism only gives us the folded result — we lose the original structure.
-- A paramorphism gives us both: F(μF × A) → A
--
-- At each layer, we receive (original subtree, already-computed result) pairs.
-- We inspect the original to check namespaces, then recurse.
--
-- = Effectful Navigation
--
-- Since resolution requires network calls (fetch child schema), we can't use
-- pure 'para'. Instead, we implement navigation directly in SynapseM,
-- following the same pattern but with effects.
module Synapse.Algebra.Navigate
  ( -- * Navigation
    navigate
  , navigateFrom

    -- * Algebra Types
  , NavAlg
  , NavResult

    -- * Helpers
  , findChild
  , findMethod
  ) where

import Data.Text (Text)
import Data.List (find)

import Synapse.Schema.Types
import Plexus.Schema.Recursive (MethodRole(..), isSlotRole)
import Synapse.Monad
import Synapse.Transport (fetchSchemaAt, fetchSlotSchemaAt)
import Synapse.Cache (fetchCached)

-- | The navigation algebra type (conceptual)
--
-- In a pure setting with recursive schemas:
-- @
-- type NavAlg = PluginSchemaF (ChildSummary, NavResult) -> NavResult
-- @
--
-- With effectful resolution:
-- @
-- type NavAlg = PluginSchema -> Path -> Path -> SynapseM SchemaView
-- @
type NavAlg = PluginSchema -> Path -> Path -> SynapseM SchemaView

-- | Result of navigation: either a schema view or an error
type NavResult = SynapseM SchemaView

-- | Navigate to a target from root
-- Fetches root schema and navigates from there
-- Note: If path starts with root namespace (e.g., "plexus"), it's stripped
-- so that "synapse plexus cone" works the same as "synapse cone"
navigate :: Path -> SynapseM SchemaView
navigate target = do
  root <- fetchSchemaAt []
  -- Normalize path: strip leading root namespace if present,
  -- but only if it's NOT also a child name (otherwise the user
  -- is trying to navigate to a child that shares the root namespace)
  let hasChildWithName seg = any (\c -> csNamespace c == seg) (pluginChildren root)
  let normalizedTarget = case target of
        (seg:rest) | seg == psNamespace root
                   , not (hasChildWithName seg) -> rest
        _ -> target
  withFreshVisited $ navigateFrom root [] normalizedTarget

-- | Navigate from a given schema
-- visited: path taken so far
-- target: remaining path to navigate
--
-- SYNAPSE-41 (NEST-21's start): a schema's slots are open names. A slot is
-- a method whose role is a dynamic child (NEST-5, @config.{key}@: its names
-- sit directly under the activation) or a peer slot (NEST-7, @dyn.peer@: its
-- names sit under the method, @dyn.peer.<name>@). A name a schema does not
-- list is asked of the service (@<path>.<name>.schema@) and stepped into if
-- the service answers it; one it says is not there is @Command not found@;
-- any other failure (an outage, a refusal, a sign-in) is that failure. A
-- slot's instance is a tree of its own — a peer is another service, a key
-- one more instance of the slot's type — so its hashes are no cycle with
-- the path that reached it.
navigateFrom :: PluginSchema -> Path -> Path -> SynapseM SchemaView
navigateFrom schema visited = \case
  -- Empty target: we've arrived
  [] -> pure $ ViewPlugin schema visited

  -- Non-empty target: try to navigate
  (seg:rest) ->
    -- First check if seg is a child namespace
    case findChild seg schema of
      Just child -> do
        let here = visited ++ [seg]
            -- a slot's listed instance starts a tree of its own
            fresh = if hasSlot schema then withFreshVisited else id
        fresh $ do
          -- Check for cycle before descending
          checkCycle (csHash child) here
          -- Fetch child schema (with caching)
          childSchema <- fetchCached (csHash child) (fetchSchemaAt here)
          -- Recurse into child
          navigateFrom childSchema here rest

      Nothing ->
        -- Not a child — check if it's a method
        case findMethod seg schema of
          Just method
            -- a peer slot the schema does not list as a child: the next
            -- segment is a peer's name
            | MethodRolePeer{} <- methodRole method
            , (name:rest') <- rest ->
                stepIntoSlot (visited ++ [seg]) name rest' (NotFound name (visited ++ [seg]) Nothing)
            | null rest -> pure $ ViewMethod method (visited ++ [seg])
            | otherwise -> throwNav $ MethodNotTerminal seg visited
          Nothing
            -- a dynamic child's key: a name directly under the activation
            | hasDynamicChild schema ->
                stepIntoSlot visited seg rest (NotFound seg visited (Just schema))
            | otherwise -> throwNav $ NotFound seg visited (Just schema)

-- | SYNAPSE-41: step into the name @name@ of a slot at @path@ — asked of the
-- service, a tree of its own — or say @notFound@ when the service says it
-- is not there.
stepIntoSlot :: Path -> Text -> Path -> NavError -> SynapseM SchemaView
stepIntoSlot path name rest notFound = do
  let here = path ++ [name]
  found <- fetchSlotSchemaAt here
  case found of
    Nothing -> throwNav notFound
    Just instanceSchema -> withFreshVisited $ do
      checkCycle (psHash instanceSchema) here
      navigateFrom instanceSchema here rest

-- | NEST-21: whether a schema has a dynamic child slot (its names directly
-- under it).
hasDynamicChild :: PluginSchema -> Bool
hasDynamicChild = any (isDynamic . methodRole) . psMethods
  where
    isDynamic MethodRoleDynamicChild{} = True
    isDynamic _ = False

-- | SYNAPSE-41: whether a schema has a slot of either kind — its children
-- are then a slot and its instances.
hasSlot :: PluginSchema -> Bool
hasSlot = any (isSlotRole . methodRole) . psMethods

-- | Find a child by namespace
findChild :: Text -> PluginSchema -> Maybe ChildSummary
findChild seg schema = find ((== seg) . csNamespace) (pluginChildren schema)

-- | Find a method by name
findMethod :: Text -> PluginSchema -> Maybe MethodSchema
findMethod seg schema = find ((== seg) . methodName) (psMethods schema)
