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
import Synapse.Monad
import Synapse.Transport (fetchSchemaAt)
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
  -- Normalize path: strip leading root namespace if present
  let normalizedTarget = case target of
        (seg:rest) | seg == psNamespace root -> rest
        _ -> target
  withFreshVisited $ navigateFrom root [] normalizedTarget

-- | Navigate from a given schema
-- visited: path taken so far
-- target: remaining path to navigate
navigateFrom :: PluginSchema -> Path -> Path -> SynapseM SchemaView
navigateFrom schema visited = \case
  -- Empty target: we've arrived
  [] -> pure $ ViewPlugin schema visited

  -- Non-empty target: try to navigate
  (seg:rest) ->
    -- First check if seg is a child namespace
    case findChild seg schema of
      Just child -> do
        -- Check for cycle before descending
        checkCycle (csHash child) (visited ++ [seg])
        -- Fetch child schema (with caching)
        childSchema <- fetchCached (csHash child) (fetchSchemaAt (visited ++ [seg]))
        -- Recurse into child
        navigateFrom childSchema (visited ++ [seg]) rest

      Nothing ->
        -- Not a child — check if it's a method
        case findMethod seg schema of
          Just method
            | null rest -> pure $ ViewMethod method (visited ++ [seg])
            | otherwise -> throwNav $ MethodNotTerminal seg visited
          Nothing -> throwNav $ NotFound seg visited (Just schema)

-- | Find a child by namespace
findChild :: Text -> PluginSchema -> Maybe ChildSummary
findChild seg schema = find ((== seg) . csNamespace) (pluginChildren schema)

-- | Find a method by name
findMethod :: Text -> PluginSchema -> Maybe MethodSchema
findMethod seg schema = find ((== seg) . methodName) (psMethods schema)
