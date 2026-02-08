{-# LANGUAGE PatternSynonyms #-}

-- | Core types for Synapse
--
-- Re-exports schema types from plexus-protocol and defines local types.
module Synapse.Schema.Types
  ( -- * Schema Types (from plexus-protocol)
    PluginSchema(..)
  , MethodSchema(..)
  , ChildSummary(..)
  , PluginHash
  , SchemaResult(..)

    -- * Query helpers
  , pluginChildren
  , childNamespaces
  , isHubActivation
  , isLeafActivation

    -- * Navigation Types
  , Path
  , SchemaView(..)
  , NavError(..)

    -- * Stream Types
  , StreamMeta(..)

    -- * Hub Protocol Types
    -- | Re-exported from Plexus.Types with Hub naming
  , HubStreamItem
  , pattern HubData
  , pattern HubProgress
  , pattern HubError
  , pattern HubDone
  , pattern HubGuidance
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Int (Int64)
import GHC.Generics (Generic)

-- Re-export from plexus-protocol
import Plexus.Schema.Recursive
  ( PluginSchema(..)
  , MethodSchema(..)
  , ChildSummary(..)
  , PluginHash
  , SchemaResult(..)
  , pluginChildren
  , childNamespaces
  , isHubActivation
  , isLeafActivation
  )

import Plexus.Types
  ( PlexusStreamItem(..)
  , Provenance
  , GuidanceErrorType
  , GuidanceSuggestion
  )

-- | A path through the plugin tree (sequence of namespace segments)
type Path = [Text]

-- | A position in the schema tree after navigation
data SchemaView
  = ViewPlugin PluginSchema Path   -- ^ Landed on a plugin, path taken to get here
  | ViewMethod MethodSchema Path   -- ^ Landed on a method, path taken to get here
  deriving stock (Show, Eq)

-- | Navigation errors
data NavError
  = NotFound Text Path             -- ^ Segment not found at path
  | MethodNotTerminal Text Path    -- ^ Method with trailing path segments
  | Cycle PluginHash Path          -- ^ Cycle detected (hash seen before)
  | FetchError Text Path           -- ^ Failed to fetch schema
  deriving stock (Show, Eq)

-- | Metadata from stream events
data StreamMeta = StreamMeta
  { smProvenance :: [Text]
  , smHash       :: PluginHash
  , smTimestamp  :: Int64
  }
  deriving stock (Show, Eq, Generic)

-- ============================================================================
-- Hub Protocol Types (re-exported from Plexus.Types with Hub naming)
-- ============================================================================

-- | Hub stream item - the universal transport envelope for streaming responses
--
-- This is a type alias for 'PlexusStreamItem' from the protocol layer,
-- renamed to use Hub terminology at the application level.
type HubStreamItem = PlexusStreamItem

-- | Pattern synonym for data events
pattern HubData :: Text -> Provenance -> Text -> Value -> HubStreamItem
pattern HubData hash prov contentType content = StreamData hash prov contentType content

-- | Pattern synonym for progress events
pattern HubProgress :: Text -> Provenance -> Text -> Maybe Double -> HubStreamItem
pattern HubProgress hash prov msg pct = StreamProgress hash prov msg pct

-- | Pattern synonym for error events
pattern HubError :: Text -> Provenance -> Text -> Bool -> HubStreamItem
pattern HubError hash prov err recoverable = StreamError hash prov err recoverable

-- | Pattern synonym for done events
pattern HubDone :: Text -> Provenance -> HubStreamItem
pattern HubDone hash prov = StreamDone hash prov

-- | Pattern synonym for guidance events
pattern HubGuidance :: Text -> Provenance -> GuidanceErrorType -> GuidanceSuggestion -> Maybe [Text] -> Maybe Value -> HubStreamItem
pattern HubGuidance hash prov errType suggestion methods schema = StreamGuidance hash prov errType suggestion methods schema
