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
  , isParentActivation
  , isLeafActivation

    -- * Navigation Types
  , Path
  , SchemaView(..)
  , NavError(..)

    -- * Stream Types
  , StreamMeta(..)

    -- * Service Protocol Types
    -- | Re-exported from Plexus.Types with Service naming
  , ServiceStreamItem
  , pattern ServiceData
  , pattern ServiceProgress
  , pattern ServiceError
  , pattern ServiceDone
  , pattern ServiceGuidance
  , pattern ServiceRequest

    -- * Bidirectional Types (re-exported)
  , Request(..)
  , StandardRequest
  , Response(..)
  , StandardResponse
  , SelectOption(..)
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
  , isParentActivation
  , isLeafActivation
  )

import Plexus.Types
  ( PlexusStreamItem(..)
  , Provenance
  , GuidanceErrorType
  , GuidanceSuggestion
  , Request(..)
  , StandardRequest
  , Response(..)
  , StandardResponse
  , SelectOption(..)
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
  = NotFound Text Path (Maybe PluginSchema)  -- ^ Segment not found at path (with schema context)
  | MethodNotTerminal Text Path              -- ^ Method with trailing path segments
  | Cycle PluginHash Path                    -- ^ Cycle detected (hash seen before)
  | FetchError Text Path                     -- ^ Failed to fetch schema
  deriving stock (Show, Eq)

-- | Metadata from stream events
data StreamMeta = StreamMeta
  { smProvenance :: [Text]
  , smHash       :: PluginHash
  , smTimestamp  :: Int64
  }
  deriving stock (Show, Eq, Generic)

-- ============================================================================
-- Service Protocol Types (re-exported from Plexus.Types with Service naming)
-- ============================================================================

-- | Service stream item - the universal transport envelope for streaming responses
--
-- This is a type alias for 'PlexusStreamItem' from the protocol layer,
-- renamed to use Service terminology at the application level.
type ServiceStreamItem = PlexusStreamItem

-- | Pattern synonym for data events
pattern ServiceData :: Text -> Provenance -> Text -> Value -> ServiceStreamItem
pattern ServiceData hash prov contentType content = StreamData hash prov contentType content

-- | Pattern synonym for progress events
pattern ServiceProgress :: Text -> Provenance -> Text -> Maybe Double -> ServiceStreamItem
pattern ServiceProgress hash prov msg pct = StreamProgress hash prov msg pct

-- | Pattern synonym for error events
pattern ServiceError :: Text -> Provenance -> Text -> Bool -> ServiceStreamItem
pattern ServiceError hash prov err recoverable = StreamError hash prov err recoverable

-- | Pattern synonym for done events
pattern ServiceDone :: Text -> Provenance -> ServiceStreamItem
pattern ServiceDone hash prov = StreamDone hash prov

-- | Pattern synonym for guidance events
pattern ServiceGuidance :: Text -> Provenance -> GuidanceErrorType -> GuidanceSuggestion -> Maybe [Text] -> Maybe Value -> ServiceStreamItem
pattern ServiceGuidance hash prov errType suggestion methods schema = StreamGuidance hash prov errType suggestion methods schema

-- | Pattern synonym for bidirectional request events
pattern ServiceRequest :: Text -> Provenance -> Text -> Request Value -> Maybe Int -> ServiceStreamItem
pattern ServiceRequest hash prov reqId reqData timeout = StreamRequest hash prov reqId reqData timeout
