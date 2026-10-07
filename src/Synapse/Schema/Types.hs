{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

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
  , pattern HubRequest

    -- * Bidirectional Types (re-exported)
  , Request(..)
  , StandardRequest
  , Response(..)
  , StandardResponse
  , SelectOption(..)
  ) where

import Data.Aeson (Value(..), encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Int (Int64)
import Data.Scientific (toRealFloat)
import GHC.Generics (Generic)

import Plexus.RemoteError (renderRemoteError)

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

-- | Pattern synonym for progress events: a message and a percentage.
--
--   Matching only (plexus 0.7: a progress frame carries the method's typed
--   progress value). The default progress type, @Note@, gives its
--   @message@ and @percentage@; any other value gives its JSON as the
--   message. RETURNS-14 renders typed progress properly.
pattern HubProgress :: Text -> Provenance -> Text -> Maybe Double -> HubStreamItem
pattern HubProgress hash prov msg pct <- StreamProgress hash prov _ (progressMessage -> (msg, pct))

-- | Pattern synonym for error events: the call failure as one line.
--
--   Matching only (plexus 0.7: an error frame carries a typed
--   'Plexus.RemoteError.RemoteError'); the message is its
--   'renderRemoteError' line. RETURNS-14 renders call failures properly.
pattern HubError :: Text -> Provenance -> Text -> Bool -> HubStreamItem
pattern HubError hash prov err recoverable <- StreamError hash prov (renderRemoteError -> err) recoverable

-- | A progress value as the 0.6-style message and percentage.
progressMessage :: Value -> (Text, Maybe Double)
progressMessage (Object o)
  | Just (String msg) <- KM.lookup "message" o =
      ( msg
      , case KM.lookup "percentage" o of
          Just (Number n) -> Just (toRealFloat n)
          _ -> Nothing
      )
progressMessage v = (TE.decodeUtf8 (LBS.toStrict (encode v)), Nothing)

-- | Pattern synonym for done events
pattern HubDone :: Text -> Provenance -> HubStreamItem
pattern HubDone hash prov = StreamDone hash prov

-- | Pattern synonym for guidance events
pattern HubGuidance :: Text -> Provenance -> GuidanceErrorType -> GuidanceSuggestion -> Maybe [Text] -> Maybe Value -> HubStreamItem
pattern HubGuidance hash prov errType suggestion methods schema = StreamGuidance hash prov errType suggestion methods schema

-- | Pattern synonym for bidirectional request events
pattern HubRequest :: Text -> Provenance -> Text -> Request Value -> Maybe Int -> HubStreamItem
pattern HubRequest hash prov reqId reqData timeout = StreamRequest hash prov reqId reqData timeout
