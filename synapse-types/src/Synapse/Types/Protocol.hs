{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Protocol types for Plexus RPC using refined types
--
-- These types represent the core Plexus RPC protocol with compile-time
-- guarantees about validity. Invalid protocol states cannot be constructed.
module Synapse.Types.Protocol
  ( -- * Metadata
    StreamMetadata(..)
  , mkStreamMetadata
  , Provenance
  , mkProvenance

    -- * Stream Items
  , StreamItemType(..)
  , ContentType
  , mkContentType

    -- * JSON-RPC Envelope
  , jsonRpcVersion

    -- * Percentage
  , Percentage
  , mkPercentage
  , mkPercentageUnsafe
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import GHC.Generics (Generic)
import Refined hiding (NonEmpty)
import Refined.Unsafe (reallyUnsafeRefine)
import Synapse.Types.Refined

-------------------------------------------------------------------------------
-- Provenance

-- | Provenance chain: non-empty list of server names
--
-- Tracks the call path through the system:
-- - Initial: @["substrate"]@
-- - After routing: @["substrate", "health"]@
-- - Further routing: @["substrate", "health", "echo"]@
type Provenance = NonEmpty Text

-- | Smart constructor for Provenance
mkProvenance :: Text -> Provenance
mkProvenance name = NE.singleton name

-------------------------------------------------------------------------------
-- Metadata

-- | Stream metadata attached to most stream items
--
-- Required for every item: data, error, done, progress
data StreamMetadata = StreamMetadata
  { smProvenance :: !Provenance    -- ^ Call chain (non-empty)
  , smPlexusHash :: !PlexusHash     -- ^ Schema hash (16 hex chars)
  , smTimestamp  :: !Timestamp      -- ^ Unix seconds (MUST be integer!)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor for StreamMetadata
--
-- Validates all constraints:
-- - Provenance non-empty
-- - Hash exactly 16 hex characters
-- - Timestamp positive integer
mkStreamMetadata
  :: Text             -- ^ Server name for provenance
  -> Text             -- ^ Hash (will be validated)
  -> Int64            -- ^ Timestamp (will be validated)
  -> Either RefineException StreamMetadata
mkStreamMetadata serverName hashText ts = do
  hash <- mkPlexusHash hashText
  timestamp <- mkTimestamp ts
  let provenance = mkProvenance serverName
  return $ StreamMetadata provenance hash timestamp

-------------------------------------------------------------------------------
-- Stream Item Types

-- | Stream item type discriminator
data StreamItemType
  = DataItem      -- ^ Regular data item (has metadata)
  | ErrorItem     -- ^ Error termination (has metadata)
  | DoneItem      -- ^ Success termination (has metadata)
  | ProgressItem  -- ^ Progress update (has metadata)
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (FromJSON, ToJSON)

-------------------------------------------------------------------------------
-- Content Type

-- | Content type in namespace.operation format
--
-- Examples: @solar.result@, @echo.response@, @cone.chat_message@
--
-- Must be snake_case on wire (not camelCase)
type ContentType = SnakeCaseField

-- | Smart constructor for ContentType
mkContentType :: Text -> Either RefineException ContentType
mkContentType = mkSnakeCaseField

-------------------------------------------------------------------------------
-- Percentage

-- | Percentage value (0-100 inclusive)
type Percentage = Refined (From 0 `And` To 100) Int

-- | Smart constructor for Percentage
mkPercentage :: Int -> Either RefineException Percentage
mkPercentage = refine

-- | Unsafe constructor (throws on invalid input)
mkPercentageUnsafe :: Int -> Percentage
mkPercentageUnsafe n
  | n >= 0 && n <= 100 = reallyUnsafeRefine n
  | otherwise = error $ "Invalid percentage: " <> show n

-------------------------------------------------------------------------------
-- JSON-RPC Version

-- | JSON-RPC protocol version (always "2.0")
-- Note: Using a simple Text constant since this is always "2.0"
jsonRpcVersion :: Text
jsonRpcVersion = "2.0"
