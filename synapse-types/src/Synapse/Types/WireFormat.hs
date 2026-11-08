{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Wire format types with GADTs enforcing metadata requirements
--
-- This module uses GADTs to enforce at the type level that every stream
-- item carries metadata. This prevents bugs where metadata is forgotten.
-- (spec/08's request item, the one without metadata, is gone with bidir:
-- ASK-1, ASK-3.)
module Synapse.Types.WireFormat
  ( -- * Stream Items (GADT)
    StreamItem(..)
  , ItemType(..)

    -- * Helper Types
  , ErrorCode
  , mkErrorCode
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Refined
import Synapse.Types.Protocol
import Synapse.Types.Refined

-------------------------------------------------------------------------------
-- Item Type (Type-Level)

-- | Stream item type at the type level
--
-- Used as a phantom type parameter to StreamItem GADT
data ItemType
  = TData      -- ^ Data item (metadata required)
  | TError     -- ^ Error item (metadata required)
  | TDone      -- ^ Done item (metadata required)
  | TProgress  -- ^ Progress item (metadata required)

-------------------------------------------------------------------------------
-- Stream Item GADT

-- | Stream item with metadata requirements enforced by GADT
--
-- The type parameter indicates what kind of item this is,
-- and the GADT structure ensures metadata is present only
-- where required.
--
-- Example:
--
-- @
-- -- ✅ DataItem requires metadata
-- dataItem :: StreamItem 'TData
-- dataItem = DataItem metadata contentType content
--
-- -- ❌ This won't compile (missing metadata):
-- -- bad = DataItem contentType content
-- @
data StreamItem (t :: ItemType) where
  -- | Data item with metadata
  --
  -- Contains actual response data from a method call.
  DataItem
    :: { diMetadata    :: !StreamMetadata  -- ^ Metadata (required!)
       , diContentType :: !ContentType     -- ^ Content type (snake_case)
       , diContent     :: !Value           -- ^ Actual data payload
       }
    -> StreamItem 'TData

  -- | Error item with metadata
  --
  -- Terminates subscription with an error.
  ErrorItem
    :: { eiMetadata    :: !StreamMetadata  -- ^ Metadata (required!)
       , eiMessage     :: !Text            -- ^ Error message
       , eiCode        :: !(Maybe ErrorCode)  -- ^ Optional error code
       , eiRecoverable :: !Bool            -- ^ Whether error is recoverable
       }
    -> StreamItem 'TError

  -- | Done item with metadata
  --
  -- Terminates subscription successfully.
  DoneItem
    :: { doneMetadata :: !StreamMetadata   -- ^ Metadata (required!)
       }
    -> StreamItem 'TDone

  -- | Progress item with metadata
  --
  -- Reports progress of long-running operation.
  ProgressItem
    :: { piMetadata   :: !StreamMetadata   -- ^ Metadata (required!)
       , piMessage    :: !Text             -- ^ Progress message
       , piPercentage :: !Percentage       -- ^ Progress (0-100)
       }
    -> StreamItem 'TProgress

-- Deriving Show for GADT requires StandaloneDeriving
deriving instance Show (StreamItem t)
deriving instance Eq (StreamItem t)

-------------------------------------------------------------------------------
-- Helper Types

-- | Error code (snake_case)
type ErrorCode = SnakeCaseField

-- | Smart constructor for ErrorCode
mkErrorCode :: Text -> Either RefineException ErrorCode
mkErrorCode = mkSnakeCaseField

-------------------------------------------------------------------------------
-- Examples and Documentation

-- $examples
--
-- = Creating stream items
--
-- @
-- import Synapse.Types.WireFormat
-- import Synapse.Types.Protocol
-- import Synapse.Types.Refined
--
-- -- Create metadata
-- Right meta <- mkStreamMetadata "substrate" "abc123def4567890" 1735052400
-- Right contentType <- mkContentType "solar.result"
--
-- -- ✅ Create data item (metadata required)
-- dataItem :: StreamItem 'TData
-- dataItem = DataItem meta contentType someValue
--
-- -- ❌ This won't compile (missing metadata):
-- -- bad = DataItem contentType someValue
-- --       ^^^ Type error: expected 3 arguments, got 2
-- @
--
-- = Type safety in action
--
-- The GADT structure prevents common bugs:
--
-- 1. **Forgetting metadata**: Won't compile
--
-- @
-- -- ❌ Forgot metadata
-- bad1 = DataItem contentType value
-- -- Type error: Missing metadata argument
-- @
--
-- 2. **Wrong item type**: Caught by type checker
--
-- @
-- sendDataItem :: StreamItem 'TData -> IO ()
--
-- -- ❌ Type mismatch
-- sendDataItem doneItem
-- -- Type error: Expected TData, got TDone
-- @
