{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Synapse.Self.Protocol.Validator
  ( -- * Types
    ProtocolViolation(..)
  , Severity(..)
  , Location(..)
  , ValidationContext(..)
  , ValidationConfig(..)

    -- * Validation functions
  , validateStreamItem
  , validateMetadata
  , validateFieldNames
  , validateDataItem
  , validateProgressItem
  , validateErrorItem
  , validateDoneItem
  ) where

import Data.Aeson (Value(..), Object)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit, isHexDigit, isLower)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (isInteger, floatingOrInteger)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Protocol violation with precise location and suggested fix
data ProtocolViolation = ProtocolViolation
  { pvMessage :: !Text
  , pvSeverity :: !Severity
  , pvLocation :: !Location
  , pvExpected :: !(Maybe Text)
  , pvActual :: !(Maybe Text)
  , pvFix :: !(Maybe Text)  -- ^ Suggested fix
  }
  deriving (Show, Eq)

data Severity = Error | Warning | Info
  deriving (Show, Eq, Ord)

data Location
  = InMessage
      { lmSubscriptionId :: !(Maybe Int)
      , lmMessageIndex :: !Int
      , lmField :: !(Maybe Text)
      }
  | InStream
      { lsMethod :: !Text
      , lsMessageCount :: !Int
      }
  | InConnection
  deriving (Show, Eq)

-- | Validation context tracking stream state
data ValidationContext = ValidationContext
  { vcViolations :: ![ProtocolViolation]
  , vcConfig :: !ValidationConfig
  }
  deriving (Show, Eq)

data ValidationConfig = ValidationConfig
  { vcCheckMetadata :: !Bool
  , vcCheckStreamDone :: !Bool
  , vcCheckProvenance :: !Bool
  , vcCheckTypes :: !Bool
  , vcStrictMode :: !Bool  -- ^ Fail on warnings
  }
  deriving (Show, Eq)

-- | Default validation config (check everything)
defaultValidationConfig :: ValidationConfig
defaultValidationConfig = ValidationConfig
  { vcCheckMetadata = True
  , vcCheckStreamDone = True
  , vcCheckProvenance = True
  , vcCheckTypes = True
  , vcStrictMode = False
  }

-- ============================================================================
-- Main Validation Function
-- ============================================================================

-- | Validate a stream item
--
-- Checks:
-- - Message structure (type field)
-- - Metadata structure (provenance, plexus_hash, timestamp)
-- - Field naming (snake_case for protocol fields)
-- - Type-specific validation (data, error, progress, done)
validateStreamItem :: Value -> IO [ProtocolViolation]
validateStreamItem item = pure $ case item of
  Object obj ->
    let typeViolations = validateType obj
        metadataViolations = validateMetadata item
        fieldNameViolations = validateFieldNames item
        typeSpecificViolations = validateByType obj
    in concat [typeViolations, metadataViolations, fieldNameViolations, typeSpecificViolations]
  _ ->
    [ ProtocolViolation
        { pvMessage = "Stream item must be a JSON object"
        , pvSeverity = Error
        , pvLocation = InConnection
        , pvExpected = Just "JSON object with 'type' field"
        , pvActual = Just $ T.pack $ show item
        , pvFix = Just "Ensure stream items are JSON objects"
        }
    ]

-- ============================================================================
-- Type Field Validation
-- ============================================================================

-- | Validate that the type field exists and is valid
validateType :: Object -> [ProtocolViolation]
validateType obj = case KM.lookup "type" obj of
  Nothing ->
    [ ProtocolViolation
        { pvMessage = "Missing 'type' field in stream item"
        , pvSeverity = Error
        , pvLocation = InConnection
        , pvExpected = Just "type field with value: data, error, progress or done"
        , pvActual = Just "missing"
        , pvFix = Just "Add 'type' field to stream item"
        }
    ]
  Just (String typeStr) ->
    -- ASK-3: spec/08's request item is gone with bidir (ASK-1)
    if typeStr `elem` ["data", "error", "progress", "done"]
      then []
      else
        [ ProtocolViolation
            { pvMessage = "Invalid stream item type: " <> typeStr
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "data, error, progress or done"
            , pvActual = Just typeStr
            , pvFix = Just "Use one of the valid stream item types"
            }
        ]
  Just other ->
    [ ProtocolViolation
        { pvMessage = "Stream item 'type' must be a string"
        , pvSeverity = Error
        , pvLocation = InConnection
        , pvExpected = Just "string"
        , pvActual = Just $ T.pack $ show other
        , pvFix = Just "Change type value to a string"
        }
    ]

-- ============================================================================
-- Metadata Validation
-- ============================================================================

-- | Validate stream metadata structure
--
-- Checks:
-- - Provenance is array of strings
-- - plexus_hash is 16-character hex string
-- - Timestamp is valid (integer, Unix seconds or ISO 8601)
validateMetadata :: Value -> [ProtocolViolation]
validateMetadata (Object obj) =
  -- Check if this item type should have metadata
  -- Every item carries metadata
  case () of
    _ ->
      case KM.lookup "metadata" obj of
        Nothing ->
          [ ProtocolViolation
              { pvMessage = "Missing 'metadata' field (required for data, error, progress, done items)"
              , pvSeverity = Error
              , pvLocation = InConnection
              , pvExpected = Just "metadata object with provenance, plexus_hash, timestamp"
              , pvActual = Just "missing"
              , pvFix = Just "Add metadata field to stream item"
              }
          ]
        Just (Object metaObj) ->
          concat
            [ validateProvenance metaObj
            , validatePlexusHash metaObj
            , validateTimestamp metaObj
            ]
        Just other ->
          [ ProtocolViolation
              { pvMessage = "Metadata must be a JSON object"
              , pvSeverity = Error
              , pvLocation = InConnection
              , pvExpected = Just "object"
              , pvActual = Just $ T.pack $ show other
              , pvFix = Just "Change metadata to an object"
              }
          ]
validateMetadata _ = []

-- | Validate provenance field (must be non-empty array of strings)
validateProvenance :: Object -> [ProtocolViolation]
validateProvenance obj = case KM.lookup "provenance" obj of
  Nothing ->
    [ ProtocolViolation
        { pvMessage = "Missing 'provenance' field in metadata"
        , pvSeverity = Error
        , pvLocation = InConnection
        , pvExpected = Just "non-empty array of strings"
        , pvActual = Just "missing"
        , pvFix = Just "Add provenance field with server names"
        }
    ]
  Just (Array arr) ->
    if V.null arr
      then
        [ ProtocolViolation
            { pvMessage = "Provenance array cannot be empty"
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "non-empty array of strings"
            , pvActual = Just "empty array"
            , pvFix = Just "Add at least one server name to provenance"
            }
        ]
      else
        -- Check each element is a string
        concatMap (validateProvenanceElement arr) (zip [0..] (V.toList arr))
  Just other ->
    [ ProtocolViolation
        { pvMessage = "Provenance must be an array"
        , pvSeverity = Error
        , pvLocation = InConnection
        , pvExpected = Just "array of strings"
        , pvActual = Just $ T.pack $ show other
        , pvFix = Just "Change provenance to an array of server names"
        }
    ]

-- | Validate a single provenance element
validateProvenanceElement :: Vector Value -> (Int, Value) -> [ProtocolViolation]
validateProvenanceElement _arr (idx, String _) = []
validateProvenanceElement _arr (idx, other) =
  [ ProtocolViolation
      { pvMessage = "Provenance element at index " <> T.pack (show idx) <> " must be a string"
      , pvSeverity = Error
      , pvLocation = InConnection
      , pvExpected = Just "string"
      , pvActual = Just $ T.pack $ show other
      , pvFix = Just "Ensure all provenance elements are strings"
      }
  ]

-- | Validate plexus_hash field (must be exactly 16 hex characters)
validatePlexusHash :: Object -> [ProtocolViolation]
validatePlexusHash obj = case KM.lookup "plexus_hash" obj of
  Nothing ->
    [ ProtocolViolation
        { pvMessage = "Missing 'plexus_hash' field in metadata"
        , pvSeverity = Error
        , pvLocation = InConnection
        , pvExpected = Just "16-character lowercase hex string"
        , pvActual = Just "missing"
        , pvFix = Just "Add plexus_hash field with schema version hash"
        }
    ]
  Just (String hashStr) ->
    let len = T.length hashStr
        isAllHex = T.all (\c -> isHexDigit c && (isDigit c || isLower c)) hashStr
    in concat
      [ if len /= 16
          then
            [ ProtocolViolation
                { pvMessage = "plexus_hash must be exactly 16 characters (got " <> T.pack (show len) <> ")"
                , pvSeverity = Error
                , pvLocation = InConnection
                , pvExpected = Just "16 characters"
                , pvActual = Just $ T.pack (show len) <> " characters: " <> hashStr
                , pvFix = Just "Use 16-character hex hash (SHA-256 truncated to 64 bits)"
                }
            ]
          else []
      , if not isAllHex
          then
            [ ProtocolViolation
                { pvMessage = "plexus_hash must be lowercase hexadecimal"
                , pvSeverity = Error
                , pvLocation = InConnection
                , pvExpected = Just "lowercase hex characters (0-9, a-f)"
                , pvActual = Just hashStr
                , pvFix = Just "Use only lowercase hexadecimal characters"
                }
            ]
          else []
      ]
  Just other ->
    [ ProtocolViolation
        { pvMessage = "plexus_hash must be a string"
        , pvSeverity = Error
        , pvLocation = InConnection
        , pvExpected = Just "string"
        , pvActual = Just $ T.pack $ show other
        , pvFix = Just "Change plexus_hash to a string"
        }
    ]

-- | Validate timestamp field (must be positive integer)
validateTimestamp :: Object -> [ProtocolViolation]
validateTimestamp obj = case KM.lookup "timestamp" obj of
  Nothing ->
    [ ProtocolViolation
        { pvMessage = "Missing 'timestamp' field in metadata"
        , pvSeverity = Error
        , pvLocation = InConnection
        , pvExpected = Just "positive integer (Unix seconds)"
        , pvActual = Just "missing"
        , pvFix = Just "Add timestamp field with Unix epoch seconds"
        }
    ]
  Just (Number n) ->
    case floatingOrInteger n of
      Left (_ :: Double) ->
        [ ProtocolViolation
            { pvMessage = "Timestamp must be an integer (not float)"
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "integer (use Math.floor(Date.now() / 1000) in JavaScript)"
            , pvActual = Just $ T.pack $ show n
            , pvFix = Just "Convert timestamp to integer: Math.floor(Date.now() / 1000)"
            }
        ]
      Right (ts :: Integer) ->
        if ts <= 0
          then
            [ ProtocolViolation
                { pvMessage = "Timestamp must be positive"
                , pvSeverity = Error
                , pvLocation = InConnection
                , pvExpected = Just "positive integer"
                , pvActual = Just $ T.pack $ show ts
                , pvFix = Just "Use positive Unix timestamp"
                }
            ]
          else []
  Just other ->
    [ ProtocolViolation
        { pvMessage = "Timestamp must be a number"
        , pvSeverity = Error
        , pvLocation = InConnection
        , pvExpected = Just "integer"
        , pvActual = Just $ T.pack $ show other
        , pvFix = Just "Change timestamp to an integer"
        }
    ]

-- ============================================================================
-- Field Naming Validation
-- ============================================================================

-- | Validate field naming conventions (snake_case for protocol fields)
validateFieldNames :: Value -> [ProtocolViolation]
validateFieldNames (Object obj) =
  let protocolFields = ["jsonrpc", "subscription_id", "method", "params", "result", "id"]
      camelCaseFields = ["subscriptionId", "plexusHash", "contentType", "requestId"]
      violations = concatMap (checkFieldNaming obj) (KM.keys obj)
  in violations
validateFieldNames _ = []

-- | Check if a field name violates naming conventions
checkFieldNaming :: Object -> K.Key -> [ProtocolViolation]
checkFieldNaming obj key =
  let keyText = K.toText key
      isCamelCase = T.any (\c -> c >= 'A' && c <= 'Z') keyText
      isProtocolField = keyText `elem` ["type", "metadata", "content_type", "content",
                                         "message", "percentage", "code", "plexus_hash",
                                         "provenance", "timestamp"]
  in if isCamelCase && isProtocolField
       then
         [ ProtocolViolation
             { pvMessage = "Protocol field '" <> keyText <> "' uses camelCase (should be snake_case)"
             , pvSeverity = Warning
             , pvLocation = InConnection
             , pvExpected = Just $ toSnakeCase keyText
             , pvActual = Just keyText
             , pvFix = Just $ "Rename to: " <> toSnakeCase keyText
             }
         ]
       else []

-- | Convert camelCase to snake_case (simple version)
toSnakeCase :: Text -> Text
toSnakeCase txt = T.pack $ go (T.unpack txt)
  where
    go [] = []
    go (c:cs)
      | c >= 'A' && c <= 'Z' = '_' : toLowerChar c : go cs  -- Convert to lowercase and prepend underscore
      | otherwise = c : go cs
    toLowerChar c = toEnum (fromEnum c + 32)

-- ============================================================================
-- Type-Specific Validation
-- ============================================================================

-- | Validate based on stream item type
validateByType :: Object -> [ProtocolViolation]
validateByType obj = case KM.lookup "type" obj of
  Just (String "data") -> validateDataItem (Object obj)
  Just (String "progress") -> validateProgressItem (Object obj)
  Just (String "error") -> validateErrorItem (Object obj)
  Just (String "done") -> validateDoneItem (Object obj)
  _ -> []

-- | Validate data item structure
validateDataItem :: Value -> [ProtocolViolation]
validateDataItem (Object obj) = concat
  [ case KM.lookup "content_type" obj of
      Nothing ->
        [ ProtocolViolation
            { pvMessage = "Missing 'content_type' field in data item"
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "content_type string (e.g., 'echo.response')"
            , pvActual = Just "missing"
            , pvFix = Just "Add content_type field"
            }
        ]
      Just (String ct) ->
        if T.null ct
          then
            [ ProtocolViolation
                { pvMessage = "content_type cannot be empty"
                , pvSeverity = Error
                , pvLocation = InConnection
                , pvExpected = Just "non-empty string"
                , pvActual = Just "empty string"
                , pvFix = Just "Provide valid content_type"
                }
            ]
          else []
      Just other ->
        [ ProtocolViolation
            { pvMessage = "content_type must be a string"
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "string"
            , pvActual = Just $ T.pack $ show other
            , pvFix = Just "Change content_type to string"
            }
        ]
  , case KM.lookup "content" obj of
      Nothing ->
        [ ProtocolViolation
            { pvMessage = "Missing 'content' field in data item"
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "content object with domain data"
            , pvActual = Just "missing"
            , pvFix = Just "Add content field"
            }
        ]
      Just _ -> []  -- Any JSON value is valid for content
  ]
validateDataItem _ = []

-- | Validate progress item structure
validateProgressItem :: Value -> [ProtocolViolation]
validateProgressItem (Object obj) = concat
  [ case KM.lookup "message" obj of
      Nothing ->
        [ ProtocolViolation
            { pvMessage = "Missing 'message' field in progress item"
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "message string"
            , pvActual = Just "missing"
            , pvFix = Just "Add message field"
            }
        ]
      Just (String _) -> []
      Just other ->
        [ ProtocolViolation
            { pvMessage = "Progress message must be a string"
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "string"
            , pvActual = Just $ T.pack $ show other
            , pvFix = Just "Change message to string"
            }
        ]
  , case KM.lookup "percentage" obj of
      Nothing -> []  -- percentage is optional
      Just Null -> []  -- null is allowed
      Just (Number n) ->
        case floatingOrInteger n of
          Left (_ :: Double) -> []  -- Allow float percentages
          Right (pct :: Integer) ->
            if pct < 0 || pct > 100
              then
                [ ProtocolViolation
                    { pvMessage = "Progress percentage must be between 0 and 100"
                    , pvSeverity = Error
                    , pvLocation = InConnection
                    , pvExpected = Just "0-100"
                    , pvActual = Just $ T.pack $ show pct
                    , pvFix = Just "Use percentage value between 0 and 100"
                    }
                ]
              else []
      Just other ->
        [ ProtocolViolation
            { pvMessage = "Progress percentage must be a number or null"
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "number or null"
            , pvActual = Just $ T.pack $ show other
            , pvFix = Just "Change percentage to number or null"
            }
        ]
  ]
validateProgressItem _ = []

-- | Validate error item structure
validateErrorItem :: Value -> [ProtocolViolation]
validateErrorItem (Object obj) = concat
  [ case KM.lookup "message" obj of
      Nothing ->
        [ ProtocolViolation
            { pvMessage = "Missing 'message' field in error item"
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "message string"
            , pvActual = Just "missing"
            , pvFix = Just "Add message field with error description"
            }
        ]
      Just (String msg) ->
        if T.null msg
          then
            [ ProtocolViolation
                { pvMessage = "Error message cannot be empty"
                , pvSeverity = Warning
                , pvLocation = InConnection
                , pvExpected = Just "non-empty error message"
                , pvActual = Just "empty string"
                , pvFix = Just "Provide descriptive error message"
                }
            ]
          else []
      Just other ->
        [ ProtocolViolation
            { pvMessage = "Error message must be a string"
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "string"
            , pvActual = Just $ T.pack $ show other
            , pvFix = Just "Change message to string"
            }
        ]
  , case KM.lookup "code" obj of
      Nothing -> []  -- code is optional
      Just Null -> []  -- null is allowed
      Just (String _) -> []  -- string code is valid
      Just other ->
        [ ProtocolViolation
            { pvMessage = "Error code must be a string or null"
            , pvSeverity = Error
            , pvLocation = InConnection
            , pvExpected = Just "string or null"
            , pvActual = Just $ T.pack $ show other
            , pvFix = Just "Change code to string or null"
            }
        ]
  ]
validateErrorItem _ = []

-- | Validate done item structure (only needs metadata)
validateDoneItem :: Value -> [ProtocolViolation]
validateDoneItem _item = []  -- Done items only need metadata, which is validated separately
