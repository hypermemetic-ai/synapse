{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Schema validation for _self test
--
-- Detects incompatibilities between different macro implementations
-- (hub_methods vs plexus-derive) by validating schema structure.

module Synapse.Self.SchemaValidator
  ( validateMethodSchema
  , SchemaIssue(..)
  , IssueSeverity(..)
  ) where

import Data.Aeson (Value(..), Object)
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

-- | Schema validation issue
data SchemaIssue = SchemaIssue
  { siMessage :: !Text
  , siSeverity :: !IssueSeverity
  , siExpected :: !(Maybe Text)
  , siActual :: !(Maybe Text)
  , siRecommendation :: !(Maybe Text)
  }
  deriving (Show, Eq)

data IssueSeverity = Critical | Warning | Info
  deriving (Show, Eq, Ord)

-- | Validate a method's schema structure
--
-- Checks for common issues that cause synapse client failures:
-- - Missing required fields
-- - Type mismatches
-- - Parameter schema format
-- - Response schema format
validateMethodSchema :: Text -> Value -> [SchemaIssue]
validateMethodSchema methodName schema = case schema of
  Object obj ->
    let nameIssues = validateMethodName methodName obj
        paramsIssues = validateParameters obj
        responseIssues = validateResponse obj
        metadataIssues = validateMetadata obj
    in concat [nameIssues, paramsIssues, responseIssues, metadataIssues]

  _ ->
    [ SchemaIssue
        { siMessage = "Schema is not a JSON object"
        , siSeverity = Critical
        , siExpected = Just "JSON object with method, params, returns fields"
        , siActual = Just $ T.pack $ show schema
        , siRecommendation = Just "Check macro implementation - schema must be an object"
        }
    ]

-- | Validate method name matches schema
validateMethodName :: Text -> Object -> [SchemaIssue]
validateMethodName methodName obj = case KM.lookup "method" obj of
  Nothing ->
    [ SchemaIssue
        { siMessage = "Schema missing 'method' field"
        , siSeverity = Critical
        , siExpected = Just "method field with method name"
        , siActual = Just "missing"
        , siRecommendation = Just "Macro should include method name in schema"
        }
    ]

  Just (String schemaMethod) ->
    if schemaMethod == methodName
      then []
      else
        [ SchemaIssue
            { siMessage = "Method name mismatch"
            , siSeverity = Critical
            , siExpected = Just methodName
            , siActual = Just schemaMethod
            , siRecommendation = Just "Schema method name should match call name"
            }
        ]

  Just other ->
    [ SchemaIssue
        { siMessage = "Method field is not a string"
        , siSeverity = Critical
        , siExpected = Just "string"
        , siActual = Just $ T.pack $ show other
        , siRecommendation = Nothing
        }
    ]

-- | Validate parameters schema
validateParameters :: Object -> [SchemaIssue]
validateParameters obj = case KM.lookup "params" obj of
  Nothing ->
    [ SchemaIssue
        { siMessage = "Schema missing 'params' field"
        , siSeverity = Warning
        , siExpected = Just "params field (array or object)"
        , siActual = Just "missing"
        , siRecommendation = Just "Methods should declare parameters even if empty"
        }
    ]

  Just (Array _) -> []  -- Array of parameter definitions is valid
  Just (Object _) -> []  -- Object with parameter definitions is valid

  Just other ->
    [ SchemaIssue
        { siMessage = "Parameters field has invalid type"
        , siSeverity = Critical
        , siExpected = Just "array or object"
        , siActual = Just $ T.pack $ show other
        , siRecommendation = Just "Check macro parameter schema generation"
        }
    ]

-- | Validate response/returns schema
validateResponse :: Object -> [SchemaIssue]
validateResponse obj =
  let hasReturns = KM.member "returns" obj
      hasResponse = KM.member "response" obj
  in if hasReturns || hasResponse
       then []
       else
         [ SchemaIssue
             { siMessage = "Schema missing 'returns' or 'response' field"
             , siSeverity = Warning
             , siExpected = Just "returns or response field describing output"
             , siActual = Just "missing"
             , siRecommendation = Just "Methods should describe their return type"
             }
         ]

-- | Validate schema metadata
validateMetadata :: Object -> [SchemaIssue]
validateMetadata obj =
  let namespaceIssues = checkField "namespace" "Namespace helps organize methods"
      versionIssues = checkField "version" "Version aids in API compatibility tracking"
      descIssues = checkField "description" "Description helps users understand method purpose"
  in concat [namespaceIssues, versionIssues, descIssues]
  where
    checkField :: Text -> Text -> [SchemaIssue]
    checkField fieldName reason = case KM.lookup fieldName obj of
      Nothing ->
        [ SchemaIssue
            { siMessage = "Schema missing '" <> fieldName <> "' field"
            , siSeverity = Info
            , siExpected = Just $ fieldName <> " field"
            , siActual = Just "missing"
            , siRecommendation = Just reason
            }
        ]
      Just (String s) ->
        if T.null s
          then
            [ SchemaIssue
                { siMessage = fieldName <> " field is empty"
                , siSeverity = Info
                , siExpected = Just "non-empty string"
                , siActual = Just "empty string"
                , siRecommendation = Just reason
                }
            ]
          else []
      Just _ -> []  -- Has the field, not a string but that's okay

-- | Compare two schemas and report differences
--
-- This is useful for comparing hub_methods vs plexus-derive schemas
compareSchemas :: Text -> Value -> Value -> [SchemaIssue]
compareSchemas methodName schema1 schema2 = case (schema1, schema2) of
  (Object obj1, Object obj2) ->
    let keys1 = KM.keys obj1
        keys2 = KM.keys obj2
        missingIn2 = filter (`notElem` KM.keys obj2) keys1
        missingIn1 = filter (`notElem` KM.keys obj1) keys2

        missingIssues = map (mkMissingIssue "Schema 1") missingIn2
                     ++ map (mkMissingIssue "Schema 2") missingIn1
    in missingIssues

  _ -> []
  where
    mkMissingIssue source key =
      SchemaIssue
        { siMessage = "Schema difference detected"
        , siSeverity = Warning
        , siExpected = Just $ "Both schemas have '" <> T.pack (show key) <> "'"
        , siActual = Just $ source <> " missing '" <> T.pack (show key) <> "'"
        , siRecommendation = Just "This may cause client compatibility issues"
        }
