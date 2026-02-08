{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Demonstrates the two-call schema discovery pattern
--
-- 1. Call substrate.schema → get list of activations
-- 2. For each activation, call substrate.activation_schema → get method schemas
--
-- The ActivationInfo structure drives both:
-- - CLI subcommand generation (arbor, cone, health)
-- - Schema enrichment requests (fetch enriched schema for each namespace)

module Main where

import Data.Aeson (Value, encode, toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Streaming.Prelude as S

import Plexus (connect, disconnect, defaultConfig)
import Substrate.Client (SubstrateConnection, substrateRpc)
import Plexus.Schema
  ( PlexusSchema(..)
  , PlexusSchemaEvent(..)
  , ActivationInfo(..)
  , EnrichedSchema(..)
  , ActivationSchemaEvent(..)
  , extractSchemaEvent
  , extractActivationSchemaEvent
  )

main :: IO ()
main = do
  putStrLn "=== Schema Discovery Demo ==="
  putStrLn ""

  conn <- connect defaultConfig

  -- STEP 1: Discover all activations
  putStrLn "Step 1: Calling substrate.schema to discover activations..."
  mSchema <- S.head_ $ S.mapMaybe extractSchemaEvent $
    substrateRpc conn "substrate.schema" (toJSON ([] :: [Value]))

  case mSchema of
    Nothing -> putStrLn "Failed to get schema"
    Just (SchemaError err) -> T.putStrLn $ "Schema error: " <> err
    Just (SchemaData schema) -> do
      putStrLn $ "Found " <> show (length (schemaActivations schema)) <> " activations"
      putStrLn ""

      -- For each activation, show what we'd do
      mapM_ (demonstrateActivation conn) (schemaActivations schema)

  disconnect conn

-- | Demonstrate how an ActivationInfo drives both CLI and schema requests
demonstrateActivation :: SubstrateConnection -> ActivationInfo -> IO ()
demonstrateActivation plexusConn act = do
  let ns = activationNamespace act
      methods = activationMethods act

  putStrLn $ "Activation: " <> T.unpack ns
  putStrLn $ "  Description: " <> T.unpack (activationDescription act)
  putStrLn $ "  Methods: " <> show (length methods)

  -- STEP 2: Use the namespace to request enriched schema
  putStrLn $ "  → Requesting enriched schema for '" <> T.unpack ns <> "'..."

  mEnriched <- S.head_ $ S.mapMaybe extractActivationSchemaEvent $
    substrateRpc plexusConn "substrate.activation_schema" (toJSON [ns])

  case mEnriched of
    Nothing -> putStrLn "    Failed to get enriched schema"
    Just (ActivationSchemaError err) ->
      T.putStrLn $ "    Schema error: " <> err
    Just (ActivationSchemaData enriched) -> do
      let variantCount = maybe 0 length (schemaOneOf enriched)
      putStrLn $ "    ✓ Got enriched schema with " <> show variantCount <> " method variants"

      -- Show the mapping: methods[i] corresponds to oneOf[i]
      putStrLn "    Mapping (index-based):"
      mapM_ (\(idx, method) ->
        putStrLn $ "      [" <> show idx <> "] " <> T.unpack method)
        (zip [0..] methods)

      putStrLn ""

      -- This demonstrates:
      -- 1. ActivationInfo.namespace → used for substrate.activation_schema RPC
      -- 2. ActivationInfo.methods → used to know which method corresponds to which oneOf variant
      -- 3. Same structure drives CLI: "symbols-dyn <namespace> <method>"

-- | The key insight: ActivationInfo is the source of truth for both
--
-- For CLI generation:
--   - activationNamespace → subcommand name ("arbor")
--   - activationMethods → sub-subcommand names ("tree-create", "tree-list")
--
-- For schema enrichment:
--   - activationNamespace → RPC parameter: substrate.activation_schema("arbor")
--   - activationMethods[i] → maps to enrichedSchema.oneOf[i]
--
-- This creates a direct correspondence:
--
--   CLI Command              | Schema Lookup
--   -------------------------|----------------------------------
--   arbor tree-create        | enriched["arbor"].oneOf[0]
--   arbor tree-get           | enriched["arbor"].oneOf[1]
--   arbor tree-list          | enriched["arbor"].oneOf[3]
--   cone list                | enriched["cone"].oneOf[1]
--
-- The mapping is guaranteed by the order in ActivationInfo.methods
-- matching the order in the enriched schema's oneOf array.
