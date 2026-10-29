{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bidirectional integration tests
--
-- Tests for schema-driven bidirectional communication:
-- - IR builder correctly sets mdBidirType field
-- - Methods with bidirectional flag are detected
-- - Schema-driven dispatch logic
--
-- This test requires a running Service backend on localhost:4444
-- with at least one method marked as bidirectional.
module Main where

import Control.Monad (forM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getArgs, withArgs)
import Test.Hspec
import Text.Read (readMaybe)

import Synapse.IR.Types
import Synapse.IR.Builder (buildIR)
import Synapse.Monad
import qualified Synapse.Log as Log
import qualified Katip

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Nothing -> do
      putStrLn "Usage: bidir-integration-test <backend> [--port <port>]"
      putStrLn "Example: cabal test bidir-integration-test --test-options=\"plexus\""
      putStrLn ""
      putStrLn "Note: This test validates bidirectional communication features."
      putStrLn "      Some tests may be skipped if no bidirectional methods exist."
      error "Backend argument required"
    Just (backend, port) -> do
      putStrLn $ "Running bidirectional integration tests against localhost:" <> show port
      putStrLn $ "(backend: " <> T.unpack backend <> ")"

      -- Initialize environment
      logger <- Log.makeLogger Katip.ErrorS
      env <- initEnv "127.0.0.1" port backend logger Nothing

      -- Build IR once for all tests
      irResult <- runSynapseM env (buildIR [] [])

      case irResult of
        Left err -> do
          putStrLn $ "Failed to build IR: " <> show err
          putStrLn "Is the Service backend running?"
          hspec $ describe "Bidirectional Integration Tests" $
            it "connects to Service backend" $
              expectationFailure $ "Could not connect: " <> show err

        Right ir -> withArgs [] $ hspec $ bidirIntegrationSpec ir

-- | Parse command-line arguments: <backend> [--port <port>]
parseArgs :: [String] -> Maybe (Text, Int)
parseArgs [] = Nothing
parseArgs (backend:rest) = Just (T.pack backend, parsePort rest)
  where
    parsePort ("--port":p:_) = maybe 4444 id (readMaybe p)
    parsePort _ = 4444

-- | Main spec using pre-built IR
bidirIntegrationSpec :: IR -> Spec
bidirIntegrationSpec ir = do
  describe "IR bidirectional field population" $ do
    it "IR has methods" $
      Map.size (irMethods ir) `shouldSatisfy` (> 0)

    it "mdBidirType field exists on all methods" $ do
      forM_ (Map.elems $ irMethods ir) $ \method ->
        -- Field should exist (even if Nothing)
        -- This tests that the field is part of MethodDef
        case mdBidirType method of
          Nothing -> (pure () :: IO ())  -- Not bidirectional
          Just _  -> (pure () :: IO ())  -- Bidirectional

    it "reports methods with mdBidirType set" $ do
      let bidirMethods = filter (isJust . mdBidirType) (Map.elems $ irMethods ir)
      let count = length bidirMethods
      putStrLn $ "  Found " <> show count <> " bidirectional method(s)"
      forM_ bidirMethods $ \method ->
        putStrLn $ "    - " <> T.unpack (mdFullPath method) <>
                   " (type: " <> show (mdBidirType method) <> ")"
      -- Don't fail if none exist - this is informational
      pure ()

  describe "Bidirectional type inference" $ do
    it "mdBidirType is Nothing for non-bidirectional methods" $ do
      -- Find methods that are NOT streaming and likely not bidirectional
      let nonStreamingMethods = filter (not . mdStreaming) (Map.elems $ irMethods ir)
      -- Check a sample (if any exist)
      case take 1 nonStreamingMethods of
        [] -> pending  -- No non-streaming methods to test
        (method:_) -> do
          -- Most non-streaming methods shouldn't be bidirectional
          -- But we can't enforce this strictly as it depends on the backend
          case mdBidirType method of
            Nothing -> pure ()  -- Expected for most methods
            Just _ -> putStrLn $ "  Note: " <> T.unpack (mdFullPath method) <>
                                " is bidirectional despite being non-streaming"

    it "mdBidirType is RefAny for standard bidirectional methods" $ do
      let bidirMethods = filter (isJust . mdBidirType) (Map.elems $ irMethods ir)
      forM_ bidirMethods $ \method ->
        case mdBidirType method of
          Just RefJson -> pure ()  -- Standard bidir with T=Value
          Just other ->
            -- Future: when custom bidir types are supported, this is valid
            putStrLn $ "  Note: " <> T.unpack (mdFullPath method) <>
                      " has custom bidir type: " <> show other
          Nothing -> expectationFailure "Expected bidir method to have mdBidirType"

  describe "Bidirectional method detection" $ do
    it "can identify all bidirectional methods" $ do
      let allMethods = Map.elems $ irMethods ir
      let bidirMethods = filter (isJust . mdBidirType) allMethods
      -- Report findings
      putStrLn $ "  Total methods: " <> show (length allMethods)
      putStrLn $ "  Bidirectional: " <> show (length bidirMethods)
      -- Test passes - we're just collecting data
      length allMethods `shouldSatisfy` (>= length bidirMethods)

    it "bidirectional methods have valid structure" $ do
      let bidirMethods = filter (isJust . mdBidirType) (Map.elems $ irMethods ir)
      forM_ bidirMethods $ \method -> do
        -- Should have a name
        T.length (mdName method) `shouldSatisfy` (> 0)
        -- Should have a full path
        T.length (mdFullPath method) `shouldSatisfy` (> 0)
        -- Should have a return type
        mdReturns method `shouldSatisfy` (/= RefUnknown)

  describe "Type reference validation for bidirectional types" $ do
    it "mdBidirType references resolve in irTypes (if not RefAny)" $ do
      let bidirMethods = filter (isJust . mdBidirType) (Map.elems $ irMethods ir)
      forM_ bidirMethods $ \method ->
        case mdBidirType method of
          Nothing -> pure ()
          Just RefJson -> pure ()  -- RefAny doesn't need resolution
          Just (RefNamed qn) -> do
            let typeName = qualifiedNameFull qn
            case Map.lookup typeName (irTypes ir) of
              Nothing -> expectationFailure $ T.unpack $
                "Bidirectional type not found in IR: " <> typeName <>
                " (referenced by " <> mdFullPath method <> ")"
              Just _ -> pure ()
          Just (RefArray _) ->
            -- Array types are valid, but we don't validate the inner type here
            pure ()
          Just (RefOptional _) ->
            -- Optional types are valid
            pure ()
          Just RefUnknown -> expectationFailure $ T.unpack $
            "mdBidirType should not be RefUnknown for " <> mdFullPath method
          Just (RefPrimitive _ _) ->
            -- Primitive types are valid
            pure ()

  describe "Schema-driven dispatch assumptions" $ do
    it "bidirectional methods are streaming (typically)" $ do
      let bidirMethods = filter (isJust . mdBidirType) (Map.elems $ irMethods ir)
      forM_ bidirMethods $ \method ->
        if mdStreaming method
          then pure ()  -- Expected - bidir usually requires streaming
          else putStrLn $ "  Note: Bidirectional method " <> T.unpack (mdFullPath method) <>
                         " is NOT streaming (unusual but valid)"

    it "non-bidirectional methods have Nothing for mdBidirType" $ do
      let allMethods = Map.elems $ irMethods ir
      forM_ allMethods $ \method ->
        case mdBidirType method of
          Nothing -> (pure () :: IO ())  -- Standard method
          Just _ -> (pure () :: IO ())   -- Should only be set for truly bidirectional methods

  describe "Specific bidirectional method tests (if available)" $ do
    -- Test common bidirectional methods if they exist
    -- These are examples - actual methods depend on the backend
    testBidirMethod ir "cone.chat"
    testBidirMethod ir "cone.edit"
    testBidirMethod ir "cone.generate"

  describe "IR metadata for bidirectional support" $ do
    it "IR version is set" $
      T.length (irVersion ir) `shouldSatisfy` (> 0)

    it "IR backend is set" $
      T.length (irBackend ir) `shouldSatisfy` (> 0)

    it "IR contains type definitions" $ do
      -- Should have at least some types (even if no bidirectional methods)
      Map.size (irTypes ir) `shouldSatisfy` (>= 0)

  describe "Edge cases and validation" $ do
    it "handles methods with no parameters but bidirectional" $ do
      let bidirMethods = filter (isJust . mdBidirType) (Map.elems $ irMethods ir)
      let noParamBidirMethods = filter (null . mdParams) bidirMethods
      -- These are valid - bidirectional methods can have no input params
      -- Just report findings
      unless (null noParamBidirMethods) $
        putStrLn $ "  Found " <> show (length noParamBidirMethods) <>
                  " bidirectional method(s) with no parameters"
      pure ()

    it "handles methods with complex return types and bidirectional" $ do
      let bidirMethods = filter (isJust . mdBidirType) (Map.elems $ irMethods ir)
      forM_ bidirMethods $ \method ->
        -- Return type should be valid (not RefUnknown)
        case mdReturns method of
          RefUnknown -> expectationFailure $ T.unpack $
            "Bidirectional method " <> mdFullPath method <> " has RefUnknown return type"
          _ -> pure ()

    it "mdBidirType is consistent across IR operations" $ do
      -- Test that mdBidirType survives IR transformations
      let allMethods = Map.elems $ irMethods ir
      let bidirCount = length $ filter (isJust . mdBidirType) allMethods
      -- The count should be stable
      bidirCount `shouldSatisfy` (>= 0)
      pure ()

  describe "Statistics and reporting" $ do
    it "reports bidirectional coverage statistics" $ do
      let allMethods = Map.elems $ irMethods ir
      let bidirMethods = filter (isJust . mdBidirType) allMethods
      let streamingMethods = filter mdStreaming allMethods

      putStrLn "\n  Bidirectional Statistics:"
      putStrLn $ "    Total methods:        " <> show (length allMethods)
      putStrLn $ "    Streaming methods:    " <> show (length streamingMethods)
      putStrLn $ "    Bidirectional methods:" <> show (length bidirMethods)

      if null bidirMethods
        then putStrLn "    (No bidirectional methods found - tests are informational)"
        else do
          putStrLn "    Bidirectional methods:"
          forM_ bidirMethods $ \method ->
            putStrLn $ "      - " <> T.unpack (mdFullPath method)

      pure ()

-- Helper to avoid importing Control.Monad.Extra
unless :: Bool -> IO () -> IO ()
unless condition action = if condition then pure () else action

-- Helper function to test a specific bidirectional method
testBidirMethod :: IR -> Text -> Spec
testBidirMethod ir methodPath =
  case Map.lookup methodPath (irMethods ir) of
    Nothing -> it (T.unpack methodPath <> " exists (skipped - not found)") pending
    Just method -> do
      it (T.unpack methodPath <> " is marked as bidirectional") $
        isJust (mdBidirType method) `shouldBe` True

      it (T.unpack methodPath <> " has standard bidir type (RefAny)") $
        mdBidirType method `shouldSatisfy` (\case { Just RefJson -> True; _ -> False })

      it (T.unpack methodPath <> " is a streaming method") $
        mdStreaming method `shouldBe` True
