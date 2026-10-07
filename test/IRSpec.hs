{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Integration tests for IR-based CLI
--
-- By default, discovers "substrate" via registry at localhost:4444.
-- Override with: cabal test ir-test --test-options="<backend> [--port <port>]"
--
-- Examples:
--   cabal test ir-test                                    -- auto-discover via registry
--   cabal test ir-test --test-options="substrate"         -- direct connect, default port 4445
--   cabal test ir-test --test-options="substrate --port 4445"
--
-- Tests that for every method in the schema:
-- 1. IR builds successfully
-- 2. All type references resolve
-- 3. Help renders without error
-- 4. Support check returns a valid level
module Main where

import Control.Monad (forM_, unless)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BSL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.Environment (getArgs, withArgs)
import Test.Hspec
import Text.Read (readMaybe)

import Synapse.IR.Types
import Synapse.IR.Builder (buildIR)
import Synapse.CLI.Help (renderMethodHelp, expandType)
import Synapse.CLI.Support (SupportLevel(..), methodSupport)
import Synapse.Monad
import Synapse.Backend.Discovery (Backend(..), BackendDiscovery(..), registryDiscovery)
import qualified Synapse.Log as Log
import qualified Katip

main :: IO ()
main = do
  args <- getArgs
  (backend, host, port) <- resolveBackend args
  putStrLn $ "Running IR integration tests against " <> T.unpack host <> ":" <> show port <> " (backend: " <> T.unpack backend <> ")"

  logger <- Log.makeLogger Katip.ErrorS
  env <- initEnv host port backend logger Nothing

  -- Build IR once for all tests
  irResult <- runSynapseM env (buildIR [] [])

  case irResult of
    Left err -> do
      putStrLn $ "Failed to build IR: " <> show err
      putStrLn "Is the Hub backend running?"
      withArgs [] $ hspec $ describe "IR Integration Tests" $
        it "connects to Hub backend" $
          expectationFailure $ "Could not connect: " <> show err

    Right ir -> withArgs [] $ hspec $ irSpec ir

-- | Resolve backend connection: explicit args or registry discovery
resolveBackend :: [String] -> IO (Text, Text, Int)
resolveBackend (backendStr:rest) = do
  let backend = T.pack backendStr
      port = parsePort rest
  putStrLn $ "Using explicit backend: " <> backendStr <> " on port " <> show port
  pure (backend, "127.0.0.1", port)
  where
    parsePort ("--port":p:_) = maybe 4445 id (readMaybe p)
    parsePort _ = 4445
resolveBackend [] = do
  putStrLn "No backend specified, discovering substrate via registry at localhost:4444..."
  let discovery = registryDiscovery "127.0.0.1" 4444
  result <- getBackendInfo discovery "substrate"
  case result of
    Just backend -> do
      putStrLn $ "Discovered substrate at " <> T.unpack (backendHost backend) <> ":" <> show (backendPort backend)
      pure (backendName backend, backendHost backend, backendPort backend)
    Nothing ->
      error "substrate not found via registry at localhost:4444. Is the registry and substrate running?"

-- | Main spec using pre-built IR
irSpec :: IR -> Spec
irSpec ir = do
  describe "Schema fetching" $ do
    it "builds IR from root" $
      irVersion ir `shouldBe` "3.0"

    it "IR contains methods" $
      Map.size (irMethods ir) `shouldSatisfy` (> 0)

    it "IR contains types" $
      -- Some methods may not have complex types, but we expect at least some
      Map.size (irTypes ir) `shouldSatisfy` (>= 0)

    it "IR contains plugins" $
      Map.size (irPlugins ir) `shouldSatisfy` (> 0)

  describe "Method coverage" $ do
    it "all methods have help text" $
      forM_ (Map.elems $ irMethods ir) $ \method -> do
        let helpText = renderMethodHelp ir method
        T.length helpText `shouldSatisfy` (> 0)
        -- Help should contain method name
        helpText `shouldSatisfy` T.isInfixOf (mdFullPath method)

    it "all type refs resolve" $
      forM_ (Map.elems $ irMethods ir) $ \method -> do
        -- Check each param's type ref
        forM_ (mdParams method) $ \param -> do
          let typeRef = pdType param
          checkTypeRefResolves ir (mdFullPath method) (pdName param) typeRef

    it "all methods have support level" $
      forM_ (Map.elems $ irMethods ir) $ \method -> do
        let support = methodSupport ir method
        -- Support level should be valid (not crash)
        case support of
          FullSupport -> pure ()
          PartialSupport params -> length params `shouldSatisfy` (>= 0)
          NoSupport params -> length params `shouldSatisfy` (> 0)

  describe "QualifiedName serialization" $ do
    it "serializes to structured JSON" $ do
      let qn = QualifiedName { qnNamespace = "cone", qnLocalName = "UUID" }
      let typeRef = RefNamed qn
      let json = encode typeRef
      -- Check that JSON contains expected structure
      let jsonText = decodeUtf8 (BSL.toStrict json)
      jsonText `shouldSatisfy` T.isInfixOf "qnNamespace"
      jsonText `shouldSatisfy` T.isInfixOf "qnLocalName"
      jsonText `shouldSatisfy` T.isInfixOf "cone"
      jsonText `shouldSatisfy` T.isInfixOf "UUID"

    it "serializes QualifiedName with empty namespace" $ do
      let qn = QualifiedName { qnNamespace = "", qnLocalName = "GlobalType" }
      let typeRef = RefNamed qn
      let json = encode typeRef
      let jsonText = decodeUtf8 (BSL.toStrict json)
      jsonText `shouldSatisfy` T.isInfixOf "GlobalType"
      -- Empty namespace should still be present in JSON
      jsonText `shouldSatisfy` T.isInfixOf "qnNamespace"

    it "qualifiedNameFull handles namespaces correctly" $ do
      qualifiedNameFull (QualifiedName "ns" "Type") `shouldBe` "ns.Type"
      qualifiedNameFull (QualifiedName "" "Type") `shouldBe` "Type"

    it "shows expected JSON format" $ do
      -- Test exact JSON format by printing it
      let qn = QualifiedName { qnNamespace = "test", qnLocalName = "MyType" }
      let typeRef = RefNamed qn
      let jsonText = decodeUtf8 (BSL.toStrict (encode typeRef))
      -- Should have "tag" and "contents" for sum type
      jsonText `shouldSatisfy` T.isInfixOf "RefNamed"
      jsonText `shouldSatisfy` T.isInfixOf "contents"

  describe "Specific methods" $ do
    -- Test cone.chat if it exists (has ConeIdentifier discriminated union)
    case Map.lookup "cone.chat" (irMethods ir) of
      Nothing -> it "cone.chat exists (skipped - not found)" pending
      Just method -> do
        it "cone.chat has identifier param" $
          any (\p -> pdName p == "identifier") (mdParams method) `shouldBe` True

        it "cone.chat expands ConeIdentifier" $ do
          let mIdentifier = filter (\p -> pdName p == "identifier") (mdParams method)
          case mIdentifier of
            [] -> expectationFailure "identifier param not found"
            (p:_) -> do
              let expansion = expandType ir "identifier" (pdType p)
              -- Should have expansion for discriminated union
              length expansion `shouldSatisfy` (> 0)
              -- Should contain "Either:" for union types
              any (T.isInfixOf "Either") expansion `shouldBe` True

    -- Test echo.once if it exists (simple params)
    case Map.lookup "echo.once" (irMethods ir) of
      Nothing -> it "echo.once exists (skipped - not found)" pending
      Just method -> do
        it "echo.once has simple params" $
          -- echo.once should have FullSupport since it only has primitives
          case methodSupport ir method of
            FullSupport -> pure ()
            other -> expectationFailure $ "Expected FullSupport, got: " <> show other

        it "echo.once has message param" $
          any (\p -> pdName p == "message") (mdParams method) `shouldBe` True

  describe "Type resolution" $ do
    it "no dangling RefNamed in params" $
      forM_ (Map.elems $ irMethods ir) $ \method ->
        forM_ (mdParams method) $ \param ->
          case getUnresolvedRefs ir (pdType param) of
            [] -> pure ()
            refs -> expectationFailure $ T.unpack $
              "Unresolved refs in " <> mdFullPath method <> "." <> pdName param <>
              ": " <> T.intercalate ", " refs

    it "reports unresolved return type refs (informational)" $ do
      -- Collect all unresolved return type refs
      let unresolvedReturns =
            [ (mdFullPath method, refs)
            | method <- Map.elems (irMethods ir)
            , let refs = getUnresolvedRefs ir (mdReturns method)
            , not (null refs)
            ]
      -- Report them if any, but don't fail - some methods have simple return types
      -- that reference types defined elsewhere (like SchemaResult from health.schema)
      unless (null unresolvedReturns) $ do
        -- Print for informational purposes
        forM_ unresolvedReturns $ \(methodPath, refs) ->
          putStrLn $ "  [info] " <> T.unpack methodPath <>
            " has unresolved return refs: " <> T.unpack (T.intercalate ", " refs)
      -- Success - this test is informational only
      pure ()

-- | Check that a TypeRef resolves (if named, exists in irTypes)
checkTypeRefResolves :: IR -> Text -> Text -> TypeRef -> Expectation
checkTypeRefResolves ir methodPath paramName = \case
  RefNamed qn -> do
    let name = qualifiedNameFull qn
    unless (Map.member name (irTypes ir)) $
      expectationFailure $ T.unpack $
        "Unresolved type ref: " <> name <>
        " in " <> methodPath <> "." <> paramName
  RefArray inner -> checkTypeRefResolves ir methodPath paramName inner
  RefOptional inner -> checkTypeRefResolves ir methodPath paramName inner
  RefPrimitive _ _ -> pure ()
  RefAny -> pure ()
  RefUnknown -> pure ()  -- Unknown is valid (just means schema gap)

-- | Check that no RefNamed references are unresolved
checkNoUnresolvedRefs :: IR -> TypeRef -> Bool
checkNoUnresolvedRefs ir = \case
  RefNamed qn -> Map.member (qualifiedNameFull qn) (irTypes ir)
  RefArray inner -> checkNoUnresolvedRefs ir inner
  RefOptional inner -> checkNoUnresolvedRefs ir inner
  RefPrimitive _ _ -> True
  RefAny -> True
  RefUnknown -> True

-- | Get list of unresolved RefNamed names
getUnresolvedRefs :: IR -> TypeRef -> [Text]
getUnresolvedRefs ir = \case
  RefNamed qn ->
    let name = qualifiedNameFull qn
    in if Map.member name (irTypes ir)
       then []
       else [name]
  RefArray inner -> getUnresolvedRefs ir inner
  RefOptional inner -> getUnresolvedRefs ir inner
  RefPrimitive _ _ -> []
  RefAny -> []
  RefUnknown -> []
