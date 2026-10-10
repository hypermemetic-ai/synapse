{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | REQ-8 acceptance: synapse renderer shows per-param x-plexus-source
-- annotations inline when rendering a method's help.
--
-- SYNAPSE-13: a listing no longer shows parameters (one line per method),
-- so the annotations are pinned on the method's page, built from the IR.
module Main where

import Data.Aeson (Value)
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.IORef as IORef
import qualified Data.Text as T
import Data.Text (Text)
import System.Exit (exitFailure, exitSuccess)

import qualified Plexus.Schema.Recursive
import Plexus.Schema.Recursive
  ( PluginSchema(..), MethodSchema(..) )
import qualified Data.Map.Strict as Map
import Synapse.CLI.Help (renderMethodHelp)
import Synapse.IR.Builder (extractMethodDef)
import Synapse.IR.Types (IR(..), MethodDef(..), emptyIR)

-- ─── Fixture ──────────────────────────────────────────────────

-- A plugin with one method that has three params:
--   - user: x-plexus-source.from = auth (resolver)
--   - origin: x-plexus-source.from = derived
--   - search: no annotation (RPC param)
fixture :: PluginSchema
fixture = PluginSchema
  { psNamespace       = "svc"
  , psVersion         = "1.0.0"
  , psDescription     = "Test service"
  , psLongDescription = Nothing
  , psHash            = "h1"
  , psMethods         = [method1]
  , psChildren        = Nothing
  , psDeprecation     = Nothing
  , psRoles = Nothing, psView = Nothing
  }
  where
    method1 = MethodSchema
      { methodName          = "list"
      , methodDescription   = "List things"
      , methodHash          = "h2"
      , methodParams        = Just paramsSchema
      , methodReturns       = Nothing
      , methodStreaming     = False
      , methodBidirectional = False
      , methodRequestType   = Nothing
      , methodResponseType  = Nothing
      , methodDeprecation   = Nothing
      , methodParamSchemas  = Nothing
      , methodRole          = defaultRole
      , methodLongDescription = Nothing, methodGrant = Nothing, methodTarget = Nothing, methodHeldOn = Nothing, methodPublic = False, methodGroup = Nothing, methodExamples = []
      }

-- | Stub role — renderSchema doesn't branch on role for this test.
defaultRole :: Plexus.Schema.Recursive.MethodRole
defaultRole = Plexus.Schema.Recursive.MethodRoleRpc

paramsSchema :: Value
paramsSchema = [aesonQQ|{
  "type": "object",
  "properties": {
    "user": {
      "type": "object",
      "x-plexus-source": {
        "from": "auth",
        "resolver": "self.db.validate_user"
      }
    },
    "origin": {
      "type": "object",
      "x-plexus-source": { "from": "derived" }
    },
    "search": {
      "type": "string"
    }
  },
  "required": ["search"]
}|]

-- ─── Runner ───────────────────────────────────────────────────

data TestResult = Pass String | Fail String String

main :: IO ()
main = do
  failures <- IORef.newIORef (0 :: Int)
  let run (Pass name)        = putStrLn $ "  [PASS] " <> name
      run (Fail name reason) = do
        putStrLn $ "  [FAIL] " <> name <> ": " <> reason
        IORef.modifyIORef' failures (+ 1)

  let (types, md) = extractMethodDef "svc" "svc" (head (psMethods fixture))
      ir = emptyIR { irMethods = Map.singleton (mdFullPath md) md, irTypes = types }
      out = renderMethodHelp ir md

  putStrLn "=== REQ-8: per-param source annotations in renderer ==="
  putStrLn "(rendered output)"
  putStrLn "---"
  putStrLn $ T.unpack out
  putStrLn "---"

  run $ assertContains out "list"    "method name appears"
  run $ assertContains out "user"    "auth-sourced param name appears"
  run $ assertContains out "origin"  "derived-sourced param name appears"
  run $ assertContains out "--search" "RPC param emitted as --flag"
  run $ assertContains out "auth: self.db.validate_user"
           "auth resolver expression appears on the auth-sourced param"
  run $ assertContains out "server-derived"
           "derived source is labeled 'server-derived'"
  run $ assertNotContains out "--user"
           "auth-sourced param is NOT emitted as --flag (server-extracted)"
  run $ assertNotContains out "--origin"
           "derived-sourced param is NOT emitted as --flag"

  fc <- IORef.readIORef failures
  if fc == 0
    then putStrLn "\nREQ-8: ALL TESTS PASSED" >> exitSuccess
    else putStrLn ("\nREQ-8: " <> show fc <> " test(s) failed") >> exitFailure

assertContains :: Text -> Text -> String -> TestResult
assertContains hay needle name =
  if needle `T.isInfixOf` hay
    then Pass name
    else Fail name ("expected output to contain: " <> T.unpack needle)

assertNotContains :: Text -> Text -> String -> TestResult
assertNotContains hay needle name =
  if needle `T.isInfixOf` hay
    then Fail name ("unexpected substring present: " <> T.unpack needle)
    else Pass name
