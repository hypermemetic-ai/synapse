{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for IR-6, IR-14 deprecation rendering, and IR-15
--   invocation-time stderr deprecation warnings.
--
--   IR-6:  per-method and per-activation decoration in 'renderSchema'.
--   IR-14: per-parameter decoration in both 'renderMethodFull' (reads
--          'methodParamSchemas' directly) and 'renderMethodHelpWith'
--          (reads 'ParamDef.pdDeprecation' after the IR builder has
--          lifted the info across).
--   IR-15: stderr warnings emitted at invocation time from
--          'Synapse.Deprecation', reusing IR-6's 'deprecationMarker' and
--          'formatDeprecationLine' for line wording. Per-session dedupe,
--          disjoint method/activation keyspaces, and the
--          @--no-deprecation-warnings@ suppression toggle.
module Main where

import Control.Exception (finally)
import Data.Aeson (object, (.=), Value)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, hFlush, openTempFile, stderr)
import Test.Hspec

import Plexus.Schema.Recursive
  ( DeprecationInfo(..)
  , MethodRole(..)
  , MethodSchema(..)
  , ParamSchema(..)
  , PluginSchema(..)
  )
import Synapse.Algebra.Render
  ( deprecationMarker
  , formatDeprecationLine
  , renderMethodFull
  , renderSchema
  )
import Synapse.CLI.Help (renderMethodHelp)
import Synapse.Deprecation
  ( emitActivationWarning
  , emitMethodWarning
  , formatActivationWarningLine
  , formatMethodWarningLine
  , resetDeprecationStateForTesting
  )
import Synapse.IR.Builder (extractMethodDef)
import Synapse.IR.Types (IR(..), emptyIR)

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

baseMethod :: MethodSchema
baseMethod = MethodSchema
  { methodName           = "move_doc"
  , methodDescription    = "Relocate a document."
  , methodHash           = "hash-move-doc"
  , methodParams         = Nothing
  , methodReturns        = Nothing
  , methodStreaming      = False
  , methodBidirectional  = False
  , methodRequestType    = Nothing
  , methodResponseType   = Nothing
  , methodDeprecation    = Nothing
  , methodParamSchemas   = Nothing
  , methodRole           = MethodRoleRpc
  , methodLongDescription = Nothing, methodGrant = Nothing, methodTarget = Nothing, methodHeldOn = Nothing, methodPublic = False, methodGroup = Nothing, methodExamples = []
  }

deprecatedMethod :: MethodSchema
deprecatedMethod = baseMethod
  { methodDeprecation = Just DeprecationInfo
      { depSince     = "0.5"
      , depRemovedIn = "0.7"
      , depMessage   = "use move_doc"
      }
  }

plainMethod :: MethodSchema
plainMethod = baseMethod { methodName = "list_docs" }

basePlugin :: PluginSchema
basePlugin = PluginSchema
  { psNamespace       = "docs"
  , psVersion         = "1.0"
  , psDescription     = "Document activation"
  , psLongDescription = Nothing
  , psHash            = "hash-docs"
  , psMethods         = [deprecatedMethod, plainMethod]
  , psChildren        = Nothing
  , psDeprecation     = Nothing
  , psRoles = Nothing, psView = Nothing
  }

deprecatedPlugin :: PluginSchema
deprecatedPlugin = basePlugin
  { psDeprecation = Just DeprecationInfo
      { depSince     = "0.4"
      , depRemovedIn = "0.8"
      , depMessage   = "use docs_v2"
      }
  }

-- ---------------------------------------------------------------------------
-- IR-14 Fixtures: param-level deprecation
-- ---------------------------------------------------------------------------

-- | JSON Schema for a method with two parameters: 'path' (deprecated, the
--   one IR-14 decorates) and 'target' (not deprecated, the control).
--
--   This matches the minimum shape 'renderParamsFull' / 'extractParams'
--   actually inspect: an object with @properties@ and @required@.
paramsSchemaJSON :: Value
paramsSchemaJSON = object
  [ "type" .= ("object" :: T.Text)
  , "properties" .= object
      [ "path"   .= object
          [ "type"        .= ("string" :: T.Text)
          , "description" .= ("Old document path (deprecated)." :: T.Text)
          ]
      , "target" .= object
          [ "type"        .= ("string" :: T.Text)
          , "description" .= ("Destination identifier." :: T.Text)
          ]
      ]
  , "required" .= (["path", "target"] :: [T.Text])
  ]

-- | ParamSchema list carrying the per-param deprecation info for 'path'
--   and leaving 'target' clean. This is the IR-5 wire-level payload.
paramSchemas :: [ParamSchema]
paramSchemas =
  [ ParamSchema
      { paramName        = "path"
      , paramDescription = Just "Old document path (deprecated)."
      , paramRequired    = True
      , paramDeprecation = Just DeprecationInfo
          { depSince     = "0.5"
          , depRemovedIn = "0.7"
          , depMessage   = "use target"
          }
      }
  , ParamSchema
      { paramName        = "target"
      , paramDescription = Just "Destination identifier."
      , paramRequired    = True
      , paramDeprecation = Nothing
      }
  ]

-- | Method with a deprecated parameter. Used to drive both
--   'renderMethodFull' (Render.hs) and 'renderMethodHelp' (Help.hs).
methodWithDeprecatedParam :: MethodSchema
methodWithDeprecatedParam = baseMethod
  { methodName         = "relocate"
  , methodDescription  = "Relocate a document."
  , methodHash         = "hash-relocate"
  , methodParams       = Just paramsSchemaJSON
  , methodParamSchemas = Just paramSchemas
  }

-- | Build a minimal IR containing just this method, so we can exercise
--   'renderMethodHelp' which consumes the IR 'MethodDef' (where
--   'pdDeprecation' lives, lifted there by 'extractMethodDef').
helpFixture :: (IR, T.Text)
helpFixture =
  let (_types, mdef) = extractMethodDef "docs" "docs" methodWithDeprecatedParam
      fullPath       = "docs.relocate"
      ir = emptyIR { irMethods = Map.singleton fullPath mdef }
  in (ir, renderMethodHelp ir mdef)

-- ---------------------------------------------------------------------------
-- IR-15 Fixtures: invocation-time deprecation warnings
-- ---------------------------------------------------------------------------

-- | Method-level deprecation info used by IR-15 invocation tests.
--   Shaped identically to the IR-6 fixture so both the marker and the
--   'formatDeprecationLine' byte sequence line up verbatim, which is
--   the whole point of IR-15 reusing those helpers.
methodDep :: DeprecationInfo
methodDep = DeprecationInfo
  { depSince     = "0.5"
  , depRemovedIn = "0.7"
  , depMessage   = "use move_doc"
  }

-- | Activation-level deprecation info. Distinct @since@ from 'methodDep'
--   so tests that emit both can tell them apart in captured stderr.
activationDep :: DeprecationInfo
activationDep = DeprecationInfo
  { depSince     = "0.4"
  , depRemovedIn = "0.8"
  , depMessage   = "use docs_v2"
  }

-- ---------------------------------------------------------------------------
-- IR-15 Helpers: stderr capture
-- ---------------------------------------------------------------------------

-- | Run an action with 'stderr' swapped for a fresh temporary file;
--   return the captured text alongside the action's result.
--
--   The swap uses 'hDuplicate' / 'hDuplicateTo' so 'System.IO.stderr'
--   itself (the global handle 'Data.Text.IO.hPutStrLn' writes to inside
--   'Synapse.Deprecation') is redirected — not just a local 'Handle'.
--   Using 'finally' for cleanup keeps the tempfile path removed even if
--   a test assertion throws.
captureStderr :: IO a -> IO (a, T.Text)
captureStderr action = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "synapse-deprecation-stderr.txt"
  (do
      originalStderr <- hDuplicate stderr
      hDuplicateTo h stderr
      hClose h
      result <- action
      hFlush stderr
      hDuplicateTo originalStderr stderr
      hClose originalStderr
      captured <- TIO.readFile path
      pure (result, captured))
    `finally` removeFile path

-- | Count occurrences of a substring. Distinguishes "fires once" from
--   "fires twice" in dedupe tests.
countOccurrences :: T.Text -> T.Text -> Int
countOccurrences needle haystack
  | T.null needle = 0
  | otherwise = length (T.breakOnAll needle haystack)

main :: IO ()
main = hspec $ do
  describe "renderSchema (IR-6 method-level deprecation)" $ do
    it "prepends the warning marker to deprecated method names" $ do
      let rendered = renderSchema basePlugin
      rendered `shouldSatisfy` T.isInfixOf deprecationMarker

    it "emits the full DEPRECATED detail line for deprecated methods" $ do
      let rendered = renderSchema basePlugin
      rendered `shouldSatisfy` T.isInfixOf "DEPRECATED since 0.5"
      rendered `shouldSatisfy` T.isInfixOf "removed in 0.7"
      rendered `shouldSatisfy` T.isInfixOf "use move_doc"

    it "leaves non-deprecated methods unchanged (no marker on their name line)" $ do
      let rendered = renderSchema basePlugin
          ls       = T.lines rendered
          -- Lines containing list_docs must NOT contain the marker.
          listDocLines = filter (T.isInfixOf "list_docs") ls
      all (not . T.isInfixOf deprecationMarker) listDocLines
        `shouldBe` True

    it "emits no deprecation markers for plugins whose methods all lack deprecation" $ do
      let cleanPlugin = basePlugin { psMethods = [plainMethod] }
          rendered    = renderSchema cleanPlugin
      rendered `shouldSatisfy` (not . T.isInfixOf deprecationMarker)
      rendered `shouldSatisfy` (not . T.isInfixOf "DEPRECATED")

  describe "renderSchema (IR-6 activation-level deprecation)" $ do
    it "renders a warning marker on the activation heading" $ do
      let rendered = renderSchema deprecatedPlugin
          ls       = T.lines rendered
          -- Some early line must contain both the namespace and the marker.
          decoratedHeading =
            any (\l -> T.isInfixOf "docs" l && T.isInfixOf deprecationMarker l)
                (take 3 ls)
      decoratedHeading `shouldBe` True

    it "includes the activation-level deprecation detail string" $ do
      let rendered = renderSchema deprecatedPlugin
      rendered `shouldSatisfy` T.isInfixOf "DEPRECATED since 0.4"
      rendered `shouldSatisfy` T.isInfixOf "removed in 0.8"
      rendered `shouldSatisfy` T.isInfixOf "use docs_v2"

    it "keeps activation rendering unchanged when psDeprecation is Nothing" $ do
      let rendered = renderSchema (basePlugin { psMethods = [plainMethod] })
      rendered `shouldSatisfy` (not . T.isInfixOf "use docs_v2")

  describe "renderMethodFull (IR-14 per-parameter deprecation in Render.hs)" $ do
    it "prepends the warning marker on the deprecated parameter's flag line" $ do
      let rendered = renderMethodFull methodWithDeprecatedParam
          ls       = T.lines rendered
          -- Line that mentions --path must carry the marker.
          pathLines = filter (T.isInfixOf "--path") ls
      pathLines `shouldSatisfy` (not . null)
      all (T.isInfixOf deprecationMarker) pathLines `shouldBe` True

    it "emits the DEPRECATED detail line for the deprecated parameter" $ do
      let rendered = renderMethodFull methodWithDeprecatedParam
      rendered `shouldSatisfy` T.isInfixOf "DEPRECATED since 0.5"
      rendered `shouldSatisfy` T.isInfixOf "removed in 0.7"
      rendered `shouldSatisfy` T.isInfixOf "use target"

    it "leaves non-deprecated sibling parameter undecorated" $ do
      let rendered = renderMethodFull methodWithDeprecatedParam
          ls       = T.lines rendered
          -- Lines mentioning --target must NOT carry the marker.  We also
          -- filter out lines that happen to contain 'use target' (the
          -- deprecation detail line for --path), since that substring
          -- would false-match.
          targetLines =
            [ l | l <- ls
                , T.isInfixOf "--target" l
                , not (T.isInfixOf "use target" l)
            ]
      targetLines `shouldSatisfy` (not . null)
      all (not . T.isInfixOf deprecationMarker) targetLines `shouldBe` True

  describe "renderMethodHelp (IR-14 per-parameter deprecation in Help.hs)" $ do
    it "prepends the warning marker on the deprecated parameter's flag line" $ do
      let (_, rendered) = helpFixture
          -- SYNAPSE-13: the options, not the usage line or an example's
          -- command line, which name the flag as it is typed
          ls            = takeWhile (not . T.isPrefixOf "Example") (dropWhile (/= "Options:") (T.lines rendered))
          pathLines     = filter (T.isInfixOf "--path") ls
      pathLines `shouldSatisfy` (not . null)
      all (T.isInfixOf deprecationMarker) pathLines `shouldBe` True

    it "emits the DEPRECATED detail line for the deprecated parameter" $ do
      let (_, rendered) = helpFixture
      rendered `shouldSatisfy` T.isInfixOf "DEPRECATED since 0.5"
      rendered `shouldSatisfy` T.isInfixOf "removed in 0.7"
      rendered `shouldSatisfy` T.isInfixOf "use target"

    it "leaves non-deprecated sibling parameter undecorated" $ do
      let (_, rendered) = helpFixture
          ls            = T.lines rendered
          targetLines =
            [ l | l <- ls
                , T.isInfixOf "--target" l
                , not (T.isInfixOf "use target" l)
            ]
      targetLines `shouldSatisfy` (not . null)
      all (not . T.isInfixOf deprecationMarker) targetLines `shouldBe` True

  -- =========================================================================
  -- IR-15: invocation-time stderr deprecation warnings
  -- =========================================================================

  describe "formatMethodWarningLine / formatActivationWarningLine (IR-15 reuses IR-6 helpers)" $ do
    it "method line carries the IR-6 deprecationMarker and the IR-6 formatDeprecationLine verbatim" $ do
      let line = formatMethodWarningLine "docs.move_doc" methodDep
      line `shouldSatisfy` T.isInfixOf deprecationMarker
      line `shouldSatisfy` T.isInfixOf "docs.move_doc"
      line `shouldSatisfy` T.isInfixOf (formatDeprecationLine methodDep)

    it "method line matches the ticket-spec shape for since / removed_in / message" $ do
      let line = formatMethodWarningLine "docs.move_doc" methodDep
      line `shouldSatisfy` T.isInfixOf "DEPRECATED since 0.5"
      line `shouldSatisfy` T.isInfixOf "removed in 0.7"
      line `shouldSatisfy` T.isInfixOf "use move_doc"

    it "activation line names the namespace and embeds the IR-6 DEPRECATED clause" $ do
      let line = formatActivationWarningLine "docs" activationDep
      line `shouldSatisfy` T.isInfixOf deprecationMarker
      line `shouldSatisfy` T.isInfixOf "'docs'"
      line `shouldSatisfy` T.isInfixOf (formatDeprecationLine activationDep)

    it "activation line mentions 'activation' so consumers can grep it apart from method lines" $ do
      let line = formatActivationWarningLine "docs" activationDep
      line `shouldSatisfy` T.isInfixOf "activation"

  -- Every IR-15 emission test resets the global dedupe IORef in a
  -- 'before_' hook so IORef leakage across tests is impossible.
  before_ resetDeprecationStateForTesting $ do
    describe "emitMethodWarning (IR-15 method-level invocation warning)" $ do
      it "(AC 2 / a) fires on the first invocation of a deprecated method" $ do
        ((), captured) <- captureStderr $
          emitMethodWarning False "docs.move_doc" (Just methodDep)
        captured `shouldSatisfy` T.isInfixOf "docs.move_doc"
        captured `shouldSatisfy` T.isInfixOf "DEPRECATED since 0.5"

      it "(AC 3 / b) suppresses on the second invocation of the same method in the same session" $ do
        ((), captured) <- captureStderr $ do
          emitMethodWarning False "docs.move_doc" (Just methodDep)
          emitMethodWarning False "docs.move_doc" (Just methodDep)
          emitMethodWarning False "docs.move_doc" (Just methodDep)
        countOccurrences "DEPRECATED since 0.5" captured `shouldBe` 1

      it "fires once for each distinct deprecated method (dedupe is per full-path key)" $ do
        ((), captured) <- captureStderr $ do
          emitMethodWarning False "docs.move_doc" (Just methodDep)
          emitMethodWarning False "docs.retire_doc"
            (Just methodDep { depMessage = "use retire_doc_v2" })
        captured `shouldSatisfy` T.isInfixOf "docs.move_doc"
        captured `shouldSatisfy` T.isInfixOf "docs.retire_doc"

      it "stays silent when the method is not deprecated" $ do
        ((), captured) <- captureStderr $
          emitMethodWarning False "docs.list" Nothing
        captured `shouldBe` ""

    describe "emitActivationWarning (IR-15 activation-level invocation warning)" $ do
      it "(AC 4 / c) fires once per session on a deprecated activation, across multiple invocations" $ do
        ((), captured) <- captureStderr $ do
          -- Simulate three calls to *different methods* on the same
          -- deprecated activation. The per-session dedupe must collapse
          -- these to a single line.
          emitActivationWarning False "docs" (Just activationDep)
          emitActivationWarning False "docs" (Just activationDep)
          emitActivationWarning False "docs" (Just activationDep)
        countOccurrences "activation" captured `shouldBe` 1
        countOccurrences "'docs'" captured `shouldBe` 1

      it "keeps method and activation keyspaces disjoint (same string key does not collide)" $ do
        -- An activation called @docs@ and a method whose full path is
        -- literally @docs@ must dedupe independently: firing one must
        -- not silence the other.
        ((), captured) <- captureStderr $ do
          emitActivationWarning False "docs" (Just activationDep)
          emitMethodWarning     False "docs" (Just methodDep)
        countOccurrences "activation" captured `shouldBe` 1
        countOccurrences "DEPRECATED since 0.5" captured `shouldBe` 1  -- method
        countOccurrences "DEPRECATED since 0.4" captured `shouldBe` 1  -- activation

      it "stays silent when the activation is not deprecated" $ do
        ((), captured) <- captureStderr $
          emitActivationWarning False "docs" Nothing
        captured `shouldBe` ""

    describe "--no-deprecation-warnings suppression (IR-15 (AC 5 / d))" $ do
      it "suppresses method-level warning when flag is on" $ do
        ((), captured) <- captureStderr $
          emitMethodWarning True "docs.move_doc" (Just methodDep)
        captured `shouldBe` ""

      it "suppresses activation-level warning when flag is on" $ do
        ((), captured) <- captureStderr $
          emitActivationWarning True "docs" (Just activationDep)
        captured `shouldBe` ""

      it "a suppressed call does NOT claim the dedupe key (next unsuppressed call still fires)" $ do
        -- Behavioural contract: toggling the flag on must not poison
        -- the dedupe set for future unsuppressed calls. If it did,
        -- scripts that toggle the flag between calls would silently
        -- hide real warnings.
        ((), captured) <- captureStderr $ do
          emitMethodWarning True  "docs.move_doc" (Just methodDep)  -- suppressed
          emitMethodWarning False "docs.move_doc" (Just methodDep)  -- should fire
        countOccurrences "DEPRECATED since 0.5" captured `shouldBe` 1

  describe "(AC 6 / e) exit codes are not the concern of the emission layer" $ do
    -- The emission functions return (), so they cannot alter exit codes
    -- by construction. This is asserted here as a contract rather than
    -- a runtime check. The end-to-end exit-code guarantee belongs to
    -- the CLI integration suite (driven against a live backend).
    it "emitMethodWarning returns unit (:: IO ())" $
      let _ = emitMethodWarning :: Bool -> T.Text -> Maybe DeprecationInfo -> IO ()
      in True `shouldBe` True
    it "emitActivationWarning returns unit (:: IO ())" $
      let _ = emitActivationWarning :: Bool -> T.Text -> Maybe DeprecationInfo -> IO ()
      in True `shouldBe` True
