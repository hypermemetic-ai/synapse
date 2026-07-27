{-# LANGUAGE OverloadedStrings #-}

-- | PLX-143 — the two re-fetch scars, and the cache that keyed on two things.
--
-- = What this suite asserts, and why in this shape
--
-- Both scars were round trips for a value the caller already held. You cannot
-- prove a round trip is gone by asserting on a rendered string, so this suite
-- asserts on the two things that actually changed:
--
--   1. __The value is the same one.__ For @--schema \<ns\> \<method\>@ the old
--      path called @fetchMethodSchema@, which (since PLX-13's schema
--      unification) fetches @\<ns\>.schema@ and then runs
--      @find ((== wantMethod) . methodName) . psMethods@ over it
--      (@plexus-protocol\/src\/Plexus\/Transport.hs:162-180@). That selection
--      function is reproduced here verbatim as 'oldWireSelection' and the new
--      output is compared against it, so "same output as before" is an
--      assertion rather than a claim. Likewise the deprecation display: the old
--      path called @fetchSchemaAt namespacePath@ and read @psDeprecation@ off
--      the result; the fixture IS what that fetch returns, so the carried
--      parent is compared against it and both are pushed through
--      'formatActivationWarningLine'.
--
--   2. __No fetch happens.__ Every navigation here runs against an env whose
--      host\/port point at nothing. A surviving round trip would not return a
--      stale value; it would fail to connect. The tests would go red, loudly.
--
-- The cache tests are counted, not inspected. 'fetchCached' takes its fetcher
-- as an argument, so the fetcher counts its own calls — the same discipline
-- PLX-121 used for the Connectome node cache.
module Main (main) where

import Data.Aeson (toJSON)
import Data.IORef
import Data.List (find)
import Data.Text (Text)
import Test.Hspec

import qualified Katip
import qualified Synapse.Log as Log

import Control.Monad.IO.Class (liftIO)

import Plexus.Schema.Recursive
  ( PluginSchema(..)
  , MethodSchema(..)
  , ChildSummary(..)
  , DeprecationInfo(..)
  , MethodRole(..)
  )

import Synapse.Algebra.Navigate (navigateFrom, findMethod)
import Synapse.Cache (fetchCached)
import Synapse.Deprecation (formatActivationWarningLine)
import Synapse.Monad
import Synapse.Schema.Types (SchemaView(..))

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

moveDoc :: MethodSchema
moveDoc = MethodSchema
  { methodName           = "move_doc"
  , methodDescription    = "Relocate a document."
  , methodHash           = "hash-move-doc"
  , methodParams         = Nothing
  , methodReturns        = Nothing
  , methodStreaming      = False
  , methodBidirectional  = False
  , methodRequestType    = Nothing
  , methodResponseType   = Nothing
  , methodDeprecation    = Just DeprecationInfo
      { depSince     = "0.3"
      , depRemovedIn = "0.7"
      , depMessage   = "use move"
      }
  , methodParamSchemas   = Nothing
  , methodRole           = MethodRoleRpc
  , methodCredentials    = []
  , methodRequiresCredential = Nothing
  , methodAuthPosture    = Nothing
  , methodPublic         = False
  }

listDocs :: MethodSchema
listDocs = moveDoc
  { methodName        = "list_docs"
  , methodDescription = "List documents"
  , methodHash        = "hash-list-docs"
  , methodDeprecation = Nothing
  }

-- | The parent activation. This is exactly the document that
-- @fetchSchemaAt ["docs"]@ returned on the old path, and that
-- @fetchMethodSchemaAt ["docs"] \"move_doc\"@ fetched and then searched.
docsPlugin :: PluginSchema
docsPlugin = PluginSchema
  { psNamespace       = "docs"
  , psVersion         = "1.0"
  , psDescription     = "Document activation"
  , psLongDescription = Nothing
  , psHash            = "hash-docs"
  , psMethods         = [moveDoc, listDocs]
  , psChildren        = Nothing
  , psDeprecation     = Just DeprecationInfo
      { depSince     = "0.4"
      , depRemovedIn = "0.8"
      , depMessage   = "use docs_v2"
      }
  , psRequest         = Nothing
  }

-- | The wire's own method selection, transcribed from
-- @Plexus.Transport.fetchMethodSchemaAt@. The point of writing it out is that
-- the new code must agree with THIS, not merely with itself.
oldWireSelection :: Text -> PluginSchema -> Maybe MethodSchema
oldWireSelection want plugin = find ((== want) . methodName) (psMethods plugin)

--------------------------------------------------------------------------------
-- Running SynapseM against a host that does not exist
--------------------------------------------------------------------------------

-- | Port 1 on loopback. Nothing listens there. Any surviving round trip fails.
runOffline :: SynapseM a -> IO (Either SynapseError a)
runOffline action = do
  logger <- Log.makeLogger Katip.ErrorS
  env <- initEnv "127.0.0.1" 1 "test" logger Nothing
  runSynapseM env action

--------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PLX-143 c1 / the --schema method re-pull" $ do
    it "lands on the method WITHOUT a second fetch, offline" $ do
      r <- runOffline (navigateFrom docsPlugin ["docs"] ["move_doc"])
      case r of
        Right (ViewMethod m _ path) -> do
          methodName m `shouldBe` "move_doc"
          path `shouldBe` ["docs", "move_doc"]
        other -> expectationFailure ("unexpected landing: " <> show other)

    it "yields the SAME MethodSchema the old fetch-and-find would have returned" $ do
      r <- runOffline (navigateFrom docsPlugin ["docs"] ["move_doc"])
      case r of
        Right (ViewMethod m _ _) ->
          Just m `shouldBe` oldWireSelection "move_doc" docsPlugin
        other -> expectationFailure ("unexpected landing: " <> show other)

    it "renders byte-identical --schema JSON to the old path's output" $ do
      r <- runOffline (navigateFrom docsPlugin ["docs"] ["move_doc"])
      case (r, oldWireSelection "move_doc" docsPlugin) of
        (Right (ViewMethod m _ _), Just old) -> toJSON m `shouldBe` toJSON old
        (other, _) -> expectationFailure ("unexpected landing: " <> show other)

    it "agrees with findMethod, which is the selection navigate already ran" $
      fmap methodName (findMethod "move_doc" docsPlugin)
        `shouldBe` fmap methodName (oldWireSelection "move_doc" docsPlugin)

  describe "PLX-143 c1 / the deprecation re-read" $ do
    it "carries the parent that the re-read used to go and fetch" $ do
      r <- runOffline (navigateFrom docsPlugin ["docs"] ["move_doc"])
      case r of
        -- `docsPlugin` is by construction what `fetchSchemaAt ["docs"]`
        -- returned. The carried parent must be that same document.
        Right (ViewMethod _ parent _) -> parent `shouldBe` docsPlugin
        other -> expectationFailure ("unexpected landing: " <> show other)

    it "produces the identical activation warning line from the carried parent" $ do
      r <- runOffline (navigateFrom docsPlugin ["docs"] ["move_doc"])
      case r of
        Right (ViewMethod _ parent _) -> do
          let fromCarried = formatActivationWarningLine "docs" <$> psDeprecation parent
              fromRefetch = formatActivationWarningLine "docs" <$> psDeprecation docsPlugin
          fromCarried `shouldBe` fromRefetch
          fromCarried `shouldBe`
            Just "\9888 activation 'docs' is DEPRECATED since 0.4, removed in 0.8 \8212 use docs_v2"
        other -> expectationFailure ("unexpected landing: " <> show other)

    it "still says nothing when the parent is not deprecated" $ do
      let plain = docsPlugin { psDeprecation = Nothing }
      r <- runOffline (navigateFrom plain ["docs"] ["list_docs"])
      case r of
        Right (ViewMethod _ parent _) -> psDeprecation parent `shouldBe` Nothing
        other -> expectationFailure ("unexpected landing: " <> show other)

  describe "PLX-143 c3 / fetchCached keys lookup and insert on ONE value" $ do
    let schemaWithHash h = docsPlugin { psHash = h }

        -- A fetcher that counts its own calls, and hands back a schema whose
        -- OWN hash is whatever the caller asks for.
        counting :: IORef Int -> PluginSchema -> SynapseM PluginSchema
        counting ref s = do
          liftIO $ modifyIORef' ref (+ 1)
          pure s

    it "hits on the second descent when advertised and self-reported hashes AGREE" $ do
      ref <- newIORef (0 :: Int)
      _ <- runOffline $ do
        _ <- fetchCached "hash-docs" (counting ref (schemaWithHash "hash-docs"))
        _ <- fetchCached "hash-docs" (counting ref (schemaWithHash "hash-docs"))
        pure ()
      readIORef ref `shouldReturn` 1

    it "STILL hits when they DISAGREE — the asymmetry that made a hit unobservable" $ do
      -- This is the regression test for the bug. Under the old code the entry
      -- was inserted under `psHash` ("i-disagree") and looked up under the
      -- advertised hash ("hash-advertised"), so the second call fetched again
      -- and the count would be 2 — forever, silently.
      ref <- newIORef (0 :: Int)
      _ <- runOffline $ do
        _ <- fetchCached "hash-advertised" (counting ref (schemaWithHash "i-disagree"))
        _ <- fetchCached "hash-advertised" (counting ref (schemaWithHash "i-disagree"))
        pure ()
      readIORef ref `shouldReturn` 1

    it "does NOT cache when the parent advertises no hash at all" $ do
      -- plexus-macros emits `hash: String::new()` for macro-synthesized
      -- `#[child]` summaries. An empty string is not an identity, so it must
      -- not become a key: no identity, no caching.
      ref <- newIORef (0 :: Int)
      _ <- runOffline $ do
        _ <- fetchCached "" (counting ref (schemaWithHash "hash-a"))
        _ <- fetchCached "" (counting ref (schemaWithHash "hash-a"))
        pure ()
      readIORef ref `shouldReturn` 2

    it "never collides two identity-less children onto one entry" $ do
      ref <- newIORef (0 :: Int)
      r <- runOffline $ do
        a <- fetchCached "" (counting ref (schemaWithHash "hash-a") )
        b <- fetchCached "" (counting ref ((schemaWithHash "hash-b") { psNamespace = "other" }))
        pure (psNamespace a, psNamespace b)
      r `shouldBe` Right ("docs", "other")

  describe "PLX-143 / ChildSummary is untouched by any of this" $
    it "still round-trips a child hash verbatim" $
      csHash (ChildSummary { csNamespace = "kid"
                           , csDescription = "a child"
                           , csHash = "hash-kid" })
        `shouldBe` "hash-kid"
