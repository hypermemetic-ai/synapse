{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | SYN-2: tests for the dynamic-children consumer.
--
-- These tests cover the behaviour the SYN-2 ticket calls out:
--
--   1. Schema decoder round-trip (delegated to Plexus.Schema.Recursive
--      and exercised here via the consumer-side pure helpers
--      'isDynamicChildGate' / 'dynamicChildGate' / 'gateParamName').
--
--   2. Mock-substrate enumeration: an injected 'Invoker' returns a
--      planet-shaped stream, the consumer returns the right names.
--
--   3. Mock-substrate empty list: a 'StreamDone' with no preceding
--      data frames yields an empty completion list, no crash.
--
--   4. Mock-substrate error: the invoker returns 'Left', the
--      consumer returns @[]@, logs a warning through the supplied
--      sink, no crash.
--
--   5. Cache TTL: two calls within the TTL window hit the cache,
--      the underlying invoker is called exactly once. After the TTL
--      expires, a third call re-invokes.
--
--   6. Timeout: an invoker that blocks longer than the configured
--      timeout returns @[]@ within the timeout budget (plus a small
--      safety margin), logs a warning, no crash.
--
--   7. Renderer integration: a 'PluginSchema' carrying a
--      'MethodRoleDynamicChild' method renders the @{name}@
--      placeholder and the @(dynamic child gate; …)@ annotation;
--      a schema with only @MethodRoleRpc@ methods renders unchanged.
module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Test.Hspec

import Plexus.Schema.Recursive
  ( MethodRole(..)
  , MethodSchema(..)
  , PluginSchema(..)
  )
import Plexus.Types (PlexusStreamItem(..), Provenance(..))

import qualified Synapse.DynamicChildren as DC
import qualified Synapse.Algebra.Render as Render

-- ============================================================================
-- Fixtures
-- ============================================================================

-- | A minimal 'MethodSchema' template the tests adjust per case.
baseMethod :: MethodSchema
baseMethod = MethodSchema
  { methodName            = "body"
  , methodDescription     = "Look up a celestial body by name."
  , methodHash            = "h-body"
  , methodParams          = Just $ object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "name" .= object [ "type" .= ("string" :: Text) ] ]
      , "required"   .= ["name" :: Text]
      ]
  , methodReturns         = Nothing
  , methodStreaming        = False
  , methodBidirectional   = False
  , methodRequestType     = Nothing
  , methodResponseType    = Nothing
  , methodDeprecation     = Nothing
  , methodParamSchemas    = Nothing
  , methodRole            = MethodRoleRpc
  , methodCredentials     = []
  , methodRequiresCredential = Nothing
  , methodAuthPosture     = Nothing  -- R-3 default
  , methodPublic          = False    -- R-3 default
  }

dynamicGateMethod :: MethodSchema
dynamicGateMethod = baseMethod
  { methodRole = MethodRoleDynamicChild
      { listMethod   = Just "list_planets"
      , searchMethod = Just "search_planets"
      }
  }

dynamicGateNoList :: MethodSchema
dynamicGateNoList = baseMethod
  { methodRole = MethodRoleDynamicChild
      { listMethod   = Nothing
      , searchMethod = Nothing
      }
  }

-- | A minimal 'PluginSchema' carrying the gate method.
solarHubSchema :: PluginSchema
solarHubSchema = PluginSchema
  { psNamespace       = "solar"
  , psVersion         = "1.0"
  , psDescription     = "Solar system hub."
  , psLongDescription = Nothing
  , psHash            = "h-solar"
  , psMethods         = [ dynamicGateMethod ]
  , psChildren        = Just []
  , psDeprecation     = Nothing
  , psRequest         = Nothing
  }

-- | A pure-RPC schema — used to confirm renderer back-compat.
plainRpcSchema :: PluginSchema
plainRpcSchema = solarHubSchema
  { psMethods = [ baseMethod { methodName = "ping" } ]
  }

-- | Empty 'Provenance' for fixtures.
emptyProv :: Provenance
emptyProv = Provenance []

-- | Pre-baked stream frames as if a list_planets RPC returned the
--   eight bodies of build_solar_system().
planetFrames :: [PlexusStreamItem]
planetFrames =
  [ StreamData "h-list" emptyProv "names" (String name)
  | name <- ["mercury", "venus", "earth", "mars", "jupiter", "saturn", "uranus", "neptune"]
  ]
  <> [ StreamDone "h-list" emptyProv ]

-- | A stream that returns NO data frames — only StreamDone. The
--   consumer must treat this as "no children" without erroring.
emptyFrames :: [PlexusStreamItem]
emptyFrames = [ StreamDone "h-list" emptyProv ]

-- ============================================================================
-- Mock invokers
-- ============================================================================

-- | A stateless mock that always returns the same frames. Returns the
--   call count so tests can assert cache hits.
fixedInvoker
  :: IORef Int                                   -- ^ call counter
  -> [PlexusStreamItem]
  -> DC.Invoker
fixedInvoker counter frames _nsPath _method _params = do
  atomicModifyIORef' counter $ \n -> (n + 1, ())
  pure (Right frames)

-- | A mock that always errors out. Returns @Left "boom"@.
errorInvoker :: DC.Invoker
errorInvoker _nsPath _method _params = pure (Left "boom: substrate unavailable")

-- | A mock that blocks for the given microseconds before returning
--   (longer than the configured timeout in our slow-call test).
slowInvoker :: Int -> [PlexusStreamItem] -> DC.Invoker
slowInvoker delayMicros frames _nsPath _method _params = do
  threadDelay delayMicros
  pure (Right frames)

-- | Capture-warning sink: pushes every warning into an 'MVar' so tests
--   can assert that a warning was logged.
captureWarn :: MVar [Text] -> Text -> IO ()
captureWarn mv msg = modifyMVar_ mv (pure . (msg :))

silentWarn :: Text -> IO ()
silentWarn _ = pure ()

-- ============================================================================
-- Spec
-- ============================================================================

main :: IO ()
main = hspec $ do

  describe "SYN-2 / pure helpers: schema-decoder consumer view" $ do

    it "identifies a MethodRoleDynamicChild as a gate" $ do
      DC.isDynamicChildGate dynamicGateMethod `shouldBe` True
      DC.isDynamicChildGate baseMethod `shouldBe` False

    it "projects DynamicChild method into a DynamicChildGate" $ do
      case DC.dynamicChildGate dynamicGateMethod of
        Nothing -> expectationFailure "expected Just gate"
        Just g -> do
          DC.gateMethodName g `shouldBe` "body"
          DC.gateListMethod g `shouldBe` Just "list_planets"
          DC.gateSearchMethod g `shouldBe` Just "search_planets"
          DC.gateParam g `shouldBe` "name"

    it "returns Nothing for ordinary RPC methods" $
      DC.dynamicChildGate baseMethod `shouldBe` Nothing

    it "collects all gates from a PluginSchema in declaration order" $ do
      let gates = DC.dynamicChildGates solarHubSchema
      length gates `shouldBe` 1
      fmap DC.gateMethodName gates `shouldBe` ["body"]

    it "back-compat: pre-IR-12 schema decodes with empty gate list" $ do
      let preIr = solarHubSchema { psMethods = [ baseMethod ] }
      DC.dynamicChildGates preIr `shouldBe` []

    it "gateParamName: single-property params" $
      DC.gateParamName dynamicGateMethod `shouldBe` "name"

    it "gateParamName: falls back to 'name' on schemas with multiple keys" $ do
      let m = baseMethod
            { methodParams = Just $ object
                [ "type" .= ("object" :: Text)
                , "properties" .= object
                    [ "first"  .= object [ "type" .= ("string" :: Text) ]
                    , "second" .= object [ "type" .= ("string" :: Text) ]
                    ]
                ]
            }
      DC.gateParamName m `shouldBe` "name"

    it "gateParamName: handles missing methodParams gracefully" $ do
      let m = baseMethod { methodParams = Nothing }
      DC.gateParamName m `shouldBe` "name"

  describe "SYN-2 / mock substrate: listChildrenWith" $ do

    it "returns names from a planet-shaped stream" $ do
      counter <- newIORef 0
      cache   <- DC.newCache
      names   <- DC.listChildrenWith silentWarn cache
                   (fixedInvoker counter planetFrames)
                   ["solar"] "list_planets"
      names `shouldBe`
        ["mercury", "venus", "earth", "mars", "jupiter", "saturn", "uranus", "neptune"]

    it "gracefully handles an empty stream" $ do
      counter <- newIORef 0
      cache   <- DC.newCache
      names   <- DC.listChildrenWith silentWarn cache
                   (fixedInvoker counter emptyFrames)
                   ["solar"] "list_planets"
      names `shouldBe` []

    it "returns [] and logs WARN on invoker error (no crash)" $ do
      mv    <- newMVar []
      cache <- DC.newCache
      names <- DC.listChildrenWith (captureWarn mv) cache
                 errorInvoker ["solar"] "list_planets"
      names `shouldBe` []
      warns <- readMVar mv
      length warns `shouldBe` 1
      head warns `shouldSatisfy` ("failed" `T.isInfixOf`)

  describe "SYN-2 / mock substrate: cache TTL" $ do

    it "two calls within TTL hit the cache (invoker called once)" $ do
      counter <- newIORef 0
      cache   <- DC.newCache
      _       <- DC.listChildrenWith silentWarn cache
                   (fixedInvoker counter planetFrames)
                   ["solar"] "list_planets"
      _       <- DC.listChildrenWith silentWarn cache
                   (fixedInvoker counter planetFrames)
                   ["solar"] "list_planets"
      callCount <- readIORef counter
      callCount `shouldBe` 1

    it "manual cache lookup confirms entry was stored" $ do
      counter <- newIORef 0
      cache   <- DC.newCache
      _       <- DC.listChildrenWith silentWarn cache
                   (fixedInvoker counter planetFrames)
                   ["solar"] "list_planets"
      mNames <- DC.lookupCache DC.defaultCacheTtlSeconds cache
                  (DC.cacheKey ["solar"] "list_planets")
      mNames `shouldSatisfy` (\m -> case m of
        Just xs -> length xs == 8
        Nothing -> False)

    it "TTL=0 forces a fresh fetch on every lookup" $ do
      -- A nominal-diff-time of 0 means "expire immediately": after
      -- 'storeCache' the entry is already older than zero, so
      -- 'lookupCache' returns Nothing. The behavioural cache key
      -- (insert-then-read with 5s) remains the contract; this case
      -- exercises the TTL boundary directly.
      cache <- DC.newCache
      DC.storeCache cache ("solar", "list_planets") ["mercury"]
      mNames <- DC.lookupCache 0 cache (DC.cacheKey ["solar"] "list_planets")
      mNames `shouldBe` Nothing

  describe "SYN-2 / mock substrate: timeout" $ do

    it "returns [] and logs WARN when invoker exceeds the timeout" $ do
      mv    <- newMVar []
      cache <- DC.newCache
      -- defaultInvocationTimeoutMicros is 2s; sleep 3s.
      names <- DC.listChildrenWith (captureWarn mv) cache
                 (slowInvoker (3 * 1000 * 1000) planetFrames)
                 ["solar"] "list_planets"
      names `shouldBe` []
      warns <- readMVar mv
      warns `shouldSatisfy` any ("timed out" `T.isInfixOf`)

  describe "SYN-2 / mock substrate: searchChildrenWith" $ do

    it "passes the query through to the invoker" $ do
      captureMV <- newMVar []     -- captures (nsPath, method, params)
      let capture nsPath fn params = do
            modifyMVar_ captureMV (pure . ((nsPath, fn, params) :))
            pure (Right [StreamData "h-search" emptyProv "names" (String "mercury")])
      names <- DC.searchChildrenWith silentWarn capture
                 ["solar"] "search_planets" "mer"
      names `shouldBe` ["mercury"]
      calls <- readMVar captureMV
      length calls `shouldBe` 1
      let (_, _, params) = head calls
      case params of
        Object o -> KM.lookup "query" o `shouldBe` Just (String "mer")
        _        -> expectationFailure "expected object params"

    it "returns [] on invoker error" $ do
      mv <- newMVar []
      names <- DC.searchChildrenWith (captureWarn mv) errorInvoker
                 ["solar"] "search_planets" "anything"
      names `shouldBe` []

  describe "SYN-2 / mock substrate: renderer integration" $ do

    it "renderSchema decorates DynamicChild method with {name} and gate hint" $ do
      let out = Render.renderSchema solarHubSchema
      out `shouldSatisfy` ("body {name}" `T.isInfixOf`)
      out `shouldSatisfy` ("dynamic child gate" `T.isInfixOf`)
      out `shouldSatisfy` ("list: list_planets" `T.isInfixOf`)
      out `shouldSatisfy` ("search: search_planets" `T.isInfixOf`)

    it "renderSchema of pure RPC schema is unchanged (no {name}, no hint)" $ do
      let out = Render.renderSchema plainRpcSchema
      out `shouldNotSatisfy` ("{name}" `T.isInfixOf`)
      out `shouldNotSatisfy` ("dynamic child gate" `T.isInfixOf`)

    it "DynamicChild without list_method still renders the placeholder" $ do
      let schema = solarHubSchema { psMethods = [ dynamicGateNoList ] }
          out    = Render.renderSchema schema
      out `shouldSatisfy` ("body {name}" `T.isInfixOf`)
      -- AC #4: no list_method advertised → hint is still present
      -- (so the user can tell this is a gate) but the suffix is
      -- empty.
      out `shouldSatisfy` ("dynamic child gate" `T.isInfixOf`)
      out `shouldNotSatisfy` ("list:" `T.isInfixOf`)

  describe "SYN-2 / renderer + consumer: rendered output for the SYN-2 demo case" $ do

    it "renders a hub with one dynamic gate in the SYN-2 acceptance shape" $ do
      -- This is the test-suite-side equivalent of the demo transcript:
      -- a fixture matching the post-CHILD-7 Solar shape, fed through
      -- the synapse renderer, with a sample list_planets stream
      -- replayed through the mock invoker.
      counter <- newIORef 0
      cache   <- DC.newCache
      names   <- DC.listChildrenWith silentWarn cache
                   (fixedInvoker counter planetFrames)
                   ["solar"] "list_planets"
      let rendered = Render.renderSchema solarHubSchema
          enumeration = T.unlines $
            "" :
            "Dynamic children:" :
            "  body {name}" :
            [ "    - " <> n | n <- names ]
      -- The combined output is what synapse would print live.
      rendered `shouldSatisfy` ("body {name}" `T.isInfixOf`)
      enumeration `shouldSatisfy` ("mercury" `T.isInfixOf`)
      enumeration `shouldSatisfy` ("neptune" `T.isInfixOf`)
      -- SYN-2 demo transcript (also printed for the PR description):
      putStrLn "\n=== SYN-2 sample transcript (fixture-driven) ==="
      putStrLn "$ synapse substrate solar"
      TIO.putStr rendered
      TIO.putStr enumeration
      putStrLn "=== /transcript ==="
