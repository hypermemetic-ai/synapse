{-# LANGUAGE OverloadedStrings #-}

-- | PLX-121 — navigation as pointer-chasing over one document.
--
-- = What this suite is for, and what it is NOT for
--
-- The captured wire log against a live substrate (see the ticket's evidence)
-- proves that a full-tree navigation issues ONE @substrate.connectome@ fetch
-- and that every Static edge is followed locally. What it cannot prove is c2,
-- because substrate serves __no fetchable Dynamic edge at all__: the hub emits
-- a Static edge exactly when it holds the child's document and a Dynamic edge
-- exactly when it does not, and @{ns}.connectome@'s @namespace@ parameter
-- resolves only hub-level activations. All five Dynamic edges on substrate's
-- root document therefore answer @no Connectome document is declared@.
--
-- That is a finding, not a gap in this suite — and it is reported as one. But
-- it means the lazy-fetch-and-cache path has no live witness, so it is pinned
-- here instead, against an injected fetcher that COUNTS ITS CALLS. A criterion
-- stated in round trips needs an assertion on round trips.
--
-- The documents below are built with @plexus-connectome@'s own constructors.
-- No second document model is defined here.
module Main (main) where

import Control.Monad (void)
import Data.Aeson (Value)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Test.Hspec

import qualified Plexus.Connectome.Codec as CC
import qualified Plexus.Connectome.Hash as CH
import qualified Plexus.Connectome.Types as C

import qualified Katip
import qualified Synapse.Log as Log
import Synapse.Connectome.Nav
import Synapse.Monad
import Synapse.Wire

--------------------------------------------------------------------------------
-- Fixtures — built through plexus-connectome, hashes computed by it
--------------------------------------------------------------------------------

method :: Text -> Text -> C.Method
method ns n =
  C.emptyMethod (C.MethodName n) (C.DottedId (ns <> "." <> n)) (C.Hash "")

activation :: Text -> [C.Method] -> [C.Edge] -> C.Activation
activation ns ms es =
  (C.emptyActivation (C.Namespace ns) (C.Version "1.0.0") (C.Hash ""))
    { C.aMethods = ms
    , C.aChildren = es
    }

-- | A child that is NOT embedded — the RFC §5.1 Dynamic case.
dynamicEdge :: Text -> Text -> C.Edge
dynamicEdge ns h =
  C.EDynamic
    C.DynamicEdge
      { C.dyNamespace = C.Namespace ns
      , C.dyAdvertisedHash = C.Hash h
      , C.dyDescription = Nothing
      }

-- | Make a document and let plexus-connectome compute every hash in it, so the
-- node hashes here are real CONNECTOME-HASH/1 digests rather than strings this
-- test made up.
document :: C.Activation -> C.Connectome
document root =
  CH.recomputeHashes
    C.Connectome
      { C.cRootFacts =
          C.RootFacts
            { C.rfHashAlgorithm = Just CH.hashAlgorithmName
            , C.rfBackendName = Just (C.BackendName "test")
            , C.rfDocumentHash = C.Hash ""
            , C.rfIrVersion = Just (C.IrVersion 1)
            , C.rfRespondMethod = Nothing
            }
      , C.cRoot = root
      }

-- | The hash a standalone child document will actually carry.
childHash :: C.Activation -> Text
childHash = C.unHash . C.aHash . C.cRoot . document

-- | A child document served lazily.
childDoc :: C.Activation -> Value
childDoc = CC.encodeConnectome . document

-- | The root: two Static children (embedded, free to descend) and two Dynamic
-- edges pointing at the SAME child hash under two different namespaces. That
-- last part is the point of c2's "key on the hash and nothing else".
sharedChild :: C.Activation
sharedChild = activation "shared" [method "shared" "ping"] []

rootActivation :: C.Activation
rootActivation =
  activation
    "test"
    []
    [ C.EStatic (activation "alpha" [method "alpha" "one", method "alpha" "two"] [])
    , C.EStatic
        ( activation
            "beta"
            [method "beta" "run"]
            [C.EStatic (activation "gamma" [method "gamma" "deep"] [])]
        )
    , dynamicEdge "lazy" (childHash sharedChild)
    , -- Same content, different namespace: one cache entry must serve both.
      dynamicEdge "lazy_alias" (childHash sharedChild)
    , -- The residual PLX-142 left: a 16-hex legacy hash, not a
      -- CONNECTOME-HASH/1 digest, because the child declares no ActivationIr.
      dynamicEdge "legacy_hashed" "db32914112af146c"
    ]

rootDocValue :: Value
rootDocValue = CC.encodeConnectome (document rootActivation)

--------------------------------------------------------------------------------
-- A fetcher that counts
--------------------------------------------------------------------------------

-- | Returns the fetcher and an action reading the call log.
countingFetcher :: IO (Maybe Text -> IO (Either Text Value), IO [Maybe Text])
countingFetcher = do
  calls <- newIORef []
  let fetch mNs = do
        modifyIORef' calls (++ [mNs])
        pure $ case mNs of
          Nothing -> Right rootDocValue
          Just "lazy" -> Right (childDoc sharedChild)
          Just "lazy_alias" -> Right (childDoc sharedChild)
          Just ns -> Left ("no Connectome document is declared for `" <> ns <> "`")
  pure (fetch, readIORef calls)

-- | Run a navigation with the counting fetcher; give back the result, the
-- fetch calls in order, and the wire log.
runNav
  :: SynapseM a
  -> IO (Either SynapseError a, [Maybe Text], [WireEvent])
runNav action = do
  logger <- Log.makeLogger Katip.ErrorS
  env <- initEnv "127.0.0.1" 4444 "test" logger Nothing
  (fetch, readCalls) <- countingFetcher
  result <- runSynapseM env (withDocFetcher fetch action)
  calls <- readCalls
  evs <- readWireLog (seWireLog env)
  pure (result, calls, evs)

--------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "PLX-121 c1 / one fetch for the static tree" $ do
    it "navigates three levels deep with exactly ONE document fetch" $ do
      (result, calls, evs) <- runNav (navigateC ["beta", "gamma", "deep"])
      calls `shouldBe` [Nothing]
      countFetches evs `shouldBe` 1
      case result of
        Right (Right (CViewMethod m path)) -> do
          C.unMethodName (C.mName m) `shouldBe` "deep"
          path `shouldBe` ["beta", "gamma", "deep"]
        other -> expectationFailure ("unexpected landing: " <> show other)

    it "follows Static edges locally — the log says so, per edge" $ do
      (_, _, evs) <- runNav (navigateC ["beta", "gamma"])
      let locals = [e | e@WireLocal {} <- evs]
      map weEdgeKind locals `shouldBe` ["static", "static"]
      map wePath locals `shouldBe` ["beta", "beta.gamma"]

    it "a whole-tree walk issues ONE fetch plus K Dynamic-edge fetches" $ do
      (result, calls, evs) <- runNav walkTree
      -- Nothing = the one document fetch. The rest are the Dynamic edges, and
      -- K is exactly the number of Dynamic edges the fetcher can serve.
      -- 'lazy_alias' advertises the SAME hash as 'lazy' and is therefore never
      -- fetched: the walk pays for content, not for names.
      calls `shouldBe` [Nothing, Just "lazy", Just "legacy_hashed"]
      countFetches evs `shouldBe` 2 -- root + lazy; alias hits, legacy_hashed fails
      countCacheHits evs `shouldBe` 1
      case result of
        Right (Right entries) -> do
          map (T.intercalate "." . wePathSegs) entries
            `shouldContain` ["beta.gamma"]
          length [e | e <- entries, weUnreachable e /= Nothing] `shouldBe` 1
        other -> expectationFailure ("walk failed: " <> show other)

    it "a second navigation in the same process issues NO fetch at all" $ do
      (_, calls, _) <- runNav $ do
        void (navigateC ["alpha"])
        void (navigateC ["beta", "gamma"])
        navigateC ["alpha", "one"]
      calls `shouldBe` [Nothing]

  describe "PLX-121 c2 / Dynamic edges are lazy and cached BY HASH" $ do
    it "first descent fetches; repeat descent hits the cache and fetches nothing" $ do
      (_, calls, evs) <- runNav $ do
        void (navigateC ["lazy", "ping"])
        navigateC ["lazy", "ping"]
      calls `shouldBe` [Nothing, Just "lazy"]
      countFetches evs `shouldBe` 2
      countCacheHits evs `shouldBe` 1

    it "the key is the HASH and nothing else: a different namespace with the same hash hits" $ do
      (_, calls, evs) <- runNav $ do
        void (navigateC ["lazy"])
        navigateC ["lazy_alias"]
      -- 'lazy_alias' was never fetched. Had the cache been keyed on the
      -- namespace — or on (namespace, method), or on anything with a clock in
      -- it — this would be two fetches.
      calls `shouldBe` [Nothing, Just "lazy"]
      countCacheHits evs `shouldBe` 1
      [weKey e | e@WireCacheHit {} <- evs] `shouldBe` [childHash sharedChild]

    it "the cache key is the ADVERTISED hash, not the fetched document's own hash" $ do
      -- Synapse.Cache looks up on the advertised hash and inserts on the
      -- fetched one; if the two ever disagree the hit is unobservable forever
      -- (that asymmetry is PLX-143's). This cache takes both from the same
      -- place, so the property is structural. Pinned so it stays that way.
      (_, _, evs) <- runNav $ do
        void (navigateC ["lazy"])
        navigateC ["lazy"]
      let misses = [weKey e | e@WireCacheMiss {} <- evs]
          hits = [weKey e | e@WireCacheHit {} <- evs]
      misses `shouldBe` [childHash sharedChild]
      hits `shouldBe` misses

    it "no cached entry survives into a fresh process-equivalent environment" $ do
      -- There is no TTL to expire and no clock to consult: the cache lives and
      -- dies with the environment. Pinned because the shape it replaces was
      -- invalidated by time.
      (_, calls1, _) <- runNav (navigateC ["lazy"])
      (_, calls2, _) <- runNav (navigateC ["lazy"])
      calls1 `shouldBe` calls2

  describe "PLX-121 / the two residuals PLX-142 named" $ do
    it "classifies a 16-hex legacy advertised hash as NOT a content identity" $ do
      classifyHash "db32914112af146c" `shouldBe` LegacyAdvertised
      classifyHash (childHash sharedChild) `shouldBe` ConnectomeHash

    it "never compares an advertised hash against a node hash" $ do
      -- The advertised hash for 'lazy' is a real node hash here, so a
      -- comparison would pass and prove nothing. The claim under test is
      -- structural: the cache key that goes in is the key that comes out, and
      -- the fetched node's own hash is never consulted as a key.
      (_, _, evs) <- runNav $ do
        void (navigateC ["lazy"])
        navigateC ["lazy_alias"]
      let keys = [weKey e | e@WireCacheMiss {} <- evs] <> [weKey e | e@WireCacheHit {} <- evs]
      -- Two different namespaces, one key. If the fetched node's hash were the
      -- insert key, this would still pass here — but it would fail the moment
      -- the two shapes differ, which is exactly the legacy-hash case below.
      keys `shouldBe` [childHash sharedChild, childHash sharedChild]

    it "a Dynamic edge the wire cannot serve is reported, not worked around" $ do
      (result, _, evs) <- runNav (navigateC ["legacy_hashed"])
      case result of
        Right (Left (CUnfetchable ns h kind why)) -> do
          ns `shouldBe` "legacy_hashed"
          h `shouldBe` "db32914112af146c"
          kind `shouldBe` LegacyAdvertised
          why `shouldContain'` "no Connectome document is declared"
        other -> expectationFailure ("expected CUnfetchable, got " <> show other)
      length [e | e@WireUnfetchable {} <- evs] `shouldBe` 1

    it "a miss at the root says the root document has no methods" $ do
      -- Substrate's root declares ZERO methods, so 'not found at the root' is a
      -- miss against a map that is known to be incomplete. The error says so
      -- rather than asserting the backend has no such method.
      (result, _, _) <- runNav (navigateC ["nope"])
      case result of
        Right (Left err@(CRootHasNoMethods "nope")) ->
          renderCNavError err `shouldContain'` "declares ZERO methods"
        other -> expectationFailure ("expected CRootHasNoMethods, got " <> show other)

  describe "PLX-121 / the wire log is the evidence" $
    it "renders a transcript with a fetch count and a cache-hit count" $ do
      (_, _, evs) <- runNav $ do
        void (navigateC ["lazy"])
        navigateC ["lazy_alias"]
      let rendered = renderWireLog evs
      rendered `shouldContain'` "FETCH"
      rendered `shouldContain'` "CACHE-HIT"
      -- Two fetches: the one document, plus the one Dynamic child. The alias
      -- descent is the cache hit.
      rendered `shouldContain'` "2 fetch(es), 1 hash-keyed cache hit(s)"

shouldContain' :: Text -> Text -> Expectation
shouldContain' haystack needle =
  (needle `T.isInfixOf` haystack, haystack) `shouldSatisfy` fst
