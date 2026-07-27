{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | PLX-124 — M3·E: the 18-item completeness inventory, one test each.
--
-- = What this suite is
--
-- PLX-73's @q-ir-completeness@ spike answer enumerated __18 things synapse
-- reads from a plexus backend__ and grouped them by disposition. M3's exit
-- gate is stated as "every operation in that inventory passes". This suite is
-- that sentence made mechanical: one @describe@ per item, numbered @item01@ to
-- @item18@ so the mapping to the inventory is not a matter of interpretation.
--
-- = The rule this suite obeys
--
-- __An item is never narrowed so that it happens to pass.__ Several items are
-- genuinely unmet as of this build, and each of those is asserted as a
-- __negative pin__: a test that encodes the gap, cites the ticket that owns it,
-- and __fails when the gap closes__. A pin that breaks is the suite doing its
-- job — it means somebody closed something and this file must be re-read, not
-- that somebody broke something.
--
-- = PLX-157 inverted two of these pins (2026-07-27)
--
-- Items 5 and 8 are now MET by a producer, and PLX-124's negative pins for
-- them were __inverted, not deleted__: each @it@ still asserts the gap against
-- PLX-124's dated capture and then asserts its closure against a new one, so
-- the flip is auditable. PLX-112's precedent.
--
-- One residual is pinned in item05's place: the @_info@ probe survives as the
-- __bootstrap__ and cannot be removed by any amount of adoption, because the
-- document that carries the backend's name is served at
-- @{namespace}.connectome@ and a client needs the name to ask for it. That is
-- an RFC 002 question, filed rather than worked around.
--
-- Items NOT met, with owners, as of 2026-07-27:
--
-- * __item09__ (credential metadata) — met in the direction that matters
--   (nothing is read from the server), but the richer axis is absent from the
--   Connectome. __PLX-120, deferred with a stated cost.__
-- * __item16__ (dynamic-instance descent) — __PLX-122, RFC-blocked on §10.1.__
-- * __item17__ (list/search response shape declared by the IR) — still guessed;
--   blocked behind item16 on the same §10.1 amendment.
--
-- And one item whose live witness is missing rather than whose behaviour is
-- wrong:
--
-- * __item01/item02 lazy half__ — 0 of 5 Dynamic edges on substrate are
--   fetchable (__PLX-148__), so the hash-keyed cache __hit__ has no live
--   witness and is pinned here against a counting fetcher instead.
--
-- = Where the coverage already exists
--
-- Items 3, 4, 12 and 13 are covered end to end by suites written for the
-- builds that produced them (@refetch-scars-test@, @dynamic-children-test@,
-- @turn-test@, @stream-tracker-test@). This suite does not re-litigate them;
-- it asserts the __structural residue__ that proves the change is still in
-- place, and the two-direction matrix in PLX-124 names the owning suite.
module Main (main) where

import Control.Monad (forM)
import Data.Aeson (Value, decodeFileStrict', object, (.=))
import qualified Data.Aeson as Aeson
import Data.IORef
import Data.List (isInfixOf, sort)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Test.Hspec

import qualified Plexus.Connectome.Codec as CC
import qualified Plexus.Connectome.Hash as CH
import qualified Plexus.Connectome.Types as C
import qualified Plexus.Schema.Recursive as W
import qualified Plexus.Types as PT

import qualified Katip
import Plexus.Client (SubstrateConfig (..))
import qualified Plexus.Transport as ST
import Synapse.Backend.Discovery (Backend (..), pingBackend)
import qualified Synapse.Backend.Discovery as Disc
import Synapse.Connectome
import Synapse.Connectome.Nav
import Synapse.DynamicChildren
import qualified Synapse.Log as Log
import Synapse.Monad
import Synapse.Transport (extractErrCode, renderProtocolError)
import Synapse.Wire

--------------------------------------------------------------------------------
-- Legacy-wire fixtures (the parity baseline: what the OLD synapse read)
--------------------------------------------------------------------------------

bareMethod :: Text -> W.MethodSchema
bareMethod n =
  W.MethodSchema
    { W.methodName = n
    , W.methodDescription = ""
    , W.methodHash = "hash-" <> n
    , W.methodParams = Nothing
    , W.methodReturns = Nothing
    , W.methodStreaming = False
    , W.methodBidirectional = False
    , W.methodRequestType = Nothing
    , W.methodResponseType = Nothing
    , W.methodDeprecation = Nothing
    , W.methodParamSchemas = Nothing
    , W.methodRole = W.MethodRoleRpc
    , W.methodCredentials = []
    , W.methodRequiresCredential = Nothing
    , W.methodAuthPosture = Nothing
    , W.methodPublic = False
    }

streamingMethod :: W.MethodSchema
streamingMethod =
  (bareMethod "chat")
    { W.methodStreaming = True
    , W.methodReturns = Just (object ["type" .= ("object" :: String)])
    }

bidirMethod :: W.MethodSchema
bidirMethod =
  (bareMethod "interact")
    { W.methodBidirectional = True
    , W.methodRequestType = Just "StandardRequest"
    , W.methodResponseType = Just "StandardResponse"
    }

legacyRoot :: W.PluginSchema
legacyRoot =
  W.PluginSchema
    { W.psNamespace = "substrate"
    , W.psVersion = "0.6.0"
    , W.psDescription = "the substrate root"
    , W.psLongDescription = Nothing
    , W.psHash = "root-hash"
    , W.psMethods = [streamingMethod, bareMethod "ping", bidirMethod]
    , W.psChildren = Just [W.ChildSummary "cone" "llm agents" "cone-hash"]
    , W.psDeprecation = Nothing
    , W.psRequest = Nothing
    }

lifted :: C.Connectome
lifted = liftConnectome "substrate" legacyRoot

--------------------------------------------------------------------------------
-- Connectome fixtures for navigation (built with plexus-connectome's own
-- constructors — no second document model is defined here)
--------------------------------------------------------------------------------

cMethod :: Text -> Text -> C.Method
cMethod ns n = C.emptyMethod (C.MethodName n) (C.DottedId (ns <> "." <> n)) (C.Hash "")

cActivation :: Text -> [C.Method] -> [C.Edge] -> C.Activation
cActivation ns ms es =
  (C.emptyActivation (C.Namespace ns) (C.Version "1.0.0") (C.Hash ""))
    { C.aMethods = ms
    , C.aChildren = es
    }

cDocument :: C.Activation -> C.Connectome
cDocument root =
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

lazyChild :: C.Activation
lazyChild = cActivation "lazy" [cMethod "lazy" "ping"] []

lazyChildHash :: Text
lazyChildHash = C.unHash (C.aHash (C.cRoot (cDocument lazyChild)))

-- | Root with a Static chain (three levels), an Indexed family, and two
-- Dynamic edges that advertise the SAME content hash under different names.
gateRoot :: C.Activation
gateRoot =
  cActivation
    "gate"
    []
    [ C.EStatic
        ( cActivation
            "alpha"
            [cMethod "alpha" "one"]
            [C.EStatic (cActivation "beta" [cMethod "beta" "deep"] [])]
        )
    , C.EIndexed
        C.IndexedEdge
          { C.ixNamespace = C.Namespace "session"
          , C.ixListMethod = C.DottedId "session.list"
          , C.ixSearchMethod = Nothing
          , C.ixIdField = C.FieldName "session_id"
          , C.ixPathTemplate = C.PathTemplate "session/{id}"
          , C.ixTemplate = cActivation "session" [cMethod "session" "chat"] []
          , C.ixDescription = Nothing
          }
    , C.EDynamic
        C.DynamicEdge
          { C.dyNamespace = C.Namespace "lazy"
          , C.dyAdvertisedHash = C.Hash lazyChildHash
          , C.dyDescription = Nothing
          }
    , C.EDynamic
        C.DynamicEdge
          { C.dyNamespace = C.Namespace "lazy_alias"
          , C.dyAdvertisedHash = C.Hash lazyChildHash
          , C.dyDescription = Nothing
          }
    ]

gateDocValue :: Value
gateDocValue = CC.encodeConnectome (cDocument gateRoot)

-- | A fetcher that counts its calls. The exit gate is stated in round trips,
-- so the assertion has to be on round trips.
runGate :: SynapseM a -> IO (Either SynapseError a, [Maybe Text], [WireEvent])
runGate action = do
  logger <- Log.makeLogger Katip.ErrorS
  env <- initEnv "127.0.0.1" 4444 "gate" logger Nothing
  calls <- newIORef []
  let fetch mNs = do
        modifyIORef' calls (++ [mNs])
        pure $ case mNs of
          Nothing -> Right gateDocValue
          Just "lazy" -> Right (CC.encodeConnectome (cDocument lazyChild))
          Just "lazy_alias" -> Right (CC.encodeConnectome (cDocument lazyChild))
          Just ns -> Left ("no Connectome document is declared for `" <> ns <> "`")
  result <- runSynapseM env (withDocFetcher fetch action)
  cs <- readIORef calls
  evs <- readWireLog (seWireLog env)
  pure (result, cs, evs)

--------------------------------------------------------------------------------
-- Source-level pins (the mechanical half of "the round trip is gone")
--------------------------------------------------------------------------------

-- | Every @.hs@ under a directory, recursively.
haskellFilesUnder :: FilePath -> IO [FilePath]
haskellFilesUnder dir = do
  entries <- listDirectory dir
  fmap concat . forM entries $ \e -> do
    let p = dir </> e
    isDir <- doesDirectoryExist p
    if isDir
      then haskellFilesUnder p
      else pure [p | ".hs" `T.isSuffixOf` T.pack p]

readSource :: FilePath -> IO String
readSource = fmap T.unpack . TIO.readFile

--------------------------------------------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  ------------------------------------------------------------------------------
  -- Group 1 of the inventory: "eliminated by a recursive IR document"
  ------------------------------------------------------------------------------

  describe "item01 — child methods / grandchildren / is-hub: one .schema per LEVEL is gone" $ do
    it "descends three levels with exactly ONE document fetch" $ do
      (result, calls, evs) <- runGate (navigateC ["alpha", "beta", "deep"])
      calls `shouldBe` [Nothing]
      countFetches evs `shouldBe` 1
      case result of
        Right (Right (CViewMethod m path)) -> do
          C.unMethodName (C.mName m) `shouldBe` "deep"
          path `shouldBe` ["alpha", "beta", "deep"]
        other -> expectationFailure ("unexpected landing: " <> show other)

    it "reaches an Indexed family's shape locally too — the template is embedded" $ do
      (result, calls, _) <- runGate (navigateC ["session", "chat"])
      calls `shouldBe` [Nothing]
      case result of
        Right (Right (CViewMethod m _)) ->
          C.unMethodName (C.mName m) `shouldBe` "chat"
        other -> expectationFailure ("unexpected landing: " <> show other)

    it "the legacy per-level fetch still exists and is NOT the Connectome path (PLX-148)" $ do
      -- Honest statement of the residual: `--connectome` is additive and the
      -- legacy path is untouched, because the served document is not yet a
      -- complete map. When PLX-148 closes and the default cuts over, this pin
      -- fails and this file must be re-read.
      src <- readSource "src/Synapse/Algebra/Navigate.hs"
      src `shouldSatisfy` isInfixOf "fetchSchemaAt"

  describe "item02 — a per-node hash, for cycle detection AND cache keying" $ do
    it "the hash is the cache key: a different namespace with the same hash HITS" $ do
      (_, calls, evs) <- runGate $ do
        _ <- navigateC ["lazy"]
        navigateC ["lazy_alias"]
      calls `shouldBe` [Nothing, Just "lazy"]
      countCacheHits evs `shouldBe` 1
      [weKey e | e@WireCacheHit {} <- evs] `shouldBe` [lazyChildHash]

    it "repeat descent to a seen hash issues NO fetch" $ do
      (_, calls, evs) <- runGate $ do
        _ <- navigateC ["lazy", "ping"]
        navigateC ["lazy", "ping"]
      calls `shouldBe` [Nothing, Just "lazy"]
      countCacheHits evs `shouldBe` 1

    it "cycle detection is by NODE hash, and it is reachable" $ do
      -- The document is a real cycle: the lazily-fetched child advertises a
      -- Dynamic edge back to itself under a second name, so the same node hash
      -- appears twice on one descent.
      logger <- Log.makeLogger Katip.ErrorS
      env <- initEnv "127.0.0.1" 4444 "cyc" logger Nothing
      let selfRefName = "loop"
          -- The child's own document, once hashed, is what the edge advertises.
          childOf h =
            cActivation
              "lazy"
              [cMethod "lazy" "ping"]
              [ C.EDynamic
                  C.DynamicEdge
                    { C.dyNamespace = C.Namespace selfRefName
                    , C.dyAdvertisedHash = C.Hash h
                    , C.dyDescription = Nothing
                    }
              ]
          -- Fixed point: hash the child that points at its own hash. One
          -- iteration is enough to make the SAME node hash appear twice,
          -- because the second descent re-serves the identical document.
          h0 = C.unHash (C.aHash (C.cRoot (cDocument (childOf ""))))
          childDoc = CC.encodeConnectome (cDocument (childOf h0))
          rootV =
            CC.encodeConnectome . cDocument $
              cActivation
                "cyc"
                []
                [ C.EDynamic
                    C.DynamicEdge
                      { C.dyNamespace = C.Namespace "lazy"
                      , C.dyAdvertisedHash = C.Hash h0
                      , C.dyDescription = Nothing
                      }
                ]
          fetch = \case
            Nothing -> pure (Right rootV)
            Just _ -> pure (Right childDoc)
      r <- runSynapseM env (withDocFetcher fetch (navigateC ["lazy", selfRefName, "ping"]))
      case r of
        Right (Left (CCycle _ _)) -> pure ()
        other ->
          -- If this ever stops being a cycle the detector has changed shape;
          -- say so loudly rather than passing.
          expectationFailure ("expected CCycle, got: " <> show other)

    it "an advertised hash of the LEGACY shape is classified, never coerced" $ do
      classifyHash "db32914112af146c" `shouldNotBe` classifyHash lazyChildHash

  describe "item03 — `--schema method` no longer re-fetches the whole plugin" $ do
    -- Behaviour is owned by refetch-scars-test (PLX-143). What is pinned here
    -- is the structural residue: the function whose ONLY caller was the scar.
    it "nothing calls fetchMethodSchema any more — the scar's only helper is dead" $ do
      files <- (++) <$> haskellFilesUnder "src" <*> pure ["app/Main.hs"]
      callers <- fmap concat . forM files $ \f -> do
        s <- readSource f
        -- The tombstone comment in Synapse.Transport is allowed to survive;
        -- a call is not. A call would be `fetchMethodSchema` followed by an
        -- argument, never by `At` (plexus-protocol's own unrelated helper).
        pure [f | any (isInfixOf "fetchMethodSchema ") (filter (not . isInfixOf "--") (lines s))]
      callers `shouldBe` []

    it "ViewMethod carries the parent the scar used to re-fetch" $ do
      src <- readSource "src/Synapse/Schema/Types.hs"
      src `shouldSatisfy` isInfixOf "ViewMethod MethodSchema PluginSchema Path"

  describe "item04 — the parent deprecation warning no longer re-fetches the parent" $
    it "the deprecation read is served from the carried parent, with no transport call" $ do
      src <- readSource "app/Main.hs"
      -- The scar was `fetchSchemaAt parentPath` purely to read psDeprecation.
      let deprLines =
            [ l | l <- lines src, "psDeprecation" `isInfixOf` l ]
      deprLines `shouldNotSatisfy` any (isInfixOf "fetchSchemaAt")

  ------------------------------------------------------------------------------
  -- Group 2: "already in schema / pure convention the IR formalizes"
  ------------------------------------------------------------------------------

  describe "item05 — root backend name: MET by declaration; the probe survives only as bootstrap" $ do
    it "the lift has to be TOLD the backend name — the LEGACY wire does not carry it" $ do
      -- liftConnectome takes the name as an argument precisely because the
      -- legacy wire has no field for it. That is unchanged by PLX-157 and is
      -- why item 5 could never be closed on the lift side.
      C.rfBackendName (C.cRootFacts lifted) `shouldBe` Just (C.BackendName "substrate")
      let anonymous = liftConnectome "" legacyRoot
      C.rfBackendName (C.cRootFacts anonymous) `shouldBe` Just (C.BackendName "")

    -- PLX-157 — PLX-124's two negative pins for item 5, INVERTED rather than
    -- deleted. Each `it` below is the same assertion with the condition
    -- flipped, and the superseded capture is still asserted on, so the flip is
    -- auditable rather than asserted.

    it "INVERTED PIN (was: the SERVED document declares no backend_name)" $ do
      -- The gap, still pinned from the historical side: PLX-124's capture of
      -- 2026-07-27 carried `namespace` and no `backend_name`, so a client had
      -- to probe.
      old <- servedDocumentPlx124
      C.rfBackendName (C.cRootFacts old) `shouldBe` Nothing
      -- And the gap, closed: a document captured from the emitter after
      -- `DynamicHub::with_backend_name` declares §3.3's root fact. This is the
      -- assertion PLX-124 said would fail when the gap closed; it now holds in
      -- the opposite direction.
      doc <- servedDocument157
      C.rfBackendName (C.cRootFacts doc) `shouldBe` Just (C.BackendName "substrate")

    it "INVERTED PIN (was: _info is the discovery mechanism, uncached)" $ do
      src <- readSource "src/Synapse/Backend/Discovery.hs"
      -- `_info` is still on the wire, and deliberately: it is the only
      -- UNNAMESPACED method the protocol offers, and `{ns}.connectome` needs
      -- the namespace, so the very first identity resolution cannot come from
      -- the document. What changed is that it is no longer uncached and no
      -- longer preferred.
      src `shouldSatisfy` isInfixOf "\"_info\""
      -- Measured live through docs/evidence/plx-157-info-counter.py against a
      -- substrate on :4499, same command and same server both times:
      --   BEFORE (plx-124-parity): _info round trips: 3
      --   AFTER  (this commit):    _info round trips: 1
      -- The memo is what deletes the other two.
      src `shouldSatisfy` isInfixOf "backendIdentityCache"
      src `shouldSatisfy` isInfixOf "noteBackendNameFromRootFact"
      -- The root fact is PREFERRED: a document's `backend_name` is seeded into
      -- the identity cache the moment the document arrives, so no later
      -- resolution for that endpoint probes at all.
      nav <- readSource "src/Synapse/Connectome/Nav.hs"
      nav `shouldSatisfy` isInfixOf "noteBackendNameFromRootFact"

    it "the memo answers a second resolution with ZERO additional probes" $ do
      -- The mechanical half of the 3 -> 1 measurement, hermetic: no server, so
      -- every probe fails, and the point is how MANY failed probes are paid.
      Disc.resetBackendIdentityCache
      _ <- Disc.getBackendAt "127.0.0.1" unusedPort
      afterFirst <- Disc.backendIdentityProbeCount
      afterFirst `shouldBe` 1
      _ <- Disc.getBackendAt "127.0.0.1" unusedPort
      _ <- Disc.getBackendAt "127.0.0.1" unusedPort
      afterThird <- Disc.backendIdentityProbeCount
      -- Was 3 before PLX-157: a dead endpoint cost three 2-second timeouts.
      afterThird `shouldBe` 1

    it "a root fact preempts the probe entirely — the document wins" $ do
      Disc.resetBackendIdentityCache
      Disc.noteBackendNameFromRootFact "127.0.0.1" unusedPort "substrate"
      name <- Disc.getBackendAt "127.0.0.1" unusedPort
      name `shouldBe` Just "substrate"
      probes <- Disc.backendIdentityProbeCount
      probes `shouldBe` 0

    it "RESIDUAL, pinned: the bootstrap probe is not removable at the RFC level" $ do
      -- Item 5's phrasing was "a root backend name REPLACES the _info probe".
      -- It cannot, for the first resolution: the document that carries the
      -- name is served at `{namespace}.connectome`, so a client must already
      -- know the name to ask for it. `_info` is unnamespaced and is the only
      -- bootstrap on offer. This asserts the circularity rather than describing
      -- it: the method name is built FROM the backend name.
      let cfg = probeConfig "substrate"
      ST.connectomeMethodName cfg `shouldBe` "substrate.connectome"
      ST.connectomeMethodName (probeConfig "other") `shouldBe` "other.connectome"
      -- Closing the last probe is an RFC 002 change (a well-known unnamespaced
      -- document endpoint), not a synapse one. Filed, not diverged from.

  describe "item06 — the dotted method-id convention, made explicit per node" $ do
    it "every lifted method carries namespace.name as its dotted id (§10.1)" $ do
      let ms = C.aMethods (C.cRoot lifted)
      sort [C.unDottedId (C.mDottedId m) | m <- ms]
        `shouldBe` sort ["substrate.chat", "substrate.ping", "substrate.interact"]

    it "the served document carries a dotted id on every method, per node" $ do
      doc <- servedDocument
      let walk a = C.aMethods a ++ concatMap edge (C.aChildren a)
          edge = \case
            C.EStatic c -> walk c
            C.EIndexed ix -> walk (C.ixTemplate ix)
            C.EDynamic _ -> []
          ms = walk (C.cRoot doc)
      ms `shouldNotBe` []
      [m | m <- ms, T.null (C.unDottedId (C.mDottedId m))] `shouldBe` []

  describe "item07 — the streaming flag is informational only" $ do
    it "it is decoded and carried" $
      W.methodStreaming streamingMethod `shouldBe` True

    it "its ONE semantic use is §6.4: a streaming terminal is Unresolved" $ do
      let ms = C.aMethods (C.cRoot lifted)
          Just chat = lookupMethod "chat" ms
          Just ping = lookupMethod "ping" ms
      case C.mTerminal chat of
        C.Unresolved _ -> pure ()
        other -> expectationFailure ("expected Unresolved terminal, got: " <> show other)
      -- A non-streaming method with no returns declares nothing rather than
      -- guessing; a streaming one cannot be resolved from one `returns` blob.
      C.mTerminal ping `shouldBe` C.NotDeclared

    it "dispatch does not branch on it — every call goes down the streaming path" $ do
      src <- readSource "app/Main.hs"
      src `shouldSatisfy` isInfixOf "invokeStreamingWithBidirCancellable"
      -- If dispatch ever consults mdStreaming, this stops being "informational
      -- only" and the inventory item changes meaning.
      let dispatchLines = [l | l <- lines src, "invokeStreaming" `isInfixOf` l]
      dispatchLines `shouldNotSatisfy` any (isInfixOf "mdStreaming")

  describe "item08 — bidirectional / request_type / response_type, and respond_method" $ do
    it "the bidir axis survives the lift's INPUT and is still readable on the wire" $ do
      W.methodBidirectional bidirMethod `shouldBe` True
      W.methodRequestType bidirMethod `shouldBe` Just "StandardRequest"
      W.methodResponseType bidirMethod `shouldBe` Just "StandardResponse"

    it "INVERTED PIN (was: no document declares a reply-channel method, §7.6)" $ do
      -- PLX-157. The lift still cannot produce one — the legacy wire has no
      -- field for it — so that half of the pin stands unchanged.
      C.rfRespondMethod (C.cRootFacts lifted) `shouldBe` Nothing
      -- The gap, still pinned from the historical side.
      old <- servedDocumentPlx124
      C.rfRespondMethod (C.cRootFacts old) `shouldBe` Nothing
      -- The gap, closed by a PRODUCER: substrate declares the reply channel,
      -- so a consumer can now tell before invoking whether it can serve a
      -- declared callback. This is the assertion PLX-124 said would fail when
      -- the gap closed.
      doc <- servedDocument157
      C.rfRespondMethod (C.cRootFacts doc)
        `shouldBe` Just (C.MethodName "substrate.respond")

    it "the declared channel is the one the runtime actually registers" $ do
      -- The §7.6 advisory is only retired honestly if the declared name is
      -- callable. plexus-core registers `{ns}.respond`; the declaration must
      -- be that dotted id and not the bare `respond` clients assumed.
      doc <- servedDocument157
      let ns = C.unNamespace (C.aNamespace (C.cRoot doc))
      C.rfRespondMethod (C.cRootFacts doc)
        `shouldBe` Just (C.MethodName (ns <> ".respond"))

    it "the client-side `.respond` CONVENTION now has a declaration to agree with" $ do
      src <- readSource "../plexus-protocol/src/Plexus/Transport.hs"
      src `shouldSatisfy` isInfixOf "\".respond\""

  describe "item09 — credential metadata is rendering only; auth comes from the LOCAL chain" $ do
    it "no module under Synapse/Self reads the server's document, at all" $ do
      -- PLX-120's finding, asserted rather than quoted: Self resolution reads
      -- ZERO fields from any server-supplied schema. This is what makes the
      -- Connectome's thin `auth` field survivable.
      files <- haskellFilesUnder "src/Synapse/Self"
      offenders <- fmap concat . forM files $ \f -> do
        s <- readSource f
        pure
          [ f
          | any
              (`isInfixOf` s)
              [ "Plexus.Schema.Recursive"
              , "Plexus.Connectome"
              , "Synapse.Connectome"
              ]
          ]
      offenders `shouldBe` []

    it "NOT MET (deferred, PLX-120): the Connectome cannot carry the richer axis" $ do
      -- The legacy wire's posture axis exists on the input and has nowhere to
      -- go. mAuth stays absent rather than being smuggled through ext(...),
      -- which §10.4 forbids. Cost is borne by hub-codegen; owner is PLX-120.
      let ms = C.aMethods (C.cRoot lifted)
      [C.mAuth m | m <- ms] `shouldSatisfy` all isNothing
      [C.unExtensions (C.mExtensions m) | m <- ms] `shouldSatisfy` all null

  ------------------------------------------------------------------------------
  -- Group 3: "legitimately dynamic — stays as lazy runtime edges"
  ------------------------------------------------------------------------------

  describe "item10 — the liveness probe stays a live 300ms _info race" $
    it "an unreachable backend is reported unreachable, not fatal" $ do
      -- Port 1 is the same address refetch-scars-test uses to make a surviving
      -- round trip fail loudly. The probe must answer, not throw.
      b <-
        pingBackend
          Backend
            { backendName = "nobody"
            , backendDescription = ""
            , backendHost = "127.0.0.1"
            , backendPort = 1
            , backendVersion = Nothing
            , backendReachable = Nothing
            }
      backendReachable b `shouldBe` Just False

  describe "item11 — the registry.* sibling-backend directory is a live call" $
    it "discovery names registry.list / registry.get / registry.deregister" $ do
      src <- readSource "src/Synapse/Backend/Discovery.hs"
      let invocation m = "[\"registry\"] \"" <> m <> "\""
      src `shouldSatisfy` isInfixOf (invocation "list")
      src `shouldSatisfy` isInfixOf (invocation "get")
      src `shouldSatisfy` isInfixOf (invocation "deregister")
      -- And it is correctly NOT in the document: the Connectome has no notion
      -- of a sibling backend, which is the inventory's own verdict.
      doc <- servedDocument
      C.unNamespace (C.aNamespace (C.cRoot doc)) `shouldBe` "substrate"

  describe "item12 — dynamic-child instance ENUMERATION stays a live call" $ do
    -- Behaviour owned by dynamic-children-test (SYN-2 / PLX-121).
    it "the IR carries WHICH method to call, and the names come off the wire" $ do
      let gate = bareMethod "list_bodies"
          schema = legacyRoot {W.psMethods = [gate {W.methodRole = W.MethodRoleDynamicChild (Just "list_bodies") Nothing}]}
      map (T.pack . show . isJust . dynamicChildGate) (W.psMethods schema)
        `shouldBe` ["True"]

    it "the TTL cache is gone: two identical enumerations invoke TWICE" $ do
      calls <- newIORef (0 :: Int)
      let inv _ _ _ = do
            modifyIORef' calls (+ 1)
            pure (Right [])
      _ <- listChildrenWith silentWarn inv ["solar"] "list_bodies"
      _ <- listChildrenWith silentWarn inv ["solar"] "list_bodies"
      readIORef calls `shouldReturn` 2

  describe "item13 — stream frames and their metadata" $ do
    it "a data frame's metadata decodes and is available" $ do
      let raw =
            object
              [ "type" .= ("data" :: Text)
              , "content_type" .= ("echo.echo" :: Text)
              , "content" .= object ["message" .= ("hi" :: Text)]
              , "metadata"
                  .= object
                    [ "provenance" .= (["substrate"] :: [Text])
                    , "plexus_hash" .= ("abc" :: Text)
                    , "timestamp" .= (1 :: Int)
                    ]
              ]
      case Aeson.fromJSON raw :: Aeson.Result PT.PlexusStreamItem of
        Aeson.Success _ -> pure ()
        Aeson.Error e -> expectationFailure ("data frame did not decode: " <> e)

    it "RESIDUAL: every synapse consumer discards the metadata it decodes" $ do
      -- Named, not fixed. Provenance/hash/timestamp reach the client and are
      -- bound as wildcards everywhere. Pinning it means a future consumer is a
      -- deliberate change rather than a surprise.
      src <- readSource "src/Synapse/Renderer.hs"
      src `shouldSatisfy` isInfixOf "HubData _ _"

  describe "item14 — guidance frames, including the server-patched method_schema" $ do
    it "a guidance frame carrying an inline method_schema decodes losslessly" $ do
      let raw =
            object
              [ "type" .= ("guidance" :: Text)
              , "metadata"
                  .= object
                    [ "provenance" .= (["substrate"] :: [Text])
                    , "plexus_hash" .= ("abc" :: Text)
                    , "timestamp" .= (1 :: Int)
                    ]
              , "error_type"
                  .= object
                    [ "error_kind" .= ("method_not_found" :: Text)
                    , "activation" .= ("echo" :: Text)
                    , "method" .= ("ecko" :: Text)
                    ]
              , "suggestion"
                  .= object
                    [ "action" .= ("try_method" :: Text)
                    , "method" .= ("echo" :: Text)
                    ]
              , "available_methods" .= (["echo"] :: [Text])
              , "method_schema" .= object ["name" .= ("echo" :: Text)]
              ]
      case Aeson.fromJSON raw :: Aeson.Result PT.PlexusStreamItem of
        Aeson.Success PT.StreamGuidance {} -> pure ()
        Aeson.Success other -> expectationFailure ("wrong constructor: " <> show other)
        Aeson.Error e -> expectationFailure ("guidance did not decode: " <> e)

    it "RESIDUAL: the inline method_schema has no consumer — the channel is decoded and dropped" $ do
      -- The inventory's own note: guidance's inline schema is the "server
      -- patches the client's schema gap" channel, redundant once the IR is
      -- complete. It is redundant already — nothing reads it. Pinned so that
      -- deleting the field, or starting to use it, is a visible decision.
      srcs <- (++) <$> haskellFilesUnder "src" <*> pure ["app/Main.hs"]
      readers <- fmap concat . forM srcs $ \f -> do
        s <- readSource f
        pure [f | "itemMethodSchema" `isInfixOf` s]
      readers `shouldBe` []

  describe "item15 — the error-code conventions synapse scrapes" $ do
    it "the four documented codes each get their semantic prefix" $ do
      let wire c = "Subscription error: RpcErrorObj {errCode = " <> c <> ", errMessage = \"nope\", errData = Nothing}"
      extractErrCode (wire "-32001") `shouldBe` Just (-32001)
      renderProtocolError (wire "-32001") `shouldSatisfy` T.isPrefixOf "Authentication required: "
      renderProtocolError (wire "-32601") `shouldSatisfy` T.isPrefixOf "Method not found: "
      renderProtocolError (wire "-32602") `shouldSatisfy` T.isPrefixOf "Invalid parameters: "
      renderProtocolError (wire "-32000") `shouldSatisfy` T.isPrefixOf "Execution error: "

    it "an unrecognised shape degrades to the raw message, never to a wrong code" $ do
      extractErrCode "something else entirely" `shouldBe` Nothing
      renderProtocolError "something else entirely"
        `shouldBe` "Protocol error: something else entirely"

    it "RESIDUAL: the code is recovered by scraping a `show`-ed record" $ do
      -- Not a defect to fix here — a fragility to name. If the server ever
      -- stops embedding `errCode = N` in prose, every semantic prefix
      -- silently degrades to "Protocol error:". Pinned as the convention the
      -- inventory says it is.
      src <- readSource "src/Synapse/Transport.hs"
      src `shouldSatisfy` isInfixOf "\"errCode = \""

  ------------------------------------------------------------------------------
  -- Group 4: "genuine capability gaps"
  ------------------------------------------------------------------------------

  describe "item16 — dynamic-instance descent: NOT MET (PLX-122, RFC-blocked on §10.1)" $ do
    it "CORRECTION 2026-07-27: the wire DOES now carry an Indexed edge, with all five §5.1 facts" $ do
      -- PLX-119 measured "the wire carries no Indexed edge" against the LEGACY
      -- lift. The served Connectome now carries one. This closes the SUPPLY
      -- half of PLX-122's block; §10.1 is the remaining cause.
      doc <- servedDocument
      let ixs = [ix | C.EIndexed ix <- C.aChildren (C.cRoot doc)]
      length ixs `shouldBe` 1
      case ixs of
        [ix] -> do
          C.unNamespace (C.ixNamespace ix) `shouldBe` "tenants"
          C.unDottedId (C.ixListMethod ix) `shouldBe` "tenants.list"
          C.unFieldName (C.ixIdField ix) `shouldBe` "tenant_id"
          C.unPathTemplate (C.ixPathTemplate ix) `shouldBe` "tenants/{id}"
          -- §5.1(c): a template subtree is present at all, which is what makes
          -- an instance's shape knowable without a round trip.
          C.unVersion (C.aVersion (C.ixTemplate ix)) `shouldBe` "1.0.0"
        _ -> expectationFailure "expected exactly one Indexed edge"

    it "the LEGACY lift still produces no Indexed edge, and must not synthesise one (§5.2)" $ do
      [() | C.EIndexed _ <- C.aChildren (C.cRoot lifted)] `shouldBe` []

    it "NEGATIVE PIN: an instance id does NOT resolve — navigation stops at the template" $ do
      -- `hub session <id> <method>` is the capability. Composing the path
      -- template with an id is exactly what §10.1 says an implementation
      -- "MUST NOT invent silently". So it is not invented, and the item is
      -- reported unmet. When PLX-122 lands this fails.
      (result, calls, _) <- runGate (navigateC ["session", "s-123", "chat"])
      calls `shouldBe` [Nothing]
      case result of
        Right (Left (CNotFound seg _)) -> seg `shouldBe` "s-123"
        other -> expectationFailure ("expected a clean miss, got: " <> show other)

    it "and the miss is CLEAN — a specific error, not a panic and not an empty descent" $ do
      (result, _, _) <- runGate (navigateC ["session", "s-123"])
      case result of
        Right (Left e) ->
          renderCNavError e `shouldSatisfy` T.isInfixOf "s-123"
        other -> expectationFailure ("expected an error, got: " <> show other)

  describe "item17 — list/search response shape: NOT MET, still guessed" $ do
    it "NEGATIVE PIN: the id field is on the wire and synapse does not consume it" $ do
      -- §5.1's id_field is now served (see item16). The consumer that guesses
      -- is Synapse.DynamicChildren, and it does not import the document model
      -- at all — so the declared field cannot reach it. Unblocking is PLX-122's
      -- descent work; until then the guess stands.
      src <- readSource "src/Synapse/DynamicChildren.hs"
      src `shouldNotSatisfy` isInfixOf "Plexus.Connectome"

    it "the three tolerated response shapes are still all tolerated" $ do
      let frame v = PT.StreamData "h" (PT.Provenance []) "names" v
          run v = listChildrenWith silentWarn (\_ _ _ -> pure (Right [frame v])) ["solar"] "list"
      bare <- run (Aeson.String "earth")
      arr <- run (Aeson.toJSON ([Aeson.String "earth"] :: [Value]))
      obj <- run (object ["name" .= ("earth" :: Text)])
      bare `shouldBe` ["earth"]
      arr `shouldBe` ["earth"]
      obj `shouldBe` ["earth"]

    it "and the param name is still GUESSED, with a literal \"name\" fallback" $ do
      let ambiguous =
            (bareMethod "list")
              { W.methodRole = W.MethodRoleDynamicChild (Just "list") Nothing
              , W.methodParams =
                  Just
                    ( object
                        [ "properties"
                            .= object ["a" .= object [], "b" .= object []]
                        ]
                    )
              }
      gateParamName ambiguous `shouldBe` "name"

  describe "item18 — version negotiation: the document carries a version and an ir_hash" $ do
    it "MET on the served wire: ir_version, hash_algorithm and ir_hash are all declared" $ do
      doc <- servedDocument
      let rf = C.cRootFacts doc
      C.rfIrVersion rf `shouldBe` Just (C.IrVersion 1)
      C.rfHashAlgorithm rf `shouldBe` Just "CONNECTOME-HASH/1"
      C.unHash (C.rfDocumentHash rf) `shouldNotBe` ""

    it "AMENDED SCOPE: the legacy LIFT still declares neither — that pair is legacy-only" $ do
      -- PLX-119 pinned §3.3 and §4.7 as Reported gaps of the LIFT. They are
      -- not gaps of the served document. Both statements are true at once and
      -- the distinction is the point: item18 is met by the emitter, not by the
      -- lift, and the lift is scaffolding that is deleted when the default
      -- navigation path cuts over.
      C.rfIrVersion (C.cRootFacts lifted) `shouldBe` Nothing
      C.rfHashAlgorithm (C.cRootFacts lifted) `shouldBe` Nothing
      map gapClause [g | g <- legacyWireGaps, gapVisibility g == Reported]
        `shouldBe` ["3.3", "4.7"]

    it "no handshake is needed: the version is a fact ON the document, not a negotiation" $ do
      src <- readSource "src/Synapse/Connectome/Nav.hs"
      src `shouldNotSatisfy` isInfixOf "negotiate"

  ------------------------------------------------------------------------------
  -- The M3 exit gate, asserted in one place
  ------------------------------------------------------------------------------

  describe "M3 exit gate — asserted, not described" $ do
    it "ONE document fetch plus K lazy Dynamic-edge fetches, with K explained" $ do
      (result, calls, evs) <- runGate walkTree
      -- Nothing = the one document fetch. K = 1: two Dynamic edges advertise
      -- the same content hash, so the walk pays for content and not for names.
      calls `shouldBe` [Nothing, Just "lazy"]
      countFetches evs `shouldBe` 2
      countCacheHits evs `shouldBe` 1
      case result of
        Right (Right entries) -> entries `shouldNotBe` []
        other -> expectationFailure ("walk failed: " <> show other)

    it "on repeat, every Dynamic descent is a hash-keyed cache HIT and zero fetches" $ do
      (_, calls, evs) <- runGate $ do
        _ <- walkTree
        navigateC ["lazy_alias", "ping"]
      calls `shouldBe` [Nothing, Just "lazy"]
      countCacheHits evs `shouldBe` 2

    it "instance descent: STATED as unmet, with its blocker named" $ do
      -- The gate says `hub session <id> <method>` resolves. It does not.
      -- PLX-122 owns it and is blocked on an RFC 002 §10.1 amendment. This
      -- assertion exists so the gate cannot be read as met.
      (result, _, _) <- runGate (navigateC ["session", "any-id", "chat"])
      case result of
        Right (Left _) -> pure ()
        other -> expectationFailure ("instance descent unexpectedly resolved: " <> show other)

    it "synapse carries no schema model of its own beyond the two named survivors" $ do
      -- PLX-119 c3/c4: model 1 deleted, model 2 (Plexus.Schema.Recursive)
      -- retained under a named ticket because nothing else decodes the live
      -- wire, and Synapse.IR.Types.TypeRef survives by name collision only.
      files <- haskellFilesUnder "src"
      cacheModel <- fmap concat . forM files $ \f -> do
        s <- readSource f
        pure [f | "Plexus.Schema.Cache" `isInfixOf` s]
      cacheModel `shouldBe` []

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

silentWarn :: Text -> IO ()
silentWarn _ = pure ()

lookupMethod :: Text -> [C.Method] -> Maybe C.Method
lookupMethod n ms = case [m | m <- ms, C.unMethodName (C.mName m) == n] of
  (m : _) -> Just m
  [] -> Nothing

-- | The document a live substrate actually served, trimmed to four edges.
--
-- Captured 2026-07-27 from plexus-substrate at @5c630784@ (PLX-150 tip) via
-- @substrate.connectome {}@. It is TRIMMED — methods and grandchildren were
-- dropped to keep the fixture small — so its node hashes no longer fold, and
-- @verifyHashes@ is deliberately never run on it here. Every field this suite
-- asserts on (root facts, edge kinds, the Indexed edge's five §5.1 facts) is
-- verbatim from the wire.
servedDocument :: IO C.Connectome
servedDocument = servedDocumentPlx124

-- | PLX-124's capture (2026-07-27), kept verbatim.
--
-- PLX-157 did NOT replace this file. The two captures come from differently
-- composed substrates — this one has four children, today's has fifteen — for
-- reasons that predate this ticket, so overwriting it would silently change
-- what six other item tests assert on. It is retained as the dated record of
-- the wire BEFORE the root facts were declared, which is what makes the
-- inverted pins auditable.
servedDocumentPlx124 :: IO C.Connectome
servedDocumentPlx124 = goldenDocument "test/golden/plx124_served_root.json"

-- | PLX-157's capture: the same @substrate.connectome@ call against a
-- substrate built from @plx-157-unowned@, which declares §3.3's
-- @backend_name@ and §7.6's @respond_method@ on the hub root.
--
-- Captured 2026-07-27 from 127.0.0.1:4499 with
-- @docs\/evidence\/plx-157-info-counter.py@'s sibling capture snippet; the
-- diff against PLX-124's is exactly three keys — @backend_name@ (added),
-- @respond_method@ (added), @ir_hash@ (moved). No activation or method hash
-- moved, which is what §4.6 promises: root facts enter the DOCUMENT preimage
-- only.
servedDocument157 :: IO C.Connectome
servedDocument157 = goldenDocument "test/golden/plx157_served_root.json"

goldenDocument :: FilePath -> IO C.Connectome
goldenDocument path = do
  mv <- decodeFileStrict' path
  case mv of
    Nothing -> fail (path <> " is not valid JSON")
    Just v -> case CC.decodeConnectome v of
      Left e -> fail ("captured document does not decode as a Connectome: " <> show e)
      Right c -> pure c

-- | A port nothing listens on, so an identity probe is guaranteed to fail.
-- The memo tests care about how many failures are PAID, not about the answer.
unusedPort :: Int
unusedPort = 4531

probeConfig :: Text -> SubstrateConfig
probeConfig backend =
  SubstrateConfig
    { substrateHost = "127.0.0.1"
    , substratePort = unusedPort
    , substratePath = "/"
    , substrateBackend = backend
    , substrateHeaders = []
    }
