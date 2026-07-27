-- | PLX-119 — the legacy wire lifted into the Connectome.
--
-- These tests do two jobs. The first is ordinary: the lift is structural and
-- faithful. The second is the interesting one — they __pin the measured
-- distance between the legacy @PluginSchema@ wire and CONNECTOME RFC 002__, as
-- judged by the RFC's own conformance checker. When M2 (PLX-94) makes a server
-- emit a real Connectome and a gap closes, the pinning test fails and says so,
-- instead of the gap quietly persisting.
module Main (main) where

import Data.Aeson (Value, object, (.=))
import Data.List (nub, sort)
import qualified Data.Text as T
import Test.Hspec

import Plexus.Connectome.Check
  ( CheckOptions (..)
  , Clause (..)
  , Severity (..)
  , Verdict (..)
  , Violation (..)
  , checkConnectome
  , defaultCheckOptions
  , isConformant
  )
import Plexus.Connectome.Codec (decodeConnectome, encodeConnectome)
import qualified Plexus.Connectome.Types as C
import qualified Plexus.Schema.Recursive as W

import Synapse.Connectome

--------------------------------------------------------------------------------
-- Fixtures — legacy wire values, shaped like what a live backend serves
--------------------------------------------------------------------------------

returnsSchema :: Value
returnsSchema = object ["type" .= ("object" :: String), "title" .= ("ChatEvent" :: String)]

-- | A method that streams: one @returns@ schema plus @streaming = True@.
streamingMethod :: W.MethodSchema
streamingMethod =
  (bareMethod "chat")
    { W.methodDescription = "interactive chat"
    , W.methodReturns = Just returnsSchema
    , W.methodStreaming = True
    }

-- | A method that does not stream.
unaryMethod :: W.MethodSchema
unaryMethod =
  (bareMethod "health")
    { W.methodReturns = Just returnsSchema
    , W.methodStreaming = False
    }

-- | A method declaring no return type at all.
voidMethod :: W.MethodSchema
voidMethod = bareMethod "ping"

bareMethod :: T.Text -> W.MethodSchema
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

rootSchema :: W.PluginSchema
rootSchema =
  W.PluginSchema
    { W.psNamespace = "substrate"
    , W.psVersion = "0.6.0"
    , W.psDescription = "the substrate root"
    , W.psLongDescription = Just "Long form docs."
    , W.psHash = "root-hash"
    , W.psMethods = [streamingMethod, unaryMethod, voidMethod]
    , W.psChildren =
        Just
          [ W.ChildSummary "cone" "llm agents" "cone-hash"
          , W.ChildSummary "arbor" "" "arbor-hash"
          ]
    , W.psDeprecation = Nothing
    , W.psRequest = Nothing
    }

lifted :: C.Connectome
lifted = liftConnectome "substrate" rootSchema

verdict :: Verdict
verdict = checkConnectome defaultCheckOptions {coReportShoulds = False} lifted

mustClauses :: [T.Text]
mustClauses =
  sort . nub $
    [unClause (vClause v) | v <- verdictViolations verdict, vSeverity v == MustViolation]

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "structural lift (PLX-119)" $ do
    it "carries the root's namespace, version and hash" $ do
      let r = C.cRoot lifted
      C.aNamespace r `shouldBe` C.Namespace "substrate"
      C.aVersion r `shouldBe` C.Version "0.6.0"
      C.aHash r `shouldBe` C.Hash "root-hash"

    it "names the backend the caller connected to (§3.3)" $
      C.rfBackendName (C.cRootFacts lifted) `shouldBe` Just (C.BackendName "substrate")

    it "composes each method's dotted id as namespace.name (§10.1)" $
      map C.mDottedId (C.aMethods (C.cRoot lifted))
        `shouldBe` [ C.DottedId "substrate.chat"
                   , C.DottedId "substrate.health"
                   , C.DottedId "substrate.ping"
                   ]

    it "spells the legacy empty-string description as absence (§3.5)" $ do
      let arbor = [e | e@(C.EDynamic d) <- C.aChildren (C.cRoot lifted), C.dyNamespace d == "arbor"]
      case arbor of
        [C.EDynamic d] -> C.dyDescription d `shouldBe` Nothing
        _ -> expectationFailure "expected exactly one arbor edge"

  describe "§5 — the lift never fabricates structure" $ do
    it "turns every child summary into a Dynamic edge carrying namespace + advertised hash" $
      C.aChildren (C.cRoot lifted)
        `shouldBe` [ C.EDynamic (C.DynamicEdge "cone" "cone-hash" (Just "llm agents"))
                   , C.EDynamic (C.DynamicEdge "arbor" "arbor-hash" Nothing)
                   ]

    it "produces no Static edge — nothing is embedded on this wire (§5.1)" $
      [() | C.EStatic _ <- C.aChildren (C.cRoot lifted)] `shouldBe` []

    it "produces no Indexed edge — the wire carries none of §5.1's five facts (§5.2)" $
      [() | C.EIndexed _ <- C.aChildren (C.cRoot lifted)] `shouldBe` []

    it "invents no subtree for a Dynamic child (§5.2)" $
      concatMap (maybe [] pure . C.edgeChildActivation) (C.aChildren (C.cRoot lifted))
        `shouldBe` []

  describe "§6.3 / §6.4 — updates and terminal stay distinguishable" $ do
    let methodNamed n = head [m | m <- C.aMethods (C.cRoot lifted), C.mName m == n]

    it "a non-streaming method declares a terminal and no updates (§6.3)" $ do
      let m = methodNamed "health"
      C.mUpdates m `shouldBe` C.NotDeclared
      C.mTerminal m `shouldBe` C.Declared (C.TypeRef (C.TypeName "") returnsSchema)

    it "a method with no returns declares neither (§6.3)" $ do
      let m = methodNamed "ping"
      C.mUpdates m `shouldBe` C.NotDeclared
      C.mTerminal m `shouldBe` C.NotDeclared

    it "a streaming method's terminal is Unresolved, never absent and never unit (§6.4)" $ do
      let m = methodNamed "chat"
      C.mUpdates m `shouldBe` C.Declared (C.TypeRef (C.TypeName "") returnsSchema)
      case C.mTerminal m of
        C.Unresolved why -> why `shouldSatisfy` T.isInfixOf "streaming"
        other -> expectationFailure ("expected Unresolved, got " <> show other)

  describe "credentials are PLX-120's, not lifted here" $ do
    it "leaves mAuth absent rather than encoding the legacy posture axis" $
      all ((== Nothing) . C.mAuth) (C.aMethods (C.cRoot lifted)) `shouldBe` True

    it "puts nothing in extensions — §10.4 forbids permissions riding in ext(...)" $
      all ((== mempty) . C.mExtensions) (C.aMethods (C.cRoot lifted)) `shouldBe` True

  describe "the lifted value is a real Connectome, not a look-alike" $
    it "round-trips through the RFC codec unchanged (§3.4)" $
      decodeConnectome (encodeConnectome lifted) `shouldBe` Right lifted

  describe "PINNED: the legacy wire's measured distance from RFC 002" $ do
    it "is non-conformant — and that is the finding, not a defect in the lift" $
      isConformant verdict `shouldBe` False

    it "the checker's MUST violations are EXACTLY the Reported gaps" $
      mustClauses
        `shouldBe` sort (nub [gapClause g | g <- legacyWireGaps, gapVisibility g == Reported])

    it "the Silent gaps really are silent — the checker never raises them" $
      filter (`elem` mustClauses) [gapClause g | g <- legacyWireGaps, gapVisibility g == Silent]
        `shouldBe` []

    it "the check is not vacuous — it does report violations" $
      mustClauses `shouldSatisfy` (not . null)

    it "every violation cites a clause and a path" $
      all (\v -> not (T.null (unClause (vClause v))) && not (T.null (vPath v)))
        (verdictViolations verdict)
        `shouldBe` True
