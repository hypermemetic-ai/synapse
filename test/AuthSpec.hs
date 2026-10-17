{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

-- | AUTH-8: synapse shows each caller its own server.
--
-- * plexus-protocol parses AUTH-2..5's new shapes — and their absence:
--   every field is optional, today's hubs answer none of it;
-- * the renders: the anonymous page, the caller's header and the grant
--   columns, the refusal block — each pinned to the ticket's transcript;
-- * the loopback's request parsing and state check, and the listener
--   itself on 127.0.0.1;
-- * the flows end to end against 'MockHub' (a hub in miniature on a port
--   of its own): a person's browser sign-in, an agent's auth.login and its
--   fallback to session.login on a hub without auth, whoami.
--
-- @SYNAPSE_MOCK_HUB=today|anonymous|caller@ runs the mock hub instead of
-- the tests (it prints its port), for transcripts by hand.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.Aeson (Value, fromJSON, Result(..), toJSON, decode)
import qualified Data.Aeson as A
import Data.Aeson.QQ.Simple (aesonQQ)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Katip
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import System.Environment (lookupEnv, setEnv)
import Test.Hspec

import Plexus.Auth
import Plexus.Schema.Recursive (ChildSummary(..), MethodSchema(..), PluginSchema(..), MethodRole(..), hasAuthColumns)
import Plexus.Types (PlexusStreamItem(..), itemRefusal)
import Synapse.Endpoint (plainAt)
import Synapse.Algebra.Render (renderSchema, renderSchemaFor, defaultStyle)
import Synapse.Auth
import Synapse.Auth.Loopback
import Synapse.Identity.Proof (generateSeed)
import qualified Synapse.Identity.Session as Session
import qualified Synapse.Log as Log

import MockHub

main :: IO ()
main = do
  serve <- lookupEnv "SYNAPSE_MOCK_HUB"
  case serve of
    Just "today" -> serveMockHub todaysHub
    Just "anonymous" -> serveMockHub (Scenario AnonymousView True False mockState False)
    Just "caller" -> serveMockHub (Scenario CallerView True False mockState False)
    _ -> do
      setEnv "SYNAPSE_NO_BROWSER" "1"
      hspec spec

-- ============================================================================
-- Fixtures
-- ============================================================================

parse :: A.FromJSON a => Value -> a
parse v = case fromJSON v of
  Success x -> x
  Error e -> error e

-- AUTH-3/4: a caller's tickets schema
callerTickets :: PluginSchema
callerTickets = parse [aesonQQ|{
  "namespace": "tickets", "version": "0.5.0", "description": "Plans as data", "hash": "v-9a77",
  "view": "caller",
  "roles": {"viewer": ["tickets.get", "tickets.list"], "editor": ["tickets.get", "tickets.list", "tickets.set_status"]},
  "methods": [
    {"name": "get", "description": "One ticket by id", "hash": "g", "grant": "tickets.get", "target": "id", "held_on": ["MENGER"]},
    {"name": "list", "description": "Tickets, filtered and paged", "hash": "l", "grant": "tickets.list", "held_on": ["MENGER"]},
    {"name": "set_status", "description": "Change a ticket's status", "hash": "s",
     "long_description": "Moves it through Pending → Ready → Coding → In Review → Complete; a doc has no lifecycle and is refused.",
     "grant": "tickets.set_status", "target": "id", "held_on": ["MENGER-12"]}
  ]}|]

-- today's: no grant, no view
todaysTickets :: PluginSchema
todaysTickets = parse [aesonQQ|{
  "namespace": "tickets", "version": "0.5.0", "description": "Plans as data", "hash": "t0",
  "methods": [
    {"name": "get", "description": "One ticket by id", "hash": "g"},
    {"name": "set_status", "description": "Change a ticket's status", "hash": "s"}
  ]}|]

methodNamed :: Text -> PluginSchema -> MethodSchema
methodNamed n ps = head [m | m <- psMethods ps, methodName m == n]

worker :: Whoami
worker = Whoami
  { whoPrincipal = "key:ac79e0f3b1c24d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6"
  , whoName = Just "worker-1", whoAgent = Just "worker-1"
  , whoMemberships = [("editor", "MENGER")], whoCeiling = ["editor@MENGER-12"] }

-- AUTH-2's refusal, as the subscription notification's result carries it
refusalItem :: Value
refusalItem = [aesonQQ|{"type":"error","error_kind":"forbidden",
  "grant":"tickets.set_status","scope":"IDENTITY",
  "roles":["editor","admin"],"held_on":["MENGER"],
  "message":"needs tickets.set_status on IDENTITY",
  "metadata":{"provenance":["menger"],"plexus_hash":"h","timestamp":0}}|]

logger :: IO Log.Logger
logger = Log.makeLogger Katip.ErrorS

-- ============================================================================
-- The spec
-- ============================================================================

spec :: Spec
spec = do
  describe "plexus-protocol: the method schema (AUTH-3/4)" $ do
    it "reads grant, target, held_on and long_description" $ do
      let m = methodNamed "set_status" callerTickets
      methodGrant m `shouldBe` Just "tickets.set_status"
      methodTarget m `shouldBe` Just "id"
      methodHeldOn m `shouldBe` Just ["MENGER-12"]
      methodLongDescription m `shouldSatisfy` maybe False ("Moves it through" `T.isPrefixOf`)
      methodTarget (methodNamed "list" callerTickets) `shouldBe` Nothing
    it "reads their absence as nothing (today's schemas)" $ do
      let m = methodNamed "set_status" todaysTickets
      (methodGrant m, methodTarget m, methodHeldOn m, methodLongDescription m, methodPublic m)
        `shouldBe` (Nothing, Nothing, Nothing, Nothing, False)
      (psRoles todaysTickets, psView todaysTickets) `shouldBe` (Nothing, Nothing)
      hasAuthColumns todaysTickets `shouldBe` False
    it "reads roles and the view on a caller's plugin schema" $ do
      psView callerTickets `shouldBe` Just Caller
      fmap (Map.lookup "viewer") (psRoles callerTickets) `shouldBe` Just (Just ["tickets.get", "tickets.list"])
      hasAuthColumns callerTickets `shouldBe` True
    it "reads an anonymous view's bare schema (a method named and public, children without hashes)" $ do
      let root = parse [aesonQQ|{"namespace":"menger","hash":"v-anon-3c1f",
            "children":[{"namespace":"auth","methods":[{"name":"providers","public":true},{"name":"login","public":true}]}]}|] :: PluginSchema
      map csNamespace (maybe [] id (psChildren root)) `shouldBe` ["auth"]
      psVersion root `shouldBe` ""
      let m = parse [aesonQQ|{"name":"login","public":true}|] :: MethodSchema
      (methodName m, methodPublic m, methodDescription m, methodRole m) `shouldBe` ("login", True, "", MethodRoleRpc)
    it "writes the new fields back only when present" $ do
      let v = toJSON (methodNamed "set_status" todaysTickets)
      (decode (A.encode v) :: Maybe MethodSchema) `shouldBe` Just (methodNamed "set_status" todaysTickets)
      show v `shouldNotContain` "grant"
      (fromJSON (toJSON callerTickets) :: Result PluginSchema) `shouldBe` Success callerTickets

  describe "plexus-protocol: _info's view and auth block (AUTH-4)" $ do
    it "today's _info: no view, no auth" $ do
      let i = parseBackendInfo [aesonQQ|{"backend":"menger","auth_capabilities":{"mechanisms":[{"kind":"anonymous"}]}}|]
      i `shouldBe` Just (BackendInfo "menger" Nothing Nothing Nothing)
    it "an anonymous _info: the door's methods and the resource metadata" $ do
      let i = parseBackendInfo [aesonQQ|{"backend":"menger","view":"anonymous",
                "auth":{"methods":["auth.providers","auth.login","auth.signup"],
                        "resource_metadata":"http://127.0.0.1:4456/.well-known/oauth-protected-resource"}}|]
      fmap infoView i `shouldBe` Just (Just Anonymous)
      fmap (fmap authMethods . infoAuth) i `shouldBe` Just (Just ["auth.providers", "auth.login", "auth.signup"])
      fmap (fmap authResourceMetadata . infoAuth) i `shouldBe` Just (Just (Just "http://127.0.0.1:4456/.well-known/oauth-protected-resource"))
    it "a caller's _info, its view's hash as view_hash, hash, or inside a view object" $ do
      fmap infoViewHash (parseBackendInfo [aesonQQ|{"backend":"m","view":"caller","view_hash":"v-9a77"}|]) `shouldBe` Just (Just "v-9a77")
      fmap infoViewHash (parseBackendInfo [aesonQQ|{"backend":"m","view":"caller","hash":"v-1"}|]) `shouldBe` Just (Just "v-1")
      let o = parseBackendInfo [aesonQQ|{"backend":"m","view":{"kind":"caller","hash":"v-2"}}|]
      fmap (\i -> (infoView i, infoViewHash i)) o `shouldBe` Just (Just Caller, Just "v-2")
    it "a view it cannot read is no view, never a failed _info" $
      fmap infoView (parseBackendInfo [aesonQQ|{"backend":"m","view":"sideways","auth":7}|]) `shouldBe` Just Nothing

  describe "plexus-protocol: the refusal item (AUTH-2)" $ do
    it "an error item with error_kind forbidden carries its grant, scope, roles and held_on" $ do
      let item = parse refusalItem :: PlexusStreamItem
      itemRefusal item `shouldBe` Just Refusal
        { refusalGrant = Just "tickets.set_status", refusalScope = Just "IDENTITY"
        , refusalRoles = ["editor", "admin"], refusalHeldOn = ["MENGER"], refusalCeiling = []
        , refusalMessage = "needs tickets.set_status on IDENTITY" }
      case item of
        StreamError _ _ msg _ -> msg `shouldBe` "needs tickets.set_status on IDENTITY"
        _ -> expectationFailure "a refusal is still an error item"
    it "parses as written in the ticket, without metadata" $ do
      let bare = parse [aesonQQ|{"type":"error","error_kind":"forbidden","grant":"tickets.set_status",
                                 "scope":"IDENTITY","roles":["editor","admin"],"held_on":["MENGER"],
                                 "message":"needs tickets.set_status on IDENTITY"}|] :: PlexusStreamItem
      fmap refusalGrant (itemRefusal bare) `shouldBe` Just (Just "tickets.set_status")
    it "any other error has no refusal, and builds as before" $ do
      let plain = parse [aesonQQ|{"type":"error","message":"Activation not found: auth",
                    "metadata":{"provenance":["menger"],"plexus_hash":"h","timestamp":0},"recoverable":false}|]
      itemRefusal plain `shouldBe` Nothing
      itemRefusal (StreamError "h" (parse [aesonQQ|["m"]|]) "boom" False) `shouldBe` Nothing
    it "keeps a refusal's fields when written back" $ do
      let item = parse refusalItem :: PlexusStreamItem
      fmap itemRefusal (decode (A.encode item)) `shouldBe` Just (itemRefusal item)
    it "a data item whose content is a refusal is one too; one missing every field is still read" $ do
      let d = parse [aesonQQ|{"type":"data","content_type":"tickets.set_status",
                "metadata":{"provenance":["menger"],"plexus_hash":"h","timestamp":0},
                "content":{"type":"error","error_kind":"forbidden","grant":"tickets.create","scope":"MENGER"}}|]
      fmap refusalMessage (itemRefusal d) `shouldBe` Just "needs tickets.create on MENGER"
      parseRefusal [aesonQQ|{"error_kind":"forbidden"}|] `shouldBe` Just (Refusal Nothing Nothing [] [] [] "forbidden")
      parseRefusal [aesonQQ|{"error_kind":"invalid_params","message":"x"}|] `shouldBe` Nothing

  describe "the anonymous page" $ do
    it "renders the ticket's transcript" $ do
      let page = AnonPage "menger" (Just "1.0.0")
                   (anonymousMethods (Just (AuthInfo ["auth.providers", "auth.login", "auth.signup"] Nothing)) Nothing)
                   "synapse -P 4455"
      renderAnonymousPage page `shouldBe` T.unlines
        [ "menger v1.0.0"
        , "  Sign in to see what you can do here."
        , ""
        , "auth"
        , "  login      Sign in \x2014 a person through the browser, an agent with its key"
        , "  signup     Create an account, or ask for access"
        , "  providers  The ways in this server offers"
        , ""
        , "  synapse -P 4455 _login menger                          # a person"
        , "  synapse -P 4455 _login menger --key agent-7 --as w-1   # an agent"
        ]
    it "takes the door's methods from _info, else the auth schema's public ones, else its own three" $ do
      let authS = parse [aesonQQ|{"namespace":"auth","methods":[
                    {"name":"login","description":"Sign in here","public":true},
                    {"name":"whoami","description":"Who","public":false},
                    {"name":"magic","description":"A link by mail","public":true}]}|]
      anonymousMethods Nothing (Just authS) `shouldBe` [("login", "Sign in here"), ("magic", "A link by mail")]
      map fst (anonymousMethods (Just (AuthInfo ["auth.signup"] Nothing)) (Just authS)) `shouldBe` ["signup"]
      map fst (anonymousMethods Nothing Nothing) `shouldBe` ["login", "signup", "providers"]
    it "names no version when the view shows none" $
      head (T.lines (renderAnonymousPage (AnonPage "menger" Nothing [] "synapse"))) `shouldBe` "menger"

  describe "the caller's header" $ do
    it "says who, what role where, and within what" $
      renderCallerHeader worker `shouldBe` "you are worker-1 (key:ac79\x2026) \x00B7 editor on MENGER, within MENGER-12"
    it "parses auth.whoami's shape" $
      parseWhoami [aesonQQ|{"type":"whoami","principal":"key:ac79e0f3b1c24d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6",
                            "name":"worker-1","agent":"worker-1",
                            "memberships":[{"role":"editor","scope":"MENGER"}],"ceiling":["editor@MENGER-12"]}|]
        `shouldBe` Just worker
    it "reads memberships as role@SCOPE strings and a ceiling as objects" $ do
      let w = parseWhoami [aesonQQ|{"principal":"oidc:workos/user_01M4QXZ7ABCDEF","name":"Ada Lovelace",
                                    "memberships":["admin@*","viewer@IDENTITY","viewer@AUTH"],
                                    "ceiling":[{"role":"viewer","scope":"AUTH"}]}|]
      fmap renderCallerHeader w `shouldBe`
        Just "you are Ada Lovelace (oidc:workos/user_01M4\x2026) \x00B7 admin on *; viewer on IDENTITY, AUTH, within AUTH"
    it "falls back to session.whoami's identity (roles with no scope)" $
      fmap renderCallerHeader (parseWhoami [aesonQQ|{"type":"identity","principal":"idp:1234","name":"ada","roles":["admin"],"tenant":null}|])
        `shouldBe` Just "you are ada (idp:1234) \x00B7 admin"
    it "an anonymous whoami is no one" $ do
      parseWhoami [aesonQQ|{"type":"anonymous"}|] `shouldBe` Nothing
      parseWhoami [aesonQQ|{"type":"identity","principal":""}|] `shouldBe` Nothing
    it "shortens a long principal and leaves a short one" $ do
      abbreviatePrincipal "key:ac79e0f3b1c24d5e6f708192a3b4c5d6" `shouldBe` "key:ac79\x2026"
      abbreviatePrincipal "oidc:workos/user_01M4QXZ7ABCDEF" `shouldBe` "oidc:workos/user_01M4\x2026"
      abbreviatePrincipal "idp:1234" `shouldBe` "idp:1234"

  describe "help in a caller's view" $ do
    let out = renderSchemaFor defaultStyle (Just (renderCallerHeader worker)) callerTickets
        ls = T.lines out
        lineOf n = head [l | l <- ls, ("  " <> n <> " ") `T.isPrefixOf` l]
        colOf needle l = T.length (fst (T.breakOn needle l))
    it "puts who the caller is under the description" $
      take 3 ls `shouldBe`
        [ "tickets v0.5.0", "  Plans as data"
        , "  you are worker-1 (key:ac79\x2026) \x00B7 editor on MENGER, within MENGER-12" ]
    it "shows each method's grant and where it is held, in aligned columns" $ do
      lineOf "get" `shouldSatisfy` T.isInfixOf "tickets.get"
      T.stripEnd (lineOf "set_status") `shouldSatisfy` T.isSuffixOf "tickets.set_status  MENGER-12"
      let grantCols = [colOf "tickets." (lineOf n) | n <- ["get", "list", "set_status"]]
          heldCols = [colOf "MENGER" (T.drop (colOf "tickets." (lineOf n)) (lineOf n)) + colOf "tickets." (lineOf n) | n <- ["get", "list", "set_status"]]
      length (filter (== head grantCols) grantCols) `shouldBe` 3
      length (filter (== head heldCols) heldCols) `shouldBe` 3
    it "renders a schema with no grants exactly as before AUTH, header or not" $ do
      renderSchemaFor defaultStyle Nothing todaysTickets `shouldBe` renderSchema todaysTickets
      T.lines (renderSchemaFor defaultStyle Nothing callerTickets) `shouldNotContain`
        ["  you are worker-1 (key:ac79\x2026) \x00B7 editor on MENGER, within MENGER-12"]

  describe "the refusal block" $ do
    let r = Refusal (Just "tickets.set_status") (Just "IDENTITY") ["editor", "admin"] ["MENGER"] [] "needs tickets.set_status on IDENTITY"
    it "renders the ticket's transcript, the ceiling from the session" $
      renderRefusal ["editor@MENGER-12"] r `shouldBe` T.unlines
        [ "refused: needs tickets.set_status on IDENTITY"
        , "  roles with it   editor, admin"
        , "  you hold it on  MENGER (within MENGER-12)" ]
    it "prefers the refusal's own ceiling, and says so without one" $ do
      T.lines (renderRefusal ["editor@X"] r { refusalCeiling = ["editor@MENGER-7"] }) !! 2
        `shouldBe` "  you hold it on  MENGER (within MENGER-7)"
      T.lines (renderRefusal [] r) !! 2 `shouldBe` "  you hold it on  MENGER"
    it "says nowhere when the caller holds it nowhere, and leaves out roles it was not told" $
      renderRefusal [] (Refusal (Just "auth.role_set") (Just "*") [] [] [] "x") `shouldBe` T.unlines
        [ "refused: needs auth.role_set on *", "  you hold it on  nowhere" ]

  describe "the loopback: a request and its state" $ do
    let req s = parseRequestLine (BS8.pack s)
    it "reads the request line's path and query, percent-decoded" $
      req "GET /cb?code=c_19ab&state=s%2F7f2c HTTP/1.1\r\nHost: x\r\n\r\n"
        `shouldBe` Just ("GET", "/cb", [("code", "c_19ab"), ("state", "s/7f2c")])
    it "takes the code only with the state it handed out" $ do
      fmap (checkCallback "s_7f2c") (req "GET /cb?code=c_1&state=s_7f2c HTTP/1.1") `shouldBe` Just (CallbackCode "c_1")
      fmap (checkCallback "s_7f2c") (req "GET /cb?code=c_1&state=s_evil HTTP/1.1") `shouldSatisfy` refused
      fmap (checkCallback "s_7f2c") (req "GET /cb?code=c_1 HTTP/1.1") `shouldSatisfy` refused
      fmap (checkCallback "s_7f2c") (req "GET /cb?state=s_7f2c HTTP/1.1") `shouldSatisfy` refused
      fmap (checkCallback "s_7f2c") (req "GET /cb?error=access_denied&state=s_7f2c HTTP/1.1") `shouldSatisfy` refused
    it "ignores anything but /cb" $
      fmap (checkCallback "s") (req "GET /favicon.ico HTTP/1.1") `shouldBe` Just CallbackIgnored
    it "listens on 127.0.0.1 and answers the browser" $ withLoopback $ \lb -> do
      loopbackRedirect lb `shouldSatisfy` T.isPrefixOf "http://127.0.0.1:"
      void $ forkIO $ do
        threadDelay 100000
        _ <- httpGet (loopbackPort lb) "/favicon.ico"
        reply <- httpGet (loopbackPort lb) "/cb?code=c_1&state=s_ok"
        reply `shouldSatisfy` BS8.isPrefixOf "HTTP/1.1 200"
      awaitCallback lb "s_ok" 5 `shouldReturn` Right "c_1"
    it "refuses a browser back with another state, and ends the wait" $ withLoopback $ \lb -> do
      void $ forkIO $ threadDelay 100000 >> void (httpGet (loopbackPort lb) "/cb?code=c_1&state=s_other")
      r <- awaitCallback lb "s_ok" 5
      r `shouldSatisfy` either ("state" `T.isInfixOf`) (const False)
    it "gives up after its time" $ withLoopback $ \lb ->
      awaitCallback lb "s_ok" 1 `shouldReturn` Left "no answer from the browser within 1 second"

  describe "against a hub (MockHub)" $ do
    it "a person signs in through the browser: login_url, the loopback, the code, a session" $
      withMockHub (Scenario AnonymousView True False mockState False) $ \port -> do
        lg <- logger
        r <- Session.loginPerson lg (plainAt "127.0.0.1" port) "menger" 10 (const (pure ()))
        fmap (\g -> (sgToken g, sgName g, sgPrincipal g)) r
          `shouldBe` Right ("mgs_person", Just "Ada Lovelace", "oidc:workos/user_01M4QXZ7ABCDEF")
        fmap (fmap renderCallerHeader . sgWhoami) r
          `shouldBe` Right (Just "you are Ada Lovelace (oidc:workos/user_01M4\x2026) \x00B7 admin on *")
    it "a browser that comes back with another state signs no one in" $
      withMockHub (Scenario AnonymousView True False "s_forged" False) $ \port -> do
        lg <- logger
        r <- Session.loginPerson lg (plainAt "127.0.0.1" port) "menger" 10 (const (pure ()))
        either ("state" `T.isInfixOf`) (const False) r `shouldBe` True
    it "a hub with no auth activation says so for a person" $
      withMockHub todaysHub $ \port -> do
        lg <- logger
        r <- Session.loginPerson lg (plainAt "127.0.0.1" port) "menger" 10 (const (pure ()))
        either ("no auth activation" `T.isInfixOf`) (const False) r `shouldBe` True
    it "an agent asks auth.login for a session where the hub has auth" $
      withMockHub (Scenario CallerView True False mockState False) $ \port -> do
        lg <- logger
        seed <- generateSeed
        r <- Session.loginWithKey lg (plainAt "127.0.0.1" port) "menger" seed "worker-1" ["editor@MENGER-12"] (Just 3600)
        fmap (\l -> (Session.lrToken l, Session.lrScope l)) r `shouldBe` Right ("mgs_agent_auth", ["editor@MENGER-12"])
    it "an agent falls back to session.login on today's hub" $
      withMockHub todaysHub $ \port -> do
        lg <- logger
        seed <- generateSeed
        r <- Session.loginWithKey lg (plainAt "127.0.0.1" port) "menger" seed "worker-1" ["editor@MENGER-12"] Nothing
        fmap (\l -> (Session.lrToken l, Session.lrScope l)) r `shouldBe` Right ("mgs_agent_session", ["editor@MENGER-12"])
    it "whoami: auth.whoami where there is one, nothing where neither answers" $ do
      withMockHub (Scenario CallerView True False mockState False) $ \port -> do
        lg <- logger
        Session.whoamiWith lg (plainAt "127.0.0.1" port) "menger" "mgs_x" `shouldReturn` Just worker
      withMockHub todaysHub $ \port -> do
        lg <- logger
        w <- Session.whoamiWith lg (plainAt "127.0.0.1" port) "menger" "mgs_x"
        isJust w `shouldBe` False
  where
    refused = \case
      Just (CallbackRefused _) -> True
      _ -> False

-- | A GET to the loopback, as a browser sends it; the reply's first bytes.
httpGet :: Int -> String -> IO BS8.ByteString
httpGet port path = do
  sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
  N.connect sock (N.SockAddrInet (fromIntegral port) (N.tupleToHostAddress (127, 0, 0, 1)))
  NB.sendAll sock (BS8.pack ("GET " <> path <> " HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"))
  r <- NB.recv sock 4096
  N.close sock
  pure r
