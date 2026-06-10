{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for Synapse.Self.Oidc (UT-3).
--
-- Pure surface only — grant-request encoding, discovery / token-response
-- parsing, issuer-precedence resolution, and the login\/logout store
-- transforms (including the defaults.json round-trip of the @oidc@
-- section). The live IdP path is exercised end-to-end against
-- plexus-idp outside the unit suite.
module SelfOidcSpec (spec) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Hspec

import Synapse.Self
  ( CredentialRef(..)
  , OidcDefaults(..)
  , StoredDefaults(..)
  , decodeDefaults
  , emptyOidcDefaults
  , emptyStoredDefaults
  , encodeDefaults
  )
import Synapse.Self.Oidc

spec :: Spec
spec = do
  describe "percentEncode" $ do
    it "passes unreserved characters through" $
      percentEncode "Az09-._~" `shouldBe` "Az09-._~"

    it "encodes reserved and special characters" $ do
      percentEncode "a b&c=d" `shouldBe` "a%20b%26c%3Dd"
      percentEncode "p@ss/wörd+!" `shouldBe` "p%40ss%2Fw%C3%B6rd%2B%21"

    it "never emits '+' for space" $
      percentEncode " " `shouldBe` "%20"

  describe "urlEncodeForm" $ do
    it "joins pairs with & in order" $
      urlEncodeForm [("a", "1"), ("b", "2")] `shouldBe` "a=1&b=2"

    it "encodes both keys and values" $
      urlEncodeForm [("key name", "v&1")] `shouldBe` "key%20name=v%261"

  describe "passwordGrantForm" $ do
    it "carries grant_type, username, password, audience" $
      urlEncodeForm (passwordGrantForm "alice" "s3cret&" "plexus:idptest")
        `shouldBe`
        "grant_type=password&username=alice&password=s3cret%26&audience=plexus%3Aidptest"

  describe "refreshGrantForm" $ do
    it "carries grant_type and refresh_token" $
      urlEncodeForm (refreshGrantForm "deadbeef")
        `shouldBe` "grant_type=refresh_token&refresh_token=deadbeef"

  describe "discoveryUrl" $ do
    it "appends the well-known path" $
      discoveryUrl "http://localhost:4461"
        `shouldBe` "http://localhost:4461/.well-known/openid-configuration"

    it "normalizes trailing slashes (Auth0-style issuers)" $
      discoveryUrl "https://tenant.auth0.com/"
        `shouldBe` "https://tenant.auth0.com/.well-known/openid-configuration"

  describe "parseDiscoveryDoc" $ do
    it "extracts token_endpoint" $
      parseDiscoveryDoc
        "{\"issuer\":\"http://i\",\"token_endpoint\":\"http://i/oauth/token\",\"jwks_uri\":\"http://i/jwks.json\"}"
        `shouldBe` Right (DiscoveryDoc (Just "http://i") "http://i/oauth/token")

    it "rejects a document without token_endpoint" $
      parseDiscoveryDoc "{\"issuer\":\"http://i\"}" `shouldSatisfy` isLeft

  describe "parseTokenResponse" $ do
    it "parses a successful plexus-idp-shaped response" $
      parseTokenResponse 200
        "{\"access_token\":\"jwt\",\"token_type\":\"Bearer\",\"expires_in\":3600,\"refresh_token\":\"r1\"}"
        `shouldBe` Right (TokenResponse "jwt" (Just "Bearer") (Just 3600) (Just "r1"))

    it "tolerates a missing refresh_token (RFC 6749 optional)" $
      parseTokenResponse 200 "{\"access_token\":\"jwt\"}"
        `shouldBe` Right (TokenResponse "jwt" Nothing Nothing Nothing)

    it "surfaces an RFC 6749 error body readably" $
      parseTokenResponse 400
        "{\"error\":\"invalid_grant\",\"error_description\":\"invalid credentials or refresh token\"}"
        `shouldBe` Left "invalid_grant: invalid credentials or refresh token"

    it "falls back to status + body preview on junk" $
      case parseTokenResponse 502 "<html>bad gateway</html>" of
        Right _  -> expectationFailure "expected Left"
        Left err -> do
          err `shouldSatisfy` ("HTTP 502" `isInfixOfT`)
          err `shouldSatisfy` ("bad gateway" `isInfixOfT`)

  describe "resolveIssuer (precedence: flag > stored > error)" $ do
    let storedSd = emptyStoredDefaults
          { sdOidc = Just emptyOidcDefaults { odIssuer = Just "http://stored:4461" } }

    it "prefers the --issuer flag over the stored issuer" $
      resolveIssuer "b" (Just "http://flag:1") storedSd
        `shouldBe` Right "http://flag:1"

    it "falls back to the persisted issuer" $
      resolveIssuer "b" Nothing storedSd `shouldBe` Right "http://stored:4461"

    it "errors helpfully, naming --issuer, when neither exists" $
      case resolveIssuer "b" Nothing emptyStoredDefaults of
        Right _  -> expectationFailure "expected Left"
        Left err -> err `shouldSatisfy` ("--issuer" `isInfixOfT`)

  describe "defaultAudience" $ do
    it "is plexus:<backend>" $
      defaultAudience "idptest" `shouldBe` "plexus:idptest"

  describe "applyLogin / clearLogin" $ do
    let tr = TokenResponse "the-jwt" (Just "Bearer") (Just 3600) (Just "the-refresh")
        loggedIn = applyLogin "http://i:4461" "plexus:b" tr emptyStoredDefaults

    it "stores cookies.access_token as literal:" $
      Map.lookup "access_token" (sdCookies loggedIn)
        `shouldBe` Just (CredentialRef "literal:the-jwt")

    it "stores oidc issuer, audience, and refresh_token ref" $
      sdOidc loggedIn `shouldBe` Just OidcDefaults
        { odIssuer       = Just "http://i:4461"
        , odAudience     = Just "plexus:b"
        , odRefreshToken = Just (CredentialRef "literal:the-refresh")
        }

    it "omits the refresh ref when the grant returned none" $ do
      let tr' = tr { trRefreshToken = Nothing }
          sd' = applyLogin "http://i" "aud" tr' emptyStoredDefaults
      (sdOidc sd' >>= odRefreshToken) `shouldBe` Nothing

    it "preserves unrelated cookies and headers" $ do
      let base = emptyStoredDefaults
            { sdCookies = Map.singleton "session" (CredentialRef "literal:s")
            , sdHeaders = Map.singleton "X-Trace" (CredentialRef "literal:t")
            }
          sd' = applyLogin "http://i" "aud" tr base
      Map.lookup "session" (sdCookies sd') `shouldBe` Just (CredentialRef "literal:s")
      Map.lookup "X-Trace" (sdHeaders sd') `shouldBe` Just (CredentialRef "literal:t")

    it "clearLogin drops tokens but keeps issuer/audience and other defaults" $ do
      let base = loggedIn
            { sdHeaders = Map.singleton "X-Keep" (CredentialRef "literal:k") }
          out = clearLogin base
      Map.lookup "access_token" (sdCookies out) `shouldBe` Nothing
      (sdOidc out >>= odRefreshToken) `shouldBe` Nothing
      (sdOidc out >>= odIssuer) `shouldBe` Just "http://i:4461"
      (sdOidc out >>= odAudience) `shouldBe` Just "plexus:b"
      Map.lookup "X-Keep" (sdHeaders out) `shouldBe` Just (CredentialRef "literal:k")

  describe "defaults.json round-trip with oidc section" $ do
    it "encode/decode round-trips login state" $ do
      let tr = TokenResponse "jwt" Nothing Nothing (Just "r")
          sd = applyLogin "http://i:4461" "plexus:b" tr emptyStoredDefaults
      decodeDefaults (encodeDefaults sd) `shouldBe` Right sd

    it "pre-UT-3 files (no oidc key) decode with sdOidc = Nothing" $ do
      let legacy = "{\"version\":1,\"defaults\":{\"cookies\":{\"access_token\":\"literal:j\"}},\"scopes\":{}}"
      case decodeDefaults legacy of
        Left err -> expectationFailure (show err)
        Right sd -> sdOidc sd `shouldBe` Nothing

    it "files without login state encode byte-identically to pre-UT-3 (no oidc key)" $ do
      let bytes = TE.decodeUtf8 (encodeDefaults emptyStoredDefaults)
      bytes `shouldSatisfy` not . ("oidc" `isInfixOfT`)

    it "the encoded oidc refresh token is a literal: ref (sensitive-chmod trigger)" $ do
      let tr = TokenResponse "jwt" Nothing Nothing (Just "r")
          sd = applyLogin "http://i" "aud" tr emptyStoredDefaults
      TE.decodeUtf8 (encodeDefaults sd) `shouldSatisfy` ("literal:r" `isInfixOfT`)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isInfixOfT :: Text -> Text -> Bool
isInfixOfT = T.isInfixOf
