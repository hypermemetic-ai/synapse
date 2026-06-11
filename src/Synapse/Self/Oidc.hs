{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | OIDC password / refresh grants for @_self \<backend\> login@ (UT-3).
--
-- Implements the client half of the UT-S01 decision doc's D1 sequence:
-- the CLI logs a user in against an OIDC issuer (plexus-idp, Auth0, or
-- any compliant IdP), and the resulting tokens land in the existing
-- 'Synapse.Self' credential-reference store so every subsequent
-- @synapse \<backend\> ...@ call attaches them automatically via the
-- WS-upgrade Cookie merge ('Synapse.Transport.mergeUpgradeHeaders').
--
-- __Issuer resolution__ (precedence, as shipped):
--
-- 1. @--issuer@ flag.
-- 2. The per-backend persisted issuer ('odIssuer' in @defaults.json@,
--    written on first successful login).
-- 3. Error naming the flag.
--
-- TODO(UT-S01): the target source of record is the backend's @_info@
-- @auth_capabilities.oidc@ advertisement (AUTHZ-CORE-3 shape). Backends
-- do not advertise it yet; when they do, it slots in between 1 and 2.
--
-- __Endpoint resolution__: the token endpoint is never guessed — it is
-- read from the issuer's discovery document
-- (@\<issuer\>\/.well-known\/openid-configuration@, OIDC Discovery),
-- exactly as a stock validator would.
--
-- __Transport__: HTTP is performed by shelling out to @curl@ via
-- 'System.Process' — the same pattern 'Synapse.Self.Resolve.Keychain'
-- uses for the @security@ CLI. Rationale: the package has no HTTP
-- client dependency (websockets-only transport), @curl@ gives us https
-- for free for real issuers, and the request body travels on stdin so
-- the password never appears in argv. Adding @http-client(-tls)@ was
-- considered and rejected as a heavier dependency footprint for two
-- requests; revisit if more HTTP surfaces accrete.
--
-- Everything network-shaped is separated from everything pure so the
-- grant encoding, response parsing, and store transforms are unit-
-- testable without a live IdP (see @test\/SelfOidcSpec.hs@).
module Synapse.Self.Oidc
  ( -- * Pure: form encoding (RFC 6749 request bodies)
    percentEncode
  , urlEncodeForm
  , passwordGrantForm
  , refreshGrantForm

    -- * Pure: response parsing
  , TokenResponse(..)
  , parseTokenResponse
  , DiscoveryDoc(..)
  , parseDiscoveryDoc
  , discoveryUrl

    -- * Pure: store transforms
  , defaultAudience
  , resolveIssuer
  , applyLogin
  , clearLogin
  , selectTenant
  , tokenTenant

    -- * Pure: freshness decision
  , expiryLeeway
  , shouldRefresh

    -- * IO: HTTP via curl
  , HttpResponse(..)
  , httpGet
  , httpPostForm
  , fetchTokenEndpoint
  , requestPasswordGrant
  , requestRefreshGrant

    -- * IO: transparent refresh (UT-3)
  , RefreshOutcome(..)
  , ensureFreshLogin
  ) where

import Data.Aeson (FromJSON(..), withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Bits (shiftR, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (intToDigit, isAlphaNum)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Word (Word8)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

import Synapse.Self.IO (writeDefaults)
import Synapse.Self.Jwt (decodeJwt, jsExp, jsOrgId)
import Synapse.Self.Resolve (resolveRef)
import Synapse.Self.Resolve.Default (defaultRegistry)
import Synapse.Self.Types
  ( CredentialRef(..)
  , OidcDefaults(..)
  , StoredDefaults(..)
  , emptyOidcDefaults
  )

-- ============================================================================
-- Form encoding
-- ============================================================================

-- | RFC 3986 percent-encoding over the UTF-8 bytes of the input.
-- Unreserved characters (@A-Z a-z 0-9 - . _ ~@) pass through; everything
-- else (including space) becomes @%XX@. Strict on purpose: OAuth servers
-- universally accept @%20@ for space, and never emitting @+@ avoids the
-- form-vs-query ambiguity entirely.
percentEncode :: Text -> Text
percentEncode = T.concat . map encodeByte . BS.unpack . TE.encodeUtf8
  where
    encodeByte :: Word8 -> Text
    encodeByte w
      | isUnreserved w = T.singleton (toEnum (fromIntegral w))
      | otherwise      = T.pack ['%', hexDigit (w `shiftR` 4), hexDigit (w .&. 0xF)]
    isUnreserved w =
      let c = toEnum (fromIntegral w) :: Char
      in isAlphaNum c && fromIntegral w < (128 :: Int)
         || c `elem` ("-._~" :: String)
    hexDigit n = toUpper' (intToDigit (fromIntegral n))
    toUpper' c = if c >= 'a' && c <= 'f' then toEnum (fromEnum c - 32) else c

-- | Encode key/value pairs as an @application\/x-www-form-urlencoded@
-- body. Pair order is preserved (deterministic for tests).
urlEncodeForm :: [(Text, Text)] -> Text
urlEncodeForm =
  T.intercalate "&" . map (\(k, v) -> percentEncode k <> "=" <> percentEncode v)

-- | Request body pairs for an RFC 6749 resource-owner password grant.
-- @audience@ follows the Auth0 convention plexus-idp implements.
-- @organization@ (Auth0 Organizations convention) is sent only when a
-- tenant has been selected; IdPs that key tenancy off the user record
-- (plexus-idp today) ignore it — the caller verifies the issued token's
-- @org_id@ afterwards and warns on divergence.
passwordGrantForm :: Text -> Text -> Text -> Maybe Text -> [(Text, Text)]
passwordGrantForm username password audience mOrganization =
  [ ("grant_type", "password")
  , ("username",   username)
  , ("password",   password)
  , ("audience",   audience)
  ]
  <> maybe [] (\org -> [("organization", org)]) mOrganization

-- | Request body pairs for an RFC 6749 refresh-token grant. Consumed by
-- 'requestRefreshGrant' (the transparent-refresh path).
refreshGrantForm :: Text -> [(Text, Text)]
refreshGrantForm refreshToken =
  [ ("grant_type",    "refresh_token")
  , ("refresh_token", refreshToken)
  ]

-- ============================================================================
-- Response parsing
-- ============================================================================

-- | The fields of a successful token-endpoint response we consume.
-- @refresh_token@ is optional per RFC 6749 (Auth0 only returns one when
-- the @offline_access@ scope is granted; plexus-idp always returns one).
data TokenResponse = TokenResponse
  { trAccessToken  :: Text
  , trTokenType    :: Maybe Text
  , trExpiresIn    :: Maybe Int
  , trRefreshToken :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON TokenResponse where
  parseJSON = withObject "TokenResponse" $ \o -> TokenResponse
    <$> o .:  "access_token"
    <*> o .:? "token_type"
    <*> o .:? "expires_in"
    <*> o .:? "refresh_token"

-- | OAuth error responses are JSON with @error@ / @error_description@.
data OauthError = OauthError
  { oeError       :: Text
  , oeDescription :: Maybe Text
  }

instance FromJSON OauthError where
  parseJSON = withObject "OauthError" $ \o -> OauthError
    <$> o .:  "error"
    <*> o .:? "error_description"

-- | Interpret a token-endpoint response body (any status). Success when
-- the body parses as a 'TokenResponse'; an RFC 6749 error body becomes a
-- readable message; anything else falls back to status + raw body.
parseTokenResponse :: Int -> BS.ByteString -> Either Text TokenResponse
parseTokenResponse status body =
  case Aeson.eitherDecodeStrict' body of
    Right tr -> Right tr
    Left _ -> case Aeson.eitherDecodeStrict' body of
      Right OauthError{..} ->
        Left $ oeError <> maybe "" (": " <>) oeDescription
      Left _ ->
        Left $ "token endpoint returned HTTP " <> T.pack (show status)
             <> ": " <> previewBody body

-- | The slice of an OIDC discovery document we consume.
data DiscoveryDoc = DiscoveryDoc
  { ddIssuer        :: Maybe Text
  , ddTokenEndpoint :: Text
  }
  deriving (Show, Eq)

instance FromJSON DiscoveryDoc where
  parseJSON = withObject "DiscoveryDoc" $ \o -> DiscoveryDoc
    <$> o .:? "issuer"
    <*> o .:  "token_endpoint"

-- | Parse a discovery document body.
parseDiscoveryDoc :: BS.ByteString -> Either Text DiscoveryDoc
parseDiscoveryDoc body =
  case Aeson.eitherDecodeStrict' body of
    Right dd -> Right dd
    Left err -> Left ("malformed discovery document: " <> T.pack err)

-- | The discovery URL for an issuer. Trailing slashes on the issuer are
-- normalized away (Auth0 issuers conventionally end in @/@).
discoveryUrl :: Text -> Text
discoveryUrl issuer =
  T.dropWhileEnd (== '/') issuer <> "/.well-known/openid-configuration"

-- | Truncated, single-line body preview for error messages.
previewBody :: BS.ByteString -> Text
previewBody bs =
  let t = T.replace "\n" " " (TE.decodeUtf8Lenient (BS.take 200 bs))
  in if BS.length bs > 200 then t <> "..." else t

-- ============================================================================
-- Store transforms (pure)
-- ============================================================================

-- | The audience requested when neither @--audience@ nor a stored
-- audience exists: @plexus:\<backend\>@ (per-backend audience is the
-- UT-S01 default; it prevents cross-backend token replay).
defaultAudience :: Text -> Text
defaultAudience backend = "plexus:" <> backend

-- | Apply the issuer precedence chain: @--issuer@ flag, then the
-- persisted per-backend issuer, then a helpful error.
--
-- TODO(UT-S01): insert @_info auth_capabilities.oidc@ between the two
-- once backends advertise it.
resolveIssuer :: Text -> Maybe Text -> StoredDefaults -> Either Text Text
resolveIssuer backend mFlag sd =
  case mFlag of
    Just issuer -> Right issuer
    Nothing -> case sdOidc sd >>= odIssuer of
      Just issuer -> Right issuer
      Nothing -> Left $
        "no issuer known for backend `" <> backend <> "`.\n"
        <> "Pass --issuer <url> (e.g. --issuer http://localhost:4461); it is\n"
        <> "persisted on first successful login and becomes the default."

-- | Fold a successful grant into the store:
--
-- * @cookies.access_token = literal:\<jwt\>@ — picked up by the existing
--   WS-upgrade Cookie attach on every later call.
-- * @oidc.issuer@ / @oidc.audience@ — persisted for flag-less re-login.
-- * @oidc.refresh_token = literal:\<opaque\>@ — when the grant returned one;
--   a previous refresh token is dropped (the IdP rotates them anyway).
-- * @oidc.tenant@ (the user's *selection*) is preserved untouched.
--
-- Used by both @login@ (password grant) and the transparent-refresh
-- path (refresh grant) — the store transform is identical.
applyLogin :: Text -> Text -> TokenResponse -> StoredDefaults -> StoredDefaults
applyLogin issuer audience TokenResponse{..} sd = sd
  { sdCookies = Map.insert "access_token"
      (CredentialRef ("literal:" <> trAccessToken)) (sdCookies sd)
  , sdOidc = Just (fromMaybe emptyOidcDefaults (sdOidc sd))
      { odIssuer       = Just issuer
      , odAudience     = Just audience
      , odRefreshToken =
          CredentialRef . ("literal:" <>) <$> trRefreshToken
      }
  }

-- | Logout: drop the access-token cookie and the refresh token, keep
-- everything else — including @oidc.issuer@ \/ @oidc.audience@ \/
-- @oidc.tenant@, so the next @login@ needs no flags.
clearLogin :: StoredDefaults -> StoredDefaults
clearLogin sd = sd
  { sdCookies = Map.delete "access_token" (sdCookies sd)
  , sdOidc = case sdOidc sd of
      Nothing -> Nothing
      Just o  -> Just o { odRefreshToken = Nothing }
  }

-- | Record (or clear, with 'Nothing') the selected tenant. Pure store
-- transform behind @_self \<backend\> tenant@ (UT-3 AC2).
selectTenant :: Maybe Text -> StoredDefaults -> StoredDefaults
selectTenant mTenant sd = sd
  { sdOidc = case (sdOidc sd, mTenant) of
      (Nothing, Nothing) -> Nothing
      (mo, t) -> Just (fromMaybe emptyOidcDefaults mo) { odTenant = t }
  }

-- | The tenant a token actually carries: its @org_id@ claim.
tokenTenant :: Text -> Maybe Text
tokenTenant tok = decodeJwt tok >>= jsOrgId

-- ============================================================================
-- Freshness decision (pure)
-- ============================================================================

-- | Refresh this long *before* the token actually expires, so a token
-- that is valid now but dead by the time the WS upgrade reaches the
-- backend doesn't produce a spurious auth failure.
expiryLeeway :: NominalDiffTime
expiryLeeway = 30

-- | Decide whether the (resolved) access token warrants a refresh
-- attempt:
--
-- * missing or empty — yes (this is also the recovery path when the
--   access-token entry was deleted or corrupted but a refresh token
--   survives);
-- * JWT with @exp@ within 'expiryLeeway' of now (or past) — yes;
-- * JWT without @exp@, or a non-JWT value (API keys, opaque tokens) —
--   no: we cannot judge freshness, so we never touch it.
shouldRefresh :: UTCTime -> Maybe Text -> Bool
shouldRefresh _ Nothing = True
shouldRefresh now (Just tok)
  | T.null (T.strip tok) = True
  | otherwise = case decodeJwt tok >>= jsExp of
      Nothing  -> False
      Just exp_ -> addUTCTime expiryLeeway now >= exp_

-- ============================================================================
-- HTTP via curl
-- ============================================================================

-- | Status code + body of an HTTP exchange.
data HttpResponse = HttpResponse
  { hrStatus :: Int
  , hrBody   :: BS.ByteString
  }
  deriving (Show, Eq)

-- | Run @curl@ with shared flags, feeding @stdin@ and splitting the
-- trailing @\\n\<status\>@ marker that @-w@ appends off the body.
-- 'Left' covers transport-level failures (DNS, refused, timeout) with
-- curl's own stderr message.
runCurl :: [String] -> String -> IO (Either Text HttpResponse)
runCurl args stdinBody = do
  (code, out, err) <- readProcessWithExitCode "curl" baseArgs stdinBody
  case code of
    ExitFailure n ->
      pure . Left $ "curl failed (exit " <> T.pack (show n) <> "): "
                  <> T.strip (T.pack err)
    ExitSuccess ->
      let outT = T.pack out
          (bodyT, statusT) = T.breakOnEnd "\n" outT
      in case reads (T.unpack statusT) of
           [(status, "")] -> pure . Right $ HttpResponse
             { hrStatus = status
             , hrBody   = TE.encodeUtf8 (T.dropWhileEnd (== '\n') bodyT)
             }
           _ -> pure . Left $ "curl produced no status marker: " <> T.take 200 outT
  where
    baseArgs =
      [ "-sS"            -- quiet, but keep error messages
      , "--max-time", "15"
      , "-w", "\n%{http_code}"
      ] <> args

-- | GET a URL.
httpGet :: Text -> IO (Either Text HttpResponse)
httpGet url = runCurl [T.unpack url] ""

-- | POST an @application\/x-www-form-urlencoded@ body. The body travels
-- on curl's stdin (@--data-binary \@-@) so credentials never appear in
-- the process's argv.
httpPostForm :: Text -> Text -> IO (Either Text HttpResponse)
httpPostForm url body =
  runCurl
    [ "-X", "POST"
    , "-H", "Content-Type: application/x-www-form-urlencoded"
    , "--data-binary", "@-"
    , T.unpack url
    ]
    (T.unpack body)

-- | Resolve an issuer's token endpoint via its discovery document.
fetchTokenEndpoint :: Text -> IO (Either Text Text)
fetchTokenEndpoint issuer = do
  r <- httpGet (discoveryUrl issuer)
  pure $ case r of
    Left err -> Left $ "could not fetch " <> discoveryUrl issuer <> ": " <> err
    Right HttpResponse{..}
      | hrStatus /= 200 ->
          Left $ "discovery document at " <> discoveryUrl issuer
               <> " returned HTTP " <> T.pack (show hrStatus)
      | otherwise -> ddTokenEndpoint <$> parseDiscoveryDoc hrBody

-- | Perform the password grant against a token endpoint.
requestPasswordGrant
  :: Text        -- ^ token endpoint
  -> Text        -- ^ username
  -> Text        -- ^ password
  -> Text        -- ^ audience
  -> Maybe Text  -- ^ organization (selected tenant), if any
  -> IO (Either Text TokenResponse)
requestPasswordGrant tokenEndpoint username password audience mOrg = do
  let body = urlEncodeForm (passwordGrantForm username password audience mOrg)
  r <- httpPostForm tokenEndpoint body
  pure $ case r of
    Left err -> Left err
    Right HttpResponse{..} -> parseTokenResponse hrStatus hrBody

-- | Perform the refresh-token grant against a token endpoint.
requestRefreshGrant
  :: Text  -- ^ token endpoint
  -> Text  -- ^ refresh token (resolved value)
  -> IO (Either Text TokenResponse)
requestRefreshGrant tokenEndpoint refreshToken = do
  let body = urlEncodeForm (refreshGrantForm refreshToken)
  r <- httpPostForm tokenEndpoint body
  pure $ case r of
    Left err -> Left err
    Right HttpResponse{..} -> parseTokenResponse hrStatus hrBody

-- ============================================================================
-- Transparent refresh (UT-3)
-- ============================================================================

-- | What 'ensureFreshLogin' did, for caller-side logging. Constructors
-- never carry token material — only human-readable reasons.
data RefreshOutcome
  = RefreshNotNeeded
    -- ^ The stored access token is present and not near expiry (or is
    --   an opaque non-JWT value we refuse to judge).
  | RefreshUnavailable
    -- ^ A refresh is warranted but the store has no issuer + refresh
    --   token pair to perform it with.
  | Refreshed
    -- ^ The access token (and rotated refresh token) were renewed and
    --   persisted.
  | RefreshFailed !Text
    -- ^ The refresh was attempted and failed (network, IdP rejection,
    --   unresolvable refresh-token ref). The store is left untouched
    --   except that the message explains the cause.
  deriving (Show, Eq)

-- | Transparent refresh: if the stored @cookies.access_token@ is
-- missing\/expired ('shouldRefresh') and the store holds an issuer +
-- refresh token, perform the refresh grant, persist the renewed pair
-- (the IdP rotates refresh tokens, so persistence must be immediate —
-- the presented token is dead the moment the grant succeeds), and
-- return the updated store.
--
-- Pulled into every dispatch path ('buildEnv' in the synapse
-- executable; 'Synapse.Self.resolveToken' for synapse-cc) so an expired
-- token mid-session heals without the user re-entering credentials —
-- the UT-3 \"transparent\" acceptance row.
--
-- Never throws for refresh-shaped failures; IO exceptions from
-- 'writeDefaults' (disk full, permissions) propagate — silently holding
-- a consumed refresh token would be worse.
ensureFreshLogin :: Text -> StoredDefaults -> IO (StoredDefaults, RefreshOutcome)
ensureFreshLogin backend sd = do
  now <- getCurrentTime
  mAccess <- case Map.lookup "access_token" (sdCookies sd) of
    Nothing  -> pure Nothing
    Just ref -> either (const Nothing) Just <$> resolveRef defaultRegistry ref
  if not (shouldRefresh now mAccess)
    then pure (sd, RefreshNotNeeded)
    else case (sdOidc sd >>= odIssuer, sdOidc sd >>= odRefreshToken) of
      (Just issuer, Just refreshRef) -> do
        eRefresh <- resolveRef defaultRegistry refreshRef
        case eRefresh of
          Left _ -> pure
            (sd, RefreshFailed "stored refresh-token ref did not resolve")
          Right refreshTok -> do
            eEndpoint <- fetchTokenEndpoint issuer
            case eEndpoint of
              Left err -> pure (sd, RefreshFailed err)
              Right tokenEndpoint -> do
                eTr <- requestRefreshGrant tokenEndpoint refreshTok
                case eTr of
                  Left err -> pure (sd, RefreshFailed err)
                  Right tr -> do
                    let audience = fromMaybe (defaultAudience backend)
                                     (sdOidc sd >>= odAudience)
                        sd' = applyLogin issuer audience tr sd
                    writeDefaults backend sd'
                    pure (sd', Refreshed)
      _ -> pure (sd, RefreshUnavailable)
