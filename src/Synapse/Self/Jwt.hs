{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | JWT inspection: decode-without-verify for display and freshness
-- decisions.
--
-- Extracted from "Synapse.Self.Command" (where it served the @show@
-- verb) so the OIDC login\/refresh machinery ("Synapse.Self.Oidc") can
-- read token expiry without importing the CLI surface — UT-3's
-- transparent-refresh path needs @exp@ at dispatch time.
--
-- Nothing here verifies signatures; this is client-side introspection
-- of tokens we already hold. Signatures are never stored or printed.
module Synapse.Self.Jwt
  ( JwtSummary(..)
  , JwtClaim(..)
  , decodeJwt
  , renderJwtSummary
  , renderRelativeExpiry
  , jwtToJson
  , formatISO8601
  ) where

import Data.Aeson (Value(..), (.=), object, eitherDecodeStrict')
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

-- | Structured summary of a JWT. We only record fields the CLI prints
-- or the refresh path consults. Signatures are never stored.
data JwtSummary = JwtSummary
  { jsAlg      :: !(Maybe Text)
  , jsKid      :: !(Maybe Text)
  , jsIss      :: !(Maybe Text)
  , jsAud      :: !(Maybe Text)  -- ^ Rendered: first aud entry if array.
  , jsSub      :: !(Maybe Text)
  , jsExp      :: !(Maybe UTCTime)
  , jsIat      :: !(Maybe UTCTime)
  , jsUsername :: !(Maybe Text)  -- ^ @preferred_username@ or @username@.
  , jsEmail    :: !(Maybe Text)
  , jsOrgId    :: !(Maybe Text)
    -- ^ @org_id@ — Auth0 Organizations convention; plexus-idp emits it
    --   and trak-style backends derive the tenant from it (UT-3).
  , jsExtraClaims :: ![JwtClaim]  -- ^ Retained for future; unused in v1.
  }
  deriving (Show, Eq)

-- | A single JWT claim, name + rendered value. Reserved for future
-- extension (e.g. scope, roles). Currently unused — the named fields
-- on 'JwtSummary' cover everything we print.
data JwtClaim = JwtClaim
  { jcName :: !Text
  , jcValue :: !Text
  }
  deriving (Show, Eq)

-- | Attempt to decode a string as a JWT. Returns 'Nothing' on anything
-- that isn't JWT-shaped. A "JWT-shaped" value has exactly two dots,
-- each of the three segments base64-decodes cleanly (URL-safe, with or
-- without padding), and the header JSON has an @alg@ field.
--
-- Decoding never fails loudly: a value that looks JWT-shaped but has
-- a non-JSON payload yields 'Nothing'.
decodeJwt :: Text -> Maybe JwtSummary
decodeJwt t = case T.splitOn "." t of
  [h, p, _sig]
    | not (T.null h), not (T.null p) -> do
        headerJson <- decodeSegment h
        payloadJson <- decodeSegment p
        -- Header must have an `alg` field to count as a JWT.
        case headerJson of
          Object o | KM.member "alg" o -> Just (buildSummary headerJson payloadJson)
          _                            -> Nothing
  _ -> Nothing

decodeSegment :: Text -> Maybe Value
decodeSegment seg =
  let bs = TE.encodeUtf8 seg
      -- JWT uses URL-safe base64 with padding stripped.
      padded = padBase64 bs
  in case B64U.decode padded of
       Left _    -> Nothing
       Right raw -> case eitherDecodeStrict' raw :: Either String Value of
         Left _  -> Nothing
         Right v -> Just v

-- | Re-pad a URL-safe base64 string to a multiple of 4. @base64-bytestring@
-- requires padding for decoding; JWTs strip it.
padBase64 :: BS.ByteString -> BS.ByteString
padBase64 bs =
  let n = BS.length bs
      r = n `mod` 4
      pad = case r of
        0 -> 0
        2 -> 2
        3 -> 1
        _ -> 0  -- 1 is invalid; let decode fail on it
  in bs <> BS8.replicate pad '='

buildSummary :: Value -> Value -> JwtSummary
buildSummary header payload = JwtSummary
  { jsAlg      = stringClaim header "alg"
  , jsKid      = stringClaim header "kid"
  , jsIss      = stringClaim payload "iss"
  , jsAud      = audClaim payload
  , jsSub      = stringClaim payload "sub"
  , jsExp      = timeClaim payload "exp"
  , jsIat      = timeClaim payload "iat"
  , jsUsername = case stringClaim payload "preferred_username" of
      Just u  -> Just u
      Nothing -> stringClaim payload "username"
  , jsEmail    = stringClaim payload "email"
  , jsOrgId    = stringClaim payload "org_id"
  , jsExtraClaims = []
  }

-- | Pull a string claim out of a JSON object. Returns 'Nothing' for
-- non-object values, missing fields, or fields that aren't strings.
stringClaim :: Value -> Text -> Maybe Text
stringClaim (Object o) k = case KM.lookup (Key.fromText k) o of
  Just (String s) -> Just s
  _               -> Nothing
stringClaim _ _ = Nothing

-- | @aud@ can be either a string or an array of strings. We render the
-- first element in either case.
audClaim :: Value -> Maybe Text
audClaim (Object o) = case KM.lookup "aud" o of
  Just (String s)   -> Just s
  Just (Array arr)  -> case foldMap stringOf arr of
    []    -> Nothing
    (x:_) -> Just x
  _ -> Nothing
  where
    stringOf (String s) = [s]
    stringOf _          = []
audClaim _ = Nothing

-- | Integer seconds-since-epoch as a 'UTCTime'.
timeClaim :: Value -> Text -> Maybe UTCTime
timeClaim (Object o) k = case KM.lookup (Key.fromText k) o of
  Just (Number n) -> Just (posixSecondsToUTCTime (realToFrac n))
  _               -> Nothing
timeClaim _ _ = Nothing

-- | Render a JWT summary as a list of aligned label/value lines.
-- Never emits the signature; never emits raw claim bodies.
renderJwtSummary :: UTCTime -> JwtSummary -> [Text]
renderJwtSummary now JwtSummary{..} = catMaybes
  [ fmap (labelled "alg") jsAlg
  , fmap (labelled "kid" . truncateKid) jsKid
  , fmap (labelled "iss") jsIss
  , fmap (labelled "aud") jsAud
  , fmap (labelled "sub") jsSub
  , fmap (labelled "org") jsOrgId
  , fmap (\t ->
      labelled "exp"
        (T.pack (formatISO8601 t) <> "  (" <> renderRelativeExpiry now t <> ")"))
      jsExp
  , fmap (\t -> labelled "iat" (T.pack (formatISO8601 t))) jsIat
  , case (jsUsername, jsEmail) of
      (Just u, Just e) -> Just $ labelled "user" (u <> " (" <> e <> ")")
      (Just u, Nothing) -> Just $ labelled "user" u
      (Nothing, Just e) -> Just $ labelled "user" e
      (Nothing, Nothing) -> Nothing
  ]
  where
    labelled label val = label <> padTo 5 label <> val
    padTo n lab = T.replicate (max 1 (n - T.length lab)) " "

-- | Long `kid` values are typically random key-IDs that add visual
-- noise without aiding identification. Keep the first 6 chars and
-- elide the tail.
truncateKid :: Text -> Text
truncateKid t
  | T.length t > 16 = T.take 6 t <> "..." <> T.takeEnd 4 t
  | otherwise       = t

formatISO8601 :: UTCTime -> String
formatISO8601 = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- | Render @exp - now@ as a human-readable relative duration:
-- "expired 16d 4h ago" for past, "valid for 4m 32s" for future.
--
-- Uses the two largest non-zero units (d/h, h/m, m/s, or just s for
-- sub-minute), which matches the operator-facing prose the ticket asks
-- for: glance-readable, enough precision to act on.
renderRelativeExpiry :: UTCTime -> UTCTime -> Text
renderRelativeExpiry now exp_ =
  let delta = diffUTCTime exp_ now
      secs :: Integer
      secs = floor (realToFrac delta :: Double)
      absSecs = abs secs
      (d, r1) = absSecs `divMod` 86400
      (h, r2) = r1     `divMod` 3600
      (m, s)  = r2     `divMod` 60
      unitPair = case (d, h, m, s) of
        _ | d > 0 && h > 0 -> T.pack (show d) <> "d " <> T.pack (show h) <> "h"
        _ | d > 0          -> T.pack (show d) <> "d"
        _ | h > 0 && m > 0 -> T.pack (show h) <> "h " <> T.pack (show m) <> "m"
        _ | h > 0          -> T.pack (show h) <> "h"
        _ | m > 0 && s > 0 -> T.pack (show m) <> "m " <> T.pack (show s) <> "s"
        _ | m > 0          -> T.pack (show m) <> "m"
        _                  -> T.pack (show s) <> "s"
  in if secs < 0
       then "expired " <> unitPair <> " ago"
       else "valid for " <> unitPair

-- | Minimal JSON render of a 'JwtSummary' for @--json@ output paths.
jwtToJson :: JwtSummary -> Value
jwtToJson JwtSummary{..} = object $ catMaybes
  [ fmap ("alg" .=) jsAlg
  , fmap ("kid" .=) jsKid
  , fmap ("iss" .=) jsIss
  , fmap ("aud" .=) jsAud
  , fmap ("sub" .=) jsSub
  , fmap ("org_id" .=) jsOrgId
  , fmap (\t -> "exp" .= formatISO8601 t) jsExp
  , fmap (\t -> "iat" .= formatISO8601 t) jsIat
  , fmap ("preferred_username" .=) jsUsername
  , fmap ("email" .=) jsEmail
  ]
