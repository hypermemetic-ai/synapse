{-# LANGUAGE OverloadedStrings #-}

-- | IDENTITY-43: an agent's key and the proof it logs in with — IDENTITY-40's
-- "The wire", exactly.
--
-- A key is an Ed25519 seed (32 bytes); its principal is
-- @key:\<64 lowercase hex of the public key\>@. A proof is a bearer value
--
-- @
-- plexus-key.\<base64url-nopad of UTF-8 JSON\>
-- {"v":1,"key":"\<64 hex\>","aud":"\<ws url\>","agent":"\<id\>","nonce":"\<b64url\>","ts":\<unix s\>,"sig":"\<b64url\>"}
-- @
--
-- where @sig@ is Ed25519 over the UTF-8 bytes of exactly
-- @plexus-key-login/v1\\n\<aud\>\\n\<agent\>\\n\<nonce\>\\n\<ts\>@ (no trailing
-- newline). The JSON's fields go in that order, compact, so the same
-- inputs give the same proof on every side (Ed25519 signing is
-- deterministic): see the fixed vector in @test/IdentitySpec.hs@.
module Synapse.Identity.Proof
  ( -- * Keys
    Seed
  , seedFromBytes
  , seedBytes
  , generateSeed
  , publicKeyHex
  , principalOf

    -- * The proof
  , canonicalMessage
  , buildProof
  , newNonce
  , wsAudience
  , validAgent
  , validKeyName

    -- * Encodings
  , b64urlNoPad
  , hexText
  , decodeSeedText
  , encodeSeedText
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed
import Crypto.Error (CryptoFailable(..))
import Crypto.Random (getRandomBytes)
import qualified Data.Aeson.Encoding as AE
import Data.Aeson ((.=))
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Printf (printf)

-- | An Ed25519 seed: the 32 bytes a key is.
newtype Seed = Seed ByteString
  deriving (Eq)

instance Show Seed where
  show _ = "Seed <redacted>"

-- | A seed from its 32 bytes, or why these are not one.
seedFromBytes :: ByteString -> Either Text Seed
seedFromBytes b
  | BS.length b == 32 = Right (Seed b)
  | otherwise = Left $ "a key is 32 bytes, not " <> T.pack (show (BS.length b))

seedBytes :: Seed -> ByteString
seedBytes (Seed b) = b

-- | A new seed from the system's randomness.
generateSeed :: IO Seed
generateSeed = Seed <$> getRandomBytes 32

secretOf :: Seed -> Ed.SecretKey
secretOf (Seed b) = case Ed.secretKey b of
  CryptoPassed sk -> sk
  CryptoFailed e -> error ("Synapse.Identity.Proof: " <> show e)  -- length is checked at construction

publicOf :: Seed -> Ed.PublicKey
publicOf = Ed.toPublic . secretOf

-- | The public key, 64 lowercase hex.
publicKeyHex :: Seed -> Text
publicKeyHex = hexText . BA.convert . publicOf

-- | @key:\<64 hex\>@ — the principal a key is.
principalOf :: Seed -> Text
principalOf s = "key:" <> publicKeyHex s

-- | The bytes the signature covers, as text:
-- @plexus-key-login/v1\\n\<aud\>\\n\<agent\>\\n\<nonce\>\\n\<ts\>@.
canonicalMessage :: Text -> Text -> Text -> Integer -> Text
canonicalMessage aud agent nonce ts =
  T.intercalate "\n" ["plexus-key-login/v1", aud, agent, nonce, T.pack (show ts)]

-- | The proof for this key, audience (the ws URL dialed), agent, nonce (its
-- raw bytes; 32 of them) and time (unix seconds).
buildProof :: Seed -> Text -> Text -> ByteString -> Integer -> Text
buildProof seed aud agent nonceRaw ts =
  let sk = secretOf seed
      pk = Ed.toPublic sk
      nonce = b64urlNoPad nonceRaw
      msg = TE.encodeUtf8 (canonicalMessage aud agent nonce ts)
      sig = Ed.sign sk pk msg
      json = AE.encodingToLazyByteString $ AE.pairs $ mconcat
        [ "v" .= (1 :: Int)
        , "key" .= hexText (BA.convert pk)
        , "aud" .= aud
        , "agent" .= agent
        , "nonce" .= nonce
        , "ts" .= ts
        , "sig" .= b64urlNoPad (BA.convert sig)
        ]
  in "plexus-key." <> b64urlNoPad (LBS.toStrict json)

-- | 32 random bytes for a proof's nonce.
newNonce :: IO ByteString
newNonce = getRandomBytes 32

-- | The URL a proof's @aud@ names: the socket synapse dials,
-- @ws://HOST:PORT@ (SCAFFOLD-21: behind TLS, @wss://HOST@ —
-- 'Synapse.Endpoint.audience' says which for an address).
wsAudience :: Text -> Int -> Text
wsAudience host port = "ws://" <> host <> ":" <> T.pack (show port)

-- | An agent id: 1–64 of @[A-Za-z0-9._-]@.
validAgent :: Text -> Bool
validAgent a = not (T.null a) && T.length a <= 64 && T.all idChar a

-- | A key's name: the same alphabet (it names a file).
validKeyName :: Text -> Bool
validKeyName n = validAgent n && n `notElem` [".", ".."]

idChar :: Char -> Bool
idChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("._-" :: String)

-- | base64url without padding.
b64urlNoPad :: ByteString -> Text
b64urlNoPad = TE.decodeLatin1 . B64U.encodeUnpadded

-- | Lowercase hex.
hexText :: ByteString -> Text
hexText = T.pack . concatMap (printf "%02x") . BS.unpack

-- | A seed as it is kept (base64 of the 32 bytes; @PLEXUS_KEY@'s form).
encodeSeedText :: Seed -> Text
encodeSeedText (Seed b) = TE.decodeLatin1 (B64.encode b)

-- | A kept seed back: standard or url base64, padded or not, surrounding
-- whitespace ignored.
decodeSeedText :: Text -> Either Text Seed
decodeSeedText t =
  let s = TE.encodeUtf8 (T.strip t)
      std = B64.decode s
      url = B64U.decodeUnpadded s
      urlp = B64U.decode s
  in case [b | Right b <- [std, urlp, url]] of
       (b : _) -> seedFromBytes b
       [] -> Left "not base64"
