{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for 'Synapse.Self.Command' (SELF-4).
--
-- Two layers:
--
-- 1. Pure tests: the set-value heuristic ('wrapAsLiteralIfNeeded'),
--    JWT decode + relative-expiry rendering, and exit-code behavior
--    for keychain-gated verbs.
--
-- 2. Integration tests: exercise 'runSelfCommand' against a tmp HOME
--    so the on-disk @~\/.plexus\/\<backend\>\/defaults.json@ lifecycle
--    is covered end-to-end.
--
-- The integration tests use 'withSystemTempDirectory' + 'withEnv "HOME"'
-- (mirroring the pattern in SelfSpec.hs) so they never touch the
-- developer's real @~\/.plexus@.
module SelfCommandSpec (spec) where

import Control.Exception (bracket_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import System.Directory (doesFileExist, getHomeDirectory, removeFile)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Synapse.Self
  ( CredentialRef(..)
  , StoredDefaults(..)
  , decodeDefaults
  , emptyStoredDefaults
  )
import Synapse.Self.Command

spec :: Spec
spec = do
  describe "wrapAsLiteralIfNeeded" $ do
    it "wraps a bare string with no scheme" $
      wrapAsLiteralIfNeeded "abc123" `shouldBe` "literal:abc123"

    it "preserves a literal: prefix" $
      wrapAsLiteralIfNeeded "literal:already" `shouldBe` "literal:already"

    it "preserves env:// refs" $
      wrapAsLiteralIfNeeded "env://FOO" `shouldBe` "env://FOO"

    it "preserves keychain:// refs" $
      wrapAsLiteralIfNeeded "keychain://svc/acct" `shouldBe` "keychain://svc/acct"

    it "preserves file:// refs" $
      wrapAsLiteralIfNeeded "file:///etc/token" `shouldBe` "file:///etc/token"

    it "preserves vault:// refs" $
      wrapAsLiteralIfNeeded "vault://secret" `shouldBe` "vault://secret"

    it "wraps a JWT (bare) as literal:<jwt>" $
      wrapAsLiteralIfNeeded "eyJhbGc.eyJzdWIi.sig"
        `shouldBe` "literal:eyJhbGc.eyJzdWIi.sig"

    it "wraps a string containing a scheme-like substring mid-value" $
      -- The heuristic only matches on *prefix*; this is a bare string.
      wrapAsLiteralIfNeeded "xyz env://FOO"
        `shouldBe` "literal:xyz env://FOO"

  describe "redactRef (defect B — never print literal: secrets)" $ do
    it "masks a long literal: body to <first6>...<last4>" $
      redactRef "literal:supersecretvalue1234567890"
        `shouldBe` "literal:supers...7890"

    it "fully masks a short literal: body (<= 16 chars)" $
      redactRef "literal:shortsecret" `shouldBe` "literal:***"

    it "leaves env:// refs untouched (pointer, not a secret)" $
      redactRef "env://MY_API_KEY" `shouldBe` "env://MY_API_KEY"

    it "leaves file:// refs untouched" $
      redactRef "file:///etc/token" `shouldBe` "file:///etc/token"

    it "leaves keychain:// refs untouched" $
      redactRef "keychain://svc/acct" `shouldBe` "keychain://svc/acct"

  describe "decodeJwt" $ do
    it "returns Nothing for a non-JWT bare string" $
      decodeJwt "hello-world" `shouldBe` Nothing

    it "returns Nothing for a 2-segment value (missing signature slot)" $
      decodeJwt "eyJhbGc.eyJzdWIi" `shouldBe` Nothing

    it "returns Nothing when the header has no alg field" $ do
      let h = encodeSeg "{\"typ\":\"JWT\"}"
          p = encodeSeg "{\"sub\":\"x\"}"
      decodeJwt (h <> "." <> p <> ".sig") `shouldBe` Nothing

    it "decodes a minimal well-formed JWT" $ do
      let h = encodeSeg "{\"alg\":\"HS256\",\"kid\":\"k1\"}"
          p = encodeSeg "{\"sub\":\"jdoe\",\"iss\":\"https://idp.example/realms/r\"}"
          jwt = h <> "." <> p <> ".sig"
      case decodeJwt jwt of
        Nothing -> expectationFailure "expected Just, got Nothing"
        Just s -> do
          jsAlg s `shouldBe` Just "HS256"
          jsKid s `shouldBe` Just "k1"
          jsSub s `shouldBe` Just "jdoe"
          jsIss s `shouldBe` Just "https://idp.example/realms/r"

    it "handles aud as an array (first element wins)" $ do
      let h = encodeSeg "{\"alg\":\"RS256\"}"
          p = encodeSeg "{\"aud\":[\"account\",\"app\"]}"
      case decodeJwt (h <> "." <> p <> ".sig") of
        Just s  -> jsAud s `shouldBe` Just "account"
        Nothing -> expectationFailure "expected Just"

    it "extracts preferred_username and email" $ do
      let h = encodeSeg "{\"alg\":\"HS256\"}"
          p = encodeSeg "{\"preferred_username\":\"ben\",\"email\":\"ben@x.dev\"}"
      case decodeJwt (h <> "." <> p <> ".sig") of
        Just s -> do
          jsUsername s `shouldBe` Just "ben"
          jsEmail s    `shouldBe` Just "ben@x.dev"
        Nothing -> expectationFailure "expected Just"

    it "decodes exp as a UTCTime" $ do
      let h = encodeSeg "{\"alg\":\"HS256\"}"
          p = encodeSeg "{\"exp\":1700000000}"
      case decodeJwt (h <> "." <> p <> ".sig") of
        Just s -> jsExp s `shouldBe`
          Just (posixSecondsToUTCTime 1700000000)
        Nothing -> expectationFailure "expected Just"

    it "tolerates URL-safe base64 with stripped padding" $ do
      -- Construct a header/payload whose un-padded base64url has length
      -- not divisible by 4.
      let headerBs = "{\"alg\":\"HS256\"}" :: BS.ByteString
          payloadBs = "{\"sub\":\"x\"}" :: BS.ByteString
          h = T.dropWhileEnd (== '=') (TE.decodeUtf8 (B64U.encode headerBs))
          p = T.dropWhileEnd (== '=') (TE.decodeUtf8 (B64U.encode payloadBs))
      case decodeJwt (h <> "." <> p <> ".sig") of
        Just s  -> jsSub s `shouldBe` Just "x"
        Nothing -> expectationFailure "expected Just"

  describe "renderRelativeExpiry" $ do
    let now = posixSecondsToUTCTime 1_700_000_000

    it "renders 'expired Nm Ns ago' for a recently-expired token" $ do
      let past = addUTCTime (-(60 * 4 + 32)) now  -- 4m 32s ago
      renderRelativeExpiry now past `shouldBe` "expired 4m 32s ago"

    it "renders 'valid for Nm Ns' for a near-future token" $ do
      let future = addUTCTime (60 * 4 + 32) now
      renderRelativeExpiry now future `shouldBe` "valid for 4m 32s"

    it "uses days+hours for old tokens" $ do
      let past = addUTCTime (-(16 * 86400 + 4 * 3600)) now
      renderRelativeExpiry now past `shouldBe` "expired 16d 4h ago"

    it "falls back to seconds-only for sub-minute deltas" $ do
      let future = addUTCTime 5 now
      renderRelativeExpiry now future `shouldBe` "valid for 5s"

    it "hours-only when minutes are zero" $ do
      let future = addUTCTime (3 * 3600) now
      renderRelativeExpiry now future `shouldBe` "valid for 3h"

  -- SELF-8: keychain-touching verbs (set-secret, upgrade-to-keychain,
  -- import-token --to-keychain) live behind an opt-in test-suite
  -- (plexus-synapse:keychain-test, gated on the `build-keychain-tests`
  -- cabal flag). They shell out to the `security` CLI on macOS, which
  -- we don't want firing during a default `cabal test` run. The
  -- keychain-unavailable error paths (non-macOS platforms) are verified
  -- there too, so this block is intentionally empty.
  pure ()

  -- ==========================================================================
  -- Integration: round-trip against a tmp HOME
  -- ==========================================================================

  describe "runSelfCommand: integration round-trip" $ do
    it "set -> show-readable -> resolve -> unset -> clear" $
      withTmpHome $ \tmpHome -> do
        let backend = "itest"
            defaultsFile = tmpHome </> ".plexus" </> T.unpack backend
                                     </> "defaults.json"

        -- 1. Set two values.
        _ <- runSelfCommand
          (CmdSet backend Cookie "access_token" "literal:abc123")
        _ <- runSelfCommand
          (CmdSet backend Header "X-Trace" "env://NONEXISTENT_ENV_SELF4")

        -- The file now exists and decodes round-trip.
        exists <- doesFileExist defaultsFile
        exists `shouldBe` True
        bytes <- BS.readFile defaultsFile
        case decodeDefaults bytes of
          Left err -> expectationFailure ("decode failed: " <> T.unpack err)
          Right sd -> do
            Map.lookup "access_token" (sdCookies sd)
              `shouldBe` Just (CredentialRef "literal:abc123")
            Map.lookup "X-Trace" (sdHeaders sd)
              `shouldBe` Just (CredentialRef "env://NONEXISTENT_ENV_SELF4")

        -- 2. resolve access_token prints the value.
        code <- runSelfCommand (CmdResolve backend Cookie "access_token" False)
        code `shouldBe` ExitSuccess

        -- 3. unset removes it.
        _ <- runSelfCommand
          (CmdUnset backend Cookie "access_token" True False)
        bytes' <- BS.readFile defaultsFile
        case decodeDefaults bytes' of
          Right sd -> Map.member "access_token" (sdCookies sd) `shouldBe` False
          Left err -> expectationFailure ("decode failed: " <> T.unpack err)

        -- 4. clear --yes deletes the file.
        _ <- runSelfCommand (CmdClear backend True)
        stillThere <- doesFileExist defaultsFile
        stillThere `shouldBe` False

    it "set wraps a bare value as literal: on disk" $
      withTmpHome $ \tmpHome -> do
        let backend = "itest-wrap"
            path = tmpHome </> ".plexus" </> T.unpack backend </> "defaults.json"
        -- Bare JWT-looking value (three dot-separated segments) should
        -- still be wrapped — the prefix heuristic doesn't look like any
        -- known scheme.
        _ <- runSelfCommand
          (CmdSet backend Cookie "t" "eyJhbGc.eyJzdWIi.sig")
        bytes <- BS.readFile path
        case decodeDefaults bytes of
          Right sd -> Map.lookup "t" (sdCookies sd) `shouldBe`
            Just (CredentialRef "literal:eyJhbGc.eyJzdWIi.sig")
          Left err -> expectationFailure ("decode failed: " <> T.unpack err)

    it "clear on a missing file is a no-op (succeeds)" $
      withTmpHome $ \_tmp -> do
        code <- runSelfCommand (CmdClear "never-touched" True)
        code `shouldBe` ExitSuccess

    it "resolve on a missing entry exits non-zero" $
      withTmpHome $ \_tmp -> do
        code <- runSelfCommand (CmdResolve "ghost" Cookie "nope" False)
        code `shouldBe` ExitFailure 1

    it "unset on a missing entry exits non-zero" $
      withTmpHome $ \_tmp -> do
        code <- runSelfCommand (CmdUnset "ghost" Cookie "nope" True False)
        code `shouldBe` ExitFailure 1

    it "import-token (from file) stores literal:<jwt>" $
      withTmpHome $ \tmpHome -> do
        let backend = "itest-import"
            tokenFile = tmpHome </> "tok.jwt"
            defaultsFile = tmpHome </> ".plexus" </> T.unpack backend
                                     </> "defaults.json"
        TIO.writeFile tokenFile "eyJhbGc.eyJzdWIi.sig\n"
        _ <- runSelfCommand
          (CmdImportToken backend (T.pack tokenFile) False)
        bytes <- BS.readFile defaultsFile
        case decodeDefaults bytes of
          Right sd -> Map.lookup "access_token" (sdCookies sd) `shouldBe`
            Just (CredentialRef "literal:eyJhbGc.eyJzdWIi.sig")
          Left err -> expectationFailure ("decode failed: " <> T.unpack err)

-- ============================================================================
-- helpers
-- ============================================================================

-- | Run @action@ with @HOME@ set to a fresh temp directory; restore on exit.
withTmpHome :: (FilePath -> IO a) -> IO a
withTmpHome action =
  withSystemTempDirectory "synapse-self4-test-" $ \tmp -> do
    prior <- lookupEnv "HOME"
    bracket_
      (setEnv "HOME" tmp)
      (case prior of
         Just v  -> setEnv "HOME" v
         Nothing -> unsetEnv "HOME")
      (action tmp)

-- | Encode a JSON string as URL-safe base64 without padding (JWT style).
encodeSeg :: BS.ByteString -> Text
encodeSeg bs =
  T.dropWhileEnd (== '=') (TE.decodeUtf8 (B64U.encode bs))
