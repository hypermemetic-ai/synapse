{-# LANGUAGE OverloadedStrings #-}

-- | IDENTITY-43: an agent's key, its proof, and where synapse keeps both.
--
-- * the canonical message the signature covers, byte for byte;
-- * a FIXED proof vector (seed 0x01..0x20, aud ws://127.0.0.1:4460, agent
--   worker-3, nonce 32 × 0x07, ts 1791600000) — printed, and pinned, so the
--   Rust side (IDENTITY-41/42) can check it computes the same string;
-- * the stores' order — PLEXUS_KEY, the keychain (an in-memory one here),
--   $CREDENTIALS_DIRECTORY, ~/.plexus/keys — and the session store's, all in
--   a directory of the test's own: nothing touches ~/.plexus, the real
--   keychain or a socket.
module Main (main) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.IORef (readIORef, newIORef, atomicModifyIORef')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus, intersectFileModes)
import Test.Hspec

import Synapse.Identity.Command (splitScopes)
import Synapse.Identity.Proof
import Synapse.Identity.Store

-- ============================================================================
-- The fixed vector
-- ============================================================================

vectorSeed :: Seed
vectorSeed = either (error . T.unpack) id (seedFromBytes (BS.pack [1 .. 32]))

vectorAud, vectorAgent :: Text
vectorAud = "ws://127.0.0.1:4460"
vectorAgent = "worker-3"

vectorNonce :: BS.ByteString
vectorNonce = BS.replicate 32 7

vectorTs :: Integer
vectorTs = 1791600000

vectorProof :: Text
vectorProof = buildProof vectorSeed vectorAud vectorAgent vectorNonce vectorTs

-- | The proof the vector gives, pinned. Ed25519 signing is deterministic,
-- so this never changes unless the wire does.
expectedProof :: Text
expectedProof =
  "plexus-key.eyJ2IjoxLCJrZXkiOiI3OWI1NTYyZThmZTY1NGY5NDA3OGIxMTJlOGE5OGJhNzkwMWY4NTNhZTY5NWJlZDdlMGUzOTEwYmFkMDQ5NjY0IiwiYXVkIjoid3M6Ly8xMjcuMC4wLjE6NDQ2MCIsImFnZW50Ijoid29ya2VyLTMiLCJub25jZSI6IkJ3Y0hCd2NIQndjSEJ3Y0hCd2NIQndjSEJ3Y0hCd2NIQndjSEJ3Y0hCd2MiLCJ0cyI6MTc5MTYwMDAwMCwic2lnIjoiSlJxSGx6VEd2WGZubm8zQWlsMWhudU9wZ2J6SzlyNFZPa0l6OHNRbmpkVzZwWlk4S1h5YlR4QlBBcmdaclFkeFR1M3NvaWtwSkxRMURSaDN3SVFLQVEifQ"

-- | Its JSON, for reading.
--
-- > {"v":1,"key":"79b5562e8fe654f94078b112e8a98ba7901f853ae695bed7e0e3910bad049664","aud":"ws://127.0.0.1:4460","agent":"worker-3","nonce":"BwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwcHBwc","ts":1791600000,"sig":"JRqHlzTGvXfnno3Ail1hnuOpgbzK9r4VOkIz8sQnjdW6pZY8KXybTxBPArgZrQdxTu3soikpJLQ1DRh3wIQKAQ"}

-- ============================================================================
-- Stores in a sandbox
-- ============================================================================

sandbox :: [(String, String)] -> Maybe Keychain -> FilePath -> StoreEnv
sandbox env kc dir = StoreEnv
  { stEnv = \k -> pure (lookup k env)
  , stPlexus = dir </> ".plexus"
  , stKeychain = kc
  }

seedOf :: Int -> Seed
seedOf n = either (error . T.unpack) id (seedFromBytes (BS.replicate 32 (fromIntegral n)))

mode :: FilePath -> IO Int
mode p = fromIntegral . intersectFileModes 0o777 . fileMode <$> getFileStatus p

main :: IO ()
main = do
  TIO.putStrLn ""
  TIO.putStrLn "IDENTITY-43 fixed vector:"
  TIO.putStrLn ("  seed      0x01..0x20 -> " <> principalOf vectorSeed)
  TIO.putStrLn ("  aud       " <> vectorAud)
  TIO.putStrLn ("  agent     " <> vectorAgent)
  TIO.putStrLn ("  nonce     32 x 0x07 -> " <> b64urlNoPad vectorNonce)
  TIO.putStrLn ("  ts        " <> T.pack (show vectorTs))
  TIO.putStrLn ("  message   " <> T.pack (show (canonicalMessage vectorAud vectorAgent (b64urlNoPad vectorNonce) vectorTs)))
  TIO.putStrLn ("  proof     " <> vectorProof)
  TIO.putStrLn ""
  hspec spec

spec :: Spec
spec = do
  describe "keys" $ do
    it "derives the RFC 8032 public key (test 1) from its seed" $ do
      let Right s = seedFromBytes (hexBytes "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
      publicKeyHex s `shouldBe` "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
      principalOf s `shouldBe` "key:d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"

    it "refuses a seed that is not 32 bytes" $ do
      seedFromBytes (BS.replicate 31 0) `shouldSatisfy` either (const True) (const False)

    it "keeps a seed as base64 and reads it back (and url base64, unpadded)" $ do
      decodeSeedText (encodeSeedText vectorSeed) `shouldBe` Right vectorSeed
      decodeSeedText (b64urlNoPad (seedBytes vectorSeed)) `shouldBe` Right vectorSeed
      decodeSeedText ("  " <> encodeSeedText vectorSeed <> "\n") `shouldBe` Right vectorSeed

    it "admits agent ids of 1-64 of [A-Za-z0-9._-]" $ do
      map validAgent ["worker-3", "a", "A.b_c-9", T.replicate 64 "x"] `shouldBe` [True, True, True, True]
      map validAgent ["", T.replicate 65 "x", "a b", "a/b", "ä", "a:b"] `shouldBe` replicate 6 False

  describe "the canonical message" $ do
    it "is plexus-key-login/v1, aud, agent, nonce, ts — newline-joined, no trailing newline" $
      canonicalMessage "ws://127.0.0.1:4455" "worker-3" "AAAA" 1791600000
        `shouldBe` "plexus-key-login/v1\nws://127.0.0.1:4455\nworker-3\nAAAA\n1791600000"

    it "names the socket dialed as the audience" $
      wsAudience "127.0.0.1" 4455 `shouldBe` "ws://127.0.0.1:4455"

  describe "the proof" $ do
    it "is plexus-key. + base64url-nopad of the JSON, fields in the wire's order" $ do
      let Just body = T.stripPrefix "plexus-key." vectorProof
          Right json = B64U.decodeUnpadded (TE.encodeUtf8 body)
          keys = map (T.takeWhile (/= '"')) (T.splitOn ",\"" (T.drop 2 (TE.decodeUtf8 json)))
      T.any (== '=') body `shouldBe` False
      keys `shouldBe` ["v", "key", "aud", "agent", "nonce", "ts", "sig"]
      let Just (A.Object o) = A.decodeStrict json
      KM.lookup "v" o `shouldBe` Just (A.Number 1)
      KM.lookup "key" o `shouldBe` Just (A.String (publicKeyHex vectorSeed))
      KM.lookup "aud" o `shouldBe` Just (A.String vectorAud)
      KM.lookup "agent" o `shouldBe` Just (A.String vectorAgent)
      KM.lookup "nonce" o `shouldBe` Just (A.String (b64urlNoPad vectorNonce))
      KM.lookup "ts" o `shouldBe` Just (A.Number 1791600000)
      case KM.lookup "sig" o of
        Just (A.String sig) -> fmap BS.length (B64U.decodeUnpadded (TE.encodeUtf8 sig)) `shouldBe` Right 64
        other -> expectationFailure ("sig: " <> show other)

    it "is the pinned string for the fixed vector" $
      vectorProof `shouldBe` expectedProof

    it "changes with every input" $ do
      let other = [ buildProof (seedOf 9) vectorAud vectorAgent vectorNonce vectorTs
                  , buildProof vectorSeed "ws://127.0.0.1:4455" vectorAgent vectorNonce vectorTs
                  , buildProof vectorSeed vectorAud "worker-4" vectorNonce vectorTs
                  , buildProof vectorSeed vectorAud vectorAgent (BS.replicate 32 8) vectorTs
                  , buildProof vectorSeed vectorAud vectorAgent vectorNonce (vectorTs + 1)
                  ]
      filter (== vectorProof) other `shouldBe` []

  describe "the key store" $ do
    it "reads PLEXUS_KEY before the keychain, the credentials directory and the file" $
      withSystemTempDirectory "identity" $ \dir -> do
        (kc, _) <- memoryKeychain
        let st0 = sandbox [("CREDENTIALS_DIRECTORY", dir </> "creds")] (Just kc) dir
        Right _ <- writeKey st0 StoreFile "a" (seedOf 4)
        createDirectoryIfMissing True (dir </> "creds")
        BS.writeFile (dir </> "creds" </> "a.key") (TE.encodeUtf8 (encodeSeedText (seedOf 3)))
        Right KeyFromKeychain <- writeKey st0 StoreKeychain "a" (seedOf 2)
        let withEnv = sandbox [("PLEXUS_KEY", T.unpack (encodeSeedText (seedOf 1))), ("CREDENTIALS_DIRECTORY", dir </> "creds")] (Just kc) dir
        fmap (fmap fst) <$> lookupKey withEnv "a" `shouldReturn` Right (Just (seedOf 1))
        fmap (fmap snd) <$> lookupKey withEnv "a" `shouldReturn` Right (Just KeyFromEnv)
        -- no env: the keychain
        fmap (fmap fst) <$> lookupKey st0 "a" `shouldReturn` Right (Just (seedOf 2))
        -- no keychain: the credentials directory
        let noKc = sandbox [("CREDENTIALS_DIRECTORY", dir </> "creds")] Nothing dir
        fmap (fmap fst) <$> lookupKey noKc "a" `shouldReturn` Right (Just (seedOf 3))
        -- none of those: the file
        let fileOnly = sandbox [] Nothing dir
        fmap (fmap fst) <$> lookupKey fileOnly "a" `shouldReturn` Right (Just (seedOf 4))
        fmap (fmap snd) <$> lookupKey fileOnly "a" `shouldReturn` Right (Just (KeyFromFile (dir </> ".plexus" </> "keys" </> "a.key")))
        lookupKey fileOnly "b" `shouldReturn` Right Nothing

    it "writes to the keychain when it answers, else a 0600 file in a 0700 directory" $
      withSystemTempDirectory "identity" $ \dir -> do
        (kc, ref) <- memoryKeychain
        writeKey (sandbox [] (Just kc) dir) StoreAuto "k" (seedOf 5) `shouldReturn` Right KeyFromKeychain
        Map.keys <$> readIORef ref `shouldReturn` [("plexus-key", [("name", "k")])]
        let p = dir </> ".plexus" </> "keys" </> "f.key"
        writeKey (sandbox [] Nothing dir) StoreAuto "f" (seedOf 6) `shouldReturn` Right (KeyFromFile p)
        mode p `shouldReturn` 0o600
        mode (dir </> ".plexus" </> "keys") `shouldReturn` 0o700
        writeKey (sandbox [] Nothing dir) StoreKeychain "g" (seedOf 6) >>= (`shouldSatisfy` either (const True) (const False))

    it "falls back to the file when the keychain refuses a write" $
      withSystemTempDirectory "identity" $ \dir -> do
        (kc, _) <- memoryKeychain
        let refusing = kc { kcStore = \_ _ _ _ -> pure (Left "locked") }
        writeKey (sandbox [] (Just refusing) dir) StoreAuto "k" (seedOf 5)
          `shouldReturn` Right (KeyFromFile (dir </> ".plexus" </> "keys" </> "k.key"))

    it "says when a name is taken, and lists keys with where they are" $
      withSystemTempDirectory "identity" $ \dir -> do
        (kc, _) <- memoryKeychain
        let st = sandbox [] (Just kc) dir
        keyExists st "a" `shouldReturn` Nothing
        _ <- writeKey st StoreKeychain "a" (seedOf 1)
        _ <- writeKey st StoreFile "b" (seedOf 2)
        keyExists st "a" `shouldReturn` Just KeyFromKeychain
        keyExists st "b" `shouldReturn` Just (KeyFromFile (dir </> ".plexus" </> "keys" </> "b.key"))
        map fst <$> listKeys st `shouldReturn` ["a", "b"]

  describe "the session store" $ do
    let rec = SessionRec
          { srToken = "mgs_abc", srPrincipal = "key:00", srAgent = "worker-3"
          , srExpiresAt = Just 1791643200, srKey = "agent-7", srScope = ["editor@MENGER-12"]
          , srTtl = Nothing, srBackend = "menger" }

    it "reads PLEXUS_SESSION first, then the keychain, then the file" $
      withSystemTempDirectory "identity" $ \dir -> do
        (kc, _) <- memoryKeychain
        let fileSt = sandbox [] Nothing dir
            p = dir </> ".plexus" </> "sessions" </> "127.0.0.1_4455" </> "worker-3.json"
        writeSession fileSt "127.0.0.1" 4455 rec `shouldReturn` SessionFromFile p
        mode p `shouldReturn` 0o600
        lookupSession fileSt "127.0.0.1" 4455 "worker-3" `shouldReturn` Just (Right rec, SessionFromFile p)
        let kcSt = sandbox [] (Just kc) dir
            rec2 = rec { srToken = "mgs_def" }
        writeSession kcSt "127.0.0.1" 4455 rec2 `shouldReturn` SessionFromKeychain
        lookupSession kcSt "127.0.0.1" 4455 "worker-3" `shouldReturn` Just (Right rec2, SessionFromKeychain)
        -- a keychain write drops the stale file
        doesFileExist p `shouldReturn` False
        let envSt = sandbox [("PLEXUS_SESSION", "mgs_env\n")] (Just kc) dir
        lookupSession envSt "127.0.0.1" 4455 "worker-3" `shouldReturn` Just (Left "mgs_env", SessionFromEnv)
        lookupSession kcSt "127.0.0.1" 4455 "worker-4" `shouldReturn` Nothing
        lookupSession kcSt "127.0.0.1" 4456 "worker-3" `shouldReturn` Nothing

    it "keeps the key name, scope and backend a re-login needs" $
      withSystemTempDirectory "identity" $ \dir -> do
        let st = sandbox [] Nothing dir
        _ <- writeSession st "127.0.0.1" 4455 rec
        raw <- BS.readFile (dir </> ".plexus" </> "sessions" </> "127.0.0.1_4455" </> "worker-3.json")
        let Just (A.Object o) = A.decodeStrict raw
        map (`KM.member` o) ["token", "principal", "agent", "expires_at", "key", "scope", "backend"]
          `shouldBe` replicate 7 True

    it "forgets a session from both stores, and the default agent with it" $
      withSystemTempDirectory "identity" $ \dir -> do
        (kc, ref) <- memoryKeychain
        let st = sandbox [] (Just kc) dir
        _ <- writeSession st "127.0.0.1" 4455 rec
        writeDefaultAgent st "127.0.0.1" 4455 "worker-3"
        readDefaultAgent st "127.0.0.1" 4455 `shouldReturn` Just "worker-3"
        removeSession st "127.0.0.1" 4455 "worker-3"
        clearDefaultAgent st "127.0.0.1" 4455 "worker-3"
        Map.null <$> readIORef ref `shouldReturn` True
        lookupSession st "127.0.0.1" 4455 "worker-3" `shouldReturn` Nothing
        readDefaultAgent st "127.0.0.1" 4455 `shouldReturn` Nothing

  describe "parallel writers (SYNAPSE-15)" $ do
    let rec = SessionRec
          { srToken = "mgs_0", srPrincipal = "key:00", srAgent = "worker-3"
          , srExpiresAt = Just 1791643200, srKey = "agent-7", srScope = []
          , srTtl = Nothing, srBackend = "menger" }

    it "32 writers of one session at once all succeed, and leave one session, 0600, and no temporary file" $
      withSystemTempDirectory "identity" $ \dir -> do
        let st = sandbox [] Nothing dir
            sdir = dir </> ".plexus" </> "sessions" </> "127.0.0.1_4455"
        rs <- mapConcurrently (\i -> try (writeSession st "127.0.0.1" 4455 rec { srToken = "mgs_" <> T.pack (show i) }))
                [1 .. 32 :: Int]
        [e | Left (e :: SomeException) <- rs] `shouldSatisfy` null
        Just (Right r, _) <- lookupSession st "127.0.0.1" 4455 "worker-3"
        srToken r `shouldSatisfy` (`elem` ["mgs_" <> T.pack (show i) | i <- [1 .. 32 :: Int]])
        mode (sdir </> "worker-3.json") `shouldReturn` 0o600
        listDirectory sdir `shouldReturn` ["worker-3.json"]

    it "the session lock lets one holder in at a time" $
      withSystemTempDirectory "identity" $ \dir -> do
        let st = sandbox [] Nothing dir
        inside <- newIORef (0 :: Int)
        most <- newIORef (0 :: Int)
        _ <- mapConcurrently (\_ -> withSessionLock st "127.0.0.1" 4455 "worker-3" $ do
               n <- atomicModifyIORef' inside (\k -> (k + 1, k + 1))
               atomicModifyIORef' most (\m -> (max m n, ()))
               threadDelay 20000
               atomicModifyIORef' inside (\k -> (k - 1, ())))
             [1 .. 8 :: Int]
        readIORef most `shouldReturn` 1

  describe "_login's --scope" $
    it "takes the flag repeated or comma-separated" $
      splitScopes ["editor@MENGER-12,viewer@PLEXUS", " admin@* "] `shouldBe` ["editor@MENGER-12", "viewer@PLEXUS", "admin@*"]

hexBytes :: Text -> BS.ByteString
hexBytes = BS.pack . go . T.unpack
  where
    go (a : b : rest) = fromIntegral (read ['0', 'x', a, b] :: Int) : go rest
    go _ = []
