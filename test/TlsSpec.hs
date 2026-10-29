{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- | SCAFFOLD-21: synapse speaks wss. A TLS terminator of the test's own —
-- a CA and a certificate for @localhost@ made with openssl for this run,
-- in front of the mock service, as the edge sits in front of a hosted app —
-- and against it: the probe, a call and a key login over wss at a path,
-- with the request's path and Host as the edge would see them; a server
-- the trust store does not vouch for, and a certificate for another name,
-- refused at the handshake before a byte of the request is sent.
module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (SomeException, bracket, finally, try)
import Control.Monad (forever, void)
import Data.Aeson (Value(..), decodeStrict, object)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Text as T
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import qualified Network.TLS as TLS
import System.Directory (findExecutable)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec

import qualified Katip
import MockService (todaysService, withMockService)
import Synapse.Backend.Discovery (probeBackendAtEp)
import Synapse.Endpoint (Endpoint(..))
import qualified Synapse.Identity.Proof as Proof
import qualified Synapse.Identity.Session as Session
import qualified Synapse.Log as Log
import Synapse.Monad (initEnvAt, runSynapseM)
import Synapse.Schema.Types (pattern ServiceData)
import Synapse.Transport (invoke, invokeStreamingWithBidir)
import Synapse.Reconnect (CallShape(..))
import Plexus.Types (Response(..))

main :: IO ()
main = do
  openssl <- findExecutable "openssl"
  hspec $ case openssl of
    Nothing -> it "needs openssl to make a CA for the run" $ pendingWith "no openssl on PATH"
    Just _ -> around withPki specs

-- | The run's PKI: a CA, and a certificate for @localhost@ it signed.
data Pki = Pki { pkiCa :: FilePath, pkiCert :: FilePath, pkiKey :: FilePath, pkiOtherCa :: FilePath }

withPki :: (Pki -> IO ()) -> IO ()
withPki act = withSystemTempDirectory "synapse-tls" $ \dir -> do
  let f = (dir </>)
      run args = do
        (code, _, err) <- readProcessWithExitCode "openssl" args ""
        case code of
          ExitSuccess -> pure ()
          _ -> fail ("openssl " <> unwords args <> ": " <> err)
      ca name = run ["req", "-x509", "-newkey", "rsa:2048", "-nodes", "-days", "1"
                    , "-keyout", f (name <> ".key"), "-out", f (name <> ".pem")
                    , "-subj", "/CN=" <> name
                    , "-addext", "basicConstraints=critical,CA:TRUE"
                    , "-addext", "keyUsage=critical,keyCertSign,cRLSign" ]
  ca "synapse-test-ca"
  ca "another-ca"
  writeFile (f "ext.cnf") "subjectAltName=DNS:localhost\nbasicConstraints=CA:FALSE\nextendedKeyUsage=serverAuth\nkeyUsage=digitalSignature,keyEncipherment\n"
  run ["req", "-newkey", "rsa:2048", "-nodes", "-keyout", f "srv.key", "-out", f "srv.csr", "-subj", "/CN=localhost"]
  run ["x509", "-req", "-in", f "srv.csr", "-CA", f "synapse-test-ca.pem", "-CAkey", f "synapse-test-ca.key"
      , "-CAcreateserial", "-days", "1", "-out", f "srv.pem", "-extfile", f "ext.cnf"]
  act (Pki (f "synapse-test-ca.pem") (f "srv.pem") (f "srv.key") (f "another-ca.pem"))

-- | What the terminator saw: each connection's request head (the bytes
-- before the first blank line), once its handshake completed.
type Heads = IORef [BS.ByteString]

-- | A TLS terminator on 127.0.0.1 (a port the OS picks) forwarding each
-- connection to the plain service at @backend@, as the edge does.
withTerminator :: Pki -> Int -> (Int -> Heads -> IO a) -> IO a
withTerminator pki backend act = do
  cred <- TLS.credentialLoadX509 (pkiCert pki) (pkiKey pki) >>= either fail pure
  heads <- newIORef []
  let params = TLS.defaultParamsServer
        { TLS.serverShared = (TLS.serverShared TLS.defaultParamsServer)
            { TLS.sharedCredentials = TLS.Credentials [cred] } }
      open = do
        sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
        N.setSocketOption sock N.ReuseAddr 1
        N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
        N.listen sock 16
        port <- fromIntegral <$> N.socketPort sock
        tid <- forkIO (forever (N.accept sock >>= void . forkIO . serve params heads . fst))
        pure (sock, tid, port)
      close (sock, tid, _) = killThread tid >> N.close sock
  bracket open close (\(_, _, port) -> act port heads)
  where
    serve params heads conn = flip finally (N.close conn) $ void $ try @SomeException $ do
      ctx <- TLS.contextNew conn params
      TLS.handshake ctx
      up <- N.socket N.AF_INET N.Stream N.defaultProtocol
      flip finally (N.close up) $ do
        N.connect up (N.SockAddrInet (fromIntegral backend) (N.tupleToHostAddress (127, 0, 0, 1)))
        seen <- newIORef False
        let down = forever $ do
              b <- NB.recv up 65536
              if BS.null b then fail "service closed" else TLS.sendData ctx (LBS.fromStrict b)
        t <- forkIO (void (try @SomeException down) >> TLS.bye ctx)
        flip finally (killThread t) $ forever $ do
          b <- TLS.recvData ctx
          if BS.null b then fail "client closed" else do
            first <- atomicModifyIORef' seen (\s -> (True, not s))
            if first then modifyIORef' heads (++ [fst (BS.breakSubstring "\r\n\r\n" b)]) else pure ()
            NB.sendAll up b

logger :: IO Log.Logger
logger = Log.makeLogger Katip.ErrorS

specs :: SpecWith Pki
specs = do
  describe "wss through a TLS terminator, the CA given with --cacert" $ do
    it "answers the _info probe, dialing the path with the Host the certificate names" $ \pki ->
      withMockService todaysService $ \service -> withTerminator pki service $ \port heads -> do
        let ep = Endpoint "localhost" port True "/ws" [pkiCa pki]
        probeBackendAtEp [] ep `shouldReturn` Right "menger"
        hs <- readIORef heads
        h <- case hs of
          (h : _) -> pure h
          [] -> expectationFailure "the terminator saw no request" >> pure ""
        BS8.unpack h `shouldStartWith` "GET /ws HTTP/1.1"
        BS8.lines h `shouldSatisfy` any (== BS8.pack ("Host: localhost:" <> show port <> "\r"))

    it "carries a call and its answer" $ \pki ->
      withMockService todaysService $ \service -> withTerminator pki service $ \port _ -> do
        lg <- logger
        env <- initEnvAt (Endpoint "localhost" port True "/ws" [pkiCa pki]) "menger" lg Nothing
        r <- runSynapseM env (invoke ["tickets"] "get" (object [("id", String "MENGER-12")]))
        case r of
          Right items -> [v | ServiceData _ _ _ (Object v) <- items, KM.lookup "id" v == Just (String "MENGER-12")]
                           `shouldSatisfy` (not . null)
          Left e -> expectationFailure (show e)

    it "logs in from a key, the proof's audience the server's wss origin" $ \pki ->
      withMockService todaysService $ \service -> withTerminator pki service $ \port heads -> do
        lg <- logger
        let ep = Endpoint "localhost" port True "/ws" [pkiCa pki]
            seed = either (error . T.unpack) id (Proof.seedFromBytes (BS.pack [1 .. 32]))
        r <- Session.loginWithKey lg ep "menger" seed "worker-1" [] Nothing
        fmap Session.lrToken r `shouldBe` Right "mgs_agent_session"
        hs <- readIORef heads
        let proofs = [ p | h <- hs, l <- BS8.lines h
                         , Just rest <- [BS.stripPrefix "Authorization: Bearer plexus-key." l]
                         , Just p <- [decodeProof (BS8.takeWhile (/= '\r') rest)] ]
        proofs `shouldSatisfy` (not . null)
        mapM_ (\p -> KM.lookup "aud" p `shouldBe` Just (String ("wss://localhost:" <> T.pack (show port)))) proofs

    it "carries a bidirectional request and its answer (respond goes over wss too)" $ \pki ->
      withMockService todaysService $ \service -> withTerminator pki service $ \port heads -> do
        lg <- logger
        env <- initEnvAt (Endpoint "localhost" port True "/ws" [pkiCa pki]) "menger" lg Nothing
        seen <- newIORef []
        asked <- newIORef []
        r <- runSynapseM env $ invokeStreamingWithBidir
          (CallShape { csStream = False, csRead = False, csSince = False })
          (pure Nothing) (const (pure ())) ["tickets"] "ask" (object [])
          (\i -> modifyIORef' seen (++ [i]))
          (\rid _ -> modifyIORef' asked (++ [rid]) >> pure (Just (Confirmed True :: Response Value)))
        r `shouldSatisfy` either (const False) (const True)
        readIORef asked `shouldReturn` ["req-1"]
        items <- readIORef seen
        [v | ServiceData _ _ _ (Object v) <- items, KM.lookup "type" v == Just (String "answered")]
          `shouldBe` [KM.fromList [("type", String "answered"), ("response", object [("type", String "confirmed"), ("value", Bool True)])]]
        -- the call and the respond: two upgrades, both through TLS at the path
        hs <- readIORef heads
        length (filter (BS.isPrefixOf "GET /ws HTTP/1.1") hs) `shouldSatisfy` (>= 2)

  describe "refusing what it cannot verify" $ do
    it "refuses a certificate the trust store does not vouch for — nothing is sent" $ \pki ->
      withMockService todaysService $ \service -> withTerminator pki service $ \port heads -> do
        r <- probeBackendAtEp [] (Endpoint "localhost" port True "/ws" [pkiOtherCa pki])
        r `shouldSatisfy` either (const True) (const False)
        readIORef heads `shouldReturn` []

    it "refuses a certificate for another name (127.0.0.1 is not localhost)" $ \pki ->
      withMockService todaysService $ \service -> withTerminator pki service $ \port heads -> do
        r <- probeBackendAtEp [] (Endpoint "127.0.0.1" port True "/ws" [pkiCa pki])
        r `shouldSatisfy` either (const True) (const False)
        readIORef heads `shouldReturn` []

    it "refuses a CA file that holds no certificate" $ \pki ->
      withMockService todaysService $ \service -> withTerminator pki service $ \port _ -> do
        r <- probeBackendAtEp [] (Endpoint "localhost" port True "/ws" [pkiKey pki])
        either T.unpack (const "") r `shouldContain` "holds no PEM certificate"

-- | A key proof's JSON (@plexus-key.<base64url>@ without its prefix).
decodeProof :: BS.ByteString -> Maybe (KM.KeyMap Value)
decodeProof b64 = case B64U.decodeUnpadded b64 of
  Right json | Just (Object o) <- decodeStrict json -> Just o
  _ -> Nothing
