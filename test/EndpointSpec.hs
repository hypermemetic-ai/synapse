{-# LANGUAGE OverloadedStrings #-}

-- | SCAFFOLD-21: where a service is reached — @--url@ and @-H@ / @-P@ with
-- @--tls@ / @--path@ read into one 'Endpoint', and every key derived from
-- it: the session store's (a plain address keyed exactly as before), the
-- key proof's audience, the options a message tells the reader to type.
module Main (main) where

import Data.Either (isLeft)
import qualified Data.Text as T
import Test.Hspec

import Plexus.Client (SubstrateConfig(..), TlsOptions(..), hostHeader)
import Synapse.Endpoint

main :: IO ()
main = hspec $ do
  describe "parseUrl" $ do
    it "reads a hosted app's socket: wss, the default port, the path" $
      parseUrl "wss://hostdemo.menger.sh/ws" `shouldBe`
        Right (Endpoint "hostdemo.menger.sh" 443 True "/ws" [])
    it "reads ws with a port and no path as /" $
      parseUrl "ws://127.0.0.1:4455" `shouldBe` Right (plainAt "127.0.0.1" 4455)
    it "defaults ws to 80 and keeps an explicit wss port" $ do
      fmap epPort (parseUrl "ws://example.org/ws") `shouldBe` Right 80
      fmap epPort (parseUrl "wss://example.org:8443/ws") `shouldBe` Right 8443
    it "lowercases the host, trims a trailing slash, takes WSS in any case" $
      parseUrl "WSS://App.Example/ws/" `shouldBe` Right (Endpoint "app.example" 443 True "/ws" [])
    it "reads a bracketed IPv6 literal" $
      parseUrl "wss://[::1]:8443/ws" `shouldBe` Right (Endpoint "::1" 8443 True "/ws" [])
    it "refuses http(s), a missing scheme, a query, a credential, a bad port" $ do
      parseUrl "https://hostdemo.menger.sh/ws" `shouldSatisfy` isLeft
      parseUrl "http://hostdemo.menger.sh" `shouldSatisfy` isLeft
      parseUrl "hostdemo.menger.sh/ws" `shouldSatisfy` isLeft
      parseUrl "wss://h/ws?x=1" `shouldSatisfy` isLeft
      parseUrl "wss://u:p@h/ws" `shouldSatisfy` isLeft
      parseUrl "wss://h:99999/ws" `shouldSatisfy` isLeft
      parseUrl "wss://h:/ws" `shouldSatisfy` isLeft
      parseUrl "wss:///ws" `shouldSatisfy` isLeft
    it "says what https:// would have meant" $
      parseUrl "https://app.example/ws" `shouldBe`
        Left "--url takes ws:// or wss://, not https:// (the socket of an https page is wss://app.example/ws)"

  describe "normalisePath" $
    it "makes /PATH of any spelling" $ do
      map normalisePath ["", "/", "ws", "/ws", "/ws/", "//a/b//"] `shouldBe` ["/", "/", "/ws", "/ws", "/ws", "/a/b"]

  describe "the session store's keys" $ do
    it "keeps a plain address's key and directory exactly as before (no stored session moves)" $ do
      addressKey (plainAt "127.0.0.1" 4455) `shouldBe` "127.0.0.1:4455"
      dirName (plainAt "127.0.0.1" 4455) `shouldBe` "127.0.0.1_4455"
    it "keys a wss address by its URL, port and path included" $ do
      let ep = Endpoint "hostdemo.menger.sh" 443 True "/ws" []
      addressKey ep `shouldBe` "wss://hostdemo.menger.sh:443/ws"
      dirName ep `shouldBe` "wss_hostdemo.menger.sh_443_ws"
    it "never gives a ws and a wss backend on one host, or two paths, one key" $ do
      let eps =
            [ plainAt "h" 443
            , Endpoint "h" 443 True "/" []
            , Endpoint "h" 443 True "/ws" []
            , Endpoint "h" 443 True "/ws/a" []
            , Endpoint "h" 443 False "/ws" []
            , Endpoint "h" 443 True "/ws_a" []
            ]
          distinct xs = length xs == length (foldr (\x acc -> if x `elem` acc then acc else x : acc) [] xs)
      map addressKey eps `shouldSatisfy` distinct
      map dirName eps `shouldSatisfy` distinct
    it "writes anything outside [A-Za-z0-9.-] in a path as %XX, never a separator" $ do
      dirName (Endpoint "h" 443 True "/a b/ü" []) `shouldBe` "wss_h_443_a%20b_%c3%bc"
      dirName (Endpoint "h" 443 True "/../x" []) `shouldBe` "wss_h_443_.._x"
      dirName (Endpoint "h" 443 True "/../x" []) `shouldNotSatisfy` ('/' `elem`)
    it "ignores the CA files (they say how to trust the server, not which it is)" $
      addressKey (Endpoint "h" 443 True "/ws" ["/tmp/ca.pem"]) `shouldBe` addressKey (Endpoint "h" 443 True "/ws" [])

  describe "audience (a key proof's aud)" $ do
    it "is ws://HOST:PORT for a plain address, as before" $
      audience (plainAt "127.0.0.1" 4455) `shouldBe` "ws://127.0.0.1:4455"
    it "is the wss origin a service run with --public-url https://HOST accepts — no path, no :443" $ do
      audience (Endpoint "hostdemo.menger.sh" 443 True "/ws" []) `shouldBe` "wss://hostdemo.menger.sh"
      audience (Endpoint "localhost" 8443 True "/ws" []) `shouldBe` "wss://localhost:8443"

  describe "cliOptions (what a message tells the reader to type)" $ do
    it "says nothing for the default address, -P / -H for a plain one" $ do
      cliOptions (plainAt "127.0.0.1" 4444) `shouldBe` ""
      cliOptions (plainAt "127.0.0.1" 4455) `shouldBe` "-P 4455"
      cliOptions (plainAt "box" 4455) `shouldBe` "-H box -P 4455"
    it "says --url, in its short form, for the rest (and each --cacert)" $ do
      cliOptions (Endpoint "hostdemo.menger.sh" 443 True "/ws" []) `shouldBe` "--url wss://hostdemo.menger.sh/ws"
      cliOptions (Endpoint "localhost" 8443 True "/ws" ["ca.pem"]) `shouldBe` "--url wss://localhost:8443/ws --cacert ca.pem"
      cliOptions (Endpoint "h" 8080 False "/ws" []) `shouldBe` "--url ws://h:8080/ws"
    it "round-trips through parseUrl" $
      mapM_ (\ep -> case T.words (cliOptions ep) of
                      ("--url" : u : _) -> parseUrl u `shouldBe` Right ep
                      other -> expectationFailure ("no --url in " <> show other))
        [ Endpoint "hostdemo.menger.sh" 443 True "/ws" []
        , Endpoint "localhost" 8443 True "/" []
        , Endpoint "h" 8080 False "/ws" [] ]

  describe "substrateConfig" $ do
    it "dials plain ws with no TLS for a plain address" $ do
      let c = substrateConfig (plainAt "127.0.0.1" 4455) "menger" []
      (substrateHost c, substratePort c, substratePath c, substrateTls c) `shouldBe` ("127.0.0.1", 4455, "/", Nothing)
    it "dials wss at the path, the CA files carried, the Host header without :443" $ do
      let c = substrateConfig (Endpoint "hostdemo.menger.sh" 443 True "/ws" ["ca.pem"]) "hostdemo" []
      (substratePath c, substrateTls c) `shouldBe` ("/ws", Just (TlsOptions ["ca.pem"]))
      hostHeader c `shouldBe` "hostdemo.menger.sh"
      hostHeader c { substratePort = 8443 } `shouldBe` "hostdemo.menger.sh:8443"
