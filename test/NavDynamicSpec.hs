{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | NEST-21: synapse steps into a dynamic child (NEST-5's slot, plexus-core's
-- @config.{key}@): a segment that is neither a child nor a method of a
-- schema with a @dynamic_child@ slot is asked of the service as the slot's
-- instance (@config.transport.schema@) and navigated into —
-- @synapse … config transport insecure_ws get@. A name the service does not
-- answer is still @Command not found@.
--
-- SYNAPSE-41: a peer slot's names too (NEST-7, plexus-dyn's @dyn.peer@), and
-- a refusal or an outage while asking for a name is that failure, never
-- @Command not found@.
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_)
import Data.Aeson
import Data.IORef
import Data.List (isInfixOf)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Test.Hspec

import MockService

main :: IO ()
main = hspec $ do
  synapse <- runIO (findExecutable "synapse")
  case synapse of
    Nothing -> it "(the synapse executable is not on PATH)" $ pendingWith "cabal test puts it there"
    Just exe -> navSpec exe

-- | A root with @config@; @config@ a slot @key@ of nodes, each a slot again;
-- @config.transport@ a group, @config.transport.insecure_ws@ a key.
configSchemas :: [(Text, Value)]
configSchemas =
  [ ("", object
      [ "namespace" .= ("menger" :: Text), "version" .= ("1.0.0" :: Text), "description" .= ("Central routing" :: Text)
      , "hash" .= ("r-config" :: Text)
      , "children" .=
          [ object ["namespace" .= ("config" :: Text), "description" .= ("Settings" :: Text), "hash" .= ("c0" :: Text)]
          , object ["namespace" .= ("dyn" :: Text), "description" .= ("Runtime peers" :: Text), "hash" .= ("d0" :: Text)]
          , object ["namespace" .= ("slot" :: Text), "description" .= ("Peers, unlisted" :: Text), "hash" .= ("s0" :: Text)] ] ])
  , ("config", node "config" "c0")
  , ("config.transport", node "config_key" "k0")
  , ("config.transport.insecure_ws", node "config_key" "k0")
    -- SYNAPSE-41: a key the caller may not read: a refusal, not "not found"
  , ("config.secret", String "forbidden")
  ] ++ peerSchemas
  where
    node :: Text -> Text -> Value
    node ns h = object
      [ "namespace" .= ns, "version" .= ("0.11.0" :: Text), "hash" .= h, "description" .= ("A setting" :: Text)
      , "methods" .=
          [ object [ "name" .= ("get" :: Text), "description" .= ("The setting as it stands" :: Text), "hash" .= ("g0" :: Text), "returns" .= object [] ]
          , object [ "name" .= ("names" :: Text), "description" .= ("The names below" :: Text), "hash" .= ("n0" :: Text), "returns" .= object [] ]
          , object [ "name" .= ("key" :: Text), "description" .= ("A setting, or a group of them" :: Text), "hash" .= ("y0" :: Text), "returns" .= object []
                   , "role" .= object ["kind" .= ("dynamic_child" :: Text), "list_method" .= ("names" :: Text)] ]
          ] ]

-- | SYNAPSE-41: peer slots (NEST-7) as plexus-dyn serves them. @dyn@ lists
-- its slot @peer@ as a child and as a method marked @peer@; @dyn.peer@
-- lists the peers it has (@far@) under an untyped dynamic child. A peer is a
-- service of its own: @far@'s @dyn@ has the host's @dyn@'s hash, which is no
-- cycle. @slot@ is a peer slot that lists nothing: its names are asked.
peerSchemas :: [(Text, Value)]
peerSchemas =
  [ ("dyn", act "dyn" "d0" [rpc "names" "The peers' names", peerSlot "peer"]
      [child "peer" "A peer by name" "p0"])
  , ("dyn.peer", act "peer" "p0" [slotMethod "peer"] [child "far" "the peer `far`" "f0"])
  , ("dyn.peer.far", act "far" "f0" [rpc "schema" "Get plugin or method schema"] [child "dyn" "Runtime peers" "d0"])
  , ("dyn.peer.far.dyn", act "dyn" "d0" [rpc "names" "The far side's peers' names", peerSlot "peer"] [])
  , ("dyn.peer.ghost", String "not_found")
  , ("slot", act "slot" "s0" [rpc "names" "The peers' names", peerSlot "peer"] [])
  , ("slot.peer.far", act "far" "f0" [rpc "ping" "Answer pong"] [])
  , ("slot.peer.ghost", String "not_found")
  , ("slot.peer.locked", String "forbidden")
  ]
  where
    act :: Text -> Text -> [Value] -> [Value] -> Value
    act ns h ms cs = object $
      [ "namespace" .= ns, "version" .= ("0.1.0" :: Text), "hash" .= h, "description" .= ns, "methods" .= ms ]
      ++ [ "children" .= cs | not (null cs) ]
    child :: Text -> Text -> Text -> Value
    child ns d h = object ["namespace" .= ns, "description" .= d, "hash" .= h]
    rpc :: Text -> Text -> Value
    rpc n d = object [ "name" .= n, "description" .= d, "hash" .= (n <> "0"), "returns" .= object [] ]
    peerSlot :: Text -> Value
    peerSlot n = object
      [ "name" .= n, "description" .= ("A peer, by name" :: Text), "hash" .= (n <> "1"), "returns" .= object []
      , "role" .= object ["kind" .= ("static_child" :: Text), "peer" .= True, "list_method" .= ("names" :: Text)] ]
    slotMethod :: Text -> Value
    slotMethod n = object
      [ "name" .= n, "description" .= ("A peer, by name" :: Text), "hash" .= (n <> "2"), "returns" .= object []
      , "role" .= object ["kind" .= ("dynamic_child" :: Text)] ]

navSpec :: FilePath -> Spec
navSpec exe = describe "a dynamic child's names (NEST-21)" $ do
  it "steps into config transport insecure_ws" $ withRun $ \run -> do
    (code, out, err) <- run ["config", "transport", "insecure_ws", "get", "--help"]
    (code, err) `shouldBe` (ExitSuccess, "")
    out `shouldSatisfy` ("The setting as it stands" `isInfixOf`)
  it "lists a node's methods" $ withRun $ \run -> do
    (code, out, _) <- run ["config", "transport"]
    code `shouldBe` ExitSuccess
    out `shouldSatisfy` ("get" `isInfixOf`)
  it "still says Command not found for a name the service does not answer" $ withRun $ \run -> do
    (code, _, err) <- run ["config", "nothing", "get"]
    code `shouldBe` ExitFailure 1
    err `shouldSatisfy` ("Command not found: 'nothing'" `isInfixOf`)
  it "says a refusal of a key as the refusal, never as Command not found" $ withRun $ \run -> do
    (code, _, err) <- run ["config", "secret", "get"]
    code `shouldNotBe` ExitSuccess
    err `shouldNotSatisfy` ("Command not found" `isInfixOf`)
    err `shouldSatisfy` ("config.secret.schema" `isInfixOf`)

  describe "a peer slot's names (SYNAPSE-41)" $ do
    it "steps through a listed peer into its own dyn, the same hash as the host's (no cycle)" $ withRun $ \run -> do
      (code, out, err) <- run ["dyn", "peer", "far", "dyn", "names", "--help"]
      (code, err) `shouldBe` (ExitSuccess, "")
      out `shouldSatisfy` ("The far side's peers' names" `isInfixOf`)
    it "lists a peer's own commands" $ withRun $ \run -> do
      (code, out, _) <- run ["dyn", "peer", "far"]
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("dyn" `isInfixOf`)
    it "says Command not found for a peer the slot does not have" $ withRun $ \run -> do
      (code, _, err) <- run ["dyn", "peer", "ghost", "dyn", "names"]
      code `shouldBe` ExitFailure 1
      err `shouldSatisfy` ("Command not found: 'ghost'" `isInfixOf`)
    it "asks an unlisted peer's name of the slot, after the slot method" $ withRun $ \run -> do
      (code, out, err) <- run ["slot", "peer", "far", "ping", "--help"]
      (code, err) `shouldBe` (ExitSuccess, "")
      out `shouldSatisfy` ("Answer pong" `isInfixOf`)
    it "says Command not found for an unlisted name the slot does not have" $ withRun $ \run -> do
      (code, _, err) <- run ["slot", "peer", "ghost", "ping"]
      code `shouldBe` ExitFailure 1
      err `shouldSatisfy` ("Command not found: 'ghost'" `isInfixOf`)
    it "says a refused peer as the refusal" $ withRun $ \run -> do
      (code, _, err) <- run ["slot", "peer", "locked", "ping"]
      code `shouldNotBe` ExitSuccess
      err `shouldNotSatisfy` ("Command not found" `isInfixOf`)
    it "still shows the slot method itself" $ withRun $ \run -> do
      (code, out, _) <- run ["slot", "peer", "--help"]
      code `shouldBe` ExitSuccess
      out `shouldSatisfy` ("A peer, by name" `isInfixOf`)
  where
    withRun act = withSystemTempDirectory "nest21" $ \dir -> withMockService todaysService $ \port -> do
      forM_ ["home", "cache", "run"] $ \d -> createDirectoryIfMissing True (dir </> d)
      env0 <- getEnvironment
      let env = [ (k, v) | (k, v) <- env0, k `notElem` ["HOME", "XDG_CACHE_HOME", "XDG_RUNTIME_DIR", "SYNAPSE_AS", "PLEXUS_SESSION"] ]
              ++ [ ("HOME", dir </> "home"), ("XDG_CACHE_HOME", dir </> "cache"), ("XDG_RUNTIME_DIR", dir </> "run")
                 , ("PLEXUS_KEYCHAIN", "off"), ("SYNAPSE_NO_HOLD", "1") ]
          run args = readCreateProcessWithExitCode
            ((proc exe (["--url", "ws://127.0.0.1:" <> show port <> "/ws", "menger"] ++ args)) { env = Just env }) ""
      writeIORef schemaOverride (Just configSchemas)
      r <- try (act run)
      writeIORef schemaOverride Nothing
      either (\(e :: SomeException) -> ioError (userError (show e))) pure r
