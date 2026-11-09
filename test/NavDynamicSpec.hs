{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | NEST-21: synapse steps into a dynamic child (NEST-5's slot, plexus-core's
-- @config.{key}@): a segment that is neither a child nor a method of a
-- schema with a @dynamic_child@ slot is asked of the service as the slot's
-- instance (@config.transport.schema@) and navigated into —
-- @synapse … config transport insecure_ws get@. A name the service does not
-- answer is still @Command not found@.
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
      , "children" .= [ object ["namespace" .= ("config" :: Text), "description" .= ("Settings" :: Text), "hash" .= ("c0" :: Text)] ] ])
  , ("config", node "config" "c0")
  , ("config.transport", node "config_key" "k0")
  , ("config.transport.insecure_ws", node "config_key" "k0")
  ]
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
