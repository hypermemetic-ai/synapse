{-# LANGUAGE OverloadedStrings #-}

-- | Command dispatcher for _self meta-commands
--
-- _self commands are client-side only and don't make RPC calls.
-- They operate on cached schemas and provide meta-functionality
-- like template generation, schema exploration, etc.
module Synapse.Self.Commands
  ( dispatch
  , showHelp
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Synapse.Monad
import Synapse.Backend.Discovery (getBackendAt)
import qualified Synapse.Self.Template as Template
import qualified Synapse.Self.Debugger as Debugger
import Plexus.Client (SubstrateConfig(..))

-- | Dispatch to a specific _self subcommand
dispatch :: Text -> [Text] -> [(Text, Text)] -> SynapseM ()
dispatch "template" rest params = Template.handleTemplate rest params
dispatch "scan" rest _ = scanPorts rest
dispatch "debug" (hostText:portText:backendText:_) _ = do
  let host = T.unpack hostText
  let port = read (T.unpack portText) :: Int
  liftIO $ Debugger.debugConnection host port backendText
dispatch "debug" _ _ =
  throwParse "Usage: synapse _self debug <host> <port> <backend>"
dispatch "validate" (hostText:portText:backendText:_) _ = do
  let host = T.unpack hostText
  let port = read (T.unpack portText) :: Int
  liftIO $ Debugger.validateProtocol host port backendText
dispatch "validate" _ _ =
  throwParse "Usage: synapse _self validate <host> <port> <backend>"
dispatch "test" rest params = do
  -- Extract flags from params
  let allowUnknown = lookup "allow_unknown" params == Just ""
  let rawJson = lookup "raw" params

  -- Remove flags from params (keep only method params)
  let methodParams = filter (\(k, _) -> k /= "allow_unknown" && k /= "raw") params

  -- Get connection settings from environment (set by -H/-P flags)
  host <- asks seHost
  port <- asks sePort
  backend <- asks seBackend

  case rest of
    methodParts | not (null methodParts) -> do
      let method = T.intercalate "." methodParts
      let config = SubstrateConfig
            { substrateHost = T.unpack host
            , substratePort = port
            , substratePath = "/"
            , substrateBackend = backend
            }
      liftIO $ Debugger.testMethod config method methodParams allowUnknown rawJson
    _ ->
      throwParse "Usage: synapse [options] _self test [--allow-unknown] [--raw <json>] <method> [--param value ...]\n  Note: Use -H/--host and -P/--port flags to set connection (default: 127.0.0.1:4444)"
dispatch "--help" _ _ = showHelp
dispatch "-h" _ _ = showHelp
dispatch cmd _ _ =
  throwParse $ "Unknown _self command: " <> cmd <> "\n\n" <> helpText

-- | Scan ports for backends
-- Default range: 4440-4459 (covers 444x range)
scanPorts :: [Text] -> SynapseM ()
scanPorts _ = do
  host <- asks seHost
  liftIO $ TIO.putStrLn $ "Scanning " <> host <> " ports 4440-4459 for backends...\n"

  -- Scan ports in parallel
  let ports = [4440..4459]
  results <- liftIO $ mapConcurrently (probePort host) ports

  -- Display results
  let found = filter (\(_, mb) -> mb /= Nothing) (zip ports results)
  if null found
    then liftIO $ TIO.putStrLn "No backends found"
    else do
      liftIO $ TIO.putStrLn $ "Found " <> T.pack (show (length found)) <> " backend(s):\n"
      liftIO $ mapM_ printResult found
  where
    probePort :: Text -> Int -> IO (Maybe Text)
    probePort host port =
      getBackendAt host port `catch` \(_ :: SomeException) -> pure Nothing

    printResult :: (Int, Maybe Text) -> IO ()
    printResult (port, Just name) =
      TIO.putStrLn $ "  " <> T.pack (show port) <> "  " <> name
    printResult _ = pure ()

-- | Show help for _self commands
showHelp :: SynapseM ()
showHelp = liftIO $ TIO.putStr helpText

-- | Help text for _self commands
helpText :: Text
helpText = T.unlines
  [ "Meta-commands (local, no RPC):"
  , ""
  , "Available commands:"
  , ""
  , "  synapse _self scan"
  , "      Scan ports 4440-4459 for backends (calls _info on each)"
  , ""
  , "  synapse _self debug <host> <port> <backend>"
  , "      Debug WebSocket connection and protocol issues"
  , "      Tests: TCP, HTTP, WebSocket handshake, Plexus RPC"
  , ""
  , "  synapse _self validate <host> <port> <backend>"
  , "      Run protocol compliance validation tests"
  , "      Tests: protocol_test, stream_test, error_test, metadata_test"
  , "      Exits with code 0 on success, 1 on failures"
  , ""
  , "  synapse [-H <host>] [-P <port>] _self test <method> [--param value ...]"
  , "      Test arbitrary method with protocol validation"
  , "      Connection: Uses -H/--host and -P/--port flags (defaults: 127.0.0.1:4444)"
  , "      Modes:"
  , "        (default)       - Validate params against schema, reject unknown"
  , "        --allow-unknown - Warn about unknown params but pass through"
  , "        --raw <json>    - Use raw JSON params, skip schema validation"
  , "      Validates: StreamDone sent, metadata structure, field naming"
  , ""
  , "  synapse _self template"
  , "      Manage Mustache templates (CRUD operations)"
  , ""
  , "      Subcommands:"
  , "        list [pattern]      - List existing templates"
  , "        show <method>       - Display template content"
  , "        generate [pattern]  - Generate new templates from IR"
  , "        delete <pattern>    - Delete templates matching pattern"
  , "        reload              - Clear template cache"
  , ""
  , "      Pattern format: backend.namespace.method"
  , "      Examples: 'plexus.cone.*', 'plexus.*.create'"
  , ""
  , "Examples:"
  , "  synapse _self scan"
  , "  synapse _self debug 127.0.0.1 4444 substrate"
  , "  synapse _self validate localhost 5001 substrate"
  , "  synapse _self test localhost 5001 substrate echo.echo --message \"hello\""
  , "  synapse _self test --allow-unknown localhost 5001 substrate echo.echo --fake \"test\""
  , "  synapse _self template list"
  , "  synapse _self template show cone.chat"
  , "  synapse _self template generate 'plexus.cone.*'"
  , "  synapse _self template delete 'plexus.cone.test'"
  , ""
  , "Templates location: ~/.config/synapse/templates/"
  , ""
  , "Use 'synapse _self template' for detailed template help"
  , ""
  ]
