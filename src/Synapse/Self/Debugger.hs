{-# LANGUAGE OverloadedStrings #-}

-- | Protocol-level debugging tools for synapse
--
-- Use: synapse _self debug <host> <port> <backend>
--
-- This module provides tools to debug WebSocket connection issues,
-- protocol mismatches, and transport-level problems.
module Synapse.Self.Debugger
  ( debugConnection
  , validateProtocol
  , testMethod
  ) where

import Control.Exception (SomeException, catch, try)
import Control.Monad (unless, forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), object, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (partition)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.WebSockets as WS
import Plexus.Client (SubstrateConfig(..))
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout, stderr, hPutStrLn)
import System.Timeout (timeout)

import Synapse.CLI.Parse (parseParams)
import Synapse.IR.Builder (buildIR)
import Synapse.IR.Types (irMethods, MethodDef, mdParams, pdName)
import Synapse.Log (Logger, makeLogger)
import qualified Katip
import Synapse.Monad (SynapseEnv(..), initEnv, runSynapseM)
import qualified Synapse.Self.Protocol.TestRunner as TestRunner

-- | Debug connection to a Plexus backend
--
-- Tests:
-- 1. TCP connection
-- 2. HTTP upgrade request
-- 3. WebSocket handshake
-- 4. Protocol version compatibility
debugConnection :: String -> Int -> Text -> IO ()
debugConnection host port backend = do
  putStrLn $ "Debugging connection to " <> host <> ":" <> show port
  putStrLn $ "Backend: " <> T.unpack backend
  putStrLn ""

  -- Step 1: TCP connection
  putStr "[1/4] Testing TCP connection... "
  hFlush stdout
  tcpResult <- timeout 3000000 $ testTCPConnection host port
  case tcpResult of
    Nothing -> do
      putStrLn "TIMEOUT (3s)"
      putStrLn "      TCP connection timed out. Server may be down or unreachable."
      return ()
    Just (Left err) -> do
      putStrLn $ "FAILED: " <> show err
      putStrLn "      Cannot establish TCP connection. Check if server is running."
      return ()
    Just (Right ()) -> do
      putStrLn "OK"

      -- Step 2: HTTP check
      putStr "[2/4] Testing HTTP endpoint... "
      hFlush stdout
      httpResult <- timeout 3000000 $ testHTTPEndpoint host port
      case httpResult of
        Nothing -> putStrLn "TIMEOUT (3s)"
        Just (Left err) -> putStrLn $ "FAILED: " <> show err
        Just (Right response) -> do
          putStrLn "OK"
          putStrLn $ "      " <> T.unpack response

      -- Step 3: WebSocket handshake
      putStr "[3/4] Testing WebSocket upgrade... "
      hFlush stdout
      wsResult <- timeout 5000000 $ testWebSocketHandshake host port
      case wsResult of
        Nothing -> do
          putStrLn "TIMEOUT (5s)"
          putStrLn ""
          putStrLn "ROOT CAUSE: WebSocket handshake hangs"
          putStrLn "  The server accepts TCP connections but doesn't complete"
          putStrLn "  the WebSocket upgrade. This indicates:"
          putStrLn "  - Server is not a WebSocket server on this port"
          putStrLn "  - Protocol version mismatch"
          putStrLn "  - Server is waiting for different handshake format"
        Just (Left err) -> do
          putStrLn $ "FAILED: " <> show err
          putStrLn ""
          putStrLn "WebSocket handshake rejected by server"
        Just (Right ()) -> do
          putStrLn "OK"

          -- Step 4: Protocol test
          putStr "[4/4] Testing Plexus RPC protocol... "
          hFlush stdout
          rpcResult <- timeout 5000000 $ testPlexusRPC host port backend
          case rpcResult of
            Nothing -> do
              putStrLn "TIMEOUT (5s)"
              putStrLn ""
              putStrLn "ROOT CAUSE: Plexus RPC protocol timeout"
              putStrLn "  The server accepts WebSocket connections but:"
              putStrLn "  - Does not send required StreamDone messages"
              putStrLn "  - Does not respond to _info requests correctly"
              putStrLn "  - May not be a valid Plexus RPC server"
            Just (Left err) -> putStrLn $ "FAILED: " <> show err
            Just (Right ()) -> putStrLn "OK"

-- | Test TCP connection
testTCPConnection :: String -> Int -> IO (Either SomeException ())
testTCPConnection host port = try $ do
  addr <- NS.getAddrInfo
    (Just NS.defaultHints { NS.addrSocketType = NS.Stream })
    (Just host)
    (Just $ show port)
  case addr of
    [] -> error "No address info"
    (serverAddr:_) -> do
      sock <- NS.socket (NS.addrFamily serverAddr) NS.Stream NS.defaultProtocol
      NS.connect sock (NS.addrAddress serverAddr)
      NS.close sock

-- | Test HTTP endpoint
testHTTPEndpoint :: String -> Int -> IO (Either SomeException Text)
testHTTPEndpoint host port = try $ do
  addr <- NS.getAddrInfo
    (Just NS.defaultHints { NS.addrSocketType = NS.Stream })
    (Just host)
    (Just $ show port)
  case addr of
    [] -> error "No address info"
    (serverAddr:_) -> do
      sock <- NS.socket (NS.addrFamily serverAddr) NS.Stream NS.defaultProtocol
      NS.connect sock (NS.addrAddress serverAddr)
      let request = "GET / HTTP/1.1\r\nHost: " <> BS.pack (host <> ":" <> show port) <> "\r\n\r\n"
      NSB.sendAll sock request
      response <- NSB.recv sock 1024
      NS.close sock
      return $ T.pack $ BS.unpack $ BS.take 100 response

-- | Test WebSocket handshake
testWebSocketHandshake :: String -> Int -> IO (Either SomeException ())
testWebSocketHandshake host port = try $
  WS.runClient host port "/" $ \_conn -> return ()

-- | Test Plexus RPC protocol
testPlexusRPC :: String -> Int -> Text -> IO (Either SomeException ())
testPlexusRPC host port backend = try $
  WS.runClient host port "/" $ \conn -> do
    -- Send _info request
    let request = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"" <> T.unpack backend <> "._info\",\"params\":{}}"
    WS.sendTextData conn (T.pack request :: Text)

    -- Wait for response (simplified - real implementation would parse)
    _response <- WS.receiveData conn :: IO Text
    return ()

-- | Validate protocol compliance of a backend
--
-- Runs the full protocol validation test suite against the specified backend
-- and reports any violations found.
--
-- Use: synapse _self validate <host> <port> <backend>
--
-- This connects to the backend and runs various protocol validation tests:
-- - Basic protocol compliance (_debug.protocol_test)
-- - Stream handling with slow data (_debug.stream_test)
-- - Progress reporting (_debug.stream_test with progress)
-- - Error handling (_debug.error_test)
-- - Metadata edge cases (_debug.metadata_test)
--
-- Exits with code 0 on success, 1 on protocol violations or errors.
validateProtocol :: String -> Int -> Text -> IO ()
validateProtocol host port backend = do
  TIO.putStrLn $ "Running protocol validation tests against " <> T.pack host <> ":" <> T.pack (show port) <> "..."
  TIO.putStrLn $ "Backend: " <> backend
  TIO.putStrLn ""

  -- Run the protocol test suite
  result <- TestRunner.runProtocolTests (T.pack host) port backend

  case result of
    Left errorReport -> do
      -- Protocol violations detected or test errors
      TIO.putStrLn errorReport
      TIO.putStrLn ""
      exitFailure

    Right successReport -> do
      -- All tests passed
      TIO.putStrLn successReport
      TIO.putStrLn ""
      exitSuccess

-- | Test an arbitrary method with protocol validation
--
-- Three modes:
-- - Mode 1 (default): Validated - uses parseParams, rejects unknown params
-- - Mode 2 (--allow-unknown): Warns about unknown params but passes them through
-- - Mode 3 (--raw <json>): Parses params as raw JSON, skips schema validation
--
-- Use: synapse _self test [--allow-unknown] [--raw <json>] <host> <port> <backend> <method> [--param value ...]
--
-- Examples:
--   synapse _self test localhost 5001 substrate echo.echo --message "hello"
--   synapse _self test --allow-unknown localhost 5001 substrate echo.echo --message "hello" --fake "test"
--   synapse _self test --raw '{"message":"hello"}' localhost 5001 substrate echo.echo
testMethod :: SubstrateConfig -> Text -> [(Text, Text)] -> Bool -> Maybe Text -> IO ()
testMethod config method cliParams allowUnknown rawJson = do
  let backend = substrateBackend config
  let host = substrateHost config
  let port = substratePort config

  -- Mode 3: Raw JSON
  case rawJson of
    Just jsonStr -> do
      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 jsonStr) of
        Left err -> do
          TIO.putStrLn $ "Error: Invalid JSON: " <> T.pack err
          exitFailure
        Right params -> runTest config method params

    -- Mode 1 & 2: Fetch schema and parse params
    Nothing -> do
      putStr "Fetching schema... "
      hFlush stdout

      -- Build IR for the method's namespace
      let methodParts = T.splitOn "." method
      let namespacePath = if length methodParts > 1 then init methodParts else []

      -- Create a SynapseM environment for buildIR
      logger <- makeLogger Katip.ErrorS  -- Use error-level logger for test command
      env <- initEnv (T.pack host) port backend logger

      -- Run buildIR within SynapseM
      buildResult <- runSynapseM env (buildIR [] namespacePath)

      case buildResult of
        Left err -> do
          putStrLn "FAILED"
          TIO.putStrLn ""
          TIO.putStrLn "[SCHEMA ERROR] Failed to build IR from schema"
          TIO.putStrLn $ "Error: " <> T.pack (show err)
          TIO.putStrLn ""
          TIO.putStrLn "This usually means:"
          TIO.putStrLn "  1. Method schema is incompatible with synapse client"
          TIO.putStrLn "  2. Schema format differs from expected structure"
          TIO.putStrLn "  3. Method was generated by incompatible macro (hub_methods vs plexus-derive)"
          TIO.putStrLn ""
          TIO.putStrLn "Try testing with raw WebSocket to verify method works:"
          TIO.putStrLn $ "  echo '{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"" <> method <> "\",\"params\":{...}}' | websocat ws://" <> T.pack host <> ":" <> T.pack (show port)
          TIO.putStrLn ""
          TIO.putStrLn "Or use the websocket-raw-test:"
          TIO.putStrLn "  cabal test websocket-raw-test"
          exitFailure
        Right ir -> do
          putStrLn "OK"

          -- Look up method definition
          case Map.lookup method (irMethods ir) of
            Nothing -> do
              TIO.putStrLn $ "Error: Method not found in schema: " <> method
              exitFailure
            Just methodDef -> do
              -- Mode 1: Validated (strict)
              if not allowUnknown
                then do
                  case parseParams ir methodDef cliParams of
                    Left errors -> do
                      TIO.putStrLn "Parameter validation failed:"
                      mapM_ (TIO.putStrLn . T.pack . show) errors
                      exitFailure
                    Right params ->
                      runTest config method params

              -- Mode 2: Allow unknown params
              else do
                let (knownParams, unknownParams) = partitionParams methodDef cliParams

                unless (null unknownParams) $
                  TIO.putStrLn $ "⚠️  Unknown parameters: " <> T.intercalate ", " (map fst unknownParams) <> " (passing through anyway)"

                -- Parse known params with IR validation
                knownParsed <- case parseParams ir methodDef knownParams of
                  Left errors -> do
                    TIO.putStrLn "Error parsing known parameters:"
                    mapM_ (TIO.putStrLn . T.pack . show) errors
                    exitFailure
                  Right p -> pure p

                -- Parse unknown params with best-effort type inference
                let unknownParsed = parseGeneric unknownParams

                -- Merge the two
                let params = mergeJson knownParsed unknownParsed
                runTest config method params

-- | Run the actual test and report results
runTest :: SubstrateConfig -> Text -> Value -> IO ()
runTest config method params = do
  TIO.putStrLn $ "Testing: " <> method
  result <- TestRunner.testEndpoint config method params

  case result of
    Left err -> do
      TIO.putStrLn "[FAIL] Test FAILED"
      TIO.putStrLn err
      exitFailure

    Right (violations, msgCount, rawMessages) -> do
      if null violations
        then do
          TIO.putStrLn $ "[PASS] Protocol VALID (" <> T.pack (show msgCount) <> " messages)"
          TIO.putStrLn "   - StreamDone sent"
          TIO.putStrLn "   - Metadata structure correct"
          TIO.putStrLn "   - Field naming valid"
          exitSuccess
        else do
          TIO.putStrLn $ "[FAIL] Protocol violations (" <> T.pack (show (length violations)) <> ")"
          TIO.putStrLn ""
          TIO.putStrLn "Raw messages received:"
          forM_ (zip [1..] rawMessages) $ \(idx, msg) -> do
            TIO.putStrLn $ "  Message " <> T.pack (show idx) <> ":"
            TIO.putStrLn $ "    " <> (TE.decodeUtf8 $ LBS.toStrict $ encodePretty msg)
          TIO.putStrLn ""
          TIO.putStrLn "Violations detected:"
          mapM_ (TIO.putStrLn . ("  " <>) . T.pack . show) violations
          exitFailure

-- | Partition params into known (in method def) and unknown
-- Handles dotted parameters like "identifier.type" by extracting the first segment
partitionParams :: MethodDef -> [(Text, Text)] -> ([(Text, Text)], [(Text, Text)])
partitionParams methodDef params =
  let paramNames = map pdName (mdParams methodDef)
      isKnown (key, _) =
        let baseKey = case T.breakOn "." key of
              (base, "") -> base  -- No dot, use the whole key
              (base, _)  -> base  -- Has dot, use first segment
        in baseKey `elem` paramNames
  in partition isKnown params

-- | Parse unknown params with best-effort type inference
parseGeneric :: [(Text, Text)] -> Value
parseGeneric pairs = object
  [ (K.fromText k, inferValue v) | (k, v) <- pairs ]
  where
    inferValue :: Text -> Value
    inferValue t
      | t == "true" = Bool True
      | t == "false" = Bool False
      | t == "" = Bool True  -- Flag with no value
      | Just n <- readMaybeInt t = Number (fromIntegral n)
      | Just n <- readMaybeDouble t = Number (realToFrac n)
      | otherwise = String t

    readMaybeInt :: Text -> Maybe Integer
    readMaybeInt = readMaybe . T.unpack

    readMaybeDouble :: Text -> Maybe Double
    readMaybeDouble = readMaybe . T.unpack

-- | Merge two JSON objects
mergeJson :: Value -> Value -> Value
mergeJson (Object a) (Object b) = Object (KM.union a b)
mergeJson (Object a) _ = Object a
mergeJson _ (Object b) = Object b
mergeJson a _ = a

-- | Read helper
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing
