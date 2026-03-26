{-# LANGUAGE OverloadedStrings #-}

-- | Minimal WebSocket test - does exactly what websocat does
--
-- Usage: cabal run websocket-raw-test
--
-- This test replicates the exact behavior of:
--   echo '{"jsonrpc":"2.0","id":1,"method":"ping_pong","params":{"message":"test"}}' | websocat ws://127.0.0.1:4444

module Main (main) where

import qualified Data.Aeson as Aeson
import Data.Aeson (object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Network.WebSockets as WS
import Control.Monad (forever, void)
import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (catch, SomeException)
import System.Exit (exitSuccess, exitFailure)

main :: IO ()
main = do
  TIO.putStrLn "WebSocket Raw Test - Connecting to ws://127.0.0.1:4444"

  result <- catch runTest handleError

  case result of
    True -> do
      TIO.putStrLn "\n[OK] Test PASSED"
      exitSuccess
    False -> do
      TIO.putStrLn "\n[ERR] Test FAILED"
      exitFailure

runTest :: IO Bool
runTest = WS.runClient "127.0.0.1" 4444 "/" $ \conn -> do
  TIO.putStrLn "[OK] WebSocket connection established"

  -- Send the exact JSON-RPC request that websocat sends
  let request = object
        [ "jsonrpc" .= ("2.0" :: T.Text)
        , "id" .= (1 :: Int)
        , "method" .= ("ping_pong" :: T.Text)
        , "params" .= object
            [ "message" .= ("test" :: T.Text)
            ]
        ]

  let requestBS = Aeson.encode request
  TIO.putStrLn $ "> Sending: " <> T.pack (show request)
  WS.sendTextData conn requestBS

  -- Receive messages (subscription ID, data, done)
  TIO.putStrLn "\nWaiting for responses..."

  -- Message 1: Subscription ID
  msg1 <- WS.receiveData conn
  TIO.putStrLn $ "< Received 1: " <> T.pack (show (msg1 :: LBS.ByteString))

  case Aeson.decode msg1 of
    Nothing -> do
      TIO.putStrLn "[ERR] Failed to decode message 1 as JSON"
      return False
    Just (Aeson.Object obj) -> do
      case KM.lookup "result" obj of
        Nothing -> do
          TIO.putStrLn "[ERR] Message 1 missing 'result' field (subscription ID)"
          return False
        Just (Aeson.Number subId) -> do
          TIO.putStrLn $ "[OK] Got subscription ID: " <> T.pack (show subId)

          -- Message 2: Data event
          msg2 <- WS.receiveData conn
          TIO.putStrLn $ "\n< Received 2: " <> T.pack (show (msg2 :: LBS.ByteString))

          case Aeson.decode msg2 of
            Nothing -> do
              TIO.putStrLn "[ERR] Failed to decode message 2 as JSON"
              return False
            Just (Aeson.Object obj2) -> do
              case KM.lookup "params" obj2 of
                Nothing -> do
                  TIO.putStrLn "[ERR] Message 2 missing 'params' field"
                  return False
                Just (Aeson.Object params) -> do
                  case KM.lookup "result" params of
                    Nothing -> do
                      TIO.putStrLn "[ERR] Message 2 params missing 'result' field"
                      return False
                    Just (Aeson.Object result) -> do
                      case KM.lookup "type" result of
                        Just (Aeson.String "data") -> do
                          TIO.putStrLn "[OK] Got data event"

                          -- Message 3: Done event
                          msg3 <- WS.receiveData conn
                          TIO.putStrLn $ "\n< Received 3: " <> T.pack (show (msg3 :: LBS.ByteString))

                          case Aeson.decode msg3 of
                            Nothing -> do
                              TIO.putStrLn "[ERR] Failed to decode message 3 as JSON"
                              return False
                            Just (Aeson.Object obj3) -> do
                              case KM.lookup "params" obj3 of
                                Nothing -> do
                                  TIO.putStrLn "[ERR] Message 3 missing 'params' field"
                                  return False
                                Just (Aeson.Object params3) -> do
                                  case KM.lookup "result" params3 of
                                    Nothing -> do
                                      TIO.putStrLn "[ERR] Message 3 params missing 'result' field"
                                      return False
                                    Just (Aeson.Object result3) -> do
                                      case KM.lookup "type" result3 of
                                        Just (Aeson.String "done") -> do
                                          TIO.putStrLn "[OK] Got done event"
                                          TIO.putStrLn "\n[OK] All messages received successfully"
                                          return True
                                        _ -> do
                                          TIO.putStrLn "[ERR] Message 3 type is not 'done'"
                                          return False
                                    _ -> do
                                      TIO.putStrLn "[ERR] Message 3 result is not an object"
                                      return False
                                _ -> do
                                  TIO.putStrLn "[ERR] Message 3 params is not an object"
                                  return False
                            _ -> do
                              TIO.putStrLn "[ERR] Message 3 is not an object"
                              return False
                        _ -> do
                          TIO.putStrLn "[ERR] Message 2 type is not 'data'"
                          return False
                    _ -> do
                      TIO.putStrLn "[ERR] Message 2 result is not an object"
                      return False
                _ -> do
                  TIO.putStrLn "[ERR] Message 2 params is not an object"
                  return False
            _ -> do
              TIO.putStrLn "[ERR] Message 2 is not an object"
              return False
        _ -> do
          TIO.putStrLn "[ERR] Message 1 result is not a number"
          return False
    _ -> do
      TIO.putStrLn "[ERR] Message 1 is not an object"
      return False

handleError :: SomeException -> IO Bool
handleError e = do
  TIO.putStrLn $ "[ERR] Exception: " <> T.pack (show e)
  return False
