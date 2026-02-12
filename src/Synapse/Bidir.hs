{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bidirectional communication support for Synapse
--
-- This module handles bidirectional requests (confirm, prompt, select) from
-- the backend, supporting both interactive TTY mode and non-interactive
-- piped/scripted modes.
--
-- = Modes
--
-- - @interactive@: Uses TTY prompts (requires terminal)
-- - @json@: Outputs request as JSON to stdout, reads response from stdin
-- - @auto-cancel@: Automatically cancels all requests with a warning
-- - @defaults@: Uses default values from requests if available
--
-- = Protocol
--
-- When a @StreamRequest@ arrives, the handler:
-- 1. Processes it according to the current mode
-- 2. Sends the response via @{backend}.respond@ RPC method
module Synapse.Bidir
  ( -- * Types
    BidirMode(..)
  , parseBidirMode
  , defaultBidirMode

    -- * TTY Detection
  , isTTY
  , detectBidirMode

    -- * Request Handling
  , handleBidirRequest
  , BidirHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, hIsTerminalDevice, stderr, stdin, stdout)

import Plexus.Types
  ( StandardRequest(..)
  , StandardResponse(..)
  , SelectOption(..)
  )

-- | Bidirectional communication mode
data BidirMode
  = BidirInteractive   -- ^ Use TTY prompts (requires terminal)
  | BidirJson          -- ^ Output request as JSON, read response from stdin
  | BidirAutoCancel    -- ^ Automatically cancel all requests
  | BidirDefaults      -- ^ Use default values from requests
  deriving (Show, Eq)

-- | Parse bidirectional mode from string
parseBidirMode :: Text -> Maybe BidirMode
parseBidirMode = \case
  "interactive" -> Just BidirInteractive
  "json"        -> Just BidirJson
  "auto-cancel" -> Just BidirAutoCancel
  "defaults"    -> Just BidirDefaults
  _             -> Nothing

-- | Default mode when running in TTY
defaultBidirMode :: BidirMode
defaultBidirMode = BidirInteractive

-- | Check if stdin is a terminal
isTTY :: IO Bool
isTTY = hIsTerminalDevice stdin

-- | Detect the appropriate bidirectional mode based on environment
-- Returns Interactive for TTY, AutoCancel for piped mode
detectBidirMode :: IO BidirMode
detectBidirMode = do
  tty <- isTTY
  pure $ if tty then BidirInteractive else BidirAutoCancel

-- | Handler type for bidirectional requests
-- Takes request ID, request data, and returns a response
type BidirHandler = Text -> StandardRequest -> IO StandardResponse

-- | Handle a bidirectional request according to the mode
-- Returns the appropriate StandardResponse
handleBidirRequest :: BidirMode -> Text -> StandardRequest -> IO StandardResponse
handleBidirRequest mode requestId req = case mode of
  BidirInteractive -> handleInteractive requestId req
  BidirJson        -> handleJson requestId req
  BidirAutoCancel  -> handleAutoCancel requestId req
  BidirDefaults    -> handleDefaults requestId req

-- | Interactive TTY handling
handleInteractive :: Text -> StandardRequest -> IO StandardResponse
handleInteractive _requestId req = case req of
  ConfirmRequest{..} -> do
    let defaultHint = case confirmDefault of
          Just True  -> " [Y/n]"
          Just False -> " [y/N]"
          Nothing    -> " [y/n]"
    TIO.putStr $ confirmMessage <> defaultHint <> ": "
    hFlush stdout
    input <- TIO.getLine
    let response = case T.toLower (T.strip input) of
          ""  -> case confirmDefault of
                   Just d  -> d
                   Nothing -> False
          "y" -> True
          "yes" -> True
          _ -> False
    pure $ ConfirmedResponse response

  PromptRequest{..} -> do
    let hint = case promptPlaceholder of
          Just ph -> " (" <> ph <> ")"
          Nothing -> ""
    let defaultHint = case promptDefault of
          Just d  -> " [" <> d <> "]"
          Nothing -> ""
    TIO.putStr $ promptMessage <> hint <> defaultHint <> ": "
    hFlush stdout
    input <- TIO.getLine
    let response = if T.null input
          then case promptDefault of
                 Just d  -> d
                 Nothing -> input
          else input
    pure $ TextResponse response

  SelectRequest{..} -> do
    TIO.putStrLn selectMessage
    mapM_ printOption (zip [1..] selectOptions)
    TIO.putStr "Select (enter number): "
    hFlush stdout
    input <- TIO.getLine
    case reads (T.unpack input) of
      [(n, "")] | n >= 1 && n <= length selectOptions ->
        let selected = optionValue (selectOptions !! (n - 1))
        in pure $ SelectedResponse [selected]
      _ -> pure CancelledResponse
  where
    printOption :: (Int, SelectOption) -> IO ()
    printOption (idx, SelectOption{..}) = do
      let desc = case optionDescription of
            Just d  -> " - " <> d
            Nothing -> ""
      TIO.putStrLn $ "  " <> T.pack (show idx) <> ") " <> optionLabel <> desc

-- | JSON protocol handling for piped mode
-- Outputs request as JSON line, reads response from stdin
handleJson :: Text -> StandardRequest -> IO StandardResponse
handleJson requestId req = do
  -- Output request as JSON to stdout
  let jsonReq = object
        [ "type" .= ("bidir_request" :: Text)
        , "request_id" .= requestId
        , "request" .= req
        ]
  LBS.putStrLn $ encode jsonReq
  hFlush stdout

  -- Read single line response from stdin
  inputLine <- TIO.getLine
  case eitherDecode (LBS.fromStrict $ T.encodeUtf8 inputLine) of
    Left err -> do
      TIO.hPutStrLn stderr $ "Failed to parse response: " <> T.pack err
      pure CancelledResponse
    Right resp -> parseJsonResponse resp

-- | Parse JSON response from stdin
parseJsonResponse :: Value -> IO StandardResponse
parseJsonResponse val = case val of
  Object o -> case KM.lookup "type" o of
    Just (String "confirmed") -> case KM.lookup "value" o of
      Just (Bool b) -> pure $ ConfirmedResponse b
      _ -> pure CancelledResponse
    Just (String "text") -> case KM.lookup "value" o of
      Just (String t) -> pure $ TextResponse t
      _ -> pure CancelledResponse
    Just (String "selected") -> case KM.lookup "values" o of
      Just (Array arr) -> do
        let vals = [t | String t <- map id (foldr (:) [] arr)]
        pure $ SelectedResponse vals
      _ -> pure CancelledResponse
    Just (String "cancelled") -> pure CancelledResponse
    _ -> do
      TIO.hPutStrLn stderr "Unknown response type"
      pure CancelledResponse
  _ -> do
    TIO.hPutStrLn stderr "Response must be a JSON object"
    pure CancelledResponse

-- | Auto-cancel mode: cancels all requests with a warning
handleAutoCancel :: Text -> StandardRequest -> IO StandardResponse
handleAutoCancel requestId req = do
  let reqType = case req of
        ConfirmRequest{} -> "confirm"
        PromptRequest{}  -> "prompt"
        SelectRequest{}  -> "select"
  TIO.hPutStrLn stderr $ "[synapse] Auto-cancelling " <> reqType
    <> " request (id: " <> requestId <> ") - not in interactive mode"
  TIO.hPutStrLn stderr $ "[synapse] Use --bidir-mode to change behavior: json, defaults, interactive"
  pure CancelledResponse

-- | Defaults mode: uses default values from requests
handleDefaults :: Text -> StandardRequest -> IO StandardResponse
handleDefaults requestId req = case req of
  ConfirmRequest{..} -> case confirmDefault of
    Just d -> do
      TIO.hPutStrLn stderr $ "[synapse] Using default for confirm (id: "
        <> requestId <> "): " <> if d then "yes" else "no"
      pure $ ConfirmedResponse d
    Nothing -> do
      TIO.hPutStrLn stderr $ "[synapse] No default for confirm (id: "
        <> requestId <> "), cancelling"
      pure CancelledResponse

  PromptRequest{..} -> case promptDefault of
    Just d -> do
      TIO.hPutStrLn stderr $ "[synapse] Using default for prompt (id: "
        <> requestId <> "): " <> d
      pure $ TextResponse d
    Nothing -> do
      TIO.hPutStrLn stderr $ "[synapse] No default for prompt (id: "
        <> requestId <> "), cancelling"
      pure CancelledResponse

  SelectRequest{..} -> do
    TIO.hPutStrLn stderr $ "[synapse] No default for select (id: "
      <> requestId <> "), cancelling"
    pure CancelledResponse
