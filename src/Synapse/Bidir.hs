{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bidirectional communication support for Synapse
--
-- This module handles bidirectional requests (confirm, prompt, select) from
-- the backend, supporting multiple modes for both human and agent users.
--
-- = Modes
--
-- - @interactive@: Uses TTY prompts (requires terminal)
-- - @json@: Outputs request as JSON to stdout, reads response from stdin
-- - @auto-cancel@: Automatically cancels all requests with a warning
-- - @defaults@: Uses default values from requests if available
-- - @--bidir-cmd CMD@: Spawns a subprocess per request (stdin=JSON, stdout=JSON)
-- - @--bidir-respond@: Prints request to stdout as JSON, returns Nothing so the
--   agent can respond via a separate @synapse \<backend\> respond@ invocation
--
-- Under @-o FILE@ (SYNAPSE-39) stdout is FILE, renamed into place only when
-- the call ends: every prompt and request line goes to stderr instead, to
-- be seen while the call waits.
--
-- = Protocol
--
-- When a @StreamRequest@ arrives, the handler:
-- 1. Processes it according to the current mode
-- 2. If a response is produced, sends it via @{backend}.respond@ RPC method
-- 3. If Nothing is returned (BidirRespond), the caller skips sending a response
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

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import System.IO (hClose, hFlush, hIsTerminalDevice, stderr, stdin, stdout)

import Synapse.Output (promptHandle)
import System.Process (createProcess, proc, std_in, std_out, StdStream(..), waitForProcess)
import System.Exit (ExitCode(..))

import Plexus.Types
  ( Request(..)
  , Response(..)
  , SelectOption(..)
  )

-- | Bidirectional communication mode
data BidirMode
  = BidirInteractive   -- ^ Use TTY prompts (requires terminal)
  | BidirJson          -- ^ Output request as JSON, read response from stdin
  | BidirAutoCancel    -- ^ Automatically cancel all requests
  | BidirDefaults      -- ^ Use default values from requests
  | BidirCmd Text      -- ^ Spawn subprocess per request (stdin=JSON request, stdout=JSON response)
  | BidirRespond       -- ^ Print request to stdout as JSON; agent responds via separate process call
  deriving (Show, Eq)

-- | Parse bidirectional mode from string
parseBidirMode :: Text -> Maybe BidirMode
parseBidirMode = \case
  "interactive" -> Just BidirInteractive
  "json"        -> Just BidirJson
  "auto-cancel" -> Just BidirAutoCancel
  "defaults"    -> Just BidirDefaults
  "respond"     -> Just BidirRespond
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
-- Takes request ID, request data, and returns an optional response.
-- Nothing means no immediate response (e.g., BidirRespond mode; agent responds separately).
type BidirHandler = Text -> Request Value -> IO (Maybe (Response Value))

-- | Handle a bidirectional request according to the mode
-- The response schema (if available) is passed for modes that output requests (e.g., BidirRespond)
handleBidirRequest :: BidirMode -> Maybe Value -> Text -> Request Value -> IO (Maybe (Response Value))
handleBidirRequest mode mResponseSchema requestId req = case mode of
  BidirInteractive -> Just <$> handleInteractive requestId req
  BidirJson        -> Just <$> handleJson requestId req
  BidirAutoCancel  -> Just <$> handleAutoCancel requestId req
  BidirDefaults    -> Just <$> handleDefaults requestId req
  BidirCmd cmd     -> Just <$> handleCmd cmd requestId req
  BidirRespond     -> handleRespond mResponseSchema requestId req

-- | Interactive TTY handling
handleInteractive :: Text -> Request Value -> IO (Response Value)
handleInteractive _requestId req = case req of
  Confirm{..} -> do
    let defaultHint = case confirmDefault of
          Just True  -> " [Y/n]"
          Just False -> " [y/N]"
          Nothing    -> " [y/n]"
    promptHandle >>= \h -> TIO.hPutStr h $ confirmMessage <> defaultHint <> ": "
    promptHandle >>= hFlush
    input <- TIO.getLine
    let confirmed = case T.toLower (T.strip input) of
          ""    -> case confirmDefault of
                     Just d  -> d
                     Nothing -> False
          "y"   -> True
          "yes" -> True
          _     -> False
    pure $ Confirmed confirmed

  Prompt{..} -> do
    let hint = case promptPlaceholder of
          Just ph -> " (" <> ph <> ")"
          Nothing -> ""
    promptHandle >>= \h -> TIO.hPutStr h $ promptMessage <> hint <> ": "
    promptHandle >>= hFlush
    input <- TIO.getLine
    let response = if T.null input
          then case promptDefault of
                 Just d  -> d
                 Nothing -> String input
          else String input
    pure $ Value response

  Select{..} -> do
    promptHandle >>= \h -> TIO.hPutStrLn h selectMessage
    mapM_ printOption (zip [1..] selectOptions)
    promptHandle >>= \h -> TIO.hPutStr h "Select (enter number): "
    promptHandle >>= hFlush
    input <- TIO.getLine
    case reads (T.unpack input) of
      [(n, "")] | n >= 1 && n <= length selectOptions ->
        let selected = optionValue (selectOptions !! (n - 1))
        in pure $ Selected [selected]
      _ -> pure Cancelled
  where
    printOption :: (Int, SelectOption Value) -> IO ()
    printOption (idx, SelectOption{..}) = do
      let desc = case optionDescription of
            Just d  -> " - " <> d
            Nothing -> ""
      promptHandle >>= \h -> TIO.hPutStrLn h $ "  " <> T.pack (show idx) <> ") " <> optionLabel <> desc

-- | JSON protocol handling for piped mode
-- Outputs request as a JSON line to stdout (stderr under -o, SYNAPSE-39), reads a JSON response line from stdin
handleJson :: Text -> Request Value -> IO (Response Value)
handleJson requestId req = do
  let jsonReq = encode $ object
        [ "type"       .= ("bidir_request" :: Text)
        , "request_id" .= requestId
        , "request"    .= req
        ]
  promptHandle >>= \h -> LBS.hPutStrLn h jsonReq
  promptHandle >>= hFlush
  inputLine <- TIO.getLine
  case eitherDecode (LBS.fromStrict $ T.encodeUtf8 inputLine) of
    Left err -> do
      TIO.hPutStrLn stderr $ "[synapse] Failed to parse bidir response: " <> T.pack err
      pure Cancelled
    Right resp -> pure resp

-- | Auto-cancel mode: cancels all requests with a warning
handleAutoCancel :: Text -> Request Value -> IO (Response Value)
handleAutoCancel requestId req = do
  TIO.hPutStrLn stderr $ "[synapse] Auto-cancelling " <> requestTypeName req
    <> " request (id: " <> requestId <> ") - not in interactive mode"
  TIO.hPutStrLn stderr "[synapse] Use --bidir-mode to change: json, defaults, interactive"
  TIO.hPutStrLn stderr "[synapse] Or use --bidir-cmd CMD / --bidir-respond for agent mode"
  pure Cancelled

-- | Defaults mode: uses default values from requests if available
handleDefaults :: Text -> Request Value -> IO (Response Value)
handleDefaults requestId req = case req of
  Confirm{..} -> case confirmDefault of
    Just d -> do
      TIO.hPutStrLn stderr $ "[synapse] Using default for confirm (id: "
        <> requestId <> "): " <> if d then "yes" else "no"
      pure $ Confirmed d
    Nothing -> do
      TIO.hPutStrLn stderr $ "[synapse] No default for confirm (id: "
        <> requestId <> "), cancelling"
      pure Cancelled

  Prompt{..} -> case promptDefault of
    Just d -> do
      TIO.hPutStrLn stderr $ "[synapse] Using default for prompt (id: " <> requestId <> ")"
      pure $ Value d
    Nothing -> do
      TIO.hPutStrLn stderr $ "[synapse] No default for prompt (id: "
        <> requestId <> "), cancelling"
      pure Cancelled

  Select{..} -> do
    TIO.hPutStrLn stderr $ "[synapse] No default for select (id: "
      <> requestId <> "), cancelling"
    pure Cancelled

-- | Command mode: spawn a subprocess per request
-- The subprocess receives a JSON object on stdin:
--   {"request_id": "...", "request": {...}}
-- And must write a JSON Response object to stdout:
--   {"type": "confirmed", "value": true}
handleCmd :: Text -> Text -> Request Value -> IO (Response Value)
handleCmd cmd requestId req = do
  let reqJson = encode $ object
        [ "request_id" .= requestId
        , "request"    .= req
        ]
  (Just hIn, Just hOut, _, ph) <- createProcess (proc "sh" ["-c", T.unpack cmd])
    { std_in  = CreatePipe
    , std_out = CreatePipe
    }
  LBS.hPutStr hIn reqJson
  hClose hIn
  outputBytes <- LBS.hGetContents hOut
  exitCode <- waitForProcess ph
  case exitCode of
    ExitFailure code -> do
      TIO.hPutStrLn stderr $ "[synapse] --bidir-cmd subprocess exited with code "
        <> T.pack (show code) <> " (id: " <> requestId <> ")"
      pure Cancelled
    ExitSuccess -> case eitherDecode outputBytes of
      Left err -> do
        TIO.hPutStrLn stderr $ "[synapse] Failed to parse --bidir-cmd response: " <> T.pack err
        pure Cancelled
      Right resp -> pure resp

-- | Respond mode: print request to stdout as JSON, return Nothing
-- The caller (invokeStreamingWithBidir) will skip sending a response.
-- The agent is expected to call: synapse <backend> respond --request-id <id> --response <json>
handleRespond :: Maybe Value -> Text -> Request Value -> IO (Maybe (Response Value))
handleRespond mResponseSchema requestId req = do
  let baseFields =
        [ "type"       .= ("bidir_request" :: Text)
        , "request_id" .= requestId
        , "request"    .= req
        ]
      -- Include response_schema if available
      allFields = case mResponseSchema of
        Just schema -> baseFields ++ ["response_schema" .= schema]
        Nothing     -> baseFields
      jsonReq = encode $ object allFields
  promptHandle >>= \h -> LBS.hPutStrLn h jsonReq
  promptHandle >>= hFlush
  pure Nothing

-- | Get a human-readable name for the request type
requestTypeName :: Request t -> Text
requestTypeName = \case
  Confirm{} -> "confirm"
  Prompt{}  -> "prompt"
  Select{}  -> "select"
