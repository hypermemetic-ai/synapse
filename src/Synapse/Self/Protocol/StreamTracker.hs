{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Synapse.Self.Protocol.StreamTracker
  ( -- * Stream Tracker
    StreamTracker
  , StreamState(..)

    -- * Operations
  , newTracker
  , trackMessage
  , checkCompletion
  , getAllViolations

    -- * For testing
  , getStreamCount
  ) where

import Control.Monad (forM)
import Data.Aeson (Value(..), Object)
import qualified Data.Aeson.KeyMap as KM
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

import Synapse.Self.Protocol.Validator
  ( ProtocolViolation(..)
  , Severity(..)
  , Location(..)
  )

-- ============================================================================
-- Types
-- ============================================================================

-- | State for a single stream subscription
data StreamState = StreamState
  { ssSubscriptionId :: !Int
    -- ^ The subscription ID for this stream
  , ssMessages :: ![Value]
    -- ^ All messages received for this stream
  , ssHasDone :: !Bool
    -- ^ Whether StreamDone was received
  , ssViolations :: ![ProtocolViolation]
    -- ^ Violations detected for this stream
  , ssMethod :: !(Maybe Text)
    -- ^ The method name that created this subscription (if known)
  }
  deriving (Show, Eq)

-- | Stream tracker for protocol validation
-- Tracks multiple concurrent streams and detects protocol violations
data StreamTracker = StreamTracker
  { stStreams :: !(IORef (HashMap Int StreamState))
    -- ^ Map from subscription ID to stream state
  }

-- ============================================================================
-- Constructor
-- ============================================================================

-- | Create a new stream tracker
newTracker :: IO StreamTracker
newTracker = do
  ref <- newIORef HM.empty
  pure $ StreamTracker ref

-- ============================================================================
-- Message Tracking
-- ============================================================================

-- | Track a message and detect any protocol violations
--
-- This function processes incoming messages and:
-- 1. Extracts subscription information
-- 2. Updates stream state
-- 3. Detects violations (duplicate StreamDone, missing subscription ID, etc.)
trackMessage :: StreamTracker -> Value -> IO [ProtocolViolation]
trackMessage tracker msg = do
  case parseMessage msg of
    Nothing ->
      -- Not a stream notification, ignore
      pure []

    Just (subId, msgType, method) -> do
      modifyIORef' (stStreams tracker) $ \streams ->
        let currentState = HM.lookupDefault (emptyStreamState subId method) subId streams
            updatedState = updateStreamState currentState msgType msg
        in HM.insert subId updatedState streams

      -- Return violations found in this message
      streams <- readIORef (stStreams tracker)
      case HM.lookup subId streams of
        Nothing -> pure []
        Just state -> pure $ ssViolations state

-- | Parse a message to extract subscription info and message type
parseMessage :: Value -> Maybe (Int, MessageType, Maybe Text)
parseMessage (Object o) = do
  -- Check if this is a subscription notification
  method <- KM.lookup "method" o
  case method of
    String "subscription" -> do
      -- Extract params object
      params <- KM.lookup "params" o
      case params of
        Object paramsObj -> do
          -- Extract subscription ID
          subIdValue <- KM.lookup "subscription" paramsObj
          subId <- case subIdValue of
            Number n -> Just $ truncate n
            String s -> case reads (T.unpack s) of
              [(n, "")] -> Just n
              _ -> Nothing
            _ -> Nothing

          -- Extract result object to determine message type
          result <- KM.lookup "result" paramsObj
          msgType <- case result of
            Object resultObj -> do
              typeValue <- KM.lookup "type" resultObj
              case typeValue of
                String "done" -> Just StreamDoneMsg
                String "data" -> Just StreamDataMsg
                String "progress" -> Just StreamProgressMsg
                String "error" -> Just StreamErrorMsg
                String "guidance" -> Just StreamGuidanceMsg
                _ -> Just UnknownMsg
            _ -> Just UnknownMsg

          pure (subId, msgType, Nothing)
        _ -> Nothing
    _ -> Nothing
parseMessage _ = Nothing

-- | Message types in the stream protocol
data MessageType
  = StreamDoneMsg
  | StreamDataMsg
  | StreamProgressMsg
  | StreamErrorMsg
  | StreamGuidanceMsg
  | UnknownMsg
  deriving (Show, Eq)

-- | Create an empty stream state
emptyStreamState :: Int -> Maybe Text -> StreamState
emptyStreamState subId method = StreamState
  { ssSubscriptionId = subId
  , ssMessages = []
  , ssHasDone = False
  , ssViolations = []
  , ssMethod = method
  }

-- | Update stream state with a new message
updateStreamState :: StreamState -> MessageType -> Value -> StreamState
updateStreamState state msgType msg =
  let newMessages = ssMessages state ++ [msg]
      newViolations = detectMessageViolations state msgType
  in case msgType of
    StreamDoneMsg ->
      if ssHasDone state
        then -- Duplicate StreamDone violation
          state
            { ssMessages = newMessages
            , ssViolations = ssViolations state ++ newViolations ++
                [ ProtocolViolation
                    { pvMessage = "Multiple StreamDone messages received for subscription " <> T.pack (show $ ssSubscriptionId state)
                    , pvSeverity = Error
                    , pvLocation = InMessage
                        { lmSubscriptionId = Just (ssSubscriptionId state)
                        , lmMessageIndex = length newMessages
                        , lmField = Just "type"
                        }
                    , pvExpected = Just "Only one StreamDone per subscription"
                    , pvActual = Just "Multiple StreamDone messages"
                    , pvFix = Just "Server should only send StreamDone once per subscription"
                    }
                ]
            }
        else -- First StreamDone, mark as complete
          state
            { ssMessages = newMessages
            , ssHasDone = True
            , ssViolations = ssViolations state ++ newViolations
            }

    _ -> -- Other message types
      if ssHasDone state
        then -- Received message after StreamDone
          state
            { ssMessages = newMessages
            , ssViolations = ssViolations state ++ newViolations ++
                [ ProtocolViolation
                    { pvMessage = "Received message after StreamDone for subscription " <> T.pack (show $ ssSubscriptionId state)
                    , pvSeverity = Error
                    , pvLocation = InMessage
                        { lmSubscriptionId = Just (ssSubscriptionId state)
                        , lmMessageIndex = length newMessages
                        , lmField = Just "type"
                        }
                    , pvExpected = Just "No messages after StreamDone"
                    , pvActual = Just $ "Received " <> T.pack (show msgType) <> " after StreamDone"
                    , pvFix = Just "Server should not send messages after StreamDone"
                    }
                ]
            }
        else
          state
            { ssMessages = newMessages
            , ssViolations = ssViolations state ++ newViolations
            }

-- | Detect violations in a specific message
detectMessageViolations :: StreamState -> MessageType -> [ProtocolViolation]
detectMessageViolations _state _msgType =
  -- For now, we don't detect per-message violations beyond what's in updateStreamState
  -- This could be extended to check message structure, metadata, etc.
  []

-- ============================================================================
-- Completion Checking
-- ============================================================================

-- | Check if all streams have completed (received StreamDone)
-- Returns violations for any incomplete streams
checkCompletion :: StreamTracker -> IO [ProtocolViolation]
checkCompletion tracker = do
  streams <- readIORef (stStreams tracker)
  let incompleteStreams = HM.filter (not . ssHasDone) streams
  pure $ map mkIncompleteViolation (HM.elems incompleteStreams)
  where
    mkIncompleteViolation :: StreamState -> ProtocolViolation
    mkIncompleteViolation state = ProtocolViolation
      { pvMessage = "Stream never received StreamDone for subscription " <> T.pack (show $ ssSubscriptionId state)
      , pvSeverity = Error
      , pvLocation = InStream
          { lsMethod = fromMaybe "unknown" (ssMethod state)
          , lsMessageCount = length (ssMessages state)
          }
      , pvExpected = Just "StreamDone message to complete the stream"
      , pvActual = Just $ T.pack (show $ length $ ssMessages state) <> " messages without StreamDone"
      , pvFix = Just "Server must send StreamDone to properly close each stream"
      }

-- ============================================================================
-- Violation Retrieval
-- ============================================================================

-- | Get all violations detected across all streams
getAllViolations :: StreamTracker -> IO [ProtocolViolation]
getAllViolations tracker = do
  streams <- readIORef (stStreams tracker)
  let allStreamViolations = concatMap ssViolations (HM.elems streams)
  completionViolations <- checkCompletion tracker
  pure $ allStreamViolations ++ completionViolations

-- ============================================================================
-- Testing Helpers
-- ============================================================================

-- | Get the number of tracked streams (for testing)
getStreamCount :: StreamTracker -> IO Int
getStreamCount tracker = do
  streams <- readIORef (stStreams tracker)
  pure $ HM.size streams
