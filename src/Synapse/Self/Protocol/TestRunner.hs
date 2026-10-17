{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Synapse.Self.Protocol.TestRunner
  ( -- * Main Test Runner
    runProtocolTests

    -- * Helper Functions
  , testEndpoint
  , TestResult(..)
  ) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar_)
import Control.Exception (SomeException, catch)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Plexus.Transport as PT
import qualified Plexus.Types as PT
import Plexus.Client (SubstrateConfig(..))

import Synapse.Self.Protocol.StreamTracker
  ( StreamTracker
  , newTracker
  , trackMessage
  , getAllViolations
  )
import Synapse.Self.Protocol.Validator
  ( ProtocolViolation(..)
  , validateStreamItem
  )
import Synapse.Self.Protocol.Reporter
  ( renderViolations
  , renderSummary
  )

-- ============================================================================
-- Types
-- ============================================================================

-- | Result of a single test
data TestResult = TestResult
  { trTestName :: !Text
  , trViolations :: ![ProtocolViolation]
  , trMessageCount :: !Int
  , trError :: !(Maybe Text)
  }
  deriving (Show, Eq)

-- ============================================================================
-- Main Test Runner
-- ============================================================================

-- | Run protocol validation tests against a backend
--
-- Connects to the specified host/port/backend and runs a suite of
-- protocol validation tests using the debug endpoints:
-- - _debug.protocol_test (basic protocol compliance)
-- - _debug.stream_test with various scenarios (slow, progress)
-- - _debug.error_test (error handling)
-- - _debug.metadata_test (metadata edge cases)
--
-- Returns either an error message or a success report with detailed
-- violation information.
runProtocolTests :: Text -> Int -> Text -> IO (Either Text Text)
runProtocolTests host port backend = do
  let config = SubstrateConfig
        { substrateHost    = T.unpack host
        , substratePort    = port
        , substratePath    = "/"
        , substrateBackend = backend
        , substrateHeaders = []
        }

  -- Run all test suites
  results <- runTestSuite config

  -- Aggregate all violations
  let allViolations = concatMap trViolations results
  let hasErrors = any (\r -> not (null (trViolations r))) results
  let hasTestErrors = any (\r -> case trError r of Just _ -> True; Nothing -> False) results

  -- Generate report
  if hasTestErrors
    then do
      -- Some tests failed to run
      let errorReport = T.intercalate "\n" $ map formatTestError results
      pure $ Left $ "Test execution errors:\n" <> errorReport
    else if hasErrors
      then do
        -- Protocol violations detected
        let report = renderViolations allViolations
                  <> "\n"
                  <> renderSummary allViolations
                  <> "\n\nTest Results:\n"
                  <> formatTestResults results
        pure $ Left report
      else do
        -- All tests passed
        let report = renderSummary allViolations
                  <> "\n\nTest Results:\n"
                  <> formatTestResults results
        pure $ Right report

-- | Format test error for display
formatTestError :: TestResult -> Text
formatTestError TestResult{..} =
  case trError of
    Nothing -> ""
    Just err -> "  [" <> trTestName <> "] ERROR: " <> err

-- | Format test results summary
formatTestResults :: [TestResult] -> Text
formatTestResults results =
  T.intercalate "\n" $ map formatTestResult results

-- | Format a single test result
formatTestResult :: TestResult -> Text
formatTestResult TestResult{..} =
  let status = if null trViolations
                 then "PASS"
                 else "FAIL (" <> T.pack (show (length trViolations)) <> " violations)"
  in "  [" <> trTestName <> "] " <> status <> " (" <> T.pack (show trMessageCount) <> " messages)"

-- ============================================================================
-- Test Suite
-- ============================================================================

-- | Run the full test suite
runTestSuite :: SubstrateConfig -> IO [TestResult]
runTestSuite config = do
  -- Test 1: Basic protocol compliance
  r1 <- testEndpointWithName config "Protocol Test" "_debug.protocol_test" Aeson.Null

  -- Test 2: Stream test with slow scenario
  r2 <- testEndpointWithName config "Stream Test (slow)" "_debug.stream_test"
          (object ["scenario" .= ("slow" :: Text)])

  -- Test 3: Stream test with progress scenario
  r3 <- testEndpointWithName config "Stream Test (progress)" "_debug.stream_test"
          (object ["scenario" .= ("progress" :: Text)])

  -- Test 4: Error test with recoverable error
  r4 <- testEndpointWithName config "Error Test (recoverable)" "_debug.error_test"
          (object ["error_type" .= ("recoverable" :: Text)])

  -- Test 5: Metadata edge cases
  r5 <- testEndpointWithName config "Metadata Test" "_debug.metadata_test" Aeson.Null

  pure [r1, r2, r3, r4, r5]

-- | Test an endpoint with a name
testEndpointWithName :: SubstrateConfig -> Text -> Text -> Value -> IO TestResult
testEndpointWithName config testName method params = do
  result <- testEndpoint config method params
  case result of
    Left err ->
      pure $ TestResult
        { trTestName = testName
        , trViolations = []
        , trMessageCount = 0
        , trError = Just err
        }
    Right (violations, msgCount, _rawMessages) ->
      pure $ TestResult
        { trTestName = testName
        , trViolations = violations
        , trMessageCount = msgCount
        , trError = Nothing
        }

-- ============================================================================
-- Endpoint Testing
-- ============================================================================

-- | Test a specific endpoint for protocol violations
--
-- Connects to the endpoint, subscribes to the stream, collects all messages,
-- validates each message, and returns all violations found.
--
-- Returns either an error message (connection/timeout issues) or a tuple of:
-- - List of protocol violations
-- - Message count
-- - Raw messages (for debugging failures)
testEndpoint :: SubstrateConfig -> Text -> Value -> IO (Either Text ([ProtocolViolation], Int, [Value]))
testEndpoint config method params = do
  -- Create tracker for this test
  tracker <- newTracker

  -- Counter for messages received
  msgCountRef <- newIORef 0

  -- Collect all violations
  violationsRef <- newIORef []

  -- Collect raw messages for debugging
  messagesRef <- newIORef []

  -- Result MVar
  resultMVar <- newEmptyMVar

  -- Build the RPC path: {backend}.call with method parameter
  let backend = substrateBackend config
  let callMethod = backend <> ".call"
  let callParams = object
        [ "method" .= method
        , "params" .= params
        ]

  -- Start the streaming call in a separate thread
  _ <- forkIO $ do
    result <- PT.rpcCallStreaming config callMethod callParams $ \item -> do
      -- Convert PlexusStreamItem to Value for validation
      let itemValue = plexusItemToValue item

      -- Store raw message for debugging
      modifyIORef' messagesRef (++ [itemValue])

      -- Track the message
      trackViolations <- trackMessage tracker itemValue
      modifyIORef' violationsRef (++ trackViolations)

      -- Validate the stream item structure
      structureViolations <- validateStreamItem itemValue
      modifyIORef' violationsRef (++ structureViolations)

      -- Increment message count
      modifyIORef' msgCountRef (+1)

    case result of
      Left transportErr ->
        putMVar resultMVar $ Left $ formatTransportError transportErr
      Right () ->
        -- Check for completion violations
        putMVar resultMVar $ Right ()

  -- Wait for completion with timeout (15 seconds)
  timeoutResult <- timeout 15000000 (takeMVar resultMVar)

  case timeoutResult of
    Nothing ->
      -- Timeout occurred
      pure $ Left "Timeout: Test did not complete within 15 seconds"

    Just (Left err) ->
      -- Transport error
      pure $ Left err

    Just (Right ()) -> do
      -- Get completion violations (streams that didn't receive StreamDone)
      completionViolations <- getAllViolations tracker

      -- Get all violations
      allViolations <- readIORef violationsRef
      let finalViolations = allViolations ++ completionViolations

      -- Get message count
      msgCount <- readIORef msgCountRef

      -- Get raw messages
      rawMessages <- readIORef messagesRef

      pure $ Right (finalViolations, msgCount, rawMessages)

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Convert PlexusStreamItem to Value for validation
--
-- This extracts the JSON representation from the subscription notification
plexusItemToValue :: PT.PlexusStreamItem -> Value
plexusItemToValue (PT.StreamData hash prov ct content) =
  object
    [ "type" .= ("data" :: Text)
    , "content_type" .= ct
    , "content" .= content
    , "metadata" .= object
        [ "provenance" .= provenanceToValue prov
        , "plexus_hash" .= hash
        , "timestamp" .= (0 :: Int)
        ]
    ]
plexusItemToValue (PT.StreamProgress hash prov msg pct) =
  object
    [ "type" .= ("progress" :: Text)
    , "message" .= msg
    , "percentage" .= pct
    , "metadata" .= object
        [ "provenance" .= provenanceToValue prov
        , "plexus_hash" .= hash
        , "timestamp" .= (0 :: Int)
        ]
    ]
plexusItemToValue (PT.StreamError hash prov msg recoverable) =
  object
    [ "type" .= ("error" :: Text)
    , "message" .= msg
    , "code" .= if recoverable then Just ("recoverable" :: Text) else Nothing
    , "metadata" .= object
        [ "provenance" .= provenanceToValue prov
        , "plexus_hash" .= hash
        , "timestamp" .= (0 :: Int)
        ]
    ]
plexusItemToValue (PT.StreamDone hash prov) =
  object
    [ "type" .= ("done" :: Text)
    , "metadata" .= object
        [ "provenance" .= provenanceToValue prov
        , "plexus_hash" .= hash
        , "timestamp" .= (0 :: Int)
        ]
    ]
plexusItemToValue (PT.StreamRequest hash prov reqId reqData timeout) =
  object
    [ "type" .= ("request" :: Text)
    , "request_id" .= reqId
    , "request_data" .= reqData
    , "timeout" .= timeout
    , "metadata" .= object
        [ "provenance" .= provenanceToValue prov
        , "plexus_hash" .= hash
        , "timestamp" .= (0 :: Int)
        ]
    ]
plexusItemToValue (PT.StreamGuidance hash prov errType suggestion availMethods methodSchema) =
  object $
    [ "type" .= ("guidance" :: Text)
    , "metadata" .= object
        [ "provenance" .= provenanceToValue prov
        , "plexus_hash" .= hash
        , "timestamp" .= (0 :: Int)
        ]
    , "error_type" .= errType
    , "suggestion" .= suggestion
    ]
    ++ maybe [] (\m -> ["available_methods" .= m]) availMethods
    ++ maybe [] (\s -> ["method_schema" .= s]) methodSchema

-- | Convert Provenance to Value
provenanceToValue :: PT.Provenance -> Value
provenanceToValue (PT.Provenance servers) =
  Aeson.toJSON servers


-- | Format transport error for display
formatTransportError :: PT.TransportError -> Text
formatTransportError (PT.ConnectionRefused h p) =
  "Connection refused to " <> h <> ":" <> T.pack (show p)
formatTransportError (PT.ConnectionTimeout h p) =
  "Connection timeout to " <> h <> ":" <> T.pack (show p)
formatTransportError (PT.ProtocolError msg) =
  "Protocol error: " <> msg
formatTransportError (PT.NetworkError msg) =
  "Network error: " <> msg
formatTransportError (PT.ConnectionLost msg _) =
  "Connection lost: " <> msg

-- | Simple timeout implementation
-- Returns Nothing if timeout occurs, Just result otherwise
timeout :: Int -> IO a -> IO (Maybe a)
timeout microseconds action = do
  resultMVar <- newEmptyMVar
  -- Start the action
  _ <- forkIO $ do
    result <- action
    putMVar resultMVar (Just result)
  -- Start the timeout thread
  _ <- forkIO $ do
    threadDelay microseconds
    putMVar resultMVar Nothing
  -- Wait for either to complete
  takeMVar resultMVar
