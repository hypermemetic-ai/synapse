# Frontend Migration Guide: Stream-Based Guidance API

## Overview

Substrate now provides error guidance as **stream events** instead of JSON-RPC error data. When an error occurs, the stream yields a sequence of events: **Guidance → Error → Done**.

This document describes the new API structure and how frontends (like Symbols) should handle it.

## What Changed

### Before (Legacy Middleware Approach)

Errors were returned as JSON-RPC errors with guidance in the error data:

```json
// JSON-RPC error response (legacy)
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32600,
    "message": "Activation 'foo' not found",
    "data": {
      "try": {
        "method": "substrate.schema",
        "params": []
      },
      "context": {
        "available_activations": ["arbor", "bash", "cone", "health"]
      }
    }
  }
}
```

**Problem**: This required middleware that duplicated activation lookup logic and had incomplete coverage.

### After (Stream-Based Guidance)

Errors are returned as **successful subscriptions** that yield guidance streams:

```json
// Successful subscription (new)
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": "subscription_id_123"
}

// Stream event 1: Guidance
{
  "plexus_hash": "7a1a43920ee194e1",
  "type": "guidance",
  "provenance": {
    "segments": ["plexus"]
  },
  "error_kind": "activation_not_found",
  "activation": "foo",
  "action": "call_substrate.schema"
}

// Stream event 2: Error
{
  "plexus_hash": "7a1a43920ee194e1",
  "type": "error",
  "provenance": {
    "segments": ["plexus"]
  },
  "error": "Activation not found: foo",
  "recoverable": false
}

// Stream event 3: Done
{
  "plexus_hash": "7a1a43920ee194e1",
  "type": "done",
  "provenance": {
    "segments": ["plexus"]
  }
}
```

**Benefits**: Guidance flows through the same stream system as data, includes richer context, and covers all error types.

## New Event Type: Guidance

### Core Structure

Every guidance event has these fields:

```typescript
interface GuidanceEvent {
  // Standard stream fields
  plexus_hash: string;
  type: "guidance";
  provenance: {
    segments: string[];
  };

  // Flattened error type (one of these groups)
  error_kind: "activation_not_found" | "method_not_found" | "invalid_params";

  // Optional context
  available_methods?: string[];
  method_schema?: Schema;

  // Flattened suggestion (one of these groups)
  action: "call_substrate.schema" | "call_activation_schema" | "try_method" | "custom";
}
```

### Error Types

#### 1. ActivationNotFound

Triggered when namespace doesn't exist (e.g., `foo.bar` where `foo` is unknown).

```json
{
  "type": "guidance",
  "error_kind": "activation_not_found",
  "activation": "foo",
  "action": "call_substrate.schema"
}
```

**Frontend action**: Suggest running `substrate.schema` to discover available activations.

#### 2. MethodNotFound

Triggered when activation exists but method doesn't (e.g., `bash.invalid`).

```json
{
  "type": "guidance",
  "error_kind": "method_not_found",
  "activation": "bash",
  "method": "invalid",
  "available_methods": ["execute"],
  "action": "try_method",
  "method": "bash_execute",
  "example_params": null
}
```

**Frontend action**:
- List available methods from `available_methods`
- Suggest trying the `method` from suggestion
- If `example_params` is present, show example usage

#### 3. InvalidParams

Triggered when method parameters don't match schema.

```json
{
  "type": "guidance",
  "error_kind": "invalid_params",
  "method": "bash.execute",
  "reason": "missing required parameter: command",
  "method_schema": {
    "type": "object",
    "properties": {
      "command": {
        "type": "string",
        "description": "Shell command to execute"
      }
    },
    "required": ["command"]
  },
  "action": "call_activation_schema",
  "namespace": "bash"
}
```

**Frontend action**:
- Show the schema for the attempted method
- Highlight required vs optional parameters
- Suggest calling `substrate.activation_schema` for full details

### Suggestion Actions

The `action` field tells frontends what to suggest:

#### 1. CallPlexusSchema

```json
{
  "action": "call_substrate.schema"
}
```

**Meaning**: User should call `substrate.schema` to discover available activations.

**Example CLI output**:
```
Error: Activation 'foo' not found

Try: synapse --help
Or:  synapse schema
```

#### 2. CallActivationSchema

```json
{
  "action": "call_activation_schema",
  "namespace": "bash"
}
```

**Meaning**: User should call `substrate.activation_schema` with the namespace.

**Example CLI output**:
```
Error: Invalid parameters for bash.execute

Try: synapse bash --help
Or:  synapse schema bash
```

#### 3. TryMethod

```json
{
  "action": "try_method",
  "method": "bash_execute",
  "example_params": "echo 'Hello, World!'"
}
```

**Meaning**: User should try calling this specific method with example params.

**Example CLI output**:
```
Error: Method 'invalid' not found in activation 'bash'

Available methods: execute

Try: synapse bash execute "echo 'Hello, World!'"
```

#### 4. Custom

```json
{
  "action": "custom",
  "message": "The command parameter must be a valid shell command"
}
```

**Meaning**: Activation provided custom guidance message.

**Example CLI output**:
```
Error: Invalid parameters

Suggestion: The command parameter must be a valid shell command
```

## Stream Flow Patterns

### Success Response (No Guidance)

```
Data → Data → ... → Done
```

No guidance events appear when calls succeed.

### Error Response (With Guidance)

```
Guidance → Error → Done
```

All errors (except ExecutionError) include guidance.

### ExecutionError (No Guidance)

```
Error → Done
```

Runtime execution errors don't include guidance (e.g., bash command failed).

## Frontend Implementation Guide

### 1. Subscribe to Methods

No changes needed - subscription API is unchanged:

```typescript
const subscription = await client.subscribe(
  "bash_execute",
  ["echo hello"],
  "unsubscribe_execute"
);
```

### 2. Handle Stream Events

Update event handler to recognize `guidance` type:

```typescript
for await (const event of subscription) {
  switch (event.type) {
    case "guidance":
      handleGuidance(event);
      break;
    case "error":
      handleError(event);
      break;
    case "data":
      handleData(event);
      break;
    case "done":
      handleDone(event);
      break;
    default:
      // Ignore unknown event types (forward compatibility)
      console.warn("Unknown event type:", event.type);
  }
}
```

### 3. Implement Guidance Handler

Extract actionable information from guidance events:

```typescript
function handleGuidance(event: GuidanceEvent): void {
  // Store guidance for error context
  const guidance = parseGuidance(event);

  // Wait for accompanying error event
  // (guidance is informational, error has the message)
}

function parseGuidance(event: GuidanceEvent): GuidanceInfo {
  switch (event.error_kind) {
    case "activation_not_found":
      return {
        problem: `Activation '${event.activation}' not found`,
        suggestion: "Run 'synapse --help' to see available activations",
        nextAction: "call_substrate.schema"
      };

    case "method_not_found":
      return {
        problem: `Method '${event.method}' not found in '${event.activation}'`,
        suggestion: event.available_methods
          ? `Available methods: ${event.available_methods.join(", ")}`
          : `Run 'synapse ${event.activation} --help'`,
        nextAction: event.action === "try_method"
          ? buildTryMethodCommand(event)
          : `synapse schema ${event.activation}`
      };

    case "invalid_params":
      return {
        problem: `Invalid parameters: ${event.reason}`,
        suggestion: event.method_schema
          ? formatSchemaHelp(event.method_schema)
          : `Run 'synapse schema ${event.namespace}'`,
        nextAction: event.method_schema ? null : `call_activation_schema`
      };
  }
}
```

### 4. Combine Guidance with Error

Display guidance context when error arrives:

```typescript
let pendingGuidance: GuidanceInfo | null = null;

for await (const event of subscription) {
  switch (event.type) {
    case "guidance":
      pendingGuidance = parseGuidance(event);
      break;

    case "error":
      if (pendingGuidance) {
        console.error(pendingGuidance.problem);
        console.error("  Error:", event.error);
        console.log("\n" + pendingGuidance.suggestion);
        if (pendingGuidance.nextAction) {
          console.log("Try:", pendingGuidance.nextAction);
        }
        pendingGuidance = null;
      } else {
        // ExecutionError - no guidance
        console.error("Error:", event.error);
      }
      break;
  }
}
```

## Example Output Formatting

### Symbols CLI Implementation

For the Symbols Haskell CLI, here's how to format guidance:

```haskell
data GuidanceInfo = GuidanceInfo
  { guidanceErrorKind :: Text
  , guidanceSuggestion :: SuggestionAction
  , guidanceAvailableMethods :: Maybe [Text]
  , guidanceMethodSchema :: Maybe Schema
  }

handleGuidanceEvent :: Value -> IO ()
handleGuidanceEvent event = do
  let guidance = parseGuidance event
  -- Store for next error event
  putMVar guidanceVar guidance

handleErrorEvent :: Value -> Maybe GuidanceInfo -> IO ()
handleErrorEvent event mbGuidance = case mbGuidance of
  Nothing -> do
    -- ExecutionError - no guidance
    putStrLn $ "Error: " <> errorMessage event

  Just guidance -> do
    -- Error with guidance
    putStrLn $ formatErrorWithGuidance event guidance

formatErrorWithGuidance :: Value -> GuidanceInfo -> Text
formatErrorWithGuidance errorEvent guidance =
  unlines
    [ "Error: " <> errorMessage errorEvent
    , ""
    , formatSuggestion (guidanceSuggestion guidance)
    ]

formatSuggestion :: SuggestionAction -> Text
formatSuggestion = \case
  CallPlexusSchema ->
    "Try: synapse --help"

  CallActivationSchema ns ->
    "Try: synapse " <> ns <> " --help"

  TryMethod method (Just params) ->
    "Try: synapse " <> formatMethodCall method params

  TryMethod method Nothing ->
    "Available method: " <> method

  Custom msg ->
    "Suggestion: " <> msg
```

## Backward Compatibility

### Safe Forward Compatibility

Frontends that ignore unknown event types will continue to work:

```typescript
// Old code that only handles data/error/done
switch (event.type) {
  case "data":
    handleData(event);
    break;
  case "error":
    handleError(event);  // Still works!
    break;
  case "done":
    handleDone(event);
    break;
  // "guidance" events are silently ignored
}
```

**Result**: Frontends see error messages but miss helpful suggestions.

### Migration Strategy

1. **Deploy substrate** with stream-based guidance
2. **Update frontends** to handle guidance events (optional)
3. **Remove legacy middleware** (Phase 6) - no breaking change since frontends already migrated

### Breaking Changes

**None** - this is a purely additive change:
- Old frontends ignore `guidance` events
- Error messages still appear via `error` events
- Successful responses unchanged

## Testing Your Implementation

### Test 1: Unknown Activation

```bash
# Call unknown activation
synapse foo bar

# Expected stream:
# 1. Guidance: { error_kind: "activation_not_found", ... }
# 2. Error: { error: "Activation not found: foo" }
# 3. Done
```

**Verify**:
- Guidance event arrives first
- Error event includes "Activation not found"
- Suggestion points to `substrate.schema`

### Test 2: Unknown Method

```bash
# Call unknown method on bash
synapse bash invalid

# Expected stream:
# 1. Guidance: { error_kind: "method_not_found", available_methods: ["execute"], ... }
# 2. Error: { error: "Method not found: bash.invalid" }
# 3. Done
```

**Verify**:
- `available_methods` includes "execute"
- Suggestion provides example call

### Test 3: Invalid Parameters

```bash
# Call bash.execute without required params
synapse bash execute

# Expected stream:
# 1. Guidance: { error_kind: "invalid_params", method_schema: {...}, ... }
# 2. Error: { error: "Invalid params: ..." }
# 3. Done
```

**Verify**:
- `method_schema` includes parameter details
- Suggestion shows required vs optional fields

### Test 4: Successful Call (No Guidance)

```bash
# Successful call
synapse bash execute "echo hello"

# Expected stream:
# 1. Data: { type: "data", content_type: "bash.stdout", ... }
# 2. Data: { type: "data", content_type: "bash.exit", ... }
# 3. Done
```

**Verify**:
- No guidance events
- Normal data flow

## Full Example: Symbols Integration

Here's a complete example for the Symbols Haskell CLI:

```haskell
module Symbols.Guidance where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T

-- Parse guidance event
data GuidanceEvent = GuidanceEvent
  { geErrorKind :: ErrorKind
  , geAction :: SuggestionAction
  , geAvailableMethods :: Maybe [Text]
  , geMethodSchema :: Maybe Schema
  } deriving (Show, Eq)

data ErrorKind
  = ActivationNotFound { activation :: Text }
  | MethodNotFound { activation :: Text, method :: Text }
  | InvalidParams { method :: Text, reason :: Text }
  deriving (Show, Eq)

data SuggestionAction
  = CallPlexusSchema
  | CallActivationSchema { namespace :: Text }
  | TryMethod { method :: Text, exampleParams :: Maybe Value }
  | Custom { message :: Text }
  deriving (Show, Eq)

-- Parse from JSON
instance FromJSON GuidanceEvent where
  parseJSON = withObject "GuidanceEvent" $ \o -> do
    errorKind <- parseErrorKind o
    action <- parseSuggestionAction o
    availableMethods <- o .:? "available_methods"
    methodSchema <- o .:? "method_schema"
    pure $ GuidanceEvent errorKind action availableMethods methodSchema

parseErrorKind :: Object -> Parser ErrorKind
parseErrorKind o = do
  kind <- o .: "error_kind"
  case kind of
    "activation_not_found" -> ActivationNotFound <$> o .: "activation"
    "method_not_found" -> MethodNotFound <$> o .: "activation" <*> o .: "method"
    "invalid_params" -> InvalidParams <$> o .: "method" <*> o .: "reason"
    _ -> fail $ "Unknown error_kind: " <> kind

parseSuggestionAction :: Object -> Parser SuggestionAction
parseSuggestionAction o = do
  action <- o .: "action"
  case action of
    "call_substrate.schema" -> pure CallPlexusSchema
    "call_activation_schema" -> CallActivationSchema <$> o .: "namespace"
    "try_method" -> TryMethod <$> o .: "method" <*> o .:? "example_params"
    "custom" -> Custom <$> o .: "message"
    _ -> fail $ "Unknown action: " <> action

-- Format for CLI display
formatGuidance :: GuidanceEvent -> Text
formatGuidance GuidanceEvent{..} =
  case geAction of
    CallPlexusSchema ->
      "Try: synapse --help"

    CallActivationSchema ns ->
      T.unlines
        [ "Available activations can be listed with:"
        , "  synapse --help"
        , ""
        , "For " <> ns <> " schema:"
        , "  synapse " <> ns <> " --help"
        ]

    TryMethod method (Just params) | Just methods <- geAvailableMethods ->
      T.unlines
        [ "Available methods: " <> T.intercalate ", " methods
        , ""
        , "Try: synapse " <> formatMethodInvocation method params
        ]

    TryMethod method Nothing ->
      "Try: synapse --help " <> method

    Custom msg ->
      "Suggestion: " <> msg

-- Main event loop
processStream :: Subscription -> IO ()
processStream sub = do
  guidanceRef <- newIORef Nothing

  runConduit $
    subscriptionSource sub
    .| awaitForever (\event -> liftIO $ handleEvent guidanceRef event)

handleEvent :: IORef (Maybe GuidanceEvent) -> Value -> IO ()
handleEvent guidanceRef event = do
  case Aeson.parseMaybe (.: "type") event of
    Just "guidance" -> do
      -- Store guidance for next error
      case Aeson.fromJSON event of
        Success guidance -> writeIORef guidanceRef (Just guidance)
        Error err -> putStrLn $ "Failed to parse guidance: " <> err

    Just "error" -> do
      -- Check if we have pending guidance
      mbGuidance <- readIORef guidanceRef
      writeIORef guidanceRef Nothing

      let errorMsg = event ^? key "error" . _String

      case mbGuidance of
        Just guidance -> do
          putStrLn $ "Error: " <> T.unpack (fromMaybe "Unknown error" errorMsg)
          putStrLn ""
          putStrLn $ T.unpack $ formatGuidance guidance

        Nothing -> do
          -- ExecutionError - no guidance
          putStrLn $ "Error: " <> T.unpack (fromMaybe "Unknown error" errorMsg)

    Just "data" ->
      handleDataEvent event

    Just "done" ->
      return ()

    _ ->
      -- Unknown event type - ignore (forward compatibility)
      return ()
```

## Migration Checklist

### For Symbols Frontend

- [ ] Add `GuidanceEvent` data type with parser
- [ ] Update stream event handler to recognize `type: "guidance"`
- [ ] Store guidance events for pairing with errors
- [ ] Format guidance suggestions for CLI display
- [ ] Add tests for guidance event parsing
- [ ] Add tests for formatted output
- [ ] Update help text to mention enhanced error messages

### For Other Frontends

- [ ] Parse `type: "guidance"` from stream events
- [ ] Extract `error_kind`, `action`, and optional fields
- [ ] Pair guidance with subsequent error event
- [ ] Format suggestions appropriate for your UI:
  - CLI: Plain text with "Try: ..." suggestions
  - Web UI: Clickable links to schema/help pages
  - IDE: Quick-fix actions with code snippets
- [ ] Test all error scenarios (activation not found, method not found, invalid params)
- [ ] Add forward compatibility (ignore unknown event types)

## Related Documentation

- [Stream-Based Guidance Architecture](./16680881573410764543_guidance-stream-based-errors.md) - Implementation details
- [Dynamic CLI Type-Driven Schemas](./16680891033387373567_dynamic-cli-type-driven-schemas.md) - How schemas work
- [Testing Strategy](./16680885909985432575_testing-strategy.md) - Test coverage for guidance

## Support

If you encounter issues during migration:

1. **Check stream shape**: Ensure `Guidance → Error → Done` sequence
2. **Verify JSON parsing**: Test with example events from this doc
3. **Test error scenarios**: Try unknown activations/methods to see guidance in action
4. **Check plexus_hash**: All events should include hash for cache invalidation
