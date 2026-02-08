# Guidance System Implementation Status

**Status:** ✅ **COMPLETE** - Middleware removed, stream-based guidance active
**Date:** 2025-12-16
**Substrate Version:** main branch (commits: 22feb70, e4a5682, e45f097, 0682dd5)

## Summary

The stream-based error guidance system is **fully implemented and deployed**. The old `GuidedErrorMiddleware` has been **removed** from substrate. Frontends must update to handle guidance events.

## What Changed

### Before (Middleware-Based)

```json
// JSON-RPC error response
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32601,
    "message": "Activation 'foo' not found",
    "data": {
      "available_activations": ["arbor", "bash", "cone", "health"],
      "try": {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "substrate.schema",
        "params": []
      }
    }
  }
}
```

### After (Stream-Based)

```json
// Stream event 1: Guidance
{
  "plexus_hash": "7a1a43920ee194e1",
  "type": "guidance",
  "provenance": {"segments": ["plexus"]},
  "error_type": {
    "error_kind": "activation_not_found",
    "activation": "foo"
  },
  "suggestion": {
    "action": "call_substrate_schema"
  }
}

// Stream event 2: Error
{
  "plexus_hash": "7a1a43920ee194e1",
  "type": "error",
  "provenance": {"segments": ["plexus"]},
  "error": "Activation 'foo' not found",
  "fatal": false
}

// Stream event 3: Done
{
  "plexus_hash": "7a1a43920ee194e1",
  "type": "done",
  "provenance": {"segments": ["plexus"]}
}
```

## Implementation Status

| Phase | Description | Status | Commit |
|-------|-------------|--------|--------|
| **Phase 1** | Add Guidance Event Types | ✅ Complete | 22feb70 |
| **Phase 2** | Create Error Stream Helper | ✅ Complete | 22feb70 |
| **Phase 3** | Modify Plexus::call() | ✅ Complete | 22feb70 |
| **Phase 4** | Add Activation Override | ✅ Complete | 22feb70 |
| **Phase 5** | Bash Custom Guidance Example | ✅ Complete | e45f097 |
| **Phase 6** | Remove Middleware | ✅ Complete | 0682dd5 |
| **Phase 7** | Integration Tests | 🚧 In Progress | - |

### Phase 1: Guidance Event Types ✅

**File:** `src/plexus/types.rs`

Added new event types:
- `GuidanceErrorType` enum (ActivationNotFound, MethodNotFound, InvalidParams)
- `GuidanceSuggestion` enum (CallBackendSchema, CallActivationSchema, TryMethod, Custom)
- `PlexusStreamEvent::Guidance` variant
- `PlexusStreamItem::guidance()` constructor

**Tests:** 3 tests passing

### Phase 2: Error Stream Helper ✅

**File:** `src/plexus/guidance.rs` (new file)

Created guidance module with:
- `error_stream_with_guidance()` function
- `CustomGuidance` trait
- `ActivationGuidanceInfo` trait

Returns streams with: Guidance → Error → Done

**Tests:** 2 tests passing

### Phase 3: Plexus::call() Modified ✅

**File:** `src/plexus/plexus.rs`

Changed signature behavior:
- Still returns `Result<PlexusStream, PlexusError>`
- But **always returns `Ok(PlexusStream)`** (never `Err`)
- Errors are expressed as stream events

**Tests:** 2 tests passing

### Phase 4: Activation Custom Guidance ✅

**File:** `src/plexus/plexus.rs`

Added to `Activation` trait:
```rust
fn custom_guidance(&self, method: &str, error: &PlexusError) -> Option<GuidanceSuggestion> {
    None  // Default: no custom guidance
}
```

Activations can override to provide method-specific suggestions.

**Tests:** 1 test passing

### Phase 5: Bash Example ✅

**File:** `src/activations/bash/activation.rs`

Implemented custom guidance for bash activation:
```rust
fn custom_guidance(&self, method: &str, error: &PlexusError) -> Option<GuidanceSuggestion> {
    match (method, error) {
        ("execute", PlexusError::InvalidParams(_)) => {
            Some(GuidanceSuggestion::TryMethod {
                method: "bash_execute".to_string(),
                example_params: Some(json!("echo 'Hello, World!'")),
            })
        }
        _ => None,
    }
}
```

**Tests:** 3 tests passing

### Phase 6: Middleware Removed ✅

**Files:**
- `src/main.rs` - Removed middleware setup
- `src/plexus/middleware.rs` - Deprecated with notice
- `src/plexus/errors.rs` - Deprecated with notice
- `src/plexus/mod.rs` - Exports marked deprecated

The server now runs without middleware. Guidance is provided via stream events.

**Verification:** Server starts successfully ✅

### Phase 7: Integration Tests 🚧

**Status:** Unit tests complete, integration tests in progress

**Current tests (11 passing):**
- 3 guidance type serialization tests
- 2 error stream helper tests
- 2 plexus call behavior tests
- 1 custom guidance contract test
- 3 bash custom guidance tests

**Needed:**
- Integration tests with RPC client
- End-to-end guidance flow tests

## Frontend Migration Guide

### Quick Start: What Frontends Need to Do

1. **Subscribe to methods as before** - No change to subscription API
2. **Handle new event type** - Add handler for `type: "guidance"`
3. **Extract guidance info** - Use `error_type` and `suggestion` fields
4. **Map to next action** - Convert `action` to method call

### Step-by-Step Migration

#### 1. Add Guidance Event Handler

**TypeScript (Symbols):**
```typescript
type GuidanceEvent = {
  plexus_hash: string;
  type: "guidance";
  provenance: { segments: string[] };
  error_type: GuidanceErrorType;
  available_methods?: string[];
  method_schema?: Schema;
  suggestion: GuidanceSuggestion;
};

type GuidanceErrorType =
  | { error_kind: "activation_not_found"; activation: string }
  | { error_kind: "method_not_found"; activation: string; method: string }
  | { error_kind: "invalid_params"; method: string; reason: string };

type GuidanceSuggestion =
  | { action: "call_backend_schema" }
  | { action: "call_activation_schema"; namespace: string }
  | { action: "try_method"; method: string; example_params?: any }
  | { action: "custom"; message: string };
```

**Haskell:**
```haskell
data GuidanceEvent = GuidanceEvent
  { plexusHash :: Text
  , provenance :: Provenance
  , errorType :: GuidanceErrorType
  , availableMethods :: Maybe [Text]
  , methodSchema :: Maybe Schema
  , suggestion :: GuidanceSuggestion
  }

data GuidanceErrorType
  = ActivationNotFound { activation :: Text }
  | MethodNotFound { activation :: Text, method :: Text }
  | InvalidParams { method :: Text, reason :: Text }

data GuidanceSuggestion
  = CallBackendSchema
  | CallActivationSchema { namespace :: Text }
  | TryMethod { method :: Text, exampleParams :: Maybe Value }
  | Custom { message :: Text }
```

#### 2. Handle Guidance in Stream Processing

**TypeScript:**
```typescript
async function handleStream(subscription: AsyncIterator<Event>) {
  for await (const event of subscription) {
    switch (event.type) {
      case "guidance":
        handleGuidance(event as GuidanceEvent);
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
    }
  }
}

function handleGuidance(guidance: GuidanceEvent) {
  console.error(`Error: ${guidance.error_type.error_kind}`);

  // Show available methods if provided
  if (guidance.available_methods) {
    console.log("Available methods:", guidance.available_methods);
  }

  // Show suggestion
  const nextCall = getNextCall(guidance.suggestion);
  console.log("Try:", nextCall);
}

function getNextCall(suggestion: GuidanceSuggestion): { method: string; params: any[] } {
  switch (suggestion.action) {
    case "call_backend_schema":
      return { method: "substrate.schema", params: [] };
    case "call_activation_schema":
      return { method: "substrate.activation_schema", params: [suggestion.namespace] };
    case "try_method":
      return { method: suggestion.method, params: [suggestion.example_params] };
    case "custom":
      console.log("Suggestion:", suggestion.message);
      return null;
  }
}
```

**Haskell:**
```haskell
handleStream :: AsyncStream Event -> IO ()
handleStream stream = do
  event <- readStream stream
  case eventType event of
    "guidance" -> handleGuidance (parseGuidance event)
    "error" -> handleError event
    "data" -> handleData event
    "done" -> handleDone event

handleGuidance :: GuidanceEvent -> IO ()
handleGuidance guidance = do
  putStrLn $ "Error: " <> show (errorType guidance)

  -- Show available methods
  case availableMethods guidance of
    Just methods -> putStrLn $ "Available: " <> intercalate ", " methods
    Nothing -> pure ()

  -- Show suggestion
  let nextCall = getNextCall (suggestion guidance)
  putStrLn $ "Try: " <> show nextCall

getNextCall :: GuidanceSuggestion -> (Text, [Value])
getNextCall CallBackendSchema = ("substrate.schema", [])
getNextCall (CallActivationSchema ns) = ("substrate.activation_schema", [toJSON ns])
getNextCall (TryMethod method params) = (method, maybe [] pure params)
getNextCall (Custom msg) = error $ "Custom guidance: " <> msg
```

#### 3. Update Error Display

**Before:**
```typescript
// Parse error.data.try
if (error.data?.try) {
  console.log("Try calling:", error.data.try.method);
}
```

**After:**
```typescript
// Wait for guidance event before error event
const guidance = await getNextEvent(); // Should be type: "guidance"
const error = await getNextEvent();     // Then type: "error"

if (guidance.type === "guidance") {
  const nextCall = getNextCall(guidance.suggestion);
  console.log("Try calling:", nextCall.method);
}
```

### Information Parity: What You Gain

The new system provides **everything the middleware did, plus more:**

| Feature | Middleware | Stream Guidance |
|---------|-----------|-----------------|
| Error type | ✅ In message | ✅ Structured `error_type` |
| Next method to call | ✅ In `data.try` | ✅ In `suggestion.action` |
| Available activations | ✅ In `data.available_activations` | ✅ Via `call_backend_schema` |
| Available methods | ❌ Not provided | ✅ In `available_methods` array |
| Method schema | ❌ Not provided | ✅ In `method_schema` field |
| Example parameters | ❌ Not provided | ✅ In `suggestion.example_params` |
| Custom suggestions | ❌ Not possible | ✅ Activation-specific guidance |

## Testing the New System

### 1. Test Activation Not Found

**Request:**
```bash
synapse foo bar
```

**Expected Stream:**
```json
// Event 1: Guidance
{
  "type": "guidance",
  "error_type": {
    "error_kind": "activation_not_found",
    "activation": "foo"
  },
  "suggestion": {
    "action": "call_substrate_schema"
  }
}

// Event 2: Error
{
  "type": "error",
  "error": "Activation 'foo' not found"
}

// Event 3: Done
{
  "type": "done"
}
```

### 2. Test Method Not Found

**Request:**
```bash
synapse bash nonexistent
```

**Expected Stream:**
```json
// Event 1: Guidance
{
  "type": "guidance",
  "error_type": {
    "error_kind": "method_not_found",
    "activation": "bash",
    "method": "nonexistent"
  },
  "available_methods": ["execute"],
  "suggestion": {
    "action": "try_method",
    "method": "bash_execute",
    "example_params": null
  }
}
```

### 3. Test Invalid Params with Custom Guidance

**Request:**
```bash
synapse bash execute
```
(Missing required `command` parameter)

**Expected Stream:**
```json
// Event 1: Guidance (custom from bash activation)
{
  "type": "guidance",
  "error_type": {
    "error_kind": "invalid_params",
    "method": "execute",
    "reason": "expected string or object with 'command'"
  },
  "suggestion": {
    "action": "try_method",
    "method": "bash_execute",
    "example_params": "echo 'Hello, World!'"
  }
}
```

## Migration Checklist for Frontends

- [ ] **Update event handler** - Add case for `type: "guidance"`
- [ ] **Parse guidance structure** - Extract `error_type` and `suggestion`
- [ ] **Map actions to methods** - Convert `suggestion.action` to RPC call
- [ ] **Handle available_methods** - Display when provided
- [ ] **Handle example_params** - Show helpful examples to users
- [ ] **Update error display** - Show guidance before error
- [ ] **Remove middleware parsing** - Delete code that reads `error.data.try`
- [ ] **Test all error cases** - ActivationNotFound, MethodNotFound, InvalidParams
- [ ] **Test with substrate main branch** - Middleware is removed

## Rollback Plan

If frontends encounter issues, substrate can temporarily revert:

```bash
git revert 0682dd5  # Revert middleware removal
cargo build && cargo run
```

This will restore the middleware while frontends complete migration.

## Timeline

- **2025-12-16:** Phases 1-6 complete, middleware removed
- **Now:** Frontends should update to handle guidance events
- **Next:** Phase 7 integration tests completion

## Related Documentation

- **[Frontend Migration Guide](./16680880693241553663_frontend-guidance-migration.md)** - Detailed migration steps with code examples
- **[Stream-Based Errors Architecture](./16680881573410764543_guidance-stream-based-errors.md)** - Complete design and implementation plan
- **[Middleware Deprecation Plan](./16680878804024120575_guided-error-middleware-deprecation.md)** - What was removed and why
- **[Dynamic CLI Schemas](./16680891033387373567_dynamic-cli-type-driven-schemas.md)** - How frontends use schemas from guidance

## Contact

For questions about migration, see:
- Implementation: `src/plexus/guidance.rs`
- Example usage: `src/activations/bash/activation.rs`
- Tests: `src/plexus/plexus.rs` (guidance_tests module)
