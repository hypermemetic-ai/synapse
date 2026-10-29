# Error Handling Improvements: Synapse & Substrate

**Date:** 2025-01-25
**Status:** Proposed
**Scope:** Synapse CLI (Haskell) + Substrate/Plexus Backend (Rust)

## Executive Summary

This document proposes comprehensive improvements to error handling across the Plexus stack. Current errors lack context, making debugging difficult for users. We propose structured error types in Substrate, enriched error context in Synapse, and a phased implementation plan that delivers immediate value while building toward a robust error system.

**Key Goals:**
- Provide actionable error messages with suggestions
- Include sufficient context for debugging
- Distinguish error categories for programmatic handling
- Maintain backward compatibility during migration

## Current State Analysis

### Synapse Error Architecture

Synapse uses a well-structured error type hierarchy:

```haskell
data SynapseError
  = NavError NavError          -- Path navigation errors
  | TransportError Text        -- Network/RPC errors (LACKS CONTEXT)
  | ParseError Text            -- Parameter parsing errors
  | ValidationError Text       -- Validation errors

data NavError
  = NotFound Text Path         -- Segment not found
  | MethodNotTerminal Text Path
  | Cycle PluginHash Path
  | FetchError Text Path
```

**Strengths:**
- Clear type-based distinction between error sources
- Path information preserved in navigation errors
- Structured parse error variants with suggestions

**Weaknesses:**
1. **TransportError is opaque** - just a text message, no context about method/params
2. **Parse errors don't suggest help** - no `--help` hints
3. **Template rendering failures are silent** - users don't know why output looks wrong
4. **Cycle detection doesn't show traversal path**
5. **Connection errors lack troubleshooting guidance**

### Substrate/Plexus Error Model

Backend errors arrive as streaming items:

```rust
// From Plexus protocol
enum StreamItem {
    Data(Value),
    Progress { message: String, details: Option<Value> },
    Error { message: String, recoverable: bool },  // MINIMAL!
    Done,
    Guidance { message: String },
}
```

**Strengths:**
- Streaming error model allows partial results
- Recoverable flag indicates operation continuability
- Guidance messages provide user help

**Weaknesses:**
1. **No error codes** - can't programmatically distinguish error types
2. **No structured error data** - just a message string
3. **No field-level validation details** - which param failed?
4. **No suggestions** - doesn't tell user how to fix
5. **Guidance underutilized** - rarely sent before errors

## Problem Statement

### User Experience Issues

**Scenario 1: Unknown Parameter**
```bash
$ synapse cone chat --id test --promtp "hello"
Parse error: Unknown parameter: --promtp
```

**Issues:**
- Doesn't suggest `--prompt` (typo correction)
- No hint to run `--help`
- Doesn't show valid parameters

**Scenario 2: Connection Failure**
```bash
$ synapse cone list
Transport error: Connection refused
```

**Issues:**
- No troubleshooting guidance
- Doesn't mention Substrate server
- No hint about how to start server
- No indication of expected endpoint

**Scenario 3: Invalid Parameter Value**
```bash
$ synapse cone create --model invalid --name test
Error: Invalid params
```

**Issues:**
- Doesn't say which param is invalid
- Doesn't show valid options
- No suggestion for correction
- Error comes from backend but lacks detail

### Developer Experience Issues

**Debugging Challenges:**
1. Can't correlate errors across Synapse/Substrate boundary
2. No request IDs for tracing
3. Silent failures in template rendering
4. Insufficient logging at error boundaries

## Proposed Solution

### Architecture Principles

1. **Contextual Errors**: Every error includes sufficient context to understand what failed
2. **Actionable Messages**: Errors suggest how to fix the problem
3. **Layered Detail**: Default messages for users, verbose mode for debugging
4. **Structured Data**: Machine-readable error information
5. **Backward Compatible**: Graceful degradation for old clients

### Phase 1: Synapse Quick Wins (No Backend Changes)

#### 1.1 Enhanced Parse Error Messages

**File:** `app/Main.hs` (renderParseError function)

```haskell
renderParseError :: [Text] -> Parse.ParseError -> String
renderParseError validParams = \case
  Parse.UnknownParam name ->
    "Unknown parameter: --" <> T.unpack name <> "\n\n"
    <> suggestSimilar name validParams
    <> "\nRun '" <> programName <> " --help' for available parameters."

  Parse.MissingRequired name ->
    "Missing required parameter: --" <> T.unpack name <> "\n\n"
    <> "This parameter is mandatory for this command.\n"
    <> "Run '" <> programName <> " --help' for usage examples."

  Parse.InvalidValue name reason ->
    "Invalid value for --" <> T.unpack name <> ":\n"
    <> "  " <> T.unpack reason <> "\n\n"
    <> "Run '" <> programName <> " --help' for expected format."

  -- ... other cases with similar enhancements

suggestSimilar :: Text -> [Text] -> String
suggestSimilar input valid =
  case findCloseMatches input valid of
    [] -> ""
    [match] -> "Did you mean: --" <> T.unpack match <> "?\n"
    matches -> "Did you mean one of:\n"
            <> unlines (map (\m -> "  --" <> T.unpack m) matches)
```

**Implementation:**
- Add Levenshtein distance for typo suggestions
- Include command path in help hint
- Show valid options when relevant

#### 1.2 Connection Error Diagnostics

**File:** `src/Synapse/Transport.hs`

```haskell
data TransportError = TransportError
  { teCategory :: TransportErrorCategory
  , teMessage :: Text
  , teEndpoint :: Maybe Text
  , teSuggestion :: Maybe Text
  }

data TransportErrorCategory
  = ConnectionRefused
  | Timeout
  | InvalidResponse
  | ProtocolError
  deriving (Show, Eq)

renderTransportError :: TransportError -> String
renderTransportError te = case teCategory te of
  ConnectionRefused ->
    "Cannot connect to Plexus server" <> endpointInfo <> "\n\n"
    <> "Possible causes:\n"
    <> "  • Substrate server is not running\n"
    <> "  • Wrong host/port configuration\n"
    <> "  • Firewall blocking WebSocket connection\n\n"
    <> "To start Substrate:\n"
    <> "  substrate run\n\n"
    <> "To check if port is in use:\n"
    <> "  lsof -i :4444"

  Timeout ->
    "Request timed out" <> endpointInfo <> "\n\n"
    <> T.unpack (teMessage te) <> "\n\n"
    <> "The server may be overloaded or unresponsive.\n"
    <> "Try again or check server logs."

  InvalidResponse ->
    "Received invalid response from server" <> endpointInfo <> "\n\n"
    <> T.unpack (teMessage te) <> "\n\n"
    <> "This may indicate a protocol version mismatch.\n"
    <> "Ensure Synapse and Substrate versions are compatible."

  ProtocolError ->
    "Protocol error: " <> T.unpack (teMessage te)
  where
    endpointInfo = case teEndpoint te of
      Just ep -> " at " <> T.unpack ep
      Nothing -> ""
```

#### 1.3 Verbose/Debug Mode

**File:** `src/Synapse/Monad.hs`

```haskell
data SynapseConfig = SynapseConfig
  { scEndpoint :: Text
  , scTimeout :: Int
  , scVerbose :: Bool        -- NEW: Show detailed error context
  , scDebug :: Bool          -- NEW: Show full RPC traces
  , scTemplateCache :: TemplateCache
  }

-- In error rendering:
renderWithContext :: SynapseConfig -> SynapseError -> String
renderWithContext cfg err =
  baseMessage err <>
  if scVerbose cfg then verboseContext err else "" <>
  if scDebug cfg then debugTrace err else ""

verboseContext :: SynapseError -> String
verboseContext = \case
  TransportError te -> case teMethod te of
    Just method -> "\n\nContext:\n"
                <> "  Method: " <> T.unpack method <> "\n"
                <> "  Params: " <> show (teParams te)
    Nothing -> ""
  NavError (Cycle hash path) ->
    "\n\nTraversal path:\n" <> showTraversal path
  _ -> ""
```

**CLI Flags:**
```bash
synapse --verbose cone chat ...  # Show error context
synapse --debug cone chat ...    # Show full RPC traces
```

#### 1.4 Template Rendering Error Visibility

**File:** `src/Synapse/Renderer.hs`

```haskell
renderItem :: SynapseConfig -> ServiceStreamItem -> IO (Maybe Text)
renderItem cfg item = do
  result <- runExceptT $ renderWithTemplate cfg item
  case result of
    Right text -> pure (Just text)
    Left err
      | scVerbose (rcConfig cfg) -> do
          hPutStrLn stderr $ "Warning: Template render failed: " <> show err
          hPutStrLn stderr $ "Falling back to raw output"
          pure Nothing
      | otherwise -> pure Nothing
```

### Phase 2: Synapse Enhanced Context (No Backend Changes)

#### 2.1 Rich Transport Errors

**File:** `src/Synapse/Monad.hs`

```haskell
data TransportError = TransportError
  { teCategory :: TransportErrorCategory
  , teMessage :: Text
  , teMethod :: Maybe Text           -- Which method was called
  , teParams :: Maybe Value          -- What params were sent
  , teRequestId :: Maybe Text        -- For correlation
  , teEndpoint :: Maybe Text         -- WS endpoint
  , teSuggestion :: Maybe Text       -- How to fix
  }

-- Usage in Transport.hs:
throwTransport :: Text -> Text -> Value -> Text -> SynapseM a
throwTransport method params endpoint msg =
  throwError $ TransportError TransportError
    { teCategory = categorizeError msg
    , teMessage = msg
    , teMethod = Just method
    , teParams = Just params
    , teRequestId = Nothing  -- TODO: get from RPC layer
    , teEndpoint = Just endpoint
    , teSuggestion = Nothing
    }
```

#### 2.2 Navigation Breadcrumbs

**File:** `src/Synapse/Algebra/Navigate.hs`

```haskell
data NavError
  = NotFound Text Path [Text]           -- Add: suggestions
  | MethodNotTerminal Text Path
  | Cycle PluginHash Path [PluginHash]  -- Add: full traversal
  | FetchError Text Path (Maybe Text)   -- Add: suggestion

-- Enhanced rendering:
renderNavError :: NavError -> String
renderNavError = \case
  NotFound seg path suggestions ->
    "Not found: '" <> T.unpack seg <> "' at path:\n"
    <> showBreadcrumb path seg <> "\n\n"
    <> case suggestions of
         [] -> "No similar commands found."
         [s] -> "Did you mean: " <> T.unpack s <> "?"
         ss -> "Did you mean one of:\n"
            <> unlines (map (("  • " <>) . T.unpack) ss)

  Cycle hash path traversal ->
    "Cycle detected in plugin schema:\n\n"
    <> showTraversal traversal
    <> "\n\nPlugin hash " <> T.unpack hash
    <> " was already visited at position "
    <> show (elemIndex hash traversal) <> ".\n\n"
    <> "This indicates a circular dependency in the plugin configuration."

showBreadcrumb :: Path -> Text -> String
showBreadcrumb path current =
  "  " <> intercalate " → " (map T.unpack path) <> " → " <> T.unpack current
       <> "\n  " <> replicate (length (intercalate " → " (map T.unpack path)) + 4) ' '
       <> "^^^^"
```

#### 2.3 Aggregated Parse Errors with Priority

**File:** `app/Main.hs`

```haskell
data ErrorPriority = Critical | Important | Minor
  deriving (Eq, Ord)

prioritizeParseErrors :: [Parse.ParseError] -> [(ErrorPriority, Parse.ParseError)]
prioritizeParseErrors = map classify
  where
    classify err = case err of
      Parse.MissingRequired _ -> (Critical, err)
      Parse.InvalidValue _ _ -> (Important, err)
      Parse.UnknownParam _ -> (Minor, err)
      _ -> (Important, err)

renderParseErrors :: [Parse.ParseError] -> String
renderParseErrors errs =
  let sorted = sortOn fst (prioritizeParseErrors errs)
      grouped = groupBy (\a b -> fst a == fst b) sorted
  in unlines $ concat
    [ renderGroup "Required parameters missing:" Critical grouped
    , renderGroup "Invalid values:" Important grouped
    , renderGroup "Unknown parameters:" Minor grouped
    ]
    <> ["\nRun '" <> programName <> " --help' for more information."]
```

### Phase 3: Substrate/Plexus Backend Improvements

#### 3.1 Structured Error Response Schema

**File:** `substrate/plexus-protocol/src/error.rs` (new)

```rust
use serde::{Deserialize, Serialize};
use serde_json::Value;

/// JSON-RPC 2.0 error codes
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ErrorCode {
    // Standard JSON-RPC
    ParseError = -32700,
    InvalidRequest = -32600,
    MethodNotFound = -32601,
    InvalidParams = -32602,
    InternalError = -32603,

    // Plexus extensions (application-specific range: -32000 to -32099)
    ValidationError = -32001,
    NotFound = -32002,
    Unauthorized = -32003,
    Conflict = -32004,
    StateError = -32005,
}

/// Structured error data for detailed diagnostics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ErrorData {
    /// High-level error category for programmatic handling
    pub category: ErrorCategory,

    /// Parameter/field that caused the error (if applicable)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub field: Option<String>,

    /// Expected value or constraint
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expected: Option<String>,

    /// Actual value received
    #[serde(skip_serializing_if = "Option::is_none")]
    pub actual: Option<Value>,

    /// Human-readable suggestion for fixing
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suggestion: Option<String>,

    /// Link to relevant documentation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs_url: Option<String>,

    /// Whether operation can be retried
    pub recoverable: bool,

    /// Request ID for correlation (from JSON-RPC request)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_id: Option<String>,

    /// Additional context for debugging
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ErrorCategory {
    Validation,
    NotFound,
    Authentication,
    Authorization,
    Conflict,
    RateLimit,
    Internal,
    Network,
    Timeout,
}

/// Structured error for streaming protocol
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlexusError {
    /// JSON-RPC error code
    pub code: ErrorCode,

    /// Human-readable error message
    pub message: String,

    /// Structured error data
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<ErrorData>,
}

impl PlexusError {
    /// Create a validation error for invalid parameter
    pub fn invalid_param(
        field: &str,
        expected: &str,
        actual: Value,
        suggestion: Option<String>,
    ) -> Self {
        PlexusError {
            code: ErrorCode::InvalidParams,
            message: format!("Invalid parameter: {}", field),
            data: Some(ErrorData {
                category: ErrorCategory::Validation,
                field: Some(field.to_string()),
                expected: Some(expected.to_string()),
                actual: Some(actual),
                suggestion,
                docs_url: None,
                recoverable: true,
                request_id: None,
                context: None,
            }),
        }
    }

    /// Create a not found error
    pub fn not_found(resource: &str, id: &str) -> Self {
        PlexusError {
            code: ErrorCode::NotFound,
            message: format!("{} not found: {}", resource, id),
            data: Some(ErrorData {
                category: ErrorCategory::NotFound,
                field: Some("id".to_string()),
                expected: Some(format!("existing {} identifier", resource)),
                actual: Some(Value::String(id.to_string())),
                suggestion: Some(format!("Use '{}_list' to see available resources", resource)),
                docs_url: None,
                recoverable: false,
                request_id: None,
                context: None,
            }),
        }
    }
}
```

#### 3.2 Enhanced Stream Error Item

**File:** `substrate/plexus-protocol/src/stream.rs`

```rust
use crate::error::PlexusError;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum StreamItem {
    Data {
        hash: String,
        provenance: Provenance,
        sequence: u64,
        data: Value,
    },

    Progress {
        hash: String,
        provenance: Provenance,
        sequence: u64,
        message: String,
        percent: Option<f64>,
        details: Option<Value>,
    },

    /// Enhanced error with structured data
    Error {
        hash: String,
        provenance: Provenance,
        sequence: u64,
        error: PlexusError,  // Was: just String + bool
    },

    /// User guidance - suggestions before errors
    Guidance {
        hash: String,
        provenance: Provenance,
        sequence: u64,
        message: String,
        severity: GuidanceSeverity,
        context: Option<Value>,
    },

    Done {
        hash: String,
        provenance: Provenance,
        sequence: u64,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GuidanceSeverity {
    Info,      // FYI
    Warning,   // Might be wrong but continuing
    Error,     // Definitely wrong, error follows
}
```

#### 3.3 Validation Error Aggregation

**File:** `substrate/plugins/*/src/validation.rs` (pattern for all plugins)

```rust
use plexus_protocol::error::{PlexusError, ErrorCode, ErrorData, ErrorCategory};

/// Validate parameters and collect all errors
pub fn validate_params(params: &Value) -> Result<(), Vec<PlexusError>> {
    let mut errors = Vec::new();

    // Validate each field
    if let Some(model_id) = params.get("model_id") {
        if let Err(e) = validate_model_id(model_id) {
            errors.push(e);
        }
    } else {
        errors.push(PlexusError::invalid_param(
            "model_id",
            "string matching pattern ^(gpt-|claude-)",
            Value::Null,
            Some("Use 'claude-3-haiku-20240307' or 'gpt-4o-mini'".to_string()),
        ));
    }

    if let Some(name) = params.get("name") {
        if let Err(e) = validate_name(name) {
            errors.push(e);
        }
    }

    // Return all errors or success
    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

/// Validate model ID format
fn validate_model_id(value: &Value) -> Result<String, PlexusError> {
    let model_str = value.as_str()
        .ok_or_else(|| PlexusError::invalid_param(
            "model_id",
            "string",
            value.clone(),
            None,
        ))?;

    let valid_prefixes = ["claude-", "gpt-", "anthropic."];
    if !valid_prefixes.iter().any(|p| model_str.starts_with(p)) {
        return Err(PlexusError::invalid_param(
            "model_id",
            "string with prefix: claude-, gpt-, or anthropic.",
            value.clone(),
            Some(format!(
                "Valid examples: claude-3-haiku-20240307, gpt-4o-mini. \
                 Run 'cone_registry' to see available models."
            )),
        ));
    }

    Ok(model_str.to_string())
}
```

#### 3.4 Proactive Guidance Messages

**File:** `substrate/plugins/cone/src/lib.rs`

```rust
use plexus_protocol::stream::{StreamItem, GuidanceSeverity};

impl ConePlugin {
    pub async fn chat(&self, params: ChatParams) -> Result<(), PlexusError> {
        // Send guidance before potential errors
        if params.ephemeral.unwrap_or(false) {
            self.emit_guidance(
                "Using ephemeral mode: messages will not be saved to history",
                GuidanceSeverity::Info,
                None,
            ).await;
        }

        // Validate cone exists
        let cone = match self.get_cone(&params.identifier).await {
            Ok(c) => c,
            Err(_) => {
                // Send helpful guidance before error
                self.emit_guidance(
                    "Cone not found. Run 'synapse cone list' to see available cones.",
                    GuidanceSeverity::Error,
                    Some(json!({
                        "requested": params.identifier,
                        "available_command": "synapse cone list"
                    })),
                ).await;

                return Err(PlexusError::not_found("cone", &params.identifier));
            }
        };

        // ... rest of implementation
    }
}
```

### Phase 4: Advanced Features

#### 4.1 Request ID Correlation

**Synapse Side:**
```haskell
-- Generate request ID for each RPC call
invokeWithId :: Path -> Text -> Value -> SynapseM ([ServiceStreamItem], Text)
invokeWithId path method params = do
  requestId <- liftIO randomRequestId
  cfg <- getConfig
  result <- liftIO $ ST.invokeMethodWithId cfg path method params requestId
  case result of
    Left err -> throwTransport method params (scEndpoint cfg) err requestId
    Right items -> pure (items, requestId)

-- Include in debug output
debugTrace :: Text -> [ServiceStreamItem] -> String
debugTrace requestId items =
  "Request ID: " <> T.unpack requestId <> "\n"
  <> "Response items: " <> show (length items) <> "\n"
  <> unlines (map showItem items)
```

**Substrate Side:**
```rust
// Include request ID in all stream items
pub struct StreamContext {
    pub request_id: String,
    pub method: String,
    pub start_time: Instant,
}

impl StreamItem {
    pub fn with_context(self, ctx: &StreamContext) -> Self {
        match self {
            StreamItem::Error { mut error, .. } => {
                if let Some(ref mut data) = error.data {
                    data.request_id = Some(ctx.request_id.clone());
                    data.context = Some(json!({
                        "method": ctx.method,
                        "elapsed_ms": ctx.start_time.elapsed().as_millis(),
                    }));
                }
                self
            }
            _ => self,
        }
    }
}
```

#### 4.2 Error Recovery Suggestions

**Backend:** Include recovery suggestions in errors:

```rust
impl ClaudeCodePlugin {
    pub async fn chat(&self, params: ChatParams) -> Result<(), PlexusError> {
        // Check session exists
        if !self.session_exists(&params.name).await {
            return Err(PlexusError {
                code: ErrorCode::NotFound,
                message: format!("Session '{}' not found", params.name),
                data: Some(ErrorData {
                    category: ErrorCategory::NotFound,
                    field: Some("name".to_string()),
                    suggestion: Some(format!(
                        "Create a session with: synapse claudecode create --name {} --working-dir . --model sonnet",
                        params.name
                    )),
                    recoverable: false,
                    ..Default::default()
                }),
            });
        }

        // ... rest
    }
}
```

**Frontend:** Parse and display suggestions prominently:

```haskell
renderServiceError :: ServiceStreamItem -> String
renderServiceError (ServiceError _ _ err recoverable) =
  case parseStructuredError err of
    Just structured ->
      "Error: " <> seMessage structured <> "\n\n"
      <> maybe "" (\s -> "Suggestion: " <> s <> "\n\n") (seSuggestion structured)
      <> if recoverable then "(This error is recoverable)\n" else ""
    Nothing ->
      "Error: " <> T.unpack err
```

## Implementation Plan

### Phase 1: Quick Wins (1-2 days)
**No backend changes required**

- [ ] Add `--help` hints to parse errors
- [ ] Implement typo suggestions (Levenshtein distance)
- [ ] Enhanced connection error messages
- [ ] Add `--verbose` and `--debug` flags
- [ ] Template rendering error visibility

**Deliverables:**
- Improved UX for common errors
- Better troubleshooting guidance
- Debug mode for developers

### Phase 2: Synapse Context (2-3 days)
**No backend changes required**

- [ ] Enhance `TransportError` with context
- [ ] Navigation breadcrumbs with suggestions
- [ ] Aggregated parse errors with priority
- [ ] Cycle detection with full traversal path

**Deliverables:**
- Rich error context throughout Synapse
- Better debugging information
- Clearer error messages

### Phase 3: Backend Structured Errors (3-5 days)
**Requires Substrate/Plexus changes**

- [ ] Define structured error types in `plexus-protocol`
- [ ] Update `StreamItem::Error` to use `PlexusError`
- [ ] Implement validation error aggregation
- [ ] Add proactive guidance messages
- [ ] Update all plugins to use structured errors

**Deliverables:**
- Structured error responses from backend
- Field-level validation details
- Programmatic error handling

### Phase 4: Advanced Features (3-5 days)
**Requires coordination between Synapse and Substrate**

- [ ] Request ID correlation
- [ ] Error recovery suggestions
- [ ] Documentation links in errors
- [ ] Error analytics/telemetry (optional)

**Deliverables:**
- Full request tracing
- Actionable error recovery
- Comprehensive error system

## Migration Strategy

### Backward Compatibility

**Protocol Version Negotiation:**

```rust
// Backend sends protocol version in initial response
pub struct HandshakeResponse {
    pub version: String,  // "1.0.0"
    pub features: Vec<String>,  // ["structured_errors"]
}

// Old clients ignore extra fields in Error items
// New clients check for structured error data
```

**Graceful Degradation:**

```haskell
-- Synapse handles both old and new error formats
parseErrorResponse :: Value -> Either Text PlexusError
parseErrorResponse v =
  case fromJSON v of
    Success structured -> Right structured  -- New format
    Error _ -> Left (extractMessage v)      -- Old format
```

### Rollout Plan

1. **Week 1:** Deploy Phase 1 (Synapse only)
   - Immediate UX improvements
   - No backend dependency

2. **Week 2:** Deploy Phase 2 (Synapse only)
   - Enhanced context
   - Better debugging

3. **Week 3:** Deploy Phase 3 (Backend + Synapse)
   - Deploy Substrate with structured errors
   - Update Synapse to parse structured errors
   - Maintain fallback to old format

4. **Week 4:** Deploy Phase 4 (Backend + Synapse)
   - Request ID correlation
   - Advanced features

## Success Metrics

### Quantitative
- Reduce "I don't understand this error" support requests by 70%
- 90% of errors include actionable suggestions
- Average error message length increases (more context)
- Debug mode adoption rate

### Qualitative
- User feedback: "Errors are now helpful"
- Developers can debug issues from error messages alone
- Reduced back-and-forth in issue reports

## Examples: Before & After

### Example 1: Invalid Parameter

**Before:**
```bash
$ synapse cone create --model invalid-model --name test
Error: Invalid params
```

**After (Phase 3):**
```bash
$ synapse cone create --model invalid-model --name test
Error: Invalid parameter: model_id

Expected: string with prefix: claude-, gpt-, or anthropic.
Actual: "invalid-model"

Suggestion: Valid examples: claude-3-haiku-20240307, gpt-4o-mini
            Run 'synapse cone registry' to see all available models.

Documentation: https://docs.plexus.dev/models
```

### Example 2: Typo in Parameter

**Before:**
```bash
$ synapse cone chat --id test --promtp "hello"
Parse error: Unknown parameter: --promtp
```

**After (Phase 1):**
```bash
$ synapse cone chat --id test --promtp "hello"
Unknown parameter: --promtp

Did you mean: --prompt

Run 'synapse cone chat --help' to see available parameters.
```

### Example 3: Connection Failure

**Before:**
```bash
$ synapse cone list
Transport error: Connection refused
```

**After (Phase 1):**
```bash
$ synapse cone list
Cannot connect to Plexus server at ws://localhost:4444

Possible causes:
  • Substrate server is not running
  • Wrong host/port configuration
  • Firewall blocking WebSocket connection

To start Substrate:
  substrate run

To check if port is in use:
  lsof -i :4444
```

### Example 4: Not Found with Suggestion

**Before:**
```bash
$ synapse cone chat --id nonexistent --prompt "hi"
Error: Cone not found
```

**After (Phase 3):**
```bash
$ synapse cone chat --id nonexistent --prompt "hi"
Cone not found: nonexistent

Suggestion: Create a cone with:
  synapse cone create --name nonexistent --model claude-3-haiku-20240307

Or list existing cones with:
  synapse cone list
```

### Example 5: Debug Mode

**After (Phase 1):**
```bash
$ synapse --debug cone chat --id test --prompt "hi"
[DEBUG] Navigating path: ["cone", "chat"]
[DEBUG] Resolved method: cone_chat
[DEBUG] Parsed params: {
  "identifier": {"type": "by_name", "name": "test"},
  "prompt": "hi",
  "ephemeral": null
}
[DEBUG] Invoking RPC: ws://localhost:4444
[DEBUG] Request ID: 550e8400-e29b-41d4-a716-446655440000
[DEBUG] Sent at: 2025-01-25T10:30:45Z

[streaming response...]

[DEBUG] Response completed in 234ms
[DEBUG] Total items: 5 (3 data, 1 progress, 1 done)
```

## Related Work

- **JSON-RPC 2.0 Spec**: Error codes and structure
- **gRPC Error Model**: Rich error details with retry info
- **Rust's `anyhow`/`thiserror`**: Context-preserving errors
- **HTTP Problem Details (RFC 7807)**: Structured API errors

## Future Considerations

### Error Analytics
- Collect anonymized error statistics
- Identify common user mistakes
- Improve error messages based on data

### I18n Support
- Localized error messages
- Maintain structured data for translation

### Error Documentation
- Auto-generated docs from error types
- Examples for each error code
- Troubleshooting guides

### IDE Integration
- Language server protocol for Synapse
- Inline error hints in editors
- Quick fixes for common errors

## Conclusion

This proposal establishes a comprehensive error handling strategy for the Plexus stack. By implementing in phases, we can deliver immediate value (Phase 1-2) while building toward a robust, production-grade error system (Phase 3-4).

The key insight: **errors are a user interface**. Good errors don't just report failures—they guide users toward success.

**Next Steps:**
1. Review and approve this proposal
2. Implement Phase 1 (Synapse quick wins)
3. Gather user feedback
4. Proceed with remaining phases based on priority
