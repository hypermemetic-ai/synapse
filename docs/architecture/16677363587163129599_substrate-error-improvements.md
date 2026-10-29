# Substrate Error Improvements: Detailed Implementation Guide

**Date:** 2025-01-25
**Status:** Proposed
**Scope:** Substrate Backend (Rust)
**Related:** 16677364953367891711_error-handling-improvements.md

## Executive Summary

This document provides a detailed analysis of Substrate's current error handling and proposes specific improvements to enable richer, more actionable errors for Synapse clients. Based on exploration of the codebase, we have a solid foundation but lack structured error data, field-level validation details, and actionable suggestions.

**Current Architecture:** Stream-based errors with optional error codes
**Proposed Architecture:** Structured error data with validation details, suggestions, and recovery guidance

## Current Error Architecture

### 1. Core Error Types

**`PlexusError` Enum** (`plexus-core/src/plexus/plexus.rs`)

```rust
#[derive(Debug, Clone)]
pub enum PlexusError {
    ActivationNotFound(String),           // Namespace doesn't exist
    MethodNotFound {                      // Wrong method name
        activation: String,
        method: String,
    },
    InvalidParams(String),                // Parameter validation failure
    ExecutionError(String),               // Runtime error during execution
    HandleNotSupported(String),           // Handle resolution not supported
}
```

**Strengths:**
- Clear distinction between error categories
- Type-safe error handling in Rust
- Composable with Result type

**Weaknesses:**
- **String-based details** - no structured data
- **No error codes** for programmatic handling
- **No suggestions** for users
- **No field-level details** - which param failed?
- **No recovery guidance** - can user retry?

### 2. Stream Error Format

**`PlexusStreamItem::Error`** (`plexus-core/src/plexus/types.rs`)

```rust
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum PlexusStreamItem {
    Error {
        metadata: StreamMetadata,
        message: String,           // Human-readable message
        code: Option<String>,      // Optional error code (e.g., "INVALID_PARAMS")
        recoverable: bool,         // Can operation be retried?
    },
    // ... other variants
}
```

**Wire Format Example:**
```json
{
  "type": "error",
  "metadata": {
    "provenance": ["plexus", "cone"],
    "plexus_hash": "abc123",
    "timestamp": 1735052400
  },
  "message": "Cone not found: test",
  "code": "NOT_FOUND",
  "recoverable": false
}
```

**Strengths:**
- Provenance tracking via metadata
- Optional error codes for categorization
- Recoverable flag for retry logic
- Consistent structure across all transports (WebSocket, stdio, HTTP)

**Weaknesses:**
- **Unstructured error codes** - no registry or documentation
- **No error data field** - nowhere to put validation details
- **No suggestions** for how to fix
- **No context** about what was being attempted
- **No documentation links**

### 3. Helper Functions

**Error Stream Constructors** (`plexus-core/src/plexus/streaming.rs`)

```rust
pub fn error_stream(
    message: String,
    provenance: Vec<String>,
    recoverable: bool,
) -> PlexusStream {
    Box::pin(stream::once(async move {
        PlexusStreamItem::Error {
            metadata: StreamMetadata::new(provenance, PlexusContext::hash()),
            message,
            code: None,
            recoverable,
        }
    }))
}

pub fn error_stream_with_code(
    message: String,
    code: String,
    provenance: Vec<String>,
    recoverable: bool,
) -> PlexusStream {
    // Same as above but includes error code
}
```

**Usage Pattern in Activations:**
```rust
// From cone activation
if handle.meta.is_empty() {
    return Err(PlexusError::ExecutionError(
        "Cone handle missing message ID in meta".to_string()
    ));
}
```

**Problem:** Activations can't provide structured error details, only string messages.

## Proposed Improvements

### Phase 1: Structured Error Data (Breaking Change - New Fields)

#### 1.1 Enhanced PlexusStreamItem::Error

**File:** `plexus-core/src/plexus/types.rs`

```rust
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum PlexusStreamItem {
    Error {
        metadata: StreamMetadata,

        /// Human-readable error message
        message: String,

        /// Structured error code
        code: ErrorCode,  // Changed from Option<String> to structured type

        /// Whether operation can be retried
        recoverable: bool,

        /// NEW: Structured error details
        #[serde(skip_serializing_if = "Option::is_none")]
        details: Option<ErrorDetails>,
    },
    // ... other variants unchanged
}

/// Structured error details
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ErrorDetails {
    /// Category for programmatic handling
    pub category: ErrorCategory,

    /// Field/parameter that caused the error
    #[serde(skip_serializing_if = "Option::is_none")]
    pub field: Option<String>,

    /// Expected value or constraint
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expected: Option<String>,

    /// Actual value received (redacted if sensitive)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub actual: Option<Value>,

    /// Suggestion for how to fix
    #[serde(skip_serializing_if = "Option::is_none")]
    pub suggestion: Option<String>,

    /// Link to relevant documentation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub docs_url: Option<String>,

    /// Additional context for debugging
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<Value>,

    /// Related resources or examples
    #[serde(skip_serializing_if = "Option::is_none")]
    pub related: Option<Vec<String>>,
}

/// Error category enum for programmatic handling
#[derive(Debug, Clone, Copy, Serialize, Deserialize, JsonSchema, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum ErrorCategory {
    Validation,      // Invalid parameters
    NotFound,        // Resource doesn't exist
    Authentication,  // Auth required
    Authorization,   // Permission denied
    Conflict,        // State conflict
    RateLimit,       // Too many requests
    Internal,        // Server error
    Network,         // Network/transport error
    Timeout,         // Operation timed out
    Unsupported,     // Feature not supported
}
```

#### 1.2 Structured Error Codes

**File:** `plexus-core/src/plexus/error_codes.rs` (new)

```rust
use serde::{Deserialize, Serialize};
use schemars::JsonSchema;

/// Structured error codes following JSON-RPC 2.0 + extensions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum ErrorCode {
    // Standard JSON-RPC codes (-32700 to -32600)
    ParseError,          // -32700
    InvalidRequest,      // -32600
    MethodNotFound,      // -32601
    InvalidParams,       // -32602
    InternalError,       // -32603

    // Plexus application codes (custom range)
    ValidationError,     // Parameter validation failed
    NotFound,           // Resource not found
    AlreadyExists,      // Resource already exists
    Unauthorized,       // Authentication required
    Forbidden,          // Permission denied
    Conflict,           // State conflict
    Timeout,            // Operation timed out
    Unsupported,        // Feature not supported
    RateLimited,        // Too many requests

    // Activation-specific codes
    ActivationNotFound,
    HandleInvalid,
    HandleNotSupported,
    StateError,         // Invalid state for operation
}

impl ErrorCode {
    /// Get numeric code for JSON-RPC compatibility
    pub fn to_i32(&self) -> i32 {
        match self {
            // Standard JSON-RPC
            ErrorCode::ParseError => -32700,
            ErrorCode::InvalidRequest => -32600,
            ErrorCode::MethodNotFound => -32601,
            ErrorCode::InvalidParams => -32602,
            ErrorCode::InternalError => -32603,

            // Application codes (-32000 to -32099)
            ErrorCode::ValidationError => -32001,
            ErrorCode::NotFound => -32002,
            ErrorCode::AlreadyExists => -32003,
            ErrorCode::Unauthorized => -32004,
            ErrorCode::Forbidden => -32005,
            ErrorCode::Conflict => -32006,
            ErrorCode::Timeout => -32007,
            ErrorCode::Unsupported => -32008,
            ErrorCode::RateLimited => -32009,
            ErrorCode::ActivationNotFound => -32010,
            ErrorCode::HandleInvalid => -32011,
            ErrorCode::HandleNotSupported => -32012,
            ErrorCode::StateError => -32013,
        }
    }

    /// Get category for this error code
    pub fn category(&self) -> ErrorCategory {
        match self {
            ErrorCode::InvalidParams | ErrorCode::ValidationError => ErrorCategory::Validation,
            ErrorCode::NotFound | ErrorCode::ActivationNotFound => ErrorCategory::NotFound,
            ErrorCode::Unauthorized => ErrorCategory::Authentication,
            ErrorCode::Forbidden => ErrorCategory::Authorization,
            ErrorCode::AlreadyExists | ErrorCode::Conflict | ErrorCode::StateError => ErrorCategory::Conflict,
            ErrorCode::RateLimited => ErrorCategory::RateLimit,
            ErrorCode::Timeout => ErrorCategory::Timeout,
            ErrorCode::Unsupported | ErrorCode::HandleNotSupported => ErrorCategory::Unsupported,
            _ => ErrorCategory::Internal,
        }
    }

    /// Is this error recoverable by default?
    pub fn is_recoverable(&self) -> bool {
        matches!(self,
            ErrorCode::Timeout |
            ErrorCode::RateLimited |
            ErrorCode::Conflict
        )
    }
}
```

#### 1.3 Enhanced PlexusError

**File:** `plexus-core/src/plexus/plexus.rs`

```rust
use super::error_codes::{ErrorCode, ErrorCategory};
use super::types::ErrorDetails;

#[derive(Debug, Clone)]
pub enum PlexusError {
    /// Activation/namespace not found
    ActivationNotFound {
        namespace: String,
        suggestion: Option<String>,
    },

    /// Method not found in activation
    MethodNotFound {
        activation: String,
        method: String,
        available_methods: Vec<String>,
    },

    /// Invalid parameters with structured details
    InvalidParams {
        message: String,
        details: ErrorDetails,
    },

    /// Execution error with context
    ExecutionError {
        message: String,
        code: ErrorCode,
        details: Option<ErrorDetails>,
    },

    /// Handle not supported
    HandleNotSupported {
        activation: String,
        reason: String,
    },
}

impl PlexusError {
    /// Convert to stream error item
    pub fn to_stream_error(&self, provenance: Vec<String>) -> PlexusStreamItem {
        let (code, message, recoverable, details) = match self {
            PlexusError::ActivationNotFound { namespace, suggestion } => {
                let details = ErrorDetails {
                    category: ErrorCategory::NotFound,
                    field: Some("namespace".to_string()),
                    expected: Some("valid activation namespace".to_string()),
                    actual: Some(json!(namespace)),
                    suggestion: suggestion.clone(),
                    docs_url: Some("https://docs.plexus.dev/activations".to_string()),
                    context: None,
                    related: None,
                };
                (
                    ErrorCode::ActivationNotFound,
                    format!("Activation not found: {}", namespace),
                    false,
                    Some(details),
                )
            }

            PlexusError::MethodNotFound { activation, method, available_methods } => {
                let suggestion = if !available_methods.is_empty() {
                    Some(format!(
                        "Available methods: {}. Run 'synapse {} --help' for details.",
                        available_methods.join(", "),
                        activation
                    ))
                } else {
                    None
                };

                let details = ErrorDetails {
                    category: ErrorCategory::NotFound,
                    field: Some("method".to_string()),
                    expected: Some("valid method name".to_string()),
                    actual: Some(json!(method)),
                    suggestion,
                    docs_url: Some(format!("https://docs.plexus.dev/activations/{}", activation)),
                    context: Some(json!({
                        "activation": activation,
                        "available_methods": available_methods,
                    })),
                    related: None,
                };
                (
                    ErrorCode::MethodNotFound,
                    format!("Method '{}' not found in activation '{}'", method, activation),
                    false,
                    Some(details),
                )
            }

            PlexusError::InvalidParams { message, details } => {
                (
                    ErrorCode::InvalidParams,
                    message.clone(),
                    false,
                    Some(details.clone()),
                )
            }

            PlexusError::ExecutionError { message, code, details } => {
                (
                    *code,
                    message.clone(),
                    code.is_recoverable(),
                    details.clone(),
                )
            }

            PlexusError::HandleNotSupported { activation, reason } => {
                let details = ErrorDetails {
                    category: ErrorCategory::Unsupported,
                    field: None,
                    expected: Some("activation with handle resolution support".to_string()),
                    actual: Some(json!(activation)),
                    suggestion: Some(format!(
                        "Activation '{}' does not support handle resolution. Reason: {}",
                        activation, reason
                    )),
                    docs_url: Some("https://docs.plexus.dev/handles".to_string()),
                    context: None,
                    related: None,
                };
                (
                    ErrorCode::HandleNotSupported,
                    format!("Handle resolution not supported by '{}'", activation),
                    false,
                    Some(details),
                )
            }
        };

        PlexusStreamItem::Error {
            metadata: StreamMetadata::new(provenance, PlexusContext::hash()),
            message,
            code,
            recoverable,
            details,
        }
    }
}
```

### Phase 2: Validation Framework

#### 2.1 Validator Trait

**File:** `plexus-core/src/plexus/validation.rs` (new)

```rust
use serde_json::Value;
use super::error_codes::ErrorCode;
use super::types::ErrorDetails;

/// Result type for validation
pub type ValidationResult<T> = Result<T, ValidationError>;

/// Validation error with structured details
#[derive(Debug, Clone)]
pub struct ValidationError {
    pub field: String,
    pub message: String,
    pub expected: String,
    pub actual: Option<Value>,
    pub suggestion: Option<String>,
}

impl ValidationError {
    /// Convert to ErrorDetails
    pub fn to_details(self) -> ErrorDetails {
        ErrorDetails {
            category: ErrorCategory::Validation,
            field: Some(self.field),
            expected: Some(self.expected),
            actual: self.actual,
            suggestion: self.suggestion,
            docs_url: None,
            context: None,
            related: None,
        }
    }
}

/// Trait for parameter validation
pub trait Validate: Sized {
    fn validate(&self) -> ValidationResult<()>;

    /// Validate and return self on success
    fn validated(self) -> ValidationResult<Self> {
        self.validate()?;
        Ok(self)
    }
}

/// Macro for common validations
#[macro_export]
macro_rules! validate_field {
    ($field:expr, required) => {
        if $field.is_none() {
            return Err(ValidationError {
                field: stringify!($field).to_string(),
                message: format!("Field '{}' is required", stringify!($field)),
                expected: "non-null value".to_string(),
                actual: Some(Value::Null),
                suggestion: None,
            });
        }
    };

    ($field:expr, non_empty) => {
        if $field.is_empty() {
            return Err(ValidationError {
                field: stringify!($field).to_string(),
                message: format!("Field '{}' cannot be empty", stringify!($field)),
                expected: "non-empty string".to_string(),
                actual: Some(json!($field)),
                suggestion: None,
            });
        }
    };

    ($field:expr, matches $pattern:expr) => {
        if !$pattern.is_match(&$field) {
            return Err(ValidationError {
                field: stringify!($field).to_string(),
                message: format!("Field '{}' does not match required pattern", stringify!($field)),
                expected: format!("string matching: {}", $pattern.as_str()),
                actual: Some(json!($field)),
                suggestion: None,
            });
        }
    };
}

/// Collect multiple validation errors
pub struct ValidationErrors {
    errors: Vec<ValidationError>,
}

impl ValidationErrors {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn add(&mut self, error: ValidationError) {
        self.errors.push(error);
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn into_result(self) -> Result<(), PlexusError> {
        if self.is_empty() {
            Ok(())
        } else {
            // Aggregate multiple errors into one
            let message = format!(
                "{} validation error(s): {}",
                self.errors.len(),
                self.errors.iter()
                    .map(|e| format!("{}: {}", e.field, e.message))
                    .collect::<Vec<_>>()
                    .join("; ")
            );

            // Use first error's details, but include all in context
            let details = ErrorDetails {
                category: ErrorCategory::Validation,
                field: self.errors.first().map(|e| e.field.clone()),
                expected: self.errors.first().map(|e| e.expected.clone()),
                actual: self.errors.first().and_then(|e| e.actual.clone()),
                suggestion: Some(format!(
                    "Fix the following errors:\n{}",
                    self.errors.iter()
                        .map(|e| format!("  • {}: {}", e.field, e.message))
                        .collect::<Vec<_>>()
                        .join("\n")
                )),
                docs_url: None,
                context: Some(json!({
                    "errors": self.errors.iter().map(|e| json!({
                        "field": e.field,
                        "message": e.message,
                        "expected": e.expected,
                        "actual": e.actual,
                    })).collect::<Vec<_>>(),
                })),
                related: None,
            };

            Err(PlexusError::InvalidParams { message, details })
        }
    }
}
```

#### 2.2 Validation Helper Functions

**File:** `plexus-core/src/plexus/validation.rs` (continued)

```rust
/// Common validators
pub mod validators {
    use super::*;
    use regex::Regex;

    /// Validate model ID format
    pub fn validate_model_id(field: &str, value: &str) -> ValidationResult<()> {
        lazy_static::lazy_static! {
            static ref MODEL_PATTERN: Regex = Regex::new(
                r"^(claude-|gpt-|anthropic\.)"
            ).unwrap();
        }

        if !MODEL_PATTERN.is_match(value) {
            return Err(ValidationError {
                field: field.to_string(),
                message: "Invalid model ID format".to_string(),
                expected: "string starting with 'claude-', 'gpt-', or 'anthropic.'".to_string(),
                actual: Some(json!(value)),
                suggestion: Some(
                    "Valid examples: claude-3-haiku-20240307, gpt-4o-mini. \
                     Run 'synapse cone registry' to see available models.".to_string()
                ),
            });
        }
        Ok(())
    }

    /// Validate UUID format
    pub fn validate_uuid(field: &str, value: &str) -> ValidationResult<uuid::Uuid> {
        uuid::Uuid::parse_str(value).map_err(|e| ValidationError {
            field: field.to_string(),
            message: format!("Invalid UUID: {}", e),
            expected: "valid UUID (e.g., 550e8400-e29b-41d4-a716-446655440000)".to_string(),
            actual: Some(json!(value)),
            suggestion: None,
        })
    }

    /// Validate non-empty string
    pub fn validate_non_empty(field: &str, value: &str) -> ValidationResult<()> {
        if value.is_empty() {
            return Err(ValidationError {
                field: field.to_string(),
                message: "Value cannot be empty".to_string(),
                expected: "non-empty string".to_string(),
                actual: Some(json!(value)),
                suggestion: None,
            });
        }
        Ok(())
    }

    /// Validate string length
    pub fn validate_length(
        field: &str,
        value: &str,
        min: Option<usize>,
        max: Option<usize>,
    ) -> ValidationResult<()> {
        let len = value.len();

        if let Some(min_len) = min {
            if len < min_len {
                return Err(ValidationError {
                    field: field.to_string(),
                    message: format!("Value too short (minimum {} characters)", min_len),
                    expected: format!("string with at least {} characters", min_len),
                    actual: Some(json!(value)),
                    suggestion: None,
                });
            }
        }

        if let Some(max_len) = max {
            if len > max_len {
                return Err(ValidationError {
                    field: field.to_string(),
                    message: format!("Value too long (maximum {} characters)", max_len),
                    expected: format!("string with at most {} characters", max_len),
                    actual: Some(json!(value)),
                    suggestion: Some(format!("Truncate to {} characters", max_len)),
                });
            }
        }

        Ok(())
    }

    /// Validate enum value
    pub fn validate_enum<T: AsRef<str>>(
        field: &str,
        value: &str,
        valid_values: &[T],
    ) -> ValidationResult<()> {
        let valid_strs: Vec<&str> = valid_values.iter().map(|v| v.as_ref()).collect();

        if !valid_strs.contains(&value) {
            return Err(ValidationError {
                field: field.to_string(),
                message: format!("Invalid value '{}'", value),
                expected: format!("one of: {}", valid_strs.join(", ")),
                actual: Some(json!(value)),
                suggestion: Some(format!(
                    "Valid values are: {}",
                    valid_strs.join(", ")
                )),
            });
        }
        Ok(())
    }
}
```

### Phase 3: Activation Integration Examples

#### 3.1 Cone Activation with Validation

**File:** `substrate/src/activations/cone/activation.rs`

```rust
use plexus_core::plexus::validation::{ValidationErrors, validators};
use plexus_core::plexus::types::ErrorDetails;
use plexus_core::plexus::error_codes::{ErrorCode, ErrorCategory};

impl ConeActivation {
    pub async fn create(&self, params: CreateParams) -> Result<PlexusStream, PlexusError> {
        // Collect all validation errors
        let mut errors = ValidationErrors::new();

        // Validate name
        if let Err(e) = validators::validate_non_empty("name", &params.name) {
            errors.add(e);
        }
        if let Err(e) = validators::validate_length("name", &params.name, Some(1), Some(100)) {
            errors.add(e);
        }

        // Validate model_id
        if let Err(e) = validators::validate_model_id("model_id", &params.model_id) {
            errors.add(e);
        }

        // Check for validation errors
        errors.into_result()?;

        // Check if cone already exists
        if self.cone_exists(&params.name).await {
            return Err(PlexusError::ExecutionError {
                message: format!("Cone '{}' already exists", params.name),
                code: ErrorCode::AlreadyExists,
                details: Some(ErrorDetails {
                    category: ErrorCategory::Conflict,
                    field: Some("name".to_string()),
                    expected: Some("unique cone name".to_string()),
                    actual: Some(json!(params.name)),
                    suggestion: Some(format!(
                        "Use a different name or delete the existing cone with: \
                         synapse cone delete --id {}",
                        params.name
                    )),
                    docs_url: None,
                    context: None,
                    related: Some(vec![
                        "synapse cone list".to_string(),
                        format!("synapse cone delete --id {}", params.name),
                    ]),
                }),
            });
        }

        // ... rest of implementation
    }

    pub async fn chat(&self, params: ChatParams) -> Result<PlexusStream, PlexusError> {
        // Validate identifier
        let cone = match self.get_cone(&params.identifier).await {
            Ok(c) => c,
            Err(_) => {
                // Fetch available cones for suggestion
                let available = self.list_cones().await?;
                let available_names: Vec<String> = available.iter()
                    .map(|c| c.name.clone())
                    .collect();

                let suggestion = if available_names.is_empty() {
                    "No cones exist yet. Create one with: synapse cone create --name my-cone --model claude-3-haiku-20240307".to_string()
                } else {
                    format!(
                        "Available cones: {}. Run 'synapse cone list' for details.",
                        available_names.join(", ")
                    )
                };

                return Err(PlexusError::ExecutionError {
                    message: format!("Cone not found: {}", params.identifier),
                    code: ErrorCode::NotFound,
                    details: Some(ErrorDetails {
                        category: ErrorCategory::NotFound,
                        field: Some("identifier".to_string()),
                        expected: Some("existing cone name or UUID".to_string()),
                        actual: Some(json!(params.identifier)),
                        suggestion: Some(suggestion),
                        docs_url: Some("https://docs.plexus.dev/activations/cone".to_string()),
                        context: Some(json!({
                            "available_cones": available_names,
                        })),
                        related: Some(vec![
                            "synapse cone list".to_string(),
                            "synapse cone create --name <name> --model <model>".to_string(),
                        ]),
                    }),
                });
            }
        };

        // Validate prompt
        let mut errors = ValidationErrors::new();
        if let Err(e) = validators::validate_non_empty("prompt", &params.prompt) {
            errors.add(e);
        }
        errors.into_result()?;

        // ... rest of implementation
    }
}
```

#### 3.2 Bash Activation with Validation

**File:** `substrate/src/activations/bash/activation.rs`

```rust
impl BashActivation {
    pub async fn execute(&self, params: ExecuteParams) -> Result<PlexusStream, PlexusError> {
        // Validate command
        let mut errors = ValidationErrors::new();

        if let Err(e) = validators::validate_non_empty("command", &params.command) {
            errors.add(e);
        }

        // Check for dangerous commands (if safety is enabled)
        if self.config.safety_enabled {
            let dangerous_patterns = ["rm -rf /", ":(){ :|:& };:", "mkfs", "dd if=/dev/"];
            for pattern in &dangerous_patterns {
                if params.command.contains(pattern) {
                    errors.add(ValidationError {
                        field: "command".to_string(),
                        message: format!("Dangerous command pattern detected: {}", pattern),
                        expected: "safe shell command".to_string(),
                        actual: Some(json!(params.command)),
                        suggestion: Some(
                            "This command could cause data loss. \
                             Disable safety checks with --unsafe flag if you're sure.".to_string()
                        ),
                    });
                }
            }
        }

        errors.into_result()?;

        // ... rest of implementation
    }
}
```

### Phase 4: Enhanced Stream Helpers

#### 4.1 Rich Error Stream Constructors

**File:** `plexus-core/src/plexus/streaming.rs`

```rust
use super::error_codes::ErrorCode;
use super::types::{ErrorDetails, ErrorCategory};

/// Create error stream with full details
pub fn rich_error_stream(
    code: ErrorCode,
    message: impl Into<String>,
    details: ErrorDetails,
    provenance: Vec<String>,
) -> PlexusStream {
    let message = message.into();
    let recoverable = code.is_recoverable();

    Box::pin(stream::once(async move {
        PlexusStreamItem::Error {
            metadata: StreamMetadata::new(provenance, PlexusContext::hash()),
            message,
            code,
            recoverable,
            details: Some(details),
        }
    }))
}

/// Create validation error stream
pub fn validation_error_stream(
    field: impl Into<String>,
    message: impl Into<String>,
    expected: impl Into<String>,
    actual: Option<Value>,
    suggestion: Option<String>,
    provenance: Vec<String>,
) -> PlexusStream {
    let details = ErrorDetails {
        category: ErrorCategory::Validation,
        field: Some(field.into()),
        expected: Some(expected.into()),
        actual,
        suggestion,
        docs_url: None,
        context: None,
        related: None,
    };

    rich_error_stream(
        ErrorCode::ValidationError,
        message,
        details,
        provenance,
    )
}

/// Create not found error stream
pub fn not_found_stream(
    resource: impl Into<String>,
    identifier: impl Into<String>,
    suggestion: Option<String>,
    provenance: Vec<String>,
) -> PlexusStream {
    let resource = resource.into();
    let identifier = identifier.into();

    let details = ErrorDetails {
        category: ErrorCategory::NotFound,
        field: Some("id".to_string()),
        expected: Some(format!("existing {} identifier", resource)),
        actual: Some(json!(identifier)),
        suggestion,
        docs_url: None,
        context: None,
        related: None,
    };

    rich_error_stream(
        ErrorCode::NotFound,
        format!("{} not found: {}", resource, identifier),
        details,
        provenance,
    )
}
```

### Phase 5: Proactive Guidance

#### 5.1 Enhanced Guidance Stream Item

**File:** `plexus-core/src/plexus/types.rs`

```rust
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum PlexusStreamItem {
    // ... existing variants

    /// User guidance - suggestions and warnings
    Guidance {
        metadata: StreamMetadata,

        /// Guidance message
        message: String,

        /// Severity level
        severity: GuidanceSeverity,

        /// Additional context
        #[serde(skip_serializing_if = "Option::is_none")]
        context: Option<Value>,

        /// Related commands or actions
        #[serde(skip_serializing_if = "Option::is_none")]
        related: Option<Vec<String>>,
    },
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case")]
pub enum GuidanceSeverity {
    Info,      // FYI
    Warning,   // Might be wrong but continuing
    Error,     // Definitely wrong, error follows
}
```

#### 5.2 Guidance Helper Functions

**File:** `plexus-core/src/plexus/streaming.rs`

```rust
/// Create guidance stream item
pub fn guidance_stream(
    message: impl Into<String>,
    severity: GuidanceSeverity,
    context: Option<Value>,
    related: Option<Vec<String>>,
    provenance: Vec<String>,
) -> PlexusStream {
    Box::pin(stream::once(async move {
        PlexusStreamItem::Guidance {
            metadata: StreamMetadata::new(provenance, PlexusContext::hash()),
            message: message.into(),
            severity,
            context,
            related,
        }
    }))
}
```

#### 5.3 Usage in Activations

**File:** `substrate/src/activations/cone/activation.rs`

```rust
impl ConeActivation {
    pub async fn chat(&self, params: ChatParams) -> Result<PlexusStream, PlexusError> {
        // Send guidance before attempting operation
        if params.ephemeral.unwrap_or(false) {
            let guidance = guidance_stream(
                "Using ephemeral mode: messages will not be saved to history",
                GuidanceSeverity::Info,
                Some(json!({
                    "ephemeral": true,
                    "persistence": "none"
                })),
                None,
                self.provenance(),
            );

            // Prepend guidance to result stream
            // (implementation depends on stream combining strategy)
        }

        // Validate cone exists
        let cone = match self.get_cone(&params.identifier).await {
            Ok(c) => c,
            Err(_) => {
                // Send helpful guidance BEFORE error
                let guidance = guidance_stream(
                    format!(
                        "Cone '{}' not found. Run 'synapse cone list' to see available cones.",
                        params.identifier
                    ),
                    GuidanceSeverity::Error,
                    Some(json!({
                        "requested": params.identifier,
                        "action": "synapse cone list"
                    })),
                    Some(vec![
                        "synapse cone list".to_string(),
                        "synapse cone create --name <name> --model <model>".to_string(),
                    ]),
                    self.provenance(),
                );

                // Return combined guidance + error stream
                return Ok(Box::pin(stream::iter(vec![
                    guidance.next().await.unwrap(),
                    self.create_not_found_error(&params.identifier),
                ])));
            }
        };

        // ... rest of implementation
    }
}
```

## Migration Path

### Backward Compatibility Strategy

**Option 1: Additive Fields (Recommended)**
- Add `details` field as `Option<ErrorDetails>` to `PlexusStreamItem::Error`
- Old clients ignore unknown fields (JSON deserialization with `#[serde(default)]`)
- New clients check for `details` field and fall back to `message` if missing
- **No breaking changes** - fully backward compatible

**Option 2: Version Negotiation**
- Client sends protocol version in initial handshake
- Server returns appropriate error format based on client version
- More complex but allows future breaking changes

**Recommended: Option 1** - Add optional fields, maintain backward compatibility.

### Rollout Phases

**Phase 1: Add Types (Week 1)**
- Add `ErrorDetails`, `ErrorCode`, `ErrorCategory` types
- Add `details` field to `PlexusStreamItem::Error` (optional)
- Add validation framework
- **No activation changes yet**
- Deploy to staging

**Phase 2: Update Core Activations (Week 2)**
- Update `cone`, `claudecode`, `bash` activations to use structured errors
- Add validation to parameter handling
- Deploy to staging, test with old and new Synapse clients

**Phase 3: Update Remaining Activations (Week 3)**
- Update all other activations
- Add proactive guidance where appropriate
- Full staging validation

**Phase 4: Production Rollout (Week 4)**
- Deploy to production
- Monitor error metrics
- Gather user feedback

### Testing Strategy

**Unit Tests:**
```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validation_error_details() {
        let error = ValidationError {
            field: "model_id".to_string(),
            message: "Invalid model ID".to_string(),
            expected: "string starting with 'claude-' or 'gpt-'".to_string(),
            actual: Some(json!("invalid")),
            suggestion: Some("Use 'claude-3-haiku-20240307'".to_string()),
        };

        let details = error.to_details();
        assert_eq!(details.category, ErrorCategory::Validation);
        assert_eq!(details.field, Some("model_id".to_string()));
    }

    #[test]
    fn test_error_code_to_i32() {
        assert_eq!(ErrorCode::InvalidParams.to_i32(), -32602);
        assert_eq!(ErrorCode::NotFound.to_i32(), -32002);
    }

    #[test]
    fn test_validation_errors_aggregation() {
        let mut errors = ValidationErrors::new();
        errors.add(ValidationError {
            field: "name".to_string(),
            message: "Required".to_string(),
            expected: "non-empty string".to_string(),
            actual: None,
            suggestion: None,
        });
        errors.add(ValidationError {
            field: "model_id".to_string(),
            message: "Invalid format".to_string(),
            expected: "valid model ID".to_string(),
            actual: Some(json!("bad")),
            suggestion: None,
        });

        let result = errors.into_result();
        assert!(result.is_err());

        if let Err(PlexusError::InvalidParams { message, details }) = result {
            assert!(message.contains("2 validation error(s)"));
            assert!(details.context.is_some());
        } else {
            panic!("Expected InvalidParams error");
        }
    }
}
```

**Integration Tests:**
```rust
#[tokio::test]
async fn test_cone_create_validation() {
    let activation = ConeActivation::new();

    // Test missing name
    let params = json!({
        "model_id": "claude-3-haiku-20240307"
    });
    let result = activation.call("create", params).await;
    assert!(result.is_err());

    // Extract error details
    let stream = result.unwrap_err().to_stream_error(vec!["test".to_string()]);
    if let PlexusStreamItem::Error { details, .. } = stream {
        assert!(details.is_some());
        let details = details.unwrap();
        assert_eq!(details.category, ErrorCategory::Validation);
        assert_eq!(details.field, Some("name".to_string()));
    }
}
```

## Wire Format Examples

### Before (Current)

```json
{
  "type": "error",
  "metadata": {
    "provenance": ["plexus", "cone"],
    "plexus_hash": "abc123",
    "timestamp": 1735052400
  },
  "message": "Invalid params",
  "code": null,
  "recoverable": false
}
```

### After (With Structured Details)

```json
{
  "type": "error",
  "metadata": {
    "provenance": ["plexus", "cone"],
    "plexus_hash": "abc123",
    "timestamp": 1735052400
  },
  "message": "Invalid parameter: model_id",
  "code": "INVALID_PARAMS",
  "recoverable": false,
  "details": {
    "category": "validation",
    "field": "model_id",
    "expected": "string starting with 'claude-', 'gpt-', or 'anthropic.'",
    "actual": "invalid-model",
    "suggestion": "Valid examples: claude-3-haiku-20240307, gpt-4o-mini. Run 'synapse cone registry' to see available models.",
    "docs_url": "https://docs.plexus.dev/models",
    "context": null,
    "related": null
  }
}
```

### Guidance Example

```json
{
  "type": "guidance",
  "metadata": {
    "provenance": ["plexus", "cone"],
    "plexus_hash": "abc123",
    "timestamp": 1735052400
  },
  "message": "Cone 'test' not found. Run 'synapse cone list' to see available cones.",
  "severity": "error",
  "context": {
    "requested": "test",
    "action": "synapse cone list"
  },
  "related": [
    "synapse cone list",
    "synapse cone create --name <name> --model <model>"
  ]
}
```

## Performance Considerations

### Impact Analysis

**Additional Memory:**
- `ErrorDetails` struct: ~200-500 bytes per error
- Negligible impact (errors are infrequent)

**Serialization Overhead:**
- JSON serialization of `details` field
- Minimal impact (only on error path, not hot path)

**Validation Overhead:**
- Parameter validation adds ~1-5ms per request
- Acceptable tradeoff for better UX

**Optimization Strategies:**
- Use `Arc<ErrorDetails>` for shared error details
- Cache validation regex patterns (`lazy_static`)
- Skip expensive validations in hot paths (defer to execution)

## Documentation Requirements

### API Documentation

**Error Code Registry:**
```markdown
# Error Codes

## Standard JSON-RPC Codes
- `-32700` PARSE_ERROR: Invalid JSON
- `-32600` INVALID_REQUEST: Invalid JSON-RPC request
- `-32601` METHOD_NOT_FOUND: Method doesn't exist
- `-32602` INVALID_PARAMS: Invalid parameters
- `-32603` INTERNAL_ERROR: Server error

## Plexus Application Codes
- `-32001` VALIDATION_ERROR: Parameter validation failed
- `-32002` NOT_FOUND: Resource not found
- `-32003` ALREADY_EXISTS: Resource already exists
- `-32004` UNAUTHORIZED: Authentication required
- `-32005` FORBIDDEN: Permission denied
...
```

**Error Handling Guide:**
```markdown
# Handling Errors

## Error Structure
All errors include:
- `message`: Human-readable description
- `code`: Structured error code
- `recoverable`: Whether operation can be retried
- `details` (optional): Structured error details

## Error Details
When present, `details` includes:
- `category`: Error category (validation, not_found, etc.)
- `field`: Which parameter/field caused the error
- `expected`: What was expected
- `actual`: What was received
- `suggestion`: How to fix the error
- `docs_url`: Link to relevant documentation
- `context`: Additional debugging context
- `related`: Related commands or resources
```

## Summary

### Current State
- ✅ Stream-based error architecture
- ✅ Optional error codes (string)
- ✅ Recoverable flag
- ✅ Provenance tracking
- ❌ No structured error details
- ❌ No validation framework
- ❌ No field-level error information
- ❌ No suggestions for fixes

### Proposed Improvements
1. **Structured Error Details** - Add `ErrorDetails` type with validation info
2. **Error Code Registry** - Standardized `ErrorCode` enum
3. **Validation Framework** - Reusable validators with structured errors
4. **Proactive Guidance** - Send guidance before errors
5. **Activation Integration** - Update all activations to use rich errors

### Implementation Priority
1. **Phase 1** (Week 1): Add types, maintain backward compatibility
2. **Phase 2** (Week 2): Update core activations (cone, claudecode, bash)
3. **Phase 3** (Week 3): Update remaining activations
4. **Phase 4** (Week 4): Production rollout with monitoring

### Key Benefits
- **Better UX**: Users get actionable error messages with suggestions
- **Easier Debugging**: Structured details help identify root cause
- **Programmatic Handling**: Synapse can handle specific error codes differently
- **Backward Compatible**: Old clients continue to work
- **Future Proof**: Framework supports rich errors going forward

**Next Steps:** Review and approve, then implement Phase 1 types and validation framework.
