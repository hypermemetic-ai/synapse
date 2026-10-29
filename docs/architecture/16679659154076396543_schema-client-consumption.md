# Schema-Client Consumption Gap Analysis

**Status**: Analysis
**Scope**: Plexus (backend) + Synapse (client)
**Date**: 2024-12-30

## Executive Summary

Plexus generates rich, typed schemas with full JSON Schema for parameters and discriminated union return types. However, Synapse underutilizes these schemas—using them for navigation and help display but not for validation, response typing, or intelligent template generation.

The contract is **declared but not enforced** at the client layer.

## Current State

### What Plexus Provides (Complete)

```
cone.chat schema:
├── params:
│   ├── identifier: $ref ConeIdentifier
│   ├── prompt: string (required)
│   └── ephemeral: boolean | null (default: null)
└── returns:
    └── oneOf:
        ├── chat_start:   { type: "chat_start", cone_id, user_position }
        ├── chat_content: { type: "chat_content", cone_id, content }
        ├── chat_complete: { type: "chat_complete", cone_id, new_head, usage }
        └── error:        { type: "error", message }
```

- Full `params` schema with types, required fields, defaults, formats
- Full `returns` schema with discriminated unions (`oneOf` + `const` discriminator)
- Format constraints (`format: "uuid"`)
- Nested `$ref` definitions for complex types
- Recursive schema hashing for cache invalidation

### What Synapse Does With Schemas

| Capability | Used? | How |
|------------|-------|-----|
| Navigation/discovery | Yes | `psMethods`, `psChildren` for path resolution |
| Help display | Yes | `methodDescription`, `methodParams` for CLI help |
| Default extraction | Yes | Extracts `default` values from param properties |
| Required field detection | Partial | Checks if `required` array exists, but doesn't validate |
| Param validation | No | Sends raw `Value` to backend |
| Return type awareness | No | Templates matched by `content_type` string |
| Per-variant templates | No | `oneOf` variants flattened into single template |
| Format validation | No | `format: "uuid"` ignored |

## Identified Gaps

### Gap 1: No Parameter Validation Before Send

**Location**: `Synapse.Transport.invoke` (line 57-63)

```haskell
invoke :: Path -> Text -> Value -> SynapseM [PlexusStreamItem]
invoke namespacePath method params = do
  cfg <- getConfig
  result <- liftIO $ ST.invokeMethod cfg namespacePath method params
  -- ^^^ params sent directly, no validation
```

**Symptom**:
```bash
$ synapse cone create -p '{"name": 123, "model_id": true}'
Error: Invalid params: invalid type: integer `123`, expected a string
```

Error comes from backend serde, not client validation.

**Impact**:
- Wasted network round-trip for invalid params
- Serde error messages less helpful than schema-based validation
- No client-side autocomplete/validation in potential GUI

**Fix**: Validate `params` against `methodParams` schema before sending.

### Gap 2: Return Schema Flattened Into Single Template

**Location**: `Synapse.Algebra.TemplateGen.generateVariants` (line 240-247)

```haskell
generateVariants variants =
  let allProps = concatMap extractProps variants  -- Flattens ALL variants!
      uniqueKeys = dedupe $ map fst allProps
  in T.intercalate " " $ map (\k -> "{{" <> k <> "}}") displayKeys
```

**Symptom**:
```mustache
{{! cone.chat }}
{{cone_id}} {{user_position}} {{content}} {{new_head}} {{usage}} {{message}}
```

All 4 event types merged into one useless template.

**Impact**:
- Can't render `chat_content` differently from `chat_complete`
- Most fields empty for any given event
- Streaming output looks broken

**Fix**: Generate per-variant templates, keyed by discriminator value.

### Gap 3: Discriminator Field Filtered Out

**Location**: `Synapse.Algebra.TemplateGen.isInternalField` (line 302-305)

```haskell
isInternalField :: Text -> Bool
isInternalField "event" = True
isInternalField "type" = True  -- But this IS the discriminator!
```

**Impact**: The `type` field that distinguishes `chat_start` from `chat_content` is hidden.

**Fix**: Use discriminator for template dispatch, not hide it.

### Gap 4: Template Resolution Ignores Return Schema

**Location**: `Synapse.Renderer.resolveTemplate` (line 130-144)

```haskell
resolveTemplate cfg contentType = do
  let (namespace, method) = parseContentType contentType  -- String parsing only
  let candidates = [ path </> namespace </> method <.> "mustache" | ... ]
```

Templates resolved by `content_type` string (e.g., `"cone.chat_content"`), but:
1. Template generator doesn't know to create `chat_content.mustache`
2. No connection between `returns` schema and template names

**Fix**: Parse `returns.oneOf[].properties.type.const` to discover event types, generate/resolve templates accordingly.

## Proposed Architecture

### Phase 1: Param Validation (Low effort, high value)

```haskell
-- New module: Synapse.Validation
validateParams :: MethodSchema -> Value -> Either ValidationError Value
validateParams method params = case methodParams method of
  Nothing -> Right params
  Just schema -> validateAgainstSchema schema params

-- In Transport.hs
invoke path method params = do
  schema <- fetchMethodSchema path method
  case validateParams schema params of
    Left err -> throwValidation err
    Right validated -> invokeRaw ...
```

Consider using `hjsonschema` or similar for JSON Schema validation.

### Phase 2: Per-Variant Template Generation

```haskell
-- New structure for oneOf-aware templates
data VariantTemplate = VariantTemplate
  { vtNamespace :: Text
  , vtMethod :: Text
  , vtVariant :: Text        -- e.g., "chat_content"
  , vtDiscriminator :: Text  -- e.g., "type"
  , vtTemplate :: Text
  }

-- Generate one template per variant
generateVariantTemplates :: MethodSchema -> [VariantTemplate]
generateVariantTemplates method = case methodReturns method of
  Just schema -> extractOneOfVariants schema >>= \(discValue, variantSchema) ->
    [ VariantTemplate { vtVariant = discValue, ... } ]
  Nothing -> []
```

Output:
```
.substrate/templates/cone/
  chat.chat_start.mustache
  chat.chat_content.mustache
  chat.chat_complete.mustache
  chat.error.mustache
```

### Phase 3: Discriminator-Based Template Dispatch

```haskell
-- In Renderer.hs
resolveTemplate cfg contentType eventData = do
  -- First try: exact content_type + discriminator
  let discriminator = extractDiscriminator eventData  -- e.g., "chat_content"
  let variantPath = namespace </> method <> "." <> discriminator <.> "mustache"

  -- Fallback: just content_type
  let methodPath = namespace </> method <.> "mustache"

  firstExisting [variantPath, methodPath, defaultPath]

extractDiscriminator :: Value -> Maybe Text
extractDiscriminator (Object o) =
  case KM.lookup "type" o of
    Just (String s) -> Just s
    _ -> Nothing
```

## Schema Consumer Maturity Model

| Level | Description | Synapse Today |
|-------|-------------|---------------|
| 0 | Ignores schemas entirely | |
| 1 | Uses schemas for discovery/navigation | Yes |
| 2 | Uses schemas for documentation/help | Yes |
| 3 | Validates params against schema | No |
| 4 | Validates responses against schema | No |
| 5 | Generates code/templates from schema | Partial (broken) |
| 6 | Full bidirectional type safety | No |

**Current level: 2.5** (discovery + docs + partial template gen)

**Target level: 5** (validated params, variant-aware templates, schema-driven dispatch)

## Implementation Priority

1. **Param validation** - Immediate win, prevents round-trip errors
2. **Per-variant templates** - Makes streaming output usable
3. **Discriminator dispatch** - Enables rich per-event rendering
4. **Response validation** - Catches backend bugs, enables strict mode

## Related Work

- Plexus MCP bridge validates `type: "object"` at root (minimal)
- Plexus serde does runtime type checking (backend-side)
- `plexus-macros` generates complete schemas from Rust types
- Schema hashing enables client-side cache invalidation

## Questions

1. Should validation be opt-in (`--strict`) or default?
2. Should invalid responses warn or error?
3. Where should generated variant templates live in the filesystem?
4. Should Synapse cache validated schemas or re-fetch on each call?
