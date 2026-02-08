# Dynamic CLI with Type-Driven Schemas

## Overview

This document describes the complete architecture of the self-documenting RPC system, from substrate schema generation to frontend CLI rendering. The system enables frontends to discover available RPC methods and their parameters at runtime, with zero hardcoding.

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                         SUBSTRATE (Rust)                             │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  1. Activation Types (with schemars derive)                         │
│     ┌─────────────────────────────────────────────────────┐         │
│     │ #[derive(JsonSchema)]                               │         │
│     │ pub enum ArborMethod {                              │         │
│     │   /// Get a complete tree with all nodes            │         │
│     │   TreeGet {                                          │         │
│     │     /// UUID of the tree                            │         │
│     │     tree_id: Uuid,                                  │         │
│     │   }                                                  │         │
│     │ }                                                    │         │
│     └─────────────────────────────────────────────────────┘         │
│                             │                                        │
│                             │ schemars derives                       │
│                             ▼                                        │
│  2. Automatic Schema Generation                                     │
│     • format: "uuid" (from Uuid type)                               │
│     • description (from doc comments)                               │
│     • required: ["tree_id"] (non-Option fields)                     │
│                             │                                        │
│                             ▼                                        │
│  3. Plexus RPC Methods                                              │
│     ┌─────────────────────────────────────────────────────┐         │
│     │ substrate.schema          → List activations/methods   │         │
│     │ substrate.activation_schema → Full schema for one      │         │
│     │ plexus_hash            → Cache invalidation hash    │         │
│     └─────────────────────────────────────────────────────┘         │
│                             │                                        │
│                             │ WebSocket JSON-RPC                     │
└─────────────────────────────┼─────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│                      FRONTEND (Haskell)                              │
├─────────────────────────────────────────────────────────────────────┤
│                                                                       │
│  1. Schema Discovery                                                 │
│     call plexus_hash        → "7a1a43920ee194e1"                    │
│     call substrate.schema      → {activations: [...]}                  │
│     for each activation:                                             │
│       call substrate.activation_schema(ns) → {oneOf: [...]}            │
│                             │                                        │
│                             ▼                                        │
│  2. Cache with Hash (TTL removed)                                   │
│     ~/.cache/symbols/schema.json:                                   │
│     {                                                                │
│       "hash": "7a1a43920ee194e1",                                   │
│       "schema": {...},                                               │
│       "enriched": {                                                  │
│         "arbor": {oneOf: [...]},                                    │
│         "cone": {oneOf: [...]}                                      │
│       }                                                              │
│     }                                                                │
│                             │                                        │
│                             ▼                                        │
│  3. Dynamic Parser Generation                                       │
│     PlexusSchema → optparse-applicative parsers                     │
│     • Subcommand per activation                                     │
│     • Sub-subcommand per method                                     │
│     • Typed flags per parameter                                     │
│                             │                                        │
│                             ▼                                        │
│  4. CLI Execution                                                    │
│     $ synapse arbor tree-get --tree-id <UUID>                   │
│                                                                       │
└─────────────────────────────────────────────────────────────────────┘
```

## Phase 1: Substrate Type-Driven Schema Generation

### The Problem (Before)

Previously, schema generation required manual "enrichment":

```rust
// Types were stringly-typed
pub enum ArborMethod {
    TreeGet {
        tree_id: String,  // No format annotation
    }
}

// Manual enrichment for each method
impl ArborMethod {
    pub fn describe_by_name(method_name: &str) -> Option<MethodEnrichment> {
        match method_name {
            "tree_get" => Some(MethodEnrichment {
                fields: vec![
                    FieldEnrichment::uuid("tree_id", "UUID of the tree", true),
                ],
            }),
            _ => None,
        }
    }
}
```

This was error-prone and required duplicating information.

### The Solution (Now)

Use proper types + schemars features:

```rust
use uuid::Uuid;
use schemars::JsonSchema;

#[derive(JsonSchema)]
pub enum ArborMethod {
    /// Get a complete tree with all nodes
    TreeGet {
        /// UUID of the tree
        tree_id: Uuid,  // schemars adds format: "uuid" automatically
    }
}

// In Cargo.toml
// schemars = { version = "1.0", features = ["derive", "uuid1"] }
```

### What schemars Provides Automatically

| Feature | Source | Generated |
|---------|--------|-----------|
| Format annotation | `Uuid` type | `"format": "uuid"` |
| Description | Doc comments (`///`) | `"description": "UUID of the tree"` |
| Required fields | Non-`Option<T>` | `"required": ["tree_id"]` |
| Nullable types | `Option<T>` | `"type": ["string", "null"]` |

Generated schema for `TreeGet`:

```json
{
  "properties": {
    "method": { "const": "tree_get" },
    "params": {
      "type": "object",
      "properties": {
        "tree_id": {
          "type": "string",
          "format": "uuid",
          "description": "UUID of the tree"
        }
      },
      "required": ["tree_id"]
    }
  }
}
```

## Phase 2: Schema Discovery Protocol

The frontend uses a **two-phase discovery** process:

### Phase 2.1: Discover Activations

```json
Request:  {"method": "substrate.schema", "params": []}
Response: {
  "activations": [
    {
      "namespace": "arbor",
      "version": "1.0.0",
      "description": "Manage conversation trees",
      "methods": ["tree_create", "tree_get", "tree_list", ...]
    },
    {
      "namespace": "cone",
      "methods": ["create", "chat", "list", ...]
    }
  ],
  "total_methods": 28
}
```

This tells the frontend:
- Which activations exist (arbor, cone, bash, health)
- Which methods each activation has
- **Order matters**: methods[i] corresponds to oneOf[i] in enriched schema

### Phase 2.2: Enrich Each Activation

```json
Request:  {"method": "substrate.activation_schema", "params": ["arbor"]}
Response: {
  "oneOf": [
    { /* tree_create schema */ },
    { /* tree_get schema */ },
    { /* tree_list schema */ },
    ...
  ]
}
```

The frontend fetches this for each activation namespace, building a complete map:

```haskell
enriched = Map.fromList
  [ ("arbor", arborSchema)
  , ("cone", coneSchema)
  , ("bash", bashSchema)
  , ("health", healthSchema)
  ]
```

### Phase 2.3: Cache Invalidation Hash

```json
Request:  {"method": "plexus_hash", "params": []}
Response: {
  "hash": "7a1a43920ee194e1"
}
```

The hash is computed from: `namespace:version:methods` for all activations.

**Changes hash:**
- Activation added/removed
- Activation version changed
- Methods added/removed/renamed

**Doesn't change hash:**
- Schema enrichment details (descriptions, formats)
- Internal implementation changes

## Phase 3: Frontend Cache System

### Cache Structure

```json
{
  "hash": "7a1a43920ee194e1",
  "schema": {
    "activations": [...],
    "total_methods": 28
  },
  "enriched": {
    "arbor": { "oneOf": [...] },
    "cone": { "oneOf": [...] },
    "bash": { "description": "bash activation schema" },
    "health": { "description": "health activation schema" }
  }
}
```

### Cache Validation (Hash-Based, Not TTL)

**Before (TTL-based):**
```haskell
loadCache → check age < 1 hour → use or refetch
```

**After (Hash-based):**
```haskell
loadCache       → get cachedHash
fetchHash       → get currentHash
if cachedHash == currentHash
  then use cache
  else refetch all schemas
```

**Benefits:**
- ✅ Instant invalidation when schema changes
- ✅ No stale cache even after days
- ✅ Lighter weight (quick hash check vs full fetch)

### Cache Management Commands

```bash
# Show cache contents
synapse cache show

# Check if cache is fresh
synapse cache status
# Output:
#   Cached hash:  7a1a43920ee194e1
#   Current hash: 7a1a43920ee194e1
#   Status: Fresh ✓

# Clear cache
synapse cache clear

# Force refresh
synapse cache refresh
```

## Phase 4: Dynamic Parser Generation

### Index-Based Schema Lookup

The frontend uses **index-based mapping** between method lists and enriched schemas:

```haskell
-- From substrate.schema
activationMethods = ["tree_create", "tree_get", "tree_list", ...]
                     [0]            [1]          [2]

-- From substrate.activation_schema
enrichedSchema.oneOf = [treeCreateSchema, treeGetSchema, treeListSchema, ...]
                        [0]               [1]            [2]

-- Lookup: methods[i] → oneOf[i]
buildMethodParser activation method = do
  let idx = findIndex (== method) (activationMethods activation)
  let schema = enrichedSchema.oneOf !! idx
  buildTypedParser schema
```

### Typed Parameter Generation

For each parameter in the schema:

```haskell
buildParamParser :: ParamSchema -> Parser (Text, Maybe Value)
buildParamParser param =
  case paramType param of
    ParamBoolean -> toJSON <$> switch (long flagName)
    ParamInteger -> toJSON <$> option auto (long flagName)
    ParamString | Just "uuid" <- paramFormat param ->
      toJSON <$> strOption (long flagName <> metavar "UUID")
    ParamString ->
      toJSON <$> strOption (long flagName <> metavar "TEXT")
```

### Required vs Optional Parameters

Parameters are sorted to show **required first, optional second**:

```haskell
buildTypedMethodParser namespace method schema = do
  let sortedParams = sortOn (not . paramRequired) (methodParams schema)
  paramValues <- sequenceA $ map buildParamParser sortedParams
  ...
```

Result:

```bash
# Required params (no brackets)
synapse arbor node-create-text --content TEXT --tree-id UUID

# Then optional params (in brackets)
[--metadata JSON] [--parent UUID]
```

### Help Generation

Generated help includes:
- Method descriptions from doc comments
- Parameter types from schema
- Format hints (UUID, TEXT, JSON, INT)
- Which parameters are required

```bash
$ synapse arbor tree-get --help
Usage: synapse arbor tree-get --tree-id UUID

  Get a complete tree with all nodes

Available options:
  --tree-id UUID           UUID of the tree
  -p,--params JSON         Override with raw JSON object
  -h,--help                Show this help text
```

## Phase 5: RPC Response Format

All responses include `plexus_hash` for opportunistic invalidation:

```json
{
  "plexus_hash": "7a1a43920ee194e1",
  "type": "data",
  "provenance": {"segments": ["arbor"]},
  "content_type": "arbor.event",
  "data": { ... }
}
```

Frontends can check the hash on every response and refresh cache if it changes.

## Implementation Status

### Substrate Activations

| Activation | Type-Driven | Enriched Schema | Status |
|------------|-------------|-----------------|--------|
| **arbor** | ✅ Uuid types | ✅ Full (19 methods) | Complete |
| **cone** | ✅ Uuid types | ✅ Full (7 methods) | Complete |
| **bash** | ❌ String param | ❌ Minimal only | Generic help |
| **health** | ❌ No params | ❌ Minimal only | Generic help |

**Bash example (still needs update):**

```bash
# Current (generic)
$ synapse bash execute --help
Usage: synapse bash execute [-p|--params JSON] [ARG...]

# Would be better with type-driven schema
$ synapse bash execute --help
Usage: synapse bash execute --command TEXT
```

### Frontend Features

| Feature | Status | Notes |
|---------|--------|-------|
| Hash-based cache invalidation | ✅ | TTL removed |
| Cache management commands | ✅ | show, clear, status, refresh |
| Typed parameter parsing | ✅ | UUID, TEXT, INT, JSON |
| Required param validation | ✅ | Shows errors before RPC call |
| Template rendering | ✅ | Mustache templates for output |
| Schema introspection | ✅ | `--schema` flag |

## Example: Complete Flow

```bash
# 1. First run - fetch and cache schemas
$ synapse arbor tree-list
# → Calls plexus_hash
# → Calls substrate.schema
# → Calls substrate.activation_schema for each activation
# → Caches to ~/.cache/symbols/schema.json
# → Executes command

# 2. Second run - use cache
$ synapse arbor tree-get --tree-id 00000000-0000-0000-0000-000000000000
# → Calls plexus_hash
# → Compares with cached hash
# → Hash matches, uses cache
# → Executes command

# 3. After schema change on substrate
# (New method added to arbor, hash changes)
$ synapse arbor tree-list
# → Calls plexus_hash
# → Hash differs from cache
# → Refetches all schemas
# → Updates cache with new hash
# → Executes command

# 4. Check cache status
$ synapse cache status
Cached hash:  7a1a43920ee194e1
Current hash: 8b2b54a30ff295f2
Status: Stale (hash mismatch)

$ synapse cache refresh
Refreshing cache...
Cache refreshed successfully
  Hash: 8b2b54a30ff295f2
  Activations: 4
```

## Benefits

### For Substrate Developers

1. **No manual schema maintenance** - Types drive everything
2. **Compile-time safety** - Wrong UUID usage is a compiler error
3. **Single source of truth** - Doc comments become user-visible help
4. **Automatic propagation** - Add a field, frontends see it immediately

### For Frontend Developers

1. **Zero hardcoding** - No method names or param types in code
2. **Instant updates** - New methods appear without rebuilding
3. **Type safety** - Parser validates params before RPC call
4. **Better UX** - Helpful error messages, tab completion potential

### For Users

1. **Discoverable** - `--help` shows all available commands
2. **Self-documenting** - Descriptions from substrate code
3. **Type-guided** - Clear distinction between UUID, TEXT, JSON
4. **Early validation** - Errors show before network call

## Related Documents

- Type-driven generation: `16680892147769332735_schema-type-driven-generation.md`
- Schema enrichment history: `16680895651461426687_schema-enrichment-cache-invalidation.md`
- Hash-based invalidation: `16680894298193956607_plexus-hash-cache-invalidation.md`
- Dynamic CLI implementation: `16680974430007725567_dynamic-cli-implementation.md`

## Future Improvements

1. **Shell completions** - Generate bash/zsh completions from schema
2. **Interactive mode** - REPL with tab completion
3. **Better bash/health schemas** - Add type-driven generation to remaining activations
4. **OpenAPI export** - Generate OpenAPI/Swagger from schemas
5. **Multi-language frontends** - Python, TypeScript, etc. using same protocol
