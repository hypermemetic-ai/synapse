# Schema Enrichment and Cache Invalidation

## Overview

This document describes two related improvements to the plexus RPC system:

1. **Schema Required Params Fix** - The `required` field is now properly included in params objects
2. **Cache Invalidation Hash** - A new `plexus_hash` method enables efficient schema cache invalidation

## Schema Required Params (Fixed)

### The Problem

The enriched schema exposed via `substrate.activation_schema` was missing the `required` array at the params level. Clients received:

```json
{
  "params": {
    "type": "object",
    "properties": {
      "tree_id": { "type": "string", "format": "uuid" },
      "content": { "type": "string" }
    }
    // NO required array - all params appear optional
  }
}
```

### The Fix

Two changes were required:

1. **SchemaProperty.required field** - Already existed in `src/plexus/schema.rs`

2. **apply_enrichments() sets required** - Modified to collect required field names from enrichment data and set them on params:

```rust
// src/plexus/schema.rs - SchemaVariant::apply_enrichments()
pub fn apply_enrichments(&mut self, enrichment: &MethodEnrichment) {
    // Collect required field names
    let required_fields: Vec<String> = enrichment.fields
        .iter()
        .filter(|f| f.required)
        .map(|f| f.name.clone())
        .collect();

    // Apply format and description enrichments...

    // Set the required array on the params object itself
    if !required_fields.is_empty() {
        if let Some(params) = self.params_mut() {
            params.required = Some(required_fields);
        }
    }
}
```

3. **Explicit enrichment for non-UUID required fields** - Added `FieldEnrichment` entries for fields like `content` that are required but not UUIDs:

```rust
// src/activations/arbor/methods.rs - describe_by_name()
"node_create_text" => {
    vec![
        FieldEnrichment::uuid("tree_id", "UUID of the tree", true),
        FieldEnrichment::uuid("parent", "UUID of the parent node (optional)", false),
        FieldEnrichment {
            name: "content".to_string(),
            format: None,
            description: Some("Text content for the node".to_string()),
            required: true,  // <-- This is now captured
        },
    ]
}
```

### Result

Clients now receive:

```json
{
  "params": {
    "type": "object",
    "properties": {
      "tree_id": { "type": "string", "format": "uuid", "description": "UUID of the tree" },
      "content": { "type": "string", "description": "Text content for the node" },
      "parent": { "type": ["string", "null"], "format": "uuid" }
    },
    "required": ["tree_id", "content"]
  }
}
```

## Frontend Fix (Haskell Client)

### The Frontend Bug

The Haskell client's `SchemaProperty` type was missing the `propRequired` field, causing the JSON parser to silently drop the `required` array during deserialization.

**Files Changed:**
- `src/Plexus/Schema.hs:141-175` - Added `propRequired :: Maybe [Text]` field
- `src/Plexus/Schema.hs:257` - Fixed parameter extraction to use `propRequired paramsProp`
- `src/Plexus/Dynamic.hs:119` - Sorted parameters to show required first

**Before:**
```
Usage: synapse arbor node-create-text
         [--content TEXT] [--metadata JSON] [--parent UUID] [--tree-id UUID]
         ^^^^^^^^^^^^^^^^^^^^^ all optional (all in brackets)
```

**After:**
```
Usage: synapse arbor node-create-text
         --content TEXT --tree-id UUID [--metadata JSON] [--parent UUID]
         ^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^  required first, no brackets
```

## Cache Invalidation Hash

### The Problem

CLI frontends cache the schema for performance, but have no efficient way to know when to refresh. Options were:

1. **Always fetch** - Slow, defeats caching
2. **Time-based expiry** - May serve stale schema
3. **Manual invalidation** - Error-prone

### The Solution: plexus_hash

A new plexus-level RPC method that returns a deterministic hash of all activations:

```json
{"jsonrpc":"2.0","id":1,"method":"plexus_hash","params":[]}
```

Response:

```json
{
  "type": "data",
  "content_type": "plexus.hash",
  "data": { "hash": "a1b2c3d4e5f67890" }
}
```

### Hash Computation

The hash is computed from a deterministic string of all activation namespaces, versions, and method names:

```rust
// src/plexus/plexus.rs - compute_hash()
pub fn compute_hash(&self) -> String {
    // Build deterministic string: "namespace:version:method1,method2,..."
    let mut activation_strings: Vec<String> = self
        .activations
        .values()
        .map(|a| {
            let mut methods: Vec<&str> = a.methods();
            methods.sort();
            format!("{}:{}:{}", a.namespace(), a.version(), methods.join(","))
        })
        .collect();
    activation_strings.sort();

    let combined = activation_strings.join(";");

    let mut hasher = DefaultHasher::new();
    combined.hash(&mut hasher);
    format!("{:016x}", hasher.finish())
}
```

### What Changes the Hash

- Activation added or removed
- Activation version changed
- Activation methods added/removed/renamed

### What Does NOT Change the Hash

- Schema enrichment details (format, description, required)
- Internal activation implementation

This is intentional - if an activation's external interface changes, the hash changes. Internal improvements that don't affect the API don't trigger cache invalidation.

### Frontend Usage Pattern

```haskell
-- Pseudocode
cachedHash <- readCacheFile "plexus_hash"
currentHash <- call "plexus_hash" []

if cachedHash /= currentHash then do
    -- Refresh all activation schemas
    schema <- call "substrate.schema" []
    forM_ (activations schema) $ \activation -> do
        enriched <- call "substrate.activation_schema" [namespace activation]
        writeCacheFile (namespace activation) enriched
    writeCacheFile "plexus_hash" currentHash
else
    -- Use cached schemas
    loadFromCache
```

## Available Plexus Methods

After this change, the plexus exposes three methods:

| Method | Purpose |
|--------|---------|
| `substrate.schema` | List all activations and method counts |
| `substrate.activation_schema` | Get enriched schema for one activation |
| `plexus_hash` | Get cache invalidation hash |

## Testing

```bash
# Get the hash
echo '{"jsonrpc":"2.0","id":1,"method":"plexus_hash","params":[]}' | websocat ws://127.0.0.1:4444

# Get schema and verify required field
echo '{"jsonrpc":"2.0","id":1,"method":"substrate.activation_schema","params":["arbor"]}' | \
  websocat ws://127.0.0.1:4444 | \
  jq '.params.result.data.oneOf[] | select(.properties.method.const == "node_create_text") | .properties.params.required'
# Expected: ["tree_id", "content"]

# Verify frontend help shows required params first
cabal run synapse -- arbor node-create-text --help
# Expected: --content TEXT --tree-id UUID [--metadata JSON] [--parent UUID]
```

## Debugging Process

The bug was discovered by comparing raw substrate responses with cached schema:

```bash
# Raw from substrate (correct)
cat /tmp/raw_schema.txt | sed -n '2p' | jq '.params.result.data.data.oneOf[7].properties.params.required'
# Output: ["tree_id", "content"]

# Cached (after parsing - incorrect before fix)
cat ~/.cache/symbols/schema.json | jq '.enriched.arbor.oneOf[7].properties.params.required'
# Output: null (before fix)
# Output: ["tree_id", "content"] (after fix)
```

This confirmed the backend was sending the data correctly, but the Haskell parser was dropping it.

## Related

- Dynamic CLI Implementation: `16680974430007725567_dynamic-cli-implementation.md`
- Dynamic CLI Architecture: `16681001483371084543_dynamic-cli-architecture.md`
- Self-documenting RPC: `16680998353176467711_self-documenting-rpc.md` (if exists)
- Guided errors: `16680966217191669503_guided-errors.md` (if exists)
