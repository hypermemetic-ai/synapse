# Dynamic CLI Implementation

A CLI that discovers its command structure from the substrate at runtime, generating subcommands, flags, and help text from the server's schema.

## Implementation Status

| Phase | Status | Notes |
|-------|--------|-------|
| Phase 1: Schema Types | ✅ Done | `src/Plexus/Schema.hs` - includes EnrichedSchema types |
| Phase 2: Schema Fetching | ✅ Done | Via `substrate.schema` + `substrate.activation_schema` RPC |
| Phase 3: Schema Caching | ✅ Done | `src/Plexus/Schema/Cache.hs` - caches enriched schemas |
| Phase 4: Dynamic Parser | ✅ Done | Typed flags via index-based schema lookup |
| Phase 5: Main Entry Point | ✅ Done | `app/Dyn.hs` |
| Phase 6: Help Generation | ✅ Done | Typed flags with descriptions from schema |

**Resolved:** The enriched schema doesn't include method name identifiers (method property is just `{"type": "string"}`), but we work around this by using **index-based lookup** - the methods list in `ActivationInfo` is in the same order as the `oneOf` variants in the enriched schema.

**Implementation:**
- `buildTypedMethodParser` generates typed flags from `MethodSchema`
- `parseMethodVariantByIndex` looks up schema by index, not name
- Enriched schemas are fetched via `substrate.activation_schema` RPC
- Schemas are cached in `~/.cache/symbols/schema.json`
- Help shows typed flags: `--tree-id UUID`, `--owner-id TEXT`, etc.

## Server Schema Types

The substrate exposes these types via `substrate.schema` subscription:

```rust
// PlexusSchema - top level response
PlexusSchema {
    activations: Vec<ActivationInfo>,
    total_methods: usize,
}

// ActivationInfo - per activation
ActivationInfo {
    namespace: String,    // "arbor", "cone", "health", "bash"
    version: String,      // "1.0.0"
    description: String,  // "Arbor tree storage operations"
    methods: Vec<String>, // ["tree_create", "tree_get", ...]
}

// Enriched Schema (via enrich_schema()) - full param details
Schema {
    one_of: Vec<Schema>,                      // Method variants
    properties: HashMap<String, SchemaProperty>,
}

SchemaProperty {
    property_type: Value,          // "string", "integer", ["string", "null"]
    description: Option<String>,   // "The tree identifier"
    format: Option<String>,        // "uuid"
    required: Option<Vec<String>>,
}
```

## Implementation Phases

### Phase 1: Schema Types

**File: `src/Plexus/Schema.hs`**

```haskell
-- Mirror server types
data ActivationInfo = ActivationInfo
  { activationNamespace   :: Text
  , activationVersion     :: Text
  , activationDescription :: Text
  , activationMethods     :: [Text]
  }

data PlexusSchema = PlexusSchema
  { schemaActivations  :: [ActivationInfo]
  , schemaTotalMethods :: Int
  }

-- Parsed method with params (from enriched schema)
data MethodSchema = MethodSchema
  { methodName        :: Text
  , methodDescription :: Maybe Text
  , methodParams      :: [ParamSchema]
  }

data ParamSchema = ParamSchema
  { paramName     :: Text
  , paramType     :: Text       -- "string", "integer", "object"
  , paramFormat   :: Maybe Text -- "uuid"
  , paramRequired :: Bool
  , paramDesc     :: Maybe Text
  }
```

### Phase 2: Schema Fetching

**Add to `src/Plexus/Client.hs`:**

```haskell
-- Fetch plexus schema
plexusSchema :: PlexusConnection -> Stream (Of PlexusSchema) IO ()
plexusSchema conn =
  S.mapMaybe extractSchema $
    plexusRpc conn "substrate.schema" (toJSON ([] :: [Value]))
```

The `substrate.schema` subscription returns schema with content_type `"substrate.schema"`.

### Phase 3: Schema Caching

**File: `src/Plexus/Schema/Cache.hs`**

```haskell
data CachedSchema = CachedSchema
  { cachedAt     :: UTCTime
  , cachedTTL    :: Int              -- seconds, default 3600
  , cachedSchema :: PlexusSchema
  }

-- Cache path: ~/.symbols/schema.json
defaultCachePath :: IO FilePath

-- Load cached schema if exists and fresh
loadCache :: FilePath -> IO (Maybe CachedSchema)

-- Save schema to cache
saveCache :: FilePath -> CachedSchema -> IO ()

-- Check if cache is still valid
isFresh :: CachedSchema -> IO Bool

-- Load schema: cache-first, fetch if stale/missing
loadSchemaWithCache :: Bool -> PlexusConfig -> IO PlexusSchema
loadSchemaWithCache forceRefresh config = do
  path <- defaultCachePath
  unless forceRefresh $ do
    mCached <- loadCache path
    case mCached of
      Just c | isFresh c -> return (cachedSchema c)
      _ -> fetchAndCache path config
```

### Phase 4: Dynamic Parser Generation

**File: `src/Plexus/Dynamic.hs`**

Core algorithm:

```
PlexusSchema
    │
    ▼
For each ActivationInfo:
    │
    ├─► subparser "arbor" with progDesc "Arbor tree storage"
    │       │
    │       ▼
    │   For each method in activationMethods:
    │       │
    │       ├─► command "tree-create"
    │       │       │
    │       │       ▼
    │       │   For each param in methodParams:
    │       │       ├─► --tree-id UUID (required)
    │       │       ├─► --metadata JSON (optional)
    │       │       └─► ...
    │       │
    │       └─► command "tree-get" ...
    │
    └─► subparser "cone" ...
```

**Param type mapping:**

| Schema Type | Format | CLI Flag |
|-------------|--------|----------|
| string | uuid | `--tree-id UUID` |
| string | - | `--name TEXT` |
| integer | - | `--count N` |
| boolean | - | `--flag` (switch) |
| object | - | `--metadata JSON` |
| array | - | `--ids ID [ID...]` |
| nullable X | - | optional flag |

**Result type:**

```haskell
data CommandInvocation = CommandInvocation
  { invMethod :: Text       -- "arbor_tree_create"
  , invParams :: Value      -- JSON array: [treeId, metadata, ...]
  }
```

### Phase 5: Main Entry Point

**File: `app/Dyn.hs`**

```haskell
data GlobalOpts = GlobalOpts
  { optRefresh :: Bool    -- --refresh: force schema refetch
  , optHost    :: String  -- --host
  , optPort    :: Int     -- --port
  , optJson    :: Bool    -- --json: raw JSON output
  }

main :: IO ()
main = do
  -- 1. Parse global options first (before schema needed)
  (globalOpts, remaining) <- parseGlobalOpts

  -- 2. Load schema (uses cache unless --refresh)
  schema <- loadSchemaWithCache (optRefresh globalOpts) plexusConfig

  -- 3. Build dynamic parser from schema
  let dynamicParser = buildParser schema

  -- 4. Parse remaining args with dynamic parser
  invocation <- execParserWith dynamicParser remaining

  -- 5. Execute RPC and stream results
  conn <- connect plexusConfig
  executeCommand conn invocation
  disconnect conn
```

### Phase 6: Help Generation

Help text derived from schema:

```
$ synapse --help
synapse - Dynamic CLI for Plexus

Usage: synapse [--refresh] [--host HOST] [--port PORT] COMMAND

Commands:
  health    Health check
  bash      Execute shell commands
  arbor     Arbor tree storage operations
  cone      Cone LLM operations

$ synapse arbor --help
Arbor tree storage operations

Commands:
  tree-create       Create a new conversation tree
  tree-get          Get full tree with all nodes
  tree-get-skeleton Get tree structure without data
  node-create-text  Create a text node
  ...

$ synapse arbor tree-create --help
Create a new conversation tree

Options:
  --metadata JSON    Tree metadata (optional)
  --owner-id TEXT    Owner identifier (required)
  -h,--help          Show this help text
```

## File Summary

| File | Action | Purpose |
|------|--------|---------|
| `src/Plexus/Schema.hs` | Create | Schema types (ActivationInfo, PlexusSchema, etc.) |
| `src/Plexus/Schema/Cache.hs` | Create | Cache load/save/freshness |
| `src/Plexus/Dynamic.hs` | Create | Parser generation from schema |
| `src/Plexus/Client.hs` | Modify | Add `plexusSchema` function |
| `app/Dyn.hs` | Replace | New dynamic main |
| `symbols.cabal` | Modify | Add modules and deps |

## Dependencies

```cabal
build-depends:
    ...
    directory >= 1.3,    -- for cache path
    time >= 1.9,         -- for cache timestamps
    filepath >= 1.4      -- for path manipulation
```

## Error Handling

1. **Schema fetch fails**: Use stale cache with warning
2. **No cache, fetch fails**: Error with connection instructions
3. **Unknown method**: Suggest `--refresh` to update schema
4. **Invalid params**: Show expected types from schema

## Future Enhancements

1. **Enriched schemas**: Fetch per-activation schemas for full param details
2. **Shell completions**: Generate bash/zsh completions from schema
3. **Interactive mode**: REPL with tab completion
4. **Schema versioning**: Invalidate cache on server version change
