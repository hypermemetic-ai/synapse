# Client Codegen Requirements

## Overview

This document analyzes what is required to automatically generate typed client code from the introspection protocol, what already exists in the substrate/synapse architecture, and what gaps remain.

## Goal

Generate complete typed client modules (e.g., `Activation.Arbor`) from server introspection data, eliminating manual synchronization between server and client type definitions.

## Current Architecture

### Server Side (Substrate)

Each activation defines two parallel type hierarchies:

```
src/activations/arbor/
├── methods.rs    # ArborMethod enum - INPUT types (what clients send)
├── types.rs      # ArborEvent enum - OUTPUT types (what server returns)
└── activation.rs # Glue: dispatches methods, emits events
```

**Input Types (methods.rs):**
```rust
#[derive(JsonSchema)]
#[serde(tag = "method", content = "params")]
pub enum ArborMethod {
    TreeGet { tree_id: Uuid },
    TreeCreate { metadata: Option<Value>, owner_id: String },
    // ...
}
```

**Output Types (types.rs):**
```rust
#[derive(Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ArborEvent {
    TreeCreated { tree_id: TreeId },
    TreeData { tree: Tree },
    NodeCreated { tree_id: TreeId, node_id: NodeId, parent: Option<NodeId> },
    // ...
}
```

**Domain Types (types.rs):**
```rust
pub struct Tree { id: TreeId, root: NodeId, nodes: HashMap<NodeId, Node>, ... }
pub struct Node { id: NodeId, parent: Option<NodeId>, children: Vec<NodeId>, data: NodeType, ... }
pub struct Handle { source: String, source_version: String, identifier: String, ... }
pub enum NodeType { Text { content: String }, External { handle: Handle } }
pub enum ResourceState { Active, ScheduledDelete, Archived }
```

### Client Side (Symbols)

Each activation module mirrors the server types:

```
src/Activation/
├── Arbor.hs   # ArborEvent, Tree, Node, Handle, typed functions
├── Cone.hs    # ConeEvent, ConeConfig, Position, typed functions
├── Bash.hs    # BashEvent, typed functions
└── Health.hs  # HealthEvent, typed functions
```

**Generated Pattern:**
```haskell
-- Domain types (manually written today)
data Tree = Tree { treeId :: TreeId, treeRoot :: NodeId, ... }
data Node = Node { nodeId :: NodeId, nodeParent :: Maybe NodeId, ... }

-- Event sum type (manually written today)
data ArborEvent
  = TreeCreated { eventTreeId :: TreeId }
  | TreeData { eventTree :: Tree }
  | NodeCreated { eventTreeId :: TreeId, eventNodeId :: NodeId, ... }

-- Typed functions (manually written today)
treeGet :: PlexusConnection -> TreeId -> Stream (Of ArborEvent) IO ()
treeCreate :: PlexusConnection -> Maybe Value -> Text -> Stream (Of ArborEvent) IO ()
```

### Introspection Protocol (Current)

| Endpoint | Returns | Exposed Via |
|----------|---------|-------------|
| `substrate.schema` | Module list + method names | Yes |
| `substrate.activation_schema` | JSON Schema for method inputs | Yes |
| `plexus_hash` | Cache invalidation hash | Yes |
| Response event schemas | **Not exposed** | No |
| Domain type schemas | **Not exposed** | No |

## What Already Exists

### ✅ Complete for Input Types

The method schema contains everything needed to generate input types:

| Schema Element | Haskell Output |
|----------------|----------------|
| Method name (`tree_get`) | Function name (`treeGet`) |
| Required params | Non-Maybe arguments |
| Optional params | `Maybe a` arguments |
| `type: "string", format: "uuid"` | `UUID` type |
| `type: "string"` | `Text` type |
| `type: "integer"` | `Int` type |
| `type: "boolean"` | `Bool` type |
| `type: "array"` | `[a]` type |
| `type: "object"` with properties | Record type |
| `$defs` references | Shared type definitions |
| `oneOf` | Sum types |
| `description` | Haddock comments |

**Example: Method schema → Function signature**

Schema:
```json
{
  "method": { "const": "tree_get" },
  "properties": {
    "tree_id": { "type": "string", "format": "uuid", "description": "UUID of the tree" }
  },
  "required": ["tree_id"]
}
```

Generated:
```haskell
-- | UUID of the tree
treeGet :: PlexusConnection -> UUID -> Stream (Of ArborEvent) IO ()
```

### ✅ Dynamic CLI Already Does This

`symbols/src/Plexus/Dynamic.hs` already:
- Parses method schemas
- Maps JSON Schema types to CLI argument types
- Handles required vs optional
- Resolves `$ref` references
- Generates help text from descriptions

This proves the input side is fully achievable.

### ❌ Missing for Output Types

The server DOES define output types (`ArborEvent`, `Tree`, `Node`) but does NOT expose them via introspection:

| What's Needed | Where It Exists | Exposed? |
|---------------|-----------------|----------|
| Event variants | `ArborEvent` enum in `types.rs` | No |
| Event field types | Struct fields in `types.rs` | No |
| Domain types | `Tree`, `Node`, `Handle` in `types.rs` | No |
| Type discriminator | `#[serde(tag = "type")]` attribute | No |
| Content type | `ActivationStreamItem::content_type()` | Implicit only |

## Gap Analysis

### Server Changes Required

To enable full codegen, the server needs to expose response schemas:

**Option A: Add response schema endpoint**
```
plexus_activation_response_schema(namespace) -> JSON Schema of event types
```

**Option B: Extend existing schema endpoint**
```
substrate.activation_schema(namespace) -> {
  "methods": { ... },       // existing
  "responses": { ... },     // new: event type schema
  "types": { ... }          // new: shared domain types
}
```

**Option C: Convention-based**
- Require server types to derive `JsonSchema`
- Add `ArborEvent::schema()` similar to `ArborMethod::schema()`
- Expose via new introspection method

### Required Schema Additions

**1. Response Event Schema**

The server already has:
```rust
#[derive(Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ArborEvent {
    TreeCreated { tree_id: TreeId },
    TreeData { tree: Tree },
    // ...
}
```

Needs to add:
```rust
#[derive(Serialize, Deserialize, JsonSchema)]  // Add JsonSchema
#[serde(tag = "type")]
pub enum ArborEvent { ... }
```

And expose:
```rust
pub fn response_schema() -> serde_json::Value {
    schemars::schema_for!(ArborEvent)
}
```

**2. Domain Type Schemas**

Already have `JsonSchema` on some types:
```rust
#[derive(JsonSchema)]
pub struct Handle { ... }
```

Need to ensure all domain types derive `JsonSchema`:
```rust
#[derive(JsonSchema)]
pub struct Tree { ... }

#[derive(JsonSchema)]
pub struct Node { ... }

#[derive(JsonSchema)]
pub enum NodeType { ... }
```

**3. Method-to-Response Mapping**

Need to know which events each method can return:

| Method | Possible Responses |
|--------|-------------------|
| `tree_get` | `TreeData` |
| `tree_create` | `TreeCreated` |
| `tree_list` | `TreeList` |
| `node_create_text` | `NodeCreated` |

Options:
- Explicit annotation in method definition
- Convention: `tree_get` → `TreeData`, `tree_create` → `TreeCreated`
- Documentation-only (codegen produces union type)

## Codegen Architecture

### Proposed Module Structure

```
codegen/
├── SchemaFetcher.hs     # Fetch schemas from running server
├── TypeMapper.hs        # JSON Schema → Haskell types
├── CodeEmitter.hs       # Generate Haskell source
└── Main.hs              # CLI driver

generated/
└── Activation/
    ├── Arbor.hs         # Generated
    ├── Cone.hs          # Generated
    └── Types.hs         # Shared types from $defs
```

### Type Mapping Rules

| JSON Schema | Haskell | Notes |
|-------------|---------|-------|
| `{"type": "string"}` | `Text` | |
| `{"type": "string", "format": "uuid"}` | `UUID` | |
| `{"type": "string", "format": "date-time"}` | `UTCTime` | |
| `{"type": "integer"}` | `Int` | |
| `{"type": "number"}` | `Double` | |
| `{"type": "boolean"}` | `Bool` | |
| `{"type": "array", "items": X}` | `[X]` | Recursive |
| `{"type": "object", "properties": {...}}` | Record | Generate data type |
| `{"type": ["string", "null"]}` | `Maybe Text` | Nullable |
| `{"oneOf": [...]}` | Sum type | `data X = A \| B \| C` |
| `{"enum": ["a", "b"]}` | Sum type | `data X = A \| B` |
| `{"$ref": "#/$defs/Foo"}` | `Foo` | Reference shared type |
| `{"additionalProperties": X}` | `Map Text X` | |

### Naming Conventions

| Schema | Haskell |
|--------|---------|
| `tree_get` | `treeGet` (function) |
| `TreeGet` | `TreeGet` (type) |
| `tree_id` | `treeId` (field) |
| `ArborMethod` | `Arbor.Method` or `ArborMethod` |

### Generated Code Template

```haskell
-- | AUTO-GENERATED from plexus schema
-- | Do not edit manually - regenerate with: codegen --activation arbor
module Activation.Arbor
  ( -- * Types
    ArborEvent(..)
  , Tree(..)
  , Node(..)
  , Handle(..)
  , NodeType(..)
    -- * Operations
  , treeGet
  , treeCreate
  , treeList
  -- ...
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Streaming (Stream, Of)

import Plexus.Client (PlexusConnection, plexusRpc)
import Plexus.Types (PlexusStreamItem(..))

-- ============================================================================
-- Domain Types (from response schema $defs)
-- ============================================================================

-- | Handle pointing to external data with versioning
data Handle = Handle
  { handleSource        :: Text
  , handleSourceVersion :: Text
  , handleIdentifier    :: Text
  , handleMetadata      :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Node type discriminator
data NodeType
  = TextNode { nodeContent :: Text }
  | ExternalNode { nodeHandle :: Handle }
  deriving stock (Show, Eq, Generic)
  -- Custom FromJSON/ToJSON for tagged union

-- ... more types ...

-- ============================================================================
-- Event Types (from response schema oneOf)
-- ============================================================================

-- | Events emitted by Arbor operations
data ArborEvent
  = TreeCreated { eventTreeId :: Text }
  | TreeData { eventTree :: Tree }
  | TreeList { eventTreeIds :: [Text] }
  | NodeCreated { eventTreeId :: Text, eventNodeId :: Text, eventParent :: Maybe Text }
  -- ... more variants ...
  deriving stock (Show, Eq, Generic)

instance FromJSON ArborEvent where
  -- Generated from serde tag = "type" pattern
  parseJSON = withObject "ArborEvent" $ \o -> do
    typ <- o .: "type"
    case typ :: Text of
      "tree_created" -> TreeCreated <$> o .: "tree_id"
      "tree_data"    -> TreeData <$> o .: "tree"
      -- ... more cases ...

-- ============================================================================
-- Operations (from method schema)
-- ============================================================================

-- | Get a complete tree with all nodes
treeGet :: PlexusConnection -> UUID -> Stream (Of ArborEvent) IO ()
treeGet conn treeId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_get" (toJSON [treeId])

-- | Create a new conversation tree
treeCreate
  :: PlexusConnection
  -> Maybe Value  -- ^ Optional metadata
  -> Text         -- ^ Owner ID
  -> Stream (Of ArborEvent) IO ()
treeCreate conn metadata ownerId =
  S.mapMaybe extractArborEvent $
    plexusRpc conn "arbor_tree_create" (toJSON [toJSON metadata, toJSON ownerId])

-- ... more operations ...

-- ============================================================================
-- Internal
-- ============================================================================

extractArborEvent :: PlexusStreamItem -> Maybe ArborEvent
extractArborEvent (StreamData _ _ contentType dat)
  | contentType == "arbor.event" =
      case fromJSON dat of
        Success evt -> Just evt
        Error _     -> Nothing
  | otherwise = Nothing
extractArborEvent _ = Nothing
```

## Implementation Phases

### Phase 1: Response Schema Exposure (Server)

1. Add `JsonSchema` derive to all event enums
2. Add `JsonSchema` derive to all domain types
3. Implement `plexus_activation_response_schema` endpoint
4. Include content_type in schema metadata

**Estimated scope:** ~200 lines across activation modules

### Phase 2: Schema Fetcher (Client)

1. Extend `Plexus.Schema` to parse response schemas
2. Add response schema to `CachedSchema`
3. Update cache invalidation to include response schemas

**Estimated scope:** ~100 lines

### Phase 3: Type Mapper

1. JSON Schema → Haskell AST
2. Handle all type mappings (see table above)
3. Resolve $ref references
4. Generate record types with field accessors
5. Generate sum types with constructors

**Estimated scope:** ~400 lines

### Phase 4: Code Emitter

1. Haskell AST → Source text
2. Module header generation
3. Import management
4. Proper formatting

**Estimated scope:** ~300 lines

### Phase 5: Integration

1. CLI tool: `codegen --server localhost:4444 --output src/Generated/`
2. Build system integration
3. Staleness detection (compare hashes)

**Estimated scope:** ~150 lines

## Alternative: Hybrid Approach

Instead of full codegen, generate only what's mechanical and keep hand-written parts:

```haskell
-- Generated: Activation/Arbor/Types.hs
module Activation.Arbor.Types where
-- All types generated from schema

-- Hand-written: Activation/Arbor.hs
module Activation.Arbor (module Activation.Arbor.Types, ...) where
import Activation.Arbor.Types
-- Hand-written convenience functions, re-exports
```

Benefits:
- Types stay in sync automatically
- Custom helpers remain hand-written
- Gradual adoption

## Summary

| Component | Exists? | Exposed? | Codegen Ready? |
|-----------|---------|----------|----------------|
| Method schemas | ✅ | ✅ | ✅ |
| Method descriptions | ✅ | ✅ | ✅ |
| Required/optional params | ✅ | ✅ | ✅ |
| Type formats (uuid, etc) | ✅ | ✅ | ✅ |
| Shared input types ($defs) | ✅ | ✅ | ✅ |
| Response event types | ✅ | ❌ | ❌ |
| Domain types (Tree, Node) | ✅ | ❌ | ❌ |
| Event discriminator tag | ✅ | ❌ | ❌ |
| Content type mapping | ✅ | Implicit | Partial |
| Method→Response mapping | Implicit | ❌ | ❌ |

**Bottom line:** Input types (method signatures) are fully codegen-ready today. Output types (events, domain objects) require exposing response schemas from the server—the types exist, they just aren't introspectable yet.
