# Schema Type Deduplication: Analysis and Solutions

## Problem Statement

When generating TypeScript clients from Plexus schemas, types defined in parent plugins are duplicated across all child plugins. For example, the `SolarEvent` type defined in the `solar` plugin appears 17 times in the IR:

```
solar.SolarEvent
jupiter.SolarEvent
io.SolarEvent
europa.SolarEvent
ganymede.SolarEvent
callisto.SolarEvent
saturn.SolarEvent
titan.SolarEvent
... (and 9 more)
```

This results in:
1. **Bloated generated code** - identical types repeated in each namespace module
2. **Type incompatibility** - `jupiter.SolarEvent` is not assignable to `solar.SolarEvent` even though they're identical
3. **Confusion** - unclear which namespace "owns" a type

## Root Cause Analysis

### 1. Schemars Generates Complete Schemas

The Rust `schemars` crate generates self-contained JSON schemas. Each call to `schema_for!(T)` includes all referenced types in `$defs`:

```rust
// In hub-macro/src/codegen/method_enum.rs (lines 81-97)
let return_schema_entries: Vec<TokenStream> = methods
    .iter()
    .map(|m| {
        if let Some(item_ty) = &m.stream_item_type {
            // PROBLEM: Each call generates complete schema with all $defs
            quote! { (Some(schemars::schema_for!(#item_ty)), Vec::<&str>::new()) }
        } else {
            quote! { (None, Vec::<&str>::new()) }
        }
    })
    .collect();
```

### 2. Child Plugins Embed Parent Types

When `jupiter.info()` returns `SolarEvent`, the generated schema includes:

```json
{
  "namespace": "jupiter",
  "methods": [{
    "name": "info",
    "returns": {
      "$defs": {
        "BodyType": { "enum": ["star", "planet", "dwarf_planet", "moon"] },
        "SolarEvent": { "oneOf": [...] }
      },
      "oneOf": [{ "$ref": "#/$defs/SolarEvent" }]
    }
  }]
}
```

The same `$defs` appear in every celestial body's schema because each independently calls `schema_for!(SolarEvent)`.

### 3. No Cross-Plugin Type Sharing

Plexus returns each plugin's schema independently via `substrate.schema` and `<namespace>.schema`. There's no mechanism to:
- Identify types that appear in multiple plugins
- Reference a canonical definition
- Deduplicate before returning to clients

### 4. Synapse Namespace-Qualifies All Types

The IR builder qualifies each type with its plugin's namespace:

```haskell
-- In Synapse/IR/Builder.hs
extractDefs :: Text -> KM.KeyMap Value -> Map Text TypeDef
extractDefs namespace o =
  -- Types get qualified: "jupiter" + "SolarEvent" = "jupiter.SolarEvent"
  Map.fromList $ mapMaybe (extractTypeDef namespace) (KM.toList defs)
```

Since `jupiter.SolarEvent` and `io.SolarEvent` have different qualified names, they're treated as distinct types.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                           Current Flow                                   │
└─────────────────────────────────────────────────────────────────────────┘

  Rust Types          schemars            Plexus              Synapse IR
  ──────────          ────────            ──────              ──────────

  SolarEvent  ──────► schema_for!() ────► solar.schema ────► solar.SolarEvent
       │
       │              schema_for!() ────► jupiter.schema ──► jupiter.SolarEvent
       │
       │              schema_for!() ────► io.schema ───────► io.SolarEvent
       │
       └──────────────────────────────────────────────────────► (17 copies!)


┌─────────────────────────────────────────────────────────────────────────┐
│                           Desired Flow                                   │
└─────────────────────────────────────────────────────────────────────────┘

  Rust Types          schemars            Plexus              Synapse IR
  ──────────          ────────            ──────              ──────────

  SolarEvent  ──────► schema_for!() ────► Shared $defs ─────► solar.SolarEvent
       │                    │                  │                    ▲
       │                    │                  │                    │
       │              (reuse cached)    jupiter.schema ────► $ref ──┘
       │                    │                  │
       │              (reuse cached)    io.schema ─────────► $ref ──┘
       │
       └──────────────────────────────────────────────────────► (1 copy!)
```

## Solution Options

### Option A: Schema Registry at Compile Time (hub-macro)

**Approach**: Modify the hub-macro code generator to emit schemas that reference a shared type registry instead of embedding types inline.

**Implementation**:

1. **Create a type registry macro**:
   ```rust
   // New: hub-macro/src/codegen/type_registry.rs

   /// Collect all types used across methods into a shared registry
   pub fn generate_type_registry(methods: &[MethodDef]) -> TokenStream {
       let mut seen_types = HashSet::new();
       let mut registry_entries = Vec::new();

       for method in methods {
           if let Some(ty) = &method.stream_item_type {
               let type_name = quote!(#ty).to_string();
               if seen_types.insert(type_name.clone()) {
                   registry_entries.push(quote! {
                       (#type_name, schemars::schema_for!(#ty))
                   });
               }
           }
       }

       quote! {
           fn type_registry() -> std::collections::HashMap<&'static str, schemars::schema::Schema> {
               [#(#registry_entries),*].into_iter().collect()
           }
       }
   }
   ```

2. **Modify method schema generation** to use `$ref`:
   ```rust
   // Instead of embedding full schema:
   quote! { (Some(schemars::schema_for!(#item_ty)), Vec::<&str>::new()) }

   // Generate a reference:
   quote! { (Some(json!({"$ref": concat!("#/$defs/", stringify!(#item_ty))})), Vec::<&str>::new()) }
   ```

3. **Attach shared `$defs` to plugin schema**:
   ```rust
   impl Activation for MyPlugin {
       fn plugin_schema(&self) -> PluginSchema {
           PluginSchema::new(...)
               .with_shared_defs(Self::type_registry())
       }
   }
   ```

**Pros**:
- Types deduplicated at source
- No runtime overhead
- Clean separation of concerns

**Cons**:
- Requires significant macro changes
- May need changes to PluginSchema structure
- Complex to implement correctly

**Estimated Effort**: High (2-3 days)

---

### Option B: Runtime Deduplication in Plexus (hub-core)

**Approach**: When returning schemas, Plexus collects all `$defs` from the tree, deduplicates them, and returns a flattened schema with shared types.

**Implementation**:

1. **Add deduplication to schema collection**:
   ```rust
   // In hub-core/src/plexus/plexus.rs

   /// Collect schemas with deduplicated type definitions
   pub fn collect_deduplicated_schemas(&self) -> DeduplicatedSchema {
       let mut all_defs: HashMap<String, Value> = HashMap::new();
       let mut schemas: Vec<PluginSchema> = Vec::new();

       for activation in self.activations() {
           let mut schema = activation.plugin_schema();

           // Extract $defs from each method's returns
           for method in &mut schema.methods {
               if let Some(returns) = &mut method.returns {
                   if let Some(defs) = returns.get("$defs") {
                       // Merge into global defs (first wins)
                       for (name, def) in defs.as_object().unwrap() {
                           all_defs.entry(name.clone()).or_insert(def.clone());
                       }
                       // Remove local $defs
                       returns.as_object_mut().unwrap().remove("$defs");
                   }
               }
           }

           schemas.push(schema);
       }

       DeduplicatedSchema { schemas, shared_defs: all_defs }
   }
   ```

2. **Add new RPC method**:
   ```rust
   // {backend}.full_schema_deduplicated or enhance existing plexus.schema
   #[hub_method]
   async fn full_schema(&self) -> PlexusFullSchema {
       let deduped = self.collect_deduplicated_schemas();
       PlexusFullSchema {
           version: "1.0",
           shared_defs: deduped.shared_defs,
           plugins: deduped.schemas,
       }
   }
   ```

3. **Update schema structure**:
   ```rust
   // In hub-core/src/plexus/schema.rs

   #[derive(Serialize, JsonSchema)]
   pub struct PlexusFullSchema {
       pub version: String,
       #[serde(rename = "$defs")]
       pub shared_defs: HashMap<String, Value>,
       pub plugins: Vec<PluginSchema>,
   }
   ```

**Pros**:
- Centralized fix - clients automatically benefit
- Backward compatible (add new method, keep old)
- Reasonable implementation complexity

**Cons**:
- Runtime overhead (though minimal)
- Requires protocol version awareness in clients
- "First wins" may not always pick the best definition

**Estimated Effort**: Medium (1-2 days)

---

### Option C: Synapse IR Post-Processing (synapse)

**Approach**: After building the IR, deduplicate types by content hash or unqualified name.

**Implementation**:

1. **Add content-based deduplication**:
   ```haskell
   -- In Synapse/IR/Builder.hs

   -- | Deduplicate types that have identical structure
   deduplicateTypes :: IR -> IR
   deduplicateTypes ir = ir { irTypes = deduped, irMethods = updated }
     where
       -- Group types by their structural hash (ignoring namespace)
       typesByHash :: Map TypeHash [(Text, TypeDef)]
       typesByHash = Map.fromListWith (++)
         [ (hashTypeDef td, [(fullName, td)])
         | (fullName, td) <- Map.toList (irTypes ir)
         ]

       -- Pick canonical version (prefer shorter namespace, then alphabetical)
       canonical :: Map TypeHash (Text, TypeDef)
       canonical = Map.map (minimumBy compareNamespace) typesByHash

       -- Build redirect map: old qualified name -> canonical qualified name
       redirects :: Map Text Text
       redirects = Map.fromList
         [ (oldName, canonicalName)
         | (_, dups) <- Map.toList typesByHash
         , let canonicalName = fst (head (sortBy compareNamespace dups))
         , (oldName, _) <- dups
         , oldName /= canonicalName
         ]

       -- Deduplicated types (canonical versions only)
       deduped = Map.fromList [(name, td) | (_, (name, td)) <- Map.toList canonical]

       -- Update method references
       updated = Map.map (updateRefs redirects) (irMethods ir)

   -- | Hash a TypeDef by its structure (ignoring namespace)
   hashTypeDef :: TypeDef -> TypeHash
   hashTypeDef TypeDef{..} = hash (tdName, tdKind)

   -- | Update type references in a method
   updateRefs :: Map Text Text -> MethodDef -> MethodDef
   updateRefs redirects md = md
     { mdParams = map (updateParamRef redirects) (mdParams md)
     , mdReturns = updateTypeRef redirects (mdReturns md)
     }
   ```

2. **Integrate into buildIR**:
   ```haskell
   buildIR :: Path -> SynapseM IR
   buildIR path = do
       raw <- walkSchema irAlgebra path
       pure (deduplicateTypes raw)
   ```

**Pros**:
- No backend changes required
- Can be implemented incrementally
- Handles any duplication source

**Cons**:
- Client-side fix (each client must implement)
- Doesn't reduce data transfer
- More complex type reference tracking

**Estimated Effort**: Medium (1-2 days)

---

### Option D: Hybrid - Backend Registry + Client Awareness (Recommended)

**Approach**: Combine Options A and B for a comprehensive solution.

**Phase 1: Backend Type Registry** (Option B, simplified)

Add a `$defs` hoisting mechanism to Plexus that:
1. Collects all type definitions when building schemas
2. Stores them in a top-level `$defs` keyed by unqualified name
3. Replaces inline definitions with `$ref` pointers

```rust
// Enhanced plexus.schema response
{
  "version": "1.0",
  "hash": "abc123...",
  "$defs": {
    "SolarEvent": { /* single definition */ },
    "BodyType": { /* single definition */ },
    "ConeIdentifier": { /* single definition */ }
  },
  "plugins": {
    "solar": {
      "methods": [{
        "name": "observe",
        "returns": { "$ref": "#/$defs/SolarEvent" }
      }]
    },
    "jupiter": {
      "methods": [{
        "name": "info",
        "returns": { "$ref": "#/$defs/SolarEvent" }
      }]
    }
  }
}
```

**Phase 2: Smart Client Resolution**

Update Synapse IR builder to:
1. First check top-level `$defs` for type definitions
2. Resolve `$ref` pointers to canonical types
3. Qualify types by their *defining* namespace (where first seen)

```haskell
-- When encountering a $ref
resolveRef :: Text -> SchemaContext -> TypeRef
resolveRef refPath ctx
  | "#/$defs/" `T.isPrefixOf` refPath =
      let typeName = T.drop 8 refPath  -- Remove "#/$defs/"
          definingNs = lookupDefiningNamespace typeName ctx
      in RefNamed (definingNs <> "." <> typeName)
  | otherwise = RefUnknown
```

**Phase 3: Namespace Ownership Hints**

Extend the schema to include type ownership:

```json
{
  "$defs": {
    "SolarEvent": {
      "x-namespace": "solar",
      "x-defined-in": "solar/types.rs",
      "oneOf": [...]
    }
  }
}
```

This allows clients to correctly attribute types to their defining namespace.

**Pros**:
- Comprehensive solution
- Backward compatible (clients can ignore hints)
- Enables future tooling (type provenance, docs)
- Single source of truth

**Cons**:
- Requires coordinated changes
- More complex schema structure

**Estimated Effort**: High (3-4 days total, can be phased)

---

## Recommended Implementation Order

### Immediate (This PR)
- Document the issue (this file)
- Add a TODO comment in Synapse IR builder noting the duplication

### Short-term (Next Sprint)
1. **Implement Option B** - Runtime deduplication in Plexus
   - Modify `plexus.full_schema` to hoist `$defs`
   - Add `x-namespace` hints to type definitions
   - Update schema documentation

2. **Update Synapse** to handle hoisted `$defs`
   - Resolve `$ref` pointers
   - Use `x-namespace` for type qualification

### Medium-term (Future)
3. **Implement Option A** - Compile-time registry
   - Refactor hub-macro to generate type registries
   - Reduce runtime deduplication needs

4. **Enhance tooling**
   - Add type provenance to generated docs
   - Warn on type shadowing across namespaces

## Testing Strategy

### Unit Tests
- Verify type deduplication produces single canonical type
- Verify `$ref` resolution works correctly
- Verify cross-namespace references resolve

### Integration Tests
- Solar system plugin produces single `SolarEvent` type
- Generated TypeScript compiles without duplicate identifier errors
- Types are assignable across namespace boundaries

### Regression Tests
- Existing schemas continue to work
- No breaking changes to `substrate.schema` response

## Appendix: Affected Files

### Backend (Substrate)
- `hub-macro/src/codegen/method_enum.rs` - Schema generation
- `hub-macro/src/codegen/activation.rs` - Plugin schema building
- `hub-core/src/plexus/plexus.rs` - Schema collection
- `hub-core/src/plexus/schema.rs` - Schema structures

### Synapse (Haskell)
- `src/Synapse/IR/Builder.hs` - IR construction
- `src/Synapse/IR/Types.hs` - TypeDef structure
- `src/Synapse/Schema/Types.hs` - Schema parsing

### Hub-Codegen (Rust)
- `src/ir.rs` - IR structures
- `src/generator/types.rs` - Type generation
