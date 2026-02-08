# JSON Schema $ref Resolution Implementation

**Status:** Implemented
**Date:** 2025-12-18
**Affects:** `substrate.activation_schema` parsing, CLI parameter generation

## Overview

Implemented automatic JSON Schema `$ref` resolution to handle complex enum types like `ConeIdentifier`. When substrate sends schemas with references to `$defs`, Symbols now automatically inlines the definitions during parsing.

## The Problem

Substrate sends schemas with `$ref` pointers to shared definitions:

```json
{
  "oneOf": [{
    "properties": {
      "method": {"const": "cone_chat"},
      "params": {
        "properties": {
          "identifier": {
            "$ref": "#/$defs/ConeIdentifier",
            "description": "Cone identifier (provide either name or id)"
          },
          "prompt": {"type": "string"}
        }
      }
    }
  }],
  "$defs": {
    "ConeIdentifier": {
      "oneOf": [
        {
          "properties": {
            "by_name": {
              "properties": {"name": {"type": "string"}},
              "required": ["name"]
            }
          },
          "required": ["by_name"]
        },
        {
          "properties": {
            "by_id": {
              "properties": {"id": {"type": "string", "format": "uuid"}},
              "required": ["id"]
            }
          },
          "required": ["by_id"]
        }
      ]
    }
  }
}
```

**Before resolution:**
- CLI showed: `--identifier JSON`
- User had no idea what JSON structure to provide
- Help text incomplete

**After resolution:**
- `$ref` replaced with actual definition
- CLI can show full structure (when we implement that)
- Schema introspection works correctly

## Implementation

### Type Changes

Added fields to support `$ref` and `$defs`:

```haskell
-- src/Plexus/Schema.hs

data EnrichedSchema = EnrichedSchema
  { schemaTitle       :: Maybe Text
  , schemaDescription :: Maybe Text
  , schemaType        :: Maybe Value
  , schemaProperties  :: Maybe (Map Text SchemaProperty)
  , schemaRequired    :: Maybe [Text]
  , schemaOneOf       :: Maybe [EnrichedSchema]
  , schemaDefs        :: Maybe (Map Text Value)  -- NEW: Schema definitions
  , schemaRef         :: Maybe Text              -- NEW: Reference pointer
  }

data SchemaProperty = SchemaProperty
  { propType        :: Maybe Value
  , propDescription :: Maybe Text
  , propFormat      :: Maybe Text
  , propItems       :: Maybe SchemaProperty
  , propDefault     :: Maybe Value
  , propEnum        :: Maybe [Value]
  , propProperties  :: Maybe (Map Text SchemaProperty)
  , propRequired    :: Maybe [Text]
  , propOneOf       :: Maybe [SchemaProperty]  -- NEW: For enum variants
  , propRef         :: Maybe Text              -- NEW: Reference pointer
  }
```

### Resolution Algorithm

**Automatic resolution during parsing:**

```haskell
instance FromJSON EnrichedSchema where
  parseJSON val@(Object o) = do
    -- Parse base schema
    base <- EnrichedSchema
      <$> o .:? "title"
      <*> o .:? "description"
      <*> o .:? "type"
      <*> o .:? "properties"
      <*> o .:? "required"
      <*> o .:? "oneOf"
      <*> o .:? "$defs"
      <*> o .:? "$ref"
    -- Apply $ref resolution automatically
    pure $ resolveSchemaRefs base
```

**Resolution logic:**

```haskell
resolveSchemaRefs :: EnrichedSchema -> EnrichedSchema
resolveSchemaRefs schema = case schemaRef schema of
  Just ref | Just defs <- schemaDefs schema ->
    -- 1. Look up the reference in $defs
    case lookupRef ref defs of
      Just resolved ->
        -- 2. Merge resolved schema with local description
        let merged = mergeSchema schema resolved
        -- 3. Continue resolving recursively
        in resolveSchemaRefs merged { schemaRef = Nothing, schemaDefs = schemaDefs schema }
      Nothing -> schema
  _ ->
    -- 4. Recursively resolve nested properties and oneOf
    -- IMPORTANT: Propagate $defs to child schemas so they can resolve their refs
    let defs = schemaDefs schema
    in schema
      { schemaProperties = resolvePropertiesRefs defs <$> schemaProperties schema
      , schemaOneOf = map (propagateDefsAndResolve defs) <$> schemaOneOf schema
      }
  where
    -- Propagate parent $defs to child schema before resolving
    propagateDefsAndResolve parentDefs child =
      let childWithDefs = case (parentDefs, schemaDefs child) of
            (Just pDefs, Nothing) -> child { schemaDefs = Just pDefs }
            _ -> child
      in resolveSchemaRefs childWithDefs
```

**Reference lookup:**

```haskell
lookupRef :: Text -> Map Text Value -> Maybe EnrichedSchema
lookupRef ref defs
  | T.isPrefixOf "#/$defs/" ref =
      let defName = T.drop 8 ref  -- Drop "#/$defs/"
      in case Map.lookup defName defs of
        Just val -> case fromJSON val of
          Success schema -> Just schema
          Error _ -> Nothing
        Nothing -> Nothing
  | otherwise = Nothing
```

**Description preservation:**

```haskell
mergeSchema :: EnrichedSchema -> EnrichedSchema -> EnrichedSchema
mergeSchema local resolved = resolved
  { schemaDescription = schemaDescription local <|> schemaDescription resolved
  }
```

The local description (e.g., "Cone identifier (provide either name or id)") takes precedence over the definition's generic description.

### Recursive Resolution

Resolution happens at three levels:

1. **Top-level schema refs** - Resolves `$ref` in `EnrichedSchema`
2. **Property refs** - Resolves `$ref` in `SchemaProperty`
3. **Nested structures** - Recursively processes:
   - `schemaProperties` (object fields)
   - `schemaOneOf` (enum variants)
   - `propProperties` (nested objects)
   - `propOneOf` (nested enums)
   - `propItems` (array element schemas)

Example traversal for `cone_chat`:

```
EnrichedSchema (top-level)
  ├─ schemaOneOf[0] (cone_chat method variant)
  │   └─ schemaProperties["params"]
  │       └─ propProperties["identifier"]
  │           ├─ propRef = "#/$defs/ConeIdentifier"  ← RESOLVE THIS
  │           ├─ propDescription = "Cone identifier..."
  │           └─ (after resolution)
  │               ├─ propOneOf[0] (by_name variant)
  │               └─ propOneOf[1] (by_id variant)
  └─ schemaDefs["ConeIdentifier"] (source for resolution)
```

## Resolution Process

### Step-by-Step Example

**Input schema:**
```json
{
  "properties": {
    "identifier": {
      "$ref": "#/$defs/ConeIdentifier",
      "description": "Cone identifier (provide either name or id)"
    }
  },
  "$defs": {
    "ConeIdentifier": {
      "description": "Identifier for a cone",
      "oneOf": [...]
    }
  }
}
```

**Resolution steps:**

1. **Parse JSON** → `EnrichedSchema` with `schemaDefs` and nested `propRef`
2. **Detect `$ref`** in `identifier` property
3. **Lookup** `"ConeIdentifier"` in `$defs` map
4. **Decode** definition as `SchemaProperty`
5. **Merge** local description with resolved structure
6. **Replace** `$ref` with resolved `oneOf` variants
7. **Continue** recursively for nested refs

**Output schema:**
```haskell
SchemaProperty
  { propDescription = Just "Cone identifier (provide either name or id)"
  , propOneOf = Just
      [ SchemaProperty { propProperties = ... by_name ... }
      , SchemaProperty { propProperties = ... by_id ... }
      ]
  , propRef = Nothing  -- Resolved!
  }
```

## Benefits

### 1. Complete Schema Information
- CLI sees full structure of enum types
- Help text can show all variants
- Schema introspection (`--schema`) returns resolved definitions

### 2. Automatic Processing
- No manual intervention required
- Happens during JSON parsing
- Cached schemas already resolved

### 3. Recursive Support
- Handles nested `$ref`s
- Works with complex structures
- Preserves `$defs` for child schemas

### 4. Description Preservation
- Local descriptions override generic ones
- Context-specific help text maintained
- Better UX than raw definitions

## Limitations

### 1. Only Supports `#/$defs/` References
- External refs (`http://...`) not supported
- Relative refs (`../schemas/...`) not supported
- Only internal document refs work

**Rationale:** Substrate only uses `$defs`, no need for external refs yet.

### 2. Circular References Not Handled
- Infinite recursion possible if schema has cycles
- No cycle detection implemented

**Mitigation:** Substrate schemas don't have cycles currently. If they did, we'd need:
```haskell
resolveSchemaRefsWithDepth :: Int -> EnrichedSchema -> EnrichedSchema
resolveSchemaRefsWithDepth maxDepth schema
  | maxDepth <= 0 = schema  -- Stop recursion
  | otherwise = ...
```

### 3. Resolution Happens at Parse Time
- Can't toggle resolution on/off
- Can't inspect unresolved schema
- Slightly slower parsing (minimal impact)

**Alternative:** Could make resolution explicit:
```haskell
-- Don't auto-resolve
cachedSchema <- loadSchema opts
-- Resolve on-demand
let resolved = resolveSchemaRefs cachedSchema
```

## Trade-offs

### vs `substrate.full_schema` Endpoint

| Aspect | `$ref` Resolution | `substrate.full_schema` |
|--------|-------------------|----------------------|
| **Implementation** | Client-side resolution | Server sends resolved |
| **Complexity** | Complex recursive logic | Simple parsing |
| **Schema format** | `oneOf` with method enum | Flat method list |
| **Return types** | Not included | Included (`returns` field) |
| **Compatibility** | Works with old substrate | Requires new endpoint |
| **Performance** | Small parsing overhead | No overhead |

### When to Use Which

**Use `$ref` resolution when:**
- Working with `substrate.activation_schema` (current format)
- Need backward compatibility
- Substrate might not send resolved schemas

**Use `substrate.full_schema` when:**
- Need return type information
- Want cleaner method structure
- Substrate supports it

**Use both when:**
- Migrating between formats
- Want fallback behavior
- Supporting multiple substrate versions

## Future Improvements

### 1. CLI Generation from Resolved Schemas

Currently we resolve but don't use the full structure. Could improve CLI:

```bash
# Current
synapse cone chat --identifier JSON

# With resolved oneOf
synapse cone chat --identifier-by-name TEXT
# OR
synapse cone chat --identifier-by-id UUID
```

Implementation would need to detect `oneOf` in parameter parsing:
```haskell
buildParamParser :: ParamSchema -> Parser (Text, Maybe Value)
buildParamParser param = case paramSchema param of
  -- Check for oneOf variants
  Just (Object o) | Just variants <- o ^? key "oneOf" ->
    -- Generate subcommand for each variant
    subparser $ mconcat (map variantCommand variants)
  _ -> -- Standard flag parsing
```

### 2. Cycle Detection

Add depth tracking to prevent infinite recursion:

```haskell
resolveSchemaRefs :: EnrichedSchema -> EnrichedSchema
resolveSchemaRefs = resolveWithDepth 10  -- Max 10 levels

resolveWithDepth :: Int -> EnrichedSchema -> EnrichedSchema
resolveWithDepth depth schema
  | depth <= 0 = schema  -- Stop if too deep
  | otherwise = case schemaRef schema of
      Just ref -> ...
        resolveWithDepth (depth - 1) merged
      Nothing -> ...
```

### 3. External Reference Support

If substrate starts using external refs:

```haskell
data RefType = InternalRef Text | ExternalRef Text

parseRef :: Text -> RefType
parseRef ref
  | T.isPrefixOf "#/" ref = InternalRef ref
  | T.isPrefixOf "http://" ref = ExternalRef ref
  | otherwise = InternalRef ref

resolveExternalRef :: Text -> IO (Maybe EnrichedSchema)
resolveExternalRef url = do
  -- Fetch schema from URL
  -- Cache externally fetched schemas
  -- Return parsed schema
```

### 4. Schema Validation

Validate resolved schemas match expected structure:

```haskell
validateResolvedSchema :: EnrichedSchema -> Either Text ()
validateResolvedSchema schema = do
  -- Check no $refs remain
  when (isJust (schemaRef schema)) $
    Left "Unresolved $ref found"
  -- Check $defs are valid
  -- Check oneOf variants are valid
  -- ...
```

## Testing

### Unit Tests Needed

1. **Basic ref resolution:**
   ```haskell
   test_resolve_simple_ref = do
     let schema = EnrichedSchema {
       schemaRef = Just "#/$defs/Foo",
       schemaDefs = Just (Map.singleton "Foo" fooDefinition)
     }
     let resolved = resolveSchemaRefs schema
     schemaRef resolved `shouldBe` Nothing
   ```

2. **Description preservation:**
   ```haskell
   test_preserve_local_description = do
     let schema = EnrichedSchema {
       schemaRef = Just "#/$defs/Type",
       schemaDescription = Just "Local desc",
       schemaDefs = Just (Map.singleton "Type" typeWithDesc)
     }
     let resolved = resolveSchemaRefs schema
     schemaDescription resolved `shouldBe` Just "Local desc"
   ```

3. **Nested resolution:**
   ```haskell
   test_resolve_nested_refs = do
     let schema = EnrichedSchema {
       schemaProperties = Just (Map.singleton "field" propWithRef)
     }
     let resolved = resolveSchemaRefs schema
     -- Check propRef is None in resolved.properties.field
   ```

4. **Missing ref:**
   ```haskell
   test_missing_ref_keeps_original = do
     let schema = EnrichedSchema {
       schemaRef = Just "#/$defs/Missing",
       schemaDefs = Just Map.empty
     }
     let resolved = resolveSchemaRefs schema
     schemaRef resolved `shouldBe` Just "#/$defs/Missing"  -- Unchanged
   ```

### Integration Tests

Test with real substrate schemas:

```bash
# Get cone schema from substrate
synapse --schema cone chat > cone_chat.json

# Verify $refs are resolved
jq '.params.properties.identifier | has("$ref")' cone_chat.json
# Should output: false

# Verify oneOf is present
jq '.params.properties.identifier | has("oneOf")' cone_chat.json
# Should output: true
```

## Related Documents

- [Schema $ref Resolution Issue](./16680871380507538687_schema-ref-resolution.md) - Original problem description
- [Full Schema Endpoint](./16680682640959994623_full-schema-endpoint.md) - Alternative approach via new RPC method
- [Dynamic CLI Type-Driven Schemas](./16680891033387373567_dynamic-cli-type-driven-schemas.md) - How schemas drive CLI generation

## Decision Log

**2025-12-18:** Implemented `$ref` resolution
- **Why:** Enables proper handling of `ConeIdentifier` and other enum types
- **Trade-off:** Added complexity vs waiting for `substrate.full_schema`
- **Mitigation:** Keep implementation contained, easy to remove if we switch to `substrate.full_schema`

**Future Decision Point:** When substrate supports `substrate.full_schema`
- Re-evaluate if `$ref` resolution still needed
- Consider deprecating in favor of cleaner endpoint
- Or keep both for backward compatibility

## Implementation Checklist

- [x] Add `$ref` and `$defs` fields to `EnrichedSchema`
- [x] Add `$ref` and `oneOf` fields to `SchemaProperty`
- [x] Implement `resolveSchemaRefs` function
- [x] Implement `lookupRef` helper
- [x] Implement `mergeSchema` to preserve descriptions
- [x] Add recursive resolution for properties
- [x] Add recursive resolution for oneOf variants
- [x] Update JSON parsers (FromJSON/ToJSON)
- [x] Build and verify no compilation errors
- [x] Fix $defs propagation to child schemas
- [x] Test with `cone chat` (has ConeIdentifier) - ✅ $ref resolved, oneOf present
- [x] Test with `cone get` (has ConeIdentifier) - ✅ $ref resolved
- [x] Test with `cone delete` (has ConeIdentifier) - ✅ $ref resolved
- [x] Test schema introspection (`--schema cone chat`) - ✅ Shows full resolved structure
- [x] Verify description preservation - ✅ Local descriptions preserved
- [x] Run existing tests - ✅ All tests pass (guidance-test, cache-invalidation-test)
- [ ] Add unit tests for resolution logic (future)
- [ ] Add integration tests with real schemas (future)
- [ ] Update CLI generation to use resolved oneOf (future)
- [ ] Document in user-facing help (future)
