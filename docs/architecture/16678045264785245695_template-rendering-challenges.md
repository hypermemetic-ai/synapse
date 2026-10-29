# Template Rendering: Current State & Challenges

## Summary

The cone rendering issue has been **resolved**. We fixed:
1. **IR type namespace collision** - types now properly qualified
2. **Nested struct rendering** - template generator no longer wraps structs incorrectly

## Completed Work

### 1. IR Type Namespace Qualification

**Problem**: Multiple plugins could define types with the same name (e.g., both `cone` and `claudecode` define `ListResult`). The IR stored types in a flat `Map Text TypeDef`, causing collisions.

**Solution**: All type names are now namespace-qualified:
- `ListResult` → `cone.ListResult` or `claudecode.ListResult`
- `ConeInfo` → `cone.ConeInfo`
- `Position` → `cone.Position`

**Files changed**:
- `src/Synapse/IR/Builder.hs` - All extraction functions now take `namespace` parameter and prefix type names

### 2. Template Generation Defaults

**Change**: `--generate-templates` now writes to `~/.config/synapse/templates/` by default instead of `.substrate/templates/`.

### 3. Nested Struct Rendering Fix

**Problem**: Template generator wrapped nested struct fields in `name=(...)` format:

```mustache
{{! Generated (broken) }}
{{#cones}}
  head=(node_id={{head.node_id}} tree_id={{head.tree_id}})
{{/cones}}
```

When mustache encountered `head=(...)`, it tried to render the literal text `head=` followed by the parenthesized content. But the parentheses caused confusion - mustache saw `head` as a variable to substitute, which resolved to the entire struct object, triggering Haskell's `Show` instance (`fromList [...]`).

**Root Cause Analysis**:

The bug was in `generateFieldRefInContext` (lines 412-423 of `src/Synapse/CLI/Template.hs`):

```haskell
-- BROKEN: Wraps struct fields in name=(...)
RefNamed typeName -> case Map.lookup typeName (irTypes ir) of
  Just TypeDef{tdKind = KindStruct fields} ->
    let displayFields = filter (not . isInternalField) fields
        nestedRefs = map (generateFieldRefInContext ir fullPath) displayFields
    in name <> "=(" <> T.intercalate " " nestedRefs <> ")"
```

The `name <> "=(" ... ")"` wrapper caused the issue. When mustache processed the template:
1. It saw `head=(...)` as literal text with embedded variables
2. But `head` appeared as a bare variable reference
3. Mustache resolved `head` to the JSON object `{"node_id":"...", "tree_id":"..."}`
4. Converting that object to string used Haskell's `Show` → `fromList [...]`

**Solution**: Don't wrap nested structs. Generate flat dot-notation references:

```haskell
-- FIXED: Flatten to dot notation without wrapper
RefNamed typeName -> case Map.lookup typeName (irTypes ir) of
  Just TypeDef{tdKind = KindStruct fields} ->
    let displayFields = filter (not . isInternalField) fields
        dotRefs = map (\f -> fdName f <> "={{" <> fullPath <> "." <> fdName f <> "}}") displayFields
    in T.intercalate " " dotRefs
```

**Generated template (fixed)**:
```mustache
{{#cones}}
  node_id={{head.node_id}} tree_id={{head.tree_id}} id={{id}} ...
{{/cones}}
```

**Key insight**: The mustache library's dot notation (`{{head.node_id}}`) works correctly. The problem was the template generator creating malformed templates where the struct field name appeared as a label that mustache tried to substitute.

## IR Architecture

The IR is produced via **hylomorphism** - fusing unfold (schema coalgebra) with fold (IR algebra):

```haskell
buildIR :: Path -> SynapseM IR
buildIR = walkSchema irAlgebra  -- hyloM irAlgebra schemaCoalgebra
```

This means:
- No intermediate tree is materialized
- Schema is fetched lazily as needed
- Types and methods are collected in a single traversal

## File Summary

| File | Changes |
|------|---------|
| `src/Synapse/IR/Builder.hs` | Namespace-qualify all type names |
| `src/Synapse/CLI/Template.hs` | Fix nested struct expansion - no wrapper |
| `app/Main.hs` | Default template output to `~/.config/synapse/templates/` |
| `plexus-synapse.cabal` | Added `filepath` dependency to executable |

### 4. Template Search Path Priority

**Problem**: After fixing the template generator, output still showed `head=fromList [...]`. The fix wasn't taking effect.

**Root Cause**: Local `.substrate/templates/` took precedence over global `~/.config/synapse/templates/`. Old templates in `.substrate/templates/cone/list.mustache` still had:

```mustache
{{! Old broken template }}
head={{head}}
```

While the newly generated templates in `~/.config/synapse/templates/` had the correct:

```mustache
{{! New fixed template }}
node_id={{head.node_id}} tree_id={{head.tree_id}}
```

**Solution**: Removed `.substrate/templates/cone/` directory. Templates should live in `~/.config/synapse/templates/` (user-specific, not repo-specific).

**Template search order** (renderer checks):
1. `.substrate/templates/<plugin>/<method>.mustache` (local override)
2. `~/.config/synapse/templates/<plugin>/<method>.mustache` (user default)

## Lessons Learned

1. **Template format matters**: Mustache templates must not have bare field names that could be interpreted as variables when you want them as labels

2. **Dot notation works**: The `mustache` library properly supports `{{a.b.c}}` path traversal. Issues are usually in template generation, not the library.

3. **Test with raw output**: Using `--raw` flag to see actual JSON helps distinguish between data issues and rendering issues

4. **Check template search paths**: When fixes don't take effect, verify which template file is actually being used. Local overrides can shadow global templates.
