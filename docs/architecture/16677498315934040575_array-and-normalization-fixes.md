# Array Handling and Path Normalization Fixes

**Date**: 2026-01-24
**Status**: Implemented & Tested
**Affects**:
- `src/Synapse/CLI/Parse.hs` - Array handling
- `src/Synapse/CLI/Support.hs` - Array representability
- `app/Main.hs` - Path normalization

## Problems Fixed

### 1. Array Parameter Handling

Arrays were incorrectly marked as non-CLI-representable. The infrastructure for repeated flags existed (`groupByPrefix` accumulating with `Map.insertWith (++)`), but the `RefArray` case only grabbed the first value.

**Before**:
```haskell
RefArray _ ->
  case lookup "" kvs of
    Just val -> parseJsonValue val  -- Only first value!
    Nothing -> Left $ InvalidValue (pdName param) "arrays need JSON input"
```

**After**:
```haskell
RefArray innerType ->
  let values = [val | ("", val) <- kvs]
  in if null values
     then Left $ InvalidValue (pdName param) "array requires at least one value"
     else Right $ Array $ V.fromList $ map (buildTypedValue ir innerType) values
```

### 2. Hyphen-to-Underscore Normalization

Method names with hyphens (e.g., `queue-add`) weren't being normalized to underscores, causing "Method not found" errors. Parameter names were normalized but path segments weren't.

**Before**:
```haskell
-- Regular path segment
| otherwise =
    let segments = filter (not . T.null) $ T.splitOn "." x
    in go (reverse segments ++ path) params helpReq xs
```

**After**:
```haskell
-- Regular path segment - normalize hyphens to underscores
| otherwise =
    let segments = map (T.replace "-" "_") $ filter (not . T.null) $ T.splitOn "." x
    in go (reverse segments ++ path) params helpReq xs
```

## Usage Examples

### Arrays with Repeated Flags

```bash
# String array
synapse changelog queue_add \
  --description "Fix bug" \
  --tags backend --tags critical --tags urgent

# Result: {"description": "Fix bug", "tags": ["backend", "critical", "urgent"]}

# Integer array
synapse some-command --ids 1 --ids 2 --ids 3
# Result: {"ids": [1, 2, 3]}
```

### Hyphenated Method Names

```bash
# All of these now work:
synapse changelog queue-add --help
synapse plexus.cone.queue-add --name test
synapse arbor tree-create --tree-id abc
synapse cone queue-list --max-count 10

# Path normalization:
# queue-add    → queue_add
# tree-create  → tree_create
# max-count    → max_count
```

### Parameter Normalization

```bash
# Parameters with hyphens work naturally
synapse cone create \
  --working-dir /tmp \
  --dry-run \
  --max-count 5

# Normalized to:
# working_dir, dry_run, max_count
```

## Test Results

### 1. Path Normalization Tests (12/12 passed)

```
parsePathAndParams - hyphen normalization
  ✓ normalizes hyphens in simple path segments
  ✓ normalizes hyphens in dot-notation paths
  ✓ normalizes hyphens in parameter names
  ✓ handles mixed hyphens in path and params
  ✓ normalizes complex dot-notation with hyphens
  ✓ handles underscore paths unchanged
  ✓ normalizes nested parameter paths
  ✓ normalizes _self template commands
  ✓ handles repeated array parameters with hyphens

parsePathAndParams - edge cases
  ✓ handles empty input
  ✓ preserves hyphens in parameter values
  ✓ handles boolean flags with hyphens
```

### 2. Array Parsing Tests (5/5 passed)

```
groupByPrefix
  ✓ groups dotted keys by prefix
  ✓ collects repeated keys for arrays

buildParamValue with arrays
  ✓ builds string array from repeated values
  ✓ builds integer array from repeated values
  ✓ fails when array has no values
```

### 3. CLI Integration Tests (15/15 passed)

All existing CLI tests continue to pass, confirming backward compatibility.

## Technical Details

### Normalization Scope

**Path segments** (normalized):
- Method names: `queue-add` → `queue_add`
- Namespace paths: `plexus.cone.chat-async` → `["plexus", "cone", "chat_async"]`
- Dot-notation: `cone.queue-add` splits and normalizes to `["cone", "queue_add"]`

**Parameter keys** (normalized):
- Flag names: `--working-dir` → `working_dir`
- Nested params: `--identifier.type` → `identifier.type` (dot preserved for IR parsing)

**Parameter values** (NOT normalized):
- User data preserved: `--name "my-test-cone"` → value stays `"my-test-cone"`
- UUIDs preserved: `--tree-id "abc-123"` → value stays `"abc-123"`

### Array Type Checking

Arrays are now CLI-representable if their element type is representable:

```haskell
-- Before
RefArray _ -> Left ReasonArray

-- After
RefArray innerType -> canCLIRepresentWithReason ir innerType
```

This means:
- `string[]` → Representable via repeated flags
- `integer[]` → Representable via repeated flags
- `ComplexObject[]` → Not representable (would need JSON)

## Files Changed

1. **src/Synapse/CLI/Parse.hs**
   - Added `Data.Vector` import
   - Fixed `RefArray` case to collect all values
   - Updated module documentation

2. **src/Synapse/CLI/Support.hs**
   - Updated representability rules for arrays
   - Fixed depth checking for arrays

3. **app/Main.hs:332**
   - Added hyphen-to-underscore normalization for path segments

4. **test/ParseSpec.hs** (new)
   - Unit tests for array parsing

5. **test/PathNormalizationSpec.hs** (new)
   - Comprehensive tests for path/param normalization

6. **plexus-synapse.cabal**
   - Added two new test suites

7. **docs/architecture/16680606527890548735_schema-to-cli-pipeline.md**
   - Updated type correspondence table

## Impact

- **Breaking**: None - these are bug fixes that add expected functionality
- **CLI UX**: Significantly improved - natural hyphenated names work
- **Arrays**: Can now use repeated flags instead of JSON
- **Method support levels**: More methods show `FullSupport`
- **Backward compatibility**: All existing tests pass

## Related Issues

The original error that prompted this fix:
```
synapse changelog queue-add --help
Error: Method not found
```

This was caused by CLI sending `changelog.queue-add` but backend expecting `changelog.queue_add`.
