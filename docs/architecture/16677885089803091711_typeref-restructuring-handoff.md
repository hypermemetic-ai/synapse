# TypeRef Restructuring: Handoff Document

**Status**: ⚠️ Implementation Complete, Testing Blocked
**Branch**: `feature/synapse-reimplementation`
**Date**: 2026-01-19
**IR Version**: 1.0 → 2.0 (breaking change)

## Executive Summary

We've completed a major restructuring of the IR (Intermediate Representation) to replace flat string-based type references (`RefNamed("cone.UUID")`) with structured qualified names (`RefNamed { qnNamespace: "cone", qnLocalName: "UUID" }`).

**Goal**: Eliminate string parsing throughout the codebase and fix malformed TypeScript imports.

**Status**:
- ✅ All code changes complete (Haskell + Rust)
- ✅ Both codebases compile cleanly
- ⚠️ **Blocked**: IR generation still outputs v1.0 format despite source showing v2.0
- ❌ End-to-end testing not yet performed

## Problem Statement

### Root Cause

TypeRef used concatenated strings like `"cone.UUID"` instead of structured data:

```haskell
-- Old (v1.0)
data TypeRef = RefNamed Text  -- "cone.UUID"

-- New (v2.0)
data TypeRef = RefNamed QualifiedName
data QualifiedName = QualifiedName
  { qnNamespace :: Text  -- "cone"
  , qnLocalName :: Text  -- "UUID"
  }
```

### Impact

This caused malformed TypeScript imports:
```typescript
// BEFORE (broken):
import type { Forge.ForgeRepoSummary } from '../../hyperforge/types';
//             ^^^^^^^^^^^^^^^^^^^^^ Invalid identifier with dot

// AFTER (fixed):
import type { ForgeRepoSummary } from '../../hyperforge/forge/types';
//             ^^^^^^^^^^^^^^^^ Clean local name
```

The string parsing was happening in 8+ locations across both codebases with `split_qualified_name()`, `rfind('.')`, etc.

## Implementation Completed

### Phase 1: Type Definitions

#### Phase 1.1: Haskell Types ✅
**File**: `/Users/user/dev/controlflow/hypermemetic/synapse/src/Synapse/IR/Types.hs`

Added:
```haskell
data QualifiedName = QualifiedName
  { qnNamespace :: Text
  , qnLocalName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

qualifiedNameFull :: QualifiedName -> Text
qualifiedNameFull QualifiedName{..}
  | T.null qnNamespace = qnLocalName
  | otherwise = qnNamespace <> "." <> qnLocalName
```

Updated:
```haskell
data TypeRef
  = RefNamed QualifiedName  -- Changed from: RefNamed Text
  | ...
```

#### Phase 1.2: Rust Types ✅
**File**: `/Users/user/dev/controlflow/hypermemetic/service-codegen/src/ir.rs`

Added:
```rust
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub struct QualifiedName {
    pub qn_namespace: String,
    pub qn_local_name: String,
}

impl QualifiedName {
    pub fn full_name(&self) -> String { ... }
    pub fn namespace(&self) -> Option<&str> { ... }
    pub fn local_name(&self) -> &str { ... }
}
```

Updated deserializer to expect:
```json
{
  "tag": "RefNamed",
  "contents": {
    "qnNamespace": "cone",
    "qnLocalName": "UUID"
  }
}
```

### Phase 2: IR Builders ✅

**File**: `/Users/user/dev/controlflow/hypermemetic/synapse/src/Synapse/IR/Builder.hs`

Changed all `RefNamed` construction sites (5 locations):
```haskell
-- BEFORE
RefNamed (namespace <> "." <> extractRefName ref)

-- AFTER
RefNamed QualifiedName
  { qnNamespace = namespace
  , qnLocalName = extractRefName ref
  }
```

Added helper for deduplication:
```haskell
parseQualifiedName :: Text -> Maybe QualifiedName
parseQualifiedName t =
  case T.breakOnEnd "." t of
    ("", _) -> Nothing
    (ns, local) | T.null local -> Nothing
                | otherwise -> Just QualifiedName
                    { qnNamespace = T.dropEnd 1 ns
                    , qnLocalName = local
                    }
```

### Phase 3: TypeScript Generators ✅

**Files**:
- `/Users/user/dev/controlflow/hypermemetic/service-codegen/src/generator/types.rs`
- `/Users/user/dev/controlflow/hypermemetic/service-codegen/src/generator/namespaces.rs`

Replaced all string parsing with structured access:
```rust
// BEFORE
TypeRef::RefNamed(name) => {
    if let Some(last_dot) = name.rfind('.') {
        let namespace = &name[..last_dot];
        let local_name = &name[last_dot + 1..];
        // ... string manipulation
    }
}

// AFTER
TypeRef::RefNamed(qn) => {
    if let Some(ns) = qn.namespace() {
        if ns != current_namespace {
            imports.entry(ns.to_string())
                .or_default()
                .push(to_pascal(qn.local_name()));
        }
    }
}
```

**Deleted**: `split_qualified_name()` function (no longer needed)

### Phase 4: CLI Parser ✅

**Files**:
- `/Users/user/dev/controlflow/hypermemetic/synapse/src/Synapse/CLI/Support.hs`
- `/Users/user/dev/controlflow/hypermemetic/synapse/src/Synapse/CLI/Parse.hs`
- `/Users/user/dev/controlflow/hypermemetic/synapse/src/Synapse/CLI/Help.hs`
- `/Users/user/dev/controlflow/hypermemetic/synapse/src/Synapse/CLI/Template.hs`
- `/Users/user/dev/controlflow/hypermemetic/synapse/test/IRSpec.hs`

Updated all pattern matches to convert `QualifiedName` to full name for Map lookups:
```haskell
-- BEFORE
RefNamed name -> Map.lookup name (irTypes ir)

-- AFTER
RefNamed qn -> Map.lookup (qualifiedNameFull qn) (irTypes ir)
```

Fixed 15+ compilation errors across 5 files.

### Phase 5: Version Bump ✅

**Changed**:
```haskell
-- synapse/src/Synapse/IR/Types.hs
emptyIR :: IR
emptyIR = IR
  { irVersion = "2.0"  -- Bumped from "1.0"
  , ...
  }
```

**Added version check**:
```rust
// service-codegen/src/generator/mod.rs
pub fn generate(ir: &IR) -> Result<GenerationResult> {
    if ir.ir_version != "2.0" {
        anyhow::bail!(
            "Unsupported IR version: {}. Expected 2.0.\n\
             This version of service-codegen requires IR v2.0 with structured TypeRef.\n\
             Please regenerate IR with latest Synapse.",
            ir.ir_version
        );
    }
    // ...
}
```

## Current Issues

### Issue 1: IR Version Not Updating ⚠️

**Problem**: Despite source code showing `irVersion = "2.0"`, generated IR outputs `"irVersion": "1.0"`

**Evidence**:
```bash
$ grep "irVersion" src/Synapse/IR/Types.hs
  { irVersion = "2.0"  -- Bumped: TypeRef now uses structured QualifiedName

$ cabal run synapse -- --emit-ir plexus | jq -r '.irVersion'
1.0  # ← Should be 2.0!
```

**Attempted Fixes**:
- ✅ Confirmed source file is correct
- ✅ `cabal clean` and rebuild
- ✅ Deleted `dist-newstyle/` and rebuilt
- ✅ Used explicit path to built executable
- ❌ Still outputs v1.0

**Hypotheses**:
1. **Cached IR**: Plexus might be returning cached IR instead of generating fresh
2. **Multiple definitions**: Another `emptyIR` definition somewhere
3. **Build artifact issue**: Old `.o` files being used despite clean
4. **Runtime IR loading**: IR being deserialized from a file instead of generated

**Next Debug Steps**:
```bash
# 1. Check for multiple irVersion definitions
grep -rn "irVersion" src/ | grep "="

# 2. Check if Plexus has IR caching
grep -r "cache" # in plexus/substrate code

# 3. Test with debug output
# Add traceShow to emptyIR to confirm it's being called

# 4. Try different Plexus instance
# Stop substrate, restart, retry
```

### Issue 2: Untested End-to-End Flow ❌

**What We Haven't Verified**:
- [ ] IR generation produces structured `QualifiedName` format
- [ ] service-codegen can deserialize the new format
- [ ] TypeScript generation works with structured refs
- [ ] Cross-namespace imports are fixed
- [ ] TypeScript compilation succeeds with fewer errors
- [ ] CLI parsing still works with real schemas

**Blocker**: Can't test until Issue 1 is resolved

## Testing Strategy

### Diagnostic Phase

#### Step 1: Verify Source Code
```bash
# Confirm source file
cat src/Synapse/IR/Types.hs | grep -A 5 "emptyIR = IR"

# Check for duplicates
grep -rn "irVersion.*=" src/

# Look for caching
grep -rn "cache" src/ | grep -i "ir"
```

#### Step 2: Build Verification
```bash
# Fresh build
cabal clean
rm -rf dist-newstyle
cabal build synapse

# Find executable
synapse_exe=$(find dist-newstyle -name synapse -type f -executable)
echo "Built: $synapse_exe"

# Test it directly (not through cabal run)
$synapse_exe --help
```

#### Step 3: IR Format Inspection
```bash
# Generate IR to file
$synapse_exe --emit-ir plexus > /tmp/ir-test.json 2>&1

# Check version
jq -r '.irVersion' /tmp/ir-test.json

# Check ONE type reference
jq '.irMethods["cone.chat"].mdReturns' /tmp/ir-test.json
```

**Expected (v2.0)**:
```json
{
  "tag": "RefNamed",
  "contents": {
    "qnNamespace": "cone",
    "qnLocalName": "ChatEvent"
  }
}
```

**If v1.0**:
```json
{
  "tag": "RefNamed",
  "contents": "cone.ChatEvent"
}
```

### Testing Phase (Once IR is v2.0)

#### Step 4: Deserialization Test
```bash
cd service-codegen
cargo run --release -- /tmp/ir-test.json --dry-run 2>&1 | head -20
```

**Expected**:
- If v2.0: Succeeds, shows file list
- If v1.0: Fails with version error message

#### Step 5: TypeScript Generation
```bash
cargo run --release -- /tmp/ir-test.json -o /tmp/ts-test

# Count generated files
find /tmp/ts-test -name "*.ts" | wc -l

# Check imports in one namespace
cat /tmp/ts-test/hyperforge/forge/types.ts | grep "^import"
```

**Expected Imports**:
```typescript
import type { Forge } from '../../hyperforge/org/types';
import type { ForgeRepoSummary, TokenStatus } from './types';
```

**NOT**:
```typescript
import type { Forge.ForgeRepoSummary } from '../../hyperforge/types';
```

#### Step 6: TypeScript Compilation
```bash
cd /tmp/ts-test
npm install
npm run typecheck 2>&1 | tee /tmp/typecheck-results.txt

# Count errors
grep "error TS" /tmp/typecheck-results.txt | wc -l
```

**Baseline**: 1146 errors (before fix)
**Target**: <50 errors (after fix)

#### Step 7: CLI Parsing Test
```bash
# Test complex nested parameter
synapse cone chat \
  --identifier.type by_name \
  --identifier.name test-cone \
  --prompt "hello"
```

**Expected**: Successfully parses and calls method

### Integration Test

```bash
# Full pipeline
cd synapse
cabal build synapse

# Generate IR
./dist-newstyle/.../synapse --emit-ir plexus > /tmp/ir-final.json

# Verify v2.0
jq -r '.irVersion' /tmp/ir-final.json  # Should be "2.0"

# Generate TypeScript
cd ../service-codegen
cargo run --release -- /tmp/ir-final.json -o /tmp/client-final

# Install and test
cd /tmp/client-final
npm install
npm run typecheck

# Run smoke test
npm test
```

## File Manifest

### Modified Files (Haskell)
```
synapse/src/Synapse/IR/Types.hs          - QualifiedName type, version bump
synapse/src/Synapse/IR/Builder.hs        - Construct QualifiedName objects
synapse/src/Synapse/CLI/Support.hs       - Convert to full names for Map lookup
synapse/src/Synapse/CLI/Parse.hs         - Convert to full names for Map lookup
synapse/src/Synapse/CLI/Help.hs          - Convert to full names for rendering
synapse/src/Synapse/CLI/Template.hs      - Convert to full names for Map lookup
synapse/test/IRSpec.hs                   - Update test helpers
```

### Modified Files (Rust)
```
service-codegen/src/ir.rs                    - QualifiedName struct, TypeRef update
service-codegen/src/generator/types.rs       - Use structured access, fix imports
service-codegen/src/generator/namespaces.rs  - Use structured access
service-codegen/src/generator/mod.rs         - Version check
```

### Build Status
```
✅ Synapse: Compiles cleanly (cabal build synapse)
✅ service-codegen: Compiles cleanly (cargo build --release)
✅ Zero compilation errors
✅ 7 minor warnings in Rust (unused code, pre-existing)
```

## Rollback Procedure

If this needs to be reverted:

```bash
cd synapse
git checkout HEAD -- src/Synapse/IR/Types.hs
git checkout HEAD -- src/Synapse/IR/Builder.hs
git checkout HEAD -- src/Synapse/CLI/Support.hs
git checkout HEAD -- src/Synapse/CLI/Parse.hs
git checkout HEAD -- src/Synapse/CLI/Help.hs
git checkout HEAD -- src/Synapse/CLI/Template.hs
git checkout HEAD -- test/IRSpec.hs

cd ../service-codegen
git checkout HEAD -- src/ir.rs
git checkout HEAD -- src/generator/

# Rebuild both
cd ../synapse && cabal clean && cabal build
cd ../service-codegen && cargo clean && cargo build --release
```

## Next Steps

### Immediate (Unblock Testing)

1. **Debug IR version issue**:
   - Add debug tracing to `emptyIR` construction
   - Check if Plexus/Substrate caches IR
   - Look for alternate IR sources in Substrate
   - Test with fresh Substrate instance

2. **Verify IR generation**:
   - Confirm structured `QualifiedName` format
   - Check multiple type references
   - Verify both empty and non-empty namespaces

### Short-term (Complete Testing)

3. **Run integration tests**:
   - Test IR → TypeScript pipeline
   - Verify import generation
   - Confirm TypeScript compilation
   - Test CLI parameter parsing

4. **Performance check**:
   - Compare IR generation time (v1.0 vs v2.0)
   - Compare TypeScript generation time
   - Ensure no regressions

### Long-term (Cleanup)

5. **Documentation**:
   - Update README with v2.0 changes
   - Document migration path
   - Add examples of new IR format

6. **Consider other improvements** (from analysis):
   - Empty namespace consistency (Issue #2)
   - Type metadata (format, descriptions) (Issue #4)
   - Source location tracking (Issue #5)
   - Nullable semantics clarification (Issue #6)

## Related Documents

- **IR Analysis**: See agent output from subagent a414752 for comprehensive analysis of 8 IR format issues
- **Implementation Plan**: See agent output from subagent a2fc8d1 for detailed implementation plan
- **Namespace Collision**: `16677936867500161535_typescript-namespace-collision.md` (original issue)

## Key Insights

### What Went Well

1. **Structured approach**: Breaking into phases made the refactoring manageable
2. **Type safety**: Compiler caught all the places needing updates
3. **Clean separation**: TypeRef changes didn't affect other parts of the codebase
4. **No data migration**: Only format change, not data structure

### What Was Challenging

1. **Multiple consumers**: IR is used by both CLI and codegen, requiring updates to both
2. **Map key compatibility**: `irTypes` map still uses `Text` keys, requiring conversion
3. **JSON serialization**: Had to ensure Aeson/serde serialize to compatible formats
4. **Build caching**: Mysterious issue with IR version not updating

### Lessons Learned

1. **Test incrementally**: Should have tested each phase before moving to next
2. **Version early**: Should have bumped version in Phase 1, not Phase 5
3. **Debug output**: Should have added tracing to confirm IR construction path
4. **Integration first**: Should have done a minimal end-to-end test before full implementation

## Contact

Questions about this work? Check:
- Git history for detailed changes
- Agent transcripts (IDs listed in Related Documents)
- Architecture docs in `docs/architecture/`
