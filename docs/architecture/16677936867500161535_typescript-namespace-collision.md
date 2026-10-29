# TypeScript Namespace Collision Issue

## Current Status

**Branch**: `fix/typescript-codegen-issues`

**Progress**: **266 → 62 TypeScript errors** (77% reduction)

### ✅ Completed
1. **Global types.ts cleanup** - Only exports core transport types (commit 1e5f7a6)
2. **Namespace-scoped stub generation** - Missing types get stubs in their namespace files (commit b94f059)
3. **Synapse namespace extraction** - Uses full path instead of final segment (commit 5d3101e)
4. **Service-codegen nested directory support** - Converts dots to slashes, calculates relative imports (commit e9f2313)

### 🚧 In Progress
- **Types.ts generation for nested namespaces** - Some types.ts files not generating for deeply nested namespaces

### 📋 Remaining Issues (62 errors)
1. **Missing types.ts files** (~54 errors) - Debug why not all namespace types.ts files generate
2. **Reserved keywords** (~6 errors) - Parameter named `protected` needs escaping
3. **Test exports** (~2 errors) - Update smoke test to use new namespace structure

## Problem 1: Namespace Collision (54 errors)

### Manifestation

TypeScript reports duplicate method implementations in generated client files:

```typescript
// repos/client.ts
export interface ReposClient {
  diff(refresh?: boolean): AsyncGenerator<RepoEvent>;  // From hyperforge.workspace.repos.diff
  diff(refresh?: boolean): AsyncGenerator<RepoEvent>;  // From hyperforge.org.hypermemetic.repos.diff
  diff(path: string, refresh?: boolean): AsyncGenerator<RepoEvent>; // From hyperforge.org.juggernautlabs.repos.diff
  // TypeScript error: Duplicate function implementation
}
```

### Root Cause

The IR builder extracts only the **final segment** as the namespace, causing methods from different nested paths to collide.

**Example from IR**:
```json
{
  "hyperforge.workspace.repos.diff": {
    "mdNamespace": "repos",  // ❌ Should be "hyperforge.workspace.repos"
    "mdName": "diff",
    "mdFullPath": "hyperforge.workspace.repos.diff"
  }
}
```

**Location**: `synapse/src/Synapse/IR/Builder.hs`

```haskell
-- Current behavior (line ~215-230)
extractFromPlugin :: Text -> Text -> PluginSchema -> (Map Text TypeDef, Map Text MethodDef)
extractFromPlugin namespace pathPrefix schema =
  let methods = psMethods schema
      results = map (extractMethodDef namespace pathPrefix) methods
      -- 'namespace' is just the final segment (e.g., "repos")
      -- but methods may come from "hyperforge.workspace.repos", "hyperforge.org.hypermemetic.repos", etc.
```

The walker passes only the final path segment as the namespace:

```haskell
-- Line ~60-70
walkAlgebra = Algebra
  { ...
  , walkPlugin = \path plugin -> do
      let namespace = pathToText path  -- e.g., "repos" from ["hyperforge", "workspace", "repos"]
          pathPrefix = pathToText path  -- Same value
      extractFromPlugin namespace pathPrefix plugin
  }
```

### Why This Happens

The schema walker navigates a tree like:

```
hyperforge/
  workspace/
    repos/
      - diff()
      - list()
  org/
    hypermemetic/
      repos/
        - diff()
        - list()
    juggernautlabs/
      repos/
        - diff()
        - list()
```

Each `repos` plugin is a **separate instance** at different tree locations, but they all report `namespace = "repos"` because only the final segment is used.

### Impact

1. **TypeScript compilation fails** - duplicate method signatures in interface
2. **Service-codegen groups unrelated methods** - all "repos" methods go to same file
3. **Name collisions** - can't distinguish `hyperforge.workspace.repos.diff` from `hyperforge.org.hypermemetic.repos.diff`

## Solution Options

### Option A: Use Full Path as Namespace (Recommended)

Change namespace extraction to use the **full path** instead of just the final segment.

**Before**:
```haskell
let namespace = pathToText path  -- "repos"
```

**After**:
```haskell
let namespace = T.intercalate "." (map pathToText path)  -- "hyperforge.workspace.repos"
```

**Result**:
```json
{
  "hyperforge.workspace.repos.diff": {
    "mdNamespace": "hyperforge.workspace.repos",  // ✅ Full path
    "mdName": "diff",
    "mdFullPath": "hyperforge.workspace.repos.diff"
  }
}
```

**TypeScript output**:
```
hyperforge/
  workspace/
    repos/
      types.ts
      client.ts    // Contains only hyperforge.workspace.repos methods
      index.ts
```

**Pros**:
- Solves collision completely
- Proper namespacing matches directory structure
- Clear which plugin instance methods belong to

**Cons**:
- Breaks existing clients expecting flat namespace structure
- More verbose imports: `import { ReposClient } from './hyperforge/workspace/repos'`

### Option B: Deduplicate Methods by Full Path

Keep namespace as final segment but deduplicate methods in codegen based on full path.

```rust
// service-codegen/src/generator/namespaces.rs
pub fn generate_namespaces(ir: &IR) -> HashMap<String, String> {
    // Group by namespace, but track full paths
    let mut methods_by_ns: HashMap<String, Vec<&MethodDef>> = HashMap::new();
    for method in ir.ir_methods.values() {
        // Only include if mdFullPath matches mdNamespace pattern
        if method.md_full_path.ends_with(&format!(".{}.{}", method.md_namespace, method.md_name)) {
            methods_by_ns.entry(method.md_namespace.clone())
                .or_default()
                .push(method);
        }
    }
}
```

**Pros**:
- Preserves flat namespace structure
- Less disruptive to existing code

**Cons**:
- Doesn't solve the conceptual issue
- Still unclear which "repos" plugin is which
- Complex filtering logic

### Option C: Hybrid - Namespace Hierarchy in TypeScript

Use full paths as namespaces but generate nested TypeScript module structure:

```typescript
// index.ts
export * as Hyperforge from './hyperforge';

// hyperforge/index.ts
export * as Workspace from './workspace';
export * as Org from './org';

// hyperforge/workspace/index.ts
export * as Repos from './repos';
```

**Usage**:
```typescript
import { Hyperforge } from '@plexus/client';
await Hyperforge.Workspace.Repos.diff();
```

**Pros**:
- Natural hierarchical API
- Clear which plugin instance
- Matches plexus schema tree structure

**Cons**:
- Most complex to implement
- Deeply nested imports

## Recommendation

**Implement Option A** for correctness, then consider Option C for developer experience.

**Immediate**: Use full path as namespace to fix collisions
**Future**: Add convenience exports for common patterns

## Implementation (Option A) ✅

### Synapse Changes (commit 5d3101e)

Updated `synapse/src/Synapse/IR/Builder.hs`:

```haskell
irAlgebra :: SchemaF IR -> SynapseM IR
irAlgebra (PluginF schema path childIRs) = do
  -- Use full path as namespace to avoid collisions
  -- e.g., "hyperforge.workspace.repos" instead of just "repos"
  let namespace = T.intercalate "." path  -- ✅ Full path
      pathPrefix = namespace

  let (localTypes, localMethods) = extractFromPlugin namespace pathPrefix schema
  -- ... rest of implementation
```

**Result**: IR now has proper namespaces:
```json
{
  "hyperforge.workspace.repos.diff": {
    "mdNamespace": "hyperforge.workspace.repos",  // ✅ Full path
    "mdFullPath": "hyperforge.workspace.repos.diff"
  },
  "hyperforge.org.hypermemetic.repos.diff": {
    "mdNamespace": "hyperforge.org.hypermemetic.repos",  // ✅ Different namespace
    "mdFullPath": "hyperforge.org.hypermemetic.repos.diff"
  }
}
```

### Service-codegen Changes (commit e9f2313)

**1. File path generation** - Convert dots to slashes:
```rust
// generator/types.rs & generator/namespaces.rs
let path = namespace.replace('.', "/");
files.insert(format!("{}/types.ts", path), content);
// "hyperforge.workspace.repos" → "hyperforge/workspace/repos/types.ts"
```

**2. Relative imports** - Calculate based on depth:
```rust
fn calculate_relative_import_path(from_namespace: &str, to_namespace: &str) -> String {
    let from_depth = from_namespace.matches('.').count();
    let ups = "../".repeat(from_depth + 1);
    let to_path = to_namespace.replace('.', "/");
    format!("{}{}", ups, to_path)
}
// From "hyperforge.workspace.repos" to "io" → "../../../io"
```

**3. PascalCase conversion** - Handle dots:
```rust
fn to_pascal(s: &str) -> String {
    // ...
    for c in s.chars() {
        if c == '_' || c == '-' || c == '.' {  // ✅ Treat dots as word boundaries
            capitalize = true;
        }
        // ...
    }
}
// "hyperforge.workspace.repos" → "HyperforgeWorkspaceRepos"
```

**4. Index exports** - Use nested paths:
```typescript
// index.ts (generated)
export * as HyperforgeWorkspaceRepos from './hyperforge/workspace/repos';
export * as HyperforgeOrgHypermemeticRepos from './hyperforge/org/hypermemetic/repos';
```

### Generated Structure

```
hyperforge/
  workspace/
    repos/
      types.ts     // RepoEvent, DiffStatus, etc.
      client.ts    // ReposClient with diff(), list(), etc.
      index.ts     // Re-exports types + client
  org/
    hypermemetic/
      repos/
        types.ts
        client.ts
        index.ts
    juggernautlabs/
      repos/
        types.ts
        client.ts
        index.ts
```

### Known Issues

**Types.ts generation incomplete**: Not all nested namespace types.ts files are being generated. Client files generate correctly, but some types.ts files are missing. Needs further debugging to identify why the type collection/filtering is incomplete for deeply nested namespaces.

## Problem 2: Reserved Keyword Parameter Names (6 errors)

TypeScript fails when parameters use JavaScript reserved words:

```typescript
// repos/client.ts line 199
update(path: string, protected: boolean): AsyncGenerator<RepoEvent>
//                   ^^^^^^^^^ Error: 'protected' is a reserved word
```

### Solution

Escape reserved keywords in parameter names:

```rust
// service-codegen/src/generator/namespaces.rs
fn to_camel(s: &str) -> String {
    let camel = pascal_to_camel(to_pascal(s));

    // Escape JavaScript reserved words
    if is_reserved_word(&camel) {
        format!("{}_", camel)  // protected → protected_
    } else {
        camel
    }
}

fn is_reserved_word(s: &str) -> bool {
    matches!(s,
        "break" | "case" | "catch" | "class" | "const" | "continue" |
        "debugger" | "default" | "delete" | "do" | "else" | "enum" |
        "export" | "extends" | "false" | "finally" | "for" | "function" |
        "if" | "import" | "in" | "instanceof" | "new" | "null" |
        "return" | "super" | "switch" | "this" | "throw" | "true" |
        "try" | "typeof" | "var" | "void" | "while" | "with" | "yield" |
        "let" | "static" | "protected" | "public" | "private" | "package" |
        "interface" | "implements" | "async" | "await"
    )
}
```

## Problem 3: Test Export Mismatch (2 errors)

Generated test expects convenience exports that don't exist:

```typescript
// test/smoke.test.ts
import { createClient, createHealthClient, createEchoClient } from '../index';
//                      ^^^^^^^^^^^^^^^^^^  ^^^^^^^^^^^^^^^^ Not exported
```

### Solution

Update test to use namespace imports:

```typescript
import { createClient, Health, Echo } from '../index';

async function main() {
  const rpc = createClient({ url: 'ws://localhost:4444' });

  // Test health.check
  const health = Health.createHealthClient(rpc);
  const status = await health.check();

  // Test echo.once
  const echo = Echo.createEchoClient(rpc);
  const result = await echo.once('test');
}
```

Or generate top-level convenience exports:

```typescript
// index.ts (generated)
export { createHealthClient } from './health';
export { createEchoClient } from './echo';
```

## Implementation Progress

### ✅ Phase 1: Fix Namespace Collision (Completed)

1. ✅ Updated Synapse IR builder to use full path as namespace (commit 5d3101e)
2. ✅ Verified IR output has correct namespaces
   ```bash
   jq '.irMethods."hyperforge.workspace.repos.diff".mdNamespace' /tmp/ir-fixed.json
   # Output: "hyperforge.workspace.repos" ✓
   ```
3. ✅ Updated service-codegen to handle dotted namespaces (commit e9f2313)
   - Convert dots to slashes for directory paths
   - Calculate relative imports based on nesting depth
   - Update PascalCase conversion to handle dots
4. 🚧 TypeScript generation partially working
   - Client files generate correctly
   - Some types.ts files missing for nested namespaces

### 🔄 Phase 2: Fix Reserved Keywords (Pending)

1. ⏳ Add `is_reserved_word()` function to service-codegen
2. ⏳ Update `to_camel()` to escape reserved words
3. ⏳ Test with repos methods

### 🔄 Phase 3: Fix Test Exports (Pending)

1. ⏳ Update smoke test to use namespace imports
2. ⏳ Optionally generate convenience exports

### 🔄 Phase 4: Debug Types Generation (In Progress)

1. 🚧 Investigate why types.ts files aren't generated for all namespaces
2. ⏳ Fix type collection/filtering for deeply nested namespaces
3. ⏳ Verify all types.ts files generate correctly

### 🔄 Phase 5: Documentation (Pending)

1. ⏳ Update README with new import patterns
2. ⏳ Document namespace hierarchy
3. ⏳ Add migration guide for existing clients

## Testing Strategy

### Current Test Results

```bash
# 1. ✅ Rebuild Synapse with namespace fix
cabal build synapse
# Build successful

# 2. ✅ Generate new IR with full namespaces
cabal run synapse -- --emit-ir plexus > /tmp/ir-fixed.json
# IR generated successfully

# 3. ✅ Verify namespaces are full paths
jq '.irMethods."hyperforge.workspace.repos.diff".mdNamespace' /tmp/ir-fixed.json
# Output: "hyperforge.workspace.repos" ✓

jq '.irPlugins | keys | .[] | select(contains("repos"))' /tmp/ir-fixed.json
# Output:
#   "hyperforge.org.hypermemetic.repos"
#   "hyperforge.org.juggernautlabs.repos"
#   "hyperforge.workspace.repos"
# All unique ✓

# 4. 🚧 Regenerate TypeScript (partial success)
cd service-codegen && cargo run --release -- /tmp/ir-fixed.json -o /tmp/codegen-test

# Generated structure (example):
#   hyperforge/workspace/repos/client.ts ✓
#   hyperforge/workspace/repos/index.ts ✓
#   hyperforge/workspace/repos/types.ts ✗ (missing)

# 5. ⏳ Type check (blocked by missing types.ts files)
cd /tmp/codegen-test && npm run typecheck
# 62 errors (down from 266)
# - ~54 errors: missing types.ts files
# - ~6 errors: reserved keyword 'protected'
# - ~2 errors: smoke test exports
```

### Next Steps

1. **Debug types.ts generation**: Investigate why `generate_namespace_types()` doesn't create files for all namespaces
2. **Test namespace isolation**: Verify no method collisions after fix
3. **Integration test**: Full round-trip from schema → IR → TypeScript → npm typecheck

## Related Issues

- Type deduplication (resolved) - [`16678100000000000000_schema-type-deduplication.md`](./16678100000000000000_schema-type-deduplication.md)
- Namespace refactoring (resolved) - [`16678110000000000000_namespace-refactoring.md`](./16678110000000000000_namespace-refactoring.md)
- Stub generation fix (resolved) - [`16678090000000000000_ts-stub-generation-fix.md`](./16678090000000000000_ts-stub-generation-fix.md)

## Open Questions

1. **Breaking change**: Should we maintain backward compatibility with flat namespace structure?
2. **Convenience exports**: Do we want `createHealthClient()` at top level or only `Health.createHealthClient()`?
3. **Directory depth**: Should generated structure match full nesting or flatten where possible?
