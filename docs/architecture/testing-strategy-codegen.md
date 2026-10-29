# Testing Strategy: IR → TypeScript Code Generation Pipeline

**Created:** 2026-01-19
**Context:** After TypeRef v2.0 migration, we encountered confusion from stale output directories and lack of validation that generated code actually compiles.

## Problem Statement

The codegen pipeline has multiple failure modes that weren't caught:

1. **Stale output confusion**: Old `/tmp/codegen-test/` had incomplete files, leading to false negative conclusions
2. **Silent structural issues**: Missing types.ts files weren't immediately obvious
3. **No compilation validation**: Generated TypeScript wasn't tested for actual compilability
4. **Import correctness**: Malformed imports like `Forge.ForgeRepoSummary` weren't caught early
5. **Version mismatch**: IR version hardcoding wasn't detected until manual inspection

## Testing Layers

### Layer 1: Unit Tests (Haskell - Synapse)

**Purpose:** Validate IR generation correctness at the type level

**Location:** `synapse/test/`

#### 1.1 TypeRef Serialization Tests
```haskell
-- test/TypeRefJsonSpec.hs
describe "QualifiedName JSON serialization" $ do
  it "serializes with namespace" $ do
    let qn = QualifiedName "arbor" "Handle"
    encode qn `shouldBe` "{\"qnNamespace\":\"arbor\",\"qnLocalName\":\"Handle\"}"

  it "serializes empty namespace" $ do
    let qn = QualifiedName "" "UUID"
    encode qn `shouldBe` "{\"qnNamespace\":\"\",\"qnLocalName\":\"UUID\"}"

describe "TypeRef serialization" $ do
  it "RefNamed uses structured QualifiedName" $ do
    let ref = RefNamed (QualifiedName "cone" "UUID")
    let json = encode ref
    json `shouldContain` "qnNamespace"
    json `shouldContain` "qnLocalName"
```

#### 1.2 IR Version Tests
```haskell
-- test/IRSpec.hs
describe "IR version" $ do
  it "emptyIR has correct version" $
    irVersion emptyIR `shouldBe` "2.0"

  it "irAlgebra preserves version" $ do
    let ir = buildIR []  -- actual build
    irVersion ir `shouldBe` irVersion emptyIR
```

#### 1.3 Namespace Construction Tests
```haskell
describe "Namespace paths" $ do
  it "uses full path for nested plugins" $ do
    -- Mock schema with hyperforge.workspace.repos
    let ir = buildIRFromMockSchema mockNestedPlugin
    let methods = Map.keys (irMethods ir)
    methods `shouldContain` "hyperforge.workspace.repos.list"

    -- Check the namespace is full path
    let method = irMethods ir Map.! "hyperforge.workspace.repos.list"
    mdNamespace method `shouldBe` "hyperforge.workspace.repos"
```

**Action Items:**
- [ ] Add `TypeRefJsonSpec.hs` with serialization round-trip tests
- [ ] Extend `IRSpec.hs` with version consistency checks
- [ ] Add property tests for QualifiedName invariants
- [ ] Test all RefNamed construction sites produce valid QualifiedNames

---

### Layer 2: Unit Tests (Rust - service-codegen)

**Purpose:** Validate TypeScript generation logic

**Location:** `service-codegen/tests/`

#### 2.1 TypeRef Import Generation Tests
```rust
// tests/type_imports.rs
#[test]
fn qualified_name_extracts_clean_local_name() {
    let qn = QualifiedName {
        qn_namespace: "hyperforge.forge".to_string(),
        qn_local_name: "ForgeRepoSummary".to_string(),
    };

    assert_eq!(qn.local_name(), "ForgeRepoSummary");
    assert_eq!(qn.namespace(), Some("hyperforge.forge"));
    assert_eq!(qn.full_name(), "hyperforge.forge.ForgeRepoSummary");
}

#[test]
fn import_generation_uses_local_names() {
    // Create a TypeRef with cross-namespace reference
    let type_ref = TypeRef::RefNamed(QualifiedName {
        qn_namespace: "hyperforge.forge".to_string(),
        qn_local_name: "Forge".to_string(),
    });

    let imports = collect_imports_for_type(&type_ref, "hyperforge.workspace");

    // Should generate: import type { Forge } from '../forge/types';
    assert!(imports.contains_key("hyperforge.forge"));
    assert_eq!(imports.get("hyperforge.forge").unwrap(), &vec!["Forge"]);

    // Should NOT contain dots in the imported name
    let import_line = format_import("hyperforge.forge", &imports["hyperforge.forge"]);
    assert!(!import_line.contains("Forge."));
}
```

#### 2.2 File Structure Tests
```rust
#[test]
fn generates_all_required_files() {
    let ir = load_test_ir("fixtures/ir-v2-sample.json");
    let result = generate(&ir).unwrap();

    // Core files
    assert!(result.files.contains_key("types.ts"));
    assert!(result.files.contains_key("rpc.ts"));
    assert!(result.files.contains_key("transport.ts"));
    assert!(result.files.contains_key("index.ts"));

    // Namespace type files for each namespace with types
    for (_, typedef) in &ir.ir_types {
        if !typedef.td_namespace.is_empty() {
            let path = format!("{}/types.ts", typedef.td_namespace.replace('.', "/"));
            assert!(
                result.files.contains_key(&path),
                "Missing types.ts for namespace: {}", typedef.td_namespace
            );
        }
    }

    // Client files for each plugin
    for namespace in ir.ir_plugins.keys() {
        if !namespace.is_empty() {
            let path = format!("{}/client.ts", namespace.replace('.', "/"));
            assert!(
                result.files.contains_key(&path),
                "Missing client.ts for namespace: {}", namespace
            );
        }
    }
}
```

#### 2.3 IR Version Validation Tests
```rust
#[test]
fn rejects_old_ir_versions() {
    let mut ir = load_test_ir("fixtures/ir-v2-sample.json");
    ir.ir_version = "1.0".to_string();

    let result = generate(&ir);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Unsupported IR version"));
}

#[test]
fn accepts_current_ir_version() {
    let ir = load_test_ir("fixtures/ir-v2-sample.json");
    assert_eq!(ir.ir_version, "2.0");

    let result = generate(&ir);
    assert!(result.is_ok());
}
```

**Action Items:**
- [ ] Create `tests/type_imports.rs` with import validation
- [ ] Create `tests/file_structure.rs` with completeness checks
- [ ] Add `fixtures/` with known-good IR samples
- [ ] Test relative import path calculation for all nesting levels

---

### Layer 3: Integration Tests (Cross-boundary)

**Purpose:** Test the full Haskell → Rust pipeline

**Location:** `synapse/test/integration/` or `service-codegen/tests/integration/`

#### 3.1 Golden File Tests
```bash
#!/bin/bash
# test/integration/golden_test.sh

# Generate IR from live Plexus
cabal run synapse -- -i > /tmp/test-ir-current.json

# Generate TypeScript
cargo run --manifest-path ../service-codegen/Cargo.toml -- \
  /tmp/test-ir-current.json -o /tmp/test-codegen-output

# Compare against golden files (known good structure)
diff -r test/golden/expected-structure.txt <(find /tmp/test-codegen-output -type f | sort)

# Check no malformed imports (no dots in type names after 'import type {')
! grep -r "import type {[^}]*\\..*}" /tmp/test-codegen-output || {
  echo "ERROR: Found malformed imports with dots"
  exit 1
}

# Check all expected namespace types exist
for ns in hyperforge hyperforge.forge hyperforge.org hyperforge.workspace; do
  path="${ns//./\/}/types.ts"
  [ -f "/tmp/test-codegen-output/$path" ] || {
    echo "ERROR: Missing $path"
    exit 1
  }
done
```

#### 3.2 TypeScript Compilation Tests
```typescript
// test/integration/compilation.test.ts
import { describe, it } from 'vitest';
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

describe('Generated TypeScript compilation', () => {
  it('compiles without errors', async () => {
    // Run tsc on generated code
    const { stdout, stderr } = await execAsync(
      'npx tsc --noEmit',
      { cwd: '/tmp/test-codegen-output' }
    );

    expect(stderr).toBe('');
    expect(stdout).not.toContain('error TS');
  });

  it('has no duplicate identifiers', async () => {
    const { stdout } = await execAsync(
      'npx tsc --noEmit 2>&1 | grep "Duplicate identifier" || true',
      { cwd: '/tmp/test-codegen-output' }
    );

    expect(stdout).toBe('');
  });
});
```

#### 3.3 Snapshot Tests
```rust
// service-codegen/tests/snapshots.rs
use insta::assert_snapshot;

#[test]
fn snapshot_hyperforge_forge_types() {
    let ir = load_test_ir("fixtures/ir-v2-hyperforge.json");
    let result = generate(&ir).unwrap();

    let forge_types = result.files.get("hyperforge/forge/types.ts").unwrap();
    assert_snapshot!(forge_types);
}

#[test]
fn snapshot_cross_namespace_imports() {
    let ir = load_test_ir("fixtures/ir-v2-cross-refs.json");
    let result = generate(&ir).unwrap();

    let workspace_types = result.files.get("hyperforge/workspace/types.ts").unwrap();
    assert_snapshot!(workspace_types);

    // Snapshot should show clean imports like:
    // import type { Forge } from '../forge/types';
}
```

**Action Items:**
- [ ] Create `test/integration/golden_test.sh` with structure validation
- [ ] Add TypeScript compilation check to test suite
- [ ] Set up `insta` for snapshot testing in Rust
- [ ] Create fixture IRs representing common patterns

---

### Layer 4: End-to-End Validation

**Purpose:** Verify the generated code actually works

#### 4.1 Smoke Tests
```typescript
// generated/test/smoke.test.ts (already exists, enhance it)
import { describe, it, expect } from 'vitest';
import { createWebSocketTransport } from '../transport';
import { HyperforgeClient } from '../hyperforge';

describe('Generated client smoke tests', () => {
  it('imports resolve correctly', () => {
    // If imports are malformed, this won't even compile
    expect(HyperforgeClient).toBeDefined();
  });

  it('has correct method signatures', () => {
    const transport = createWebSocketTransport('ws://localhost:4444');
    const client = new HyperforgeClient(transport);

    // TypeScript will error if signature is wrong
    const versionGen = client.version();
    expect(versionGen).toBeDefined();
  });
});
```

#### 4.2 Regression Detection
```bash
#!/bin/bash
# test/integration/regression_check.sh

# Generate from current IR
cabal run synapse -- -i > /tmp/ir-current.json
cargo run --manifest-path ../service-codegen/Cargo.toml -- \
  /tmp/ir-current.json -o /tmp/codegen-current

# Count files generated
CURRENT_FILES=$(find /tmp/codegen-current -name "*.ts" | wc -l)

# Compare to baseline (stored in test/baseline/)
BASELINE_FILES=$(cat test/baseline/file_count.txt)

if [ "$CURRENT_FILES" -lt "$BASELINE_FILES" ]; then
  echo "ERROR: Generated fewer files than baseline ($CURRENT_FILES < $BASELINE_FILES)"
  echo "Missing files:"
  comm -23 \
    <(cat test/baseline/files.txt | sort) \
    <(find /tmp/codegen-current -name "*.ts" | sort)
  exit 1
fi

# Check for specific known-bad patterns
if grep -r "import.*\\..*}" /tmp/codegen-current/; then
  echo "ERROR: Found malformed imports with dots in identifiers"
  exit 1
fi

echo "✓ Regression checks passed"
```

**Action Items:**
- [ ] Enhance smoke test with actual method calls
- [ ] Create baseline snapshots of file structure
- [ ] Add pre-commit hook to run regression checks
- [ ] Set up CI to fail on fewer files generated

---

## Automation Strategy

### Pre-commit Hooks
```bash
# .git/hooks/pre-commit
#!/bin/bash

# Quick checks before allowing commit
echo "Running pre-commit checks..."

# 1. Haskell tests
cabal test || exit 1

# 2. Rust tests
(cd ../service-codegen && cargo test) || exit 1

# 3. IR generation test
cabal run synapse -- -i > /tmp/pre-commit-ir.json
jq -e '.irVersion == "2.0"' /tmp/pre-commit-ir.json || {
  echo "ERROR: IR version is not 2.0"
  exit 1
}

echo "✓ Pre-commit checks passed"
```

### CI Pipeline
```yaml
# .github/workflows/codegen-test.yml
name: Codegen Pipeline Tests

on: [push, pull_request]

jobs:
  test-haskell:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: haskell/actions/setup@v2
      - run: cabal test

  test-rust:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions-rs/toolchain@v1
      - run: cargo test
        working-directory: service-codegen

  test-e2e:
    runs-on: ubuntu-latest
    needs: [test-haskell, test-rust]
    services:
      substrate:
        image: substrate:latest
    steps:
      - name: Generate IR
        run: cabal run synapse -- -i > /tmp/ir.json

      - name: Generate TypeScript
        run: cargo run --manifest-path service-codegen/Cargo.toml -- /tmp/ir.json -o /tmp/generated

      - name: Validate structure
        run: test/integration/golden_test.sh

      - name: TypeScript compilation
        run: |
          cd /tmp/generated
          npm install
          npx tsc --noEmit

      - name: Run smoke tests
        run: |
          cd /tmp/generated
          npm test
```

---

## Test Fixtures Strategy

### Minimal Reproduction Cases

Create minimal IR fixtures for specific scenarios:

**`test/fixtures/ir-v2-cross-namespace.json`**
```json
{
  "irVersion": "2.0",
  "irTypes": {
    "forge.Forge": {
      "tdName": "Forge",
      "tdNamespace": "forge",
      "tdKind": {"tag": "KindEnum", "keVariants": [...]}
    },
    "workspace.WorkspaceEvent": {
      "tdName": "WorkspaceEvent",
      "tdNamespace": "workspace",
      "tdKind": {
        "tag": "KindStruct",
        "ksFields": [{
          "fdName": "forge",
          "fdType": {
            "tag": "RefNamed",
            "contents": {
              "qnNamespace": "forge",
              "qnLocalName": "Forge"
            }
          }
        }]
      }
    }
  }
}
```

This fixture specifically tests cross-namespace references.

**Action Items:**
- [ ] Create minimal fixtures for each test scenario
- [ ] Add fixtures for edge cases (empty namespace, deep nesting, collisions)
- [ ] Version fixtures alongside IR version bumps

---

## Validation Checklist (Run After Changes)

```bash
#!/bin/bash
# test/validate_codegen.sh - Run this after any IR or generator changes

set -e

echo "=== Validation Pipeline ==="

# 1. Clean slate
rm -rf /tmp/validation-test

# 2. Unit tests
echo "→ Running Haskell tests..."
cabal test

echo "→ Running Rust tests..."
(cd ../service-codegen && cargo test)

# 3. Generate fresh IR
echo "→ Generating IR..."
cabal run synapse -- -i > /tmp/validation-test/ir.json

# 4. Validate IR structure
echo "→ Validating IR..."
jq -e '.irVersion == "2.0"' /tmp/validation-test/ir.json
jq -e '.irTypes | length > 0' /tmp/validation-test/ir.json
jq -e '.irMethods | length > 0' /tmp/validation-test/ir.json

# 5. Generate TypeScript
echo "→ Generating TypeScript..."
cargo run --manifest-path ../service-codegen/Cargo.toml --release -- \
  /tmp/validation-test/ir.json -o /tmp/validation-test/generated

# 6. Check file structure
echo "→ Validating file structure..."
[ -f /tmp/validation-test/generated/types.ts ] || exit 1
[ -f /tmp/validation-test/generated/rpc.ts ] || exit 1

# Count namespace type files
NAMESPACE_TYPES=$(find /tmp/validation-test/generated -path "*/*/types.ts" | wc -l)
echo "  Found $NAMESPACE_TYPES namespace type files"
[ "$NAMESPACE_TYPES" -gt 0 ] || exit 1

# 7. Check for malformed imports
echo "→ Checking for malformed imports..."
! grep -r "import type {[^}]*\\..*}" /tmp/validation-test/generated || {
  echo "ERROR: Found malformed imports"
  exit 1
}

# 8. TypeScript compilation
echo "→ Compiling TypeScript..."
cd /tmp/validation-test/generated
npm install --silent
npx tsc --noEmit

# 9. Run generated smoke tests
echo "→ Running smoke tests..."
npm test

echo "✅ All validation checks passed!"
```

**Usage:**
```bash
# After making changes to IR types or generator
./test/validate_codegen.sh

# Add to CI to run on every PR
```

---

## Specific Issue Prevention

### Issue 1: Stale Output Directories

**Prevention:**
- Always use timestamped or unique output directories in tests
- Add cleanup step to test scripts
- Use `mktemp -d` for temporary test outputs
- CI should always start from clean slate

**Example:**
```bash
# BAD: Reuses same directory
cargo run -- ir.json -o /tmp/test-output

# GOOD: Unique directory per run
TEST_DIR=$(mktemp -d)
cargo run -- ir.json -o "$TEST_DIR"
# ... validate ...
rm -rf "$TEST_DIR"
```

### Issue 2: Missing Type Files

**Prevention:**
- File structure test (Layer 2.2 above) explicitly checks for all expected files
- Golden test compares full file tree
- Integration test fails if any namespace is missing types.ts

### Issue 3: Malformed Imports

**Prevention:**
- Unit test for `QualifiedName::local_name()` extraction
- Regex check in integration tests: `! grep "import.*\..*}"`
- TypeScript compilation will fail on invalid identifiers
- Snapshot tests capture import format

### Issue 4: Version Mismatches

**Prevention:**
- IR version test in Haskell (emptyIR == irAlgebra result)
- Rust version validation test (reject old versions)
- CI checks IR version before attempting generation

---

## Metrics to Track

Add these assertions to tests:

1. **File count stability**: Generated file count shouldn't decrease
2. **Import cleanliness**: Zero imports with dots in identifiers
3. **Compilation time**: TypeScript compilation should complete in < 10s
4. **Test coverage**: Aim for 80%+ coverage on generator logic
5. **Breaking changes**: Track IR version bumps and deprecation

---

## Implementation Priority

1. **P0 (Critical):**
   - [ ] Integration test: TypeScript compilation check
   - [ ] Validation script: `test/validate_codegen.sh`
   - [ ] Malformed import regex check

2. **P1 (High):**
   - [ ] Rust unit tests for import generation
   - [ ] File structure completeness test
   - [ ] Golden test for file tree structure

3. **P2 (Medium):**
   - [ ] Snapshot tests with `insta`
   - [ ] Haskell property tests for QualifiedName
   - [ ] Baseline regression detection

4. **P3 (Nice to have):**
   - [ ] Pre-commit hooks
   - [ ] Full CI pipeline
   - [ ] Coverage reporting

---

## Rollout Plan

**Week 1:**
- Implement validation script (`validate_codegen.sh`)
- Add malformed import check
- Create basic TypeScript compilation test

**Week 2:**
- Add Rust unit tests for file structure
- Create golden test for expected files
- Set up test fixtures

**Week 3:**
- Implement snapshot testing
- Add Haskell version consistency tests
- Create regression baseline

**Week 4:**
- Set up CI pipeline
- Add pre-commit hooks
- Document test running procedures

---

## Success Criteria

Tests are successful when:

1. ✅ A change that breaks imports is caught by unit tests
2. ✅ Missing type files are caught before manual inspection
3. ✅ Stale output directories are impossible (tests use temp dirs)
4. ✅ Version mismatches are caught in < 5 seconds
5. ✅ Generated TypeScript compiles in CI
6. ✅ Regression (fewer files generated) is caught automatically

---

## References

- **Property Testing:** Use QuickCheck (Haskell) / proptest (Rust) for invariants
- **Snapshot Testing:** `insta` crate for Rust
- **Golden Testing:** Store expected outputs in `test/golden/`
- **Coverage:** `cargo-tarpaulin` for Rust, HPC for Haskell
