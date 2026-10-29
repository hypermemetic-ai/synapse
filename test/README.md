# Synapse Test Suite

This directory contains comprehensive tests for the Synapse CLI, including unit tests, integration tests, and bidirectional communication tests.

## Test Suites

### 1. Unit Tests

#### `bidir-test` - Bidirectional Communication Unit Tests
Tests for the bidirectional communication types and handlers in `Synapse.Bidir`.

**What it tests:**
- Request type constructors (Confirm, Prompt, Select, CustomRequest)
- Response type constructors (Confirmed, Value, Selected, CustomResponse, Cancelled)
- JSON serialization/deserialization for Request and Response types
- SelectOption type handling
- BidirMode parsing and equality
- Request/Response type compatibility
- Complex scenarios (nested JSON, multiple selections, etc.)
- Edge cases (empty strings, empty lists, etc.)

**Run it:**
```bash
HOME=/tmp/cabal312home cabal-3.12.1.0 test bidir-test --project-dir=/workspace/hypermemetic/synapse
```

**Test count:** 56 examples

**No backend required** - Pure unit tests

---

#### `parse-test` - CLI Parsing Tests
Tests for command-line argument parsing.

**Run it:**
```bash
HOME=/tmp/cabal312home cabal-3.12.1.0 test parse-test --project-dir=/workspace/hypermemetic/synapse
```

---

#### `path-normalization-test` - Path Normalization Tests
Tests for path normalization logic.

**Run it:**
```bash
HOME=/tmp/cabal312home cabal-3.12.1.0 test path-normalization-test --project-dir=/workspace/hypermemetic/synapse
```

---

#### `typeref-json` - TypeRef JSON Serialization
Tests for TypeRef JSON encoding/decoding.

**Run it:**
```bash
HOME=/tmp/cabal312home cabal-3.12.1.0 test typeref-json --project-dir=/workspace/hypermemetic/synapse
```

---

### 2. Integration Tests

#### `bidir-integration-test` - Bidirectional Schema Integration
Tests for schema-driven bidirectional communication features.

**What it tests:**
- IR builder correctly populates `mdBidirType` field
- Methods with `bidirectional: true` in schema are detected
- `mdBidirType` is set to `RefAny` for standard bidirectional methods
- Type references in `mdBidirType` resolve correctly
- Bidirectional methods have valid structure (name, path, return type)
- Schema-driven dispatch assumptions
- Statistical reporting of bidirectional coverage

**Requires:** Running Service backend (default: localhost:4444)

**Run it:**
```bash
# Default: localhost:4444, backend "plexus"
HOME=/tmp/cabal312home cabal-3.12.1.0 test bidir-integration-test \
  --project-dir=/workspace/hypermemetic/synapse \
  --test-options="plexus"

# Custom port
HOME=/tmp/cabal312home cabal-3.12.1.0 test bidir-integration-test \
  --project-dir=/workspace/hypermemetic/synapse \
  --test-options="plexus --port 5555"
```

**Graceful degradation:** If no backend is running, the test will report a connection error but won't crash. If no bidirectional methods exist in the schema, tests will be informational only.

---

#### `ir-test` - IR Integration Tests
Tests for the Intermediate Representation builder.

**What it tests:**
- Schema fetching and IR building
- Type reference resolution
- Method help rendering
- Support level detection

**Requires:** Running Service backend

**Run it:**
```bash
HOME=/tmp/cabal312home cabal-3.12.1.0 test ir-test \
  --project-dir=/workspace/hypermemetic/synapse \
  --test-options="plexus"
```

---

#### `cli-test` - CLI Integration Tests
End-to-end tests for CLI navigation and invocation.

**What it tests:**
- Navigation through plugin hierarchy
- Method help display
- Method invocation with parameters
- Output validation

**Requires:** Built synapse executable

**Run it:**
```bash
HOME=/tmp/cabal312home cabal-3.12.1.0 test cli-test --project-dir=/workspace/hypermemetic/synapse
```

---

## Running All Tests

Run all test suites:
```bash
HOME=/tmp/cabal312home cabal-3.12.1.0 test --project-dir=/workspace/hypermemetic/synapse
```

For integration tests that require a backend:
```bash
HOME=/tmp/cabal312home cabal-3.12.1.0 test --project-dir=/workspace/hypermemetic/synapse \
  --test-options="plexus"
```

---

## Test Organization

```
test/
├── BidirSpec.hs                  # Unit tests for bidirectional types
├── BidirIntegrationSpec.hs       # Integration tests for bidir + IR
├── CLISpec.hs                    # CLI integration tests
├── IRSpec.hs                     # IR builder integration tests
├── ParseSpec.hs                  # Parser unit tests
├── PathNormalizationSpec.hs      # Path normalization tests
├── TypeRefJson.hs                # TypeRef JSON tests
├── validate_codegen.sh           # Codegen validation script
└── README.md                     # This file
```

---

## Bidirectional Communication Test Coverage

The bidirectional tests cover the complete feature set:

### 1. Type Handling (`BidirSpec.hs`)
- ✅ Request constructors: Confirm, Prompt, Select, CustomRequest
- ✅ Response constructors: Confirmed, Value, Selected, CustomResponse, Cancelled
- ✅ SelectOption handling
- ✅ JSON serialization (Request → JSON → Request)
- ✅ JSON deserialization (Response → JSON → Response)
- ✅ Wire format compatibility (Value response uses "text" tag)

### 2. Mode Handling (`BidirSpec.hs`)
- ✅ BidirMode parsing from strings
- ✅ Mode equality and comparison
- ✅ Default mode selection

### 3. Schema Integration (`BidirIntegrationSpec.hs`)
- ✅ IR builder sets `mdBidirType` field
- ✅ Bidirectional methods detected from schema
- ✅ Type inference (RefAny for standard bidir)
- ✅ Type reference resolution
- ✅ Method structure validation
- ✅ Coverage statistics

### Not Tested (require actual subprocess/TTY)
- ❌ BidirCmd mode (subprocess spawning)
- ❌ BidirRespond mode (stdout printing)
- ❌ BidirInteractive mode (TTY prompts)
- ❌ BidirJson mode (stdin/stdout piping)
- ❌ BidirAutoCancel mode (stderr warnings)
- ❌ BidirDefaults mode (default value handling)

These modes require integration tests with actual I/O, which are better suited for end-to-end testing or manual verification.

---

## Test Framework

All tests use **Hspec** (https://hspec.github.io/), a behavior-driven development framework for Haskell.

Key features:
- Descriptive test organization with `describe` and `it`
- Expressive matchers: `shouldBe`, `shouldSatisfy`, `shouldNotBe`
- Clear failure messages
- Easy to read test output

---

## Adding New Tests

### Adding a Unit Test

1. Create a new test file in `test/` (e.g., `MyFeatureSpec.hs`)
2. Add test suite to `plexus-synapse.cabal`:
   ```cabal
   test-suite my-feature-test
     type:             exitcode-stdio-1.0
     main-is:          MyFeatureSpec.hs
     build-depends:
       base >= 4.17 && < 5,
       plexus-synapse,
       hspec >= 2.10 && < 2.12
     hs-source-dirs:   test
     default-language: GHC2021
     ghc-options:      -threaded
   ```
3. Write tests using Hspec:
   ```haskell
   main :: IO ()
   main = hspec $ do
     describe "My Feature" $ do
       it "does something" $ do
         result `shouldBe` expected
   ```

### Adding an Integration Test

Follow the same steps as unit tests, but:
- Accept command-line arguments for backend/port
- Handle connection failures gracefully
- Use `pending` for tests that can't run without backend
- Report informational messages with `putStrLn`

See `BidirIntegrationSpec.hs` for a complete example.

---

## Continuous Integration

To run tests in CI:

1. Start a Service backend (if integration tests are included)
2. Run all tests with appropriate options:
   ```bash
   cabal test --project-dir=/workspace/hypermemetic/synapse --test-options="plexus"
   ```
3. Check exit code (0 = success, non-zero = failure)

For unit tests only (no backend required):
```bash
cabal test bidir-test parse-test path-normalization-test typeref-json
```

---

## Troubleshooting

### "Could not connect" error
- Ensure a Service backend is running on the specified port
- Check firewall settings
- Verify the backend name matches the running backend

### "No bidirectional methods found"
- This is informational, not a failure
- The backend may not have any methods marked with `#[bidirectional]`
- Tests will pass but report zero coverage

### Build errors
- Ensure cabal is up to date: `cabal update`
- Clean build artifacts: `cabal clean`
- Check GHC version compatibility (requires GHC 9.4+)

### Test failures
- Read the failure message carefully - Hspec provides detailed output
- Check the test file for the specific assertion that failed
- Run with `--test-show-details=direct` for more verbose output

---

## Test Results Summary

As of the latest run:

| Test Suite | Status | Count | Backend Required |
|------------|--------|-------|------------------|
| bidir-test | ✅ PASS | 56 | No |
| bidir-integration-test | ⚠️ Requires backend | - | Yes |
| ir-test | ⚠️ Requires backend | - | Yes |
| cli-test | ⚠️ Requires binary | - | Yes |
| parse-test | ✅ (existing) | - | No |
| path-normalization-test | ✅ (existing) | - | No |
| typeref-json | ✅ (existing) | - | No |

**Total bidirectional unit tests: 56 ✅**

---

## Future Improvements

1. **BidirHandler I/O Tests**
   - Mock stdin/stdout for Json mode
   - Test subprocess creation for Cmd mode
   - Verify stderr output for AutoCancel mode

2. **End-to-End Bidir Tests**
   - Full workflow: schema → dispatch → handler → response
   - Test with actual bidirectional methods
   - Verify request/response round-trip

3. **Performance Tests**
   - JSON parsing benchmarks
   - Large payload handling
   - Streaming performance

4. **Property-Based Tests**
   - Use QuickCheck for JSON roundtrip properties
   - Generate random Request/Response values
   - Test invariants (e.g., decode . encode = id)

---

## Questions?

For questions about the test suite, see:
- Test files themselves (well-commented)
- Main Synapse documentation
- Hspec documentation: https://hspec.github.io/
