# Synapse Test Suite

This directory contains comprehensive tests for the Synapse CLI, including unit tests and integration tests.

## Test Suites

### 1. Unit Tests

#### `ask-test` - A method that asks (ASK-3)
The built synapse against the mock service's methods that propose
(`needs_input`): on a terminal (a pty) `[y/N]` and a prompt per field, then
the same call with the fields and `confirm`; no terminal: the proposal, the
ready command on stderr, exit 10; `--yes`; `--help`'s `--confirm TOKEN`; the
IR's `mdAsks`.

**Run it:** `cabal test ask-test` (no backend required)

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
├── AskSpec.hs                    # A method that asks (needs_input), against the mock
├── CLISpec.hs                    # CLI integration tests
├── IRSpec.hs                     # IR builder integration tests
├── ParseSpec.hs                  # Parser unit tests
├── PathNormalizationSpec.hs      # Path normalization tests
├── TypeRefJson.hs                # TypeRef JSON tests
├── validate_codegen.sh           # Codegen validation script
└── README.md                     # This file
```

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

See `IRSpec.hs` for an example.

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
cabal test ask-test parse-test path-normalization-test typeref-json
```

---

## Troubleshooting

### "Could not connect" error
- Ensure a Service backend is running on the specified port
- Check firewall settings
- Verify the backend name matches the running backend

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
| ask-test | ✅ PASS | 21 | No |
| ir-test | ⚠️ Requires backend | - | Yes |
| cli-test | ⚠️ Requires binary | - | Yes |
| parse-test | ✅ (existing) | - | No |
| path-normalization-test | ✅ (existing) | - | No |
| typeref-json | ✅ (existing) | - | No |

---

## Future Improvements

1. **Performance Tests**
   - JSON parsing benchmarks
   - Large payload handling
   - Streaming performance

2. **Property-Based Tests**
   - Use QuickCheck for JSON roundtrip properties
   - Generate random stream items
   - Test invariants (e.g., decode . encode = id)

---

## Questions?

For questions about the test suite, see:
- Test files themselves (well-commented)
- Main Synapse documentation
- Hspec documentation: https://hspec.github.io/
