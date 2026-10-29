#!/bin/bash
# Validation script for IR → TypeScript codegen pipeline
# Run this after making changes to IR types or generator logic

set -e

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  Codegen Pipeline Validation"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo

# Create unique test directory
TEST_DIR=$(mktemp -d -t codegen-validation-XXXXXX)
trap "rm -rf $TEST_DIR" EXIT

echo "Test directory: $TEST_DIR"
echo

# ============================================================================
# Step 1: Unit Tests
# ============================================================================
echo "${YELLOW}→ Running Haskell tests...${NC}"
if cabal test 2>&1 | tail -5; then
  echo "${GREEN}✓ Haskell tests passed${NC}"
else
  echo "${RED}✗ Haskell tests failed${NC}"
  exit 1
fi
echo

echo "${YELLOW}→ Running Rust tests...${NC}"
if (cd ../service-codegen && cargo test 2>&1 | tail -10); then
  echo "${GREEN}✓ Rust tests passed${NC}"
else
  echo "${RED}✗ Rust tests failed${NC}"
  exit 1
fi
echo

# ============================================================================
# Step 2: Generate Fresh IR
# ============================================================================
echo "${YELLOW}→ Generating IR...${NC}"
IR_FILE="$TEST_DIR/ir.json"

if ! cabal run synapse -- -i > "$IR_FILE" 2>/dev/null; then
  echo "${RED}✗ IR generation failed${NC}"
  exit 1
fi

# Validate IR structure
IR_VERSION=$(jq -r '.irVersion' "$IR_FILE")
TYPE_COUNT=$(jq '.irTypes | length' "$IR_FILE")
METHOD_COUNT=$(jq '.irMethods | length' "$IR_FILE")

echo "  IR Version: $IR_VERSION"
echo "  Types: $TYPE_COUNT"
echo "  Methods: $METHOD_COUNT"

if [ "$IR_VERSION" != "2.0" ]; then
  echo "${RED}✗ IR version is $IR_VERSION, expected 2.0${NC}"
  exit 1
fi

if [ "$TYPE_COUNT" -eq 0 ]; then
  echo "${RED}✗ IR has no types${NC}"
  exit 1
fi

if [ "$METHOD_COUNT" -eq 0 ]; then
  echo "${RED}✗ IR has no methods${NC}"
  exit 1
fi

echo "${GREEN}✓ IR generation successful${NC}"
echo

# ============================================================================
# Step 3: Validate IR Structure
# ============================================================================
echo "${YELLOW}→ Validating IR type structure...${NC}"

# Check that RefNamed uses QualifiedName structure
REFNAMED_COUNT=$(jq '[.irTypes | to_entries[].value.tdKind | .. | objects | select(.tag == "RefNamed")] | length' "$IR_FILE")
if [ "$REFNAMED_COUNT" -gt 0 ]; then
  # Check that RefNamed contents have qnNamespace and qnLocalName
  STRUCTURED_COUNT=$(jq '[.irTypes | to_entries[].value.tdKind | .. | objects | select(.tag == "RefNamed" and .contents.qnNamespace != null)] | length' "$IR_FILE")

  if [ "$STRUCTURED_COUNT" -ne "$REFNAMED_COUNT" ]; then
    echo "${RED}✗ Found RefNamed without QualifiedName structure${NC}"
    echo "  RefNamed count: $REFNAMED_COUNT"
    echo "  Structured count: $STRUCTURED_COUNT"
    exit 1
  fi

  echo "  Found $STRUCTURED_COUNT properly structured RefNamed types"
fi

echo "${GREEN}✓ IR structure is valid${NC}"
echo

# ============================================================================
# Step 4: Generate TypeScript
# ============================================================================
echo "${YELLOW}→ Generating TypeScript...${NC}"
GEN_DIR="$TEST_DIR/generated"

if ! cargo run --manifest-path ../service-codegen/Cargo.toml --release -- "$IR_FILE" -o "$GEN_DIR" 2>&1 | tail -20; then
  echo "${RED}✗ TypeScript generation failed${NC}"
  exit 1
fi

echo "${GREEN}✓ TypeScript generation successful${NC}"
echo

# ============================================================================
# Step 5: Validate File Structure
# ============================================================================
echo "${YELLOW}→ Validating file structure...${NC}"

# Core files
REQUIRED_FILES=("types.ts" "rpc.ts" "transport.ts" "index.ts" "package.json" "tsconfig.json")
for file in "${REQUIRED_FILES[@]}"; do
  if [ ! -f "$GEN_DIR/$file" ]; then
    echo "${RED}✗ Missing required file: $file${NC}"
    exit 1
  fi
done

# Count namespace type files (should have at least one per namespace)
NAMESPACE_TYPES=$(find "$GEN_DIR" -path "*/*/types.ts" | wc -l | tr -d ' ')
echo "  Namespace type files: $NAMESPACE_TYPES"

if [ "$NAMESPACE_TYPES" -eq 0 ]; then
  echo "${RED}✗ No namespace type files generated${NC}"
  exit 1
fi

# Count client files
CLIENT_FILES=$(find "$GEN_DIR" -name "client.ts" | wc -l | tr -d ' ')
echo "  Client files: $CLIENT_FILES"

if [ "$CLIENT_FILES" -eq 0 ]; then
  echo "${RED}✗ No client files generated${NC}"
  exit 1
fi

echo "${GREEN}✓ File structure is complete${NC}"
echo

# ============================================================================
# Step 6: Check for Malformed Imports
# ============================================================================
echo "${YELLOW}→ Checking for malformed imports...${NC}"

# Look for imports with dots in type names (e.g., "import type { Forge.Something }")
MALFORMED=$(grep -r "import type {[^}]*\.[^/]*}" "$GEN_DIR" || true)

if [ -n "$MALFORMED" ]; then
  echo "${RED}✗ Found malformed imports with dots in identifiers:${NC}"
  echo "$MALFORMED"
  exit 1
fi

echo "${GREEN}✓ All imports are well-formed${NC}"
echo

# ============================================================================
# Step 7: TypeScript Compilation
# ============================================================================
echo "${YELLOW}→ Installing TypeScript dependencies...${NC}"
cd "$GEN_DIR"
npm install --silent 2>&1 | grep -v "^npm WARN" || true

echo "${YELLOW}→ Compiling TypeScript...${NC}"
# Use PIPESTATUS to capture tsc exit code, not tee's exit code
npx tsc --noEmit 2>&1 | tee /tmp/tsc-output.txt
TSC_EXIT=${PIPESTATUS[0]}

if [ $TSC_EXIT -eq 0 ]; then
  echo "${GREEN}✓ TypeScript compilation successful${NC}"
else
  echo "${RED}✗ TypeScript compilation failed with exit code $TSC_EXIT:${NC}"
  cat /tmp/tsc-output.txt
  exit 1
fi
echo

# ============================================================================
# Summary
# ============================================================================
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "${GREEN}✅ All validation checks passed!${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
echo "Summary:"
echo "  IR Version: $IR_VERSION"
echo "  Types: $TYPE_COUNT"
echo "  Methods: $METHOD_COUNT"
echo "  Namespace type files: $NAMESPACE_TYPES"
echo "  Client files: $CLIENT_FILES"
echo "  Generated files: $(find "$GEN_DIR" -name "*.ts" | wc -l | tr -d ' ')"
echo
