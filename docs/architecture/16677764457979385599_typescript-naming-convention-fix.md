# TypeScript Naming Convention Fix: Complete Implementation

**Status**: Implemented
**Date**: 2026-01-21
**Related**: `16677793986132070655_typescript-codegen-snake-case.md`, `16677786647750223615_typescript-codegen-root-cause.md`

## Summary

Fixed TypeScript client naming convention mismatch between idiomatic TypeScript (camelCase) and Plexus RPC protocol (snake_case). Implemented bidirectional transformation at the RPC boundary, similar to Rust's `#[serde(rename_all = "camelCase")]`.

## Problem

TypeScript clients failed to communicate with Plexus backend due to naming convention mismatch:

**Requests**: TypeScript sent `camelCase` parameters but Plexus expected `snake_case`
```typescript
// ❌ Before
cone.create(modelId, systemPrompt)
  → RPC: { modelId: "...", systemPrompt: "..." }  // Backend doesn't understand
```

**Responses**: Backend sent `snake_case` fields but TypeScript expected `camelCase`
```json
// Backend sends:
{ "cone_id": "...", "head": { "node_id": "...", "tree_id": "..." } }

// TypeScript expects:
{ "coneId": "...", "head": { "nodeId": "...", "treeId": "..." } }
```

## Architecture Principle

**Library representation varies by language idioms, but RPC wire format must be consistent.**

- **TypeScript APIs**: Use `camelCase` (idiomatic for TypeScript/JavaScript)
- **Rust APIs**: Use `snake_case` (idiomatic for Rust)
- **RPC wire protocol**: Always use `snake_case` (Plexus convention)
- **Transformation**: Happens automatically at RPC boundary

This matches how Rust's serde handles naming conventions across serialization boundaries.

## Implementation

### Part 1: Request Parameter Transformation

**File**: `service-codegen/src/generator/namespaces.rs`
**Function**: `generate_params_object()`

**Before** (used JavaScript shorthand):
```rust
fn generate_params_object(params: &[ParamDef]) -> String {
    let fields: Vec<String> = params
        .iter()
        .map(|p| {
            let name = to_camel(&p.pd_name);  // ❌ camelCase only
            name                               // ❌ Shorthand syntax
        })
        .collect();
    format!("{{ {} }}", fields.join(", "))
}
```

Generated: `{ modelId, systemPrompt }` ❌

**After** (explicit property mapping):
```rust
fn generate_params_object(params: &[ParamDef]) -> String {
    let fields: Vec<String> = params
        .iter()
        .map(|p| {
            let camel_name = to_camel(&p.pd_name);    // TypeScript variable
            let snake_name = &p.pd_name;               // RPC wire format
            format!("{}: {}", snake_name, camel_name)  // ✓ Explicit mapping
        })
        .collect();
    format!("{{ {} }}", fields.join(", "))
}
```

Generated: `{ model_id: modelId, system_prompt: systemPrompt }` ✓

### Part 2: Response Field Transformation

**File**: `service-codegen/src/generator/rpc.rs`
**Functions**: Added `toCamelCase()`, `transformKeys()`, modified `extractData()` and `collectOne()`

**Implementation**:
```typescript
/**
 * Convert snake_case string to camelCase.
 */
function toCamelCase(str: string): string {
  return str.replace(/_([a-z])/g, (_, letter) => letter.toUpperCase());
}

/**
 * Recursively transform all object keys from snake_case to camelCase.
 * Similar to Rust's #[serde(rename_all = "camelCase")].
 */
function transformKeys(obj: unknown): unknown {
  if (obj === null || obj === undefined) return obj;
  if (typeof obj !== 'object') return obj;
  if (Array.isArray(obj)) return obj.map(transformKeys);

  const result: Record<string, unknown> = {};
  for (const [key, value] of Object.entries(obj)) {
    const camelKey = toCamelCase(key);
    result[camelKey] = transformKeys(value);  // Recursive
  }
  return result;
}
```

**Integration in data extraction**:
```typescript
export async function* extractData<T>(
  stream: AsyncGenerator<PlexusStreamItem>
): AsyncGenerator<T> {
  for await (const item of stream) {
    switch (item.type) {
      case 'data':
        yield transformKeys(item.content) as T;  // ✓ Transform here
        break;
      // ...
    }
  }
}

export async function collectOne<T>(
  stream: AsyncGenerator<PlexusStreamItem>
): Promise<T> {
  for await (const item of stream) {
    switch (item.type) {
      case 'data':
        return transformKeys(item.content) as T;  // ✓ Transform here
      // ...
    }
  }
  throw new Error('No data received from method call');
}
```

## Data Flow

```
┌─────────────────────────────────────────────────────────────┐
│                     TypeScript Client                        │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  cone.create(modelId, systemPrompt)                         │
│       ↓ (Function parameters: camelCase)                    │
│  Generated RPC call:                                         │
│  {                                                           │
│    model_id: modelId,        ← Explicit mapping             │
│    system_prompt: systemPrompt                              │
│  }                                                           │
│       ↓ (Wire format: snake_case)                           │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       │ JSON-RPC 2.0 over WebSocket
                       │
┌──────────────────────▼──────────────────────────────────────┐
│                    Plexus Backend (Rust)                     │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Receives: { model_id: "...", system_prompt: "..." }       │
│  Processes with snake_case internally                       │
│  Sends: { cone_id: "...", head: { node_id: "..." } }       │
│       ↑ (Wire format: snake_case)                           │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       │ JSON-RPC 2.0 over WebSocket
                       │
┌──────────────────────▼──────────────────────────────────────┐
│                     TypeScript Client                        │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  Receives: { cone_id: "...", head: { node_id: "..." } }    │
│       ↓                                                      │
│  transformKeys() recursively transforms:                    │
│       ↓                                                      │
│  Result: { coneId: "...", head: { nodeId: "..." } }        │
│       ↓ (TypeScript types: camelCase)                       │
│  TypeScript code sees idiomatic camelCase                   │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Benefits

1. **Zero runtime overhead for requests** - Transformation happens at codegen time
2. **Minimal runtime overhead for responses** - Simple regex-based key transformation
3. **Type-safe** - TypeScript types remain fully typed with camelCase
4. **Idiomatic** - Each language uses its own conventions
5. **Automatic** - Works for all current and future activations
6. **Debuggable** - Generated code is readable, no magic
7. **Protocol-stable** - Wire format never changes

## Validation

All validation checks pass:
- ✅ IR generation: 101 types, 164 methods
- ✅ TypeScript generation: 346 files
- ✅ TypeScript compilation: No errors
- ✅ Type checking: All examples type-safe
- ✅ Runtime testing: Agent examples work end-to-end

## Secondary Fix: Type System Restoration

**Problem**: Agent example code reinvented `ConeIdentifier` type incorrectly:

```typescript
// ❌ In src/agent/types.ts (hand-written, wrong)
interface AgentConfig {
  coneId: string | { name: string };  // Ambiguous!
}

// In executor - assumed string = name, but got UUID
const identifier = typeof this.config.coneId === 'string'
  ? { type: 'by_name', name: this.config.coneId }  // ❌ Wrong!
  : { type: 'by_name', name: this.config.coneId.name };
```

**Solution**: Use the properly generated type:

```typescript
// ✓ Use generated discriminated union
import type { ConeIdentifier } from '@plexus/client/cone';

interface AgentConfig {
  coneId: ConeIdentifier;  // ✓ Proper type!
}

// No conversion needed - already correct type
const identifier = this.config.coneId;
```

**Lesson**: Always use generated types from codegen. Don't reinvent them.

## Files Modified

### Code Changes

**service-codegen** (Rust):
- `src/generator/namespaces.rs` - Request parameter mapping
- `src/generator/rpc.rs` - Response field transformation

**substrate-sandbox-ts** (TypeScript):
- `lib/*` - Regenerated with fixes (346 files)
- `src/agent/types.ts` - Use proper `ConeIdentifier` type
- `src/agent/executor.ts` - Remove broken conversion logic
- `src/agent-example.ts` - Create proper identifiers
- `src/agent-simple-example.ts` - Create proper identifiers
- `src/test-response-debug.ts` - Fix client initialization

### Documentation

**synapse**:
- `docs/architecture/16677764457979385599_typescript-naming-convention-fix.md` - This document
- `docs/architecture/16677786647750223615_typescript-codegen-root-cause.md` - Root cause analysis
- `docs/architecture/16677793986132070655_typescript-codegen-snake-case.md` - Original bug report

## Testing Strategy

**Validation script**: `synapse/test/validate_codegen.sh`

Automated checks:
1. Haskell tests (synapse)
2. Rust tests (service-codegen)
3. IR generation and validation
4. TypeScript generation
5. Import structure validation
6. TypeScript compilation
7. Smoke tests

**Manual verification**:
```bash
# Test response transformation
cd substrate-sandbox-ts
npx tsx src/test-response-debug.ts
# Should show: { coneId: "...", head: { nodeId: "...", treeId: "..." } }

# Test agent examples
npm run agent:simple
# Should successfully create cone and chat
```

## Comparison to Alternatives

| Approach | Location | Cost | Type Safety | Idiomatic |
|----------|----------|------|-------------|-----------|
| **1. Keep snake_case in TS** | Codegen | Zero runtime | ✓ | ❌ |
| **2a. Runtime transform (requests only)** | RPC client | Medium | ✓ | ✓ |
| **2b. Codegen + runtime (implemented)** | Both | Low | ✓✓ | ✓✓ |
| **3. Manual mappings everywhere** | Codegen | Zero runtime | ✓ | ✓ |

**Our solution (2b)** combines the best of all approaches:
- Requests: Zero-cost transformation at codegen time (explicit mapping)
- Responses: Minimal-cost transformation at runtime (simple regex)
- Perfect type safety and idiomatic APIs

## Future Considerations

### Edge Cases Handled

1. **Nested objects** - `transformKeys()` is recursive
2. **Arrays** - Transformation preserves arrays and transforms elements
3. **Null/undefined** - Handled gracefully
4. **Single-word fields** - Identity mapping (no transformation needed)
5. **Already camelCase** - No-op (transformation is idempotent)

### Edge Cases Not Handled (Acceptable)

1. **Acronyms** - `model_id` → `modelId` (not `modelID`)
   - Acceptable: TypeScript convention is camelCase throughout
2. **Mixed conventions in same field** - Not expected in Plexus
3. **Performance on huge objects** - Transformation is O(n) where n = fields
   - Acceptable: Plexus messages are reasonably sized

### Potential Optimizations

If profiling shows transformation overhead:
1. Memoize common field names
2. Generate field-specific transformation code
3. Use a faster key transformation algorithm

Current implementation prioritizes correctness and simplicity over micro-optimizations.

## Related Documents

- `16677793986132070655_typescript-codegen-snake-case.md` - Original bug report
- `16677786647750223615_typescript-codegen-root-cause.md` - Root cause analysis
- `16679314737030628607_ir-codegen-chain.md` - Codegen pipeline architecture
- `16680785784679922687_client-codegen-requirements.md` - Client generation requirements
- `testing-strategy-codegen.md` - Testing approach

## Implementation Timeline

- **2026-01-20**: Root cause identified
- **2026-01-21**: Implementation completed
- **2026-01-21**: Validation passed, agent examples working
- **2026-01-21**: Documentation completed

## Acknowledgments

This fix implements the correct architectural principle: **language-idiomatic representations with consistent wire protocol**. The transformation approach mirrors Rust's serde rename functionality, providing a familiar and proven pattern for handling naming convention differences across serialization boundaries.
