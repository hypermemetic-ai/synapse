# TypeScript Codegen Root Cause Analysis

**Status**: Root cause identified
**Date**: 2026-01-20
**Severity**: High - Blocks TypeScript clients
**Related**: `16677793986132070655_typescript-codegen-snake-case.md`

## Executive Summary

TypeScript clients fail because parameter names are sent as `camelCase` over RPC, but Plexus expects `snake_case`.

**Root cause**: `service-codegen/src/generator/namespaces.rs:295` uses JavaScript shorthand property syntax `{ modelId }` instead of explicit mapping `{ model_id: modelId }`.

## Architecture Principle

The correct approach to this problem:

> Library representation should vary by language (TypeScript = camelCase, Rust = snake_case),
> BUT the RPC wire format should always use one consistent representation (snake_case).

This principle is correct because:
- **Language idioms matter** - TypeScript devs expect camelCase
- **Wire protocol must be stable** - Plexus backend expects snake_case
- **Transformation happens at the boundary** - RPC layer bridges the two conventions

## Pipeline Flow

```
Rust Code                TypeScript Code              RPC Wire
─────────────────────    ──────────────────────       ─────────────────
model_id: String    →    modelId: string        →     { model_id: "..." }
system_prompt: Opt  →    systemPrompt?: string  →     { system_prompt: "..." }
```

**Currently broken at**: Step 2 → 3 (TypeScript to RPC wire)

## Code Analysis

### Bug Location

**File**: `service-codegen/src/generator/namespaces.rs`
**Function**: `generate_params_object()` (lines 284-302)

```rust
fn generate_params_object(params: &[ParamDef]) -> String {
    let fields: Vec<String> = params
        .iter()
        .map(|p| {
            let name = to_camel(&p.pd_name);  // ❌ Converts to camelCase
            name                               // ❌ Uses shorthand syntax
        })
        .collect();

    format!("{{ {} }}", fields.join(", "))
}
```

**Generated TypeScript** (wrong):
```typescript
async create(modelId: string, name: string, systemPrompt?: string): Promise<CreateResult> {
  const stream = this.rpc.call('cone.create', { modelId, name, systemPrompt });
  //                                            ^^^^^^^^ ^^^^ ^^^^^^^^^^^^
  //                                            camelCase names sent over wire!
  return collectOne<CreateResult>(stream);
}
```

**What RPC receives** (line 157):
```javascript
{ modelId: "...", systemPrompt: "..." }  // ❌ Backend doesn't understand these keys
```

**What Plexus expects**:
```javascript
{ model_id: "...", system_prompt: "..." }  // ✓ snake_case
```

### The Fix

Change `generate_params_object()` to use **explicit property mapping**:

```rust
fn generate_params_object(params: &[ParamDef]) -> String {
    if params.is_empty() {
        return "{}".to_string();
    }

    let fields: Vec<String> = params
        .iter()
        .map(|p| {
            let camel_name = to_camel(&p.pd_name);    // TypeScript variable (camelCase)
            let snake_name = &p.pd_name;               // RPC wire format (snake_case)

            // Use explicit mapping: { snake_case: camelCase }
            format!("{}: {}", snake_name, camel_name)
        })
        .collect();

    format!("{{ {} }}", fields.join(", "))
}
```

**Generated TypeScript** (fixed):
```typescript
async create(modelId: string, name: string, systemPrompt?: string): Promise<CreateResult> {
  const stream = this.rpc.call('cone.create', {
    model_id: modelId,           // ✓ Explicit mapping
    name: name,                  // ✓ Works for both conventions
    system_prompt: systemPrompt  // ✓ Explicit mapping
  });
  return collectOne<CreateResult>(stream);
}
```

**What RPC receives** (correct):
```javascript
{ model_id: "...", name: "...", system_prompt: "..." }  // ✓ snake_case
```

## Why This Approach is Correct

This implements **Option 2** from the original bug doc ("Transform in RPC layer"), but does it at **codegen time** rather than runtime:

### Advantages
1. **Zero runtime cost** - No dynamic transformation needed
2. **Type-safe** - TypeScript knows exactly what's happening
3. **Debuggable** - Generated code is readable, no magic
4. **Language-idiomatic** - TypeScript APIs use camelCase
5. **Protocol-stable** - Wire format stays snake_case
6. **Works everywhere** - All generated clients fixed automatically

### Comparison to Alternatives

| Approach | Location | When | Cost |
|----------|----------|------|------|
| **Option 1: Keep snake_case** | Codegen | Compile | Non-idiomatic TS |
| **Option 2a: Runtime transform** | RPC client | Runtime | CPU + debugging complexity |
| **Option 2b: Codegen explicit mapping** | Codegen | Compile | **Zero runtime cost** ✓ |
| **Option 3: Manual mappings** | Codegen | Compile | More verbose, same result |

**Option 2b (codegen explicit mapping)** is the best solution.

## Testing

After fix, this should work:

```typescript
import { createConeClient } from '@plexus/cone';
import { createRpcClient } from '@plexus/transport';

const rpc = createRpcClient('ws://localhost:4444');
const cone = createConeClient(rpc);

// This call should succeed:
const result = await cone.create(
  'claude-sonnet-4-5-20250929',  // modelId  (camelCase in TS)
  'test-cone',                    // name
  {},                             // metadata
  'You are a test assistant'      // systemPrompt (camelCase in TS)
);

// Backend receives:
// { model_id: "...", name: "...", metadata: {}, system_prompt: "..." }
//   ^^^^^^^^                                    ^^^^^^^^^^^^^
//   snake_case over the wire!
```

## Files to Modify

1. **service-codegen/src/generator/namespaces.rs**
   - Line 284-302: Update `generate_params_object()`
   - Change from shorthand to explicit property mapping

2. **Test the fix**:
   ```bash
   # Regenerate TypeScript clients
   synapse plexus -i | cargo run --manifest-path ../service-codegen/Cargo.toml -- -o /tmp/test-client

   # Verify generated code has explicit mappings
   cat /tmp/test-client/cone/client.ts | grep "model_id:"

   # Should see: model_id: modelId
   ```

## Related Documents

- `16677793986132070655_typescript-codegen-snake-case.md` - Original bug report
- `16679314737030628607_ir-codegen-chain.md` - Codegen pipeline architecture
- `16680785784679922687_client-codegen-requirements.md` - Client generation requirements

## Implementation Notes

- This is a **one-line change** conceptually, but affects all generated clients
- All TypeScript function signatures stay the same (camelCase parameters)
- Only the RPC call object literal changes
- Backwards compatible: doesn't affect Haskell or other clients
- Forward compatible: works for all future activations automatically
