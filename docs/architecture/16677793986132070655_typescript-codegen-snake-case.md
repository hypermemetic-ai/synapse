# TypeScript Codegen: Parameter Name Convention Mismatch

**Status**: Bug
**Date**: 2025-01-20
**Severity**: High - Blocks TypeScript clients from working

## Problem

Generated TypeScript clients cannot call Plexus methods because of a parameter naming convention mismatch:

- **TypeScript side** (generated): Uses `camelCase` (`modelId`, `systemPrompt`)
- **Plexus RPC side** (expected): Uses `snake_case` (`model_id`, `system_prompt`)

This causes all multi-word parameter calls to fail with `RPC error -32603: Internal error`.

## Example Failure

```typescript
// Generated TypeScript client signature:
async create(
  modelId: string,        // ❌ camelCase
  name: string,
  metadata?: unknown,
  systemPrompt?: string   // ❌ camelCase
): Promise<CreateResult>

// What it sends over RPC:
{ modelId: "...", systemPrompt: "..." }  // ❌ Wrong

// What Plexus expects:
{ model_id: "...", system_prompt: "..." }  // ✓ Correct
```

**Result**: `RPC error -32603: Internal error` from backend

## Affected Components

### All generated TypeScript clients with multi-word parameters:

1. **Cone**: `model_id`, `system_prompt`
2. **Arbor**: `tree_id`, `parent_id`, `node_id`
3. **Claudecode**: `working_dir`, `loopback_enabled`, `system_prompt`
4. **Any activation** with Rust-style snake_case params

### Files affected:

```
substrate-sandbox-ts/lib/
├── cone/client.ts         # create(), chat(), etc.
├── arbor/client.ts        # tree-create(), node-create(), etc.
├── claudecode/client.ts   # create(), chat(), etc.
└── */client.ts            # Any client with multi-word params
```

## Root Cause

The TypeScript codegen pipeline (service-codegen → synapse → TypeScript output) has a naming convention gap:

1. **Rust backend**: Uses `snake_case` (Rust convention)
   ```rust
   struct CreateParams {
       model_id: String,
       system_prompt: Option<String>,
   }
   ```

2. **JSON Schema** (IR): Preserves Rust names as `snake_case`
   ```json
   {
     "properties": {
       "model_id": { "type": "string" },
       "system_prompt": { "type": "string" }
     }
   }
   ```

3. **TypeScript codegen**: Converts to `camelCase` (TypeScript convention)
   ```typescript
   interface CreateParams {
     modelId: string;
     systemPrompt?: string;
   }
   ```

4. **RPC call**: Sends `camelCase` names (WRONG!)
   ```typescript
   this.rpc.call('cone.create', { modelId, systemPrompt })
   //                              ^^^^^^^ Not transformed!
   ```

**The transformation happens but isn't reversed when making the RPC call.**

## Current Workaround

Bypass typed clients and call RPC directly with snake_case:

```typescript
// ❌ FAILS - Typed client with camelCase:
const cone = new Cone.ConeClientImpl(rpc);
await cone.create('claude-sonnet-4-5', 'test', {}, 'prompt');

// ✓ WORKS - Direct RPC with snake_case:
const stream = rpc.call('cone.create', {
  model_id: 'claude-sonnet-4-5',
  name: 'test',
  system_prompt: 'prompt'
});
```

## Proposed Solutions

### Option 1: Keep snake_case everywhere (Simplest)

**Change**: Don't transform to camelCase in codegen

**Pros**:
- Matches backend exactly
- No transformation needed
- No bugs from naming mismatches

**Cons**:
- Non-idiomatic TypeScript (`model_id` looks weird in TS)
- Linter warnings (TypeScript convention is camelCase)

**Example**:
```typescript
// Generated code would be:
async create(
  model_id: string,      // snake_case in TypeScript
  name: string,
  system_prompt?: string
): Promise<CreateResult>
```

### Option 2: Transform camelCase → snake_case in RPC layer (Cleanest)

**Change**: Add automatic name transformation in `rpc.call()`

**Pros**:
- TypeScript APIs are idiomatic (camelCase)
- Backend APIs are idiomatic (snake_case)
- Best developer experience

**Cons**:
- Magic transformation (harder to debug)
- Need to handle edge cases (acronyms, etc.)
- Performance overhead (minimal)

**Implementation**:
```typescript
// In lib/rpc.ts or lib/transport.ts
function transformKeys(obj: Record<string, any>): Record<string, any> {
  const result: Record<string, any> = {};
  for (const [key, value] of Object.entries(obj)) {
    // camelCase → snake_case
    const snakeKey = key.replace(/([A-Z])/g, '_$1').toLowerCase();
    result[snakeKey] = value;
  }
  return result;
}

// In RpcClient.call():
call(method: string, params: any): AsyncIterable<PlexusStreamItem> {
  const transformedParams = transformKeys(params);
  // ... send transformedParams over wire
}
```

### Option 3: Generate both names in client code (Explicit)

**Change**: Codegen creates mappings at call site

**Pros**:
- Explicit (no magic)
- Easy to debug
- TypeScript APIs stay idiomatic

**Cons**:
- More generated code
- Repetitive

**Example**:
```typescript
// Generated:
async create(
  modelId: string,
  name: string,
  systemPrompt?: string
): Promise<CreateResult> {
  const stream = this.rpc.call('cone.create', {
    model_id: modelId,        // Explicit mapping
    name: name,
    system_prompt: systemPrompt
  });
  return collectOne<CreateResult>(stream);
}
```

## Recommendation

**Option 2: Transform in RPC layer**

Why?
1. **Best DX**: TypeScript developers see idiomatic camelCase
2. **Automatic**: Works for all methods without manual mappings
3. **Centralized**: One place to fix (RPC layer)
4. **Future-proof**: New activations work automatically

Implementation location: `lib/rpc.ts` or `lib/transport.ts`

## Testing

After fix, this should work:

```typescript
const cone = new Cone.ConeClientImpl(rpc);

// This call should succeed:
const result = await cone.create(
  'claude-sonnet-4-5-20250929',
  'test-cone',
  {},
  'You are a test assistant'
);

// Backend should receive:
// { model_id: "...", name: "...", system_prompt: "..." }
```

## Related Issues

- TypeScript agent implementation blocked (can't create cones)
- All TypeScript examples in substrate-sandbox-ts broken
- Documentation examples don't work

## Files to Modify

### If choosing Option 2 (recommended):

1. **`lib/rpc.ts`** or **`lib/transport.ts`**:
   - Add `transformKeys()` helper
   - Transform params in `call()` method

2. **Test in `substrate-sandbox-ts`**:
   - Verify typed clients work
   - Run agent examples

### If choosing Option 1:

1. **Codegen source** (service-codegen or synapse):
   - Remove camelCase transformation
   - Keep snake_case in TypeScript output

### If choosing Option 3:

1. **Codegen source**:
   - Generate explicit mappings at call sites
   - Map `modelId` → `model_id` in generated code

## Timeline

**Priority**: High (blocks TypeScript development)

**Effort**:
- Option 1: Low (remove transformation)
- Option 2: Medium (add RPC layer logic)
- Option 3: Medium (update codegen templates)

**Recommendation**: Implement Option 2 in next sprint

## References

- Example failure: `substrate-sandbox-ts/src/agent-example.ts`
- Workaround: `substrate-sandbox-ts/test-cone-direct.ts`
- Bug report: `substrate-sandbox-ts/BUGS.md`
