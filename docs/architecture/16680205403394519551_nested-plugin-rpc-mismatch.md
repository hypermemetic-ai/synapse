# Nested Plugin RPC Registration Mismatch

## Issue

The recursive schema advertises methods on nested plugins, but these methods are not registered as RPC endpoints.

## Example

Schema shows mercury (child of solar) with an `info` method:

```json
{
  "namespace": "solar",
  "methods": [{"name": "observe"}, {"name": "info"}],
  "children": [{
    "namespace": "mercury",
    "methods": [{"name": "info"}]  // <-- advertised but not callable
  }]
}
```

## What Works

```bash
# Parent methods work
solar_info with {"path": "mercury"}  ✓

# Top-level plugin methods work
echo_once  ✓
health_check  ✓
```

## What Doesn't Work

```bash
mercury_info          # Method not found
solar_mercury_info    # Method not found
solar.mercury.info    # Method not found
```

## The Problem

The CLI recursively builds a parser from the schema. When the user navigates to `synapse solar mercury info`, we need to invoke an RPC method. But what method?

Current attempts:
- `mercury_info` - not registered
- `solar_mercury_info` - not registered

The schema implies mercury has callable methods, but they're not actually exposed as RPC endpoints.

## Questions for Substrate

1. **Are nested plugin methods intentionally not registered?**
   - If so, should they be removed from the schema?
   - Or should the schema include a flag like `"callable": false`?

2. **Should nested methods be registered with a path prefix?**
   - e.g., `solar_mercury_info` or `solar.mercury.info`
   - This would match the CLI navigation model

3. **Is the intended pattern to use parent methods with path params?**
   - e.g., `solar_info --path mercury` instead of `mercury info`
   - If so, the schema should not advertise methods on children

## Proposed Solutions

### Option A: Register Nested Methods
Register child plugin methods with full path: `solar_mercury_info`

Pros: CLI navigation works naturally
Cons: More methods to register, potential naming conflicts

### Option B: Remove Methods from Nested Schema
Only advertise methods that are actually callable.

Pros: Schema matches reality
Cons: Loses documentation value of showing what's available

### Option C: Add Callability Metadata
Add a flag to indicate whether a method is directly callable:

```json
{
  "namespace": "mercury",
  "methods": [{
    "name": "info",
    "callable": false,  // or "via": "solar_info"
    "description": "Use solar info --path mercury"
  }]
}
```

Pros: Schema is self-documenting
Cons: More complexity

### Option D: Route Through Plexus
Use `substrate.call` with the full path:

```json
{"method": "substrate.call", "params": {"method": "solar.mercury.info", "params": {}}}
```

Pros: Single routing point
Cons: Requires plexus to understand nested paths
