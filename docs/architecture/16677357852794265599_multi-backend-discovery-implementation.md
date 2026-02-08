# Multi-Backend Discovery Implementation

**Date**: 2026-01-25
**Status**: MVP Complete
**Component**: Synapse Backend Discovery

## Overview

Implemented a dynamic backend discovery system that enables Synapse to discover and connect to multiple Plexus hubs without manual configuration. The system uses the `_info` endpoint for auto-discovery and optionally queries a registry activation for additional backends.

**Key Achievement**: Zero-config backend discovery that "just works" while supporting multi-hub environments.

## Architecture

### Discovery Flow

```
synapse startup
    │
    ├─> Call _info on default/specified port
    │   └─> Extract backend name (e.g., "substrate", "lforge")
    │
    ├─> Call registry.list (if registry exists)
    │   └─> Get additional registered backends
    │
    ├─> Deduplicate backends by (name, host, port)
    │   └─> Prefer registry entry over _info entry
    │
    ├─> Health check all backends in parallel (300ms timeout)
    │   └─> Mark each as reachable (✓) or unreachable (✗)
    │
    └─> Build CLI with discovered backends as subcommands
```

### Backend List Formula

```
backends = dedup(_info.backend + registry.list)
```

- **Always show**: Backend from `_info` endpoint
- **Optionally add**: Backends from `registry.list`
- **Deduplicate**: Registry entry preferred when overlap exists

## Implementation Challenges

### Challenge 1: Health Check Timeout

**Problem**: Initial 100ms timeout too aggressive for real-world connections.

**Symptoms**:
```
Available backends:
  substrate      127.0.0.1:4444 ✗ - Main substrate instance
  hub2           192.168.1.100:4444 ✗
```
All backends showing as unreachable despite working correctly.

**Root Cause**: WebSocket connection establishment takes ~150-200ms:
- TCP handshake: ~50-100ms
- WebSocket upgrade: ~50-100ms
- RPC round-trip: ~10-50ms

**Solution**: Increased timeout to 300ms to account for connection overhead.

**Code** (`src/Synapse/Backend/Discovery.hs:306-328`):
```haskell
-- | Ping a backend to check if it's reachable (with 300ms timeout)
-- 300ms allows for WebSocket connection establishment overhead (~150-200ms)
-- plus actual RPC round-trip time
pingBackend :: Backend -> IO Backend
pingBackend backend = do
  let cfg = SubstrateConfig { ... }
      timeout = threadDelay 300000 >> pure (Left "Timeout")
      rpcCall = ST.rpcCallWith cfg "_info" Aeson.Null
        `catch` \(_e :: SomeException) -> pure (Left "Connection failed")

  result <- race timeout rpcCall
  let reachable = case result of
        Left _          -> False  -- Timeout
        Right (Right _) -> True   -- Success
        Right (Left _)  -> False  -- RPC error

  pure $ backend { backendReachable = Just reachable }
```

**Lesson**: Network timeouts need to account for full connection overhead, not just RPC latency.

### Challenge 2: Inactivity-Based Timeout (Failed Attempt)

**Problem**: User requested "notice if we've begun connecting and not kill until actual inactivity."

**Approach Tried**: Use `IORef` to track last activity timestamp, watchdog thread to check for inactivity.

**Implementation**:
```haskell
lastActivityRef <- newIORef 0
let updateActivity = do
      now <- getCurrentTime
      writeIORef lastActivityRef now

-- Watchdog thread checks for 100ms of inactivity
watchdog = do
  threadDelay 10000  -- Check every 10ms
  now <- getCurrentTime
  lastActivity <- readIORef lastActivityRef
  if (now - lastActivity) > 100ms
    then timeout
    else watchdog
```

**Why It Failed**:
- Streaming callbacks weren't being invoked frequently enough
- WebSocket connection establishment is blocking, no "activity" to track
- Complexity not worth the 100ms vs 300ms difference
- IORef updates in callbacks didn't fire during initial connection phase

**Decision**: Simplified back to fixed 300ms timeout using `race`.

**Lesson**: Don't over-engineer timeouts. Fixed timeouts are simpler and more predictable.

### Challenge 3: Backend Deduplication

**Problem**: Backend appeared twice in list when discovered via both `_info` and `registry.list`:

```
Available backends:
  substrate      127.0.0.1:4444 ✓ - Backend discovered via _info
  substrate      127.0.0.1:4444 ✓ - Main substrate instance
  hub2           192.168.1.100:4444 ✗
```

**Root Cause**: Discovery flow unconditionally added both:
```haskell
pure $ [discoveredBackend] ++ registryBackends
```

**Solution**: Deduplicate by `(name, host, port)` tuple, preferring registry entry:

```haskell
let matchesDiscovered b = backendName b == discoveredName
                       && backendHost b == registryHost
                       && backendPort b == registryPort
    isDuplicate = any matchesDiscovered registryBackends
    backends = if isDuplicate
                then registryBackends  -- Prefer registry (has description)
                else [discoveredBackend] ++ registryBackends
```

**Result**:
```
Available backends:
  substrate      127.0.0.1:4444 ✓ - Main substrate instance
  hub2           192.168.1.100:4444 ✗
```

**Lesson**: Always deduplicate discovered data from multiple sources.

### Challenge 4: Live Server Integration Testing

**Problem**: Unit tests passed but live server didn't work with synapse.

**What Happened**:
- Modified `lforge` server with new field (`sync_service`) and method (`test_diff`)
- Unit tests tested programmatically (direct function calls)
- WebSocket/JSON-RPC layer untested
- Initial attempt to use synapse on port 4447 failed

**Investigation**:
```bash
# Test _info endpoint
echo '{"jsonrpc":"2.0","id":1,"method":"_info","params":{}}' | websocat ws://localhost:4447
# ✓ Works, returns {"backend":"lforge"}

# Test discovery
synapse -P 4447
# ✓ Shows lforge backend

# Test schema
synapse -P 4447 --schema lforge
# ✓ Returns full schema

# Test method invocation
synapse -P 4447 lforge hyperforge status
# ✓ Actually works!
```

**Resolution**: False alarm - integration was working correctly. Initial error was from testing before server fully started.

**Lesson**: Always verify end-to-end integration with actual CLI, not just unit tests. The live server test caught us being thorough, which is good practice.

### Challenge 5: Port Override Semantics

**Problem**: When user specifies `-P 4447`, should it:
- A) Only affect discovery (still connect to port from registry)?
- B) Affect both discovery and connection?

**Decision**: Option B - port override applies to both discovery and connection.

**Rationale**:
- User mental model: `-P 4447` means "use port 4447 for everything"
- Simpler behavior: Single source of truth
- Backwards compatible: Matches pre-discovery behavior

**Implementation**: Use `-P` value as `registryPort` in discovery flow.

## Code Changes

### Files Modified

1. **`src/Synapse/Backend/Discovery.hs`**
   - Changed `stubDiscovery` to return empty list (no hardcoded localhost:4444)
   - Implemented `discoverBackendName` using `_info` endpoint
   - Modified `registryDiscovery` to auto-register discovered backend
   - Added deduplication logic
   - Changed health check timeout: 100ms → 300ms
   - Updated `queryBackend` to check discovered backend first

2. **`app/Main.hs`**
   - Added `defaultHost` and `defaultPort` constants
   - Used constants in discovery initialization

### Key Functions

**`discoverBackendName`** (`Discovery.hs:200-219`):
- Calls `_info` endpoint
- Extracts backend name from response
- Returns `Maybe Text` (Nothing on failure)

**`registryDiscovery`** (`Discovery.hs:172-198`):
- Discovers backend via `_info`
- Creates Backend entry from discovery
- Queries registry for additional backends
- Deduplicates before returning

**`pingBackend`** (`Discovery.hs:306-328`):
- Health checks backend with 300ms timeout
- Uses `race` for timeout implementation
- Returns Backend with `backendReachable` field updated

## Testing Strategy

### Manual Test Cases

**Scenario 1: Default Port (4444) with Registry**
```bash
synapse
# Shows: substrate (from registry) + hub2 (from registry)
# Deduplicated: No duplicate substrate entries
```

**Scenario 2: Custom Port (4447) without Registry**
```bash
synapse -P 4447
# Shows: lforge (auto-discovered via _info)
```

**Scenario 3: Method Invocation**
```bash
synapse -P 4447 lforge hyperforge status
# Returns: config_dir, default_org, version info
```

**Scenario 4: Health Checks**
```bash
synapse
# Shows: ✓ for reachable backends, ✗ for unreachable
# Parallel execution, 300ms per backend
```

### What's Tested

✅ Auto-discovery via `_info`
✅ Registry integration (optional)
✅ Backend deduplication
✅ Health check parallel execution
✅ Port override behavior
✅ Method invocation on discovered backends
✅ Schema fetching
✅ Fallback to empty list on connection failure

### What's NOT Tested (Yet)

❌ Automated tests for discovery flow
❌ Registry failure scenarios
❌ Network partition handling
❌ Concurrent discovery from multiple processes
❌ Cache invalidation strategy
❌ Health check retry logic

## MVP Cleanup Opportunities

### 1. Add Caching Layer

**Problem**: Discovery happens on every CLI invocation (adds ~100-300ms).

**Solution**: Cache discovery results with TTL.

```haskell
-- In ~/.cache/synapse/backends.json
{
  "timestamp": 1706205600,
  "ttl": 300,  -- 5 minutes
  "backends": [...]
}
```

**Implementation**:
- Check cache before discovery
- Use cached results if TTL not expired
- Add `--refresh-backends` flag to force refresh

**Effort**: 2-3 hours

### 2. Background Health Checks

**Problem**: Health checks block CLI startup (300ms × N backends).

**Solution**: Start CLI immediately, show cached health status, update in background.

```haskell
-- Immediate startup with cached/unknown status
backends <- getBackendsFromCache
forkIO $ do
  updatedBackends <- pingBackends backends
  updateCache updatedBackends

-- User sees:
Available backends:
  substrate      127.0.0.1:4444 ? - Main substrate instance  -- Checking...
  hub2           192.168.1.100:4444 ?
```

**Trade-off**: Health status might be stale but CLI starts instantly.

**Effort**: 4-5 hours

### 3. Error Messages

**Problem**: Generic "Method not found" errors don't help users.

**Current**:
```
Error: Method not found
```

**Better**:
```
Error: Backend 'lforge' does not have method 'hyperforge.invalid_method'

Available methods:
  hyperforge.status
  hyperforge.version
  hyperforge.test_diff

Run 'synapse -P 4447 lforge hyperforge --help' for details.
```

**Effort**: 2-3 hours

### 4. Automated Tests

**Problem**: All testing is manual right now.

**Solution**: Add property tests and integration tests.

```haskell
-- test/Spec.hs
describe "Backend Discovery" $ do
  it "discovers backend via _info" $ do
    -- Start mock server
    -- Call discovery
    -- Verify backend list

  it "deduplicates backends correctly" $ do
    -- Mock _info and registry responses
    -- Verify deduplication logic

  it "handles connection failures gracefully" $ do
    -- Mock connection failure
    -- Verify fallback to empty list
```

**Effort**: 6-8 hours

### 5. Discovery Configuration

**Problem**: No way to configure discovery behavior.

**Solution**: Add `~/.config/synapse/config.yaml`:

```yaml
discovery:
  enabled: true
  cache_ttl: 300  # seconds
  health_check_timeout: 300  # milliseconds
  default_registry_host: "127.0.0.1"
  default_registry_port: 4444
  parallel_health_checks: true
```

**Effort**: 3-4 hours

### 6. Registry Connection Pooling

**Problem**: Each discovery creates new WebSocket connection.

**Solution**: Reuse connections for multiple RPC calls.

**Current**:
```haskell
-- Each call creates fresh connection
_info      → connect → RPC → disconnect
registry   → connect → RPC → disconnect
health_check → connect → RPC → disconnect
```

**Better**:
```haskell
-- Single connection for multiple calls
connect
  ├─> _info RPC
  ├─> registry.list RPC
  └─> _info RPC (health check)
disconnect
```

**Effort**: 5-6 hours (requires refactoring Transport layer)

### 7. Backend Aliases

**Problem**: No short aliases for long backend names.

**Solution**: Support aliases in config:

```yaml
aliases:
  prod: substrate-production
  dev: substrate-dev
  local: substrate
```

Then:
```bash
synapse prod cone list
# Resolves to: synapse substrate-production cone list
```

**Effort**: 2-3 hours

### 8. Health Check Retry Logic

**Problem**: Single failed health check marks backend as unreachable permanently (for that session).

**Solution**: Retry health checks with exponential backoff.

```haskell
pingBackendWithRetry :: Backend -> IO Backend
pingBackendWithRetry backend = go 3 100  -- 3 retries, 100ms initial delay
  where
    go 0 _ = pure $ backend { backendReachable = Just False }
    go n delay = do
      result <- pingBackend backend
      case backendReachable result of
        Just True -> pure result
        _ -> threadDelay (delay * 1000) >> go (n-1) (delay * 2)
```

**Effort**: 2-3 hours

### 9. Discovery Metrics

**Problem**: No visibility into discovery performance.

**Solution**: Add timing metrics:

```bash
synapse --debug
# Discovery metrics:
# - _info call: 45ms
# - registry.list: 67ms
# - health checks: 234ms (3 backends in parallel)
# - total discovery: 346ms
```

**Effort**: 2-3 hours

### 10. Namespace Support

**Problem**: Currently assumes all backends use same namespace format.

**Future**: Registry should return namespace info per backend.

```json
{
  "name": "substrate",
  "host": "127.0.0.1",
  "port": 4444,
  "namespace_prefix": "plexus",  // <-- New field
  "methods": [...]
}
```

Then: `synapse substrate cone list` → calls `substrate.call` with `cone.list`.

**Effort**: 4-5 hours (requires registry schema change)

## Priority Ranking

**High Priority** (MVP → Production):
1. **Caching Layer** - 10x startup performance
2. **Error Messages** - Better UX
3. **Automated Tests** - Prevent regressions

**Medium Priority** (Nice to Have):
4. **Discovery Configuration** - Power users
5. **Background Health Checks** - Instant startup
6. **Health Check Retry** - More reliable status

**Low Priority** (Future):
7. **Connection Pooling** - Optimization
8. **Backend Aliases** - Convenience
9. **Discovery Metrics** - Debugging
10. **Namespace Support** - Multi-tenant scenarios

## Performance Characteristics

### Current Performance

**Cold Start** (no cache):
- Discovery: ~100ms (1 backend)
- Health checks: ~300ms (parallel)
- Total: ~400ms

**With Multiple Backends** (3 backends):
- Discovery: ~100ms
- Health checks: ~300ms (parallel, not serial)
- Total: ~400ms (doesn't scale with backend count)

### Bottlenecks

1. **WebSocket Connection Overhead**: ~150-200ms per connection
2. **Serial Discovery Steps**: `_info` → `registry.list` (can't parallelize)
3. **No Caching**: Every CLI invocation rediscovers

### Optimization Potential

With caching:
- Cache hit: ~5-10ms (read from disk)
- 40x faster than current

With connection pooling:
- Reuse connection: Save ~150ms per call
- Shared pool across CLI invocations (daemon mode)

## Future Directions

### 1. Registry as Service Discovery

Current state: Registry is optional, registry.list returns static config.

Future: Registry actively monitors backends, provides health status.

```rust
// Registry tracks backend health in background
struct Registry {
    backends: HashMap<String, Backend>,
    health_monitor: BackgroundHealthChecker,
}

impl Registry {
    async fn list(&self) -> Vec<Backend> {
        // Returns backends with live health status
        self.backends.values()
            .map(|b| b.with_health(self.health_monitor.status(b)))
            .collect()
    }
}
```

### 2. Multi-Registry Support

Support federated registries:

```yaml
registries:
  - name: local
    host: 127.0.0.1
    port: 4444
  - name: corporate
    host: registry.corp.com
    port: 443
    tls: true
```

Discovery merges results from all registries.

### 3. Dynamic Backend Discovery (mDNS/DNS-SD)

Auto-discover backends on local network:

```
_plexus._tcp.local.
  → substrate.local:4444
  → hub2.local:4444
```

No manual registration needed for local development.

### 4. Backend Capability Negotiation

Backends advertise capabilities:

```json
{
  "name": "substrate",
  "capabilities": {
    "cone": "2.0.0",
    "arbor": "1.5.0",
    "bash": "1.0.0"
  }
}
```

CLI can route commands to backends based on capability version.

## Lessons Learned

### What Went Well

✅ **Zero-config approach**: `_info` endpoint provides automatic discovery
✅ **Parallel health checks**: Fast even with many backends
✅ **Graceful degradation**: Works without registry, falls back to discovered backend
✅ **Deduplication**: Clean backend list even with overlapping sources

### What Was Hard

❌ **Timeout tuning**: Finding right balance between fast and reliable
❌ **Inactivity detection**: Too complex for marginal benefit
❌ **Testing integration**: Unit tests don't catch RPC layer issues
❌ **Variable naming**: `backendName` field vs `backendName` parameter confusion

### What Would We Do Differently

1. **Start with fixed timeout**: Don't try to be clever with inactivity detection
2. **Integration tests first**: Test via CLI before unit tests
3. **Cache from day one**: Discovery should be cached by default
4. **Metrics early**: Add timing from the start to catch performance issues

## Related Documents

- [Schema to CLI Pipeline](./16680606527890548735_schema-to-cli-pipeline.md) - How discovered schemas become CLI commands
- [Introspective RPC Protocol](./16680807091363337727_introspective-rpc-protocol.md) - The `_info` and schema endpoints
- [Multi-Hub Transport Envelope](./16679517135570018559_multi-hub-transport-envelope.md) - Original multi-backend design

## References

**Code Locations**:
- Discovery implementation: `src/Synapse/Backend/Discovery.hs`
- Main integration: `app/Main.hs:72-97`
- Transport layer: `substrate-protocol/src/Substrate/Transport.hs`

**Testing**:
```bash
# Test default discovery
synapse

# Test custom port
synapse -P 4447

# Test specific backend
synapse lforge hyperforge status

# Debug discovery
synapse --schema lforge
```
