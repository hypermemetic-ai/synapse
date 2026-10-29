# JSON-RPC Library Comparison for Plugin System

**Date**: 2025-12-07
**Status**: Recommendation

## Executive Summary

**Recommendation**: Use `jsonrpsee` for the new plugin-based context management system.

`jsonrpsee` is the modern, actively maintained successor to `jsonrpc-core`, with native async/await support, WebSocket subscriptions, and powerful proc macros that eliminate boilerplate while maintaining full type safety.

## Library Comparison

### jsonrpc-core + jsonrpc-derive

**Current Status**: Currently used in the project

| Aspect | Rating | Notes |
|--------|--------|-------|
| Maintenance | ⚠️ | No longer actively maintained |
| Downloads | ✅ | 68k+/month - stable and proven |
| Async Support | ⚠️ | Added later, not native |
| Streaming | ❌ | No native subscription support |
| Type Safety | ✅ | Good with `jsonrpc-derive` |
| Proc Macros | ✅ | `#[rpc]` trait macro available |
| Documentation | ✅ | Well documented |
| Ecosystem | ⚠️ | Legacy, moving to jsonrpsee |

**Pros**:
- Already in use
- Stable and battle-tested
- Good trait-based API with `#[rpc]` macro
- No migration needed for existing code

**Cons**:
- Not actively maintained
- No native WebSocket subscription support
- Async support is retrofitted, not native
- Community moving to jsonrpsee

### jsonrpsee

**Current Status**: Actively maintained successor by ParityTech

| Aspect | Rating | Notes |
|--------|--------|-------|
| Maintenance | ✅ | Active development |
| Adoption | ✅ | Used by Polkadot, Substrate, zkSync |
| Async Support | ✅ | Built for async/await from ground up |
| Streaming | ✅ | Native WebSocket subscriptions |
| Type Safety | ✅ | Excellent with proc macros |
| Proc Macros | ✅ | Powerful `#[rpc(server, client)]` |
| Documentation | ✅ | Comprehensive |
| Ecosystem | ✅ | Growing, modern |

**Pros**:
- Modern async/await design
- Native WebSocket subscription support
- Generates both server AND client code
- Better performance
- Active maintenance and security updates
- Large production deployments
- Built-in support for our streaming use case

**Cons**:
- Requires migration from current `jsonrpc-core`
- Slightly different API patterns

## Feature Comparison

### Method Definition

**jsonrpc-derive**:
```rust
#[rpc(server)]
pub trait MyRpc {
    #[rpc(name = "foo")]
    fn foo(&self, a: u64) -> Result<u64>;
}
```

**jsonrpsee**:
```rust
#[rpc(server, namespace = "my")]
pub trait MyRpc {
    #[method(name = "foo")]
    async fn foo(&self, a: u64) -> Result<u64, Error>;
}
```

**Winner**: `jsonrpsee` - cleaner async/await, namespace support

### Subscriptions (Streaming)

**jsonrpc-derive**:
```rust
// Subscriptions supported but complex setup
#[rpc(server)]
pub trait MyRpc {
    #[pubsub(subscription = "hello", subscribe, name = "subscribe_hello")]
    fn subscribe(&self, _: Self::Metadata, _: Subscriber<String>, _: u64);

    #[pubsub(subscription = "hello", unsubscribe, name = "unsubscribe_hello")]
    fn unsubscribe(&self, _: Option<Self::Metadata>, _: SubscriptionId) -> Result<bool>;
}
```

**jsonrpsee**:
```rust
#[rpc(server)]
pub trait MyRpc {
    #[subscription(name = "subscribe_hello", item = String)]
    async fn subscribe_hello(&self, val: u64) -> SubscriptionResult;
}
```

**Winner**: `jsonrpsee` - Much simpler, one method instead of two

### Client Generation

**jsonrpc-derive**:
```rust
// No automatic client generation
// Need separate client implementation
```

**jsonrpsee**:
```rust
#[rpc(server, client)] // <-- Add `client` to generate client code
pub trait MyRpc {
    #[method(name = "foo")]
    async fn foo(&self, a: u64) -> Result<u64>;
}

// Automatically generates:
// - MyRpcServer (for server implementation)
// - MyRpcClient (for client calls)
```

**Winner**: `jsonrpsee` - Automatic client generation

## Plugin System Integration

### How jsonrpsee Fits Our Architecture

Our requirements:
1. ✅ **Star routing (one router, many spokes)**: Each plugin is an RPC trait
2. ✅ **Type-safe methods**: Proc macros ensure compile-time safety
3. ✅ **Streaming support**: Native subscriptions for our stream-based design
4. ✅ **Plugin path tracking**: We add this as a wrapper around subscription items
5. ✅ **Nested plugin calls**: Client generation enables inter-plugin communication

### Example: Plugin Definition with jsonrpsee

```rust
use jsonrpsee::proc_macros::rpc;
use jsonrpsee::core::async_trait;

// Step 1: Define the plugin's RPC interface
#[rpc(server, client, namespace = "agent")]
pub trait AgentPlugin {
    /// Non-streaming method
    #[method(name = "list")]
    async fn list_agents(&self) -> Result<Vec<Agent>, Error>;

    /// Streaming method via subscription
    #[subscription(name = "chat", item = TrackedStreamItem)]
    async fn chat(&self, agent_id: String, message: String) -> SubscriptionResult;
}

// Step 2: Implement the server trait
pub struct AgentPluginImpl {
    ctx: Arc<PluginContext>,
}

#[async_trait]
impl AgentPluginServer for AgentPluginImpl {
    async fn list_agents(&self) -> Result<Vec<Agent>, Error> {
        self.ctx.db().query_agents().await
    }

    async fn chat(&self, pending: PendingSubscription, agent_id: String, message: String)
        -> SubscriptionResult
    {
        let sink = pending.accept().await?;
        let path = PluginPath::root("agent");

        // Our streaming logic with path tracking
        tokio::spawn(async move {
            let stream = generate_chat_stream(agent_id, message, path);
            pin_mut!(stream);

            while let Some(item) = stream.next().await {
                if sink.send(&item).await.is_err() {
                    break; // Client disconnected
                }
            }
        });

        Ok(())
    }
}

// Step 3: Use the generated client for inter-plugin calls
async fn call_mcp_plugin(client: &impl McpPluginClient, tool: String) -> Result<ToolResult> {
    // Type-safe, generated client method
    client.execute_tool(tool).await
}
```

### Path Tracking Integration

We wrap subscription items with our path tracking:

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackedStreamItem {
    pub plugin_path: PluginPath,
    pub item: ServiceStreamItem,
}

// Plugin implementations yield TrackedStreamItem
// jsonrpsee handles serialization/deserialization
// Client receives properly typed items with full path tracking
```

## Migration Strategy

### Phase 1: Add jsonrpsee (Parallel Installation)
```toml
[dependencies]
# Keep existing
jsonrpc-core = "18.0"
jsonrpc-stdio-server = "18.0"

# Add new
jsonrpsee = { version = "0.21", features = ["server", "macros", "ws-client"] }
```

### Phase 2: Create New Plugin System
- Build new plugin infrastructure using `jsonrpsee`
- Keep existing RPC handlers on `jsonrpc-core`
- Both systems run in parallel

### Phase 3: Migrate Plugins One-by-One
- Convert each plugin to `jsonrpsee` trait
- Test thoroughly
- Deprecate old implementation

### Phase 4: Remove Legacy Code
- Once all plugins migrated, remove `jsonrpc-core`
- Clean up old infrastructure

## Recommendation Summary

**Use `jsonrpsee` for the new plugin system because**:

1. **Native Streaming**: Subscriptions are first-class, perfect for our stream-based architecture
2. **Modern Async**: Built for async/await, not retrofitted
3. **Active Maintenance**: Security updates, bug fixes, new features
4. **Production Proven**: Used by major blockchain projects with high reliability requirements
5. **Client Generation**: Automatic client code enables type-safe inter-plugin calls
6. **Better DX**: Cleaner APIs, better error messages, simpler subscription handling
7. **Future-Proof**: Active ecosystem, growing adoption

**Migration is worth it because**:
- Plugin system is new - no legacy code to convert
- Subscriptions solve our streaming requirements elegantly
- Type-safe client generation enables nested plugin calls
- Better foundation for future features

## References

- [jsonrpsee GitHub](https://github.com/paritytech/jsonrpsee)
- [jsonrpsee Documentation](https://docs.rs/jsonrpsee)
- [jsonrpsee Proc Macros](https://docs.rs/jsonrpsee-proc-macros)
- [jsonrpc-derive](https://lib.rs/crates/jsonrpc-derive) (legacy)
- [jsonrpc-core](https://lib.rs/crates/jsonrpc-core) (current)

## Decision

**Status**: ✅ Recommended

Use `jsonrpsee` for the new plugin-based context management system. Keep `jsonrpc-core` for existing code during transition, then migrate gradually.
