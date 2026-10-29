# Plugin-Based Context Management Architecture

**Date**: 2025-12-07
**Status**: Design Phase

## Vision

A pluggable agent context management system with external JSON-RPC access, operating as a star-shaped model (one router, many spokes) where plugins manage their own internal context while returning type-safe streaming responses with full path tracking for nested plugin calls.

## Core Architecture

### Star Model (one router, many spokes)

```
                         ┌─────────────────┐
                         │                 │
                         │   JSON-RPC Service  │
                         │                 │
                         └────────┬────────┘
                                  │
                ┌─────────────────┼─────────────────┐
                │                 │                 │
         ┌──────▼──────┐   ┌─────▼──────┐   ┌─────▼──────┐
         │  Plugin A   │   │  Plugin B  │   │  Plugin C  │
         │             │   │            │   │            │
         │  (Context)  │   │ (Context)  │   │ (Context)  │
         └──────┬──────┘   └─────┬──────┘   └─────┬──────┘
                │                 │                 │
                │                 │                 │
         ┌──────▼──────┐   ┌─────▼──────┐   ┌─────▼──────┐
         │   SQLite    │   │  SQLite    │   │  SQLite    │
         └─────────────┘   └────────────┘   └────────────┘
```

**Key Principles**:
- Service receives JSON-RPC requests
- Service routes to appropriate plugin based on method name
- Each plugin has full access to the underlying system context
- Each plugin manages its own internal state (SQLite database)
- Plugins return streams that can be converted to service's stream type
- All streams carry plugin path metadata for nested call tracking

### System Context

The underlying "system" that plugins have access to (details TBD):
- Global configuration
- Shared resources (e.g., database pools, HTTP clients)
- Inter-plugin communication channel
- Authentication/authorization context
- Telemetry and logging infrastructure

## Type-Safe Streaming Interface

### Plugin Trait Definition

```rust
pub trait Plugin: Send + Sync {
    /// Plugin identifier (used in routing)
    fn name(&self) -> &str;

    /// List of JSON-RPC methods this plugin handles
    fn methods(&self) -> Vec<MethodDescriptor>;

    /// Main call interface - always returns a stream
    fn call<'a>(
        &'a self,
        ctx: &'a SystemContext,
        method: &'a str,
        params: serde_json::Value,
        path: PluginPath,
    ) -> Pin<Box<dyn Stream<Item = PluginStreamItem> + Send + 'a>>;
}
```

### Stream Type Hierarchy

```rust
/// Plugin-level stream item (plugin-specific)
pub struct PluginStreamItem {
    /// The actual data (plugin-specific type)
    pub data: Box<dyn StreamItemData>,

    /// Metadata about the source
    pub source: PluginPath,
}

/// Marker trait for plugin stream data
pub trait StreamItemData: Send + Sync {
    fn as_any(&self) -> &dyn Any;
    fn to_service_item(&self) -> ServiceStreamItem;
}

/// Service-level stream item (unified across all plugins)
pub enum ServiceStreamItem {
    /// Progress update
    Progress {
        plugin_path: PluginPath,
        message: String,
        percentage: Option<f32>,
    },

    /// Data chunk
    Data {
        plugin_path: PluginPath,
        content_type: String,
        data: serde_json::Value,
    },

    /// Error occurred
    Error {
        plugin_path: PluginPath,
        error: String,
        recoverable: bool,
    },

    /// Stream complete
    Done {
        plugin_path: PluginPath,
    },
}

impl<T: StreamItemData> From<PluginStreamItem> for ServiceStreamItem {
    fn from(item: PluginStreamItem) -> Self {
        item.data.to_service_item()
    }
}
```

### Plugin Path Tracking

```rust
/// Tracks the path of nested plugin calls
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PluginPath {
    /// Ordered list of plugin names in the call chain
    /// Example: ["agent_db", "mcp_client", "filesystem"]
    segments: Vec<String>,
}

impl PluginPath {
    pub fn root(plugin_name: impl Into<String>) -> Self {
        Self {
            segments: vec![plugin_name.into()],
        }
    }

    pub fn extend(&self, plugin_name: impl Into<String>) -> Self {
        let mut new_path = self.clone();
        new_path.segments.push(plugin_name.into());
        new_path
    }

    pub fn depth(&self) -> usize {
        self.segments.len()
    }

    pub fn to_string(&self) -> String {
        self.segments.join(".")
    }
}
```

## JSON-RPC Method Routing

### Auto-Generated Method Registration

Using proc macros to generate JSON-RPC interfaces:

```rust
/// Plugin methods enum - one per plugin
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[jsonrpc_methods] // <-- Proc macro generates all the boilerplate
pub enum AgentDbMethods {
    #[jsonrpc(name = "agent.create")]
    CreateAgent {
        name: String,
        description: Option<String>,
    },

    #[jsonrpc(name = "agent.list")]
    ListAgents {
        filter: Option<String>,
    },

    #[jsonrpc(name = "agent.delete")]
    DeleteAgent {
        agent_id: String,
    },
}

/// The proc macro generates:
/// - JSON Schema for each method's parameters
/// - Routing logic from method name to enum variant
/// - Type-safe parameter deserialization
/// - Tool descriptors for `tools/list`
```

### Service Routing Logic

```rust
pub struct PluginService {
    plugins: HashMap<String, Arc<dyn Plugin>>,
    context: Arc<SystemContext>,
}

impl PluginService {
    pub async fn handle_request(
        &self,
        method: &str,
        params: serde_json::Value,
    ) -> Result<impl Stream<Item = ServiceStreamItem>, Error> {
        // Parse method name: "plugin_name.method_name"
        let (plugin_name, plugin_method) = method
            .split_once('.')
            .ok_or_else(|| Error::InvalidMethod(method.to_string()))?;

        // Find plugin
        let plugin = self.plugins
            .get(plugin_name)
            .ok_or_else(|| Error::PluginNotFound(plugin_name.to_string()))?;

        // Initialize plugin path
        let path = PluginPath::root(plugin_name);

        // Call plugin and convert stream
        let plugin_stream = plugin.call(&self.context, plugin_method, params, path);

        // Convert PluginStreamItem -> ServiceStreamItem
        let service_stream = plugin_stream.map(|item| item.into());

        Ok(service_stream)
    }
}
```

## Type-Safe RPC Interface Generation

### Leveraging Existing Libraries

Instead of building custom proc macros, we'll leverage **existing JSON-RPC libraries** with proven macro support:

#### Option 1: `jsonrpc-derive` (Current Ecosystem)
- ✅ Already using `jsonrpc-core` in the project
- ✅ Trait-based approach with `#[rpc]` macro
- ⚠️ No longer actively maintained
- ✅ Stable, battle-tested (68k+ downloads/month)

#### Option 2: `jsonrpsee` (Recommended - Modern Successor)
- ✅ Actively maintained by ParityTech
- ✅ Built for async/await from ground up
- ✅ Native WebSocket subscription support
- ✅ Powerful `#[rpc(server, client)]` macro
- ✅ Better performance and ergonomics
- ✅ Used by Polkadot, Substrate, and other major projects

**Recommendation**: Migrate to `jsonrpsee` for the new plugin system.

### Using `jsonrpsee` for Plugin Definition

```rust
use jsonrpsee::proc_macros::rpc;
use jsonrpsee::core::async_trait;

/// Define the plugin's RPC interface
#[rpc(server, namespace = "agent")]
pub trait AgentPlugin {
    /// Create a new agent
    #[method(name = "create")]
    async fn create_agent(&self, name: String, description: Option<String>) -> Result<Agent, Error>;

    /// List all agents
    #[method(name = "list")]
    async fn list_agents(&self, filter: Option<String>) -> Result<Vec<Agent>, Error>;

    /// Chat with an agent (returns subscription for streaming)
    #[subscription(name = "chat", item = ChatStreamItem)]
    async fn chat(&self, agent_id: String, message: String) -> SubscriptionResult;
}

/// Plugin implementation
pub struct AgentPluginImpl {
    ctx: Arc<PluginContext>,
}

#[async_trait]
impl AgentPluginServer for AgentPluginImpl {
    async fn create_agent(&self, name: String, description: Option<String>) -> Result<Agent, Error> {
        // Implementation
    }

    async fn list_agents(&self, filter: Option<String>) -> Result<Vec<Agent>, Error> {
        // Implementation
    }

    async fn chat(&self, pending: PendingSubscription, agent_id: String, message: String)
        -> SubscriptionResult
    {
        let sink = pending.accept().await?;

        // Create stream from plugin logic
        let stream = self.chat_stream_internal(&agent_id, &message).await?;

        // Forward stream items to subscription
        tokio::spawn(async move {
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
```

### What `jsonrpsee` Provides Out of the Box

The `#[rpc]` macro automatically generates:
- ✅ **JSON-RPC method routing**: From method name to implementation
- ✅ **Type-safe parameters**: Compile-time checked deserialization
- ✅ **Server trait**: `AgentPluginServer` with method signatures
- ✅ **Client trait**: For testing and inter-plugin calls
- ✅ **Subscription support**: Native WebSocket streaming
- ✅ **Error handling**: Standard `jsonrpsee::core::Error` types

### Integrating with Plugin Architecture

We'll bridge `jsonrpsee`'s subscription model with our plugin path tracking:

```rust
/// Wrapper for plugin stream items with path tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackedStreamItem {
    /// Plugin path that generated this item
    pub plugin_path: PluginPath,

    /// The actual stream data
    pub item: ServiceStreamItem,
}

impl AgentPluginImpl {
    /// Internal method that produces tracked stream
    async fn chat_stream_internal(
        &self,
        agent_id: &str,
        message: &str,
    ) -> Result<impl Stream<Item = TrackedStreamItem>, Error> {
        let path = PluginPath::root("agent");
        let ctx = self.ctx.clone();

        Ok(stream! {
            yield TrackedStreamItem {
                plugin_path: path.clone(),
                item: ServiceStreamItem::Progress {
                    plugin_path: path.clone(),
                    message: "Starting chat...".to_string(),
                    percentage: None,
                },
            };

            // Generate agent response
            let response = ctx.generate_response(agent_id, message).await?;

            yield TrackedStreamItem {
                plugin_path: path.clone(),
                item: ServiceStreamItem::Data {
                    plugin_path: path.clone(),
                    content_type: "AgentResponse".to_string(),
                    data: serde_json::to_value(response)?,
                },
            };

            yield TrackedStreamItem {
                plugin_path: path.clone(),
                item: ServiceStreamItem::Done {
                    plugin_path: path,
                },
            };
        })
    }
}

## Plugin Context Management

### Per-Plugin SQLite Database

Each plugin gets its own SQLite database for managing internal state:

```rust
pub struct PluginContext {
    /// Plugin-specific SQLite connection pool
    db: SqlitePool,

    /// Plugin identifier
    plugin_name: String,

    /// Reference to system context
    system: Arc<SystemContext>,
}

impl PluginContext {
    pub async fn new(plugin_name: impl Into<String>, system: Arc<SystemContext>) -> Result<Self> {
        let plugin_name = plugin_name.into();
        let db_path = system.config.plugin_db_path(&plugin_name);

        let db = SqlitePool::connect(&db_path).await?;

        // Run plugin-specific migrations
        sqlx::migrate!(&format!("./migrations/{}", plugin_name))
            .run(&db)
            .await?;

        Ok(Self {
            db,
            plugin_name,
            system,
        })
    }

    pub fn db(&self) -> &SqlitePool {
        &self.db
    }

    pub fn system(&self) -> &SystemContext {
        &self.system
    }
}
```

### System Context (Shared Across Plugins)

```rust
pub struct SystemContext {
    /// Global configuration
    pub config: Arc<SystemConfig>,

    /// Inter-plugin message bus
    pub event_bus: Arc<EventBus>,

    /// Shared HTTP client
    pub http_client: reqwest::Client,

    /// Telemetry and logging
    pub telemetry: Arc<TelemetryService>,

    /// Authentication context
    pub auth: Arc<AuthContext>,
}
```

## Nested Plugin Calls

### Example Flow

```
User Request: "agent.chat"
  ↓
Service receives: method="agent.chat", params={...}
  ↓
Service routes to: AgentPlugin
  path = ["agent"]
  ↓
AgentPlugin calls: mcp_client.execute_tool
  path = ["agent", "mcp_client"]
  ↓
McpClientPlugin calls: filesystem.read
  path = ["agent", "mcp_client", "filesystem"]
  ↓
FilesystemPlugin returns stream:
  - Progress: path=["agent","mcp_client","filesystem"], msg="Reading file..."
  - Data: path=["agent","mcp_client","filesystem"], content={...}
  - Done: path=["agent","mcp_client","filesystem"]
  ↓
McpClientPlugin returns stream:
  - Progress: path=["agent","mcp_client"], msg="Processing tool result..."
  - Data: path=["agent","mcp_client"], tool_result={...}
  - Done: path=["agent","mcp_client"]
  ↓
AgentPlugin returns stream:
  - Progress: path=["agent"], msg="Generating response..."
  - Data: path=["agent"], response={...}
  - Done: path=["agent"]
```

### Inter-Plugin Call Mechanism

```rust
impl SystemContext {
    /// Call another plugin from within a plugin
    pub async fn call_plugin(
        &self,
        current_path: &PluginPath,
        plugin_name: &str,
        method: &str,
        params: serde_json::Value,
    ) -> Result<impl Stream<Item = ServiceStreamItem>, Error> {
        let service = self.service.read().await;
        let full_method = format!("{}.{}", plugin_name, method);
        let nested_path = current_path.extend(plugin_name);

        // Service handles the routing with the extended path
        service.handle_request_with_path(&full_method, params, nested_path).await
    }
}
```

## JSON-RPC Wire Format

### Request

```json
{
  "jsonrpc": "2.0",
  "method": "agent.chat",
  "params": {
    "agent_id": "123",
    "message": "Hello!"
  },
  "id": 1
}
```

### Response (Streaming via SSE)

```json
{
  "jsonrpc": "2.0",
  "result": {
    "stream_id": "550e8400-...",
    "sse_url": "/streams/550e8400-..."
  },
  "id": 1
}
```

### SSE Events

```
event: progress
data: {"plugin_path":"agent","message":"Starting...","percentage":null}

event: data
data: {"plugin_path":"agent.mcp_client","content_type":"ToolResult","data":{...}}

event: progress
data: {"plugin_path":"agent","message":"Generating response...","percentage":75}

event: data
data: {"plugin_path":"agent","content_type":"AgentResponse","data":{...}}

event: done
data: {"plugin_path":"agent"}
```

## Implementation Phases

### Phase 1: Core Infrastructure
- [ ] Define `Plugin` trait
- [ ] Implement `PluginPath` type
- [ ] Create `ServiceStreamItem` enum
- [ ] Build `PluginService` router
- [ ] Set up `SystemContext` structure

### Phase 2: Proc Macro Development
- [ ] Design macro syntax
- [ ] Implement method enum parsing
- [ ] Generate parameter structs
- [ ] Generate JSON Schema impls
- [ ] Generate routing logic

### Phase 3: Plugin Context System
- [ ] Implement `PluginContext` with SQLite
- [ ] Set up per-plugin database isolation
- [ ] Create inter-plugin call mechanism
- [ ] Build event bus for plugin communication

### Phase 4: Example Plugin
- [ ] Create simple "echo" plugin as reference
- [ ] Demonstrate nested plugin calls
- [ ] Show context management patterns
- [ ] Document best practices

### Phase 5: Migration
- [ ] Migrate existing agent_db functionality
- [ ] Convert to new plugin architecture
- [ ] Preserve backward compatibility
- [ ] Update tests

## Design Decisions

### Why Streams Always?

**Decision**: All plugin methods return streams, even for "simple" operations.

**Rationale**:
1. **Uniform interface**: No special-casing synchronous vs async operations
2. **Progressive disclosure**: Simple operations can emit one item and close
3. **Future-proof**: Easy to add progress reporting later
4. **Nested call tracking**: Path metadata flows naturally through streams
5. **Cancellation**: Stream dropping = request cancellation

**Example** - Simple operation:
```rust
fn simple_method(&self) -> impl Stream<Item = PluginStreamItem> {
    stream! {
        yield PluginStreamItem::data(&path, "result", json!({"status": "ok"}));
        yield PluginStreamItem::done(&path);
    }
}
```

### Why Plugin Path Instead of Request IDs?

**Decision**: Use hierarchical plugin paths instead of flat request IDs.

**Rationale**:
1. **Semantic meaning**: Path shows call hierarchy
2. **Debugging**: Easy to trace nested calls
3. **Telemetry**: Natural grouping for metrics
4. **Cancellation**: Cancel entire subtree by path prefix

### Why Per-Plugin Databases?

**Decision**: Each plugin gets its own SQLite database.

**Rationale**:
1. **Isolation**: Plugin failures don't corrupt others' data
2. **Migration**: Plugins manage their own schema evolution
3. **Simplicity**: No need for complex table namespacing
4. **Performance**: No lock contention between plugins
5. **Deployment**: Plugins can be developed/tested independently

### Why Use Existing Libraries Instead of Custom Macros?

**Decision**: Use `jsonrpsee` instead of building custom proc macros.

**Rationale**:
1. **Battle-tested**: Used in production by major projects (Polkadot, Substrate)
2. **Maintained**: Active development and security updates
3. **Feature-complete**: Subscriptions, error handling, client generation
4. **Performance**: Optimized for async/await patterns
5. **Community**: Large ecosystem and support
6. **Time-to-market**: Avoid building and maintaining custom macro infrastructure
7. **Standards compliance**: Full JSON-RPC 2.0 spec compliance

### Migration Path from `jsonrpc-core`

Current codebase uses `jsonrpc-core` (older library). Migration involves:

1. **Add `jsonrpsee` dependencies**:
```toml
[dependencies]
jsonrpsee = { version = "0.21", features = ["server", "macros"] }
```

2. **Convert manual method registration to traits**:
```rust
// Before (jsonrpc-core)
handler.add_method("agent.create", move |params| {
    // Manual parameter parsing
    let value: Value = params.parse()?;
    let name = value.get("name")...
});

// After (jsonrpsee)
#[rpc(server, namespace = "agent")]
trait AgentPlugin {
    #[method(name = "create")]
    async fn create(&self, name: String) -> Result<Agent, Error>;
}
```

3. **Leverage subscriptions for streaming**:
```rust
// Native streaming support via subscriptions
#[subscription(name = "chat_stream", item = ChatItem)]
async fn chat(&self, agent_id: String) -> SubscriptionResult;
```

4. **Backward compatibility**: Keep `jsonrpc-core` for existing code during transition

## Open Questions

1. **Context System Details**: What exactly is in `SystemContext`? (TBD)
2. **Authentication**: How do we authenticate plugin calls?
3. **Rate Limiting**: Per-plugin? Per-user? Both?
4. **Plugin Discovery**: Static registration or dynamic loading?
5. **Versioning**: How do we handle plugin version conflicts?
6. **Schema Evolution**: How do plugins migrate their databases?

## Next Steps

1. Get feedback on overall architecture
2. Clarify `SystemContext` requirements
3. Start Phase 1 implementation
4. Design proc macro syntax in detail
5. Create example plugin as reference implementation
