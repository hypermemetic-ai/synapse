# Typed Subscription Pattern for Plugin System

**Date**: 2025-12-07
**Status**: Design

## Problem

We want plugins to:
1. Return **tightly-typed, specific stream types** (e.g., `Stream<AgentChatItem>`)
2. Have the service **uniformly handle all plugins as subscriptions**
3. **Enforce at compile-time** that plugin return types are convertible to `SubscriptionResult`

This gives us:
- Type safety in plugin implementations
- Uniform subscription-based service interface
- Compile-time verification that all plugins fit the model

## Solution: Trait Bounds + Conversion Layer

### Core Trait: `IntoSubscription`

```rust
use jsonrpsee::types::SubscriptionResult;
use jsonrpsee::PendingSubscription;
use futures::Stream;

/// Types that can be converted into a subscription
pub trait IntoSubscription: Send + 'static {
    /// The item type this stream yields
    type Item: Serialize + Send + 'static;

    /// Convert this stream into a subscription
    async fn into_subscription(
        self,
        pending: PendingSubscription,
        plugin_path: PluginPath,
    ) -> SubscriptionResult;
}
```

### Blanket Implementation for Streams

```rust
impl<S, T> IntoSubscription for S
where
    S: Stream<Item = T> + Send + Unpin + 'static,
    T: Serialize + Send + 'static,
{
    type Item = T;

    async fn into_subscription(
        self,
        pending: PendingSubscription,
        plugin_path: PluginPath,
    ) -> SubscriptionResult {
        let sink = pending.accept().await?;

        tokio::spawn(async move {
            pin_mut!(self);
            while let Some(item) = self.next().await {
                // Wrap item with plugin path
                let tracked = TrackedStreamItem {
                    plugin_path: plugin_path.clone(),
                    item,
                };

                if sink.send(&tracked).await.is_err() {
                    break; // Client disconnected
                }
            }
        });

        Ok(())
    }
}
```

### Plugin Trait with Typed Returns

```rust
/// Base trait that all plugins must implement
pub trait Plugin: Send + Sync + 'static {
    fn name(&self) -> &str;
}

/// Marker trait: methods must return types implementing IntoSubscription
pub trait PluginMethod {
    type Output: IntoSubscription;
}
```

## Plugin Definition Pattern

### Option A: Using jsonrpsee with Helper Wrapper

Since `jsonrpsee`'s `#[subscription]` macro expects `SubscriptionResult`, we create a helper:

```rust
use jsonrpsee::proc_macros::rpc;

// Step 1: Define the tightly-typed plugin trait
#[async_trait]
pub trait AgentPluginTyped {
    /// Returns a tightly-typed stream
    async fn chat_stream(
        &self,
        agent_id: String,
        message: String,
    ) -> Result<impl Stream<Item = AgentChatItem> + Send + 'static, Error>;

    async fn list_agents(&self) -> Result<Vec<Agent>, Error>;
}

// Step 2: Define the jsonrpsee RPC interface that wraps the typed trait
#[rpc(server, namespace = "agent")]
pub trait AgentPlugin {
    #[method(name = "list")]
    async fn list(&self) -> Result<Vec<Agent>, Error>;

    #[subscription(name = "chat", item = TrackedStreamItem<AgentChatItem>)]
    async fn chat(&self, agent_id: String, message: String) -> SubscriptionResult;
}

// Step 3: Implementation bridges typed trait to RPC trait
pub struct AgentPluginImpl {
    ctx: Arc<PluginContext>,
}

#[async_trait]
impl AgentPluginTyped for AgentPluginImpl {
    async fn chat_stream(
        &self,
        agent_id: String,
        message: String,
    ) -> Result<impl Stream<Item = AgentChatItem> + Send + 'static, Error> {
        let ctx = self.ctx.clone();

        Ok(stream! {
            yield AgentChatItem::Started;

            let response = ctx.generate_response(&agent_id, &message).await?;

            yield AgentChatItem::Token(response);

            yield AgentChatItem::Done;
        })
    }

    async fn list_agents(&self) -> Result<Vec<Agent>, Error> {
        self.ctx.db().query_agents().await
    }
}

#[async_trait]
impl AgentPluginServer for AgentPluginImpl {
    async fn list(&self) -> Result<Vec<Agent>, Error> {
        // Direct delegation
        AgentPluginTyped::list_agents(self).await
    }

    async fn chat(&self, pending: PendingSubscription, agent_id: String, message: String)
        -> SubscriptionResult
    {
        // Get the typed stream
        let stream = self.chat_stream(agent_id, message).await
            .map_err(|e| jsonrpsee::core::Error::Custom(e.to_string()))?;

        // Convert to subscription using our trait
        let path = PluginPath::root(self.ctx.plugin_name());
        stream.into_subscription(pending, path).await
    }
}
```

### Option B: Custom Proc Macro That Generates Both

Create a macro that generates both the typed trait and jsonrpsee wrapper:

```rust
use cognition_plugin::plugin;

#[plugin(namespace = "agent")]
pub trait AgentPlugin {
    /// Non-streaming method
    #[method(name = "list")]
    async fn list_agents(&self) -> Result<Vec<Agent>, Error>;

    /// Streaming method - returns tightly typed stream
    #[stream(name = "chat", item = AgentChatItem)]
    async fn chat(
        &self,
        agent_id: String,
        message: String,
    ) -> Result<impl Stream<Item = AgentChatItem> + Send, Error>;
}
```

The macro generates:
1. The typed trait with exact return types
2. A jsonrpsee `#[rpc]` wrapper trait
3. Bridge code that converts typed streams to `SubscriptionResult`
4. Compile-time verification of `IntoSubscription` bounds

## Tracked Stream Items

```rust
/// Wrapper that adds plugin path to stream items
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackedStreamItem<T> {
    /// Path through plugin call chain
    pub plugin_path: PluginPath,

    /// The actual stream item (strongly typed)
    #[serde(flatten)]
    pub item: T,
}

/// Example: Agent chat item
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "data")]
pub enum AgentChatItem {
    Started,
    Token(String),
    ToolCall { name: String, args: Value },
    ToolResult { result: Value },
    Done,
    Error { message: String },
}
```

## Type Safety Guarantees

### Compile-Time Verification

```rust
// This compiles - AgentChatItem implements Serialize
#[stream(name = "chat", item = AgentChatItem)]
async fn chat(&self, ...) -> Result<impl Stream<Item = AgentChatItem>, Error>;

// This FAILS to compile - MyType doesn't implement Serialize
#[stream(name = "bad", item = MyType)]
async fn bad(&self, ...) -> Result<impl Stream<Item = MyType>, Error>;
// Error: MyType doesn't implement Serialize, required by IntoSubscription
```

### Service Can Treat Everything Uniformly

```rust
pub struct PluginService {
    plugins: HashMap<String, Arc<dyn RpcModule>>,
}

impl PluginService {
    pub async fn call_plugin(&self, method: &str, params: Value) -> SubscriptionResult {
        // All plugins expose jsonrpsee RPC modules
        // All streaming methods return SubscriptionResult
        // Service doesn't need to know about specific types
        let module = self.plugins.get(method)?;
        module.call_subscription(method, params).await
    }
}
```

## Inter-Plugin Calls with Type Safety

When one plugin calls another, use the generated client:

```rust
impl AgentPluginImpl {
    async fn execute_tool(&self, tool_name: &str) -> Result<ToolResult, Error> {
        // Get MCP client (type-safe, generated by jsonrpsee)
        let mcp_client = self.ctx.get_client::<McpPluginClient>()?;

        // Call is fully typed
        let stream: impl Stream<Item = TrackedStreamItem<ToolExecutionItem>> =
            mcp_client.execute_tool(tool_name).await?;

        // We can work with the specific type
        pin_mut!(stream);
        while let Some(tracked) = stream.next().await {
            match tracked.item {
                ToolExecutionItem::Progress(msg) => log::info!("Tool progress: {}", msg),
                ToolExecutionItem::Result(result) => return Ok(result),
                ToolExecutionItem::Error(err) => return Err(err.into()),
            }
        }

        Err(Error::ToolDidNotComplete)
    }
}
```

## Comparison: Option A vs Option B

### Option A: Manual Wrapper (Two Traits)

**Pros**:
- Uses standard `jsonrpsee` without custom macros
- Clear separation of typed interface vs RPC interface
- Works today with existing tools

**Cons**:
- Some boilerplate in the bridge code
- Two trait definitions to maintain

**Example structure**:
```rust
// Typed trait - what plugin author implements
trait AgentPluginTyped { ... }

// RPC trait - jsonrpsee generated
trait AgentPluginServer { ... }

// Bridge implementation
impl AgentPluginServer for T where T: AgentPluginTyped { ... }
```

### Option B: Custom Macro

**Pros**:
- Single trait definition
- Less boilerplate
- Can add custom validation

**Cons**:
- Need to build and maintain macro
- Adds complexity
- Potential compatibility issues with jsonrpsee updates

**Example structure**:
```rust
#[plugin(namespace = "agent")]
trait AgentPlugin {
    #[stream(...)]
    async fn chat(...) -> Result<impl Stream<...>, Error>;
}
// Macro generates everything
```

## Recommendation

**Use Option A (Manual Wrapper)** for the initial implementation:

1. **Leverage jsonrpsee** - battle-tested, maintained, works today
2. **Minimal custom code** - just the `IntoSubscription` trait
3. **Clear intent** - separation makes the pattern obvious
4. **Easy to understand** - no macro magic to debug
5. **Future upgrade path** - can add custom macro later if boilerplate becomes problematic

## Implementation Example

```rust
// 1. Define strongly-typed plugin interface
#[async_trait]
pub trait AgentPluginTyped {
    async fn chat_stream(
        &self,
        agent_id: String,
        message: String,
    ) -> Result<impl Stream<Item = AgentChatItem> + Send + 'static, Error>;
}

// 2. Define jsonrpsee RPC interface
#[rpc(server, client, namespace = "agent")]
pub trait AgentPlugin {
    #[subscription(name = "chat", item = TrackedStreamItem<AgentChatItem>)]
    async fn chat(&self, agent_id: String, message: String) -> SubscriptionResult;
}

// 3. Implement typed interface
impl AgentPluginTyped for AgentPluginImpl {
    async fn chat_stream(&self, agent_id: String, message: String)
        -> Result<impl Stream<Item = AgentChatItem> + Send + 'static, Error>
    {
        Ok(stream! {
            yield AgentChatItem::Started;
            // ... implementation
            yield AgentChatItem::Done;
        })
    }
}

// 4. Bridge to RPC interface
#[async_trait]
impl AgentPluginServer for AgentPluginImpl {
    async fn chat(&self, pending: PendingSubscription, agent_id: String, message: String)
        -> SubscriptionResult
    {
        let stream = self.chat_stream(agent_id, message).await?;
        let path = PluginPath::root("agent");
        stream.into_subscription(pending, path).await
    }
}
```

## Benefits

✅ **Type Safety**: Plugin methods return specific types
✅ **Uniform Service**: Everything is a subscription at the RPC layer
✅ **Compile-Time Checks**: `IntoSubscription` bound enforced by compiler
✅ **Inter-Plugin Calls**: Generated clients provide typed interfaces
✅ **Path Tracking**: Automatic wrapping with `TrackedStreamItem`
✅ **Flexibility**: Plugins can define arbitrary item types

## Next Steps

1. Implement `IntoSubscription` trait
2. Create `TrackedStreamItem<T>` wrapper
3. Build example plugin using the pattern
4. Validate with nested plugin calls
5. Consider custom macro if boilerplate becomes excessive
