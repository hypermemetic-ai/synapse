# Terminology: Body, Behavior, Provenance

## Summary

The substrate codebase uses biological metaphor for its core abstractions:

| Concept | Type | Description |
|---------|------|-------------|
| **Body** | `Body` | The central organism that hosts and coordinates behaviors |
| **Behavior** | `trait Behavior` | A capability the body can perform (loom, agent, bash, health) |
| **Provenance** | `Provenance` | Chain of custody tracking which behavior emitted an event |

## Rationale

### Why "Body" instead of the routing-device name

The routing-device name is a network topology term—it implies a passive routing device. The substrate is more than a router; it's the **living infrastructure** on which behaviors operate.

A body:
- Has behaviors (not plugins)
- Maintains state (storage, connections)
- Coordinates activity
- Is the ground truth for what exists

### Why "Behavior" instead of "Plugin"

"Plugin" implies something external, optional, plugged in from outside. Our behaviors are **intrinsic capabilities**—they define what the body can do.

A behavior:
- Is a capability, not an add-on
- Operates through the body, not on it
- Has a namespace, version, and methods
- Produces streams of events with provenance

### Why "Provenance" instead of "PluginPath"

"Path" is overloaded (file paths, tree paths, URL paths). "Provenance" precisely describes what we're tracking: **the origin and chain of custody** of an event.

```rust
Provenance {
    segments: ["agent", "loom"]  // agent called loom internally
}
```

This tells you:
- Who emitted this event (agent)
- Through what chain of calls (agent → loom)
- For debugging, filtering, and routing

## Type Mappings

| Old | New |
|-----|-----|
| `Service` | `Body` |
| `Plugin` | `Behavior` (trait) |
| `ServiceError` | `BodyError` |
| `ServiceStream` | `BodyStream` |
| `ServiceStreamItem` | `BodyStreamItem` |
| `PluginPath` | `Provenance` |
| `PluginStreamItem` | `BehaviorStreamItem` |
| `into_service_stream` | `into_body_stream` |
| `into_service_item` | `into_body_item` |
| `HealthPlugin` | `HealthBehavior` |
| `BashPlugin` | `BashBehavior` |
| `LoomPlugin` | `LoomBehavior` |
| `AgentPlugin` | `AgentBehavior` |

## Directory Structure

```
substrate/src/
├── body/
│   ├── body.rs      # Body struct, Behavior trait
│   ├── path.rs      # Provenance type
│   ├── types.rs     # BodyStreamItem
│   └── schema.rs    # Schema generation
├── behaviors/
│   ├── agent/
│   │   ├── behavior.rs
│   │   ├── storage.rs
│   │   └── types.rs
│   ├── bash/
│   ├── health/
│   └── loom/
└── plugin_system/
    ├── conversion.rs  # IntoSubscription trait
    └── types.rs       # BehaviorStreamItem trait
```

## Usage Examples

### Registering Behaviors

```rust
let body = Body::new()
    .register(HealthBehavior::new())
    .register(BashBehavior::new())
    .register(LoomBehavior::new(config).await?);
```

### Implementing a Behavior

```rust
#[async_trait]
impl Behavior for MyBehavior {
    fn namespace(&self) -> &str { "my" }
    fn version(&self) -> &str { "1.0.0" }
    fn methods(&self) -> Vec<&str> { vec!["do_thing"] }

    async fn call(&self, method: &str, params: Value) -> Result<BodyStream, BodyError> {
        // ...
    }

    fn into_rpc_methods(self) -> Methods {
        self.into_rpc().into()
    }
}
```

### Stream Items with Provenance

```rust
impl BehaviorStreamItem for MyEvent {
    fn into_body_item(self, provenance: Provenance) -> BodyStreamItem {
        BodyStreamItem::Data {
            provenance,
            content_type: "my.event".to_string(),
            data: serde_json::to_value(self).unwrap(),
        }
    }
}
```

### Creating Provenance

```rust
// At behavior root
let provenance = Provenance::root("loom");

// For nested calls
let nested = provenance.extend("internal_behavior");
```

## Theoretical Grounding

This terminology connects to the broader architectural philosophy (see `naming-philosophy.md`):

- **Body** is the substrate layer—ground truth, formal verification
- **Behaviors** are capabilities—what the body can do
- **Provenance** is attestation—proof of where events originated

The biological metaphor reinforces that this is a **living system**, not a passive message router.
