# Terminology: Plexus, Activation, Provenance

## The Stack

```
┌─────────────────────────────────────────────────────────────────┐
│                         SYMBOLS                                  │
│  Haskell frontend: typed APIs, semantic interpretation          │
└──────────────────────────────┬──────────────────────────────────┘
                               │ JSON-RPC WebSocket
┌──────────────────────────────▼──────────────────────────────────┐
│                        SUBSTRATE                                 │
│  Rust backend: ground truth, storage, execution                 │
│                                                                  │
│  ┌────────────────────────────────────────────────────────────┐ │
│  │                        PLEXUS                               │ │
│  │  Nerve center: routes calls, coordinates activations       │ │
│  │                                                             │ │
│  │  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐      │ │
│  │  │  Agent   │ │   Loom   │ │   Bash   │ │  Health  │      │ │
│  │  │          │ │          │ │          │ │          │      │ │
│  │  │impl      │ │impl      │ │impl      │ │impl      │      │ │
│  │  │Activation│ │Activation│ │Activation│ │Activation│      │ │
│  │  └──────────┘ └──────────┘ └──────────┘ └──────────┘      │ │
│  └────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────┘
```

## Terminology

### Substrate

The entire Rust backend. The **ground**—where invariants are enforced, data persists, and proofs are generated.

Substrate is not a layer in a traditional sense. It's the **physical symbol system** (Newell & Simon) on which higher-level reasoning operates.

### Plexus

From anatomy: a **nerve plexus** is a branching network of intersecting nerves. The solar plexus coordinates autonomic functions.

In substrate, the Plexus is the **coordination layer**:
- Routes method calls to the appropriate activation
- Manages activation lifecycle (registration, RPC conversion)
- Maintains the registry of available capabilities

```rust
let plexus = Plexus::new()
    .register(Health::new())
    .register(Bash::new())
    .register(Loom::new(config).await?)
    .register(Agent::new(config).await?);
```

The Plexus is **inside** substrate, not a separate tier. It's the connective tissue that binds activations into a coherent system.

### Activation

A capability that can be triggered. The `Activation` trait defines what it means to be a callable capability:

```rust
#[async_trait]
pub trait Activation: Send + Sync + 'static {
    fn namespace(&self) -> &str;
    fn version(&self) -> &str;
    fn methods(&self) -> Vec<&str>;

    async fn call(&self, method: &str, params: Value)
        -> Result<PlexusStream, PlexusError>;

    fn into_rpc_methods(self) -> Methods;
}
```

Concrete types implement this trait:
- `Agent` implements `Activation`
- `Loom` implements `Activation`
- `Bash` implements `Activation`
- `Health` implements `Activation`

We don't say `AgentActivation`—the struct is just `Agent`. The trait relationship is implicit, like how you don't say `VecCollection` in Rust.

### Provenance

The **origin and chain of custody** of an event. When an activation emits a stream event, the provenance tracks where it came from.

```rust
Provenance {
    segments: ["agent"]           // Direct from agent
}

Provenance {
    segments: ["agent", "loom"]   // Agent called loom internally
}
```

Provenance enables:
- **Debugging**: Which activation produced this error?
- **Filtering**: Show me only events from the loom activation
- **Nested calls**: Track the full call chain when activations invoke each other

## Directory Structure

```
substrate/src/
├── plexus/
│   ├── plexus.rs      # Plexus struct, Activation trait
│   ├── path.rs        # Provenance type
│   ├── types.rs       # PlexusStreamItem
│   └── schema.rs      # Schema generation for documentation
├── activations/
│   ├── agent/
│   │   ├── activation.rs   # Agent implements Activation
│   │   ├── storage.rs      # Agent-specific storage
│   │   └── types.rs        # AgentEvent, etc.
│   ├── bash/
│   ├── health/
│   └── loom/
└── plugin_system/
    ├── conversion.rs   # IntoSubscription for RPC streaming
    └── types.rs        # ActivationStreamItem trait
```

## Type Reference

| Type | Purpose |
|------|---------|
| `Plexus` | Coordinator that routes calls to activations |
| `Activation` | Trait for callable capabilities |
| `ActivationInfo` | Metadata about an activation (namespace, version, methods) |
| `PlexusStream` | Stream of events from an activation call |
| `PlexusStreamItem` | Individual event (Data, Progress, Error, Done) |
| `PlexusError` | Error from plexus operations |
| `Provenance` | Chain of custody for events |
| `ActivationStreamItem` | Trait for activation-specific events |

## Why These Names

### Why "Plexus" not the routing-device name or "Body"

- **The routing-device name** is a network topology term—passive routing
- **Body** implies the whole organism, but this is just the coordinator inside substrate
- **Plexus** is precisely what this is: a nerve network that coordinates and routes signals

### Why "Activation" not "Plugin" or "Behavior"

- **Plugin** implies external, optional, plugged-in from outside
- **Behavior** is good but overloaded (behavioral patterns, etc.)
- **Activation** captures the essence: these are capabilities that get **activated** when called

### Why "Provenance" not "Path" or "Origin"

- **Path** is overloaded (file paths, tree paths, URL paths)
- **Origin** is singular—doesn't capture the chain
- **Provenance** is precise: origin AND chain of custody, used in art/archival contexts for exactly this purpose

## Theoretical Context

This layer is **below SST** (Stratified Systems Theory). SST operates at the level of agents making decisions with different time horizons. We're building the **physical symbol system** that SST-style agents will use.

```
SST Layer (future)
    Agents managing agents, stratified context
              ↓ uses
Symbols Layer
    Typed APIs, semantic interpretation
              ↓ uses
Substrate Layer (this document)
    Plexus coordinates activations
    Activations provide capabilities
    Provenance tracks origin
```

The Plexus doesn't make decisions—it routes. The activations don't interpret—they execute. Meaning emerges at the Symbols layer; strategy emerges at the SST layer.
