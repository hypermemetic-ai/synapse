# Dynamic CLI Architecture

## Problem

Clients connecting to the substrate service have no way to discover:
1. Which activations are available
2. What methods each activation exposes
3. Which LLM services and models are available through Cone
4. What parameters each method accepts

This forces clients to hardcode method names and parameters, creating tight coupling between client and server versions.

## Solution: Runtime Schema Discovery

Expose the substrate's full capability schema via a dedicated RPC method. Clients query this at startup and construct their interface dynamically.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                          Substrate Service                           │
├─────────────────────────────────────────────────────────────────┤
│  Plexus                                                          │
│  ├── Health                                                      │
│  ├── Bash                                                        │
│  ├── Arbor                                                       │
│  └── Cone ─────────────────────────────────────────────────────┐│
│       │                                                         ││
│       ├── cone_chat, cone_create, cone_list, etc.              ││
│       │                                                         ││
│       └── cone_registry  ◄──── NEW: Exposes cllient registry   ││
│            │                                                    ││
│            └── Returns: services, models, capabilities         ││
└─────────────────────────────────────────────────────────────────┘
                              │
                              │ WebSocket JSON-RPC
                              │
┌─────────────────────────────────────────────────────────────────┐
│                        Dynamic CLI                               │
├─────────────────────────────────────────────────────────────────┤
│  1. Connect to ws://127.0.0.1:4444                              │
│  2. Call service_schema → receive all methods + params              │
│  3. Call cone_registry → receive models + services              │
│  4. Build command structure from schema                         │
│  5. Execute user commands via RPC                               │
└─────────────────────────────────────────────────────────────────┘
```

## New RPC Methods

### `service_schema`

Returns the complete schema of all registered activations and their methods.

**Response structure:**
```
activations: [
  {
    name: "health",
    methods: [
      {
        name: "health_check",
        params: [],
        description: "Returns health status and uptime"
      }
    ]
  },
  {
    name: "arbor",
    methods: [
      {
        name: "arbor_tree_create",
        params: [
          { name: "metadata", type: "object | null" },
          { name: "owner_id", type: "string" }
        ]
      },
      ...
    ]
  },
  ...
]
```

This is derived from the Activation trait's existing `schema()` method, which returns `schemars::schema::Schema`.

### `cone_registry`

Exposes the cllient ModelRegistry to clients.

**Response structure:**
```
services: [
  {
    name: "anthropic",
    base_url: "https://api.anthropic.com",
    message_builder: "anthropic"
  },
  {
    name: "openai",
    base_url: "https://api.openai.com",
    message_builder: "openai"
  },
  ...
]

families: [
  "claude",
  "openai",
  "google",
  "deepseek",
  ...
]

models: [
  {
    id: "claude-3-5-sonnet-20241022",
    family: "claude",
    service: "anthropic",
    capabilities: {
      context_window: 200000,
      max_output_tokens: 8192,
      vision: true,
      streaming: true
    },
    pricing: {
      input_per_1k_tokens: 0.003,
      output_per_1k_tokens: 0.015
    }
  },
  ...
]
```

## CLI Bootstrap Sequence

```
1. Parse CLI args for connection target (default: ws://127.0.0.1:4444)

2. Establish WebSocket connection to service

3. Subscribe to service_schema
   → Receive activation/method definitions
   → Build command tree:
       cli arbor tree-create <metadata> <owner>
       cli arbor tree-get <tree-id>
       cli cone chat <cone-id> <prompt>
       ...

4. Subscribe to cone_registry
   → Receive available models
   → Build model completion/validation:
       cli cone create --model <tab-completes to available models>

5. Enter command loop or execute single command

6. For each user command:
   → Map command path to RPC method name
   → Validate arguments against schema
   → Subscribe to method, stream results
```

## Command Mapping

The CLI translates user commands to RPC calls:

| User Command | RPC Method | Params |
|-------------|------------|--------|
| `cli health check` | `health_check` | `[]` |
| `cli bash exec "ls -la"` | `bash_execute` | `["ls -la"]` |
| `cli arbor tree create --meta '{}' --owner me` | `arbor_tree_create` | `[{}, "me"]` |
| `cli cone create myagent gpt-4o` | `cone_create` | `["myagent", "gpt-4o", null, null]` |
| `cli cone chat <id> "hello"` | `cone_chat` | `["<id>", "hello"]` |

## Schema-Driven Features

With runtime schema, the CLI can provide:

1. **Tab completion** - Complete command names, subcommands, model IDs
2. **Help generation** - `cli arbor --help` shows all arbor methods
3. **Argument validation** - Reject invalid params before RPC call
4. **Type coercion** - Parse JSON objects from string args
5. **Version independence** - CLI works with any service version

## Substrate Integration Points

### Plexus

The Plexus already tracks registered activations. Add a method to serialize the full schema:

```
Plexus
├── activations: HashMap<String, Box<dyn Activation>>
├── list_methods() → Vec<String>           // existing
└── export_schema() → ActivationSchema     // new
```

### Activation Trait

The Activation trait already has `fn schema(&self) -> Schema`. This returns a JSON Schema that can be serialized and sent to clients.

### Cone Activation

Add `cone_registry` method that wraps `cllient::ModelRegistry`:

```
Cone
├── cone_chat(...)
├── cone_create(...)
├── ...
└── cone_registry() → RegistryInfo        // new
```

The `ModelRegistry` from cllient already provides:
- `list_models()` - All model IDs
- `list_families()` - All family names
- `list_services()` - All service names
- `get_model_info(id)` - Full model config

## Fallback Behavior

If schema discovery fails:
1. CLI can use embedded fallback schema (compiled from known version)
2. Or CLI can operate in "raw mode" with manual method/param entry
3. Warn user that completion/validation unavailable

## Benefits

1. **Single source of truth** - Server defines capabilities, client discovers
2. **Loose coupling** - CLI doesn't hardcode method names
3. **Automatic updates** - New activations appear in CLI without rebuild
4. **Model awareness** - CLI knows which models exist, their capabilities, pricing
5. **Better UX** - Tab completion, help, validation all derived from schema

## Related Documents

- [Substrate RPC Protocol](./16681062792662575615_substrate-rpc-protocol.md) - Wire protocol details
- [Arbor/Cone Terminology](./16681067404565638655_arbor-cone-terminology.md) - Naming conventions
