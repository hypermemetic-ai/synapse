# Synapse

**A CLI that writes itself from your API.**

Synapse is the command-line frontend for [Plexus RPC](../plexus-protocol/) — a streaming JSON-RPC 2.0 protocol built for LLM orchestration. Define methods in Rust, and Synapse discovers them at runtime: commands, help text, parameter validation, and output rendering all come from the schema. No codegen, no static config.

```
                  ┌─────────────┐
                  │  Substrate   │  Rust backend hub
                  │  (Plexus)    │  hosts activations
                  └──────┬──────┘
                         │ WebSocket JSON-RPC 2.0
                         │ streaming responses
                  ┌──────┴──────┐
                  │   Synapse    │  Haskell CLI
                  │  discovers   │  builds commands from schema
                  │  & invokes   │  renders streaming output
                  └─────────────┘
```

## The Full Picture: Define → Call

### 1. Define a method in Rust

An **activation** is a namespace of related methods. You define one by annotating an impl block:

```rust
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

// Event type — what the method streams back
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "event", rename_all = "snake_case")]
pub enum EchoEvent {
    Echo { message: String, count: u32 },
}

// The activation
pub struct Echo;

#[plexus_macros::hub_methods(
    namespace = "echo",
    version = "1.0.0",
    description = "Echo messages back"
)]
impl Echo {
    #[plexus_macros::hub_method(
        description = "Echo a message back the specified number of times",
        params(
            message = "The message to echo",
            count = "Number of times to repeat (default: 1)"
        )
    )]
    async fn echo(
        &self,
        message: String,
        count: u32,
    ) -> impl Stream<Item = EchoEvent> + Send + 'static {
        stream! {
            for i in 0..count {
                yield EchoEvent::Echo {
                    message: message.clone(),
                    count: i + 1,
                };
            }
        }
    }

    #[plexus_macros::hub_method(
        description = "Echo a message once",
        params(message = "The message to echo")
    )]
    async fn once(
        &self,
        message: String,
    ) -> impl Stream<Item = EchoEvent> + Send + 'static {
        stream! {
            yield EchoEvent::Echo { message, count: 1 };
        }
    }
}
```

The macro generates the JSON-RPC server, `Activation` trait impl, method enum, and JSON Schemas — all from the function signatures and `JsonSchema` derives. Register it with the hub:

```rust
let hub = DynamicHub::new("substrate")
    .register(Echo::new())
    .register(Health::new())
    .register(Cone::new());
```

That's the entire backend. No route tables, no handler boilerplate, no schema files.

### 2. Call it from Synapse

```bash
# Install
cd synapse && cabal install

# Synapse discovers the echo activation and its methods automatically
$ synapse substrate echo once --message "hello"
message: hello
count: 1

$ synapse substrate echo echo --message "hello" --count 3
message: hello
count: 1

message: hello
count: 2

message: hello
count: 3
```

Every method you add to the Rust backend is immediately available as a CLI command. The parameter names, types, descriptions, and validation all come from the schema.

### 3. See what's on the wire

```bash
# Dry-run: show the JSON-RPC request without sending
$ synapse --dry-run substrate echo echo --message "hello" --count 3
{"method":"echo.echo","params":{"message":"hello","count":3}}

# JSON mode: see the raw stream items
$ synapse --json substrate echo echo --message "hello" --count 3
{"type":"data","content":{"event":"echo","message":"hello","count":1}}
{"type":"data","content":{"event":"echo","message":"hello","count":2}}
{"type":"data","content":{"event":"echo","message":"hello","count":3}}
{"type":"complete"}

# Fetch the JSON Schema for the activation
$ synapse --schema substrate echo
{"namespace":"echo","methods":[...]}
```

## The Registry

Plexus backends register themselves with a **registry** — a central service that tracks what's running and where. Synapse talks to the registry by default (at `localhost:4444`), so you never need to know host:port pairs for individual backends. You just use the backend by name:

```bash
# This just works — the registry resolves "substrate" to its host:port
synapse substrate echo once --message "hello"
```

When you run `synapse` with no arguments, it queries the registry and lists everything available:

```bash
$ synapse
Available backends:
  substrate      127.0.0.1:4444 [OK]
  lforge         127.0.0.1:4447 [OK]
  secrets        127.0.0.1:4446 [OK]
```

If your backend isn't showing up, it needs to register with the registry. Synapse also auto-registers backends it connects to directly — so if you point at a non-default port once, it becomes available by name for future calls:

```bash
# Direct connection (also registers with the registry at :4444)
synapse -P 4447 lforge hyperforge repos_list --org juggernaut

# Now this works without -P
synapse lforge hyperforge repos_list --org juggernaut
```

The key mental model: **don't think in terms of hosts and ports.** Register your backends, then use them by name. The registry is the source of truth.

## Progressive Discovery

Once you're connected, just start typing. Synapse navigates the backend like a filesystem — each level shows what's available, so you never need external docs:

```bash
# What can substrate do?
$ synapse substrate
  arbor               Tree-structured conversation storage
  bash                Execute bash commands
  cone                Conversational AI sessions
  echo                Echo messages back
  health              Health check endpoint

# What methods does echo have?
$ synapse substrate echo
  echo                Echo a message back the specified number of times
  once                Echo a message once

# What does cone chat need? (auto-help when required params are missing)
$ synapse substrate cone chat
chat - Conversational interaction with a cone

  --identifier.type <string> (required)
      Discriminator for cone lookup (by_id, by_name)

  --identifier.name <string> (required if type=by_name)
  --prompt <string> (required)
      User message to send

# Inspect the raw JSON Schema for an activation
$ synapse --schema substrate cone
```

## Parameters

Synapse parses flags into typed JSON using the schema's IR:

```bash
# Simple flags
synapse substrate echo echo --message "hello" --count 3

# Dotted keys for nested objects
synapse substrate cone chat \
  --identifier.type by_name \
  --identifier.name my-assistant \
  --prompt "hello"

# Arrays — repeated flags or comma-separated
synapse substrate tags set --tags backend --tags critical
synapse substrate tags set --tags backend,critical,urgent

# Boolean flags (bare flag = true)
synapse substrate cone create --ephemeral

# Raw JSON override
synapse substrate cone chat -p '{
  "identifier": {"type": "by_name", "name": "my-assistant"},
  "prompt": "hello"
}'
```

Path parameters (`--path`, `--working_dir`, etc.) expand `~`, resolve relative paths, and substitute `$ENV` variables automatically.

## Output Rendering

Synapse renders streaming responses through a Mustache template pipeline:

```bash
# Default: schema-aware pretty output
synapse substrate health check
status: healthy
uptime: 12345

# --raw: content JSON only (skip template rendering)
synapse --raw substrate health check
{"status":"healthy","uptime":12345}

# --json: full JSON-RPC stream items
synapse --json substrate health check
{"type":"data","content":{"status":"healthy","uptime":12345}}
```

Templates are resolved in order: `.substrate/templates/{namespace}/{method}.mustache` (project-local) → `~/.config/synapse/templates/` (user) → YAML-like fallback.

Generate templates from the schema:

```bash
synapse --generate-templates substrate
```

## Code Generation

Synapse emits a structured Intermediate Representation for building typed clients:

```bash
$ synapse --emit-ir substrate > substrate.ir.json
```

```json
{
  "irVersion": "2.0",
  "irTypes": {
    "echo.EchoEvent": {
      "tdKind": {
        "tag": "KindEnum",
        "keDiscriminator": "event",
        "keVariants": [
          {"vdName": "echo", "vdFields": [
            {"fdName": "message", "fdType": {"tag": "RefPrimitive", "contents": ["string", null]}},
            {"fdName": "count", "fdType": {"tag": "RefPrimitive", "contents": ["integer", "uint32"]}}
          ]}
        ]
      }
    }
  },
  "irMethods": {
    "echo.echo": {
      "mdStreaming": true,
      "mdParams": [
        {"pdName": "message", "pdType": {"tag": "RefPrimitive", "contents": ["string", null]}, "pdRequired": true},
        {"pdName": "count", "pdType": {"tag": "RefPrimitive", "contents": ["integer", "uint32"]}, "pdRequired": true}
      ],
      "mdReturns": {"tag": "RefNamed", "contents": ["echo", "EchoEvent"]}
    }
  }
}
```

Types are deduplicated by content hash across namespaces. Streaming is inferred from return type structure (enum with >1 non-error variant = streaming).

## Client Generation (synapse-cc)

[**synapse-cc**](../synapse-cc/) (Synapse Compiler Collection) is a separate project that consumes Synapse's IR to produce typed client libraries. It imports plexus-synapse as a library, generates IR from a live backend, pipes it through [hub-codegen](../hub-codegen/) (a stateless Rust code generator), then handles merging, dependencies, building, and testing.

```bash
synapse-cc init                    # scaffold synapse.config.json
synapse-cc build                   # generate typed client from config
synapse-cc watch substrate         # rebuild on schema changes
```

The generated client gives you typed methods matching the backend 1:1:

```typescript
const client = new SubstrateClient("ws://localhost:4444");

for await (const event of client.echo.echo({ message: "hello", count: 3 })) {
  console.log(event.message, event.count);
}
```

Three-way merge preserves user edits across regeneration. See the [synapse-cc architecture doc](docs/architecture/16673264336036332543_synapse-cc-pipeline.md) for the full pipeline, config format, and caching strategy.

## Error Messages

```bash
$ synapse substrate invalid-plugin
Command not found: 'invalid-plugin' at substrate

Available at substrate:
  Methods:
    schema              - Get schema information
    health              - Health check endpoint

  Child plugins:
    arbor               - Tree-structured conversation storage
    bash                - Execute bash commands
    cone                - Conversational AI sessions

$ synapse substrate cone chat --mesage "hello"
Unknown parameter: --mesage

Did you mean: --message?
```

## Debugging & Testing

Synapse provides built-in tools for debugging connections and validating protocol compliance.

### Connection Debugging

Test the complete connection stack (TCP → HTTP → WebSocket → RPC):

```bash
synapse _self debug localhost 5001 substrate
```

This tests each layer sequentially:
- **TCP**: Can we reach the port?
- **HTTP**: Does the server respond to HTTP?
- **WebSocket**: Can we upgrade to WebSocket?
- **RPC**: Can we call `_info`?

### Protocol Validation

Run the full protocol compliance test suite:

```bash
synapse _self validate localhost 5001 substrate
```

This calls the `_debug.*` endpoints (enabled with `PLEXUS_DEBUG=true` on the server) and validates:
- StreamDone messages are sent
- Metadata structure is correct
- Field naming follows conventions
- Progress percentages are in range

### Test Arbitrary Methods

Test any method with protocol validation:

```bash
# Use CLI flags for connection settings
synapse -P 5001 _self test substrate.ping.pong --message "test"

# Test with schema validation
synapse -P 5001 _self test substrate.echo.echo --message "hello" --count 3

# Allow unknown parameters (warns but passes through)
synapse -P 5001 _self test --allow-unknown substrate.echo.echo --fake "value"

# Use raw JSON (skips schema validation)
synapse -P 5001 _self test --raw '{"message":"hello"}' substrate.echo.echo
```

**Important**: Connection settings come from `-H`/`--host` (default: `127.0.0.1`) and `-P`/`--port` (default: `4444`) flags, not positional arguments. This ensures the test uses the same connection that discovery validated.

### Port Scanning

Discover backends running on common ports:

```bash
synapse _self scan
# Scans ports 4440-4459 for Plexus backends
```

## Architecture

Synapse treats the plugin hierarchy as a **category** and implements operations as **algebras** over recursion schemes:

```haskell
type SynapseM = ExceptT SynapseError (ReaderT SynapseEnv IO)

-- Navigate to a path (paramorphism — fold with access to original structure)
navigate :: Path -> SynapseM SchemaView

-- Walk the full tree (hylomorphism — fused unfold + fold, no intermediate tree)
walkSchema :: (SchemaF a -> SynapseM a) -> Path -> SynapseM a

-- Build IR (hylomorphism with parallel child fetching)
buildIR :: Path -> SynapseM IR
```

The base functor:

```haskell
data SchemaF a
  = PluginF PluginSchema Path [a]   -- Interior node (namespace with children)
  | MethodF MethodSchema Text Path  -- Leaf (invocable method)
```

Schemas are fetched lazily during navigation, cached by content hash, with cycle detection for recursive plugin graphs.

## Project Structure

```
synapse/
  app/Main.hs              # CLI entry, two-phase arg parsing, dispatch
  src/Synapse/
    Monad.hs               # SynapseM effect stack, error types
    Transport.hs           # WebSocket JSON-RPC bridge
    Schema/
      Types.hs             # Path, PluginSchema, MethodSchema, NavError
      Functor.hs           # SchemaF base functor, Fix
    Algebra/
      Recursion.hs         # cata, ana, hylo, para, apo (pure + monadic)
      Navigate.hs          # Path navigation (paramorphism)
      Walk.hs              # Tree walking (hylomorphism)
      Render.hs            # Schema → text rendering
    CLI/
      Parse.hs             # IR-driven --flag parsing → typed JSON
      Help.hs              # Auto-generated help from IR
      Transform.hs         # Path expansion, env var substitution
      Template.hs          # Mustache template generation from IR
    IR/
      Types.hs             # IR, TypeDef, MethodDef, TypeRef
      Builder.hs           # Schema → IR (hylomorphism + type dedup)
    Backend/
      Discovery.hs         # Registry discovery, health checks
    Self/
      Commands.hs          # _self meta-commands (scan, templates)

../plexus-protocol/        # Shared Plexus types (sibling package)
```

## Development

```bash
cabal build                        # Build
cabal test                         # Run tests (needs backend on localhost:4444)
cabal install                      # Install synapse binary
cabal build -f build-examples      # Build optional examples
```

## License

MIT
