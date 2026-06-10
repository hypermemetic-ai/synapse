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

## Two problem domains synapse solves

1. **Schema-driven invocation.** Method discovery, parameter parsing, help text, and output rendering are all derived live from the backend's JSON Schema. No codegen, no static config — change the backend, the CLI follows.

2. **Credential lifecycle.** Per-backend defaults at `~/.plexus/<backend>/defaults.json` hold cookies and headers as **credential-reference URIs** (`literal:`, `env://`, `file://`, `keychain://`) resolved at request time. JWT decoding surfaces expiry inline (`expired 16d ago`), legacy `~/.plexus/tokens/<backend>` files auto-migrate, and the `_self` subcommand lets you inspect, set, and rotate credentials without ever pasting them into a shell. See [Credentials & Headers](#credentials--headers).

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

#[plexus_macros::activation(
    namespace = "echo",
    version = "1.0.0",
    description = "Echo messages back"
)]
impl Echo {
    /// Echo a message back the specified number of times.
    #[plexus_macros::method]
    async fn echo(
        &self,
        /// The message to echo
        message: String,
        /// Number of times to repeat (default: 1)
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

    /// Echo a message once.
    #[plexus_macros::method]
    async fn once(
        &self,
        /// The message to echo
        message: String,
    ) -> impl Stream<Item = EchoEvent> + Send + 'static {
        stream! {
            yield EchoEvent::Echo { message, count: 1 };
        }
    }
}
```

The macro reads `///` doc comments on both the function and each parameter; they feed the JSON Schema descriptions. (Explicit `description = "..."` and `params(name = "...")` attribute syntax still works and wins when set; see [Macro Reference](#macro-reference) for precedence.) The macro generates the JSON-RPC server, `Activation` trait impl, method enum, and JSON Schemas — all from the function signatures and `JsonSchema` derives. Register it with the hub:

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

## Credentials & Headers

Synapse sends a JWT as `Cookie: access_token=<jwt>` on WebSocket upgrade when one is available, plus any extra cookies or headers you specify. All of these come from a single per-backend defaults file with a priority chain for per-invocation overrides.

### Defaults store

Each backend has a JSON file at `~/.plexus/<backend>/defaults.json` holding default cookies and headers. Values are **credential-reference URIs**, not raw secrets — synapse resolves them at request time.

```json
{
  "version": 1,
  "defaults": {
    "cookies": {
      "access_token": "literal:eyJhbGc..."
    },
    "headers": {
      "X-API-Key": "env://MY_API_KEY"
    }
  },
  "scopes": {}
}
```

Supported schemes:

| Scheme | Example | Meaning |
|---|---|---|
| `literal:` | `literal:eyJ...` | Raw value verbatim after the colon. |
| `env://` | `env://MY_JWT` | Read from environment variable. |
| `file://` | `file:///abs/path` | File contents (trailing newline stripped). |
| `keychain://` | `keychain://uscis/access_token` | OS keychain item (macOS via `security` CLI; Linux/Windows return a "not implemented" error). |

File mode is content-aware: `0600` if any `literal:` value is present, `0644` if the file is a pure manifest of references.

#### Threat model for `keychain://`

The keychain resolver shells out to `/usr/bin/security`. Items it creates have ACLs scoped to the `security` binary itself, not to synapse — meaning **any process running as your user can read them silently** by shelling out to `security` with the matching service/account. What `keychain://` actually protects against:

- Other users on the same machine
- Plaintext-on-disk leaks (the file contains only URIs; `defaults.json` becomes commit-safe when no `literal:` refs are present)
- Disk theft (the keychain DB is encrypted at rest, unlocked at login)

What it does **not** protect against:

- Any process running as you that knows the service/account (`security find-generic-password -s plexus -a <…> -w` works without a prompt)
- A compromised synapse binary

This is the standard tradeoff for "shell out to `security`" tools (cargo, gcloud, aws-cli all do the same). If you need per-process sandboxing, the upgrade path is to use the Security framework C API directly with code-signing-anchored ACLs — not implemented in v1.

### Managing defaults: the `_self` subcommand

```bash
# Inspect current state — decodes JWTs, flags expired tokens
synapse _self <backend> show

# Set a value (auto-wrapped as literal: unless you provide a scheme)
synapse _self <backend> set cookie access_token "eyJ..."
synapse _self <backend> set header X-API-Key "env://MY_API_KEY"

# Read from stdin (always literal:, bypasses the scheme heuristic)
cat my-jwt.txt | synapse _self <backend> set-from-stdin cookie access_token

# Import a JWT file into cookies.access_token
synapse _self <backend> import-token ~/Downloads/jwt.txt

# Resolve a single value (debug): prints the resolved concrete value
synapse _self <backend> resolve cookie access_token

# Remove entries
synapse _self <backend> unset cookie access_token
synapse _self <backend> clear --yes
```

`show` decodes JWT-shaped values and prints `alg`, `kid`, `iss`, `aud`, `sub`, `exp` (with human-relative rendering like `"expired 15d 23h ago"` or `"valid for 4m 32s"`), `iat`, `preferred_username`, `email`. Signatures are never printed.

### Per-invocation overrides

Four CLI paths populate credentials at call time, highest priority first:

```bash
# --token takes a JWT, sends it as Cookie: access_token=<jwt>
synapse --token eyJ... <backend> <method>

# SYNAPSE_TOKEN env
SYNAPSE_TOKEN=eyJ... synapse <backend> <method>

# --token-file reads from a path
synapse --token-file ~/tokens/uscis <backend> <method>

# --cookie / --header add arbitrary key/values on the upgrade request
synapse --cookie session=abc --header X-Trace-Id=xyz <backend> <method>
```

Invocation flags override the defaults store per key. CLI flags apply only to that one call; they don't write to disk.

### Legacy token file

The pre-SELF convention was `~/.plexus/tokens/<backend>` holding a raw JWT. On the first `loadDefaults` call for a backend that has a legacy file but no `defaults.json`, synapse auto-migrates: the JWT becomes `cookies.access_token = literal:<jwt>` in the new file, and the legacy file is deleted. An INFO log line names both paths and suggests upgrading to keychain storage.

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

## Credentials end-to-end

Plexus has a first-class **credential primitive** — `Credential<T>` is a sealed type that activation code can return as part of any response. The framework intercepts it at serialization time, replaces the value with a sentinel, routes the real value through a sidecar, and (for HTTP transport with cookie-attach metadata) projects it to a `Set-Cookie` header. Generated TypeScript clients auto-extract the credential on receipt, auto-store it on the active session, and auto-attach it on subsequent calls that require it.

### Backend: return a credential

```rust
use plexus_auth_core::{Credential, CredentialMinter, CredentialMetadata, CredentialKind, AttachmentSite};

#[derive(Debug, Clone, Serialize, JsonSchema, plexus_macros::Credentials)]
pub struct LoginResponse {
    user_id: String,
    #[credential(
        kind = "bearer",
        attach_as(header = "authorization"),
        scheme = "Bearer ",
    )]
    token: Credential<String>,
}

#[plexus_macros::activation(namespace = "auth", version = "1.0.0")]
impl Auth {
    /// Log in with a username/password.
    #[plexus_macros::method]
    async fn login(
        &self,
        minter: &CredentialMinter,
        /// Username
        username: String,
        /// Password
        password: String,
    ) -> impl Stream<Item = AuthEvent> + Send + 'static {
        stream! {
            // ... validate credentials ...
            let token = minter.mint("eyJ...".to_string(), bearer_metadata());
            yield AuthEvent::LoggedIn(LoginResponse { user_id: "u-1".into(), token });
        }
    }
}
```

### Wire envelope (transparent to your code)

```json
{
  "type": "data",
  "content": {
    "user_id": "u-1",
    "token": {"$credential": "cred_0"}
  },
  "_credentials": {
    "cred_0": {
      "value": "eyJ...",
      "metadata": {"kind": {"kind": "bearer"}, "attach_as": {"site": "header", "name": "authorization"}, "scheme": "Bearer "}
    }
  }
}
```

Over HTTP with `kind = "cookie"` and `attach_as(cookie = "session")`, the framework strips the value from the JSON and emits `Set-Cookie: session=…` on the response. Over stdio (no header surface) the value remains in the sidecar.

### Generated TypeScript client

```typescript
import { createAuthClient } from './client/auth';
import { InMemorySessionRegistry } from './client/plexus_auth';

const session = new InMemorySessionRegistry();
const auth = createAuthClient(rpc, { session });

// First call: credential auto-stored on the session
await auth.login('alice', 'hunter2');
// → session.store('default', [<bearer token>])

// Subsequent call that declares requires_credential: { kind: "bearer", scopes: [...] }:
//   the matching credential is selected and attached automatically.
const room = await chat.send({ message: 'hello' });
//   → Authorization: Bearer eyJ... was sent on the WS upgrade or RPC param
```

### Generated Rust client

```rust
use my_backend_client::{AuthClient, ChatClient};
use my_backend_client::plexus_auth::{InMemorySessionRegistry, Session};
use std::sync::Arc;

let session = Arc::new(InMemorySessionRegistry::new());
let auth = AuthClient::new(rpc.clone(), session.clone());
let chat = ChatClient::new(rpc.clone(), session.clone());

// First call: credential auto-stored on the active session
auth.login("alice", "hunter2").send().await?;

// Subsequent call: the matching credential is selected and attached automatically.
// Builder methods scope per call:
chat.send("hello").with_session("default").send().await?;
chat.send("hello").no_session().send().await?;  // explicit opt-out
```

### What's wired today vs. what's still v1-stub

| Capability | Today |
|---|---|
| Backend returns `Credential<T>` from any method | ✅ |
| Dispatch builds wire envelope with sentinels + `_credentials` sidecar | ✅ |
| Cookie-attach projects to `Set-Cookie` on HTTP, kept inline on stdio | ✅ |
| IR carries `mdCredentials` + `mdRequiresCredential` (synapse decodes them) | ✅ |
| TypeScript clients auto-store credentials on a `SessionRegistry` | ✅ (CG-1) |
| TypeScript clients auto-attach on `requires_credential` methods | ✅ (`in_rpc_param` site fully threaded; `header`/`cookie`/`first_frame` are stubbed for v1.1) |
| Rust clients auto-store + auto-attach via `.with_session(name)` / `.no_session()` builder | ✅ (CG-2) |
| Bare `cargo publish` to crates.io | pending end-of-wave |

### Macro Reference

| Macro | Purpose |
|---|---|
| `#[plexus_macros::activation(...)]` | Decorates an `impl` block. Carries `namespace`, `version`, `description`, `auth_posture` (`"required"` \| `"mixed"` \| `"anonymous"`). |
| `#[plexus_macros::method]` | Decorates an `async fn`. Reads `///` doc comments on the function and on each parameter. Explicit `description = "..."` and `params(name = "...")` still work and win when set. |
| `#[plexus_macros::child(...)]` | Declares a child router accessor; generates the `ChildRouter` glue. |
| `#[derive(plexus_macros::Credentials)]` | Marks a return-type struct/enum whose fields include `Credential<T>`. Pairs with `#[credential(kind = ..., attach_as(...), ...)]` per field. |
| `#[plexus_macros::from_auth(...)]` | Pulls a parameter from `AuthContext` at dispatch time (auth resolver). |

Doc-comment fallback precedence (function + params):
1. Explicit attribute argument wins (`description = "..."`, `params(name = "...")`).
2. `///` doc comment on the item (function or parameter).
3. Empty / `None` if neither.

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

## Docker

Synapse can be run in a Docker container for easy deployment and testing.

### Build Docker Image

```bash
# From the synapse directory
docker build -t synapse:latest .

# Or use docker-compose
docker-compose build
```

### Run with Docker

```bash
# Show help
docker run --rm synapse:latest

# Call a method (requires --network host to access local backends)
docker run --rm --network host synapse:latest substrate echo.echo --message "hello"

# Enable debug logging
docker run --rm --network host -e LOG_LEVEL=debug synapse:latest substrate hash

# Connect to remote backend
docker run --rm synapse:latest -H 192.168.1.100 -P 5001 backend method

# Run protocol validation
docker run --rm --network host synapse:latest _self debug localhost 4444 substrate
```

### Docker Compose

```bash
# Show help
docker-compose run --rm synapse

# Run a method
docker-compose run --rm synapse synapse substrate echo.echo --message "hello"

# Debug connection
BACKEND=substrate PLEXUS_PORT=4444 docker-compose --profile debug up

# With environment variables
LOG_LEVEL=debug docker-compose run --rm synapse synapse substrate hash
```

### Image Details

- **Base**: debian:bookworm-slim (minimal runtime)
- **Size**: ~200MB (multi-stage build)
- **User**: Non-root user (uid 1000)
- **Default log level**: error (quiet mode)
- **Network**: Use `--network host` to access localhost backends

## Development

```bash
cabal build                        # Build
cabal test                         # Run tests (needs backend on localhost:4444)
cabal install                      # Install synapse binary
cabal build -f build-examples      # Build optional examples
```

## License

MIT
