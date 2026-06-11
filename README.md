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
        /// Number of times to repeat
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

(`#[activation]` / `#[method]` are the current macro names — older docs say
`hub_methods` / `hub_method`; see `docs/MACRO_NAMING.md` for the mapping.
Doc comments on the method and each parameter become the schema
descriptions.)

The macro generates the JSON-RPC server, `Activation` trait impl, method enum, and JSON Schemas — all from the function signatures and `JsonSchema` derives. Register it with the hub:

```rust
// The hub root name ("echo_hub") is the backend identifier you type first
// at the CLI. It must differ from every registered activation's namespace —
// registration panics on a collision (Z2H-8).
let hub = Arc::new(DynamicHub::new("echo_hub").register(Echo));
```

That's the entire backend. No route tables, no handler boilerplate, no schema files. (This exact backend ships as `examples/echo.rs` in [plexus-rpc](../plexus-rpc/) — `cargo run --example echo`.)

### 2. Call it from Synapse

```bash
# Install
cd synapse && cabal install

# Synapse discovers the echo activation and its methods automatically
$ synapse echo_hub echo once --message "hi"
count: 1
event: echo
message: hi

$ synapse echo_hub echo echo --message "hello" --count 3
count: 1
event: echo
message: hello
count: 2
event: echo
message: hello
count: 3
event: echo
message: hello
```

Every method you add to the Rust backend is immediately available as a CLI command. The parameter names, types, descriptions, and validation all come from the schema. (Path shape: `echo_hub` is the backend — the hub root — then activation `echo`, then the method.)

### 3. See what's on the wire

Options go **before** the backend/path:

```bash
# Dry-run: show the JSON-RPC request without sending
$ synapse --dry-run echo_hub echo echo --message "hello" --count 3
{"id":1,"jsonrpc":"2.0","method":"echo_hub.call","params":{"method":"echo.echo","params":{"count":3,"message":"hello"}}}

# JSON mode: see the raw stream items
$ synapse --json echo_hub echo echo --message "hello" --count 2
{"content":{"count":1,"event":"echo","message":"hello"},"content_type":"echo.echo","metadata":{...},"type":"data"}
{"content":{"count":2,"event":"echo","message":"hello"},"content_type":"echo.echo","metadata":{...},"type":"data"}
{"metadata":{...},"type":"done"}

# Fetch the JSON Schema for the activation
$ synapse --schema echo_hub echo
{"namespace":"echo","version":"1.0.0","description":"Echo messages back","methods":[...]}
```

## The Registry

Plexus backends register themselves with a **registry** — a central service that tracks what's running and where. Since Z2H-6, registration is **server-side**: a backend built on current `plexus-transport` registers itself (name/host/port) at boot with the registry named by `PLEXUS_REGISTRY_URL` (default `ws://127.0.0.1:4444`), deregisters on SIGTERM, and entries that stop answering are lazily evicted when synapse fails to verify them. Opt a service out with `PLEXUS_NO_REGISTRY=1` or `.without_registry()`.

Synapse resolves bare backend names through the same endpoint, so you never need host:port pairs for registered backends:

```bash
# This just works — the registry resolves "zechodev" to its host:port
synapse zechodev greeter greet --name World
```

Two escape hatches:

- **Explicit target**: `synapse -H <host> -P <port> <backend> …` connects directly; no registry is consulted or required.
- **No registry at all**: if the backend itself sits on the registry endpoint's port (the out-of-the-box `:4444` case), synapse probes its `_info` and matches the name — bare invocation works with zero registry processes.

When a name doesn't resolve, synapse lists what the registry knows:

```bash
$ synapse nosuch greeter greet
Backend not found: 'nosuch' is not registered.

Backends self-register with the registry when they start (Z2H-6).
...
Available backends:
  zechodev       127.0.0.1:4454 [OK]
  ...
```

(`synapse` with no arguments prints the usage banner, not this list — the backend listing appears on resolution failure, or scan for live ports with `synapse _self scan`.)

Synapse does **not** auto-register backends it connects to with `-P` — client-side auto-registration was removed in Z2H-6. Registration belongs to the service's own boot path.

The key mental model: **don't think in terms of hosts and ports.** Backends register themselves; use them by name. The registry is the source of truth.

## Progressive Discovery

Once you're connected, just start typing. Each level of the path shows what's available, so you never need external docs:

```bash
# What methods does the echo activation have?
$ synapse echo_hub echo
echo v1.0.0

  Echo messages back

methods

  echo         Echo a message back the specified number of times.
              --count <integer>    Number of times to repeat
              --message <string>   The message to echo

  once         Echo a message once.
              --message <string>   The message to echo

# What does echo.echo need? (auto-help when required params are missing)
$ synapse echo_hub echo echo
echo.echo

Echo a message back the specified number of times.

Parameters:

  --count <uint32>  (required)
    Number of times to repeat

  --message <string>  (required)
    The message to echo

# Inspect the raw JSON Schema for an activation
$ synapse --schema echo_hub echo
```

Note: `synapse <backend>` with *no* path currently prints the global usage
banner rather than listing the backend's activations. To see the top level,
use `synapse --schema <backend>`, or give any path — an unknown one lists the
available methods and child plugins (see Error Messages).

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
# Inspect current state — decodes JWTs, flags expired tokens,
# shows OIDC login status / tenant / refresh-token presence
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

`show` decodes JWT-shaped values and prints `alg`, `kid`, `iss`, `aud`, `sub`, `exp` (with human-relative rendering like `"expired 15d 23h ago"` or `"valid for 4m 32s"`), `iat`, `preferred_username`, `email`. Signatures are never printed. Note that `show` and `resolve` print resolved `literal:` values in full — treat their output as sensitive.

Keychain **write** verbs (`set-secret`, `upgrade-to-keychain`, `import-token --to-keychain`) parse but return a friendly "requires SELF-8" error until the keychain write path ships; `keychain://` **reads** resolve today on macOS via the `security` CLI.

### Login lifecycle (OIDC)

The `_self` tree also owns the OIDC login lifecycle (UT-3) — no more JWT plumbing through `-t`:

```bash
# Password grant against the backend's IdP; stores access + refresh tokens
synapse _self <backend> login

# Refresh the access token now via the stored refresh token
synapse _self <backend> refresh

# Show or select the tenant/organization for this backend
synapse _self <backend> tenant

# Clear stored access/refresh tokens (keeps other defaults)
synapse _self <backend> logout
```

After `login`, ordinary invocations are hands-off: when the stored access token is expired, synapse **transparently refreshes** it via the stored refresh token before the call (an INFO log notes the refresh). If auto-refresh fails, the call proceeds with the stale token and a warning names the failure and suggests `synapse _self <backend> login`.

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

Templates are resolved in order: `.substrate/templates/{namespace}/{method}.mustache` (project-local) → `~/.config/synapse/templates/{namespace}/{method}.mustache` (user) → YAML-like fallback.

> **Hazard — stale user templates silently swallow output.** A user template
> wins over the schema-aware fallback by *namespace/method name alone*, even
> if it was generated against an old version of the backend — or a different
> backend whose activation happens to share the namespace (`echo` is the
> classic). A stale template whose section names no longer match the event
> shape renders **empty output**: the call succeeds, `--json` shows the
> events, human mode prints nothing. If a command streams data under
> `--json` but prints nothing without it, look in
> `~/.config/synapse/templates/<namespace>/` and delete or regenerate the
> stale templates. See [Troubleshooting](#troubleshooting).

Generate templates from the schema:

```bash
synapse --generate-templates substrate
```

## Code Generation

Synapse emits a structured Intermediate Representation for building typed clients:

```bash
$ synapse --emit-ir echo_hub > echo_hub.ir.json
```

Top-level keys: `irBackend`, `irHash`, `irVersion`, `irMetadata` (generator
tool/version, timestamp), `irPlugins` / `irPluginHashes` / `irPluginRequests`,
`irTypes`, `irMethods`. Trimmed excerpt:

```json
{
  "irBackend": "echo_hub",
  "irVersion": "2.0",
  "irTypes": {
    "echo.EchoEvent": {
      "tdName": "EchoEvent",
      "tdNamespace": "echo",
      "tdKind": {"tag": "KindEnum", "keDiscriminator": "type", "keVariants": ["..."]}
    }
  },
  "irMethods": {
    "echo.echo": {
      "mdName": "echo",
      "mdNamespace": "echo",
      "mdFullPath": "echo.echo",
      "mdDescription": "Echo a message back the specified number of times.",
      "mdParams": [
        {"pdName": "count", "pdType": {"tag": "RefPrimitive", "contents": ["integer", "uint32"]}, "pdRequired": true, "pdDescription": "Number of times to repeat"},
        {"pdName": "message", "pdType": {"tag": "RefPrimitive", "contents": ["string", null]}, "pdRequired": true, "pdDescription": "The message to echo"}
      ],
      "mdReturns": {"tag": "RefNamed", "contents": {"qnNamespace": "echo", "qnLocalName": "EchoEvent"}}
    }
  }
}
```

Types are deduplicated by content hash across namespaces. Streaming is inferred from return type structure (enum with >1 non-error variant = streaming).

## Client Generation (synapse-cc)

[**synapse-cc**](../synapse-cc/) (Synapse Compiler Collection) is a separate project that consumes Synapse's IR to produce typed client libraries. It imports plexus-synapse as a library, generates IR from a live backend, pipes it through [hub-codegen](../hub-codegen/) (a stateless Rust code generator), then handles merging, dependencies, building, and testing.

```bash
synapse-cc init [backend]          # scaffold synapse.config.json (backend
                                   #   inferred from a co-located service
                                   #   crate or the registry when omitted)
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
$ synapse echo_hub invalid-plugin
Command not found: 'invalid-plugin' at root

Available commands:

  Methods:
    call                Route a call to a registered activation
    hash                Get plexus configuration hash (from the recursive schema)
    hashes              Get plugin hashes for cache validation
    schema              Get plugin or method schema. Pass {"method": "name"} for a specific method.

  Child plugins:
    echo                Echo messages back

$ synapse echo_hub echo echo --mesage "hello" --count 1
Unknown parameter: --mesage

Did you mean: --message?
```

## Debugging & Testing

### Port Scanning

Discover backends running on the conventional port range:

```bash
$ synapse _self scan
Scanning 127.0.0.1 ports 4440-4459 for backends...

Found 4 backend(s):

  4452  zecho
  4453  registry-hub
  4454  zechodev
  4455  echo_hub
```

### Legacy debug verbs

`synapse _self debug|validate|test …` (layer-by-layer connection debugging
and protocol compliance checks) still parse, but are currently unreliable:
`debug` mis-parses IPv4 literals (`127.0.0.1` is read as host `127`, port
`0`) and `localhost` resolves to `::1` on dual-stack machines while most
backends bind only `127.0.0.1`. Until that's fixed, prefer:

```bash
# Is the method reachable and answering? (raw stream items)
synapse --json -P <port> <backend> <activation> <method> --param value

# What would be sent? (no connection needed)
synapse --dry-run -P <port> <backend> <activation> <method> --param value
```

## Troubleshooting

### A call "succeeds" but prints nothing

If `synapse --json …` streams data items but the same invocation without
`--json` prints nothing, a **stale user template** is swallowing the output.
Templates in `~/.config/synapse/templates/<namespace>/<method>.mustache` win
over the schema-aware fallback by name alone — including templates generated
long ago against a different backend that happens to use the same activation
namespace (`echo` is the classic collision). A template whose section names
no longer match the event shape renders empty output, silently.

Fix: inspect `~/.config/synapse/templates/<namespace>/`, delete the stale
files (the YAML-like fallback returns immediately), or regenerate with
`synapse --generate-templates <backend>`.

### Bare backend name doesn't resolve

`Backend not found: '<name>' is not registered` — the backend either isn't
running, didn't self-register (older `plexus-transport`, or
`PLEXUS_NO_REGISTRY=1`), or registered with a different registry than the one
synapse is querying (`PLEXUS_REGISTRY_URL`, default `ws://127.0.0.1:4444`).
Connect directly with `-H <host> -P <port>` to bypass the registry entirely.

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
