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

# Lists — repeated flags or comma-separated (see Lists below)
synapse substrate tags set --tags backend --tags critical
synapse substrate tags set --tags backend,critical,urgent
synapse substrate tags set --tags '"red, green",blue'   # an element with a comma

# Boolean flags (bare flag = true)
synapse substrate cone create --ephemeral
synapse substrate cone create --ephemeral=false

# A value may begin with dashes; --key=value works too
synapse substrate echo once --message "--- starts with dashes"
synapse substrate echo once --message="$(cat note.md)"

# Raw JSON override
synapse substrate cone chat -p '{
  "identifier": {"type": "by_name", "name": "my-assistant"},
  "prompt": "hello"
}'
```

Path parameters (`--path`, `--working_dir`, etc.) expand `~`, resolve relative paths, and substitute `$ENV` variables automatically. **No other value is touched**: `--body 'costs $HOME'` sends `costs $HOME`. (Before 3.14.2 every value had `$VAR` substituted — a ticket body read from a file with `--body "$(cat body.md)"` lost its literal `$HOME`, `$PATH`, … The shell already expands what you leave unquoted, so synapse doing it again only ever rewrote text you had protected.)

### Flag values

Once synapse has found the method, it reads each flag by its parameter's type:

| typed | read as |
| -- | -- |
| `--body VALUE` | the next argument is the value, whatever it begins with: `---`, `--force`, `-5`, `--`, or nothing (`''`) |
| `--body=VALUE` | everything after the first `=`, which may be empty or contain `=` |
| `--summary` (a boolean) | `true`; it takes the next argument only when that is `true` or `false` (or `1`, `0`), so `--summary --limit 3` is two flags |
| `--summary=false` | `false` |
| `--no-such x` (names no parameter) | `Unknown parameter: --no-such`, nothing is sent; such a flag takes the next argument unless it begins with `--` |

So a markdown body with frontmatter passes as itself: `--body "$(cat body.md)"` or `--body="$(cat body.md)"`. Only the flag's name is normalized (`--blocked-by` is `blocked_by`), never its value. An argument that no flag takes (`--summary yes`) is an error, and nothing is sent. `--help` is always help, even right after a flag: to send the text `--help`, write `--body=--help`. Synapse's own options (`-P`, `-n`, `--json`, …) still go before the backend name.

**Changed in 3.14.1.** Through 3.14.0 a value that began with `--` was read as the next flag (`--body "---…"` failed with `Unknown parameter: --_`), any flag followed by another flag was bare whatever its type, and `--body=…` was read as a flag named `body=…`.

### Lists

A list parameter takes its elements one flag at a time, several to a flag, or both. When the elements are scalars (strings, integers, numbers, booleans, or a named type that is one of those, such as an id or a string enum), each flag value is read as one comma-separated record, with double quotes for an element that contains a comma:

| flag value as synapse receives it | elements |
| -- | -- |
| `a,b,c` | `a` `b` `c` |
| `a` | `a` |
| `"a,b,c,d","e,f,g"` | `a,b,c,d` and `e,f,g` — a double-quoted element keeps its commas |
| `"say ""hi""",b` | `say "hi"` and `b` — `""` inside quotes is one quote |
| `a, b ,c` | `a` `b` `c` — space around an unquoted element is dropped |
| `a,,b,` | `a` `b` — an empty unquoted element is dropped |
| `"",a` | the empty string and `a` |
| `"a,b` or `"a"x,b` | an error naming the parameter and the value; nothing is sent |

Repeated flags append in order: `--tags a,b --tags c` is `a` `b` `c`. Elements are then typed as before (`--ids 1,2,3` sends `[1,2,3]` for an integer list). A list of objects, of lists, or of untyped values is not split — each flag value is one element, since JSON has commas of its own. Nothing else splits: a comma in `--title` is just a comma, and `-p '{…}'` is sent as written. `<method> --help` marks each list parameter with the forms it takes.

**The shell removes double quotes before synapse runs**, so an element that contains a comma needs its quotes protected by single quotes:

| typed | what synapse receives |
| -- | -- |
| `--commits a,b,c,e` | `a,b,c,e` — four elements |
| `--multi "a,b,c,d","e,f,g"` | `a,b,c,d,e,f,g` — seven elements; the shell dropped the quotes |
| `--multi '"a,b,c,d","e,f,g"'` | `"a,b,c,d","e,f,g"` — two elements |

**Changed in 3.14.0.** Through 3.13 every flag value was exactly one element, commas included (`--tags a,b` sent `["a,b"]`). Now a single element that contains a comma must be quoted (`--tags '"a,b"'`), and a value that begins with `"` is read as a quoted element.

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

## Notes: `_note`

A note is a thought written down before it is a ticket: dated, never
titled, kept in one place — `$SYNAPSE_NOTES_DIR`, else `~/.plexus/notes`,
whatever directory or backend synapse is run from. Each note is one
markdown file, `<id>.md`, whose frontmatter holds only its `date` (UTC) and
whose id is the moment it was taken (`20261010-150312`; `-2`, `-3` for more
in the same second). Writing, listing and reading notes is local: no
backend need be running.

```bash
synapse _note add "the palette should remember its last query"
echo "watch drops events when the laptop sleeps" | synapse _note add
synapse _note list                 # waiting notes, newest first (--all: upgraded too; --json)
synapse _note show 20261010-1503   # an id, or the start of exactly one
synapse _note path                 # where notes live
```

A note becomes a ticket with `upgrade`: `tickets.create` on the backend
(`--backend`, default `menger`), reached as any synapse call is — so its
port goes before `_note`:

```bash
synapse -P 4455 _note upgrade 20261010-1503 --epic MENGER --parent MENGER-2
# upgraded 20261010-150312 → MENGER-41  (menger tickets.create)
```

The ticket's title is `--title`, else the note's first line with words
(markdown marks dropped, at most 80 characters, cut at a word); its body is
the note's text and a line saying when it was noted; `--kind` and
`--parent` are passed only when given. The note is kept, its frontmatter
now naming the `ticket`, the `backend` and when it was `upgraded`, and
`list` no longer shows it (`list --all` does, with `→ MENGER-41`). A note
already upgraded is refused; a backend's refusal is printed and leaves the
note as it was.

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
