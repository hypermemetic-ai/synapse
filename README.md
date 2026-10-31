# Synapse

**A CLI that writes itself from your API.**

Synapse is the command-line frontend for [Plexus RPC](../plexus-protocol/) — a streaming JSON-RPC 2.0 protocol built for LLM orchestration. Define methods in Rust, and Synapse discovers them at runtime: commands, help text, parameter validation, and output rendering all come from the schema. No codegen, no static config.

```
                  ┌─────────────┐
                  │  Substrate   │  Rust backend service
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
    #[plexus_macros::method(
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

    #[plexus_macros::method(
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

The macro generates the JSON-RPC server, `Activation` trait impl, method enum, and JSON Schemas — all from the function signatures and `JsonSchema` derives. Register it with the service:

```rust
let service = DynamicService::new("substrate")
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

## A service behind TLS: `--url wss://…` (SCAFFOLD-21, 3.15.0)

A hosted app sits behind a proxy that terminates TLS and routes its socket
at a path — every app on the house's edge is `wss://<app>.menger.sh/ws`.
Name it with `--url`, before the backend name like every global option:

```bash
synapse --url wss://hostdemo.menger.sh/ws hostdemo greeter hello --name Ada
synapse --url wss://hostdemo.menger.sh/ws -j hostdemo greeter count --to 3
synapse --url wss://hostdemo.menger.sh/ws -s hostdemo greeter        # the schema
synapse --url wss://hostdemo.menger.sh/ws                            # the backend there, [OK]
synapse -H hostdemo.menger.sh --tls --path /ws hostdemo greeter hello --name Ada   # the same address
```

| option | meaning |
| -- | -- |
| `--url ws[s]://HOST[:PORT][/PATH]` | the whole address; the port defaults to the scheme's (80, 443), the path to `/`. Not with `-H`/`-P`/`--tls`/`--path`; `http(s)://` is refused with the `ws(s)://` it means |
| `--tls` | dial `wss://` at `-H`/`-P` (port 443 unless `-P`) |
| `--path PATH` | the socket's path at `-H`/`-P` (default `/`) |
| `--cacert FILE` | with wss, also trust the CA certificate(s) in this PEM file (repeatable) |

**The certificate is always verified**: the chain against the system's trust
store (the distribution's bundle; `SYSTEM_CERTIFICATE_PATH` points synapse at
another directory or file) and the name against the host, which is also sent
as SNI. A failed check fails the handshake before any of the request — a
token included — is sent. There is no switch that skips the check: for a local
CA (a throwaway proxy, `caddy … tls internal`), pass its root with `--cacert`.

**Everything rides the same address**: the `_info` probe, calls, streams and
their reconnects (SYNAPSE-20 — the outage line names the URL), `-s`, `-i`,
`_find`, bidirectional requests and their `respond`, `_login` / `_logout` and the
session every later call carries. The upgrade's `Host` is the host alone on 443
(`host:port` otherwise). A registry lists plain `ws://HOST:PORT` backends, so an
address behind TLS or on a path is never auto-registered: name it each time,
or put `--url` in a shell alias.

**Sessions are kept per URL.** A plain `ws://HOST:PORT/` keeps the key it
always had (`~/.plexus/sessions/HOST_PORT/`, keychain `backend=HOST:PORT`), so
no stored session moves. Any other address is keyed by its URL with the port
written — `~/.plexus/sessions/wss_hostdemo.menger.sh_443_ws/AGENT.json`,
keychain `backend=wss://hostdemo.menger.sh:443/ws` — so a ws and a wss backend
on one host, or two paths, never share a session. Sign in with the options
the calls use:

```bash
synapse --url wss://app.example/ws _login app --key builder --as builder
synapse --url wss://app.example/ws app tickets health     # carries builder's session
```

An agent's key proof names the server's socket origin as its audience:
`ws://HOST:PORT` on a plain address (as before), `wss://HOST` (`:PORT` when
not 443) behind TLS — what a service run with `--public-url https://HOST` accepts
(plexus-login's `socket_urls`). The path is not part of it.

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

### Help and search: `_find`

A listing (`synapse -P 4455 menger`, `… menger tickets`) is one line per
activation and method; a method's page (`… tickets set_status --help`) is its
usage, aligned options and examples (SYNAPSE-12, SYNAPSE-13). When you don't
know the name, search for it (SYNAPSE-17):

```bash
$ synapse -P 4455 _find menger comit
5 matches for 'comit' (as 'commit') in menger

METHODS
  tickets link_commits      Link commits to a ticket, or to many
  tickets unlink_commits    Unlink commits from a ticket

PARAMETERS
  deployments record        [--commits <string[]>]    Every commit in previous..revision…
  tickets link_commits      --commits <string[]>      Commit references, in link order
  tickets unlink_commits    --commits <string[]>      Commit references to remove…

Run 'synapse -P 4455 menger tickets link_commits --help' for its options.
```

`synapse [OPTIONS] _find <backend> [activation…] <words…> [--limit N] [--methods | --params]`

- Every method and parameter whose name or description matches **all** the
  words, best first: an exact name, then a name's prefix (or a `snake_case`
  segment's), then an abbreviation (`setpar` → `set_parent`, prefixes of its
  segments in order) or a typo (one edit for 4–7 letters, two beyond), then a
  word of a description. Ties: the shorter path, then alphabetical.
- Grouped `METHODS` · `PARAMETERS` · `MENTIONED IN` (found only in a
  description; left out for a method already listed above). A parameter line is
  its method, its flag as the method's page shows it (`[brackets]` when
  optional) and its description, with `[values: …]` under it.
- The header says what a typo or an abbreviation was taken as; on a terminal
  the matched characters are bold (piped, or with `NO_COLOR`, plain text).
- Activation names before the words narrow it: `_find menger tickets setpar`.
  `--limit N` (default 20; the footer says how many more), `--methods` /
  `--params` for one group. Nothing found: the three nearest names, exit 1.
- `-j` (before `_find`): one JSON object per hit — `kind` (method | param),
  `path`, `param`, `type`, `required`, `description`, `values`, `group`,
  `score`, `matched`.
- Under login it searches the caller's view of the schema: a method the caller
  can't see is never a hit. The IR is kept in `~/.cache/synapse/ir` under the
  root schema's hash (and the caller's view), so a search costs one schema call
  while nothing has changed; `--no-cache` walks the backend again.

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
synapse substrate echo once --message @note.md

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
| `--body @body.md` | the file's contents, UTF-8, exactly as they are (3.14.8, SYNAPSE-10): no argument carries the value, so a body may be as large as the backend accepts; `~/` and a relative path resolve as the shell's would; a file that cannot be read is a refusal naming the flag and the path |
| `--body -` | standard input, read once however many values ask for it; it cannot be combined with `--bidir-mode json`, which reads stdin too |
| `--handle @@alice` | the text `@alice`: one `@` is dropped, so a value that really begins with `@` can still be sent |
| `--summary` (a boolean) | `true`; it takes the next argument only when that is `true` or `false` (or `1`, `0`), so `--summary --limit 3` is two flags |
| `--summary=false` | `false` |
| `--no-such x` (names no parameter) | `Unknown parameter: --no-such`, nothing is sent; such a flag takes the next argument unless it begins with `--` |

So a markdown body with frontmatter passes as itself: `--body @body.md`, or `--body "$(cat body.md)"` while it is under the shell's 128 KB argument cap. The same sources serve `-p`: `-p @params.json` and `-p -`. Only the flag's name is normalized (`--blocked-by` is `blocked_by`), never its value. An argument that no flag takes (`--summary yes`) is an error, and nothing is sent. `--help` is always help, even right after a flag: to send the text `--help`, write `--body=--help`. Synapse's own options (`-P`, `-n`, `--json`, …) still go before the backend name.

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

### The answer in a file: `-o FILE` (SYNAPSE-39, 3.17.3)

An answer you mean to read goes to a file, then you read the file — never
through a filter that may stop reading early (a `grep` that cuts a list short,
a `head` that closes on a write):

```bash
synapse -P 4455 -o /tmp/t.json -j menger tickets list --summary true
synapse -P 4455 --output /tmp/t.txt menger tickets get --id MENGER-12
```

- `-o FILE` / `--output FILE` is a global option (before the service name,
  as `-j`). What the call prints goes to FILE instead of stdout, in the same
  form a pipe would get (`-j` lines with `-j`, the rendered answer without);
  stdout gets nothing, stderr what it always gets.
- It is written to a temporary file beside FILE and renamed over FILE when
  the call ends with 0, so a reader never sees half an answer. FILE is
  `0600` (an answer may carry a session token — `session login`).
- A call that ends with anything else leaves no FILE — an older FILE is
  removed too, so nothing stale is read as this call's answer — and no
  temporary file. The exit code is the call's (75, 77, 69…).
- A stream (`watch`) has no end: its items are appended to FILE and flushed
  as each comes. A stream that fails keeps what it wrote.
- A write's answer is written whole, whoever reads it.
- What must be seen while the call waits goes to **stderr**, not FILE: a
  two-way request (the interactive prompt, `--bidir-mode json`'s and
  `respond`'s request lines — the answer still goes to FILE), and under
  `-j` an error or a refusal (its `-j` line goes with the failed run's FILE,
  so a refused call says why on stderr and exits 77).
- `-o -` is stdout. A FILE that cannot be made (no such directory, a
  directory) is said on stderr and the run **exits 73** before anything is
  sent; a write to it that fails later (a full disk, a file size limit) is
  no outage — the call ends, unretried — and the run **exits 74**.
- A SIGTERM or SIGHUP ends the run as a failed one (143, 129): no FILE and
  no temporary file. A SIGKILL can leave the temporary file
  (`FILE<n>.tmp`) behind.

## Outages: reconnecting (SYNAPSE-20)

synapse rides out a backend that goes away — a restart, a promote, a
network blip — for **ten minutes** by default.

- **A lost connection is noticed**: the server closing it, a reset, or
  silence. While a call is open, synapse pings after 15 s with nothing heard
  and counts the connection lost when 10 s more pass without an answer
  (`PLEXUS_KEEPALIVE=IDLE[,TIMEOUT]` in seconds, `0` for none). A call is
  never left waiting on a dead socket.
- **It retries for the window**: 0.5 s, doubling, to 15 s between tries.
  `--reconnect-for 10m` (`90s`, `1h`, `0` = never; or
  `SYNAPSE_RECONNECT_FOR`) sets how long. A backend that is down when synapse
  starts is waited for the same way (a wrong port that answers something
  other than Plexus fails at once). Progress goes to **stderr only**:

  ```
  synapse: connection to 127.0.0.1:4455 lost (the connection was closed) — reconnecting for up to 10m
  synapse: retrying 127.0.0.1:4455 in 4s (4s of 10m)
  synapse: reconnected to 127.0.0.1:4455 after 8s
  ```

- **A read is sent again; a write is not.** A method is a read when its
  schema's `group` is `read` (else when its name is a read's: `get`, `list`,
  `watch`, …); anything else, and anything bidirectional, is a write. A
  write whose request went out before the connection was lost may have
  happened, so synapse says so and **exits 75** (`connection to … lost
  before an answer — the write may have happened; check before sending it
  again`). A write that never left (the backend was down when it was
  sent) is sent once the backend is back.
- **A stream resumes.** `watch`, `await_reply` and any other streaming read
  are subscribed again with the same params. A method that takes `since`
  (menger's `await_reply`) gets `since` = the unix time the call began
  unless you gave one, so an answer given during the outage is still
  delivered. Under `-j` a line marks the resume, in the stream on stdout:

  ```json
  {"type":"reconnected","outage_secs":8,"attempts":4,"since":1792204276}
  ```

  (`since` only when the resumed call carried it.) The resumed stream
  starts as a new call does — `await_reply` says `waiting` again. Without
  `since`, events during the gap may be missing: re-read what you need.
  Other modes say it on stderr.
- **After the window**, synapse **exits 69** with the reason (`… unreachable
  for 10m (connection refused) — giving up`). It never hangs.
- **A closed stdout is not an outage** (SYNAPSE-26, 3.17.2). When the
  reader of synapse's stdout goes — `… | head -1`, `… | grep -m1` — synapse
  stops at once and **exits 141** (what a shell says of a filter its pipe
  closed on), saying nothing: no retry, no reconnect, a stream unsubscribed,
  a write sent once. Whatever window `--reconnect-for` sets, and on `-j` and
  the plain printer alike. (Before 3.17.2 the EPIPE was taken for a lost
  connection and a write was sent again for as long as the pipe stayed
  closed.) The socket's own broken pipe — the service gone — is still an
  outage. A stream notices its reader is gone only when it next prints: once
  `tickets watch | head -1` has its line, synapse keeps running until the
  next event arrives, then exits 141 — with no event, it waits. A run
  already ending with a code of its own — 75, 77, 69 — keeps it; 141 only
  replaces a success.

Many agents' calls at once ride out a restart too (SYNAPSE-15): a call
whose stored session the restart ended signs in again from its key under a
lock on that session, and a call that waited for the lock takes the session
the first one made. Sixteen or thirty-two parallel calls after a restart
sign in once and all succeed; a connection refused for the server's
connection limits (429) is waited out like any outage.

| exit | meaning |
| -- | -- |
| 69 | the backend stayed unreachable for the whole window |
| 75 | a write went out and its answer never came — it may have happened |
| 77 | the service refused the call (AUTH-8) |
| 73 | `-o FILE` could not be made (SYNAPSE-39): nothing was sent |
| 74 | a write to `-o`'s FILE failed (SYNAPSE-39): the call ended there |
| 141 | stdout's reader closed the pipe (SYNAPSE-26): nothing more was sent |

Through the connection holder (SYNAPSE-28, below) all of this holds as it is.

## One round trip a call (SYNAPSE-28, 3.16.0)

A call costs one round trip and the server's time. Against app.menger.sh
(77 ms away) `tickets health` takes **81 ms**, where it took 4.6 s.

```
$ synapse --url wss://app.menger.sh/ws --as builder menger tickets health   # the first: 2.6 s
$ synapse --url wss://app.menger.sh/ws --as builder menger tickets health   # every one after: 81 ms
```

Two things make it so:

- **A connection held between runs**, as ssh's ControlMaster holds one. The
  first call to a backend starts a holder (`synapse _hold`, a process of
  synapse's own) that keeps the signed-in WebSocket open and listens on a
  unix socket: `$XDG_RUNTIME_DIR/synapse/ADDRESS@AGENT-VERSION.sock` (0600,
  in a 0700 directory of yours). Later runs hand it their requests and read
  the replies back, so the TCP, TLS and upgrade handshakes (about 0.3 s to a
  hosted service) are paid once. One holder per address and agent (`--as`), never
  shared between agents; it exits after **10 minutes** with nothing to do
  (`--hold-for 30m`, or `SYNAPSE_HOLD_FOR`). `--no-hold` (`SYNAPSE_NO_HOLD=1`)
  skips it: that run dials one connection of its own and closes it. A call
  that brings its own credential (`-t`, `--token-file`, `--cookie`,
  `--header`, `PLEXUS_SESSION`) never uses a holder.
- **Send first, check on failure.** synapse keeps what a call needs from the
  backend in `~/.cache/synapse/schemas/` (`$XDG_CACHE_HOME`): where the
  backend is, its `_info` as your agent was answered, each activation's
  schema (`PATH@VERSION-HASH.json`, pinned by `PATH.pin`) and a **plan** per
  method (its schema, its activation's deprecation, the types its flags parse
  by: kilobytes, where menger's `tickets` schema is 2.6 MB). A call to a
  method it has a plan for is built from the plan and sent: no probe, no
  schema fetch. The probe and the fetch run, on the same connection, only
  when that fails as the schema explains: a method or flag the cache does not
  know, or the server refusing what the cache said (a method it does not
  have, under the AUTH gate one only admin holds; params with a field
  missing, unknown or of another type). Those refusals come before the
  method runs, so the call is then sent again, a write too; the plans it
  misled are forgotten. Help, listings, `-s`, `-i` and `_find` always read
  the server.

Everything else is as before. Outages are ridden out by the same rules, a
read sent again and a write never resent blind (exits 69 and 75), because
the holder reports a lost connection, and whether the request had gone out,
exactly as a connection of the run's own would. A session that ended is made
again from the key by the run, and the holder dials a connection with the
new one beside the old, closing the old when idle. A run that goes away
mid-stream (Ctrl-C) ends its call at the server. Twenty runs starting at
once share one connection, and the reads they all ask first (the schema,
`_info`) are asked once.

Measured against app.menger.sh (ping 77 ms), 2026-10-22:

| | 3.15.0 | 3.16.0 |
| -- | -- | -- |
| `tickets health`, warm | 4,615 ms | 81 ms |
| `tickets queued`, warm | 7,398 ms | 84 ms |
| 20 `tickets health` at once, warm | 31.6 s | 107 ms |
| the first call (no holder, nothing cached) | — | 2.6 s |
| the first call after the holder exited (cache warm) | — | 380 ms |
| `--no-hold`, cache warm | — | 382 ms |
| 20 at once, no holder, nothing cached | — | 3.1 s |

The 0.9 s that went before every call in 3.15.0 was a second `_info` probe,
made anonymously on a connection of its own to check the name the first
probe had already answered; a run now probes at most once.

## Code Generation

Synapse emits a structured Intermediate Representation for building typed clients:

```bash
$ synapse --emit-ir substrate > substrate.ir.json
```

```json
{
  "irVersion": "2.1",
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

### IR 2.1: read whole, or refused by name (SYNAPSE-CC-8, 3.17.0)

Every schema is read whole or refused; nothing is dropped. The reader
(`src/Synapse/IR/Read.hs`) is pure: a value and the refusals met.

- **Unions.** A `oneOf` / `anyOf` is a string enum (every member an `enum`
  or a `const` of strings: `TransitionField` is `status, kind, archive`), a
  tagged enum (`KindEnum`, `keDiscriminator` the property every member gives
  a string `const` — `type`, `kind`, `event`, …; a newtype variant carries
  its struct's fields), an externally tagged enum (`keRepr: "external"`,
  `keDiscriminator: ""`: `{"browser": {…}}`), an untagged union of named
  types (`keRepr: "untagged"`, each variant's `vdRef`), or a nullable
  (`RefOptional`). `keRepr` is left out when internal, so 2.0's enums read
  as before. Any other union is refused, naming the member it could not read.
- **Refusals.** `irRefusals` lists each site the IR cannot type
  (`rfSite`, `rfKeyword`, `rfVariant`, `rfDetail`; left out when empty).
  Where a refused site stood the IR says `RefUnknown`; a refused type is left
  out and its uses say `RefUnknown`. With refusals, `synapse -i` still prints
  the IR on stdout, names each site on stderr and **exits 65**; no generator
  may run on it. Calls, help, flags and `_find` ignore refusals.
- **Sites** are `<ns.Type>[.<variant>].<field>`, `<method path>(<param>)`,
  `<method path> returns`, with `[]` (element), `?` (optional), `{}` (map
  value) and `.N` (tuple element): `tickets.Component.preview[].0`.
- **The JSON type.** A `serde_json::Value` is a type, not a gap: `true`,
  `{}`, annotations only, `type` naming all six JSON types (`["null",
  "boolean", "number", "string", "array", "object"]`, as plexus-core writes
  it since SYNAPSE-CC-9) or a `$ref` to a definition named `Json` is
  `RefJson` (`{"tag": "RefJson"}`): `JsonValue` / `Json` /
  `serde_json::Value` in a client. Still refused: a bare `{"type":
  "object"}` (neither properties nor additionalProperties), an array with no
  items, a type list that is neither one type (± null) nor all six. IR 2.0's
  `RefAny` reads as `RefJson`.
- **New refs.** `RefMap V` (an object with `additionalProperties: V` and no
  properties), `RefTuple [..]` (`prefixItems` with `minItems = maxItems` =
  its length), `RefRefined base {minimum, maxLength, …}` (only a bound the
  format does not imply: `minimum: 0` on a `uint64` adds nothing), and
  `RefChild "<path>"` (a static or dynamic child's return: its client). An
  rpc method with no `returns` is refused at `<path> returns`.
- **Injected params** (`x-plexus-source` other than `rpc`) are not in
  `mdParams`: they are `mdInjected` (`ipName`, `ipSource` verbatim,
  `ipDescription`; left out when empty). Help still shows them.
- **`mdInput`** is a method's `input` schema as a type, reserved for
  BIDIR-6; left out when absent (no server sends it yet).
- **The closed keyword list.** A schema in a type position uses only `type
  $ref properties required additionalProperties items prefixItems oneOf
  anyOf enum const format minimum maximum exclusiveMinimum exclusiveMaximum
  minLength maxLength pattern minItems maxItems default description title
  examples deprecated $schema $defs definitions x-plexus-source
  x-plexus-values`; any other (`allOf`, `multipleOf`, …) is
  refused by name.
- **Where a shared type lives.** The same type (name and structure) in
  several namespaces is kept once, in the namespace whose methods and types
  refer to it most; a tie goes to the namespace that is an ancestor of the
  others, then the shortest name, then alphabetical; the protocol root `""`
  only when no generated namespace has a copy.
- **Caches.** A plan or a `_find` catalogue holding another IR version
  (3.16.0's 2.0) is not read; it is built again.

`bench/IRBench.hs` (`cabal bench ir-bench`) times the builder alone over
menger's 2026-10-29 schemas (`test/fixtures/ir-coverage`), with no network;
`ir-coverage-test` holds one fixture per construct, menger whole (21
refusals; the IR equal to `menger-2026-10-29.ir-2.1.json`), and the guard
that every construct 2.0 already typed serializes as before.

## Client Generation (synapse-cc)

[**synapse-cc**](../synapse-cc/) (Synapse Compiler Collection) is a separate project that consumes Synapse's IR to produce typed client libraries. It imports plexus-synapse as a library, generates IR from a live backend, pipes it through [service-codegen](../service-codegen/) (a stateless Rust code generator), then handles merging, dependencies, building, and testing.

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
scripts/naming-gate.sh             # The naming gate: each line that says the retired word (-c: the count)
```

A Plexus backend is a *service* (NAMING-9; `~/AGENTS.md` "Naming"):
`test-suite naming-gate` runs the gate, so `cabal test` fails the day the
retired word is written again.

## License

MIT
