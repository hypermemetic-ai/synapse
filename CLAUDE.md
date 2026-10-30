# Synapse Development Guide

## Editor & Tools

- Use `codium --goto <file>:<line>` when I say "open this"
- Use `tre` to view directory structure

## Project Overview

**Synapse** is a Haskell CLI frontend for **Plexus** - a streaming JSON-RPC protocol for LLM orchestration.

The stack:
- **Substrate** (Rust) - The backend service that hosts activations
- **Plexus** - The RPC protocol layer (WebSocket, JSON-RPC 2.0, streaming)
- **Synapse** (this project) - Haskell CLI that discovers and invokes Plexus methods
- **plexus-protocol** (sibling package) - Shared types for the Plexus protocol

Synapse dynamically discovers available commands at runtime by querying `substrate.schema` and `substrate.full_schema`, then builds typed CLI parsers from the JSON Schema.

## Using Synapse CLI

Synapse is the primary way to interact with Plexus. Always check `--help` for correct argument formats.

### Basic Commands

```bash
# List available activations
synapse --help

# List commands in an activation
synapse cone --help

# Get help for a specific command
synapse cone chat --help
```

### Common Operations

```bash
# List cones (LLM agents)
synapse cone list

# Chat with a cone (use name or UUID)
synapse cone chat --id haiku-test --prompt "hello"

# List conversation trees
synapse arbor tree-list

# Execute bash command
synapse bash execute --command "echo hello"

# Check system health
synapse health check
```

### Output Modes

```bash
# Default: schema-aware rendering (clean output)
synapse cone list

# Raw content JSON (skip template rendering)
synapse --raw cone list

# Full JSON-RPC stream items
synapse --json cone list
```

### Inspecting Schemas

```bash
# View full schema for an activation
synapse --schema cone

# View schema for a specific method
synapse --schema cone chat
```

### Argument Formats

Arguments are derived from JSON Schema. The CLI accepts simple values:
- Strings: `--name "my-cone"`
- UUIDs: `--id "550e8400-e29b-41d4-a716-446655440000"` or just `--id my-cone`
- Booleans: `--ephemeral` (bare = true), `--ephemeral true`, `--ephemeral=false`
- A value from a file or stdin (SYNAPSE-10, 3.14.8): `--body @body.md`, `--body -`, `-p @params.json`; `@@text` is a literal `@text`. The shell caps one argument at 128 KB, so `--body "$(cat body.md)"` fails past that; `@file` never does (see README "Flag values")
- Values that begin with dashes (a markdown body with frontmatter): `--body @body.md`, `--body "$(cat body.md)"` or `--body="$(cat body.md)"`; a flag that takes a value takes the next argument whatever it begins with (see README "Flag values")
- Lists of scalars: `--tags a,b,c` or `--tags a --tags b`; an element containing a comma is double-quoted, with the quotes protected from the shell: `--tags '"a,b",c'` (see README "Lists")
- Raw JSON override: `-p '{"custom": "params"}'`

## wss (SCAFFOLD-21, 3.15.0)

`synapse --url wss://hostdemo.menger.sh/ws hostdemo …` (or `-H HOST --tls
--path /ws`; `--cacert FILE` for a local CA) reaches a service behind TLS — every
command: calls, streams and their reconnects, `-s`, `_find`, bidirectional
`respond`, `_login`. The address is one `Synapse.Endpoint` (host, port, tls,
path, CA files) from `app/Main.hs`'s `resolveConnection` to every dial
(`substrateConfig`); the TLS dial is plexus-protocol's `Plexus.Client`
(`substrateTls`, the `tls` package, the system trust store via
crypton-x509-system — always verified, no skip switch). Sessions: a plain
`ws://HOST:PORT/` keeps `HOST_PORT` (nothing moves); anything else is keyed
by its URL (`sessions/wss_HOST_443_ws/`, keychain `backend=wss://HOST:443/ws`).
A key proof's `aud` is `wss://HOST` behind TLS (`Endpoint.audience`).
`test/EndpointSpec.hs` pins the keys; `test/TlsSpec.hs` runs the probe, a
call, a bidirectional request and a key login through a TLS terminator of its
own (openssl makes the CA per run). README "A service behind TLS".

## Outages (SYNAPSE-20, 3.14.9)

synapse reconnects for 10 minutes (`--reconnect-for 10m`, `0` = never;
`SYNAPSE_RECONNECT_FOR`), noticing a close, a reset or a silent drop (a
keepalive ping after 15 s idle; `PLEXUS_KEEPALIVE=IDLE[,TIMEOUT]`). A read is
sent again; a write that went out is not (exit 75: it may have happened); a
stream resubscribes, with `since` = when it began when the method takes one
(`await_reply`), and `-j` prints `{"type":"reconnected",…}`; past the window,
exit 69. Progress is on stderr. A closed stdout is no outage (SYNAPSE-26,
3.17.2): what the printer raises is marked in the item callback
(`resilientCall`'s `printing`), the call ends unretried, and an EPIPE on
stdout/stderr — told from a socket's by `ioe_handle` — exits 141
(`exitOnClosedOutput` wraps `main`); `test/PipeSpec.hs` (pipe-test) runs
the built synapse into a reader that closes early. The detection is plexus-protocol's
`Plexus.Client` (`LostConnection`, `scClosed`, the keepalive); the policy is
`Synapse.Reconnect`; `test/ReconnectSpec.hs` pins both against a flaky service.
README "Outages: reconnecting".

## One round trip a call (SYNAPSE-28, 3.16.0)

A warm call is one round trip and the server's time (`tickets health` against
app.menger.sh: 81 ms, ping 77 ms; 4.6 s in 3.15.0). Every request a run makes
goes through plexus-protocol's `Plexus.Transport.setCallRouter` (0.5.3) to
`Synapse.Hold`: the **holder** for its address and agent (`synapse _hold`,
started by the first run; `$XDG_RUNTIME_DIR/synapse/ADDRESS@AGENT-VERSION.sock`,
0600; exits idle after `--hold-for`, 10m; `--no-hold` / `SYNAPSE_NO_HOLD=1`
skip it), else the run's own **multiplexer** — one connection per address
and credential, every call on it, schema and `_info` reads asked once however
many wait. Failures read as a connection of the run's own reports them
(`LostConnection` with whether it was sent, a failed dial as `connect`'s error
call), so `Synapse.Reconnect` and the re-login run unchanged on top.
**Send first** (`Main.sendFirst`): with the backend's route, `_info` and the
method's plan in `Synapse.SchemaStore` (`~/.cache/synapse/schemas/`, per
address, agent and backend; schemas as `PATH@VERSION-HASH.json` + `PATH.pin`,
plans under `plans/`), the call is built from the plan and sent with no probe
and no fetch; a `CacheMiss` (the cache lacks it) or `StaleSchema` (it misled
the call, or the server refused what it said before the method ran) runs the
call again the old way (`slowPath`), which fills the cache. Help, listings,
`-s`, `-i`, `_find` and calls with a credential of their own read the server.
`test/HoldSpec.hs` counts connections, probes and schema fetches at
`test/MockService.hs` (cold: one connection; warm: none, no probe, no fetch;
`--no-hold`: one; a renamed method falls back and answers); `ReconnectSpec`
runs every outage again through a holder. README "One round trip a call".

## Credentials & `_self`

Per-backend credential defaults live at `~/.plexus/<backend>/defaults.json`. Each cookie/header value is a **credential-reference URI** resolved at request time — never a raw secret in the file unless using the `literal:` escape hatch.

```bash
# Inspect; decodes JWTs and flags expired tokens
synapse _self <backend> show

# Set values (bare value auto-wraps as literal:)
synapse _self <backend> set cookie access_token "eyJ..."
synapse _self <backend> set header X-API-Key "env://MY_API_KEY"

# Import a JWT file; clear the whole store
synapse _self <backend> import-token ~/jwt.txt
synapse _self <backend> clear --yes
```

**URI schemes:** `literal:` (verbatim), `env://VAR` (env var), `file:///path` (file contents, trailing newline stripped), `keychain://svc/acct` (OS keychain via macOS `security` CLI — Linux/Windows stubbed). The keychain resolver gives you encryption at rest and a commit-safe manifest, but **not per-process sandboxing**: any process running as your user can read the items via `security find-generic-password`. Same tradeoff as cargo / gcloud / aws-cli.

**Priority chain** on each invocation: `--token` / `SYNAPSE_TOKEN` env / `--token-file` / stored `cookies.access_token` — first match wins. CLI `--cookie`/`--header` flags override stored values per key.

**Legacy migration:** the pre-SELF `~/.plexus/tokens/<backend>` file auto-migrates to `defaults.json` (as `literal:<jwt>`) on first read. Migration is deliberate about preserving current security posture; users opt into keychain storage via `synapse _self <backend> upgrade-to-keychain` (SELF-8, macOS only).

**Shared with synapse-cc:** the `Synapse.Self` module family is the canonical implementation. `synapse-cc _self …` is the identical subcommand tree, backed by the same handler.

Related tickets: `plans/SELF/SELF-1..8.md` (all Complete).

## Keys, login & sessions (IDENTITY-43)

An agent proves who it is with an Ed25519 key and then carries a session by
itself. The wire and the stores are IDENTITY-40's contract
(`Synapse.Identity.Proof`, `Synapse.Identity.Store`); the fixed proof vector
is pinned in `test/IdentitySpec.hs`.

```bash
synapse _key new agent-7 [--store auto|file|keychain]   # prints key:<hex>; never overwrites
synapse _key show agent-7 ; synapse _key list
synapse -P 4455 _login menger --key agent-7 --as worker-3 [--scope editor@MENGER-12] [--ttl SECS]
synapse -P 4455 menger tickets health                   # carries worker-3's session (the default agent)
synapse -P 4455 --as worker-9 menger session whoami     # or SYNAPSE_AS=worker-9
synapse -P 4455 _logout menger [--as worker-3]
```

- Key `NAME`: `PLEXUS_KEY` (base64 seed) → keychain (`secret-tool`
  `service=plexus-key name=NAME`; macOS `security -s plexus-key -a NAME`) →
  `$CREDENTIALS_DIRECTORY/NAME.key` → `~/.plexus/keys/NAME.key` (0600).
- Session (`HOST:PORT`, agent): `PLEXUS_SESSION` → keychain
  (`service=plexus-session backend=HOST:PORT agent=AGENT`) →
  `~/.plexus/sessions/HOST_PORT/AGENT.json` (0600, with the key name and scope
  to log in again from); `…/default` names the backend's default agent.
- Writes go to the keychain when it answers (2 s limit; none without a
  session bus, or with `PLEXUS_KEYCHAIN=off`), else the file.
- A call carries `Authorization: Bearer <token>` on the probe and the upgrade
  unless it already has a credential (`-t`, `--token-file`, an `access_token`
  cookie, an `Authorization` header). An expired session, a 401 at the
  upgrade, or "you have signed out" → log in again from the key, once, with a
  one-line notice on stderr.
- Many processes at once (SYNAPSE-15, 3.14.10): a re-login holds an advisory
  lock (`flock` on `…/HOST_PORT/AGENT.lock`) and reads the stored session
  again under it — when another process has just made a live one, it takes
  that one instead of logging in again, so N parallel calls after a restart
  log in once. Every private file is written through a temporary file of
  its own (O_EXCL, 0600) renamed over the target; a session that cannot be
  kept is a warning, never the call's failure.

## Each caller its own server (AUTH-8)

On a service with the AUTH gate (AUTH-1), `_info` says the caller's `view`:

- **anonymous** — `synapse menger` shows only the door ("Sign in to see what
  you can do here.", `auth`'s login / signup / providers, and the two `_login`
  lines); any other path shows it too and exits 77.
- **caller** — help says `you are NAME (PRINCIPAL) · ROLE on SCOPES, within
  CEILING` under the description (from `auth.whoami`, else `session.whoami`),
  and each method line its `grant` and `held_on` in two aligned columns.
- A refusal (`error_kind: forbidden`) prints `refused: needs GRANT on SCOPE`,
  the roles with it and where you hold it (within the session's ceiling), and
  exits 77.
- A person signs in with `synapse -P PORT _login BACKEND` (no `--key`, no
  `--as`): synapse listens on `127.0.0.1:<random>`, asks `auth.login {flow:
  {browser: {redirect}}}`, opens the URL (`SYNAPSE_NO_BROWSER=1` only prints
  it), waits 5 minutes for `/cb?code&state` with the state it was given, trades
  the code (`auth.login {flow: {code}}`), and keeps the session as agent `me`
  (no key: when it ends, sign in again). An agent's `_login` asks `auth.login
  {flow: {session: {ttl_secs, ceiling}}}`, and `session.login` on a service
  without auth.

A service without the gate (today's menger) answers none of these fields and
renders exactly as before. The wire types are plexus-protocol's `Plexus.Auth`
and the new optional fields on `MethodSchema` / `PluginSchema`; the renders are
`Synapse.Auth`; `test/AuthSpec.hs` pins them and runs the flows against
`test/MockService.hs` (`SYNAPSE_MOCK_SERVICE=today|anonymous|caller` serves it by hand).

## Help (SYNAPSE-12, SYNAPSE-13)

Once a backend is named, help is the backend's alone. A **listing**
(`synapse -P 4455 menger`, `… menger tickets`) is gh's shape: a usage line,
then one line per activation and per method — its name and summary (the
first sentence of its description, fitted to the line) and no flags — the
methods in their schema `group`s (in the order they first appear,
alphabetical within; one METHODS section when none is given; the ungrouped
as OTHER), then `Run '… <method> --help' for its options.` In a caller's view
(AUTH-8) the `you are …` header stays and each line ends with its grant and
where it is held. A **method's page** (`… tickets set_status --help`) is
cargo's: the description; the long description; `Usage: … --id <ID>
--status <STATUS> [OPTIONS]`; `Options:` aligned, required first, optional in
[brackets], each with its type, description and `[values: …]` (a property's
`x-plexus-values`, else a string enum's literals); `Example:` — the schema's
`examples` as command lines under their notes (a list inside an object goes
whole as `-p '{…}'`, SYNAPSE-9), else one synthesised from the required
options; and, in a caller's view, `Requires: GRANT on the scope of --TARGET
(you hold it on …)`. A service that sends none of the new fields renders the same
shape without them. `Synapse.Algebra.Render.renderListing` and
`Synapse.CLI.Help.renderMethodPage`; `test/RenderSpec.hs` pins both.

**Search** (SYNAPSE-17): `synapse -P 4455 _find menger <words…>` — every
method and parameter whose name or description matches all the words (typos,
prefixes, `setpar` → `set_parent`), best first, grouped METHODS · PARAMETERS ·
MENTIONED IN in the listing's shape; `… _find menger tickets <words>` narrows,
`--limit N`, `--methods` / `--params`, `-j` one JSON object per hit. The IR is
cached on disk under the root schema's hash and the caller's view (one schema
call when nothing changed). `Synapse.Find`; `test/FindSpec.hs` pins the matcher
and the pages against `test/MockService.hs`'s catalog (`test/fixtures/`).

## Low-Level Plexus RPC

For debugging or when synapse isn't available, use `websocat` directly.

The plexus server runs on `ws://localhost:4444`.

```bash
# List all activations
(echo '{"jsonrpc":"2.0","id":1,"method":"substrate.schema","params":[]}'; sleep 1) | websocat ws://localhost:4444

# Get full schema with params and return types
(echo '{"jsonrpc":"2.0","id":1,"method":"substrate.full_schema","params":["cone"]}'; sleep 1) | websocat ws://localhost:4444

# Call a method
(echo '{"jsonrpc":"2.0","id":1,"method":"bash_execute","params":{"command":"echo hello"}}'; sleep 2) | websocat ws://localhost:4444

# Interactive session
websocat ws://localhost:4444
```

## Architecture Documentation

Architecture docs in `docs/architecture/` use reverse-chronological naming so newest appear first.

**Naming formula**: `(u64::MAX - nanotime)_title.md`

```python
import time
nanotime = int(time.time() * 1_000_000_000)
filename = (2**64 - 1) - nanotime
print(f'{filename}_your-title.md')
```

## Project Structure

```
synapse/
├── app/Main.hs                 # CLI entry point, argument parsing, dispatch
├── src/Synapse/
│   ├── Monad.hs                # SynapseM effect stack
│   ├── Transport.hs            # Network layer (WebSocket JSON-RPC)
│   ├── Cache.hs                # Content-addressed schema cache
│   ├── Renderer.hs             # Mustache template-based output rendering
│   ├── Schema/
│   │   ├── Types.hs            # Core types (Path, SchemaView, NavError)
│   │   └── Functor.hs          # SchemaF base functor
│   ├── Algebra/
│   │   ├── Recursion.hs        # Recursion schemes (cata, ana, hylo, para, apo)
│   │   ├── Navigate.hs         # Navigation (recursive descent)
│   │   ├── Render.hs           # Schema rendering to text
│   │   └── Walk.hs             # Tree walking (hylomorphism)
│   ├── CLI/
│   │   ├── Help.hs             # IR-based help rendering
│   │   ├── Parse.hs            # IR-driven parameter parsing
│   │   ├── Transform.hs        # Parameter transforms (paths, env vars)
│   │   ├── Template.hs         # Template generation from IR
│   │   └── Support.hs          # CLI representability checks
│   ├── IR/
│   │   ├── Types.hs            # IR data types
│   │   └── Builder.hs          # Schema -> IR transformation
│   ├── Backend/
│   │   └── Discovery.hs        # Backend discovery via registry
│   └── Self/
│       ├── Commands.hs          # _self meta-command dispatcher
│       ├── Template.hs          # Template CRUD operations
│       ├── Pattern.hs           # Glob-style pattern matching
│       └── Examples.hs          # Example value generation
└── test/                        # 5 test suites

../plexus-protocol/              # Shared protocol types (sibling package)
```

## Naming: a backend is a service (NAMING-9, 3.17.1)

A Plexus backend is a *service*, never the retired word (owner, 2026-10-29;
`~/AGENTS.md` "Naming"); an activation with children is a *parent*
(`isParentActivation`). `scripts/naming-gate.sh` fails on any line that says
the old word (no arguments: each line, exit 1; `-c`: the count), and
`test/NamingGateSpec.hs` runs it as `test-suite naming-gate`, so `cabal test`
and CI fail on it too. What may stay: GitHub, a line marked `naming-alias` (an
old name kept, deprecated, until NAMING-18) and the gate's `ALLOW` entries,
each with its reason and the NAMING ticket that removes it. The spec counts
the gate's lines and shows the first twenty; it never diffs the whole output
(hspec's diff of 875 lines reached 7.3 GB on 2026-10-29).
