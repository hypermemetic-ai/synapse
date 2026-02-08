# Synapse Development Guide

## Editor & Tools

- Use `codium --goto <file>:<line>` when I say "open this"
- Use `tre` to view directory structure

## Project Overview

**Synapse** is a Haskell CLI frontend for **Plexus** - a streaming JSON-RPC protocol for LLM orchestration.

The stack:
- **Substrate** (Rust) - The backend hub that hosts activations
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
- Booleans: `--ephemeral true`
- Raw JSON override: `-p '{"custom": "params"}'`

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
