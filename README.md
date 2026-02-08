```
███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗
██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝ \│/
███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  ──◉──
╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝   /│\
███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗
╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝
```

# Synapse

**A CLI that writes itself from your API's structure.**

Synapse is a schema-driven command-line interface for Plexus RPC servers. It discovers and navigates API structures at runtime - point it at a Plexus RPC server, and it generates commands, help text, and completions from the schema with no code generation step required.

---

## Quick Start

### Installation

```bash
cd synapse
cabal build
cabal install
```

### Basic Usage

Synapse connects to `127.0.0.1:4444` by default. Call methods using this pattern:

```bash
synapse <backend> <activation> <method> --<parameter> <value>
```

Examples:

```bash
# Check server health
$ synapse substrate health check
status: healthy
uptime_seconds: 42

# Execute bash commands
$ synapse substrate bash execute --command "echo hello"
line: hello
code: 0

# Echo a message
$ synapse substrate echo once --message "Hello, world!"
Hello, world!
```

Running `synapse` without arguments shows available backends:

```bash
$ synapse
Available backends:
  substrate      127.0.0.1:4444 ✓ - Main substrate instance
```

**The CLI is generated at runtime** - synapse fetches each Plexus RPC server's schema and derives commands, help text, and parameter validation from it. No code generation step, no static configuration. Point synapse at any Plexus RPC server and it adapts.

### Backend Discovery

Scan local ports to find running Plexus RPC servers:

```bash
$ synapse _self scan
Scanning 127.0.0.1 ports 4440-4459 for backends...

Found 3 backend(s):

  4444  registry-hub
  4445  substrate
  4446  secrets
```

Register discovered Plexus RPC servers so synapse can route to them by name:

```bash
$ synapse registry-hub registry register --name substrate --host 127.0.0.1 --port 4445
```

### Meta Commands (`_self`)

Synapse has local commands that don't make RPC calls:

**Scan for Plexus RPC servers** on ports 4440-4459:

```bash
$ synapse _self scan
Found 3 backend(s):
  4444  registry-hub
  4445  substrate
  4446  secrets
```

**Generate templates** from any Plexus RPC server's schema:

```bash
$ synapse _self template generate
Generating templates in ~/.config/synapse/templates...
  registry-hub.registry.list
  registry-hub.registry.register
  registry-hub.registry.update
  ...
Generated 7 template(s)
```

Templates are auto-generated from the Plexus RPC server's JSON Schema - synapse inspects return types, discriminated unions, and field structures to create Mustache templates for pretty output.

### CLI Structure

```
synapse [OPTIONS] <backend> <activation> <method> [--param value ...]
```

**Options** (like `-P`, `-H`, `--json`) must come **before** the backend. Everything **after** the backend is the path and method parameters:

```bash
# Synapse options come first, then backend/path, then method parameters
$ synapse substrate bash execute --command "pwd"

# -P is a synapse option, --name and --port are method parameters
$ synapse -P 4444 substrate registry update --name foo --port 4445

# Use --json to see raw stream output
$ synapse --json substrate bash execute --command "echo test"
```

### Connecting to Plexus RPC Servers

Synapse connects to `127.0.0.1:4444` by default. Just specify the backend and path:

```bash
$ synapse substrate bash execute --command "echo hello"
line: hello
code: 0
```

Check server health:

```bash
$ synapse substrate health check
status: healthy
uptime_seconds: 12345
```

Get the Plexus RPC hash:

```bash
$ synapse substrate hash
value: 21b9b63682192f0f
```

Connect to a different port if needed:

```bash
$ synapse -P 4445 substrate bash execute --command "echo hello"
```

### Navigating

Navigate into plugins like a filesystem:

```bash
$ synapse plexus echo
echo v0.1.0

Echo messages back

methods

  echo         Echo a message back
               --message <string>   The message to echo
               --count <integer>?   Number of times to repeat

  once         Echo a simple message once
               --message <string>   The message to echo
```

### Invoking Methods

Methods with required parameters show help:

```bash
$ synapse substrate echo once
once - Echo a simple message once

  --message <string> (required)
      The message to echo
```

Pass parameters using `--parameter-name value` syntax:

```bash
$ synapse substrate echo once --message "Hello, world!"
Hello, world!
```

```bash
$ synapse substrate bash execute --command "ls -la"
line: total 48
line: drwxr-xr-x  ...
code: 0
```

Alternatively, use JSON with `-p` for complex parameters:

```bash
$ synapse substrate echo echo -p '{"message": "hi", "count": 3}'
hi hi hi
```

Methods without required parameters auto-invoke:

```bash
$ synapse substrate health check
status: healthy
uptime_seconds: 12345
```

---

## Core Concepts

### Schema-Driven

Synapse fetches the Plexus RPC server's schema at runtime and derives the entire CLI from it:

- **Commands** come from plugin namespaces and method names
- **Help text** comes from descriptions in the schema
- **Parameters** come from JSON Schema definitions
- **Validation** happens based on `required` fields

There's no static code generation - the CLI adapts to whatever the Plexus RPC server exposes.

### Navigation

Navigation works like a filesystem path:

```
synapse plexus solar earth luna info
        |      |     |     |    +-- method
        |      |     |     +-- plugin (moon)
        |      |     +-- plugin (planet)
        |      +-- plugin (solar system)
        +-- backend
```

At each level, you can stop to see what's available. The schema is fetched lazily as you descend.

### Methods vs Plugins

- **Plugins** are namespaces containing methods and child plugins
- **Methods** are actions you can invoke

Plugins can nest arbitrarily deep. Methods are always leaves - you can't navigate into them.

### Parameters

Pass parameters using `--parameter-name value` syntax:

```bash
$ synapse substrate bash execute --command "uptime"
line:  07:15:42 up 5 days, 3:21,  0 users,  load average: 0.52, 0.58, 0.59
code: 0

$ synapse substrate echo echo --message "hello" --count 3
hello hello hello
```

For complex nested JSON, use `-p`:

```bash
$ synapse substrate echo echo -p '{"message": "hello", "count": 3}'
hello hello hello
```

Type inference: `true`/`false` become booleans, numbers become numbers, everything else is a string.

---

## Output Modes

### Default (Template Mode)

Output is formatted using Mustache templates when available, falling back to pretty-printed YAML-like output:

```bash
$ synapse plexus solar observe
name: Sol
planet_count: 8
asteroid_belt: true
```

### JSON Mode (`--json`)

Output raw JSON stream items (useful for piping):

```bash
$ synapse --json plexus solar observe
{"type":"data","provenance":["solar"],"hash":"abc123","content_type":"solar.observe","data":{"name":"Sol",...}}
```

### Raw Mode (`--raw`)

Output just the content JSON (skip template rendering):

```bash
$ synapse --raw plexus solar observe
{"name":"Sol","planet_count":8,"asteroid_belt":true}
```

### Schema Mode (`--schema`)

Fetch the raw JSON Schema for a path:

```bash
$ synapse --schema plexus echo
{"namespace":"echo","version":"0.1.0","methods":[...],"children":null}
```

### Dry Run (`--dry-run`)

Preview the Plexus RPC request (JSON-RPC 2.0 format) without sending:

```bash
$ synapse --dry-run plexus echo once --message "test"
{"jsonrpc":"2.0","id":1,"method":"plexus.call","params":{"method":"plexus.echo.once","params":{"message":"test"}}}
```

---

## Architecture Overview

Synapse treats the plugin system as a **category**:

- **Objects** are schemas (plugins and methods)
- **Morphisms** are paths (sequences of namespace segments)

The CLI implements **algebras** over this category. Each operation (navigation, rendering, completion) is a specific algebra applied via recursion schemes.

### The SynapseM Monad

The effect stack:

```haskell
SynapseM = ExceptT SynapseError (ReaderT SynapseEnv IO)
```

This provides:

- **Error handling** via `SynapseError` (navigation errors, transport errors, parse errors)
- **Environment** with schema cache and cycle detection
- **IO** for network calls

The environment tracks:

```haskell
data SynapseEnv = SynapseEnv
  { seHost    :: Text                              -- Plexus RPC server host
  , sePort    :: Int                               -- Plexus RPC server port
  , seCache   :: IORef (HashMap PluginHash PluginSchema)  -- Content-addressed cache
  , seVisited :: IORef (HashSet PluginHash)        -- Cycle detection
  }
```

---

## The Algebraic Approach

### Why Category Theory?

The plugin graph has recursive structure - plugins contain plugins contain plugins. Category theory provides tools for working with such structures:

1. **Extensibility**: New operations are just new algebras
2. **Correctness**: Recursion schemes ensure we handle all cases
3. **Fusion**: Hylomorphisms avoid building intermediate structures

### Base Functor

The schema tree is defined by its "shape":

```haskell
data SchemaF a
  = PluginF PluginSchema Path [a]   -- Interior node with children
  | MethodF MethodSchema Text Path  -- Leaf node
```

The type parameter `a` represents "what's in the recursive positions."

### Catamorphism (Fold)

A catamorphism collapses a structure bottom-up:

```haskell
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
```

For collecting methods:

```haskell
methodAlgebra :: SchemaF [MethodInfo] -> [MethodInfo]
methodAlgebra (PluginF schema path children) =
  localMethods ++ concat children
methodAlgebra (MethodF method ns path) =
  [MethodInfo method path ns]
```

### Paramorphism (Fold with Context)

Navigation needs to inspect the original structure while folding:

```haskell
para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
```

At each layer, we receive `(original subtree, already-computed result)` pairs. This lets us check namespaces before recursing.

### Hylomorphism (Unfold + Fold, Fused)

Most operations combine unfolding (fetching schemas) with folding (processing):

```haskell
hyloM :: (Monad m, Traversable f)
      => (f b -> m b)      -- algebra (fold one step)
      -> (a -> m (f a))    -- coalgebra (unfold one step)
      -> a -> m b

walkMethods :: Path -> SynapseM [MethodInfo]
walkMethods = hyloM methodAlgebraM schemaCoalgebra
```

The fused version never materializes the intermediate tree.

---

## Template System

### Mustache Templates

Output is formatted using Mustache templates. Templates are resolved by content type (e.g., `echo.once`).

### Resolution Order

For each search path (`.substrate/templates/` then `~/.config/synapse/templates/`), tries in order:

1. **Exact match**: `{searchPath}/{namespace}/{method}.mustache`
2. **Namespace default**: `{searchPath}/{namespace}/default.mustache`
3. **Global default**: `{searchPath}/default.mustache`

### Generating Templates

Scaffold templates from the schema:

```bash
$ synapse --generate-templates plexus
Generating templates in ~/.config/synapse/templates...
  echo/once.mustache
  echo/echo.mustache
  solar/observe.mustache
  ...
Generated 15 templates
```

Generated templates use the method's return schema to create appropriate Mustache syntax:

```mustache
{{! echo.once }}
{{message}}
```

---

## Code Generation

### IR Output

Synapse can emit an Intermediate Representation for transpilers:

```bash
$ synapse --emit-ir plexus > plexus.ir.json
```

### IR Structure

```json
{
  "irVersion": "2.0",
  "irTypes": {
    "Position": {
      "tdName": "Position",
      "tdKind": { "tag": "KindStruct", "ksFields": [...] }
    }
  },
  "irMethods": {
    "cone.chat": {
      "mdName": "chat",
      "mdFullPath": "cone.chat",
      "mdStreaming": true,
      "mdParams": [...],
      "mdReturns": { "tag": "RefNamed", "contents": "ChatEvent" }
    }
  },
  "irPlugins": {
    "cone": ["create", "chat", "list", "delete"]
  }
}
```

Key features:

- **Deduplicated types**: Types from `$defs` are hoisted to top level
- **Streaming inference**: Methods returning discriminated unions are marked as streaming
- **Type references**: Methods reference types by name, not inline

This IR is consumed by codegen tools to produce TypeScript, Python, Swift, etc.

---

## Internals Deep Dive

### Schema Functor

The base functor captures one layer of tree structure:

```haskell
data SchemaF a
  = PluginF
      { sfSchema   :: PluginSchema  -- Schema at this node
      , sfPath     :: Path          -- Path to this node
      , sfChildren :: [a]           -- Recursive positions
      }
  | MethodF
      { sfMethod    :: MethodSchema
      , sfNamespace :: Text
      , sfMethodPath :: Path
      }
```

The fixed point gives the full tree:

```haskell
type SchemaTree = Fix SchemaF
```

### Cycle Detection

Plugins can reference each other via hashes. Synapse tracks visited hashes:

```haskell
checkCycle :: PluginHash -> Path -> SynapseM ()
checkCycle hash path = do
  visitedRef <- asks seVisited
  visited <- liftIO $ readIORef visitedRef
  when (hash `HS.member` visited) $
    throwNav $ Cycle hash path
  liftIO $ modifyIORef' visitedRef (HS.insert hash)
```

### Content-Addressed Caching

Schemas are cached by their content hash:

```haskell
fetchCached :: PluginHash -> SynapseM PluginSchema -> SynapseM PluginSchema
fetchCached hash fetcher = do
  cached <- lookupCache hash
  case cached of
    Just schema -> pure schema
    Nothing -> do
      schema <- fetcher
      insertCache (psHash schema) schema
      pure schema
```

Same hash = same content, so caching is always safe.

### Streaming Transport

Method invocation streams results:

```haskell
invokeStreaming :: Path -> Text -> Value
                -> (PlexusStreamItem -> IO ())
                -> SynapseM ()
```

Stream items include:

- `StreamData`: Result data with content type
- `StreamProgress`: Progress updates
- `StreamError`: Error messages
- `StreamDone`: Completion signal

---

## Development

### Building

```bash
cabal build
```

### Testing

```bash
cabal test
```

There are 5 test suites:

- **cli-test** (`test/CLISpec.hs`) -- Integration tests, requires running backend on localhost:4444
- **ir-test** (`test/IRSpec.hs`) -- IR builder tests (type resolution, help text)
- **parse-test** (`test/ParseSpec.hs`) -- Unit tests for CLI parameter parsing
- **path-normalization-test** (`test/PathNormalizationSpec.hs`) -- Hyphen/underscore normalization
- **typeref-json** (`test/TypeRefJson.hs`) -- TypeRef JSON serialization

### Adding Backends

Plexus RPC servers are discovered dynamically via the registry - no code changes needed:

```bash
# Register a new Plexus RPC server
$ synapse registry-hub registry register --name mybackend --host 127.0.0.1 --port 5555

# Now it's available
$ synapse mybackend
```

The registry stores Plexus RPC server metadata (host, port, description) and synapse queries it at runtime to resolve backend names to connection details.

### Project Structure

```
synapse/
  app/
    Main.hs              -- CLI entry point, argument parsing, dispatch
  src/Synapse/
    Monad.hs             -- SynapseM effect stack
    Transport.hs         -- Network layer (WebSocket JSON-RPC)
    Cache.hs             -- Content-addressed schema cache
    Renderer.hs          -- Mustache template-based output rendering
    Schema/
      Types.hs           -- Core types (Path, SchemaView, NavError, HubStreamItem)
      Functor.hs         -- SchemaF base functor, Fix, SchemaTree
    Algebra/
      Recursion.hs       -- Pure & monadic recursion schemes (cata, ana, hylo, para, apo)
      Navigate.hs        -- Navigation (recursive descent with cycle detection)
      Render.hs          -- Schema rendering to text (prettyprinter)
      Walk.hs            -- Tree walking (hylomorphism)
    CLI/
      Help.hs            -- IR-based help rendering with type expansion
      Parse.hs           -- IR-driven parameter parsing (flat flags -> nested JSON)
      Support.hs         -- CLI representability checks for IR types
      Template.hs        -- IR-driven Mustache template generation
      Transform.hs       -- Parameter transforms (path expansion, env vars, defaults)
    IR/
      Types.hs           -- IR data types (TypeDef, MethodDef, TypeRef)
      Builder.hs         -- Schema -> IR transformation (hylomorphism + deduplication)
    Backend/
      Discovery.hs       -- Dynamic backend discovery via _info and registry
    Self/
      Commands.hs        -- _self meta-command dispatcher (scan, template)
      Template.hs        -- Template CRUD operations (list, show, generate, delete)
      Pattern.hs         -- Glob-style pattern matching for method filtering
      Examples.hs        -- Example value generation for templates
  test/
    CLISpec.hs           -- Integration tests (requires running backend)
    IRSpec.hs            -- IR builder tests (type resolution, help text)
    ParseSpec.hs         -- Unit tests for CLI parameter parsing
    PathNormalizationSpec.hs -- Hyphen/underscore normalization tests
    TypeRefJson.hs       -- TypeRef JSON serialization verification
```

---

## Architecture Documentation

Detailed architecture docs, decision records, and implementation notes are in [`docs/architecture/`](./docs/architecture/).

See the [Architecture Index](./docs/architecture/INDEX.md) for a curated list of important documents.

Recent updates:
- **[Array and Normalization Fixes](./docs/architecture/16677498315934040575_array-and-normalization-fixes.md)** - CLI parser improvements for arrays and hyphenated method names
- **[Dynamic CLI Implementation](./docs/architecture/16680974430007725567_dynamic-cli-implementation.md)** - How the schema-driven CLI works
- **[Schema to CLI Pipeline](./docs/architecture/16680606527890548735_schema-to-cli-pipeline.md)** - Parameter parsing and type correspondence

---

## License

MIT
