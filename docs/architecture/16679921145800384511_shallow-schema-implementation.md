# Shallow Schema Implementation

**Status: IMPLEMENTED**
**Date: 2024-12-27**

## Summary

Synapse CLI updated to work with shallow schemas. The schema is no longer recursive on the wire — children are summaries (namespace, description, hash), not full schemas. Navigation fetches child schemas lazily.

## What Changed

### Types (`meaning/src/Plexus/Schema/Recursive.hs`)

```haskell
-- Before: recursive
data PluginSchema = PluginSchema
  { ...
  , psChildren :: Maybe [PluginSchema]  -- full schemas
  }

-- After: shallow
data PluginSchema = PluginSchema
  { ...
  , psChildren :: Maybe [ChildSummary]  -- references only
  }

data ChildSummary = ChildSummary
  { csNamespace   :: Text
  , csDescription :: Text
  , csHash        :: PluginHash
  }
```

### CLI (`synapse/app/Algebra.hs`)

- Changed from nested optparse-applicative subcommands to positional path arguments
- Schema fetched lazily via `substrate.call` with `{path}.schema`
- Multiple parameter input modes (see below)

```bash
synapse solar earth luna info              # navigates lazily, invokes method
synapse echo once message=hello            # key=value inline params
synapse echo once -p '{"message":"hello"}' # JSON params
synapse --schema solar                     # raw schema JSON
synapse --rpc '{"method":"health.check"}'  # raw JSON-RPC passthrough
```

### CLI Flags

| Flag | Short | Description |
|------|-------|-------------|
| `--host` | `-H` | Plexus server host (default: 127.0.0.1) |
| `--port` | `-P` | Plexus server port (default: 4444) |
| `--json` | `-j` | Output raw JSON stream |
| `--dry-run` | `-n` | Show JSON-RPC request without sending |
| `--schema` | `-s` | Fetch raw schema JSON for path |
| `--params` | `-p` | Method parameters as JSON object |
| `--rpc` | `-r` | Raw JSON-RPC request (bypass navigation) |

### Parameter Input Modes

1. **Inline `--key value`**: `synapse echo echo --message hi --count 3`
2. **JSON via `-p`**: `synapse echo once -p '{"message":"hello"}'`
3. **Auto-invoke**: Methods with no required params run automatically

## Debugging with Raw JSON-RPC

When synapse isn't available or for debugging, use `websocat` directly. The Plexus server runs on `ws://localhost:4444`.

### Get Root Schema

```bash
(echo '{"jsonrpc":"2.0","id":1,"method":"substrate.call","params":{"method":"substrate.schema"}}'; sleep 1) | websocat ws://127.0.0.1:4444
```

### Get Child Schema

```bash
# Solar system schema
(echo '{"jsonrpc":"2.0","id":1,"method":"substrate.call","params":{"method":"solar.schema"}}'; sleep 1) | websocat ws://127.0.0.1:4444

# Nested child schema
(echo '{"jsonrpc":"2.0","id":1,"method":"substrate.call","params":{"method":"solar.earth.schema"}}'; sleep 1) | websocat ws://127.0.0.1:4444
```

### Invoke Methods

```bash
# Health check
(echo '{"jsonrpc":"2.0","id":1,"method":"substrate.call","params":{"method":"health.check"}}'; sleep 1) | websocat ws://127.0.0.1:4444

# Echo with params
(echo '{"jsonrpc":"2.0","id":1,"method":"substrate.call","params":{"method":"echo.once","params":{"message":"hello"}}}'; sleep 1) | websocat ws://127.0.0.1:4444

# Observe solar system
(echo '{"jsonrpc":"2.0","id":1,"method":"substrate.call","params":{"method":"solar.observe"}}'; sleep 1) | websocat ws://127.0.0.1:4444
```

### Interactive Session

```bash
websocat ws://127.0.0.1:4444
# Then type JSON-RPC requests manually
```

## Alignment with Category Theory

The design document describes a coalgebraic architecture:

| Concept | Design Doc | Implementation | Alignment |
|---------|------------|----------------|-----------|
| Objects | Schemas by hash | `PluginSchema` with `psHash` | ✓ |
| Morphisms | Paths (child refs) | `[Text]` path segments | ✓ |
| Identity | Empty path | `[]` returns current schema | ✓ |
| Composition | Path concat | `navigatePath` chains fetches | ✓ |
| Coalgebra | `schema : Plugin → F(ChildSummary)` | `fetchSchemaAt` | ✓ |
| Lazy unfold | Anamorphism | Fetch on navigation | ✓ |

**The categorical structure is preserved.** The implementation correctly models:
- Schemas as objects in a free category
- Paths as morphisms (composable sequences of child references)
- Lazy observation via coalgebraic unfold

## Shortcuts Taken

### 1. ~~No Base Functor~~ (REMOVED)

**Design doc specified** a base functor `PluginSchemaF` for recursion-schemes.

**Resolution:** Removed entirely. `PluginSchema` already has `psChildren :: Maybe [ChildSummary]` — it IS the shallow schema. The base functor was just field renaming (`ps*` ↔ `psf*`). Removed dependencies: `recursion-schemes`, `data-fix`.

### 2. No Algebras as First-Class

**Design doc specifies:**
```haskell
type RenderAlg = PluginSchemaF Text -> Text
type NavAlg = PluginSchemaF (ChildSummary, NavResult) -> NavResult

renderSchema = cata renderAlg
navigate = para navAlg
```

**Implementation:**
Direct pattern matching on `PluginSchema`:
```haskell
renderSchema :: PluginSchema -> Text
renderSchema schema = T.unlines $ [psNamespace schema, ...]
                    <> map renderChildSummary (pluginChildren schema)
```

**Impact:** Algebras are implicit in the code, not reified as separate functions. Less composable but simpler. Could refactor later.

### 3. No Zipper/Comonad for Navigation

**Design doc specifies:**
```haskell
data SchemaZipper = SchemaZipper
  { focus   :: PluginSchema
  , context :: [PluginSchema]  -- breadcrumbs
  }
```

**Implementation:**
Path tracked as `[Text]` list. No zipper structure.

**Impact:** Cannot navigate "up" the tree without re-fetching. Acceptable since CLI is stateless — each invocation is a fresh traversal.

### 4. No Effect Stack

**Design doc specifies:**
```haskell
type SynapseM = ExceptT SynapseError (ReaderT SynapseEnv IO)

data SynapseEnv = SynapseEnv
  { seCache   :: IORef SchemaCache
  , seVisited :: IORef (Set PluginHash)  -- cycle detection
  }
```

**Implementation:**
Plain `IO` with manual error handling. No cache, no cycle detection.

**Impact:**
- Multiple fetches for the same schema (no caching)
- Cycles would cause infinite loop (cycle detection missing)
- Errors handled ad-hoc with `exitFailure`

### 5. No Parameter Validation

**Design doc specifies:**
```haskell
validate :: MethodSchema -> [(Text, Text)] -> Either ValidationError Value
validate method params = validateAgainstSchema (msParams method) ...
```

**Implementation:**
Parameters passed as raw JSON via `-p` flag. No validation against method's JSON Schema.

**Impact:** Invalid params sent to backend, which returns error. Could validate client-side.

### 6. No Shell Completion

**Design doc specifies:**
```haskell
completions :: PluginSchema -> [Text]
completions schema = map msName (psMethods schema)
                  <> map csNamespace (pluginChildren schema)
```

**Implementation:**
Not implemented. Would require generating completion scripts from schema.

### 7. ~~Simplified Parameter Parsing~~ (RESOLVED)

**Design doc specifies:**
```bash
synapse echo echo --message "hi" --count 3
```

**Implementation:** Matches design doc exactly. Uses `forwardOptions` to pass unrecognized `--flags` through optparse-applicative to our custom parser. Type inference converts `"true"`→Bool, `"3"`→Number, etc.

## Shell Completion Implementation

### What's Possible

**Basic completions work.** `Synapse.Algebra.Complete` already provides:

```haskell
completions :: PluginSchema -> [Text]
completions PluginSchema{..} = concat
  [ map methodName psMethods
  , maybe [] (map csNamespace) psChildren
  ]
```

We can generate shell completion scripts that:
1. Complete path segments (namespaces + methods)
2. Show short descriptions next to each option (zsh/fish support this)
3. Complete `key=` parameter names after a method is selected

### What's NOT Possible (Natively)

**Full help pane during tab completion** — shells don't support this. Tab completion is a modal UI where the shell controls rendering. You can't inject arbitrary multi-line output while the user is mid-completion.

### Workarounds

**1. fzf Integration (Best Option)**

Use fzf's preview pane to show full help:

```bash
# In .zshrc or similar
_synapse_fzf() {
  local selection
  selection=$(synapse --completions "$LBUFFER" | fzf --preview 'synapse --help {}')
  LBUFFER="synapse $selection "
  zle redisplay
}
zle -N _synapse_fzf
bindkey '^T' _synapse_fzf
```

This gives you:
- Left pane: fuzzy-searchable list of completions
- Right pane: full `--help` output for highlighted item
- True "show entire help message at that phase"

**2. zsh Descriptions**

zsh's `_describe` shows short descriptions inline:

```zsh
_synapse() {
  local -a commands
  commands=(
    'solar:Observe the solar system'
    'echo:Echo utilities'
    'health:System health checks'
  )
  _describe 'command' commands
}
```

Output during completion:
```
solar   -- Observe the solar system
echo    -- Echo utilities
health  -- System health checks
```

**3. Fish Completions**

Fish has rich descriptions built-in:

```fish
complete -c synapse -n '__fish_use_subcommand' -a 'solar' -d 'Observe the solar system'
complete -c synapse -n '__fish_use_subcommand' -a 'echo' -d 'Echo utilities'
```

### Implementation Plan

1. **Add `--completions` flag** that outputs completion words (one per line)
2. **Add `--completions-with-desc` flag** for `word:description` format
3. **Generate shell scripts** via `synapse --generate-completions bash|zsh|fish`

```haskell
-- New flag in Main.hs
data Opts = Opts
  { ...
  , optCompletions :: Maybe Text  -- partial input to complete
  , optGenCompletions :: Maybe Shell  -- generate completion script
  }

generateCompletions :: Shell -> IO Text
generateCompletions Zsh = do
  root <- fetchSchemaAt []
  pure $ T.unlines
    [ "#compdef synapse"
    , "_synapse() {"
    , "  local -a cmds"
    , "  cmds=("
    , T.unlines $ map formatZshCompletion $ pluginChildren root
    , "  )"
    , "  _describe 'command' cmds"
    , "}"
    ]
```

### The Honest Answer

**You can't tab-complete and see the full help simultaneously** — that's not how shell completion works. But you CAN:

1. See short descriptions during completion (zsh/fish)
2. Use fzf for a preview pane with full help
3. Type `synapse solar <TAB>` to complete, then `synapse solar --help` to read

The fzf approach is the closest to "show entire help message at that phase, and allow you to tab autocomplete."

## What's Missing vs Design Doc

| Feature | Status | Priority |
|---------|--------|----------|
| Schema caching by hash | ✓ Implemented | — |
| Cycle detection | ✓ Implemented | — |
| Client-side param validation | Missing | Low |
| Shell tab completion | Missing | Medium |
| `--key value` param syntax | ✓ Implemented | — |
| `--schema` flag | ✓ Implemented | — |
| Raw JSON-RPC passthrough | ✓ Implemented | — |
| REPL mode | Missing | Low |
| Streaming output rendering | Partial | Medium |

## The Hylomorphism

The design doc describes the full pipeline as a hylomorphism:

```
Address → fetch (ana) → Schema → fold (cata) → Result
```

Our implementation:
```
Path → navigatePath (ana over IO) → PluginSchema → render/invoke (direct) → Output
```

The anamorphism (unfold) is preserved — we lazily fetch schemas as we navigate. The catamorphism (fold) is implicit in rendering and parameter handling, but not reified as a proper algebra.

## Conclusion

The implementation captures the **essential categorical structure**:
- Free category of schemas and paths
- Coalgebraic lazy observation
- Composition of morphisms (path segments)

The **shortcuts** are primarily about:
- Not reifying algebras as first-class values
- No effect stack / caching / cycle detection
- Simplified parameter handling

These are pragmatic choices for a working CLI. The categorical foundation remains intact and can be elaborated as needed.

## Related Documents

- [Category Verification Report](16680127584921047551_category-verification-report.md) - Verification that schema forms free category
- [Universal Schema Exposure](../../../substrate/docs/architecture/16680127584921047551_universal-schema-exposure.md) - Backend changes enabling `.schema` on all plugins
- [Synapse Design Document](../SYNAPSE.md) - Full categorical design specification
