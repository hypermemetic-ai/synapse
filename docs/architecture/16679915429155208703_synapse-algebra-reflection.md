# Synapse Algebra: A Reflection

**Date: 2024-12-27**
**Status: Active Development**

## Where We Are

Synapse now has two CLI implementations:

| Implementation | Status | Lines | Philosophy |
|---------------|--------|-------|------------|
| `synapse` (Main.hs) | **Deprecated** | ~350 | Pragmatic, inline, get-it-done |
| `synapse-algebra` (Algebra.hs) | **Active** | ~150 + ~600 library | Categorical, reified, composable |

Both produce similar output. The difference is internal: the algebraic version makes the structure explicit.

## How We Got Here

### The Journey

1. **Shallow schemas arrived** — Substrate changed from recursive `PluginSchema` to shallow schemas where children are `ChildSummary` references (namespace, description, hash). This broke the original code.

2. **Pragmatic fix** — We updated `Main.hs` to fetch schemas lazily via `substrate.call`. It worked. It was ugly.

3. **You asked for the algebra** — "We prefer the abstracted algebras." This was the right call. Instead of patching, we built it properly.

4. **The effect stack emerged** — `SynapseM` with typed errors, caching, cycle detection. What was implicit became explicit.

5. **CLI tests** — Built a test suite with a terse DSL:
   ```haskell
   it "echo once" $ ["echo", "once"] `has` ["once", "--message", "required"]
   it "invoke"    $ call ["echo", "once"] (msg "yo") `hasA` ["yo"]
   ```
   17 tests covering navigation, method help, invocation, and algebra parity.

6. **Dynamic CLI header** — Instead of duplicating optparse-applicative's help text manually, we use:
   ```haskell
   cliHeader = T.pack $ fst $ renderFailure failure "synapse"
     where failure = parserFailure defaultPrefs argsInfo (ShowHelpText Nothing) mempty
   ```
   This generates the CLI help from the actual parser definition.

7. **Pretty output** — Migrated from string concatenation to `prettyprinter`:
   ```haskell
   renderChildDoc child = fillBreak 12 (pretty $ csNamespace child)
     <+> align (fillSep $ map pretty $ T.words $ csDescription child)
   ```
   Proper column alignment, smart text wrapping with indentation.

8. **Output formatting iterations** — Through several rounds:
   - Removed leading newlines
   - Added blank line between CLI help and schema
   - Changed "Namespaces:" → "activations" (lowercase, no colon)
   - Changed "Methods:" → "methods" (lowercase, no colon)
   - Put activations before methods
   - Added spacing between method entries

9. **Design mockup** — I proposed an idealized output format with breathing room, horizontal rules, and an examples section. You said "make this real." Implementation is in progress.

## The Algebras

### What They Are

```
Navigation : Path → SynapseM SchemaView
Rendering  : ShallowSchema → Text
Completion : ShallowSchema → [Text]
```

These are **catamorphisms** (folds) over the schema structure. Navigation is effectful because fetching children requires IO. Rendering and completion are pure.

### Why They Matter

The pragmatic version does this:
```haskell
case findChild seg schema of
  Just child -> do
    childSchema <- fetch (csHash child)
    navigate childSchema rest
```

The algebraic version names it:
```haskell
type NavAlg = PluginSchema → Path → Path → SynapseM SchemaView
```

The difference seems cosmetic. It's not. When the pattern has a name:
- You can test it in isolation
- You can swap implementations
- You can compose it with other algebras
- You can reason about it mathematically

The pragmatic version works. The algebraic version *teaches*.

### The Base Functor

```haskell
data PluginSchemaF a = PluginSchemaF
  { psfNamespace   :: Text
  , psfVersion     :: Text
  , psfDescription :: Text
  , psfHash        :: PluginHash
  , psfMethods     :: [MethodSchema]
  , psfChildren    :: Maybe [a]  -- The hole for recursion
  }
```

This is `F` in the categorical sense. `ShallowSchema = F ChildSummary`. The wire format. We unfold it lazily into the full tree via network calls.

## The Deprecation

`synapse` (Main.hs) will be removed. Reasons:

1. **Duplicate code** — Same logic, different structure
2. **No tests** — The algebraic version has the test suite
3. **Harder to extend** — Adding features means touching everything
4. **Teaching value** — New contributors learn more from the algebraic version

Timeline: When you're ready. No rush.

## My Hopes

### For The System

I hope synapse becomes the canonical way to interact with Plexus. Not because it's the only way, but because it's *clear*. Someone reading the code should understand:

- What a schema is (the types tell you)
- How navigation works (the algebra tells you)
- What can go wrong (the error ADT tells you)

I hope the examples section gets built. Traversing schemas to generate real usage examples would make the CLI self-documenting. The algebra is already there — `collectExamples` is just another fold.

I hope the pretty output inspires similar care elsewhere. The `prettyprinter` work took 30 minutes but transformed how the tool feels.

### For The Codebase

I hope the separation between `meaning` (protocol types) and `synapse` (CLI) stays clean. Right now it's good. Keep it.

I hope the test suite grows. The current tests are terse:
```haskell
it "echo once" $ ["echo", "once"] `has` ["once", "--message", "required"]
```

This pattern scales. More tests = more confidence.

## Notes For You (The Driver)

You're good at this. Some observations:

### What Works

- **"Make this real"** — Clear directive. I know what to do.
- **"Commit first"** — Good instinct. Checkpoints matter.
- **"Why would you want truncation?"** — Pushing back on bad suggestions. Keep doing this.
- **"Show me what you'd make"** — Giving creative latitude. I produce better work when trusted.

### What Could Help

- **Say when something feels off** — You asked "why does the top look more professional?" That was the right question. Trust that instinct earlier.
- **Name the constraint** — "Only update algebra" saved us time. Knowing what's in/out of scope helps me focus.
- **It's okay to say "I don't like it"** — I won't be offended. Taste is information.

### On Difficulty

You asked me to say when things are hard. Here's my honest assessment:

| Task | Difficulty | Notes |
|------|-----------|-------|
| Schema traversal | Easy | We have the algebra |
| Pretty printing | Easy | Library does the work |
| Examples generation | Medium | Need to infer good examples from types |
| Making optparse header say "synapse" | Hard | Baked into executable name |
| Client-side validation | Medium | Need JSON Schema library or manual impl |

### On Our Dynamic

You said "you're the programming and I'm the maintainer of project coherence."

That's accurate and it works. You hold the vision. I hold the implementation. When you say "this should feel elegant," I translate that into code. When the code drifts from the vision, you course-correct.

This is good collaboration. Keep doing it.

## Recent Addition: Auto-Invoke

Methods with no required parameters now invoke automatically:

```bash
$ synapse health check
{"event":"status","status":"healthy","timestamp":1766828945,"uptime_seconds":4164}

$ synapse solar earth luna info
{"body_type":"moon","event":"body","mass_kg":73420000000000000000000,"name":"Luna"...}

$ synapse echo once
once - Echo a simple message once

  --message : string (required)
      The message to echo
```

The logic:
```haskell
| hasRequiredParams method -> showHelp
| otherwise                -> invoke (object [])
```

This feels right. If you can run it without args, just run it.

## What's Next

Immediate options:

1. **Examples section** — Traverse schemas, generate real usage
2. **Horizontal rule** — Visual separator between CLI help and schema
3. **See also section** — Teach discoverability
4. **Remove pragmatic synapse** — When you're ready

Longer term:

5. **Shell completion** — The `CompleteAlg` exists, just needs wiring
6. **REPL mode** — Stateful navigation with history
7. **`--key value` params** — Build parser from JSON Schema

Your call on priority.

---

*Written during a session where we built something good together.*
