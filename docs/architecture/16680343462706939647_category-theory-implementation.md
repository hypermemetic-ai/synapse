# Category Theory in Synapse: Implementation & Shortcuts

## The Vision

The original design described a coalgebraic architecture:

```
Rust (Substrate)     Plexus Protocol      Haskell (Synapse)
─────────────────    ───────────────      ─────────────────
νF (coalgebra)   →   JSON Schema      →   μF (algebra)
unfold/anamorphism   streaming            fold/catamorphism
```

- **Rust side**: Coalgebras that unfold plugin structure on demand
- **Haskell side**: Algebras that fold/consume the structure
- **Protocol**: The boundary where νF (possibly infinite) gets serialized to μF (finite tree)

## What We Actually Implemented

### 1. Initial Algebra (μF) for Schema

**File**: `meaning/src/Plexus/Schema/Recursive.hs`

```haskell
data PluginSchema = PluginSchema
  { psNamespace   :: Text
  , psVersion     :: Text
  , psDescription :: Text
  , psHash        :: PluginHash
  , psMethods     :: [MethodSchema]
  , psChildren    :: Maybe [PluginSchema]  -- Nothing = leaf, Just = parent
  }

makeBaseFunctor ''PluginSchema  -- Generates PluginSchemaF
```

This gives us:
- `PluginSchema` as μF (the fixed point)
- `PluginSchemaF` as the base functor F
- Automatic `Recursive` instance for folding

**Status**: ✅ Fully implemented

### 2. Catamorphisms for Folding

**File**: `meaning/src/Plexus/Schema/Recursive.hs`

```haskell
-- Collect all namespaces (flat list)
collectNamespaces :: PluginSchema -> [Text]
collectNamespaces = cata nsAlg
  where
    nsAlg (PluginSchemaF ns _ _ _ _ children) =
      ns : concat (fromMaybe [] children)

-- Calculate tree depth
schemaDepth :: PluginSchema -> Int
schemaDepth = cata depthAlg
  where
    depthAlg (PluginSchemaF _ _ _ _ _ Nothing) = 1
    depthAlg (PluginSchemaF _ _ _ _ _ (Just cs)) = 1 + maximum (0 : cs)

-- Count total methods
countMethods :: PluginSchema -> Int
countMethods = cata methodAlg
  where
    methodAlg (PluginSchemaF _ _ _ _ methods children) =
      length methods + sum (fromMaybe [] children)
```

**Status**: ✅ Implemented but underutilized

### 3. Paramorphism for Navigation

```haskell
-- Navigation needs original subtrees (not just folded results)
navigate :: [Text] -> PluginSchema -> Maybe PluginSchema
navigate path = para navAlg
  where
    navAlg :: PluginSchemaF (PluginSchema, Maybe PluginSchema) -> Maybe PluginSchema
    navAlg (PluginSchemaF ns _ _ _ _ children) = ...
```

**Status**: ✅ Defined but not used in CLI (see shortcuts below)

## Where We Skimped

### 1. CLI Parser: Manual Recursion Instead of Schemes

**File**: `synapse/app/Main.hs`

The CLI parser builds optparse-applicative parsers recursively, but uses standard Haskell recursion:

```haskell
schemaParser :: [Text] -> PluginSchema -> Parser Command
schemaParser path schema =
  subcommandParser path schema <|> pure (CmdShowHelp schema)

subcommandParser :: [Text] -> PluginSchema -> Parser Command
subcommandParser path schema = subparser $ mconcat $
  [ command (T.unpack $ psNamespace child)
      (info (schemaParser (path <> [psNamespace child]) child) ...)  -- manual recursion
  | child <- pluginChildren schema
  ]
```

**What we should have done**: Use a hylomorphism or paramorphism to build the parser from the schema algebra.

**Why we didn't**: optparse-applicative's `Parser` type doesn't compose well with recursion-schemes. Would need a custom carrier type.

### 2. Path Tracking: Ad-hoc List Instead of Zipper

We track navigation path as `[Text]`:

```haskell
data Command
  = CmdInvoke [Text] Text Value   -- path, method, params
  | CmdShowHelp PluginSchema
```

**What we should have done**: Use a zipper comonad to track position in the tree:

```haskell
-- Conceptual
data SchemaZipper = SchemaZipper
  { focus   :: PluginSchema
  , context :: [PluginSchema]  -- breadcrumbs
  }
```

**Why we didn't**: The path list is simpler and sufficient for our needs. A zipper would enable going "up" the tree, which we don't need.

### 3. Rendering: Direct Pattern Match Instead of Catamorphism

```haskell
renderSchema :: PluginSchema -> Text
renderSchema schema = T.unlines $
  [ psNamespace schema <> " - " <> psDescription schema
  ] <> map renderMethod (psMethods schema)
    <> map renderChild (pluginChildren schema)
```

**What we should have done**:

```haskell
renderSchema :: PluginSchema -> Text
renderSchema = cata renderAlg
  where
    renderAlg :: PluginSchemaF Text -> Text
    renderAlg (PluginSchemaF ns _ desc _ methods children) = ...
```

**Why we didn't**: We wanted one-level-deep rendering (methods + child names, not recursing into children's methods). A catamorphism folds the entire tree. Would need an apomorphism or paramorphism for controlled depth.

### 4. No Final Coalgebra (νF) Representation

The design document mentioned:

```
νF = greatest fixed point = final coalgebra
   = possibly infinite/cyclic structures
   = the "live" plugin network that could have cycles
```

We never implemented this. The Haskell side only sees μF (finite trees serialized over the wire).

**Why we didn't**: The protocol already serializes to finite JSON. Cycles would need explicit handling (graph rather than tree).

### 5. Streaming: Not Modeled Categorically

```haskell
plexusRpc :: PlexusConnection -> Text -> Value
          -> Stream (Of PlexusStreamItem) IO ()
```

We use `streaming` library's `Stream` type, which is a form of free monad, but we don't leverage any category-theoretic structure.

**What we could have done**: Model the stream as a coalgebra:

```haskell
-- Stream as coalgebra: S → F(S) where F(A) = Maybe (Item, A)
type StreamCoalg s = s -> Maybe (PlexusStreamItem, s)
```

**Why we didn't**: The streaming library handles this well enough. Extra abstraction wouldn't provide practical benefit.

## Assessment

| Component | Theory | Implementation | Gap |
|-----------|--------|----------------|-----|
| Schema type | μF initial algebra | ✅ `makeBaseFunctor` | None |
| Navigation | Paramorphism | ❌ Manual + path list | Significant |
| Rendering | Catamorphism | ❌ Direct recursion | Minor |
| CLI parsing | Hylomorphism | ❌ Manual recursion | Moderate |
| Streaming | Coalgebra | ❌ Library abstraction | Minor |
| Live network | νF coalgebra | ❌ Not implemented | N/A (not needed) |

## Verdict

We established the categorical foundation (`makeBaseFunctor`, `cata`, `para`) but then mostly wrote standard recursive Haskell code. The foundation is there for future refactoring, but current implementation prioritizes pragmatism over purity.

The main value of the categorical framing:
1. **Conceptual clarity**: Knowing Rust unfolds (coalgebra) and Haskell folds (algebra)
2. **Future extensibility**: Can swap in proper recursion-schemes when needed
3. **Type safety**: `PluginSchema` is correctly modeled as μF

The main gaps:
1. CLI parser could be a proper algebra
2. Navigation could use zippers/comonads
3. Rendering could be a depth-limited catamorphism
