# Synapse CLI: Design Document

## Abstract

Synapse is a command-line interface that derives its structure from the schema coalgebra exposed by the Plexus plugin system. The CLI is not hardcoded — it is computed. The plugin system forms a free category; the CLI is a functor from that category to the category of parsers and renderers.

---

## Part I: Categorical Foundation

### The Category

Plexus exposes a free category:

| Component | Instantiation |
|-----------|---------------|
| Objects | Schemas, identified by hash |
| Morphisms | Paths — sequences of child references |
| Identity | Empty path |
| Composition | Path concatenation |

This category was verified empirically. Object identity is stable (hashes are deterministic and faithful). Morphisms are stable (resolution is idempotent). Composition is total (all references resolve).

### The Functor

The schema functor F describes one layer of observation:

F(X) = Namespace × Version × Description × Hash × List(Method) × Option(List(X))

In the shallow representation, X = ChildSummary — a reference, not a full schema. Expansion is lazy.

### The Coalgebra

The plugin system is an F-coalgebra:

```
schema  : Plugin → F(ChildSummary)
resolve : ChildSummary → F(ChildSummary)
```

Each call reveals one layer. The child summaries are references to further layers. The anamorphism unfolds on demand.

### The Algebras

The CLI implements multiple F-algebras over the schema structure:

| Algebra | Carrier | Purpose |
|---------|---------|---------|
| Navigate | Path → Either Error SchemaView | Find target by path |
| Render | Text | Produce help output |
| Complete | [Text] | Collect names for shell completion |
| Validate | Params → Either Error Value | Check params against JSON schema |

Each is a catamorphism (or paramorphism where substructure is needed).

### The Hylomorphism

The full pipeline:

```
Address → fetch (ana) → Schema → fold (cata) → Result
```

Unfold from address to schema. Fold from schema to output. The composition is a distributed hylomorphism — the anamorphism runs on Rust, the catamorphism runs on Haskell, the wire is the boundary.

---

## Part II: Type System

### Core Types

```haskell
-- Object identity
newtype PluginHash = PluginHash Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, Hashable)

-- One layer of schema (what we receive on the wire)
data PluginSchema = PluginSchema
  { psNamespace   :: Text
  , psVersion     :: Text
  , psDescription :: Text
  , psHash        :: PluginHash
  , psMethods     :: [MethodSchema]
  , psChildren    :: Maybe [ChildSummary]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Reference to a child (the hole)
data ChildSummary = ChildSummary
  { csNamespace   :: Text
  , csDescription :: Text
  , csHash        :: PluginHash
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- Method description
data MethodSchema = MethodSchema
  { msName        :: Text
  , msDescription :: Text
  , msParams      :: Maybe Value  -- JSON Schema
  , msReturns     :: Maybe Value  -- JSON Schema
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
```

### The Base Functor

```haskell
-- One layer with a hole for recursion
data PluginSchemaF a = PluginSchemaF
  { psfNamespace   :: Text
  , psfVersion     :: Text
  , psfDescription :: Text
  , psfHash        :: PluginHash
  , psfMethods     :: [MethodSchema]
  , psfChildren    :: Maybe [a]
  }
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

-- The wire format is F(ChildSummary)
type ShallowSchema = PluginSchemaF ChildSummary

-- The fully expanded tree is Fix F
type PluginTree = Fix PluginSchemaF

-- Isomorphism: PluginSchema ≅ PluginSchemaF ChildSummary
toShallow :: PluginSchema -> ShallowSchema
toShallow PluginSchema{..} = PluginSchemaF
  { psfNamespace   = psNamespace
  , psfVersion     = psVersion
  , psfDescription = psDescription
  , psfHash        = psHash
  , psfMethods     = psMethods
  , psfChildren    = psChildren
  }

fromShallow :: ShallowSchema -> PluginSchema
fromShallow PluginSchemaF{..} = PluginSchema
  { psNamespace   = psfNamespace
  , psVersion     = psfVersion
  , psDescription = psfDescription
  , psHash        = psfHash
  , psMethods     = psfMethods
  , psChildren    = psfChildren
  }
```

### Navigation Types

```haskell
-- A position in the schema tree
data SchemaView
  = ViewPlugin PluginSchema [Text]   -- schema at path
  | ViewMethod MethodSchema [Text]   -- method at path
  deriving stock (Show, Eq)

-- Navigation errors
data NavError
  = NotFound Text [Text]             -- segment not found at path
  | MethodNotTerminal Text [Text]    -- method with trailing path
  | Cycle PluginHash [Text]          -- cycle detected
  deriving stock (Show, Eq)

-- Stream items from method invocation
data StreamItem
  = StreamData     StreamMeta Text Value
  | StreamProgress StreamMeta Text (Maybe Float)
  | StreamError    StreamMeta Text (Maybe Text) Bool
  | StreamDone     StreamMeta
  deriving stock (Show, Eq)

data StreamMeta = StreamMeta
  { smProvenance :: [Text]
  , smHash       :: PluginHash
  , smTimestamp  :: Int64
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
```

### Effect Stack

```haskell
-- The monad for CLI operations
type SynapseM = ExceptT SynapseError (ReaderT SynapseEnv IO)

data SynapseEnv = SynapseEnv
  { seManager    :: Manager           -- HTTP connection manager
  , seBaseUrl    :: Text              -- substrate address
  , seCache      :: IORef SchemaCache -- schema cache by hash
  , seVisited    :: IORef (Set PluginHash) -- cycle detection
  }

data SynapseError
  = NavError NavError
  | TransportError Text
  | ParseError Text
  | ValidationError Text
  deriving stock (Show, Eq)

-- Cache for resolved schemas
type SchemaCache = HashMap PluginHash PluginSchema
```

---

## Part III: The Algebras

### Navigation (Paramorphism)

Navigation requires inspecting child namespaces to match path segments. This needs the original structure, not just the folded result. Hence paramorphism.

```haskell
-- The algebra type for para
-- At each layer, we receive (original, result) pairs for children
type NavAlg = PluginSchemaF (ChildSummary, NavResult) -> NavResult
type NavResult = [Text] -> [Text] -> SynapseM SchemaView

navAlg :: NavAlg
navAlg layer = \visited path ->
  case path of
    -- Empty path: return current plugin
    [] -> pure $ ViewPlugin (reconstructSchema layer) visited

    -- Non-empty: try to navigate
    (seg:rest) ->
      case findChild seg (psfChildren layer) of
        Just (child, childNav) -> do
          -- Check for cycle before descending
          checkCycle (csHash child)
          -- Fetch the child schema
          childSchema <- fetch child
          -- Continue navigation in child
          navigate childSchema (visited ++ [seg]) rest

        Nothing ->
          case findMethod seg (psfMethods layer) of
            Just method
              | null rest -> pure $ ViewMethod method (visited ++ [seg])
              | otherwise -> throwError $ NavError $ MethodNotTerminal seg visited
            Nothing -> throwError $ NavError $ NotFound seg visited

  where
    findChild :: Text -> Maybe [(ChildSummary, a)] -> Maybe (ChildSummary, a)
    findChild seg = (>>= find ((== seg) . csNamespace . fst))

    findMethod :: Text -> [MethodSchema] -> Maybe MethodSchema
    findMethod seg = find ((== seg) . msName)

    reconstructSchema :: PluginSchemaF (ChildSummary, a) -> PluginSchema
    reconstructSchema f = fromShallow $ fmap fst f
```

However, since resolution is effectful (network calls), we cannot use pure `para`. We implement navigation directly:

```haskell
navigate :: PluginSchema -> [Text] -> [Text] -> SynapseM SchemaView
navigate schema visited = \case
  [] -> pure $ ViewPlugin schema visited

  (seg:rest) ->
    case findChild seg (psChildren schema) of
      Just child -> do
        checkCycle (csHash child)
        childSchema <- fetch child
        navigate childSchema (visited ++ [seg]) rest

      Nothing ->
        case findMethod seg (psMethods schema) of
          Just method
            | null rest -> pure $ ViewMethod method (visited ++ [seg])
            | otherwise -> throwError $ NavError $ MethodNotTerminal seg visited
          Nothing -> throwError $ NavError $ NotFound seg visited

  where
    findChild seg = (>>= find ((== seg) . csNamespace))
    findMethod seg = find ((== seg) . msName)
```

### Rendering (Catamorphism)

Help rendering is a pure fold. Children are already rendered to Text.

```haskell
type RenderAlg = PluginSchemaF Text -> Text

renderAlg :: RenderAlg
renderAlg PluginSchemaF{..} = T.unlines $ concat
  [ [psfNamespace <> " v" <> psfVersion]
  , [""]
  , [psfDescription]
  , [""]
  , renderMethods psfMethods
  , maybe [] renderChildren psfChildren
  ]

renderMethods :: [MethodSchema] -> [Text]
renderMethods [] = []
renderMethods ms = "Methods:" : map renderMethod ms

renderMethod :: MethodSchema -> Text
renderMethod MethodSchema{..} = "  " <> msName <> "    " <> msDescription

renderChildren :: [Text] -> [Text]
renderChildren [] = []
renderChildren cs = "" : "Plugins:" : map ("  " <>) cs

-- For shallow schemas, we render child summaries directly
renderShallow :: PluginSchema -> Text
renderShallow PluginSchema{..} = T.unlines $ concat
  [ [psNamespace <> " v" <> psVersion]
  , [""]
  , [psDescription]
  , [""]
  , renderMethods psMethods
  , maybe [] (("" :) . ("Plugins:" :) . map renderChildSummary) psChildren
  ]

renderChildSummary :: ChildSummary -> Text
renderChildSummary ChildSummary{..} = "  " <> csNamespace <> "    " <> csDescription
```

### Completion (Catamorphism)

Collect all names at current level for shell tab-completion.

```haskell
type CompleteAlg = PluginSchemaF [Text] -> [Text]

completeAlg :: CompleteAlg
completeAlg PluginSchemaF{..} = concat
  [ map msName psfMethods
  , fromMaybe [] psfChildren
  ]

-- For shallow schema
completions :: PluginSchema -> [Text]
completions PluginSchema{..} = concat
  [ map msName psMethods
  , maybe [] (map csNamespace) psChildren
  ]
```

### Validation (Catamorphism over JSON Schema)

Validate params against method's JSON schema.

```haskell
validate :: MethodSchema -> [(Text, Text)] -> Either ValidationError Value
validate method params = do
  case msParams method of
    Nothing
      | null params -> Right Null
      | otherwise   -> Left $ UnexpectedParams (map fst params)

    Just schema -> do
      obj <- parseParams params
      validateAgainstSchema schema obj
```

---

## Part IV: The Transport

### Wire Protocol

The substrate exposes WebSocket JSON-RPC with streaming:

```
ws://127.0.0.1:4444

{"jsonrpc":"2.0","method":"substrate.call","params":{"method":"..."},"id":1}
```

Response streams as subscription notifications.

### Resolution

```haskell
-- Fetch a schema by path
fetchSchema :: Text -> SynapseM PluginSchema
fetchSchema path = do
  let method = if T.null path then "substrate.schema" else path <> ".schema"
  result <- rpcCall "substrate.call" (object ["method" .= method])
  parseSchema result

-- Resolve a child summary to full schema
fetch :: ChildSummary -> SynapseM PluginSchema
fetch child = do
  -- Check cache first
  cached <- lookupCache (csHash child)
  case cached of
    Just schema -> pure schema
    Nothing -> do
      schema <- fetchSchema (csNamespace child)
      insertCache (csHash child) schema
      pure schema
```

### Streaming

Method invocation streams results:

```haskell
invoke :: Text -> Value -> ConduitT () StreamItem SynapseM ()
invoke method params = do
  response <- rpcCallStreaming "substrate.call"
    (object ["method" .= method, "params" .= params])
  parseStreamItems response
```

---

## Part V: Cycle Detection

The category may contain cycles. The carrier (live plugins) lives in νF. Detection happens at walk-time.

```haskell
checkCycle :: PluginHash -> SynapseM ()
checkCycle hash = do
  visited <- asks seVisited >>= liftIO . readIORef
  when (hash `Set.member` visited) $
    throwError $ NavError $ Cycle hash (Set.toList visited)
  asks seVisited >>= liftIO . flip modifyIORef (Set.insert hash)

-- Reset visited set for new navigation
withFreshVisited :: SynapseM a -> SynapseM a
withFreshVisited action = do
  ref <- asks seVisited
  old <- liftIO $ readIORef ref
  liftIO $ writeIORef ref Set.empty
  result <- action
  liftIO $ writeIORef ref old
  pure result
```

---

## Part VI: CLI Interface

### Argument Structure

```
synapse [--host HOST] [--port PORT] [PATH...] [-p JSON]
```

Examples:

```bash
synapse                              # root schema
synapse echo                         # echo plugin schema
synapse echo once -p '{"message":"hi"}'  # invoke with params
synapse solar earth luna info        # deep navigation
```

### Main Loop

```haskell
main :: IO ()
main = do
  args <- parseArgs
  env <- initEnv args
  result <- runSynapseM env (dispatch args)
  case result of
    Left err -> die (renderError err)
    Right () -> pure ()

dispatch :: Args -> SynapseM ()
dispatch Args{..} = do
  root <- fetchSchema ""
  view <- withFreshVisited $ navigate root [] argPath

  case view of
    ViewPlugin schema _ ->
      liftIO $ T.putStrLn $ renderShallow schema

    ViewMethod method path
      | null argParams ->
          liftIO $ T.putStrLn $ renderMethod method
      | otherwise -> do
          params <- liftEither $ validate method argParams
          let methodPath = T.intercalate "." path
          runConduit $ invoke methodPath params .| printStream
```

---

## Part VII: Properties and Invariants

### Category Preservation

1. **Object identity.** Same hash implies same schema. Cache is safe.

2. **Morphism stability.** Same path yields same result (modulo system changes).

3. **Composition.** Path concatenation corresponds to sequential navigation.

4. **Identity.** Empty path returns current schema.

### Termination

Navigation terminates if:
- The path is finite (always true — user input)
- Cycle detection fires before infinite loop

### Correctness

The CLI is correct if:
- `navigate schema path` returns the schema at path
- `render (navigate schema path)` produces accurate help
- `invoke path params` calls the right method with validated params

---

## Part VIII: Future Extensions

### Tab Completion

Generate shell completion scripts from schema:

```bash
synapse --generate-completion bash > /etc/bash_completion.d/synapse
```

### REPL Mode

Interactive navigation:

```
synapse> plexus
plexus v1.0.0
...

synapse/plexus> echo
echo v1.0.0
...

synapse/plexus/echo> once -p '{"message":"hi"}'
{"message": "hi"}
```

### `--key value` Parameter Syntax

Build optparse-applicative parsers from method JSON Schema:

```bash
synapse echo once --message "hello" --count 3
```

---

## Conclusion

Synapse is a CLI derived from categorical structure. The plugin system exposes a free category — schemas as objects, paths as morphisms. The CLI implements algebras over this category: navigation, rendering, completion, validation.

The CLI does not know about plugins. It knows about the functor. Add a plugin, the category extends, the CLI follows. Remove a plugin, the category contracts, the CLI adapts.

The mathematics is not metaphor. It is implementation.
