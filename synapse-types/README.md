# synapse-types

Type-safe representations of Plexus RPC protocol primitives using **refinement types**.

## Purpose

Provides compile-time guarantees for protocol invariants, making invalid protocol states **unrepresentable**.

## Type Safety Guarantees

### PlexusHash
- **Constraint**: Exactly 16 hexadecimal characters
- **Example**: `"5f14548168e540a7"`
- **Prevents**: Invalid hash formats, wrong lengths, non-hex characters

### Timestamp
- **Constraint**: Positive `Int64` (Unix seconds)
- **Prevents**: Float timestamp bug (precision loss in JSON serialization)
- **Example**: `1774238620`

### SnakeCaseField
- **Constraint**: Enforces `snake_case` wire format
- **Prevents**: Mixed case field names (`camelCase`, `PascalCase`)
- **Example**: `"content_type"`, `"plexus_hash"`

### Provenance
- **Constraint**: Non-empty chain of activation namespaces
- **Prevents**: Empty provenance (untraceable events)
- **Example**: `["substrate", "ping"]`

### Port
- **Constraint**: Valid port number (1-65535)
- **Prevents**: Port 0, negative ports, out-of-range values
- **Example**: `4444`

## Architecture

Built on the `refined` library, which provides:
- **Compile-time checking** where possible
- **Runtime validation** when values come from external sources
- **Type-level predicates** encoded in the type system

## Usage

```haskell
import Synapse.Types.Refined
import Synapse.Types.Protocol

-- Create a validated timestamp
timestamp <- refine @Positive @Int64 1774238620

-- Create a validated port
port <- refine @PortRange @Int 4444

-- Create a plexus hash (validated at construction)
hash <- refinePlexusHash "5f14548168e540a7"
```

## Why Refinement Types?

Traditional approach:
```haskell
data Metadata = Metadata
  { timestamp :: Int64  -- Could be negative!
  , hash :: String      -- Could be any string!
  , provenance :: [String]  -- Could be empty!
  }
```

With refinement types:
```haskell
data Metadata = Metadata
  { timestamp :: Refined Positive Int64
  , hash :: PlexusHash
  , provenance :: Refined NonEmpty [String]
  }
```

Invalid values **cannot be constructed** - they're rejected at compile time or construction time.

## Testing

Includes property-based tests with QuickCheck:
- Round-trip serialization (JSON)
- Refinement predicate validation
- Regression tests for known bugs (float timestamp)

```bash
cabal test synapse-types-test
```

## Integration

Used by:
- `plexus-protocol` - Wire format validation
- `synapse` - CLI parameter validation
- Substrate implementations - Event metadata

## Dependencies

- `refined ^>= 0.8` - Core refinement type library
- `aeson ^>= 2.1` - JSON serialization
- `text ^>= 2.0` - Text handling

## License

MIT
