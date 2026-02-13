# Architecture Documentation Index

This directory contains architecture decision records and implementation docs for Synapse, sorted reverse-chronologically (newest first).

## Recent Updates (2026)

### Performance Analysis
- **[IR Construction Performance Analysis](./16675729999709551615_ir-construction-performance-analysis.md)** (2026-02-13)
  - Root cause analysis: Why `synapse substrate hash` takes 8 seconds
  - Full IR construction overhead for parameter-less methods
  - Connection pooling opportunities
  - Lazy IR construction proposal
  - Impact on jsexec plexus_env pipeline
  - Implementation plan with 80x performance improvement potential

### Backend Discovery
- **[Multi-Backend Discovery Implementation](./16677357852794265599_multi-backend-discovery-implementation.md)** (2026-01-25)
  - Dynamic backend discovery via `_info` endpoint and registry
  - Zero-config auto-discovery with registry integration
  - Health check system with 300ms timeout
  - Backend deduplication logic
  - Comprehensive challenges, solutions, and cleanup opportunities documented

### CLI Parser Fixes
- **[Array and Normalization Fixes](./16677498315934040575_array-and-normalization-fixes.md)** (2026-01-24)
  - Fixed array parameter handling via repeated flags (`--tags val1 --tags val2`)
  - Added hyphen-to-underscore normalization for method names (`queue-add` → `queue_add`)
  - Comprehensive test coverage for path/param normalization

### Error Handling
- **[Error Handling Improvements](./16677364953367891711_error-handling-improvements.md)**
  - Enhanced error messages and diagnostics

### Template System
- **[Self Template CRUD Implementation](./16677504937180061695_self-template-crud-implementation.md)**
  - Template management via `_self` commands
- **[Template Rendering Challenges](./16678045264785245695_template-rendering-challenges.md)**
  - Template resolution and caching

### TypeScript Codegen
- **[TypeScript Naming Convention Fix](./16677764457979385599_typescript-naming-convention-fix.md)**
- **[TypeScript Codegen Snake Case](./16677793986132070655_typescript-codegen-snake-case.md)**
- **[TypeScript Namespace Collision](./16677936867500161535_typescript-namespace-collision.md)**

## Core Architecture

### IR (Intermediate Representation)
- **[TypeRef Restructuring](./16677885089803091711_typeref-restructuring-handoff.md)**
- **[Compiler Architecture](./16679613932789736703_compiler-architecture.md)**
- **[IR Builder](./16680785784679922687_client-codegen-requirements.md)**

### CLI Implementation
- **[Dynamic CLI Implementation](./16680974430007725567_dynamic-cli-implementation.md)**
- **[Schema to CLI Pipeline](./16680606527890548735_schema-to-cli-pipeline.md)**
- **[Method Schema Spec](./16679174944041108735_method-schema-spec.md)**

### RPC Protocol
- **[Introspective RPC Protocol](./16680807091363337727_introspective-rpc-protocol.md)**
- **[Substrate RPC Protocol](./16681062792662575615_substrate-rpc-protocol.md)**
- **[Multi-Hub Transport Envelope](./16679517135570018559_multi-hub-transport-envelope.md)**

### Category Theory Foundation
- **[Category Theory Implementation](./16680343462706939647_category-theory-implementation.md)**
- **[Guidance Implementation Status](./16680876922578453503_guidance-implementation-status.md)**

## All Documents

To see all architecture docs:
```bash
ls -t docs/architecture/*.md
```

Documents use reverse-chronological naming: `(u64::MAX - nanotime)_title.md`

This ensures newest documents appear first when sorted alphabetically.

## Creating New Docs

Generate filename:
```python
import time
nanotime = int(time.time() * 1_000_000_000)
filename = (2**64 - 1) - nanotime
print(f'{filename}_your-title.md')
```

Then add an entry to this index under the appropriate section.
