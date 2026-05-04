---
id: OUTPUT-1
title: "OUTPUT — agent-friendly stream shaping for synapse CLI"
status: Pending
type: epic
blocked_by: []
unlocks: []
---

## Goal

Make synapse's streaming output usable directly by agents without temp-file workarounds and usable by humans without scrolling through walls of NDJSON. Add stream-shaping primitives (projection, filter, head, count, table rendering) that operate on `PlexusStreamItem`s before they reach stdout, so the agent or user gets exactly what they want and nothing more.

The destination is observable: an agent can replace the current `synapse … > /tmp/x.txt 2>&1 && grep "field_a\|field_b" /tmp/x.txt` pattern with `synapse … --select field_a,field_b` (or similar), getting structured output sized for its context window. A human can replace JSON-eyeballing with `--table` for tabular results.

## Evidence

The May 2026 conversation surfaced a recurring agent-friction pattern (verbatim example: `synapse -P 44104 lforge hyperforge build dirty --path … > /tmp/dirty3.txt 2>&1 && grep "repo_name\|has_changes\|has_u…"`). Agents redirect to temp files because:

1. Output is much larger than what they need
2. The structured NDJSON is hard to grep, and `jq` for stream-of-records is awkward
3. They lack a synapse-native way to express "I only want these fields"

Verified that synapse's `--json` output is NDJSON (one `PlexusStreamItem` per line) at `synapse/app/Main.hs:1016` (`printResult True _ _ item = LBS.putStrLn $ encode item`). Synapse already sees the structured items in flight — projection at this layer is a small, mechanical addition that eliminates the temp-file dance entirely.

Tabular rendering is a separate concern but composes with the same renderer dispatch. Mustache (already in synapse) is logic-less and ill-suited to adaptive column widths; a dedicated table renderer is cleaner than extending Mustache. Server-side content-type hints (`application/json+plexus-table`) can come later as additive optimization but are not required — the chrome is the right place to decide rendering, exactly as in browsers.

## Scope

| Crate | What changes |
|-------|-------------|
| **synapse** | New CLI flags (`--select`, `--filter`, `--head`, `--tail`, `--count`, `--table`, `--columns`, `--format`); renderer dispatch refactor in `app/Main.hs`; new modules under `src/Synapse/Output/` |

This epic does not touch plexus-core, plexus-transport, hub-codegen, or substrate. Pure synapse-CLI work.

## Dependency DAG

```
OUTPUT-2 (stream-shaping flags + renderer dispatch) ──► OUTPUT-3 (table renderer plugs into dispatch)
```

OUTPUT-2 introduces both the new flags and the renderer dispatch hook. OUTPUT-3 plugs in as an additional renderer; it does not have to fight OUTPUT-2 over `printResult` because the dispatch was already established. Any future format renderer (Mustache enhancements, a chart renderer, etc.) plugs in identically.

## Phases

### Phase 1 — Projection / filter / head / count (OUTPUT-2)
Establishes the renderer dispatch in `app/Main.hs`, adds the first non-default renderer (a "shaped JSON" renderer that applies projection/filter/limit before NDJSON-encoding). Default behavior unchanged.

### Phase 2 — Table renderer (OUTPUT-3)
Adds `--table` and `--columns` as a new renderer. Buffers the data stream (with a configurable cap), aligns columns, prints the table after the stream completes. Non-data items (`progress`, `error`, `done`, `request`) handled appropriately.

## Tickets

| ID | Type | Confidence | Summary |
|---|---|---|---|
| OUTPUT-2 | impl | high | Stream-shaping flags: `--select`, `--filter`, `--head`, `--tail`, `--count` |
| OUTPUT-3 | impl | medium | Table renderer: `--table`, `--columns`, `--max-width`, `--format` |

## Out of scope

- **Server-side content-type hints.** `PlexusStreamItem::Data.content_type` already exists; activations could set richer types in the future to suggest a preferred renderer. Pure addition; not required for this epic to ship.
- **Mustache template helpers for table layouts.** The table renderer bypasses Mustache entirely. Mustache stays the right tool for custom per-method formats; tables are a separate path.
- **Large-output stderr hint** ("8.4 MB streamed; consider --select"). Worth doing later as a small standalone addition; not blocking this epic.
- **`--summary` / aggregation flag.** Future addition; not in initial scope.
- **`--jq EXPR` passthrough.** Considered but deferred. `--select` and `--filter` cover the common case in a way that doesn't require external jq; power users can still pipe to `jq` themselves.
