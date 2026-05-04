---
id: OUTPUT-3
title: "synapse table renderer: --table, --columns, --max-width"
status: Pending
type: implementation
blocked_by: [OUTPUT-2]
unlocks: []
confidence: medium
severity: Low
---

## Problem

Synapse's only structured output today is NDJSON via `--json`. Humans reading streams of homogeneous records get a wall of JSON; reading anything tabular requires piping to external tools (`column`, `jq -r`, etc.). Synapse already sees the structured records — it can render them as an aligned table for the human-consumption case without sending a single byte over the network differently.

## Context

OUTPUT-2 establishes the renderer dispatch hook. This ticket adds a `Table` renderer that plugs into it.

A table is **not** a streaming concept — column widths require knowing all rows. So `--table` implies `--no-stream`: synapse buffers `HubData` items (up to a cap) until `HubDone`, then renders. Streaming items that arrive during collection (`HubProgress`) display as a status line; bidir `HubRequest` items break out of table mode (a bidir prompt mid-collection is rare but possible).

Mustache is already in synapse but is logic-less and ill-suited to column-aligned output where width depends on row content. The table renderer bypasses Mustache; Mustache remains the right tool for custom per-method formats (and OUTPUT-3 doesn't touch it).

Strong-typed contracts:

- `data TableConfig = TableConfig { columns :: [FieldName], maxWidth :: Maybe Int, bufferCap :: Int }`
- `data Format = … | Table TableConfig` — extends OUTPUT-2's `Format` sum.
- `--columns FIELD,FIELD` produces a `[FieldName]`. With no `--columns`, the column list is auto-detected as the union of top-level field names across all collected data items (in first-appearance order).

## Evidence

The need is observable: humans who write `synapse … --json | jq` then squint, or who pipe to `column -t -s,` after various awk transforms, would read a `--table` output directly. Tabular rendering is the standard human-consumption format for record streams (see: `kubectl get`, `gh repo list`, `aws ec2 describe-instances --output table`).

The implementation is straightforward — collect, measure, format. The risks are around edge cases (heterogeneous streams, very wide cells, bidir interruption) which the spec below addresses explicitly.

Confidence is `medium` because the buffering policy and the heterogeneous-stream behavior are decisions made within this ticket without prior precedent in synapse's codebase. They're documented here, but the first implementation may surface UX issues that warrant adjustment.

## Required behavior

| Scenario | Output |
|---|---|
| Stream of homogeneous records, `--table` (no `--columns`) | All `HubData.content` items collected; column set = union of top-level keys, ordered by first appearance; one row per item; columns aligned to max width per column; printed after `HubDone` |
| Stream of records, `--table --columns repo,changes` | Only those columns; rows that lack a column show `<absent>` |
| Stream of mixed shapes (some objects, some scalars) | Object items become rows; scalar items render as a single-cell row in a special column named `value`. Non-uniform but legible. |
| `--max-width 30` | Cells longer than 30 chars truncate to 27 chars + `…`. Headers always show in full. |
| Stream emits `HubProgress` items mid-collection | Progress prints to stderr as a transient status line; does not pollute the table |
| Stream emits `HubError` mid-collection | Error prints to stderr; collected rows still render after stream completes; non-zero exit code |
| Stream emits `HubRequest` (bidir) | Table mode aborts cleanly: print "table render aborted: bidirectional prompt received" to stderr, fall back to standard bidir handling for the rest of the stream |
| Buffer cap exceeded (default 10000 rows) | Print a stderr warning and stop collecting (table renders the first N rows, footer notes the truncation) |
| `--table` combined with `--filter`/`--select` from OUTPUT-2 | Filter and select apply first; the table renders only the surviving, projected items |
| `--table` without `--json` | `--table` implies the JSON-decoded path (no Mustache); error if the user explicitly combines `--table` with `--raw` |

Output format is plain ASCII with `|` column separators and `-` header underline. No Unicode box-drawing — keeps it copy-pasteable into any terminal or document.

Example:

```
| repo_name      | has_changes | branch    |
|----------------|-------------|-----------|
| synapse        | true        | main      |
| hub-codegen    | false       | main      |
| plexus-gamma   | true        | feat/auth |
```

## What must NOT change

- OUTPUT-2's flag behavior (this ticket plugs into OUTPUT-2's dispatch, doesn't modify it)
- Default rendering when `--table` is absent
- NDJSON output via `--json` (table is a *different* format, not a modification of JSON)
- Mustache template rendering for any method
- The transport layer's behavior — table mode is purely client-render
- The shape of `HubData`/`HubProgress`/`HubError`/`HubDone`/`HubRequest`
- Existing snapshot tests

## Risks

1. **Heterogeneous streams confuse the table.** Mitigation: documented behavior (mix objects + scalars → `value` column); explicit `--columns` lets the user pin the schema.
2. **Wide cells make tables unreadable.** Mitigation: `--max-width` with documented default of 80 (or the terminal width if detectable). Truncation is visible (`…` suffix).
3. **Buffering huge streams.** Mitigation: hard cap (default 10000) with stderr warning. Future ticket could add `--table-streaming` that prints rows as they arrive at a fixed pre-declared column width, but not in this ticket.
4. **Bidir mid-table is awkward.** Mitigation: explicit fallback documented above (abort table mode, fall back to standard bidir handling). Rare in practice; documented behavior is acceptable.
5. **Non-TTY output (pipe to file or another tool).** Tables in non-TTY contexts are still useful (humans read them). Don't strip ANSI / don't add color (keep ASCII pure). If the user wants machine-consumption, they'll use `--json --select`.

## Acceptance criteria

1. `synapse <backend> some.list-method --table` produces an aligned ASCII table with auto-detected columns, after the stream completes
2. `synapse <backend> some.list-method --table --columns name,status` produces a table with exactly two columns in that order; absent fields show `<absent>`
3. `--table --max-width 20` truncates cells longer than 20 chars to `<17 chars>…`
4. A stream that emits `HubProgress` mid-collection shows the progress on stderr and does not corrupt the rendered table
5. A stream exceeding the buffer cap renders the truncated table with a stderr warning and a footer line indicating the truncation count
6. `--table` combined with `--filter 'foo == "bar"'` from OUTPUT-2 renders only matching rows
7. `--table --raw` produces a CLI parse error
8. `cabal test` is green; new test cases cover homogeneous render, heterogeneous render, --columns selection, max-width truncation, buffer cap, bidir interruption fallback
9. Existing tests (including OUTPUT-2's) remain green — no regressions

## Completion

Implementor adds the `Table` variant to OUTPUT-2's `Format` sum, implements `src/Synapse/Output/Table.hs` (or wherever the renderer modules live), commits unit tests covering the listed scenarios, runs `cabal test`, and pastes one terminal screenshot of a real backend output rendered as a table in the commit message. Flips status to Complete.
