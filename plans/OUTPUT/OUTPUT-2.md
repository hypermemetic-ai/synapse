---
id: OUTPUT-2
title: "synapse adds stream-shaping flags: --select, --filter, --head, --tail, --count"
status: Ready
type: implementation
blocked_by: []
unlocks: [OUTPUT-3]
confidence: high
severity: Medium
---

## Problem

Agents driving synapse against a backend that returns long structured streams resort to `synapse … > /tmp/x.txt && grep "field_a\|field_b" /tmp/x.txt` because synapse has no native way to project specific fields, filter by predicate, or limit row count. The temp-file dance is wasteful (writes data agents will discard) and brittle (line-grep on JSON misses fields with the same name embedded in nested structures). Synapse already has the structured items in hand — it can do better at the render boundary.

## Context

Synapse's NDJSON output path is `printResult True _ _ item = LBS.putStrLn $ encode item` at `synapse/app/Main.hs:1016`. Items arrive as `HubData`, `HubProgress`, `HubError`, `HubDone`, and (for bidir) `HubRequest`. The new flags apply only to `HubData` items — non-data items pass through unchanged so progress/error/done semantics aren't broken.

This ticket also establishes the renderer dispatch hook that OUTPUT-3 (table renderer) plugs into. The hook should be small: a `Renderer` typeclass or sum-type that `printResult` dispatches to based on the chosen format.

Strong-typed contracts:

- `FieldName(Text)` — a JSON field name referenced by `--select` or `--filter`. Newtype with `#[serde(transparent)]`-equivalent (Aeson `FromJSON`/`ToJSON`).
- `FilterExpr(Text)` — the predicate passed to `--filter`. Parsed at CLI boundary into a typed AST; evaluated against each `HubData.content`.
- `data Format = Json | ShapedJson { selectFields :: [FieldName], filterExpr :: Maybe FilterExpr, head :: Maybe Int, tail :: Maybe Int, count :: Bool } | Table {…} | …` — the dispatch tag.

## Evidence

The temp-file pattern was directly observed in user workflows (May 2026). NDJSON framing was verified at `synapse/app/Main.hs:1016`. The fix is mechanical — projection happens after JSON decoding of `content`, before re-encoding. Filter syntax: start with the smallest defensible subset (`field == "literal"`, `field != "literal"`, `field == true|false|null`, `&&` / `||` / parens). Anything more complex, the user pipes to `jq`.

Confidence is `high` because the contract is concrete (each flag has unambiguous behavior on `HubData`), the implementation surface is bounded (one new module + minimal `Main.hs` changes), and the tests are deterministic (feed a fixture stream, assert the shaped output).

## Required behavior

| Flag combination | Behavior |
|---|---|
| `--select repo_name,has_changes` | Each `HubData.content` JSON object is projected to `{ repo_name: …, has_changes: … }`. Fields absent from the source object render as `null`. Non-object content (string, number, array) passes through unchanged. |
| `--filter 'has_changes == true'` | `HubData` items where the predicate evaluates true pass; others are dropped. Predicate evaluates against `HubData.content`. Items where the field is missing evaluate to `null`, which fails the equality check (so they're dropped). |
| `--head 10` | First 10 `HubData` items pass; subsequent ones are dropped. Stream is consumed eagerly until done, but no more data items emit after the limit. |
| `--tail 10` | Last 10 `HubData` items pass. Requires buffering (memory cost noted in stderr if buffer cap exceeded). |
| `--count` | No `HubData` is emitted. After the stream completes (`HubDone`), a single line `{ "count": N }` is printed. |
| Combinations | Apply in order: filter → select → head/tail. `--count` short-circuits emission but still respects filter. |
| Without `--json` | Flags are ignored; existing template/raw rendering applies (flags are a `--json` companion only) |
| Non-`HubData` items (`HubProgress`, `HubError`, `HubDone`, `HubRequest`) | Pass through to stdout unchanged in NDJSON mode regardless of flag combination |

`--head` and `--tail` are mutually exclusive (error at CLI parse time if both given). `--count` is mutually exclusive with `--select`/`--head`/`--tail` (it's a terminal aggregation).

The `--filter` predicate grammar (subset of jq-style):

```
expr   ::= and_expr ('||' and_expr)*
and_expr ::= cmp ('&&' cmp)*
cmp    ::= field op literal | '(' expr ')' | '!' expr
op     ::= '==' | '!='
literal ::= '"text"' | number | true | false | null
field   ::= identifier ('.' identifier)*
```

Anything outside this grammar produces a CLI parse error with a one-line example in stderr.

## What must NOT change

- Behavior of `synapse --json` without any new flags (existing NDJSON output unchanged)
- Behavior of `synapse` without `--json` (Mustache/raw rendering unchanged)
- Wire format of stream items (this is a CLI-side render-boundary change)
- The shape of `HubData`, `HubProgress`, `HubError`, `HubDone`, `HubRequest` types
- Existing tests in synapse's test suite

## Risks

1. **`--filter` grammar scope creep.** Mitigation: hard-stop at the documented subset. Anything more complex, error out and suggest `| jq`.
2. **`--tail` memory.** Buffering the whole stream into memory is bad for huge streams. Mitigation: bounded ring buffer; if the cap (configurable, default 10000 items) is exceeded, emit a stderr warning and continue (truncating older items in the buffer).
3. **Field projection on non-object content.** `--select` on a string or number is meaningless. Mitigation: pass through unchanged; document the behavior.

## Acceptance criteria

1. `synapse <backend> some.method --json --select foo,bar` emits NDJSON where each data line is an object with at most `foo` and `bar` keys (and any non-data items pass through unchanged)
2. `synapse <backend> some.method --json --filter 'foo == "bar"'` emits only data lines whose `content.foo` equals `"bar"` literally
3. `synapse <backend> some.method --json --head 5` emits at most 5 data lines, then stops emitting data (other item types still pass through until stream end)
4. `synapse <backend> some.method --json --count` emits no data lines, and emits exactly one line `{"count": N}` after the stream completes
5. `--head` combined with `--tail` produces a CLI parse error with a one-sentence message
6. `--filter 'invalid syntax here'` produces a CLI parse error pointing at the column of the failure
7. Running with no new flags produces output byte-identical to the current `--json` output (regression check against a snapshot)
8. `cabal test` for synapse is green; new test cases cover at least: select projection, filter pass/drop, head limit, count aggregate, predicate-syntax-error
9. The new code is structured so that OUTPUT-3 can plug in a Table renderer without modifying any code OUTPUT-2 wrote (i.e., the renderer dispatch is extensible — new format = new module, no changes to existing)

## Completion

Implementor adds new flags to the CLI parser, factors the renderer dispatch in `Main.hs` into a small typeclass or sum, implements the `ShapedJson` renderer in a new `src/Synapse/Output/Filter.hs` (or similar), commits Hspec/Tasty tests covering each flag's behavior plus error paths, runs `cabal test`, and pastes (a) one concrete example of the temp-file workaround being replaced and (b) the green test output in the commit message. Flips status to Complete.
