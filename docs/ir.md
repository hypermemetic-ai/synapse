# The IR

`-i` (`--emit-ir`) prints a service's intermediate representation: one JSON
document of its methods and types, which a client generator reads.

```console
$ synapse -o myservice.ir.json -i myservice
$ synapse -o echo.ir.json -i myservice echo
```

A path after the service name keeps the IR to that activation.
[synapse-cc](https://github.com/hypermemetic-ai/synapse-cc) builds its IR
with synapse as a library, and turns it into a client.

## What it holds

The IR's version, `irVersion`, is `3.1`.

- `irMethods` maps each method's dotted path to its definition: its
  parameters (`mdParams`), the type of its items (`mdReturns`), its error
  type when it has one (`mdError`), its progress type when it reports
  progress (`mdProgress`), and whether it answers one item or many
  (`mdCardinality`).
- `irTypes` maps each named type to its definition: a struct, an enum, a
  string enum, or an alias.
- A parameter the service fills in itself is not in `mdParams`. It is in
  `mdInjected`.
- `mdInput` is the item type of a method's input half
  ([A stream in](input-streams.md)).
- `irMetadata` holds the version, a timestamp and the tools of the
  generation chain. `--generator-info TOOL:VERSION` adds a tool to it, and
  repeats.

Nothing is inferred. `mdCardinality` is the schema's `cardinality`. One
title in one namespace is one type.

## Refusals

Every schema is read whole or refused. Nothing is dropped silently.

`irRefusals` lists each site the IR cannot type: `rfSite` (where),
`rfKeyword` (the keyword it could not read), `rfVariant` (for a union, the
first member it could not read) and `rfDetail` (why). It is left out when
empty. Where a refused site stood, the IR says `RefUnknown`.

With refusals, `synapse -i` still prints the IR on stdout, names each site
on stderr, and exits 65. No generator may run on that IR. Calls, help, flags
and `_find` ignore refusals.

## The cache

The IR that `_find` searches is kept in `~/.cache/synapse/ir` under the
root schema's hash. `--no-cache` builds it again from the service. A cached
IR of another version is discarded and built again.

## In the source

- `src/Synapse/IR/Types.hs`: the types and `currentIrVersion`.
- `src/Synapse/IR/Read.hs`: the reader. Each schema position is read whole
  or refused.
- `src/Synapse/IR/Builder.hs`: the builder, from a walk of the service or
  from one schema.
- `test/IR3Spec.hs` and `test/IRCoverageSpec.hs`: the tests that hold the IR
  to its fixtures.
