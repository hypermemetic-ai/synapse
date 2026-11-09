# Changelog for plexus-synapse

## 4.1.0

bidir removed (ASK-1, ASK-3); plexus 0.10's schemas read (NEST-10). Breaking
under the PVP.

### Breaking

- **bidir is gone.** `--bidir-mode`, `--bidir-cmd`, `--bidir-respond`, the
  `respond` subcommand and `Synapse.Bidir` are removed, with spec/08's
  `request` item and `{ns}.respond` (plexus-protocol 0.7). Nothing replaces
  them: synapse has no in-call or protocol-wide confirmation, and an item a
  service names `needs_input` is printed like any other. IR 3.1 is 3.0
  without `mdBidirType` / `mdBidirResponseType` / `mdBidirResponseSchema`
  (an older IR's are read past). Requires `plexus-protocol >= 0.7`.
- **Holder protocol 6.** The connection holder's socket is
  `$XDG_RUNTIME_DIR/synapse/holder-6.sock` (4 carried the input half; 5 was
  VISION-7's unmerged question routing, never reused; 6 relays plexus-protocol
  0.7's items, with no `request` item, and routes nothing). A run never uses
  or replaces a holder of another protocol (3.x's `holder-2.sock`, 4.0.0's
  `holder-4.sock`), nor the reverse; they run side by side.

### Changed

- plexus-core 0.10's `MethodRole` is read whole: a peer slot is a
  `static_child` marked `peer` with its `interface` (NEST-7), and a
  `dynamic_child` carries its slot's type, `child` (NEST-5).

## 4.0.0

Everything since 3.17.3 (3.17.4 was never published; its change is listed here).

### Breaking

- **IR 3.0, on the plexus 0.7 schema.** Requires `plexus-protocol >= 0.6`.
  A method's `returns`, `error`, `progress` and `cardinality` are read from
  its schema, never guessed; servers on the published 0.7.0 line are still
  read, cardinality taken from the method's own `streaming` flag. A title
  collision (two types under one title) refuses that activation by name
  (`-i` exits 65 and says what was left out) instead of failing the whole
  walk.
- **Holder protocol 4.** The connection holder's socket is
  `$XDG_RUNTIME_DIR/synapse/holder-4.sock`. A 4.0 run never uses or
  replaces a 3.x holder (`holder-2.sock`), nor the reverse; the two run
  side by side.
- **Schema cache moved** to `$XDG_CACHE_HOME/synapse/schemas-4`
  (`~/.cache/synapse/schemas-4`). 3.x's `schemas` directory is never read,
  so a 4.0 run and a rolled-back 3.x never share an entry. The first 4.0
  run against a service fetches its schemas again.
- **A failed stream exits non-zero.** An error item, or a data item whose
  content has `type: error`, sets exit status 1. Services report their
  failures in band; scripts that relied on exit 0 after such a stream will
  now see 1.
- **One holder per user**, not one per address and agent. The agent rides
  in each request; a connection whose server is gone is let go at once; at
  most 256 connections are held, the longest idle closed first; the newest
  synapse takes over holding.

### Added

- **Streaming input (`Input<T>`, plexus-wire spec/10).** `--input -` (stdin)
  or `--input @FILE` sends a method's input half: lines, NDJSON items, or
  raw bytes as binary frames (`--chunk N`, 4096 by default, at most
  16 MiB), sent as the service grants credit, ended at EOF, carried through
  the holder. A disconnect is reported with sent / acked / unacked counts
  and never resent (exit 69); typed closes exit 70, 65 or 77, a failed read
  74. `--progress` prints sent / acked / credit on stderr once a second. A
  flag whose name a method parameter already has (`input`, `chunk`,
  `progress`) becomes `--input-stream`, `--input-chunk` or
  `--input-progress`. The IR reads a bytes input as string/binary.
- **The service's own `ir` first.** When a service's root schema lists an
  `ir` method, synapse asks it for the whole IR in one call instead of one
  schema call per activation; otherwise it walks the raw schemas as before.
  A 0.7.0 service spends no call on it.
- The server's own error message is shown and kept; an unknown error kind
  is rendered `kind: message`, and a missing grant as the Rust side writes
  it. Re-sign-in and cache repair work against both 0.8 and 0.7.0 servers.

### Changed

- `cabal.project` no longer lists `../plexus-protocol`; it comes from
  Hackage.
- The suites that need a live service (`cli-test`, `websocket-raw-test`,
  `ir-test`, `bidir-integration-test`) are opt-in behind the manual flag
  `live-tests` (`cabal test -f live-tests`), so a default `cabal test`
  needs nothing running.
