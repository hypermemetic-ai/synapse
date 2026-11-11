# Developing

## Building

The package is `plexus-synapse`. It builds with GHC 9.8.4 and cabal, and
takes [plexus-protocol](https://github.com/hypermemetic-ai/plexus-protocol)
0.8 from a checkout beside this one, named in `cabal.project.local`:

```
packages: ../plexus-protocol/
```

```sh
cabal build exe:synapse
cabal install exe:synapse
```

## The tests

```sh
cabal test                    # every suite that needs nothing running
cabal test parse-test         # one suite
cabal test -f live-tests      # also cli-test, websocket-raw-test and ir-test
cabal bench ir-bench          # the IR builder's timing, with no network
```

- A default `cabal test` needs no service. The suites that call one run
  against a mock service inside the test.
- `-f live-tests` adds the suites that need a service and a registry on
  `127.0.0.1:4444`.
- `-f build-keychain-tests` adds the keychain suite, which touches the real
  macOS keychain.
- `test-suite naming-gate` runs `scripts/naming-gate.sh`. The gate fails
  on the retired word for a service.
  `scripts/naming-gate.sh -c` prints the count of offending lines.

## Checking a service: `_self`

These run from synapse against a service's address.

| command | what it does |
| -- | -- |
| `synapse _self scan` | asks ports 4440 to 4459 for the service on each |
| `synapse _self debug <host> <port> <service>` | tests the connection layer by layer: TCP, HTTP, the WebSocket handshake, the RPC |
| `synapse _self validate <host> <port> <service>` | runs the protocol checks against the service's `_debug` methods. It exits 0 when all pass, 1 otherwise |
| `synapse [-H HOST] [-P PORT] _self test <method> [--param value ...]` | calls one method and checks its stream. `--allow-unknown` passes unknown parameters through with a warning. `--raw JSON` sends the params as written |

## The source

| path | what is there |
| -- | -- |
| `app/Main.hs` | the executable: options, dispatch, printing |
| `src/Synapse/CLI/` | flags to typed JSON, help pages, values from files |
| `src/Synapse/Algebra/` | navigation and walking of the schema tree |
| `src/Synapse/IR/` | the IR: types, reader, builder, cache |
| `src/Synapse/Identity/` | keys, proofs, sessions |
| `src/Synapse/Self/` | stored defaults and the `_self` commands |
| `src/Synapse/Hold.hs` | the connection holder |
| `src/Synapse/Reconnect.hs` | outages |
| `src/Synapse/Output.hs` | `-o FILE` |
| `src/Synapse/Input.hs` | a stream in |
| `src/Synapse/Find.hs` | `_find` |
| `src/Synapse/Endpoint.hs` | `--url` and the address |
| `synapse-types/` | a second package in the same project |
