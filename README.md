# synapse

The command line of every Plexus RPC service: it reads a service's own
schema and offers its activations, methods and flags.

## Most people want plexus-rpc

[plexus-rpc](https://github.com/hypermemetic-ai/plexus-rpc) is the front
door to Plexus RPC: its README builds a service in a few lines of Rust and
calls it. The service in every example below is that one: `myservice`, with
an `echo` activation, on `ws://127.0.0.1:4444`.

## Install

The package is `plexus-synapse` and its executable is `synapse`. It builds
with GHC 9.8.4 and cabal. Version 4.1.1 builds from this repository, with
[plexus-protocol](https://github.com/hypermemetic-ai/plexus-protocol) 0.8
checked out beside it:

```sh
git clone https://github.com/hypermemetic-ai/synapse
git clone https://github.com/hypermemetic-ai/plexus-protocol
cd synapse
echo 'packages: ../plexus-protocol/' > cabal.project.local
cabal update
cabal install exe:synapse -w ghc-9.8.4
synapse --version
```

[Hackage](https://hackage.haskell.org/package/plexus-synapse) holds
`plexus-synapse` 4.1.0.

## An example

Name the service, then an activation, and synapse lists its methods.

<!-- plexus-rpc: examples/echo/out/echo.txt -->
```console
$ synapse myservice echo
echo v0.1.0
  Say things back.

USAGE
  synapse myservice echo <method> [flags]

METHODS
  count     Ticks from 1 up to `to`.
  schema    Get plugin or method schema

Run 'synapse myservice echo <method> --help' for its options.
Run 'synapse --help' for synapse's own options.
```

A method's flags are its parameters.

<!-- plexus-rpc: examples/echo/out/help.txt -->
```console
$ synapse myservice echo count --help
Ticks from 1 up to `to`.

Usage: synapse myservice echo count --to <TO>

Options:
  --to <uint32>  the last tick

A value may be read from a file (--flag @FILE) or stdin (--flag -);
@@text sends a literal @text.

Example:
  synapse myservice echo count --to 1

Run 'synapse --help' for synapse's own options.
```

A call prints each item of the answer as it arrives.

<!-- plexus-rpc: examples/echo/out/count.txt -->
```console
$ synapse myservice echo count --to 3
n: 1
type: tick
n: 2
type: tick
n: 3
type: tick
```

On a service with sign-in, a caller signs in once with a key, and every
later call carries the session.

<!-- plexus-rpc: examples/users/out/login.txt -->
```console
$ synapse _login myservice --key dev --as dev
signed in to myservice at 127.0.0.1:4444 as dev
  principal key:b379e10bed307ff7a5f367ef07433434729d4ab7589cace401cd957da8c73549
  scope     the key's own grants
  until     2026-11-12 03:22:56 UTC
  kept in   ~/.plexus/sessions/127.0.0.1_4444/dev.json
```

<!-- plexus-rpc: examples/users/out/hello.txt -->
```console
$ synapse myservice echo hello
you: key:b379e10bed307ff7a5f367ef07433434729d4ab7589cace401cd957da8c73549
```

Each block above is a file under plexus-rpc's
[`examples/`](https://github.com/hypermemetic-ai/plexus-rpc/tree/main/examples),
which `scripts/examples.sh` there writes again from synapse's own output.

## More

- [Invoking](docs/invoking.md): the order of a command line, flags, lists, values from a file or stdin.
- [Finding a service](docs/finding-a-service.md): the registry, `-H` and `-P`, `--url` and TLS.
- [Signing in](docs/signing-in.md): keys, `_login`, `_logout`, sessions, `--as`.
- [Credentials](docs/credentials.md): `--token`, `--cookie`, `--header` and a service's stored defaults.
- [Output](docs/output.md): `-j`, `--raw`, `-o FILE`, templates, exit codes.
- [Discovery](docs/discovery.md): listings, `--help`, `_find`, `-s`.
- [Outages and the connection holder](docs/outages.md): `--reconnect-for`, `_hold`, `--no-hold`, the schema cache.
- [A stream in](docs/input-streams.md): `--input` from stdin or a file.
- [The IR](docs/ir.md): `-i`, what a client generator reads.
- [Developing](docs/developing.md): building, the tests, the `_self` checks.
- [Changelog](CHANGELOG.md)

MIT.
