# Signing in

A service with sign-in shows an anonymous caller its door: the methods open
to everyone, and how to sign in. A refused call exits 77.

<!-- plexus-rpc: examples/users/out/anonymous.txt -->
```console
$ synapse myservice echo count --to 1
n: 1
type: tick
$ synapse myservice echo hello
myservice v1.0.0
  Sign in to see what you can do here.

auth
  login       Sign in — a person through the browser, an agent with its key
  signup      Create an account, or ask for access
  providers   The ways in this server offers
  echo.count

  synapse _login myservice                          # a person
  synapse _login myservice --key agent-7 --as w-1   # an agent
synapse: sign in to reach echo hello
```

## Keys: `_key`

An agent proves who it is with an Ed25519 key. Keys are local to the
machine.

<!-- plexus-rpc: examples/users/out/key.txt -->
```console
$ synapse _key new dev
synapse: key dev kept in file ~/.plexus/keys/dev.key
key:b379e10bed307ff7a5f367ef07433434729d4ab7589cace401cd957da8c73549
```

| command | what it does |
| -- | -- |
| `synapse _key new [NAME] [--store auto\|file\|keychain]` | makes a key and prints its principal, `key:<hex>`. NAME defaults to `default`. It never overwrites a key |
| `synapse _key show [NAME]` | prints a key's principal |
| `synapse _key list` | every key synapse can find, and where |

A key name is 1 to 64 of `A-Z a-z 0-9 . _ -`. A key is kept in the OS
keychain when one answers, else in `~/.plexus/keys/NAME.key` (0600). A key
is read in this order:

1. `PLEXUS_KEY`, the base64 of the seed
2. the OS keychain
3. `$CREDENTIALS_DIRECTORY/NAME.key`
4. `~/.plexus/keys/NAME.key`

`PLEXUS_KEYCHAIN=off` skips the keychain.

## An agent: `_login --key`

<!-- plexus-rpc: examples/users/out/login.txt -->
```console
$ synapse _login myservice --key dev --as dev
signed in to myservice at 127.0.0.1:4444 as dev
  principal key:b379e10bed307ff7a5f367ef07433434729d4ab7589cace401cd957da8c73549
  scope     the key's own grants
  until     2026-11-12 03:22:56 UTC
  kept in   ~/.plexus/sessions/127.0.0.1_4444/dev.json
```

```
synapse [OPTIONS] _login BACKEND [--key NAME] --as AGENT [--scope ROLE@SCOPE] [--ttl SECS]
```

- `--key NAME` (`-k`) is the key to prove. It is `default` when only `--as`
  is given.
- `--as AGENT` is the agent id to sign in as. `SYNAPSE_AS` names it too.
- `--scope ROLE@SCOPE` is the session's ceiling. It repeats, or takes
  commas. Without it the session has the key's own grants.
- `--ttl SECS` is the session's lifetime in seconds.

The options that say where the service is (`-P`, `--url`, …) go before
`_login`, as for a call.

## A person: `_login`

`synapse [OPTIONS] _login BACKEND`, with no `--key` and no `--as`, signs a
person in through the browser. synapse prints the URL, opens it, and waits
five minutes on a loopback port of `127.0.0.1` for the browser to come back.
`SYNAPSE_NO_BROWSER=1` opens nothing. The session is kept under the agent
name `me`.

## The session a call carries

A sign-in keeps its session, and makes its agent the default for that
address. Every later call to the address carries the session by itself.

<!-- plexus-rpc: examples/users/out/hello.txt -->
```console
$ synapse myservice echo hello
you: key:b379e10bed307ff7a5f367ef07433434729d4ab7589cace401cd957da8c73549
```

A listing in a signed-in caller's view says who the caller is, and each
method's grant.

<!-- plexus-rpc: examples/users/out/echo.txt -->
```console
$ synapse myservice echo
echo v0.1.0
  Say things back.
  you are dev (key:b379…) · admin on *

USAGE
  synapse myservice echo <method> [flags]

METHODS
  count     Ticks from 1 up to `to`.                              echo.count   *
  hello     Says who you are.                                     echo.hello   *
  schema    Get plugin or method schema                           echo.schema

Run 'synapse myservice echo <method> --help' for its options.
Run 'synapse --help' for synapse's own options.
```

- `--as AGENT`, before the service name, picks which agent's session a call
  carries. Else `SYNAPSE_AS`, else the address's default agent.
- When an agent's session has ended, synapse signs in again from the key
  the session was made from, and sends the call. A person's session has no
  key: synapse says to sign in again.
- A call that brings its own credential (`-t`, `--token-file`, an
  `access_token` cookie, an `Authorization` header) carries no stored
  session. See [Credentials](credentials.md).

A session is read in this order:

1. `PLEXUS_SESSION`, the token itself
2. the OS keychain
3. `~/.plexus/sessions/HOST_PORT/AGENT.json` (0600)

A plain `ws://HOST:PORT/` address is keyed `HOST_PORT`. Any other address is
keyed by its URL with the port written, as in
`~/.plexus/sessions/wss_myservice.example_443_ws/AGENT.json`. So a `ws` and
a `wss` service on one host, or two paths, never share a session.

## Signing out: `_logout`

```
synapse [OPTIONS] _logout BACKEND [--as AGENT]
```

It ends the session at the service and forgets it. The agent is `--as`,
else `SYNAPSE_AS`, else the address's default.
