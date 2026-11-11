# Finding a service

synapse dials one address, the connection point. It is `127.0.0.1:4444`
unless an option says otherwise. The options go before the service name.

| option | meaning |
| -- | -- |
| `-H HOST`, `--host` | the host (default `127.0.0.1`) |
| `-P PORT`, `--port` | the port (default `4444`; `443` with `--tls`) |
| `--url URL` | the whole address, `ws://HOST[:PORT][/PATH]` or `wss://HOST[:PORT][/PATH]`. The port defaults to the scheme's (80, 443) and the path to `/`. It stands alone: not with `-H`, `-P`, `--tls` or `--path` |
| `--tls` | dial `wss://` at `-H` and `-P` |
| `--path PATH` | the socket's path at `-H` and `-P` (default `/`) |
| `--cacert FILE` | with `wss://`, also trust the CA certificates in this PEM file. It repeats |

## By name

The service at the connection point answers for itself by name:

```console
$ synapse myservice echo count --to 1
$ synapse -P 4444 myservice echo count --to 1
```

A service that runs a registry activation also answers for the services
registered with it. synapse asks it where the named service is and dials
that address.

- `synapse` with no service name prints synapse's options and the services
  the connection point knows, each with its address and `[OK]` or
  `[UNREACHABLE]`.
- A call with `-P` to a port other than 4444 registers the service found
  there with the registry at port 4444 of the same host, when one answers.
  An address behind TLS or on a path is not registered.
- The name is checked. When the address answers as another service, synapse
  prints `Backend mismatch` with both names and exits 1.
- `synapse _self scan` asks every port from 4440 to 4459 on the host for its
  service's name and prints those that answer.

## A service behind TLS: `--url wss://…`

A hosted service sits behind a proxy that ends TLS and routes its socket at
a path. `--url` names the whole address:

```console
$ synapse --url wss://myservice.example/ws myservice echo count --to 3
$ synapse -H myservice.example --tls --path /ws myservice echo count --to 3
```

The two lines dial the same address.

- **The certificate is always verified**: the chain against the system's
  trust store, and the name against the host, which is also sent as SNI.
  `SYSTEM_CERTIFICATE_PATH` points synapse at another directory or file. A
  failed check ends the handshake before any of the request is sent, a
  credential included. No option skips the check. For a local CA, pass its
  root with `--cacert`.
- **Everything uses the same address**: the probe, calls, streams and their
  reconnects, `-s`, `-i`, `_find`, `_login`, `_logout`, and the session
  every later call carries.
- **`http://` and `https://` are refused**, with the `ws://` or `wss://`
  address they mean. A query, a fragment and `user:password@` are refused
  too.
- **Sessions are kept per address.** A session made with `--url` is found by
  a call with the same `--url`. See [Signing in](signing-in.md).

When the probe of a `wss://` address fails, synapse says why: the TLS
handshake and what it checks, the HTTP status the upgrade was answered with,
or that nothing answered.
