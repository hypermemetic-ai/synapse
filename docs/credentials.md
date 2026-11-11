# Credentials

A session from `_login` is one way a call is known to a service
([Signing in](signing-in.md)). This page is the other: a token, cookies and
headers sent on the WebSocket upgrade, from the command line or from a
service's stored defaults.

## On one call

These are synapse's own options, so they go before the service name.

| option | what is sent |
| -- | -- |
| `-t JWT`, `--token JWT` | `Cookie: access_token=<jwt>` |
| `--token-file PATH` | the same, the JWT read from the file |
| `--cookie KEY=VALUE` | one more cookie. It repeats |
| `--header KEY=VALUE` | one more HTTP header. It repeats |

`SYNAPSE_COOKIE_<NAME>` and `SYNAPSE_HEADER_<NAME>` in the environment add a
cookie or a header named by the lowercased `<NAME>`.

An option applies to its one call and writes nothing to disk. `--token` wins
over `--token-file`. Each wins, key by key, over the stored defaults.

A call that brings a credential this way uses no connection holder and no
schema cache ([Outages](outages.md)).

## Stored defaults

Each service has a file, `~/.plexus/<service>/defaults.json`, of default
cookies and headers. A value is a reference, resolved when a call is made.

```json
{
  "version": 1,
  "defaults": {
    "cookies": { "access_token": "literal:eyJhbGc..." },
    "headers": { "X-API-Key": "env://MY_API_KEY" }
  },
  "scopes": {}
}
```

| scheme | example | meaning |
| -- | -- | -- |
| `literal:` | `literal:eyJ...` | the text after the colon |
| `env://` | `env://MY_JWT` | the environment variable's value |
| `file://` | `file:///abs/path` | the file's contents, a trailing newline dropped |
| `keychain://` | `keychain://plexus/myservice/access_token` | the OS keychain's item for that service and account. On macOS it is read with the `security` command. On Linux and Windows the resolver answers that it is not implemented |

A reference that cannot be resolved ends the run with the reason, and
nothing is sent.

A file `~/.plexus/tokens/<service>` holding a raw JWT is read once: its
token becomes `cookies.access_token` in `defaults.json`, and the file is
removed.

### `_self <service>`

```
synapse _self <service> <verb> ...
```

| verb | what it does |
| -- | -- |
| `show [--json]` | the defaults with their resolved values. A JWT is decoded: its claims and how long until it expires |
| `set KIND NAME VALUE-OR-URI` | sets a cookie or a header. KIND is `cookie` or `header`. A value with no scheme is kept as `literal:` |
| `set-from-stdin KIND NAME` | sets it from stdin, always as `literal:` |
| `set-secret KIND NAME` | puts stdin in the keychain and keeps a `keychain://` reference |
| `unset KIND NAME [--yes]` | removes the entry, and the keychain item of a `keychain://` reference unless `--no-delete-from-keychain` |
| `resolve KIND NAME` | resolves one value and prints it |
| `upgrade-to-keychain [KIND [NAME]]` | moves `literal:` references into the keychain |
| `clear [--yes]` | deletes this service's `defaults.json` |
| `import-token PATH` | keeps a JWT from a file (`-` is stdin) as `access_token`. `--to-keychain` puts it in the keychain |

`synapse _self <service> <verb> --help` prints a verb's options.

### What `keychain://` protects

On macOS the keychain is reached through `/usr/bin/security`. An item it
makes can be read by any process that runs as the same user and knows the
service and account. So `keychain://` keeps a secret out of a plaintext file
and away from other users of the machine. It does not keep it from another
process of the same user.
