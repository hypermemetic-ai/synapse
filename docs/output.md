# Output

A call prints each item of the answer as it arrives. The options on this
page are synapse's own, so they go before the service name.

## The rendered answer

Without an option, an item is rendered through its template when one
exists, else printed as its fields.

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

## The wire's JSON: `-j`

`-j` (`--json`) prints every stream item as the service sent it, one JSON
object a line: the data items, then `done`.

<!-- plexus-rpc: examples/echo/out/json.txt -->
```console
$ synapse -j myservice echo count --to 3
{"content":{"n":1,"type":"tick"},"content_type":"echo.count","metadata":{"plexus_hash":"dc0f4a3b22e4cae7","provenance":["echo"],"timestamp":0},"type":"data"}
{"content":{"n":2,"type":"tick"},"content_type":"echo.count","metadata":{"plexus_hash":"dc0f4a3b22e4cae7","provenance":["echo"],"timestamp":0},"type":"data"}
{"content":{"n":3,"type":"tick"},"content_type":"echo.count","metadata":{"plexus_hash":"dc0f4a3b22e4cae7","provenance":["echo"],"timestamp":0},"type":"data"}
{"metadata":{"plexus_hash":"dc0f4a3b22e4cae7","provenance":["echo"],"timestamp":0},"type":"done"}
```

## The content alone: `--raw`

`--raw` prints each data item's `content` as one line of JSON, with no
template.

## The answer in a file: `-o FILE`

`-o FILE` (`--output FILE`) writes what the call prints to FILE instead of
stdout, in the same form: `-j` lines with `-j`, the rendered answer without.

```console
$ synapse -o /tmp/count.txt myservice echo count --to 3
$ synapse -o /tmp/count.json -j myservice echo count --to 3
```

- The answer is written to a temporary file beside FILE and renamed over
  FILE when the call ends with 0. A reader never sees half an answer. FILE
  is 0600.
- A call that ends with any other code leaves no FILE, and removes an older
  FILE. The exit code is the call's own.
- A stream has no end to wait for: its items go to FILE as each comes. A
  stream that fails keeps what it wrote.
- Under `-j`, an error or a refusal goes to stderr, not FILE.
- `-o -` is stdout.
- A FILE that cannot be made is said on stderr and the run exits 73 before
  anything is sent. A write to it that fails later ends the call, and the
  run exits 74.
- A SIGTERM or SIGHUP ends the run as a failed one (143, 129): no FILE and
  no temporary file. A SIGKILL can leave the temporary file behind.

## A closed stdout

When the reader of synapse's stdout goes away (`… | head -1`), synapse stops
at once and exits 141. It retries nothing and sends nothing again. A stream
notices only when it next prints. A run that is ending with a code of its
own keeps that code.

## Templates

A template is a Mustache file for one method's items. synapse looks for it
in this order, first under `.substrate/templates/` in the current directory,
then under `~/.config/synapse/templates/`:

1. `<namespace>/<method>.mustache`
2. `<namespace>/default.mustache`
3. `default.mustache`

`-g` (`--generate-templates`) writes a template for each method of the named
service, or of the activation named after it, into
`~/.config/synapse/templates/`.

`synapse _self template` manages them:

| subcommand | what it does |
| -- | -- |
| `list [pattern]` | lists the templates |
| `show <method>` | prints a template |
| `generate [pattern]` | writes templates from the schema |
| `delete <pattern>` | deletes the templates that match |
| `reload` | clears the template cache |

## Exit codes

| exit | meaning |
| -- | -- |
| 0 | the call was answered |
| 1 | the call failed: an error item, a name that is not there, a parameter that does not parse |
| 2 | `--url`, `--cacert`, `--reconnect-for` or `--hold-for` was given a value synapse cannot use |
| 65 | `-i` met a schema it cannot type, or an input stream was refused ([The IR](ir.md), [A stream in](input-streams.md)) |
| 69 | the service stayed unreachable for the whole reconnect window |
| 73 | `-o FILE` could not be made: nothing was sent |
| 74 | a write to `-o`'s FILE failed |
| 75 | a write went out and its answer never came: it may have happened |
| 77 | the service refused the call |
| 141 | stdout's reader closed the pipe |

69 and 75 are in [Outages](outages.md).
