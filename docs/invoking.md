# Invoking

```
synapse [OPTIONS] <service> <path...> [--param value ...]
```

A command line has three parts, in this order.

1. **synapse's own options** (`-P`, `-j`, `-o`, `--as`, …). They go before
   the service name. `synapse --help` lists them.
2. **The service name and the path**: activation names, then a method.
3. **The method's parameters**, as flags.

Everything after the service name belongs to the service. `--help` there is
the help of the activation or method named so far.

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

## The path

Each segment is a name the schema offers at that point: a child activation
or a method. A path that ends on an activation prints its listing. A path
that ends on a method calls it.

- A hyphen in a segment or a flag name is read as an underscore:
  `--blocked-by` is the parameter `blocked_by`. A value is never changed.
- Where a schema has a dynamic child slot, the next segment is one of its
  keys, and synapse asks the service for that key's schema.
- After a method that is a peer slot, the next segment is a peer's name, and
  the path goes on into that peer's activations.
- A name the service does not have is `Command not found`, with what is
  available at that point.

A method with required parameters, called with no parameters at all, prints
its help page and sends nothing.

## Flags

synapse reads each flag by its parameter's type.

| typed | read as |
| -- | -- |
| `--flag VALUE` | the next argument is the value, whatever it begins with: `---`, `--force`, `-5`, `--`, or nothing (`''`) |
| `--flag=VALUE` | everything after the first `=`, which may be empty or contain `=` |
| `--flag @FILE` | the file's contents, UTF-8, exactly as they are |
| `--flag -` | standard input, read once however many values ask for it |
| `--flag @@text` | the text `@text`: one `@` is dropped |
| `--flag` (a boolean) | `true`; it takes the next argument only when that is `true`, `false`, `1` or `0` |
| `--flag=false` (a boolean) | `false` |
| `--a.b VALUE` | the field `b` of the object parameter `a` |
| `--no-such x` (no such parameter) | `Unknown parameter: --no-such`, with the nearest names; nothing is sent |

- An argument that no flag takes is an error, and nothing is sent.
- `--help` is always help, even right after a flag. To send the text
  `--help`, write `--flag=--help`.
- A value from a file or stdin is carried by no argument, so it can be as
  large as the service accepts. A shell argument holds 128 KB at most.
- `~/` and a relative path after `@` resolve as the shell's would. A file
  that cannot be read is a refusal that names the flag and the path.

## Path parameters

A parameter named `path`, `working_dir`, `output_dir`, `file_path`, `dir`,
`directory` or `workdir` is a filesystem path. synapse expands `~`, makes a
relative path absolute, and substitutes `$VAR` in it. A required string
parameter of that name that is left out, on a call that gives other flags,
is sent as the current directory. No other value is touched: `--flag 'costs $HOME'` sends `costs $HOME`.

## Lists

A list parameter takes its elements one flag at a time, several to a flag,
or both. Repeated flags append in order: `--flag a,b --flag c` is `a` `b`
`c`.

When the elements are scalars (strings, integers, numbers, booleans, or a
named type that is one of those), each flag value is one comma-separated
record, with double quotes for an element that contains a comma.

| flag value as synapse receives it | elements |
| -- | -- |
| `a,b,c` | `a` `b` `c` |
| `a` | `a` |
| `"a,b,c,d","e,f,g"` | `a,b,c,d` and `e,f,g` |
| `"say ""hi""",b` | `say "hi"` and `b`: `""` inside quotes is one quote |
| `a, b ,c` | `a` `b` `c`: space around an unquoted element is dropped |
| `a,,b,` | `a` `b`: an empty unquoted element is dropped |
| `"",a` | the empty string and `a` |
| `"a,b` or `"a"x,b` | an error naming the parameter and the value; nothing is sent |

Elements are then typed: `--flag 1,2,3` sends `[1,2,3]` for a list of
integers. A list of objects, of lists, or of untyped values is not split:
each flag value is one element. A comma in a parameter that is not a list is
a comma. A method's `--help` marks each list parameter with the forms it
takes.

The shell removes double quotes before synapse runs, so an element that
contains a comma needs its quotes protected by single quotes.

| typed | what synapse receives |
| -- | -- |
| `--flag a,b,c,e` | `a,b,c,e`: four elements |
| `--flag "a,b,c,d","e,f,g"` | `a,b,c,d,e,f,g`: seven elements |
| `--flag '"a,b,c,d","e,f,g"'` | `"a,b,c,d","e,f,g"`: two elements |

## The whole params object: `-p`

`-p JSON` (`--params`) gives the method's parameters as one JSON object,
sent as written. `-p @params.json` reads it from a file and `-p -` from
stdin. `-p` is one of synapse's own options, so it goes before the service
name.

## Without sending: `-n`

`-n` (`--dry-run`) prints the JSON-RPC request the call would send, and
sends nothing.

## A raw request: `-r`

`-r JSON` (`--rpc`) sends a JSON-RPC request as written, without reading the
schema, and prints what comes back.

## Deprecation

A call to a method or an activation its schema marks deprecated prints a
warning on stderr. `--no-deprecation-warnings` leaves it out.

## Logging

`--log-level LEVEL` prints synapse's own log on stderr at `error`, `warn`,
`info` or `debug`. Without it only errors are logged.
