# Discovery

Everything synapse offers for a service comes from the service's schema,
read when it is asked for. A method added to the service is a command the
next time synapse is run.

## Listings

A path that ends on an activation prints its description, its methods and
its child activations, one line each.

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

`synapse myservice` alone lists the service's activations the same way.

## A method's page: `--help`

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

The page has the method's description, its usage, each option with its type
and description, and the schema's examples as command lines. A method with
required parameters prints this page when it is called with none.

## Search: `_find`

```
synapse [OPTIONS] _find BACKEND [ACTIVATION...] WORDS... [--limit N] [--methods | --params]
```

`_find` searches the service's methods and parameters by name and
description.

```console
$ synapse _find myservice count
$ synapse _find myservice echo tick
```

- A hit matches **all** the words. Best first: an exact name, then a prefix
  of a name or of one of its `snake_case` segments, then an abbreviation
  (`setpar` finds `set_parent`) or a typo (one edit for 4 to 7 letters, two
  beyond), then a word of a description.
- Hits are grouped `METHODS`, `PARAMETERS` and `MENTIONED IN`, the last for
  a hit found only in a description. A parameter line has its method, its
  flag as the method's page shows it, and its description.
- The header says what a typo or an abbreviation was taken as. On a terminal
  the matched characters are bold. With `NO_COLOR` set, or in a pipe, the
  text is plain.
- Activation names before the words narrow the search.
- `--limit N` is the most hits printed (default 20). `--methods` and
  `--params` keep one group.
- With nothing found it exits 1.
- `-j`, before `_find`, prints one JSON object per hit: `kind` (`method` or
  `param`), `path`, `description`, `group`, `score`, `matched`, and for a
  parameter its `param`, `type`, `required` and `values`.
- On a service with sign-in it searches the caller's view of the schema.
- The catalogue is kept in `~/.cache/synapse/ir` under the root schema's
  hash, so a search costs one schema call while nothing has changed.
  `--no-cache` builds it again from the service.

## The schema: `-s`

`-s` (`--schema`) prints the raw schema JSON of the activation or method the
path names.

```console
$ synapse -s myservice echo
$ synapse -s myservice echo count
```

## The IR: `-i`

`-i` (`--emit-ir`) prints the intermediate representation a client generator
reads. See [The IR](ir.md).
