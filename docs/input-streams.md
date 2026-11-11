# A stream in

A method with an input half takes a stream in beside its stream out. Its
schema has `input` beside `params`. Standard input or a file is that stream:

```
synapse <service> <path...> [--param value ...] --input -
synapse <service> <path...> [--param value ...] --input @FILE
```

The echo service of the examples has no such method, so this page shows no
output.

## What an item is

The item's schema decides.

| the item's type | how the input is read |
| -- | -- |
| a string | one line is one item |
| bytes (`"x-plexus-binary": true`) | the bytes as they come, up to `--chunk N` a frame, each a WebSocket binary frame. N is 4096 unless said, and 16 MiB at most |
| anything else | NDJSON, one JSON value a line |

A line that is not a JSON value is named by its number. No frame carries it,
the call is ended, and synapse exits 65.

## Under credit

Items are read only as the service grants credit. With none, synapse reads
nothing more, so the pipe in front of it is the buffer and its memory stays
flat. What is ready goes out together, up to 256 items a frame. At the end
of the input synapse sends the end of the stream, and the answer prints as
any call's does.

## Never sent twice

A connection lost during the call is said on stderr, with what was sent and
what the service acknowledged, and the run exits 69. Nothing is sent again.

| exit | meaning |
| -- | -- |
| 65 | the service closed the input as `invalid_input`, `out_of_sequence` or `too_much_input`, or a line was not JSON |
| 69 | the connection was lost during the call |
| 70 | the service closed the input as `past_credit` |
| 74 | reading the input failed |
| 77 | the input was closed for a sign-in that was revoked or has ended |

## Its options

- `--progress` prints what was sent, acknowledged and granted on stderr once a
  second.
- `-n` prints the request and the first input frame, and sends nothing.
- A method's `--help` and `_find` say that it takes a stream in.
- When the method has a parameter named `input`, that parameter keeps
  `--input`, and the stream is `--input-stream`. `--chunk` and `--progress`
  become `--input-chunk` and `--input-progress` the same way.
- A value read from stdin (`--flag -`) and a stream from stdin cannot share
  one call. Give the value as `@FILE`.
