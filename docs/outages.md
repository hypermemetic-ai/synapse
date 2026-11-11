# Outages and the connection holder

## Reconnecting

synapse rides out a service that goes away, a restart or a network blip,
for ten minutes unless told otherwise.

- **A lost connection is noticed**: the server closing it, a reset, or
  silence. While a call is open, synapse pings after 15 s with nothing heard
  and counts the connection lost when 10 s more pass without an answer.
  `PLEXUS_KEEPALIVE=IDLE[,TIMEOUT]` sets both in seconds, and `0` turns the
  ping off.
- **It retries for the window**: 0.5 s between tries, doubling, to 15 s.
  `--reconnect-for DURATION` sets the window (`10m`, `90s`, `1h`; `0` is
  never), else `SYNAPSE_RECONNECT_FOR`. A service that is down when synapse
  starts is waited for the same way. Progress goes to stderr only:

  ```
  synapse: connection to 127.0.0.1:4444 lost (the connection was closed) — reconnecting for up to 10m
  synapse: retrying 127.0.0.1:4444 in 4s (4s of 10m)
  synapse: reconnected to 127.0.0.1:4444 after 8s
  ```

- **A read is sent again. A write is not.** A method is a read when its
  schema's `group` is `read`, and a write when it is `write`. With no group,
  it is a read when its name is one of a read's: `get`, `list`, `find`,
  `search`, `show`, `watch`, `health`, `schema` and the like. A write whose
  request went out before the connection was lost may have happened, so
  synapse says so and exits 75. A write that never left is sent once the
  service is back.
- **A stream resumes.** A streaming read is subscribed again with the same
  params. A method that takes `since` gets `since` set to the unix time the
  call began, unless the caller gave one, so what was said during the outage
  is still delivered. Under `-j` a line in the stream on stdout marks the
  resume:

  ```json
  {"type":"reconnected","outage_secs":8,"attempts":4,"since":1792204276}
  ```

  `since` is there only when the resumed call carried it. Without `-j` the
  resume is said on stderr.
- **After the window** synapse exits 69 with the reason.
- **A session the restart ended** is made again from its key, and the call
  goes on ([Signing in](signing-in.md)).

| exit | meaning |
| -- | -- |
| 69 | the service stayed unreachable for the whole window |
| 75 | a write went out and its answer never came: it may have happened |

## The connection holder

A call costs one round trip and the server's time, because a connection is
held between runs.

The first call starts a holder, `synapse _hold`, a process of synapse's own.
It keeps signed-in WebSockets open and listens on a unix socket,
`$XDG_RUNTIME_DIR/synapse/holder-6.sock` (0600, in a 0700 directory). Later
runs hand it their requests and read the replies back.

- **One holder serves all of a user's calls.** Inside it there is one
  connection per address, agent and credential. A call as one agent never
  uses another agent's connection.
- **A connection whose server is gone is let go at once.** The next call
  dials again. Riding out an outage stays the run's work, by the rules
  above.
- **Idle, closed.** A connection idle for `--hold-for` is closed. The holder
  exits once it has held nothing, with no call, for `--hold-for`.
  `--hold-for DURATION` is ten minutes unless said, else
  `SYNAPSE_HOLD_FOR`.
- **A cap**: 256 connections. `SYNAPSE_HOLD_MAX` sets it when the holder
  starts, and `0` is no cap. Past it the connection idle longest is closed
  first. One carrying a call never is.
- **`--no-hold`** (or `SYNAPSE_NO_HOLD=1`) skips the holder: the run dials
  one connection of its own, for all its requests, and closes it.
- **A call that brings its own credential** (`-t`, `--token-file`,
  `--cookie`, `--header`, `PLEXUS_SESSION`) never uses the holder.
- **`synapse _hold status`** prints what the holder has, as JSON: its pid,
  version and protocol, the cap, and each connection's address, agent, calls
  under way and seconds idle. It never prints a credential. It exits 1 when
  no holder answers.
- **The holder does not carry the starting run's secrets**: it is started
  without that run's `PLEXUS_KEY`, `PLEXUS_SESSION`, `SYNAPSE_TOKEN` and
  `PLEXUS_KEEPALIVE`.

The socket's name carries the holder's protocol number, 6. A synapse with
another protocol number has a holder of its own beside this one.

## The schema cache

synapse keeps what a call needs from a service in
`~/.cache/synapse/schemas-4/` (under `$XDG_CACHE_HOME` when it is set):
where the service is, its `_info` as the agent was answered, each
activation's schema, and a plan for each method called.

A call to a method with a plan is built from the plan and sent, with no
probe and no schema fetch. The probe and the fetch run only when that fails
in a way the schema explains: a method or a flag the cache does not know, or
the server refusing what the cache said. Those refusals come before the
method runs, so the call is then sent again from the server's schema.

Listings, help pages, `-s`, `-i` and `_find` always read the server. So does
any call with `--no-cache`. A call that brings its own credential has no
cache.
