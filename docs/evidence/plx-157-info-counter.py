#!/usr/bin/env python3
"""PLX-157 — count `_info` round trips a single synapse invocation puts on the wire.

Inventory item 5 (PLX-124) recorded "THREE `_info` round trips per CLI
invocation, on every path, even with -H/-P given", measured through a counting
proxy. That proxy was not kept, so this is it, written to be re-runnable.

A raw TCP tap cannot do this: client-to-server WebSocket frames are XOR-masked,
so `_info` never appears literally in the client's bytes. This is a real
WebSocket proxy that unmasks, counts, and forwards verbatim.

Usage:
    python3 plx-157-info-counter.py --listen 4488 --upstream ws://127.0.0.1:4444

Then point synapse at the proxy:
    synapse -H 127.0.0.1 -P 4488 substrate health check

On SIGINT it prints the per-method tally.
"""

import argparse
import asyncio
import json
import signal
import sys
from collections import Counter

import websockets

TALLY = Counter()
CONNECTIONS = 0


async def pump(src, dst, count_methods):
    try:
        async for message in src:
            if count_methods and isinstance(message, str):
                try:
                    frame = json.loads(message)
                except ValueError:
                    frame = None
                if isinstance(frame, dict) and "method" in frame:
                    TALLY[frame["method"]] += 1
            await dst.send(message)
    except websockets.ConnectionClosed:
        pass
    finally:
        try:
            await dst.close()
        except Exception:
            pass


async def handler(client, upstream_url):
    global CONNECTIONS
    CONNECTIONS += 1
    try:
        async with websockets.connect(upstream_url, max_size=None) as upstream:
            await asyncio.gather(
                pump(client, upstream, count_methods=True),
                pump(upstream, client, count_methods=False),
            )
    except Exception as exc:  # upstream down, bad handshake, ...
        print(f"[proxy] upstream error: {exc}", file=sys.stderr)


def report():
    print("\n--- PLX-157 `_info` counter ---")
    print(f"websocket connections opened by the client: {CONNECTIONS}")
    info = TALLY.get("_info", 0)
    print(f"_info round trips: {info}")
    others = sorted((m, n) for m, n in TALLY.items() if m != "_info")
    if others:
        print("other methods:")
        for method, n in others:
            print(f"  {method}: {n}")
    print("--- end ---")


async def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--listen", type=int, default=4488)
    ap.add_argument("--upstream", default="ws://127.0.0.1:4444")
    ap.add_argument("--seconds", type=float, default=None,
                    help="run for N seconds then report and exit")
    args = ap.parse_args()

    stop = asyncio.get_running_loop().create_future()
    try:
        signal.signal(signal.SIGINT, lambda *_: stop.cancel())
    except ValueError:
        pass

    async with websockets.serve(
        lambda ws: handler(ws, args.upstream),
        "127.0.0.1",
        args.listen,
        max_size=None,
    ):
        print(f"[proxy] 127.0.0.1:{args.listen} -> {args.upstream}", file=sys.stderr)
        if args.seconds is not None:
            await asyncio.sleep(args.seconds)
        else:
            try:
                await stop
            except asyncio.CancelledError:
                pass
    report()


if __name__ == "__main__":
    asyncio.run(main())
