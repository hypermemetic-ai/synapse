# HEADERS-1: Operator-supplied HTTP headers on the WS upgrade

**Status:** Planning
**Date:** 2026-05-01

---

## Problem

Synapse today supports exactly one auth mechanism on the WebSocket upgrade: `-t/--token JWT` which sends `Cookie: access_token=<jwt>`. That's the right shape for plexus's `SessionValidator` middleware (which reads the Cookie header), but it's the *only* shape.

Anything in the operator's environment that isn't "plexus + cookie-based JWT" is unreachable through synapse:

- A backend behind Caddy `basic_auth` (HTTP Basic) — the canonical way to gate something quickly without IdP infrastructure. The platform's PLATFORM-38 Enforcement::BasicAuth deploys exactly this.
- A backend behind Cloudflare Access service tokens (`CF-Access-Client-Id` + `CF-Access-Client-Secret` headers).
- A reverse proxy that requires a Bearer token directly (`Authorization: Bearer <token>`) — common at the edge before plexus runs.
- A service that wants a non-standard header for routing (e.g. `X-Tenant-Id`, `X-Request-Origin`).

**Real, tonight (2026-05-01)**: we deployed `https://whoami.hypermemetic.ai` — a plexus RPC backend gated by Caddy `basic_auth`. Synapse can't talk to it. The workaround is an SSH tunnel that bypasses Caddy entirely. That's a leaky abstraction.

This ticket adds operator-supplied request headers to synapse's WS upgrade so any of the above auth shapes work out of the box.

---

## Required behavior

### A `--header` flag (generic)

```bash
synapse \
  -H whoami.hypermemetic.ai \
  -P 443 \
  --header "Authorization: Basic $(printf 'operator:hunter2' | base64)" \
  plexus-whoami whoami health
```

Accepts `--header NAME: VALUE` (one per flag, repeatable). Each emit a single header on the HTTP request that initiates the WS upgrade. Order preserved.

Existing `-t JWT` continues to work alongside `--header`. They compose: synapse sends the `Cookie: access_token=...` from `-t` and any operator-supplied headers from `--header`. If both name a `Cookie:` header, `-t` is the latter and wins (or merge — implementer's call; matching curl's `--header` semantics is fine).

### A `--basic-auth USER:PASS` sugar (optional but ergonomic)

```bash
synapse \
  -H whoami.hypermemetic.ai -P 443 \
  --basic-auth operator:hunter2 \
  plexus-whoami whoami health
```

Equivalent to `--header "Authorization: Basic <b64>"`. Pure ergonomics.

### A `--bearer TOKEN` sugar (optional but useful)

```bash
synapse \
  -H api.example.com -P 443 \
  --bearer "$API_TOKEN" \
  plexus-foo foo bar
```

Equivalent to `--header "Authorization: Bearer $TOKEN"`.

### Resolver support

The existing `_self` resolver framework (SELF-7's `literal:` / `env:` / `file:` / SELF-8's `keychain:`) should work for header values too:

```bash
synapse --basic-auth "operator:env:WHOAMI_PASS" --header "X-Tenant: keychain://whoami-tenant" ...
```

Apply the same resolver pipeline that already runs on `-t/--token` values.

### Surface in `--help`

```
  --header NAME: VALUE     HTTP header to send on the WS upgrade (repeatable)
  --basic-auth USER:PASS   sugar for --header "Authorization: Basic <b64>"
  --bearer TOKEN           sugar for --header "Authorization: Bearer <TOKEN>"
```

The `-t/--token` description gets a small note: *"sent as Cookie: access_token=<jwt>; for arbitrary headers see --header"*.

### Wire output

Behavioral test: `synapse --dry-run --basic-auth operator:hunter2 -H api.x.com -P 443 plexus-foo foo bar` prints the would-be HTTP upgrade request including:

```
GET / HTTP/1.1
Host: api.x.com:443
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Key: ...
Sec-WebSocket-Version: 13
Authorization: Basic b3BlcmF0b3I6aHVudGVyMg==
```

So operators can verify what's being sent.

---

## Risks

- **Header value injection** via shell escaping. Mitigation: validate that header names match `[A-Za-z0-9-]+` and reject CR/LF in values; standard RFC 7230 rules.
- **Logging at trace level**: the existing transport logs may dump headers. Make sure `Authorization`, `Cookie`, and any header named `*-Token` / `*-Secret` get redacted in logs by default. Sensible-default redaction list, with `--log-show-headers` to bypass.
- **Multiple `Cookie:` headers**: HTTP allows it; many proxies normalize. If `-t` adds one and `--header` adds another, merge into a single Cookie header (`a=1; b=2`) per RFC.
- **Keychain access on agent runs**: `keychain://` (SELF-8) only resolves on macOS; agents on Linux pass through fine. No new failure mode.

---

## What must NOT change

- The existing `-t/--token` flag behavior. Preserved verbatim.
- Cookie header building from `-t`. Unchanged.
- The discovery/registry path (`-H`/`-P`). Unchanged.

---

## Acceptance criteria

1. `synapse --basic-auth operator:hunter2 -H whoami.hypermemetic.ai -P 443 plexus-whoami whoami health` returns the `health` event.
2. `synapse --header "Authorization: Basic b3BlcmF0b3I6aHVudGVyMg==" -H whoami.hypermemetic.ai -P 443 plexus-whoami whoami health` does the same (manual base64 form works).
3. `synapse --bearer "$TOKEN" -H api.x.com -P 443 plexus-foo foo bar` sends `Authorization: Bearer <TOKEN>`.
4. Resolvers (`literal:`, `env:`, `file:`, `keychain:`) work for `--basic-auth` and `--header` values.
5. `--dry-run` includes the operator-supplied headers in the printed request.
6. Multiple `--header` flags compose; same-name headers merge per HTTP rules.
7. `Authorization` and `Cookie` values are redacted at trace-log level by default.
8. Existing `-t/--token` and `--token-file` paths regress-test green.

---

## Cross-references

- Consumer: `hypermemetic-infra` `plans/PLATFORM/PLATFORM-38.md` (BasicAuth enforcement variant). Tonight's deploy of `https://whoami.hypermemetic.ai` is exactly the case this ticket unblocks.
- Sibling: `hypermemetic-infra` `plans/PLATFORM/PLATFORM-44.md` (Wire SessionValidator into every plexus service) — that ticket lands the JWT-via-Cookie path that `-t/--token` already supports. This ticket is about the *non-Logto* paths (basic-auth, bearer, custom edge).
- Existing infra: SELF-1..8's resolver framework gets reused for header values.
