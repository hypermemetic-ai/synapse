# _self test Connection Fix (v3.9.0)

**Date**: 2026-03-23
**Issue**: `_self test` command failed with "Connection refused"
**Root Cause**: IPv4/IPv6 address mismatch between discovery and test phases

## The Problem

Prior to v3.9.0, the `_self test` command had this signature:

```bash
synapse _self test localhost 4444 substrate ping.pong --message "test"
```

This caused a subtle but critical bug:

1. **Main.hs discovery phase**: Used `-H/--host` flag (default: `127.0.0.1`)
   - Successfully called `_info` three times
   - Backend discovered correctly

2. **testMethod execution phase**: Used positional `localhost` argument
   - Created new `SubstrateConfig` with `host = "localhost"`
   - Failed with "Connection refused"

### Why It Failed

The server was bound to `127.0.0.1` (IPv4 only). When synapse tried to connect to "localhost", the hostname resolved to `::1` (IPv6), causing the connection to fail.

### Debug Output Showing the Issue

```
[DEBUG] Checking backend at 127.0.0.1:4444
[DEBUG Discovery] RPC call succeeded, got 2 items  ✓
[DEBUG Discovery] RPC call succeeded, got 2 items  ✓
[DEBUG Discovery] RPC call succeeded, got 2 items  ✓

Testing: ping.pong
[FAIL] Test FAILED
Protocol error: Connection failed: Connection refused  ✗
```

Discovery succeeded 3 times, then the test immediately failed trying to connect.

## The Fix

### Code Changes

**1. Commands.hs - Use SynapseEnv for connection settings**

```haskell
-- Before
dispatch "test" rest params = do
  case rest of
    (hostText:portText:backendText:methodParts) -> do
      let host = T.unpack hostText
      let port = read (T.unpack portText) :: Int
      liftIO $ Debugger.testMethod host port backendText method ...

-- After
dispatch "test" rest params = do
  host <- asks seHost     -- Get from environment
  port <- asks sePort     -- Get from environment
  backend <- asks seBackend

  case rest of
    methodParts | not (null methodParts) -> do
      let config = SubstrateConfig { ... }
      liftIO $ Debugger.testMethod config method ...
```

**2. Debugger.hs - Accept SubstrateConfig directly**

```haskell
-- Before
testMethod :: String -> Int -> Text -> Text -> [(Text, Text)] -> Bool -> Maybe Text -> IO ()
testMethod host port backend method cliParams allowUnknown rawJson = do
  let config = SubstrateConfig { substrateHost = host, ... }

-- After
testMethod :: SubstrateConfig -> Text -> [(Text, Text)] -> Bool -> Maybe Text -> IO ()
testMethod config method cliParams allowUnknown rawJson = do
  -- Config is passed in, already validated by discovery
```

### New Usage

```bash
# Correct usage - connection from flags
synapse -P 4444 _self test substrate.ping.pong --message "test"

# With custom host
synapse -H 192.168.1.100 -P 5001 _self test backend.method --param value

# Raw JSON mode
synapse -P 4444 _self test --raw '{"message":"test"}' substrate.method
```

### Updated Help Text

```
synapse [-H <host>] [-P <port>] _self test <method> [--param value ...]
    Test arbitrary method with protocol validation
    Connection: Uses -H/--host and -P/--port flags (defaults: 127.0.0.1:4444)
```

## Why This Is Better

1. **Single source of truth**: Connection settings come from CLI flags, not duplicated in positional args
2. **Consistent behavior**: Test uses the same connection that discovery validated
3. **No IPv4/IPv6 confusion**: All phases use the same address
4. **Matches other commands**: `_self debug` and `_self validate` also use positional args for target, this was inconsistent

## Testing

```bash
# Before fix - fails
synapse _self test localhost 4444 substrate ping.pong --message "test"
# Error: Connection refused

# After fix - succeeds (connection-wise)
synapse -P 4444 _self test substrate.ping.pong --message "test"
# Connection succeeds, may fail if method doesn't exist but that's expected
```

## Files Changed

- `src/Synapse/Self/Commands.hs` (+18 lines)
- `src/Synapse/Self/Debugger.hs` (+6/-7 lines)
- `README.md` (+61 lines - new Debugging & Testing section)
- `plexus-synapse.cabal` (version 3.8.0 → 3.9.0)

## Related Issues

This fix resolves the connection failure but may reveal other issues:
- Backend methods that don't exist return `-32603 Internal error`
- Metadata validation may fail if server doesn't populate `plexus_hash` or `timestamp` correctly

These are **server-side issues**, not client-side issues, and are now properly visible through protocol validation.
