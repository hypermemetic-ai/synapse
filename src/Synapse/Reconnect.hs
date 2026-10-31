{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SYNAPSE-20: synapse rides out an outage.
--
-- A lost connection is noticed by the client (plexus-protocol's
-- 'Plexus.Client': a close, a reset, or a keepalive ping unanswered), and
-- reported as 'PT.ConnectionLost'. Here is what synapse does about it:
--
-- * it tries again for the reconnect window (@--reconnect-for@,
--   @SYNAPSE_RECONNECT_FOR@; ten minutes unless told), backing off from
--   0.5 s, doubling, to 15 s, and saying so on stderr — never stdout;
-- * a read is sent again; a write whose request went out is not — the
--   write may have happened, so synapse says so and exits 'exitWriteUnknown';
--   a write whose request never left is sent when the connection is back;
-- * a stream is subscribed again with the same params — and with
--   @since@ set to when the call began, when the method takes one and the
--   caller gave none (@await_reply@), so nothing said during the outage is
--   lost — and the caller is told it resumed (@-j@ prints a
--   @{"type":"reconnected"}@ line);
-- * when the window runs out, it exits 'exitUnreachable' with the reason.
--
-- SYNAPSE-26: a closed stdout is not an outage. GHC's runtime ignores
-- SIGPIPE, so a write to a stdout whose reader has gone (@… | head -1@)
-- raises an EPIPE, and the transport reports anything raised in a call as
-- a network error — which "broken pipe" made an outage, and a write was
-- sent again for as long as the pipe stayed closed. Whatever the call's
-- callbacks raise is marked where it is raised, and the call ends there,
-- never retried. Those callbacks are the caller's side of the call: the
-- printer (each item, the @-j@ reconnected line, the stderr lines) and the
-- answering of a two-way request (the prompt or @--bidir-cmd@ that
-- answers it; sending the answer, @<backend>.respond@, reports its own
-- failure as a value and raises nothing). A closed stdout (or stderr) exits
-- 'exitOutputClosed' (141, what a shell says of a filter the pipe closed
-- on) saying nothing, with the stream unsubscribed and the socket closed
-- as the transport unwinds; any other failure there is reported as
-- before, once. A socket's own broken pipe — the peer gone — is never
-- raised in a callback, and is still an outage.
module Synapse.Reconnect
  ( -- * The window
    setReconnectWindow
  , reconnectWindow
  , defaultWindow
  , parseDuration
  , renderDuration

    -- * What a call is
  , CallShape(..)
  , callShape
  , readNames

    -- * Running a call through an outage
  , Resumed(..)
  , Ending(..)
  , resilientCall
  , retrying
  , awaitBackend
  , isOutage
  , isOutageText
  , giveUp
  , reportEnding

    -- * A closed stdout
  , isClosedOutput
  , isStdoutFailure
  , exitOnClosedOutput

    -- * Exit codes
  , exitUnreachable
  , exitWriteUnknown
  , exitOutputClosed
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isDigit)
import Data.IORef
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTime)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (exitImmediately)

import Plexus.Schema.Recursive (MethodSchema(..))
import qualified Plexus.Types as PT

-- ---------------------------------------------------------------------------
-- The window

-- | Ten minutes (owner, 2026-10-17: "at least 10 minutes of outage").
defaultWindow :: Double
defaultWindow = 600

{-# NOINLINE windowRef #-}
windowRef :: IORef Double
windowRef = unsafePerformIO (newIORef defaultWindow)

-- | Set once, at start, from @--reconnect-for@ / @SYNAPSE_RECONNECT_FOR@.
setReconnectWindow :: Double -> IO ()
setReconnectWindow = writeIORef windowRef . max 0

reconnectWindow :: IO Double
reconnectWindow = readIORef windowRef

-- | @10m@, @90s@, @1h@, @1m30s@, @600@ (seconds), @0@ (never).
parseDuration :: Text -> Maybe Double
parseDuration t0 = case T.unpack (T.toLower (T.strip t0)) of
  "" -> Nothing
  s | all isDigit s -> Just (read s)
    | otherwise -> go 0 s
  where
    go acc [] = Just acc
    go acc s = case span isDigit s of
      ([], _) -> Nothing
      (ds, u : rest) | Just k <- unit u -> go (acc + read ds * k) rest
      _ -> Nothing
    unit 'h' = Just 3600
    unit 'm' = Just 60
    unit 's' = Just 1
    unit _ = Nothing

-- | @2m10s@, @10m@, @45s@, @1h@.
renderDuration :: Double -> Text
renderDuration d =
  let n = max 0 (round d) :: Int
      (h, r) = n `divMod` 3600
      (m, s) = r `divMod` 60
      part v u = if v > 0 then T.pack (show v) <> u else ""
  in if n == 0 then "0s" else part h "h" <> part m "m" <> part s "s"

-- ---------------------------------------------------------------------------
-- What a call is

-- | What decides how a call rides out an outage.
data CallShape = CallShape
  { csStream :: Bool  -- ^ the method streams (a watch, await_reply)
  , csRead   :: Bool  -- ^ sending it again is safe
  , csSince  :: Bool  -- ^ it takes a @since@ a resumed call can carry
  } deriving (Eq, Show)

-- | A method's shape from its schema: a read when its group says @read@
-- (menger's methods do), else when its name is a read's; anything else —
-- and anything bidirectional — is a write, never sent twice.
callShape :: MethodSchema -> CallShape
callShape m = CallShape
  { csStream = methodStreaming m
  , csRead   = not (methodBidirectional m) && isRead
  , csSince  = takesSince (methodParams m)
  }
  where
    isRead = case methodGroup m of
      Just "read"  -> True
      Just "write" -> False
      _            -> methodName m `elem` readNames
    takesSince (Just (Object o))
      | Just (Object props) <- KM.lookup "properties" o = KM.member "since" props
    takesSince _ = False

-- | Method names that only read, for a schema that groups nothing.
readNames :: [Text]
readNames =
  [ "schema", "hash", "hashes", "health", "get", "list", "find", "search", "show"
  , "history", "watch", "await_reply", "await_message", "queued", "whoami"
  , "providers", "mine", "requests", "counts", "tree", "groups", "extents"
  , "facets", "member", "ready", "waves", "waves_under", "waves_to", "blockers"
  , "messages", "candidates", "environments", "running", "for_ticket"
  , "theme_for", "roles", "requires", "waiting"
  ]

-- ---------------------------------------------------------------------------
-- Outages

-- | A failure that is the backend being away, not a fault in the call.
isOutage :: PT.TransportError -> Bool
isOutage = \case
  PT.ConnectionRefused{} -> True
  PT.ConnectionTimeout{} -> True
  PT.ConnectionLost{}    -> True
  PT.NetworkError msg    -> isOutageText msg
  PT.ProtocolError msg   -> tooMany msg || gatewayDown msg

-- | The server's connection limits (ACCESS-29): a 429 at the upgrade.
tooMany :: Text -> Bool
tooMany msg = any (`T.isInfixOf` T.toLower msg) ["responsecode = 429", "too many requests"]

-- | SCAFFOLD-21: a proxy in front (the edge before a hosted app) answering
-- the upgrade 502, 503 or 504 — it is up, the service behind it is not (a
-- promote's restart). Nothing was sent; the service comes back behind it.
gatewayDown :: Text -> Bool
gatewayDown msg = any (`T.isInfixOf` T.toLower msg)
  ["responsecode = 502", "responsecode = 503", "responsecode = 504"]

-- | The same, read from a message (the probe says why as text). A refused
-- upgrade (401, 403, 421) is not an outage.
isOutageText :: Text -> Bool
isOutageText msg = any (`T.isInfixOf` T.toLower msg)
  [ "refused", "timed out", "timeout", "reset by peer", "connectionlost"
  , "connection lost", "broken pipe", "connectionclosed", "end of file"
  , "no route to host", "network is unreachable", "does not exist (connection"
    -- the server's connection limits (ACCESS-29) answer 429 at the upgrade:
    -- nothing was sent, and it passes
  , "responsecode = 429", "too many requests"
    -- SCAFFOLD-21: the proxy in front answers while the service behind is away
  , "responsecode = 502", "responsecode = 503", "responsecode = 504"
  ]

-- | The upgrade's 401: the credential refused (IDENTITY-43's test).
upgradeRefused :: PT.TransportError -> Bool
upgradeRefused e = any (`T.isInfixOf` reasonOf e) ["responseCode = 401", " 401 ", "(401)", "Unauthorized"]

-- | Whether a lost call's request had gone out.
sentOf :: PT.TransportError -> Bool
sentOf (PT.ConnectionLost _ s) = s
sentOf _ = False

reasonOf :: PT.TransportError -> Text
reasonOf = \case
  PT.ConnectionRefused{} -> "connection refused"
  PT.ConnectionTimeout{} -> "connection timed out"
  PT.ConnectionLost m _  -> m
  PT.NetworkError m      -> m
  PT.ProtocolError m     -> m

-- | A reason as a person says it (the probe's is a shown error).
plainReason :: Text -> Text
plainReason why
  | "ConnectionRefused" `T.isInfixOf` why = "connection refused"
  | "ConnectionTimeout" `T.isInfixOf` why = "connection timed out"
  | Just code <- find (\c -> ("responseCode = " <> c) `T.isInfixOf` why) ["502", "503", "504"] =
      "the proxy in front answered " <> code <> ": the service behind it is away"
  | otherwise = why

-- | An outage under way: when it began, how many tries, the next pause.
data Outage = Outage
  { oStart    :: Double
  , oAttempts :: Int
  , oDelay    :: Double
  }

say :: Text -> IO ()
say t = TIO.hPutStrLn stderr ("synapse: " <> t) >> hFlush stderr

-- | Pause before the next try, or 'Nothing' when the window is spent.
pause :: Text -> Double -> Text -> Maybe Outage -> IO (Maybe Outage)
pause = pauseAs True

-- | 'pause', saying the outage began as a lost connection or (at start) a
-- backend not reached.
pauseAs :: Bool -> Text -> Double -> Text -> Maybe Outage -> IO (Maybe Outage)
pauseAs lost whereTo window why mo = do
  now <- getMonotonicTime
  o <- case mo of
    Just o -> pure o
    Nothing -> do
      say $ (if lost then "connection to " <> whereTo <> " lost (" else "cannot reach " <> whereTo <> " (")
        <> plainReason why <> ") — " <> (if lost then "reconnecting" else "waiting")
        <> " for up to " <> renderDuration window
      pure (Outage now 0 0.5)
  let elapsed = now - oStart o
      left = window - elapsed
  if left <= 0
    then pure Nothing
    else do
      let d = min (oDelay o) left
      say $ "retrying " <> whereTo <> " in " <> renderDuration' d <> " ("
        <> renderDuration elapsed <> " of " <> renderDuration window <> ")"
      threadDelay (round (d * 1000000))
      pure (Just o { oAttempts = oAttempts o + 1, oDelay = min 15 (oDelay o * 2) })
  where
    renderDuration' d | d < 1 = T.pack (show (fromIntegral (round (d * 10) :: Int) / 10 :: Double)) <> "s"
                      | otherwise = renderDuration d

-- ---------------------------------------------------------------------------
-- Running a call through an outage

-- | A call that resumed after it had delivered something: how long the
-- outage was, how many tries it took, and the @since@ it carried (if it did).
data Resumed = Resumed
  { rsOutageSecs :: Double
  , rsAttempts   :: Int
  , rsSince      :: Maybe Integer
  } deriving (Eq, Show)

-- | How a call ended.
data Ending
  = Finished
  | Failed PT.TransportError   -- ^ not an outage (or no window): reported as before
  | GaveUp Double Text         -- ^ the window ran out: its length, the last reason
  | WriteUnknown Text          -- ^ a write went out and its connection went before the answer
  | OutputClosed               -- ^ SYNAPSE-26: stdout's reader closed the pipe; nothing is tried again
  deriving (Eq, Show)

-- | Run a call, one attempt at a time, through any outage within the
-- window. @attempt params onSubscribed onItem@ is one try. @onResume@ is
-- told, as soon as a try after an outage is taken by the server, that the
-- call resumed — for a stream always (what was said in the gap may be
-- missing, unless it carried @since@), for a unary read when it had
-- already delivered part of its answer.
resilientCall
  :: Text
  -> CallShape
  -> (Value -> IO () -> (PT.PlexusStreamItem -> IO ()) -> IO (Either PT.TransportError ()))
  -> IO Bool
     -- ^ after an outage, the server refused the credential at the upgrade
     -- (it lost its sessions in the restart): sign in again, True when a new
     -- credential is in place for the next try
  -> Value
  -> (Resumed -> IO ())
  -> (PT.PlexusStreamItem -> IO ())
  -> IO Ending
resilientCall whereTo shape attempt reauth params0 onResume onItem = do
  window <- reconnectWindow
  began <- (floor <$> getPOSIXTime) :: IO Integer
  everDelivered <- newIORef False
  -- SYNAPSE-26: what the printer raised, marked where it was raised — the
  -- transport reports it as a network error, and it is no outage
  printerFailed <- newIORef Nothing
  let printing :: IO () -> IO ()
      printing act = act `catch` \(e :: SomeException) -> case fromException e of
        Just (_ :: SomeAsyncException) -> throwIO e
        Nothing -> writeIORef printerFailed (Just e) >> throwIO e
      sinceFor n
        | n > (0 :: Int), csSince shape, Object o <- params0, unset (KM.lookup "since" o) = Just began
        | otherwise = Nothing
      -- a default the schema filled in (null) is no since of the caller's
      unset Nothing = True
      unset (Just Null) = True
      unset _ = False
      paramsFor n = case (sinceFor n, params0) of
        (Just s, Object o) -> Object (KM.insert "since" (Number (fromInteger s)) o)
        _ -> params0
      go n outage = goAuth n outage False
      goAuth n outage reauthed = do
        taken <- newIORef False
        let subscribed = writeIORef taken True >> printing (case outage of
                Just o -> do
                  now <- getMonotonicTime
                  say $ "reconnected to " <> whereTo <> " after " <> renderDuration (now - oStart o)
                  before <- readIORef everDelivered
                  if csStream shape || before
                    then onResume (Resumed (now - oStart o) (oAttempts o) (sinceFor n))
                    else pure ()
                Nothing -> pure ())
            item i = writeIORef everDelivered True >> printing (onItem i)
        r <- attempt (paramsFor n) subscribed item
        failed <- readIORef printerFailed
        t <- readIORef taken
        -- a try the server took had the connection back: a new loss is a
        -- new outage, with the whole window again
        let outage' = if t then Nothing else outage
        case (failed, r) of
          -- the printer's failure ends the call, whatever the transport made of it
          (Just pe, _)
            | Just (code :: ExitCode) <- fromException pe -> throwIO code
            | isClosedOutput pe -> pure OutputClosed
            -- SYNAPSE-39: any other failed write to stdout (-o's FILE on a
            -- full disk) is no outage and no failure of the call's: it is
            -- the run's, raised to it
            | isStdoutFailure pe -> throwIO pe
          (Just _, Left e) -> pure (Failed e)
          (_, Right ()) -> pure Finished
          (Nothing, Left e)
            | Just _ <- outage', not reauthed, upgradeRefused e -> do
                -- the server came back without the session: once per
                -- outage, sign in again and try at once
                ok <- reauth
                if ok then goAuth (n + 1) outage' True else pure (Failed e)
            | not (isOutage e) -> pure (Failed e)
            | sentOf e && not (csRead shape) -> pure (WriteUnknown (reasonOf e))
            | window <= 0 -> pure (Failed e)
            | otherwise -> do
                next <- pause whereTo window (reasonOf e) outage'
                case next of
                  Nothing -> pure (GaveUp window (reasonOf e))
                  Just o -> goAuth (n + 1) (Just o) (reauthed && not t)
  go 0 Nothing

-- | A call that is safe to repeat (a schema fetch), tried through an
-- outage within the window; when the window runs out, synapse exits.
retrying :: Text -> IO (Either PT.TransportError a) -> IO (Either PT.TransportError a)
retrying whereTo act = do
  window <- reconnectWindow
  let go outage = do
        r <- act
        case r of
          Left e | isOutage e, window > 0 -> do
            next <- pause whereTo window (reasonOf e) outage
            case next of
              Nothing -> giveUp whereTo window (reasonOf e)
              Just o -> go (Just o)
          _ -> do
            case (r, outage) of
              (Right _, Just o) -> do
                now <- getMonotonicTime
                say $ "reconnected to " <> whereTo <> " after " <> renderDuration (now - oStart o)
              _ -> pure ()
            pure r
  go Nothing

-- | The probe at start, tried through an outage within the window: while
-- it fails as an outage does (refused, timed out, reset), wait and try
-- again; any other answer is the caller's.
awaitBackend :: (Text -> Bool) -> Text -> IO (Either Text a) -> IO (Either Text a)
awaitBackend outage whereTo act = do
  window <- reconnectWindow
  let go o0 = do
        r <- act
        case r of
          Left why | outage why, window > 0 -> do
            next <- pauseAs False whereTo window why o0
            case next of
              Nothing -> giveUp whereTo window (plainReason why)
              Just o -> go (Just o)
          _ -> do
            case (r, o0) of
              (Right _, Just o) -> do
                now <- getMonotonicTime
                say $ "reconnected to " <> whereTo <> " after " <> renderDuration (now - oStart o)
              _ -> pure ()
            pure r
  go Nothing

-- ---------------------------------------------------------------------------
-- Endings

-- | SYNAPSE-26: 128 + SIGPIPE — what a shell says of a filter whose
-- output pipe closed (GHC ignores the signal, so synapse exits with it).
exitOutputClosed :: Int
exitOutputClosed = 141

-- | SYNAPSE-26: an EPIPE (or any vanished resource) raised writing to
-- stdout or stderr: its reader has gone. Told apart from a socket's own by
-- the handle the 'IOException' carries — a socket's carries none — as
-- GHC's top handler tells it (#2699).
isClosedOutput :: SomeException -> Bool
isClosedOutput e = case fromException e of
  Just IOError { ioe_type = ResourceVanished, ioe_handle = Just h } -> h == stdout || h == stderr
  _ -> False

-- | SYNAPSE-39: a write to stdout that failed — the pipe's reader gone
-- ('isClosedOutput'), or -o's FILE on a full disk. Raised by the printer,
-- never an outage.
isStdoutFailure :: SomeException -> Bool
isStdoutFailure e = case fromException e of
  Just IOError { ioe_handle = Just h } -> h == stdout
  _ -> False

-- | SYNAPSE-26: run a program, and exit 'exitOutputClosed' quietly when
-- its stdout's reader went — raised while it ran, or at the last flush.
-- A run that was ending with a code of its own keeps it (a write of
-- unknown outcome's 75, a refusal's 77, a spent window's 69): 141 only
-- ever replaces a success. Every other ending is the program's.
exitOnClosedOutput :: IO a -> IO a
exitOnClosedOutput act = (act >>= \a -> flushOr ExitSuccess >> pure a) `catch` \(e :: SomeException) ->
  case fromException e of
    Just (code :: ExitCode) -> flushOr code >> throwIO code
    _ | isClosedOutput e -> closed (ExitFailure exitOutputClosed)
      | otherwise -> throwIO e
  where
    flushOr code = hFlush stdout `catch` \(e :: SomeException) ->
      if isClosedOutput e
        then closed (if code == ExitSuccess then ExitFailure exitOutputClosed else code)
        else throwIO e
    -- at once: what is left in stdout's buffer has nowhere to go, and the
    -- runtime's own last flush would fail on it again
    closed :: ExitCode -> IO b
    closed code = do
      hFlush stderr `catch` \(_ :: SomeException) -> pure ()
      exitImmediately code
      exitWith code

-- | sysexits' EX_UNAVAILABLE: the backend stayed away for the whole window.
exitUnreachable :: Int
exitUnreachable = 69

-- | sysexits' EX_TEMPFAIL: a write went out and its answer never came; it
-- may have happened.
exitWriteUnknown :: Int
exitWriteUnknown = 75

giveUp :: Text -> Double -> Text -> IO a
giveUp whereTo window why = do
  say $ whereTo <> " unreachable for " <> renderDuration window <> " (" <> why
    <> ") — giving up; --reconnect-for sets how long to wait"
  exitWith (ExitFailure exitUnreachable)

-- | The endings that are synapse's to report: a spent window and a write
-- of unknown outcome exit here; the others go back to the caller.
reportEnding :: Text -> Ending -> IO (Either PT.TransportError ())
reportEnding whereTo = \case
  Finished -> pure (Right ())
  Failed e -> pure (Left e)
  GaveUp window why -> giveUp whereTo window why
  OutputClosed -> exitWith (ExitFailure exitOutputClosed)
  WriteUnknown why -> do
    say $ "connection to " <> whereTo <> " lost before an answer (" <> why
      <> ") — the write may have happened; check before sending it again"
    exitWith (ExitFailure exitWriteUnknown)
