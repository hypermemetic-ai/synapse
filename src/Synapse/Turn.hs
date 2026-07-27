{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Synapse.Turn
-- Description : PLX-123 / M3·D — the update/terminal split, and a refusal that
--               reads as a refusal.
--
-- = What this module is for
--
-- RFC 002 §6.1: every invocation is a turn, and a turn emits zero or more
-- /updates/ and then exactly one /terminal/ carrying a stop reason. §6.2: the
-- two MUST be distinguishable by a consumer. Until this module existed synapse
-- was not that consumer — it rendered every @Data@ item identically, so a
-- turn's terminal envelope leaked onto the user's screen as raw JSON and the
-- stop kind was never read at all.
--
-- §6.6 says a __Refused__ is a considered "no" — policy, a denied permission, a
-- guardrail — and MUST NOT be conflated with __Failed__. That distinction is
-- worth nothing if the client paints both red and exits 1 for both. Hence
-- 'stopExitCode' and 'stopIsError', and hence the fact that this module treats
-- "is this an error?" as a question with a per-kind answer rather than as a
-- synonym for "did we get a value?".
--
-- = How a turn appears on the legacy wire
--
-- The legacy plexus wire has no turn vocabulary. @plexus-core@'s
-- @runtime::bridge::turn_stream_to_plexus_stream@ projects a turn onto it:
--
-- @
-- TurnEvent::Update   -> Data { content_type = "\<dotted_id\>.update" }
-- TurnEvent::Callback -> Request { request_id = "\<turn\>:\<seq\>" }
-- TurnEvent::Terminal -> Data { content_type = "\<dotted_id\>.terminal",
--                               content = {"stop": .., "value": ..} }, then Done
-- @
--
-- So the split IS on the wire, carried entirely by @content_type@ — and
-- synapse simply never read it. 'classifyItem' reads it.
--
-- = Why the guard is nominal AND structural
--
-- This is deliberately the same two-part test @plexus-transport@'s
-- @mcp::bridge::turn_terminal_stop_kind@ applies (PLX-145), reimplemented here
-- rather than approximated, so the CLI and the MCP gateway agree about what a
-- terminal /is/:
--
-- * __nominal__ — @content_type@ ends with @.terminal@;
-- * __structural__ — the content is an object with __exactly__ the two keys
--   @stop@ and @value@, whose @stop@ is an object carrying a __string__
--   @kind@.
--
-- The structural half is load-bearing: a legacy @content_type@ is
-- @\<namespace\>.\<method\>@, so an ordinary hand-written method actually
-- /named/ @terminal@ would pass a suffix-only test. It is pinned by test
-- (@a_method_named_terminal_is_not_a_turn_terminal@).
--
-- = What the terminal contributes to the output
--
-- PLX-145 decided this for the MCP gateway and the reasoning is not
-- gateway-specific, so the CLI adopts the identical rule — /envelope
-- stripping/, removing exactly the bytes the turn projection added and nothing
-- else:
--
-- +--------------------------------------------+------------------------------+
-- | terminal                                   | contributes                  |
-- +============================================+==============================+
-- | @stop.kind == complete@, @value@ non-null  | the @value@                  |
-- +--------------------------------------------+------------------------------+
-- | @stop.kind == complete@, @value@ null      | nothing                      |
-- +--------------------------------------------+------------------------------+
-- | any other @stop.kind@                      | the whole @{stop,value}@     |
-- +--------------------------------------------+------------------------------+
--
-- The third row is the one that matters and it is not an oversight: a
-- @Failed@'s structured error and a @Refused@'s 'StopDetail' are the __only__
-- record that the turn did not do what was asked. Dropping them to show a null
-- value would destroy the very information §6.6 exists to preserve.
--
-- = What is deliberately NOT decoded
--
-- This module reuses @plexus-connectome@'s 'StopKind' and 'stopKindFromWire' —
-- the closed §6.5 vocabulary should exist once — but it deliberately does NOT
-- use that package's @decodeStopReason@. That decoder types a §6.7 structured
-- error's @code@ as an 'Int', while RFC 002 §6.7 as amended by PLX-114 (and
-- @plexus-core@ in fact) makes it a __string__. Running a real @failed@
-- terminal through it fails to parse. Rather than silently drop a failure's
-- error on the floor, synapse decodes only what §6.5 actually guarantees — the
-- @kind@ — and carries @stop@ and @value@ onward as verbatim 'Value's. See
-- PLX-154.
module Synapse.Turn
  ( -- * Classifying a stream item
    TurnItem (..)
  , ObservedStop (..)
  , classifyItem
  , turnTerminal

    -- * The observed turn
  , TurnObservation (..)
  , newTurnObservation
  , observeItem
  , readTurnObservation
  , TurnVerdict (..)
  , turnVerdict

    -- * Rendering and exit codes
  , stopExitCode
  , verdictExitCode
  , stopIsError
  , stopLabel
  , renderStop
  , terminalOutput
  , stopDetailCode
  , stopDetailMessage

    -- * Cancellation (M3 decision gate 2)
  , clientCancelledStop
  ) where

import           Data.Aeson              (Value (..))
import qualified Data.Aeson.Key          as K
import qualified Data.Aeson.KeyMap       as KM
import           Data.IORef
import           Data.Text               (Text)
import qualified Data.Text               as T
import           System.Exit             (ExitCode (..))

import           Plexus.Connectome.Types (StopKind (..), stopKindFromWire,
                                          stopKindWireName)
import           Synapse.Schema.Types    (HubStreamItem, pattern HubData)

-- ---------------------------------------------------------------------------
-- Observing a terminal
-- ---------------------------------------------------------------------------

-- | A terminal as it actually arrived, decoded only as far as §6.5 guarantees.
data ObservedStop = ObservedStop
  { osKind    :: !(Maybe StopKind)
    -- ^ 'Nothing' when the wire named a kind outside §6.5's closed set. §6.5
    -- says the set is closed and that new domain reasons ride in the detail,
    -- so an unknown kind is a protocol violation and is reported as one rather
    -- than being rounded to the nearest neighbour.
  , osKindRaw :: !Text
    -- ^ The kind exactly as it appeared on the wire.
  , osStop    :: !Value
    -- ^ The whole @stop@ object, verbatim. Never flattened (§6.7).
  , osValue   :: !(Maybe Value)
    -- ^ The terminal's value. 'Nothing' for an absent or @null@ value.
  , osRawEnvelope :: !Value
    -- ^ The whole @{stop, value}@ object, verbatim — what a non-@complete@
    -- terminal contributes.
  }
  deriving stock (Eq, Show)

-- | A stream item, classified against the turn projection.
data TurnItem
  = TurnUpdate !Value
    -- ^ §6.2 — an update, arriving before the terminal.
  | TurnTerminal !ObservedStop
    -- ^ §6.1 — the one terminal.
  | TurnOpaque
    -- ^ Anything the turn projection did not produce: a legacy hand-written
    -- activation's @Data@, a progress frame, an error frame, @Done@. Rendered
    -- exactly as before — this is what keeps non-turn-native methods
    -- byte-identical.
  deriving stock (Eq, Show)

-- | Classify one stream item.
classifyItem :: HubStreamItem -> TurnItem
classifyItem = \case
  HubData _ _ ct content
    | Just stop <- turnTerminal ct content -> TurnTerminal stop
    | ".update" `T.isSuffixOf` ct          -> TurnUpdate content
  _ -> TurnOpaque

-- | The nominal + structural terminal guard. See the module header for why it
-- is both, and @plexus-transport@'s @turn_terminal_stop_kind@ for the copy
-- this one is deliberately identical to.
turnTerminal :: Text -> Value -> Maybe ObservedStop
turnTerminal contentType content = do
  _ <- if ".terminal" `T.isSuffixOf` contentType then Just () else Nothing
  o <- asObject content
  -- exactly two keys, and one of them is "value"
  _ <- if KM.size o == 2 && KM.member (K.fromText "value") o then Just () else Nothing
  stopV <- KM.lookup (K.fromText "stop") o
  stopO <- asObject stopV
  kindV <- KM.lookup (K.fromText "kind") stopO
  kindT <- case kindV of
    String t -> Just t
    _        -> Nothing
  let val = case KM.lookup (K.fromText "value") o of
        Just Null -> Nothing
        Just v    -> Just v
        Nothing   -> Nothing
  pure ObservedStop
    { osKind        = stopKindFromWire kindT
    , osKindRaw     = kindT
    , osStop        = stopV
    , osValue       = val
    , osRawEnvelope = content
    }
  where
    asObject (Object o) = Just o
    asObject _          = Nothing

-- ---------------------------------------------------------------------------
-- The observation, accumulated across a stream
-- ---------------------------------------------------------------------------

-- | What a whole invocation turned out to be. Accumulated by 'observeItem' as
-- items arrive, because the renderer is a callback with no return value and
-- the exit code has to survive to @main@.
data TurnObservation = TurnObservation
  { toUpdates   :: !Int
  , toTerminals :: ![ObservedStop]
    -- ^ Reversed. A list rather than a @Maybe@ so that a §6.1 violation — two
    -- terminals — is representable and therefore reportable, the same reason
    -- @plexus-connectome@'s 'Plexus.Connectome.Types.Turn' keeps its terminal
    -- optional.
  }
  deriving stock (Eq, Show)

emptyObservation :: TurnObservation
emptyObservation = TurnObservation 0 []

newTurnObservation :: IO (IORef TurnObservation)
newTurnObservation = newIORef emptyObservation

-- | Fold one classified item into the observation.
observeItem :: IORef TurnObservation -> TurnItem -> IO ()
observeItem ref = \case
  TurnUpdate _   -> modifyIORef' ref $ \o -> o { toUpdates = toUpdates o + 1 }
  TurnTerminal s -> modifyIORef' ref $ \o -> o { toTerminals = s : toTerminals o }
  TurnOpaque     -> pure ()

readTurnObservation :: IORef TurnObservation -> IO TurnObservation
readTurnObservation = readIORef

-- | The verdict on a whole invocation.
data TurnVerdict
  = NotATurn
    -- ^ Nothing turn-shaped was seen at all: a legacy hand-written activation,
    -- a bidirectional method, a schema fetch. Exit behaviour is unchanged, and
    -- that is the point — this is the branch every pre-M3 invocation takes.
  | TurnStopped !ObservedStop
    -- ^ §6.1 honoured: exactly one terminal.
  | TurnNoTerminal
    -- ^ §6.1 violated: updates arrived from the turn projection and the stream
    -- ended without a terminal.
  | TurnManyTerminals !Int
    -- ^ §6.1 violated: more than one terminal.
  deriving stock (Eq, Show)

turnVerdict :: TurnObservation -> TurnVerdict
turnVerdict (TurnObservation ups terms) = case reverse terms of
  []  | ups > 0   -> TurnNoTerminal
      | otherwise -> NotATurn
  [s]             -> TurnStopped s
  ss              -> TurnManyTerminals (length ss)

-- ---------------------------------------------------------------------------
-- Exit codes
-- ---------------------------------------------------------------------------

-- | The exit code for a stop kind.
--
-- The whole point of the table is that __a refusal is not a failure__, so it
-- does not get a failure's exit code. It is still non-zero, because the thing
-- the user asked for did not happen and a script must not treat it as done —
-- but a script can tell the two apart with @$?@ alone, which is the machine
-- half of the §6.6 distinction.
--
-- @2@ is not used: @synapse _self@ already spends it on usage errors
-- ("Synapse.Self.Command"), and reassigning a live code would be a silent
-- incompatibility.
--
-- +-----------+------+--------------------------------------------------+
-- | kind      | code | meaning                                          |
-- +===========+======+==================================================+
-- | Complete  | 0    | the turn did what was asked                       |
-- +-----------+------+--------------------------------------------------+
-- | Failed    | 1    | a structured error — unchanged from pre-M3        |
-- +-----------+------+--------------------------------------------------+
-- | (usage)   | 2    | reserved, pre-existing                            |
-- +-----------+------+--------------------------------------------------+
-- | Refused   | 3    | a considered no; NOT an error                     |
-- +-----------+------+--------------------------------------------------+
-- | Limited   | 4    | a bound was hit; NOT an error                     |
-- +-----------+------+--------------------------------------------------+
-- | Cancelled | 5    | the turn stopped waiting; NOT an error            |
-- +-----------+------+--------------------------------------------------+
-- | (§6.1)    | 6    | no terminal, or more than one                     |
-- +-----------+------+--------------------------------------------------+
-- | (§6.5)    | 7    | a stop kind outside the closed core set           |
-- +-----------+------+--------------------------------------------------+
stopExitCode :: Maybe StopKind -> ExitCode
stopExitCode = \case
  Just Complete  -> ExitSuccess
  Just Failed    -> ExitFailure 1
  Just Refused   -> ExitFailure 3
  Just Limited   -> ExitFailure 4
  Just Cancelled -> ExitFailure 5
  Nothing        -> ExitFailure 7

verdictExitCode :: TurnVerdict -> ExitCode
verdictExitCode = \case
  NotATurn            -> ExitSuccess
  TurnStopped s       -> stopExitCode (osKind s)
  TurnNoTerminal      -> ExitFailure 6
  TurnManyTerminals _ -> ExitFailure 6

-- | Is this stop an error? Exactly one kind is (§6.7), and 'Refused' is
-- emphatically not (§6.6).
--
-- An unknown kind counts as an error because we cannot vouch for it — but note
-- that it is a /protocol/ error, not the method's, and 'renderStop' says so.
stopIsError :: Maybe StopKind -> Bool
stopIsError = \case
  Just Failed -> True
  Nothing     -> True
  _           -> False

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

-- | The word a stop is announced with. @Failed@ keeps synapse's pre-M3
-- @Error:@ spelling so that nothing which greps stderr today breaks; every
-- other kind gets its own word, and none of them is @Error@.
stopLabel :: Maybe StopKind -> Text
stopLabel = \case
  Just Complete  -> "Complete"
  Just Failed    -> "Error"
  Just Refused   -> "Refused"
  Just Limited   -> "Limited"
  Just Cancelled -> "Cancelled"
  Nothing        -> "Protocol error"

-- | The @code@ of the §6.5 open detail, if the terminal carries one.
stopDetailCode :: ObservedStop -> Maybe Text
stopDetailCode = detailField "code"

-- | The @message@ of the §6.5 open detail, if the terminal carries one.
stopDetailMessage :: ObservedStop -> Maybe Text
stopDetailMessage = detailField "message"

detailField :: Text -> ObservedStop -> Maybe Text
detailField field s = case osStop s of
  Object o -> case KM.lookup (K.fromText "detail") o of
    Just (Object d) -> case KM.lookup (K.fromText field) d of
      Just (String t) -> Just t
      _               -> Nothing
    _ -> Nothing
  _ -> Nothing

-- | The one-line banner for a terminal, or 'Nothing' for a plain completion
-- (which announces itself by printing its value and exiting 0, and must stay
-- silent so that existing output is unchanged).
--
-- The banner is deliberately word-led rather than colour-led: colour is
-- applied by the caller only when stderr is a terminal, so a pipe, a log and a
-- test all still carry the distinction.
renderStop :: ObservedStop -> Maybe Text
renderStop s = case osKind s of
  Just Complete -> Nothing
  k             -> Just (stopLabel k <> suffix <> detail)
  where
    suffix
      | stopIsError (osKind s) = ""
      -- The parenthetical is the point of the whole ticket. A user who sees
      -- "Refused" and nothing else has to already know RFC 002 §6.6 to know
      -- their command did not break.
      | otherwise              = " (not an error)"
    detail = case (stopDetailCode s, stopDetailMessage s) of
      (Just c, Just m)  -> ": " <> c <> " — " <> m
      (Just c, Nothing) -> ": " <> c
      (Nothing, Just m) -> ": " <> m
      (Nothing, Nothing)
        | Nothing <- osKind s -> ": stop kind " <> quoted (osKindRaw s)
                                  <> " is outside RFC 002 §6.5's closed core set "
                                  <> "(" <> T.intercalate ", " (map stopKindWireName allKinds) <> ")"
        | otherwise           -> ""
    quoted t = "\"" <> t <> "\""
    allKinds = [Complete, Cancelled, Refused, Limited, Failed]

-- | What a terminal contributes to __stdout__ — PLX-145's envelope-stripping
-- rule, applied identically here. See the module header's table.
terminalOutput :: ObservedStop -> Maybe Value
terminalOutput s
  | osKind s == Just Complete = osValue s
  | otherwise                 = Just (osRawEnvelope s)

-- ---------------------------------------------------------------------------
-- Cancellation — M3 decision gate 2
-- ---------------------------------------------------------------------------

-- | The terminal synapse synthesises for itself when the __client__ cancels
-- and the server does not answer with one.
--
-- __This is a client-observed cancellation, and it is labelled as one.__ RFC
-- 002 §6.8 makes cancellation cooperative: a conformant @Cancelled@ asserts
-- that the signal was delivered to the turn, that the turn resolved, and that
-- in-flight callbacks resolved. Synapse can assert none of that from outside,
-- so the detail code is @synapse:client_cancelled@ and its message says
-- plainly that the server was not observed to confirm. It is __not__ a claim
-- that the work stopped — §6.8 forbids exactly that claim, and PLX-112
-- deferred the server-side @Cancelled@ spelling for the same reason: a turn
-- asserting a signal it never received would make the kind unfalsifiable.
--
-- If the server /does/ send a @Cancelled@ terminal, that terminal is what gets
-- rendered and this value is never constructed.
clientCancelledStop :: ObservedStop
clientCancelledStop = ObservedStop
  { osKind        = Just Cancelled
  , osKindRaw     = stopKindWireName Cancelled
  , osStop        = stop
  , osValue       = Nothing
  , osRawEnvelope = Object (KM.fromList [(K.fromText "stop", stop), (K.fromText "value", Null)])
  }
  where
    stop = Object $ KM.fromList
      [ (K.fromText "kind", String (stopKindWireName Cancelled))
      , (K.fromText "detail", Object $ KM.fromList
          [ (K.fromText "code", String "synapse:client_cancelled")
          , (K.fromText "message", String
              "the client sent cancellation and stopped consuming; the server was \
              \not observed to confirm with a Cancelled terminal (RFC 002 §6.8 \
              \makes cancellation cooperative, and §10.2 leaves the channel \
              \unspecified — see PLX-84 §10.2, amended 2026-07-26)")
          ])
      ]
