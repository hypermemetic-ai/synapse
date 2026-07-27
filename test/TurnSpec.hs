{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | PLX-123 / M3·D — the update/terminal split, five stop kinds, five exit
-- codes, and a refusal that does not read as an error.
--
-- = What this suite proves, and what the wire proves instead
--
-- Two things in this ticket are only provable in two different places, and
-- this header says which is which rather than letting the counts imply it.
--
-- * __Provable here__: the classification guard (including the collision case
--   a live server will never produce on purpose), the five renderings, the
--   five exit codes, envelope stripping, and the §6.1 violations. These are
--   total functions over wire shapes; a live backend adds nothing.
-- * __Provable only on the wire__: that a `Refused` terminal is a thing a real
--   `plexus-macros`-generated handler actually emits, and that synapse's guard
--   fires on the real bytes rather than on bytes this file wrote. That is
--   `docs/evidence/plx-123-refused-e2e.txt`, captured against a real server
--   built on PLX-112's `IntoTurnStop`.
--
-- The fixtures below are the __captured__ envelopes from that run, not
-- hand-written approximations of them — see `refusedEnvelope`.
module Main (main) where

import           Data.Aeson       (Value (..), decode, object, (.=))
import qualified Data.Aeson.Key   as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe       (fromJust, isJust, isNothing)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           System.Exit      (ExitCode (..))
import           Test.Hspec

import           Plexus.Connectome.Types (StopKind (..), allStopKinds,
                                          stopKindWireName)
import           Plexus.Types             (Provenance (..))
import           Synapse.Schema.Types    (HubStreamItem, pattern HubData,
                                          pattern HubDone, pattern HubProgress)
import           Synapse.Turn

-- ---------------------------------------------------------------------------
-- Fixtures
-- ---------------------------------------------------------------------------

prov :: Provenance
prov = Provenance ["substrate"]

-- | The envelope a `complete` unary method produces.
completeEnvelope :: Value
completeEnvelope = fromJust . decode . LBS.pack $
  "{\"stop\":{\"kind\":\"complete\"},\"value\":{\"allowed\":true,\"tool\":\"rm -rf /\"}}"

-- | __Captured, not invented.__ This is the exact `content` of the
-- `gate.ask.terminal` `Data` item emitted by the real server in
-- `docs/evidence/plx-123-refused-e2e.txt`, whose refusal comes from a real
-- `impl IntoTurnStop` compiled by the real `plexus-macros` (PLX-112).
refusedEnvelope :: Value
refusedEnvelope = fromJust . decode . LBS.pack $
  "{\"stop\":{\"kind\":\"refused\",\"detail\":{\"code\":\"acp:permission_denied\"\
  \,\"message\":\"the user declined the permission request\"\
  \,\"data\":{\"tool\":\"rm -rf /\"}}},\"value\":null}"

-- | Likewise captured: a real `Limited`, from the same server.
limitedEnvelope :: Value
limitedEnvelope = fromJust . decode . LBS.pack $
  "{\"stop\":{\"kind\":\"limited\",\"detail\":{\"code\":\"acp:max_tokens\"\
  \,\"message\":\"the budget for this turn was exhausted\"}},\"value\":null}"

-- | Likewise captured: a real `Failed`. Note `error.code` is a __string__ —
-- see `a_failed_terminals_error_code_is_a_string_not_an_int`.
failedEnvelope :: Value
failedEnvelope = fromJust . decode . LBS.pack $
  "{\"stop\":{\"detail\":{\"code\":\"gate.boom\",\"message\":\"the gate broke\"}\
  \,\"error\":{\"code\":\"gate.boom\",\"message\":\"the gate broke\"\
  \,\"retryable\":true},\"kind\":\"failed\"},\"value\":null}"

-- | Not captured, and the suite says so: PLX-112 deliberately deferred the
-- server-side `Cancelled` spelling, because RFC 002 §6.8 makes cancellation
-- cooperative and an `Err` claiming `Cancelled` would let a turn assert a
-- signal that was never delivered. No server this repo can build emits this
-- today. It is constructed here because the __client__ must still render a
-- `Cancelled` it is handed — declining to decode a kind the RFC defines would
-- be a different bug — and because `clientCancelledStop` is synapse's own.
cancelledEnvelope :: Value
cancelledEnvelope = object
  [ "stop" .= object ["kind" .= ("cancelled" :: Value)]
  , "value" .= Null
  ]

envelopeFor :: StopKind -> Value
envelopeFor Complete  = completeEnvelope
envelopeFor Refused   = refusedEnvelope
envelopeFor Limited   = limitedEnvelope
envelopeFor Failed    = failedEnvelope
envelopeFor Cancelled = cancelledEnvelope

terminalItem :: StopKind -> HubStreamItem
terminalItem k = HubData "h" prov "gate.ask.terminal" (envelopeFor k)

main :: IO ()
main = hspec $ do

  -- =======================================================================
  describe "c1 — the turn shape is honoured (RFC 002 §6.1, §6.2)" $ do

    it "an update is classified as an update, and rendered as it arrives" $ do
      classifyItem (HubData "h" prov "gate.stream.update" (object ["chunk" .= ("hi" :: Value)]))
        `shouldBe` TurnUpdate (object ["chunk" .= ("hi" :: Value)])

    it "a terminal is classified as the terminal, distinctly" $ do
      case classifyItem (terminalItem Complete) of
        TurnTerminal s -> osKind s `shouldBe` Just Complete
        other          -> expectationFailure ("not a terminal: " <> show other)

    it "updates then exactly one terminal: the verdict is that one stop" $ do
      ref <- newTurnObservation
      mapM_ (observeItem ref . classifyItem)
        [ HubData "h" prov "gate.stream.update" (object ["n" .= (1 :: Int)])
        , HubData "h" prov "gate.stream.update" (object ["n" .= (2 :: Int)])
        , terminalItem Complete
        , HubDone "h" prov
        ]
      obs <- readTurnObservation ref
      toUpdates obs `shouldBe` 2
      case turnVerdict obs of
        TurnStopped s -> osKind s `shouldBe` Just Complete
        other         -> expectationFailure (show other)

    it "exactly ONE terminal is consumed — a second is a §6.1 violation, not a silent overwrite" $ do
      ref <- newTurnObservation
      mapM_ (observeItem ref . classifyItem) [terminalItem Complete, terminalItem Refused]
      v <- turnVerdict <$> readTurnObservation ref
      v `shouldBe` TurnManyTerminals 2
      verdictExitCode v `shouldBe` ExitFailure 6

    it "updates with no terminal is a §6.1 violation" $ do
      ref <- newTurnObservation
      observeItem ref (classifyItem (HubData "h" prov "gate.stream.update" Null))
      v <- turnVerdict <$> readTurnObservation ref
      v `shouldBe` TurnNoTerminal
      verdictExitCode v `shouldBe` ExitFailure 6

    it "a stream with NOTHING turn-shaped is NotATurn and exits 0 — every pre-M3 path" $ do
      ref <- newTurnObservation
      mapM_ (observeItem ref . classifyItem)
        [ HubData "h" prov "legacy.method" (object ["ok" .= True])
        , HubProgress "h" prov "working" Nothing
        , HubDone "h" prov
        ]
      v <- turnVerdict <$> readTurnObservation ref
      v `shouldBe` NotATurn
      verdictExitCode v `shouldBe` ExitSuccess

  -- =======================================================================
  describe "the guard is nominal AND structural (parity with plexus-transport's tool_payload)" $ do

    it "a_method_named_terminal_is_not_a_turn_terminal" $ do
      -- A legacy content_type is <namespace>.<method>, so a method actually
      -- CALLED `terminal` passes the suffix test. The structural half is what
      -- stops it. This is the exact collision PLX-145 pinned in Rust.
      turnTerminal "myns.terminal" (object ["stop" .= ("go" :: Value), "value" .= (1 :: Int)])
        `shouldSatisfy` isNothing

    it "rejects an envelope with a third key" $
      turnTerminal "x.terminal"
        (object ["stop" .= object ["kind" .= ("complete" :: Value)], "value" .= Null, "extra" .= (1 :: Int)])
        `shouldSatisfy` isNothing

    it "rejects an envelope whose stop.kind is not a string" $
      turnTerminal "x.terminal" (object ["stop" .= object ["kind" .= (3 :: Int)], "value" .= Null])
        `shouldSatisfy` isNothing

    it "rejects a .terminal item whose content is not an object at all" $
      turnTerminal "x.terminal" (String "done") `shouldSatisfy` isNothing

    it "accepts the real envelope" $
      turnTerminal "gate.ask.terminal" refusedEnvelope `shouldSatisfy` isJust

  -- =======================================================================
  describe "c2 — all five stop kinds have a rendering and an exit code" $ do

    -- The table, as a test. If a kind is ever added or a code ever moves,
    -- this fails rather than the report quietly going stale.
    it "the five exit codes are exactly the stated ones, and all distinct" $ do
      map (stopExitCode . Just) allStopKinds `shouldBe`
        [ ExitSuccess        -- Complete
        , ExitFailure 5      -- Cancelled
        , ExitFailure 3      -- Refused
        , ExitFailure 4      -- Limited
        , ExitFailure 1      -- Failed
        ]

    it "2 is never handed out — synapse _self already spends it on usage errors" $
      map (stopExitCode . Just) allStopKinds `shouldSatisfy` notElem (ExitFailure 2)

    it "REFUSED IS NOT AN ERROR: not by exit code, not by word, not by colour" $ do
      let Just s = turnTerminal "gate.ask.terminal" refusedEnvelope
      -- the machine half
      stopExitCode (osKind s) `shouldBe` ExitFailure 3
      stopExitCode (osKind s) `shouldNotBe` ExitFailure 1
      -- the "is this an error" predicate the colouriser keys off
      stopIsError (osKind s) `shouldBe` False
      -- the human half
      let Just banner = renderStop s
      banner `shouldBe`
        "Refused (not an error): acp:permission_denied — the user declined the permission request"
      T.isInfixOf "Error" banner `shouldBe` False

    it "FAILED is an error, and keeps synapse's pre-M3 'Error' spelling" $ do
      let Just s = turnTerminal "gate.ask.terminal" failedEnvelope
      stopIsError (osKind s) `shouldBe` True
      stopExitCode (osKind s) `shouldBe` ExitFailure 1
      renderStop s `shouldBe` Just "Error: gate.boom — the gate broke"

    it "LIMITED reads as a bound, not as a failure and not as a refusal" $ do
      let Just s = turnTerminal "gate.ask.terminal" limitedEnvelope
      stopIsError (osKind s) `shouldBe` False
      stopExitCode (osKind s) `shouldBe` ExitFailure 4
      renderStop s `shouldBe`
        Just "Limited (not an error): acp:max_tokens — the budget for this turn was exhausted"

    it "CANCELLED reads as a cancellation" $ do
      let Just s = turnTerminal "gate.ask.terminal" cancelledEnvelope
      stopIsError (osKind s) `shouldBe` False
      stopExitCode (osKind s) `shouldBe` ExitFailure 5
      renderStop s `shouldBe` Just "Cancelled (not an error)"

    it "COMPLETE is silent — it announces itself by printing its value and exiting 0" $ do
      let Just s = turnTerminal "gate.ask.terminal" completeEnvelope
      renderStop s `shouldBe` Nothing
      stopExitCode (osKind s) `shouldBe` ExitSuccess

    it "a kind outside §6.5's closed set is a protocol violation, not a near-miss" $ do
      let Just s = turnTerminal "x.terminal"
            (object ["stop" .= object ["kind" .= ("ghosted" :: Value)], "value" .= Null])
      osKind s `shouldBe` Nothing
      osKindRaw s `shouldBe` "ghosted"
      stopExitCode (osKind s) `shouldBe` ExitFailure 7
      renderStop s `shouldSatisfy` maybe False (T.isPrefixOf "Protocol error")

  -- =======================================================================
  describe "envelope stripping (PLX-145's rule, adopted verbatim)" $ do

    it "a complete terminal with a value contributes the VALUE, not the envelope" $ do
      let Just s = turnTerminal "g.m.terminal" completeEnvelope
      terminalOutput s `shouldBe`
        Just (object ["allowed" .= True, "tool" .= ("rm -rf /" :: Value)])

    it "a complete terminal with a null value contributes NOTHING" $ do
      let Just s = turnTerminal "g.m.terminal"
            (object ["stop" .= object ["kind" .= ("complete" :: Value)], "value" .= Null])
      terminalOutput s `shouldBe` Nothing

    it "a NON-complete terminal contributes the WHOLE envelope, verbatim" $ do
      -- The load-bearing row. A Refused's StopDetail is the only record that
      -- the turn did not do what was asked; dropping it to show a null value
      -- would destroy exactly what §6.6 exists to preserve.
      let Just s = turnTerminal "g.m.terminal" refusedEnvelope
      terminalOutput s `shouldBe` Just refusedEnvelope

    it "and so does a failed one, error and all" $ do
      let Just s = turnTerminal "g.m.terminal" failedEnvelope
      terminalOutput s `shouldBe` Just failedEnvelope

  -- =======================================================================
  describe "what synapse deliberately does not decode" $ do

    it "a_failed_terminals_error_code_is_a_string_not_an_int" $ do
      -- plexus-connectome's StructuredError types `code` as Int, so its
      -- decodeStopReason REJECTS this real terminal. That is why
      -- Synapse.Turn decodes only §6.5's `kind` and carries the rest as
      -- verbatim Values: the alternative is dropping a failure's structured
      -- error on the floor. Filed as PLX-154.
      let Just s = turnTerminal "g.m.terminal" failedEnvelope
      stopDetailCode s `shouldBe` Just "gate.boom"
      case osStop s of
        Object o -> case KM.lookup (K.fromText "error") o of
          Just (Object e) -> KM.lookup (K.fromText "code") e `shouldBe` Just (String "gate.boom")
          other           -> expectationFailure ("no structured error: " <> show other)
        other -> expectationFailure (show other)

  -- =======================================================================
  describe "cancellation — M3 decision gate 2 (RFC 002 §6.8, §10.2)" $ do

    it "synapse's own cancelled terminal is LABELLED as client-observed" $ do
      -- §6.8 makes cancellation cooperative: a conformant Cancelled asserts
      -- the signal reached the turn and the turn resolved. Synapse cannot
      -- assert that from outside the server, so it does not pretend to. The
      -- detail code says whose cancellation this is.
      stopDetailCode clientCancelledStop `shouldBe` Just "synapse:client_cancelled"
      osKind clientCancelledStop `shouldBe` Just Cancelled
      stopExitCode (osKind clientCancelledStop) `shouldBe` ExitFailure 5

    it "its message says plainly that the server was not observed to confirm" $
      stopDetailMessage clientCancelledStop
        `shouldSatisfy` maybe False (T.isInfixOf "not observed to confirm")

    it "a server-sent Cancelled wins: it is rendered, not replaced" $ do
      let Just s = turnTerminal "g.m.terminal" cancelledEnvelope
      stopDetailCode s `shouldBe` Nothing
      osKind s `shouldBe` Just Cancelled

  -- =======================================================================
  describe "the wire vocabulary is shared, not re-spelled" $
    it "synapse's kinds are plexus-connectome's kinds, and the spellings match plexus-core" $
      map stopKindWireName allStopKinds
        `shouldBe` ["complete", "cancelled", "refused", "limited", "failed"]
