-- | Lifting synapse's wire view of a backend into the Connectome (PLX-119).
--
-- = Why this module exists
--
-- CONNECTOME RFC 002 (PLX-84) specifies /one/ representation of a plexus
-- interface, and @plexus-connectome@ (PLX-86) is its independent Haskell
-- implementation. M3 exists to make synapse stop carrying a notion of the
-- document of its own.
--
-- The obstacle is not modelling, it is /supply/: as of this build no plexus
-- server emits a Connectome. @plexus_core::ir::ActivationIr@ exists but is a
-- builder-side type — it is reachable from @runtime\/bridge.rs@ and the tests
-- and from nowhere on the wire. The only schema a live backend serves is the
-- legacy @PluginSchema@ over @{namespace}.schema@, which is what
-- "Plexus.Schema.Recursive" decodes. Making the server emit a Connectome is
-- M2's work (PLX-94), not this build's.
--
-- This module is therefore the /seam/: a total, structural lift from the
-- legacy wire tree into the RFC's types. It creates no third model. Everything
-- it produces is a @plexus-connectome@ value, and the moment M2 lands, this
-- module is deleted and the decoder is pointed at the real document instead.
--
-- = The rule this module obeys
--
-- __It never invents.__ RFC §5.2 ("a Dynamic edge MUST NOT be synthesized into
-- a typed subtree by a consumer. Fabricating structure that is not present is
-- the failure this representation exists to prevent") is the governing
-- instruction, and it is applied beyond edges: where the legacy wire cannot
-- express something the RFC requires, this module says so in the type rather
-- than filling the hole. §6.4's three-state 'Declared' is what makes that
-- expressible — @Unresolved@ is the honest answer to "the wire conflates two
-- things the RFC separates".
--
-- The consequence is deliberate and is the point: __a lifted legacy document
-- is NOT conformant__, and "Plexus.Connectome.Check" names exactly which
-- clauses it misses. That verdict is a measurement of the wire's distance from
-- RFC 002, produced by the RFC's own checker. 'legacyWireGaps' pins the list so
-- that when M2 closes a gap, the test that pins it fails and tells you.
--
-- = What is deliberately NOT lifted
--
-- [Parameters and type references]
--   'mParams' is always @[]@. The legacy wire carries one JSON-Schema blob per
--   method plus optional per-param names, and no canonical type names at all;
--   RFC §10.3 defines a type reference as "a canonical name plus a schema", so
--   there is no honest source for the name half. Synthesizing one would be
--   exactly the invention §5.2 forbids. Types and parameters stay on the IR
--   projection ("Synapse.IR.Builder", ~470 lines of JSON-Schema extraction),
--   which is where they already work; duplicating that extraction here would
--   create the second access path this build exists to avoid.
--
-- [Credentials]
--   'mAuth' is always 'Nothing'. The legacy wire's @auth_posture@ / @public@ /
--   @requires_credential@ axis has no representation in the Connectome beyond a
--   single optional text field. That gap is __PLX-120's__, it needs an RFC
--   amendment, and RFC §10.4 forbids routing it through @ext(...)@. This module
--   does not work around it.
--
-- [Capabilities]
--   'mCallbacks' is always the empty set. The legacy wire declares no
--   callbacks. An empty 'CapabilitySet' is a conformant declaration of "none"
--   (§7.1), not a placeholder.
module Synapse.Connectome
  ( -- * Lifting
    liftConnectome
  , liftActivation
  , liftMethod

    -- * What the legacy wire cannot say
  , LegacyWireGap (..)
  , GapVisibility (..)
  , legacyWireGaps
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import qualified Plexus.Connectome.Types as C
import qualified Plexus.Schema.Recursive as W

--------------------------------------------------------------------------------
-- Small helpers
--------------------------------------------------------------------------------

-- | The legacy wire spells "no description" as the empty string; RFC §3.5
-- spells absence as absence. This is the only place the two disagree and it is
-- a lossless correction, not an invention.
nonEmpty :: Text -> Maybe Text
nonEmpty t
  | T.null (T.strip t) = Nothing
  | otherwise = Just t

-- | §3.2's deprecation record. The legacy 'W.DeprecationInfo' makes all three
-- fields mandatory 'Text' where the Connectome makes them optional, so this is
-- a widening and cannot lose anything.
liftDeprecation :: W.DeprecationInfo -> C.Deprecation
liftDeprecation d =
  C.Deprecation
    { C.depSince = nonEmpty (W.depSince d)
    , C.depRemovedIn = nonEmpty (W.depRemovedIn d)
    , C.depMessage = nonEmpty (W.depMessage d)
    }

--------------------------------------------------------------------------------
-- Methods
--------------------------------------------------------------------------------

-- | Lift one method, given the namespace of the activation that owns it.
--
-- The dotted identifier (§10.1) is composed here as @namespace.name@ because
-- the legacy wire does not carry one. This is /not/ a §10.1 violation: §10.1
-- forbids inventing a rule for composing an __instance-qualified__ invocation
-- (a path template with a dotted id), and this is the plain qualified name that
-- synapse already keys every method by (@irMethods@, @mdFullPath@). §10.1.1 is
-- satisfied because no template placeholder can appear here — the legacy wire
-- has no path templates at all.
liftMethod :: Text -> W.MethodSchema -> C.Method
liftMethod ns m =
  C.Method
    { C.mName = C.MethodName (W.methodName m)
    , C.mDottedId = C.DottedId (ns <> "." <> W.methodName m)
    , C.mDescription = nonEmpty (W.methodDescription m)
    , C.mHash = C.Hash (W.methodHash m)
    , -- See the module header: no canonical type names exist on this wire.
      C.mParams = []
    , -- §6.9 — the request context is declared per activation on this wire
      -- (@psRequest@), never per method, so a method never overrides it.
      C.mRequestContext = C.NotDeclared
    , C.mUpdates = updates
    , C.mTerminal = terminal
    , -- §7.1 — a conformant declaration of "no callbacks".
      C.mCallbacks = C.CapabilitySet []
    , C.mHttpMethod = Nothing
    , -- PLX-120 owns this. Not worked around here.
      C.mAuth = Nothing
    , C.mDeprecation = liftDeprecation <$> W.methodDeprecation m
    , C.mExtensions = mempty
    }
  where
    -- §6.3 requires the update type and the terminal type to be separate
    -- declarations, and §6.4 requires "declares nothing" to be distinguishable
    -- from "failed to resolve". The legacy wire has ONE @returns@ schema and a
    -- @streaming@ boolean, which is strictly less information:
    --
    --   * not streaming — @returns@ is the terminal, and §6.3's "a method that
    --     emits no updates MUST declare no update type" is satisfied exactly.
    --   * streaming — @returns@ describes the update events. What the terminal
    --     is cannot be recovered from this wire, so it is 'C.Unresolved' with
    --     the reason. §6.4 forbids substituting a unit type here, and forbids
    --     collapsing it to 'C.NotDeclared', which would assert the method has
    --     no meaningful final value — a claim the wire does not support.
    (updates, terminal) = case W.methodReturns m of
      Nothing -> (C.NotDeclared, C.NotDeclared)
      Just schema
        | W.methodStreaming m ->
            ( C.Declared (returnsRef schema)
            , C.Unresolved
                "legacy PluginSchema declares a single 'returns' schema for a \
                \streaming method; §6.3's terminal type is not recoverable from \
                \this wire (see PLX-94/M2)"
            )
        | otherwise -> (C.NotDeclared, C.Declared (returnsRef schema))

    -- §10.3 — "a type reference is a canonical name plus a schema". The legacy
    -- wire supplies the schema half only. The name half is spelled as the empty
    -- 'C.TypeName' rather than guessed, so a consumer that needs a canonical
    -- name can see that it does not have one.
    returnsRef schema = C.TypeRef {C.trTypeName = C.TypeName "", C.trSchema = schema}

--------------------------------------------------------------------------------
-- Activations
--------------------------------------------------------------------------------

-- | Lift one activation.
--
-- Children become __Dynamic__ edges, which is exactly what they are: §5.1 says
-- a Dynamic edge "MUST carry the child's namespace and its advertised hash,
-- sufficient to fetch and cache it lazily", and a legacy 'W.ChildSummary' is a
-- namespace, a description and a hash, fetched lazily on descent. No Static
-- edge is ever produced, because nothing is embedded on this wire, and no
-- Indexed edge is ever produced, because the wire carries no path template, no
-- id field and no template subtree (§5.1's five required facts). Synthesizing
-- either would be the §5.2 violation.
liftActivation :: W.PluginSchema -> C.Activation
liftActivation ps =
  C.Activation
    { C.aNamespace = C.Namespace (W.psNamespace ps)
    , C.aVersion = C.Version (W.psVersion ps)
    , C.aHash = C.Hash (W.psHash ps)
    , C.aMethods = map (liftMethod (W.psNamespace ps)) (W.psMethods ps)
    , C.aChildren = map liftChild (concat (W.psChildren ps))
    , C.aDescription = nonEmpty (W.psDescription ps)
    , C.aLongDescription = W.psLongDescription ps >>= nonEmpty
    , -- §6.9 — @psRequest@ (REQ-4/REQ-5) is the activation's PlexusRequest
      -- struct: what the transport supplies to every method here, as opposed to
      -- what the caller sends. That is precisely §6.9's request context.
      C.aRequestContext = case W.psRequest ps of
        Nothing -> C.NotDeclared
        Just schema ->
          C.Declared C.TypeRef {C.trTypeName = C.TypeName "PlexusRequest", C.trSchema = schema}
    , C.aDeprecation = liftDeprecation <$> W.psDeprecation ps
    }
  where
    liftChild c =
      C.EDynamic
        C.DynamicEdge
          { C.dyNamespace = C.Namespace (W.csNamespace c)
          , C.dyAdvertisedHash = C.Hash (W.csHash c)
          , C.dyDescription = nonEmpty (W.csDescription c)
          }

-- | Lift a root activation into a whole document.
--
-- The backend name is supplied by the caller because synapse knows it (it is
-- how the connection was opened) and the legacy wire does not carry it. The
-- remaining §3.3 root facts — a declared IR version, a reply-channel method
-- name — and §4.7's hash-construction identifier genuinely do not exist on this
-- wire, so they are absent and the checker reports them. See 'legacyWireGaps'.
liftConnectome :: Text -> W.PluginSchema -> C.Connectome
liftConnectome backend root =
  C.Connectome
    { C.cRootFacts =
        C.RootFacts
          { C.rfHashAlgorithm = Nothing
          , C.rfBackendName = Just (C.BackendName backend)
          , C.rfDocumentHash = C.Hash (W.psHash root)
          , C.rfIrVersion = Nothing
          , C.rfRespondMethod = Nothing
          }
    , C.cRoot = liftActivation root
    }

--------------------------------------------------------------------------------
-- The measured distance between the legacy wire and RFC 002
--------------------------------------------------------------------------------

-- | Whether a gap is visible to "Plexus.Connectome.Check".
--
-- The distinction is the whole value of this list. A 'Reported' gap is
-- self-announcing: the checker returns a MUST violation and nobody can ship
-- past it unaware. A 'Silent' gap is the dangerous kind — the lifted document
-- is __conformant__ and the checker says nothing, because the wire's loss shows
-- up as legally-absent information rather than as an illegal document. §5.2's
-- warning is exactly about this class: the failure mode is a consumer assuming
-- structure that was never there.
data GapVisibility
  = -- | 'Plexus.Connectome.Check.checkConnectome' reports this clause as a
    -- MUST violation on a lifted document.
    Reported
  | -- | The checker cannot see this. The document passes; the information is
    -- gone anyway.
    Silent
  deriving stock (Eq, Show)

-- | One thing RFC 002 requires that the legacy @PluginSchema@ wire cannot
-- express. This is a finding list, not a TODO list: each entry is closed by
-- __the server emitting a Connectome__ (M2 / PLX-94), not by more work in
-- synapse.
data LegacyWireGap = LegacyWireGap
  { -- | The RFC clause the wire cannot satisfy, e.g. @"4.7"@.
    gapClause :: !Text
  , gapVisibility :: !GapVisibility
  , -- | What is missing, in the clause's own terms.
    gapDetail :: !Text
  }
  deriving stock (Eq, Show)

-- | The pinned list. Tests assert both halves: that the checker's MUST
-- violations on a lifted document are exactly the 'Reported' clauses, and that
-- the 'Silent' ones really are silent. Either a gap closing or a new one
-- opening therefore shows up as a test failure rather than as silence.
legacyWireGaps :: [LegacyWireGap]
legacyWireGaps =
  [ LegacyWireGap
      "3.3"
      Reported
      "the wire carries no declared IR format version; §3.3 makes it a \
      \mandatory root fact. (No reply-channel method name either, which §7.6 \
      \raises separately as a SHOULD.)"
  , LegacyWireGap
      "4.7"
      Reported
      "the wire declares no hash construction identifier, so 'hashed \
      \differently' cannot be distinguished from 'changed'"
  , LegacyWireGap
      "5.1"
      Silent
      "every child is a summary fetched lazily, so only Dynamic edges are \
      \expressible: no Static child is embedded, and no Indexed edge's \
      \list method / id field / path template / template subtree exists. A \
      \document of nothing but Dynamic edges is perfectly conformant, which is \
      \why this is invisible — and it is why M3·C (PLX-122) has no Indexed \
      \edge to descend."
  , LegacyWireGap
      "6.3"
      Silent
      "one 'returns' schema plus a streaming flag stands in for the separate \
      \update and terminal declarations. 'Unresolved' is a conformant thing for \
      \a terminal to be (§6.4), so the checker passes it; the information is \
      \still unrecoverable."
  , LegacyWireGap
      "10.3"
      Silent
      "type references carry a schema but no canonical name. §10.3 explicitly \
      \leaves name resolution to the consumer, so an empty name is not a \
      \violation — but it means parameters cannot be lifted as §10.3 type \
      \references at all, which is why 'mParams' is empty."
  ]
