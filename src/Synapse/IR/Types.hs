{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Intermediate Representation for code generation
--
-- The IR is a deduplicated, structured representation of the full
-- plugin graph, optimized for transpilers to consume.
--
-- = Design
--
-- Schema (per-plugin, self-contained, may duplicate types)
--     ↓ walk + transform
-- IR (global, deduplicated, compiler-ready)
--     ↓ transpilers
-- TypeScript / Python / Swift / etc.
--
-- = Structure
--
-- - Types are hoisted to top level and deduplicated by name
-- - Methods reference types by name, not inline definitions
-- - A method's contract is read from its schema, never inferred: its item
--   type ('mdReturns'), error type ('mdError'), progress type
--   ('mdProgress') and cardinality ('mdCardinality') — IR 3.0, plexus 0.7
-- - Discriminated unions are first-class
module Synapse.IR.Types
  ( -- * Top-level IR
    IR(..)
  , emptyIR
  , mergeIR

    -- * Version Information
  , synapseVersion

    -- * Generation Metadata
  , GeneratorInfo(..)
  , GenerationMetadata(..)

    -- * Plugin Hash Information
  , PluginHashInfo(..)

    -- * Type Definitions
  , TypeDef(..)
  , tdFullName
  , TypeKind(..)
  , FieldDef(..)
  , VariantDef(..)

    -- * Method Definitions
  , MethodDef(..)
  , MethodRole(..)
  , Cardinality(..)
  , ParamDef(..)
  , InjectedParam(..)

    -- * IR 2.1 (SYNAPSE-CC-8)
  , Refusal(..)
  , Refinements(..)
  , noRefinements
  , isNoRefinements
  , EnumRepr(..)
  , currentIrVersion

    -- * Type References
  , QualifiedName(..)
  , qualifiedNameFull
  , TypeRef(..)
  , typeRefName
  , typeRefChildren
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Char (toLower)
import Data.Scientific (Scientific)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import GHC.Generics (Generic)

import Paths_plexus_synapse (version)
import Plexus.Schema.Recursive (Cardinality(..), DeprecationInfo(..), MethodRole(..))

-- ============================================================================
-- Version Information
-- ============================================================================

-- | Synapse's own package version (@plexus-synapse.cabal@'s @version@), as
--   recorded in 'GenerationMetadata'.
synapseVersion :: Text
synapseVersion = T.pack (showVersion version)

-- ============================================================================
-- Generation Metadata
-- ============================================================================

-- | Generator tool version information
data GeneratorInfo = GeneratorInfo
  { giTool    :: Text  -- ^ Tool name (e.g., "synapse", "synapse-cc")
  , giVersion :: Text  -- ^ Version string (e.g., "0.2.0.0")
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Generation metadata tracking the full toolchain
data GenerationMetadata = GenerationMetadata
  { gmGenerators :: [GeneratorInfo]  -- ^ All tools in the generation chain
  , gmTimestamp  :: Text             -- ^ ISO 8601 timestamp of generation
  , gmIrVersion  :: Text             -- ^ IR format version
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ============================================================================
-- Plugin Hash Information
-- ============================================================================

-- | V2 hash information for a plugin
-- Supports granular cache invalidation by tracking:
-- - Composite hash (backward compatible with V1)
-- - Self hash (methods-only, for detecting method changes)
-- - Children hash (children-only, for detecting dependency changes)
data PluginHashInfo = PluginHashInfo
  { phiHash         :: Text   -- ^ Composite hash (backward compatible)
  , phiSelfHash     :: Text   -- ^ V2: Methods-only hash
  , phiChildrenHash :: Text   -- ^ V2: Children-only hash
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ============================================================================
-- Top-level IR
-- ============================================================================

-- | The complete IR for code generation
data IR = IR
  { irVersion      :: Text                         -- ^ IR format version
  , irBackend      :: Text                         -- ^ Backend name (e.g., "substrate", "plexus")
  , irHash         :: Maybe Text                   -- ^ Plexus hash for versioning
  , irMetadata     :: Maybe GenerationMetadata     -- ^ Generation toolchain metadata
  , irTypes        :: Map Text TypeDef             -- ^ All types, deduplicated by name
  , irMethods      :: Map Text MethodDef           -- ^ All methods, keyed by full path (e.g., "cone.chat")
  , irPlugins      :: Map Text [Text]              -- ^ Plugin -> method names mapping
  , irPluginHashes :: Maybe (Map Text PluginHashInfo)  -- ^ V2: Hash information per plugin
  , irPluginRequests :: Maybe (Map Text Aeson.Value)   -- ^ REQ-5: per-plugin PlexusRequest JSON Schema (Nothing = no request struct)
  , irPluginRoles  :: Maybe (Map Text (Map Text [Text]))
    -- ^ AUTH-9: per-plugin default roles (AUTH-3's @PluginSchema.roles@),
    --   plugin namespace → role → the grants it lists. Omitted from the JSON
    --   when 'Nothing', so an IR from a pre-AUTH schema is unchanged.
  , irRefusals     :: [Refusal]
    -- ^ SYNAPSE-CC-8 (IR 2.1): every site the builder could not type, each
    --   named. Omitted from the JSON when empty. Non-empty: @synapse -i@
    --   exits 65 and no generator may run on the IR.
  }
  deriving stock (Show, Eq, Generic)

-- | Generic, except that @irPluginRoles@ is left out when 'Nothing' (AUTH-9)
--   and @irRefusals@ when empty (SYNAPSE-CC-8): an IR built from a schema
--   without them serializes exactly as before.
instance ToJSON IR where
  toJSON ir = case genericToJSON defaultOptions ir of
    Object o -> Object $ dropRefusals $ case irPluginRoles ir of
      Nothing -> KM.delete "irPluginRoles" o
      Just _ -> o
    v -> v
    where
      dropRefusals o
        | null (irRefusals ir) = KM.delete "irRefusals" o
        | otherwise = o

instance FromJSON IR where
  parseJSON = withObject "IR" $ \o -> IR
    <$> o .:  "irVersion"
    <*> o .:  "irBackend"
    <*> o .:? "irHash"
    <*> o .:? "irMetadata"
    <*> o .:  "irTypes"
    <*> o .:  "irMethods"
    <*> o .:  "irPlugins"
    <*> o .:? "irPluginHashes"
    <*> o .:? "irPluginRequests"
    <*> o .:? "irPluginRoles"
    <*> o .:? "irRefusals" .!= []

-- | The IR format this synapse writes: 3.1 (ASK-3, as plexus-core's ASK-2
--   writes it: each method's @mdAsks@ in place of 3.0's @mdBidir*@) over 3.0
--   (RETURNS-13: a method's returns, error, progress and cardinality, read
--   from the plexus 0.7 schema) and 2.1's forms (SYNAPSE-CC-8). A cache holding any other is discarded and
--   built again.
currentIrVersion :: Text
currentIrVersion = "3.1"

-- ============================================================================
-- IR 2.1: refusals, injected params, refinements
-- ============================================================================

-- | SYNAPSE-CC-8: a site the builder could not type. Nothing was dropped: the
--   site is named here, and where it stood the IR says 'RefUnknown' (or the
--   type is left out and its uses say 'RefUnknown').
--
--   Sites are written as service-codegen's report and PYTHON-3's walk write them:
--   @<ns.Type>[.<variant>].<field>@ · @<method path>(<param>)@ ·
--   @<method path> returns@, with the suffixes @[]@ (element), @?@
--   (optional), @{}@ (map value) and @.N@ (tuple element).
data Refusal = Refusal
  { rfSite    :: Text        -- ^ where, e.g. @tickets.Reply@ or @.call(params)@
  , rfKeyword :: Text        -- ^ the keyword it could not read: @oneOf@, @anyOf@, @returns@, @type@, @items@, @allOf@, …
  , rfVariant :: Maybe Int   -- ^ for a union, the index of the first member it could not read
  , rfDetail  :: Text        -- ^ why
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | SYNAPSE-CC-8: a parameter the server supplies (its @x-plexus-source@ is
--   anything but @rpc@) — not a parameter a client sends, so not in
--   'mdParams'. The source is kept verbatim.
data InjectedParam = InjectedParam
  { ipName        :: Text
  , ipSource      :: Value          -- ^ e.g. @{"from": "auth", "resolver": "self.access.verify"}@
  , ipDescription :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | SYNAPSE-CC-8: the bounds of a 'RefRefined' — only those the base's
--   format does not already imply. Each is left out of the JSON when absent.
data Refinements = Refinements
  { rnMinimum          :: Maybe Scientific   -- ^ @minimum@
  , rnMaximum          :: Maybe Scientific   -- ^ @maximum@
  , rnExclusiveMinimum :: Maybe Scientific   -- ^ @exclusiveMinimum@
  , rnExclusiveMaximum :: Maybe Scientific   -- ^ @exclusiveMaximum@
  , rnMinLength        :: Maybe Int          -- ^ @minLength@
  , rnMaxLength        :: Maybe Int          -- ^ @maxLength@
  , rnPattern          :: Maybe Text         -- ^ @pattern@
  , rnMinItems         :: Maybe Int          -- ^ @minItems@
  , rnMaxItems         :: Maybe Int          -- ^ @maxItems@
  }
  deriving stock (Show, Eq, Ord, Generic)

-- | The JSON Schema keyword each field is: @rnMinimum@ is @minimum@.
refinementOptions :: Options
refinementOptions = defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = \f -> case drop 2 f of
      (c : rest) -> toLower c : rest
      [] -> f
  }

instance ToJSON Refinements where
  toJSON = genericToJSON refinementOptions

instance FromJSON Refinements where
  parseJSON = genericParseJSON refinementOptions

noRefinements :: Refinements
noRefinements = Refinements Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

isNoRefinements :: Refinements -> Bool
isNoRefinements = (== noRefinements)

-- | SYNAPSE-CC-8: how an enum's variants are told apart on the wire.
--   'Internal' (the tag is a property of each variant, named by
--   @keDiscriminator@) is the default and is left out of the JSON.
data EnumRepr
  = Internal    -- ^ @{"<tag>": "variant", …fields}@
  | External    -- ^ @{"variant": {…fields}}@ (serde's default); @keDiscriminator@ is @""@
  | Untagged    -- ^ the variant's own shape (serde's untagged); each variant names its type in @vdRef@
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON EnumRepr where
  toJSON = \case
    Internal -> String "internal"
    External -> String "external"
    Untagged -> String "untagged"

instance FromJSON EnumRepr where
  parseJSON = withText "EnumRepr" $ \case
    "internal" -> pure Internal
    "external" -> pure External
    "untagged" -> pure Untagged
    other -> fail ("unknown keRepr: " <> T.unpack other)
  omittedField = Just Internal

-- | Empty IR for folding
emptyIR :: IR
emptyIR = IR
  { irVersion = currentIrVersion  -- 3.1 (ASK-3): 3.0 with mdAsks in place of mdBidir*; 3.0: MethodDef carries returns · error · progress · cardinality (plexus 0.7); 2.1's SYNAPSE-CC-8 forms kept
  , irBackend = ""  -- Will be set by buildIR
  , irHash = Nothing
  , irMetadata = Nothing
  , irTypes = Map.empty
  , irMethods = Map.empty
  , irPlugins = Map.empty
  , irPluginHashes = Nothing
  , irPluginRequests = Nothing
  , irPluginRoles = Nothing
  , irRefusals = []
  }

-- | Merge two IRs (for combining results from tree walk)
mergeIR :: IR -> IR -> IR
mergeIR a b = IR
  { irVersion = irVersion a
  , irBackend = irBackend a  -- Take from left (parent)
  , irHash = irHash a <|> irHash b  -- Take first available hash
  , irMetadata = irMetadata a <|> irMetadata b  -- Take first available metadata
  , irTypes = Map.union (irTypes a) (irTypes b)  -- Left-biased, first definition wins
  , irMethods = Map.union (irMethods a) (irMethods b)
  , irPlugins = Map.unionWith (++) (irPlugins a) (irPlugins b)
  , irPluginHashes = case (irPluginHashes a, irPluginHashes b) of
      (Just ha, Just hb) -> Just (Map.union ha hb)  -- Merge hash maps
      (Just ha, Nothing) -> Just ha
      (Nothing, Just hb) -> Just hb
      (Nothing, Nothing) -> Nothing
  , irPluginRequests = case (irPluginRequests a, irPluginRequests b) of
      (Just ra, Just rb) -> Just (Map.union ra rb)
      (Just ra, Nothing) -> Just ra
      (Nothing, Just rb) -> Just rb
      (Nothing, Nothing) -> Nothing
  , irPluginRoles = case (irPluginRoles a, irPluginRoles b) of
      (Just ra, Just rb) -> Just (Map.union ra rb)
      (Just ra, Nothing) -> Just ra
      (Nothing, Just rb) -> Just rb
      (Nothing, Nothing) -> Nothing
  , irRefusals = irRefusals a <> irRefusals b
  }

-- ============================================================================
-- Type Definitions
-- ============================================================================

-- | A type definition extracted from schemas
data TypeDef = TypeDef
  { tdName        :: Text           -- ^ Type name (e.g., "ListResult", "ChatEvent")
  , tdNamespace   :: Text           -- ^ Namespace (e.g., "cone", "arbor")
  , tdDescription :: Maybe Text     -- ^ Documentation
  , tdKind        :: TypeKind       -- ^ What kind of type this is
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Compute the fully qualified type name
tdFullName :: TypeDef -> Text
tdFullName td = tdNamespace td <> "." <> tdName td

-- | The kind of a type definition
data TypeKind
  = KindStruct
      { ksFields :: [FieldDef]       -- ^ Struct fields
      }
  | KindEnum
      { keDiscriminator :: Text      -- ^ The tag: the property whose @const@ every variant carries (e.g., "type", "kind"); @""@ when not 'Internal'
      , keVariants :: [VariantDef]   -- ^ Possible variants
      , keRepr :: EnumRepr           -- ^ SYNAPSE-CC-8: internal (omitted), external or untagged
      }
  | KindStringEnum
      { kseValues :: [Text]          -- ^ String literal values (e.g., ["pending", "completed"])
      }
  | KindAlias
      { kaTarget :: TypeRef          -- ^ What this aliases to
      }
  | KindPrimitive
      { kpType :: Text               -- ^ "string", "integer", "boolean", etc.
      , kpFormat :: Maybe Text       -- ^ Optional format hint (e.g., "uuid", "int64")
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Generic, except that @keRepr@ is left out when 'Internal' (SYNAPSE-CC-8):
--   an enum IR 2.0 already carried serializes exactly as before.
instance ToJSON TypeKind where
  toJSON k = case genericToJSON defaultOptions k of
    Object o | KM.lookup "keRepr" o == Just (String "internal") -> Object (KM.delete "keRepr" o)
    v -> v

-- | A field in a struct
data FieldDef = FieldDef
  { fdName        :: Text           -- ^ Field name
  , fdType        :: TypeRef        -- ^ Field type
  , fdDescription :: Maybe Text     -- ^ Documentation
  , fdRequired    :: Bool           -- ^ Is this field required?
  , fdDefault     :: Maybe Value    -- ^ Default value if any
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A variant in a discriminated union
data VariantDef = VariantDef
  { vdName        :: Text           -- ^ Variant name (the discriminator value)
  , vdDescription :: Maybe Text     -- ^ Documentation
  , vdFields      :: [FieldDef]     -- ^ The variant's fields, the tag aside; a newtype variant over a struct carries the struct's fields
  , vdRef         :: Maybe TypeRef  -- ^ SYNAPSE-CC-8: an untagged variant's type (omitted when absent)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

instance ToJSON VariantDef where
  toJSON v = case genericToJSON defaultOptions v of
    Object o | vdRef v == Nothing -> Object (KM.delete "vdRef" o)
    j -> j

-- ============================================================================
-- Method Definitions
-- ============================================================================

-- | A method definition
--
--   IR 3.0: a method's contract is four facts read from its 0.7
--   'Plexus.Schema.Recursive.MethodSchema', never inferred — its item type
--   ('mdReturns'), its error type when it can say no ('mdError'), its
--   progress type when it reports progress ('mdProgress'), and whether it
--   yields one item or many ('mdCardinality'). Every 'RefNamed' among them
--   has a 'TypeDef' in 'irTypes'.
data MethodDef = MethodDef
  { mdName        :: Text           -- ^ Method name (e.g., "chat")
  , mdFullPath    :: Text           -- ^ Full path (e.g., "cone.chat")
  , mdNamespace   :: Text           -- ^ Parent namespace (e.g., "cone")
  , mdDescription :: Maybe Text     -- ^ Documentation
  , mdParams      :: [ParamDef]     -- ^ Input parameters
  , mdReturns     :: TypeRef
    -- ^ The item type @T@: what one success carries (for @Result<T, E>@ or
    --   @Progress<T, P>@, @T@ alone). From @MethodSchema.returns@.
  , mdError       :: Maybe TypeRef
    -- ^ The error type @E@ of a @Result<T, E>@ item — what the activation
    --   says when it says no. 'Nothing' when the method is infallible.
    --   From @MethodSchema.error@.
  , mdProgress    :: Maybe TypeRef
    -- ^ The progress type @P@ its progress frames carry. 'Nothing' when the
    --   method reports no progress. From @MethodSchema.progress@.
  , mdCardinality :: Cardinality
    -- ^ One item or many. From @MethodSchema.cardinality@ — never inferred
    --   from the shape of 'mdReturns'.
  , mdAsks        :: Bool
    -- ^ ASK-1: the method may finish a call with a proposal (a data item
    --   @{"type":"needs_input", …}@) and takes @confirm@, the token that
    --   answers it. From @MethodSchema.asks@; an older schema's
    --   @bidirectional@ reads as 'False'. IR 3.1 (as plexus-core writes it,
    --   ASK-2): always written; a 3.0 IR's absent one reads as 'False' and
    --   its @mdBidirType@ / @mdBidirResponseType@ / @mdBidirResponseSchema@
    --   are read past.
  , mdRole        :: MethodRole
    -- ^ Structural role of this method in the activation graph
    -- (IR-12). Mirrors 'Plexus.Schema.Recursive.MethodRole' /
    -- 'plexus_core::MethodRole'. Defaults to 'MethodRoleRpc' when the
    -- upstream schema omits @role@ (pre-IR servers).
    --
    -- Consumed by service-codegen (Rust field @md_role@, JSON key @mdRole@)
    -- to emit typed-handle clients for @DynamicChild@ methods.
  , mdGrant       :: Maybe Text
    -- ^ AUTH-9: the method's grant (AUTH-3's @MethodSchema.grant@),
    --   @<activation>.<method>@, e.g. @tickets.set_status@.
  , mdTarget      :: Maybe Text
    -- ^ AUTH-9: the param that locates what a call acts on (@MethodSchema.target@).
  , mdLongDescription :: Maybe Text
    -- ^ AUTH-9: the doc comment beside the short description
    --   (@MethodSchema.long_description@).
  , mdInjected    :: [InjectedParam]
    -- ^ SYNAPSE-CC-8: the params the server supplies (@x-plexus-source@
    --   other than rpc), out of 'mdParams'. Omitted from the JSON when empty.
  , mdInput       :: Maybe TypeRef
    -- ^ SYNAPSE-CC-8, reserved for BIDIR-6: @MethodSchema.input@ as a type.
    --   Omitted from the JSON when absent; no server sends it yet.
  }
  deriving stock (Show, Eq, Generic)

-- | Manual JSON instances so @mdRole@ defaults to 'MethodRoleRpc' when
--   absent (same posture as the Rust @#[serde(default)]@ on
--   @MethodDef.md_role@ in service-codegen).
--
--   Field names are the JSON keys; a 'Nothing' is written as @null@, as
--   the generic derivation always has. @mdCardinality@ is @"one"@ /
--   @"many"@ (the schema's own spelling) and is required: IR 3.0 does not
--   read 2.0's @mdStreaming@ (RETURNS-1 decision 12, no back-compat).
instance ToJSON MethodDef where
  toJSON MethodDef{..} = object $
    [ "mdName"                .= mdName
    , "mdFullPath"            .= mdFullPath
    , "mdNamespace"           .= mdNamespace
    , "mdDescription"         .= mdDescription
    , "mdParams"              .= mdParams
    , "mdReturns"             .= mdReturns
    , "mdError"               .= mdError
    , "mdProgress"            .= mdProgress
    , "mdCardinality"         .= mdCardinality
    , "mdAsks"                .= mdAsks
    , "mdRole"                .= mdRole
    ] <> catMaybes
    -- AUTH-9: present only when the schema says them, so a pre-AUTH
    -- schema's IR is unchanged.
    [ ("mdGrant" .=) <$> mdGrant
    , ("mdTarget" .=) <$> mdTarget
    , ("mdLongDescription" .=) <$> mdLongDescription
      -- SYNAPSE-CC-8: present only when there are any
    , if null mdInjected then Nothing else Just ("mdInjected" .= mdInjected)
    , ("mdInput" .=) <$> mdInput
    ]

instance FromJSON MethodDef where
  parseJSON = withObject "MethodDef" $ \o -> MethodDef
    <$> o .:  "mdName"
    <*> o .:  "mdFullPath"
    <*> o .:  "mdNamespace"
    <*> o .:? "mdDescription"
    <*> o .:  "mdParams"
    <*> o .:  "mdReturns"
    <*> o .:? "mdError"
    <*> o .:? "mdProgress"
    <*> o .:  "mdCardinality"
    <*> o .:? "mdAsks" .!= False
    <*> o .:? "mdRole" .!= MethodRoleRpc
    <*> o .:? "mdGrant"
    <*> o .:? "mdTarget"
    <*> o .:? "mdLongDescription"
    <*> o .:? "mdInjected" .!= []
    <*> o .:? "mdInput"

-- | A parameter definition
data ParamDef = ParamDef
  { pdName        :: Text                    -- ^ Parameter name
  , pdType        :: TypeRef                 -- ^ Parameter type
  , pdDescription :: Maybe Text              -- ^ Documentation
  , pdRequired    :: Bool                    -- ^ Is this required?
  , pdDefault     :: Maybe Value             -- ^ Default value if any
  , pdDeprecation :: Maybe DeprecationInfo
    -- ^ Per-parameter deprecation info (IR-14).
    --   Populated from 'Plexus.Schema.Recursive.ParamSchema.paramDeprecation'
    --   when the upstream schema advertises it (via @param_schemas@).
    --   Defaults to 'Nothing' when the schema lacks @param_schemas@, so
    --   pre-IR-5 producers and non-deprecated parameters are indistinguishable
    --   at the rendering layer.
  , pdSource      :: Maybe Value
    -- ^ REQ-6/REQ-9: @x-plexus-source@ extension annotation, when the wire
    --   schema declared one on this parameter. The value is the raw JSON
    --   object (e.g. @{ "from": "auth", "resolver": "self.db.validate_user" }@
    --   or @{ "from": "cookie", "key": "access_token" }@). Consumers (synapse
    --   renderer, service-codegen JSDoc) read this to identify where each param
    --   is sourced from. @Nothing@ means the schema lacked the extension —
    --   treat as RPC-sourced.
  , pdValues      :: Maybe [Text]
    -- ^ SYNAPSE-17: the values the parameter takes, as its schema hints them
    --   (@x-plexus-values@). Omitted from the JSON when 'Nothing', so an IR
    --   from a schema without the hint serializes exactly as before.
  }
  deriving stock (Show, Eq, Generic)

-- | Manual JSON instances so @pdDeprecation@ defaults to 'Nothing' when
--   deserializing IR JSON that predates IR-14.
--   All other fields preserve the derivation defaults.
instance ToJSON ParamDef where
  toJSON ParamDef{..} = object $
    [ "pdName"        .= pdName
    , "pdType"        .= pdType
    , "pdDescription" .= pdDescription
    , "pdRequired"    .= pdRequired
    , "pdDefault"     .= pdDefault
    , "pdDeprecation" .= pdDeprecation
    , "pdSource"      .= pdSource
    ]
    ++ maybe [] (\vs -> ["pdValues" .= vs]) pdValues

instance FromJSON ParamDef where
  parseJSON = withObject "ParamDef" $ \o -> ParamDef
    <$> o .:  "pdName"
    <*> o .:  "pdType"
    <*> o .:? "pdDescription"
    <*> o .:  "pdRequired"
    <*> o .:? "pdDefault"
    <*> o .:? "pdDeprecation"
    <*> o .:? "pdSource"
    <*> o .:? "pdValues"

-- ============================================================================
-- Type References
-- ============================================================================

-- | A qualified name for a type, with namespace and local name
data QualifiedName = QualifiedName
  { qnNamespace :: Text  -- ^ Namespace (can be empty for global types)
  , qnLocalName :: Text  -- ^ Local name within namespace
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Get the full qualified name as a single Text
-- Returns just the local name if namespace is empty, otherwise "namespace.localName"
qualifiedNameFull :: QualifiedName -> Text
qualifiedNameFull qn
  | T.null (qnNamespace qn) = qnLocalName qn
  | otherwise = qnNamespace qn <> "." <> qnLocalName qn

-- | A reference to a type
data TypeRef
  = RefNamed QualifiedName          -- ^ Reference to a named type (e.g., QualifiedName "cone" "UUID")
  | RefPrimitive Text (Maybe Text)  -- ^ Primitive type with optional format
  | RefArray TypeRef                -- ^ Array of some type
  | RefOptional TypeRef             -- ^ Optional (nullable) type
  | RefJson                         -- ^ SYNAPSE-CC-8: the JSON type (a @serde_json::Value@): any JSON value, typed — @JsonValue@ / @Json@ / @serde_json::Value@
  | RefUnknown                      -- ^ Not typed: only at a site 'irRefusals' names (SYNAPSE-CC-8)
  | RefMap TypeRef                  -- ^ SYNAPSE-CC-8: an object with @additionalProperties: V@ and no properties; keys are strings
  | RefTuple [TypeRef]              -- ^ SYNAPSE-CC-8: @prefixItems@ with @minItems = maxItems@ = its length
  | RefRefined TypeRef Refinements  -- ^ SYNAPSE-CC-8: a bound the base's format does not imply
  | RefChild Text                   -- ^ SYNAPSE-CC-8: a static or dynamic child's return: that child's client, by its full path
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Generic, except that an IR older than 2.1 wrote a @serde_json::Value@
--   as @{"tag": "RefAny"}@: it reads as 'RefJson'.
instance FromJSON TypeRef where
  parseJSON v = genericParseJSON defaultOptions v <|> legacyAny v
    where
      legacyAny = withObject "TypeRef" $ \o -> case (KM.lookup "tag" o, KM.member "contents" o) of
        (Just (String "RefAny"), _) -> pure RefJson
        _ -> fail "not a TypeRef"

-- | The refs directly inside a ref.
typeRefChildren :: TypeRef -> [TypeRef]
typeRefChildren = \case
  RefArray t -> [t]
  RefOptional t -> [t]
  RefMap t -> [t]
  RefTuple ts -> ts
  RefRefined t _ -> [t]
  _ -> []

-- | Get the name from a type reference (if it's a named ref)
typeRefName :: TypeRef -> Maybe Text
typeRefName (RefNamed qn) = Just (qualifiedNameFull qn)
typeRefName _ = Nothing
