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
-- - Streaming is inferred from return type structure
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
  , ParamDef(..)

    -- * Type References
  , QualifiedName(..)
  , qualifiedNameFull
  , TypeRef(..)
  , typeRefName

    -- * Utilities
  , isStreaming
  , inferStreaming
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Plexus.Schema.Recursive (DeprecationInfo(..), MethodRole(..))

-- ============================================================================
-- Version Information
-- ============================================================================

-- | Synapse version (from cabal file: plexus-synapse.cabal)
synapseVersion :: Text
synapseVersion = "0.2.0.0"

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
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Generic, except that @irPluginRoles@ is left out when 'Nothing' (AUTH-9):
--   an IR built from a schema without roles serializes exactly as before.
instance ToJSON IR where
  toJSON ir = case genericToJSON defaultOptions ir of
    Object o -> Object $ case irPluginRoles ir of
      Nothing -> KM.delete "irPluginRoles" o
      Just _ -> o
    v -> v

-- | Empty IR for folding
emptyIR :: IR
emptyIR = IR
  { irVersion = "2.0"  -- Bumped: TypeRef now uses structured QualifiedName
  , irBackend = ""  -- Will be set by buildIR
  , irHash = Nothing
  , irMetadata = Nothing
  , irTypes = Map.empty
  , irMethods = Map.empty
  , irPlugins = Map.empty
  , irPluginHashes = Nothing
  , irPluginRequests = Nothing
  , irPluginRoles = Nothing
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
      { keDiscriminator :: Text      -- ^ Field that discriminates (e.g., "type")
      , keVariants :: [VariantDef]   -- ^ Possible variants
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
  deriving anyclass (ToJSON, FromJSON)

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
  , vdFields      :: [FieldDef]     -- ^ Fields specific to this variant
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ============================================================================
-- Method Definitions
-- ============================================================================

-- | A method definition
data MethodDef = MethodDef
  { mdName        :: Text           -- ^ Method name (e.g., "chat")
  , mdFullPath    :: Text           -- ^ Full path (e.g., "cone.chat")
  , mdNamespace   :: Text           -- ^ Parent namespace (e.g., "cone")
  , mdDescription :: Maybe Text     -- ^ Documentation
  , mdStreaming   :: Bool           -- ^ Does this method stream multiple events?
  , mdParams      :: [ParamDef]     -- ^ Input parameters
  , mdReturns     :: TypeRef        -- ^ Return type reference
  , mdBidirType   :: Maybe TypeRef
    -- ^ Bidirectional channel type parameter T, when the method uses
    -- BidirChannel<StandardRequest<T>, StandardResponse<T>>.
    --
    -- Populated from the "bidirectional" / "request_type" fields in the
    -- MethodSchema (emitted by the hub-macro when #[bidirectional] is set).
    --
    -- - Nothing  → method is not bidirectional, OR uses the default
    --              T = serde_json::Value (StandardBidirChannel)
    -- - Just RefAny → bidirectional with T=Value (explicit marker)
    -- - Just (RefNamed ...) → bidirectional with a specific named T type
    --
    -- NOTE: The substrate schema currently emits 'bidirectional: true' but
    -- does not yet include a structured 'bidir_type' field.  When that field
    -- is added to MethodSchema, populate it here from methodRequestType.
    -- For now this is always Nothing.
  , mdBidirResponseType :: Maybe TypeRef
    -- ^ Expected response type for bidirectional methods.
    --
    -- Populated from the "response_type" field in the MethodSchema.
    -- This allows agents to know what response format is expected
    -- when they receive a bidirectional request.
  , mdBidirResponseSchema :: Maybe Value
    -- ^ Full JSON Schema for the bidirectional response type.
    --
    -- Cached from MethodSchema.response_type so synapse can include it
    -- in bidir_request output. Synapse controls whether to print this
    -- (e.g., first request, --bidir-schemas flag, etc.)
  , mdRole        :: MethodRole
    -- ^ Structural role of this method in the activation graph
    -- (IR-12). Mirrors 'Plexus.Schema.Recursive.MethodRole' /
    -- 'plexus_core::MethodRole'. Defaults to 'MethodRoleRpc' when the
    -- upstream schema omits @role@ (pre-IR servers).
    --
    -- Consumed by hub-codegen (Rust field @md_role@, JSON key @mdRole@)
    -- to emit typed-handle clients for @DynamicChild@ methods.
  , mdGrant       :: Maybe Text
    -- ^ AUTH-9: the method's grant (AUTH-3's @MethodSchema.grant@),
    --   @<activation>.<method>@, e.g. @tickets.set_status@.
  , mdTarget      :: Maybe Text
    -- ^ AUTH-9: the param that locates what a call acts on (@MethodSchema.target@).
  , mdLongDescription :: Maybe Text
    -- ^ AUTH-9: the doc comment beside the short description
    --   (@MethodSchema.long_description@).
  }
  deriving stock (Show, Eq, Generic)

-- | Manual JSON instances so @mdRole@ defaults to 'MethodRoleRpc' when
--   deserializing pre-IR-12 IR JSON (same back-compat posture as the
--   Rust @#[serde(default)]@ on @MethodDef.md_role@ in hub-codegen).
--
--   All other fields preserve the generic derivation defaults (field
--   names used verbatim as JSON keys; @Maybe@ fields omitted when
--   @Nothing@ to match aeson's legacy behaviour).
instance ToJSON MethodDef where
  toJSON MethodDef{..} = object $
    [ "mdName"                .= mdName
    , "mdFullPath"            .= mdFullPath
    , "mdNamespace"           .= mdNamespace
    , "mdDescription"         .= mdDescription
    , "mdStreaming"           .= mdStreaming
    , "mdParams"              .= mdParams
    , "mdReturns"             .= mdReturns
    , "mdBidirType"           .= mdBidirType
    , "mdBidirResponseType"   .= mdBidirResponseType
    , "mdBidirResponseSchema" .= mdBidirResponseSchema
    , "mdRole"                .= mdRole
    ] <> catMaybes
    -- AUTH-9: present only when the schema says them, so a pre-AUTH
    -- schema's IR is unchanged.
    [ ("mdGrant" .=) <$> mdGrant
    , ("mdTarget" .=) <$> mdTarget
    , ("mdLongDescription" .=) <$> mdLongDescription
    ]

instance FromJSON MethodDef where
  parseJSON = withObject "MethodDef" $ \o -> MethodDef
    <$> o .:  "mdName"
    <*> o .:  "mdFullPath"
    <*> o .:  "mdNamespace"
    <*> o .:? "mdDescription"
    <*> o .:  "mdStreaming"
    <*> o .:  "mdParams"
    <*> o .:  "mdReturns"
    <*> o .:? "mdBidirType"
    <*> o .:? "mdBidirResponseType"
    <*> o .:? "mdBidirResponseSchema"
    <*> o .:? "mdRole" .!= MethodRoleRpc
    <*> o .:? "mdGrant"
    <*> o .:? "mdTarget"
    <*> o .:? "mdLongDescription"

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
    --   renderer, hub-codegen JSDoc) read this to identify where each param
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
  | RefAny                          -- ^ Intentionally dynamic (serde_json::Value) - accepts any JSON
  | RefUnknown                      -- ^ Unknown type (schema gap) - should warn
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Get the name from a type reference (if it's a named ref)
typeRefName :: TypeRef -> Maybe Text
typeRefName (RefNamed qn) = Just (qualifiedNameFull qn)
typeRefName _ = Nothing

-- ============================================================================
-- Utilities
-- ============================================================================

-- | Check if a method definition is streaming
isStreaming :: MethodDef -> Bool
isStreaming = mdStreaming

-- | Infer streaming from a return type's structure
-- A method is streaming if its return type is an enum with more than 2 variants
-- (more than 1 success variant + error variant)
inferStreaming :: TypeDef -> Bool
inferStreaming td = case tdKind td of
  KindEnum _ variants ->
    let nonErrorVariants = filter (not . isErrorVariant) variants
    in length nonErrorVariants > 1
  _ -> False
  where
    isErrorVariant v = vdName v == "error"
