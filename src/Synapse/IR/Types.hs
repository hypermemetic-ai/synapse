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

    -- * Type Definitions
  , TypeDef(..)
  , tdFullName
  , TypeKind(..)
  , FieldDef(..)
  , VariantDef(..)

    -- * Method Definitions
  , MethodDef(..)
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- ============================================================================
-- Top-level IR
-- ============================================================================

-- | The complete IR for code generation
data IR = IR
  { irVersion   :: Text                    -- ^ IR format version
  , irBackend   :: Text                    -- ^ Backend name (e.g., "substrate", "plexus")
  , irHash      :: Maybe Text              -- ^ Plexus hash for versioning
  , irTypes     :: Map Text TypeDef        -- ^ All types, deduplicated by name
  , irMethods   :: Map Text MethodDef      -- ^ All methods, keyed by full path (e.g., "cone.chat")
  , irPlugins   :: Map Text [Text]         -- ^ Plugin -> method names mapping
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Empty IR for folding
emptyIR :: IR
emptyIR = IR
  { irVersion = "2.0"  -- Bumped: TypeRef now uses structured QualifiedName
  , irBackend = ""  -- Will be set by buildIR
  , irHash = Nothing
  , irTypes = Map.empty
  , irMethods = Map.empty
  , irPlugins = Map.empty
  }

-- | Merge two IRs (for combining results from tree walk)
mergeIR :: IR -> IR -> IR
mergeIR a b = IR
  { irVersion = irVersion a
  , irBackend = irBackend a  -- Take from left (parent)
  , irHash = irHash a <|> irHash b  -- Take first available hash
  , irTypes = Map.union (irTypes a) (irTypes b)  -- Left-biased, first definition wins
  , irMethods = Map.union (irMethods a) (irMethods b)
  , irPlugins = Map.unionWith (++) (irPlugins a) (irPlugins b)
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
  deriving anyclass (ToJSON)

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
  deriving anyclass (ToJSON)

-- | A field in a struct
data FieldDef = FieldDef
  { fdName        :: Text           -- ^ Field name
  , fdType        :: TypeRef        -- ^ Field type
  , fdDescription :: Maybe Text     -- ^ Documentation
  , fdRequired    :: Bool           -- ^ Is this field required?
  , fdDefault     :: Maybe Value    -- ^ Default value if any
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | A variant in a discriminated union
data VariantDef = VariantDef
  { vdName        :: Text           -- ^ Variant name (the discriminator value)
  , vdDescription :: Maybe Text     -- ^ Documentation
  , vdFields      :: [FieldDef]     -- ^ Fields specific to this variant
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

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
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | A parameter definition
data ParamDef = ParamDef
  { pdName        :: Text           -- ^ Parameter name
  , pdType        :: TypeRef        -- ^ Parameter type
  , pdDescription :: Maybe Text     -- ^ Documentation
  , pdRequired    :: Bool           -- ^ Is this required?
  , pdDefault     :: Maybe Value    -- ^ Default value if any
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- ============================================================================
-- Type References
-- ============================================================================

-- | A qualified name for a type, with namespace and local name
data QualifiedName = QualifiedName
  { qnNamespace :: Text  -- ^ Namespace (can be empty for global types)
  , qnLocalName :: Text  -- ^ Local name within namespace
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

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
  deriving anyclass (ToJSON)

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
