{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | IR-based help rendering for CLI
--
-- Renders method and parameter help directly from the IR, providing
-- structured type information including discriminated union expansion.
--
-- = Design
--
-- Instead of parsing JSON Schema ad-hoc, we use the pre-built IR:
--
-- @
-- IR (types, methods)
--   |
--   +-> renderMethodHelp  -> full method documentation
--   +-> renderParamHelp   -> parameter with type expansion
--   +-> expandType        -> discriminated union variants
--   +-> renderTypeRef     -> type signature string
-- @
--
-- = Example Output
--
-- @
-- cone.chat - Chat with a cone
--
-- Parameters:
--   --identifier <ConeIdentifier>  (required)
--       Cone name or UUID
--       Either:
--         --identifier.type by_name --identifier.name <string>
--         --identifier.type by_id --identifier.id <uuid>
--
--   --prompt <string>  (required)
--       User message / prompt to send
--
--   --ephemeral <boolean>  (optional)
--       If true, doesn't advance head
-- @
module Synapse.CLI.Help
  ( -- * Method Help
    renderMethodHelp
  , renderMethodHelpWith

    -- * Parameter Help
  , renderParamHelp
  , renderParamHelpWith

    -- * Type Rendering
  , renderTypeRef
  , expandType

    -- * Configuration
  , HelpStyle(..)
  , defaultHelpStyle
  ) where

import Data.List (intersperse)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T

import Synapse.IR.Types
import Synapse.Algebra.Render (deprecationMarker, formatDeprecationLine)
import Synapse.CLI.Parse (isScalarElement)

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Style configuration for help rendering
data HelpStyle = HelpStyle
  { hsIndent      :: !Int       -- ^ Spaces per indent level
  , hsParamWidth  :: !Int       -- ^ Width for parameter column
  , hsShowFormats :: !Bool      -- ^ Show format hints (uuid, int64, etc.)
  , hsExpandEnums :: !Bool      -- ^ Expand enum variants inline
  }
  deriving stock (Show, Eq)

-- | Default help style
defaultHelpStyle :: HelpStyle
defaultHelpStyle = HelpStyle
  { hsIndent      = 2
  , hsParamWidth  = 30
  , hsShowFormats = True
  , hsExpandEnums = True
  }

-- ============================================================================
-- Method Help
-- ============================================================================

-- | Render complete help for a method
renderMethodHelp :: IR -> MethodDef -> Text
renderMethodHelp = renderMethodHelpWith defaultHelpStyle

-- | Render method help with custom style
renderMethodHelpWith :: HelpStyle -> IR -> MethodDef -> Text
renderMethodHelpWith style ir method = T.unlines $
  [ header
  , ""
  ] ++ descSection ++ paramSection
  where
    header = mdFullPath method <> streamingIndicator

    streamingIndicator
      | mdStreaming method = " [streaming]"
      | otherwise = ""

    descSection = case mdDescription method of
      Just desc -> [desc, ""]
      Nothing -> []

    paramSection
      | null (mdParams method) = ["(no parameters)"]
      | otherwise =
          ["Parameters:"] ++
          concatMap (renderParamBlock style ir) (mdParams method)

-- | Render a parameter as a block (may be multi-line for complex types).
--
-- IR-14: when @pdDeprecation param@ is @Just DeprecationInfo@ the
-- warning marker (⚠) is prepended to the parameter's flag name and the
-- formatted @DEPRECATED since … removed in … — …@ line is appended
-- (indented one level below the header). Non-deprecated parameters are
-- rendered exactly as before.
renderParamBlock :: HelpStyle -> IR -> ParamDef -> [Text]
renderParamBlock style ir param =
  let indent' = T.replicate (hsIndent style) " "
      indent2 = T.replicate (hsIndent style * 2) " "

      -- Parameter header line (optionally prefixed with the deprecation
      -- marker; see IR-14). The marker sits before the flag so callers
      -- who grep for '--name' still match a deprecated flag line.
      rawFlag = "--" <> T.replace "_" "-" (pdName param)  -- Display as kebab-case
      flagName = case pdDeprecation param of
        Just _  -> deprecationMarker <> " " <> rawFlag
        Nothing -> rawFlag
      typeStr = renderTypeRef ir (pdType param)
      reqText = if pdRequired param then "(required)" else "(optional)"
      headerLine = indent' <> flagName <> " <" <> typeStr <> ">  " <> reqText

      -- Description line
      descLines = case pdDescription param of
        Just desc -> [indent2 <> desc]
        Nothing -> []

      -- Deprecation detail line (IR-14).  Indented one level beyond the
      -- header to match the description block's indentation.
      deprecationLines = case pdDeprecation param of
        Just di -> [indent2 <> formatDeprecationLine di]
        Nothing -> []

      -- How to pass several elements of a list (SYNAPSE-2)
      listLines = case listElement (pdType param) of
        Just inner
          | isScalarElement ir inner ->
              [indent2 <> "Repeat the flag or use commas: " <> rawFlag <> " a,b"]
          | otherwise -> [indent2 <> "Repeat the flag for each element"]
        Nothing -> []
      listElement = \case
        RefArray inner -> Just inner
        RefOptional inner -> listElement inner
        _ -> Nothing

      -- Type expansion (for enums/unions)
      expansionLines = if hsExpandEnums style
        then map (indent2 <>) (expandType ir (pdName param) (pdType param))
        else []

  in [""] ++ [headerLine] ++ descLines ++ deprecationLines ++ listLines ++ expansionLines

-- ============================================================================
-- Parameter Help
-- ============================================================================

-- | Render help for a single parameter
renderParamHelp :: IR -> ParamDef -> [Text]
renderParamHelp = renderParamHelpWith defaultHelpStyle

-- | Render parameter help with custom style
renderParamHelpWith :: HelpStyle -> IR -> ParamDef -> [Text]
renderParamHelpWith style ir param = renderParamBlock style ir param

-- ============================================================================
-- Type Rendering
-- ============================================================================

-- | Render a type reference as a type signature string
--
-- Examples:
--   RefPrimitive "string" Nothing  -> "string"
--   RefPrimitive "string" (Just "uuid") -> "uuid"
--   RefArray (RefPrimitive "string" Nothing) -> "string[]"
--   RefOptional (RefNamed "Foo") -> "Foo?"
--   RefNamed "ConeIdentifier" -> "ConeIdentifier"
renderTypeRef :: IR -> TypeRef -> Text
renderTypeRef ir = \case
  RefNamed qn ->
    -- If the named type is just a primitive or alias, unwrap it
    -- so newtypes like PaneRef(String) display as <string> not <panes.PaneRef>
    let name = qualifiedNameFull qn
    in case Map.lookup name (irTypes ir) of
      Just TypeDef{tdKind = KindPrimitive t mf}
        | Just fmt <- mf -> fmt
        | otherwise -> t
      Just TypeDef{tdKind = KindAlias target} ->
        renderTypeRef ir target
      _ -> name

  RefPrimitive typ mFormat
    | Just fmt <- mFormat -> fmt  -- Use format as type (uuid, int64, etc.)
    | otherwise -> typ

  RefArray inner -> renderTypeRef ir inner <> "[]"

  RefOptional inner -> renderTypeRef ir inner <> "?"

  RefAny -> "any"

  RefUnknown -> "unknown"

-- | Expand a type into CLI flag examples
--
-- For discriminated unions, shows each variant with its fields.
-- Returns empty list for simple types.
--
-- Example for ConeIdentifier:
-- @
--   ["Either:"
--   ,"  --identifier.type by_name --identifier.name <string>"
--   ,"  --identifier.type by_id --identifier.id <uuid>"
--   ]
-- @
expandType :: IR -> Text -> TypeRef -> [Text]
expandType ir prefix = \case
  RefNamed qn -> expandNamedType ir prefix (qualifiedNameFull qn)
  RefOptional inner -> expandType ir prefix inner
  _ -> []  -- Primitives and arrays don't expand

-- | Expand a named type by looking it up in the IR
expandNamedType :: IR -> Text -> Text -> [Text]
expandNamedType ir prefix typeName =
  case Map.lookup typeName (irTypes ir) of
    Nothing -> []  -- Type not found, no expansion
    Just typeDef -> expandTypeDef ir prefix typeDef

-- | Expand a type definition
expandTypeDef :: IR -> Text -> TypeDef -> [Text]
expandTypeDef ir prefix TypeDef{..} = case tdKind of
  -- Discriminated union: show each variant
  KindEnum discriminator variants
    | length variants > 1 -> expandEnum ir prefix discriminator variants
    | otherwise -> []

  -- Struct: could expand fields, but typically too verbose
  KindStruct _fields -> []

  -- Alias: expand the target
  KindAlias target -> expandType ir prefix target

  -- Primitives don't expand
  KindPrimitive _ _ -> []

  -- String enums don't expand (just string literals)
  KindStringEnum _ -> []

-- | Expand an enum (discriminated union) into variant examples
expandEnum :: IR -> Text -> Text -> [VariantDef] -> [Text]
expandEnum ir prefix discriminator variants =
  ["Either:"] ++ map (("  " <>) . renderVariantExample ir prefix discriminator) variants

-- | Render a single variant as a CLI example
--
-- Example: "--identifier.type by_name --identifier.name <string>"
renderVariantExample :: IR -> Text -> Text -> VariantDef -> Text
renderVariantExample ir prefix discriminator VariantDef{..} =
  let prefix' = T.replace "_" "-" prefix  -- Display as kebab-case
      disc' = T.replace "_" "-" discriminator  -- Display as kebab-case
      discFlag = "--" <> prefix' <> "." <> disc' <> " " <> vdName
      fieldFlags = map (renderFieldFlag ir prefix') vdFields
  in T.intercalate " " (discFlag : fieldFlags)

-- | Render a field as a CLI flag
--
-- Example: "--identifier.name <string>"
renderFieldFlag :: IR -> Text -> FieldDef -> Text
renderFieldFlag ir prefix FieldDef{..} =
  let name' = T.replace "_" "-" fdName  -- Display as kebab-case
  in "--" <> prefix <> "." <> name' <> " <" <> renderTypeRef ir fdType <> ">"

-- ============================================================================
-- String Enum Helpers
-- ============================================================================

-- | Check if a type is a string enum and get its values
getStringEnumValues :: IR -> TypeRef -> Maybe [Text]
getStringEnumValues ir (RefNamed qn) =
  let name = qualifiedNameFull qn
  in case Map.lookup name (irTypes ir) of
    Just TypeDef{tdKind = KindEnum "value" variants} ->
      Just $ map vdName variants
    _ -> Nothing
getStringEnumValues _ _ = Nothing

-- | Render string enum values inline
-- Example: "One of: pending, in_progress, completed"
renderStringEnumHint :: [Text] -> Text
renderStringEnumHint values = "One of: " <> T.intercalate ", " values
