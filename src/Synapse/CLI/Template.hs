{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | IR-driven Mustache template generation
--
-- Generates structured Mustache templates directly from the IR, providing:
--
-- - Discriminated union handling with ALL variants in one template
-- - Exhaustive variant coverage (every variant gets a section)
-- - Structured output with labeled fields
--
-- = Design
--
-- Templates are generated per-method with all return type variants visible:
--
-- @
-- IR (types, methods)
--   |
--   +-> generateTemplate     -> unified template for a method (all variants)
--   +-> generateAllTemplates -> all templates for a path
--   +-> generateTemplatesFor -> subset of templates (filtered)
-- @
--
-- = Example Output
--
-- For `cone.chat` (streaming enum with variants):
--
-- @
-- {{! cone.chat - returns: ConeEvent }}
-- {{! Variants: start, content, thinking, tool_use, tool_result, complete, error, passthrough }}
--
-- {{#start}}
--   id: {{id}}
--   user_position: {{user_position}}
-- {{/start}}
--
-- {{#content}}{{text}}{{/content}}
--
-- {{#thinking}}{{thinking}}{{/thinking}}
--
-- {{#tool_use}}
--   tool_name: {{tool_name}}
--   tool_use_id: {{tool_use_id}}
--   input: {{input}}
-- {{/tool_use}}
--
-- {{#tool_result}}
--   tool_use_id: {{tool_use_id}}
--   output: {{output}}
--   is_error: {{is_error}}
-- {{/tool_result}}
--
-- {{#complete}}
--   new_head: {{new_head}}
--   usage: {{usage}}
-- {{/complete}}
--
-- {{#error}}{{message}}{{/error}}
--
-- {{#passthrough}}
--   event_type: {{event_type}}
--   data: {{data}}
-- {{/passthrough}}
-- @
module Synapse.CLI.Template
  ( -- * Template Generation
    generateTemplate
  , generateAllTemplates
  , generateTemplatesFor
  , generateAllTemplatesWithCallback

    -- * Type Conversion (exposed for testing/reuse)
  , typeRefToMustache
  , typeDefToMustache
  , variantToMustache

    -- * Types
  , GeneratedTemplate(..)
  , TemplateFilter(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((</>), (<.>))

import Synapse.IR.Types
import Synapse.IR.Builder (buildIR)
import Synapse.Monad (SynapseM)
import Synapse.Schema.Types (Path)

-- ============================================================================
-- Types
-- ============================================================================

-- | A generated template with metadata
data GeneratedTemplate = GeneratedTemplate
  { gtNamespace    :: Text       -- ^ Plugin namespace (e.g., "cone")
  , gtMethod       :: Text       -- ^ Method name (e.g., "chat")
  , gtTemplate     :: Text       -- ^ Mustache template content
  , gtPath         :: FilePath   -- ^ Relative path for template file
  , gtVariants     :: [Text]     -- ^ List of variant names this template handles
  , gtReturnType   :: Maybe Text -- ^ Return type name if named
  }
  deriving stock (Show, Eq)

-- | Filter for selective template generation
data TemplateFilter
  = AllTemplates                    -- ^ Generate all templates
  | NamespaceFilter [Text]          -- ^ Only these namespaces
  | MethodFilter [(Text, Text)]     -- ^ Only these (namespace, method) pairs
  | ExcludeFilter [Text]            -- ^ Exclude these namespaces
  deriving stock (Show, Eq)

-- ============================================================================
-- High-Level Generation
-- ============================================================================

-- | Generate a unified template for a method's return type
--
-- All variants are included in a single template file.
-- The template is named by method: {namespace}/{method}.mustache
generateTemplate :: IR -> MethodDef -> GeneratedTemplate
generateTemplate ir method =
  let (body, variants, typeName) = generateMethodBody ir method
      header = generateHeader method typeName variants
      template = header <> "\n" <> body
  in GeneratedTemplate
    { gtNamespace = mdNamespace method
    , gtMethod = mdName method
    , gtTemplate = template
    , gtPath = T.unpack (mdNamespace method) </> T.unpack (mdName method) <.> "mustache"
    , gtVariants = variants
    , gtReturnType = typeName
    }

-- | Generate header comment with method info and variant list
generateHeader :: MethodDef -> Maybe Text -> [Text] -> Text
generateHeader method mTypeName variants =
  let typeInfo = maybe "" (\t -> " - returns: " <> t) mTypeName
      variantList = if null variants
                    then ""
                    else "\n{{! Variants: " <> T.intercalate ", " variants <> " }}"
  in "{{! " <> mdFullPath method <> typeInfo <> " }}" <> variantList

-- | Generate template body for a method, returning (body, variants, typeName)
generateMethodBody :: IR -> MethodDef -> (Text, [Text], Maybe Text)
generateMethodBody ir method = case mdReturns method of
  RefNamed qn ->
    let name = qualifiedNameFull qn
    in case Map.lookup name (irTypes ir) of
      Just typeDef@TypeDef{tdKind = KindEnum _ variants} ->
        -- Union type: generate sections for ALL variants (exhaustive)
        -- No blank lines between sections to avoid whitespace in output
        let variantNames = map vdName variants
            body = T.intercalate "\n" (map (variantToMustache ir 0) variants) <> "\n"
        in (body, variantNames, Just name)
      Just typeDef ->
        -- Non-union named type
        (typeDefToMustache ir 0 typeDef, [], Just name)
      Nothing ->
        -- Type not found
        ("{{.}}", [], Just name)
  other ->
    -- Primitive or other
    (typeRefToMustache ir 0 other, [], Nothing)

-- | Generate all templates by building IR and iterating methods
generateAllTemplates :: Path -> SynapseM [GeneratedTemplate]
generateAllTemplates = generateTemplatesFor AllTemplates

-- | Generate templates with filtering
generateTemplatesFor :: TemplateFilter -> Path -> SynapseM [GeneratedTemplate]
generateTemplatesFor filt path = do
  ir <- buildIR [] path  -- No generator info for internal template generation
  let methods = Map.elems (irMethods ir)
      filtered = filterMethods filt methods
      templates = map (generateTemplate ir) filtered
      -- Filter out trivial templates that just render {{.}}
      nonTrivial = filter (not . isTrivialTemplate) templates
  pure nonTrivial

-- | Filter methods based on TemplateFilter
filterMethods :: TemplateFilter -> [MethodDef] -> [MethodDef]
filterMethods AllTemplates methods = methods
filterMethods (NamespaceFilter nss) methods =
  filter (\m -> mdNamespace m `elem` nss) methods
filterMethods (MethodFilter pairs) methods =
  filter (\m -> (mdNamespace m, mdName m) `elem` pairs) methods
filterMethods (ExcludeFilter nss) methods =
  filter (\m -> mdNamespace m `notElem` nss) methods

-- | Generate templates with callback for streaming output
generateAllTemplatesWithCallback
  :: (GeneratedTemplate -> IO ())  -- ^ Called for each template
  -> Path
  -> SynapseM Int                  -- ^ Returns count
generateAllTemplatesWithCallback callback path = do
  templates <- generateAllTemplates path
  liftIO $ mapM_ callback templates
  pure $ length templates

-- | Check if a template is trivial (just {{.}})
isTrivialTemplate :: GeneratedTemplate -> Bool
isTrivialTemplate gt =
  let body = T.drop 1 $ T.dropWhile (/= '\n') (gtTemplate gt)
      -- Also drop the variants comment line if present
      bodyWithoutComments = T.unlines $ filter (not . isComment) $ T.lines body
  in T.strip bodyWithoutComments == "{{.}}"
  where
    isComment line = "{{!" `T.isPrefixOf` T.strip line

-- ============================================================================
-- Variant to Mustache (Exhaustive)
-- ============================================================================

-- | Convert a variant to a Mustache section
--
-- This ensures EVERY variant gets a section, making the mapping exhaustive.
variantToMustache :: IR -> Int -> VariantDef -> Text
variantToMustache ir indent VariantDef{..} =
  let ind = indentText indent
      sectionName = vdName
      displayFields = filter (not . isInternalField) vdFields
  in case displayFields of
    -- No displayable fields: empty section (still present for exhaustiveness)
    [] -> ind <> "{{#" <> sectionName <> "}}{{/" <> sectionName <> "}}"

    -- Single content-like field: inline rendering (triple braces to avoid HTML escaping)
    [field] | isContentField field ->
      ind <> "{{#" <> sectionName <> "}}{{{" <> fdName field <> "}}}{{/" <> sectionName <> "}}"

    -- Multiple fields: render as labeled lines (no trailing newline)
    fields ->
      let fieldLines = map (generateFieldLine ir (indent + 1)) fields
          openTag = ind <> "{{#" <> sectionName <> "}}"
          closeTag = ind <> "{{/" <> sectionName <> "}}"
      in T.intercalate "\n" $ [openTag] ++ fieldLines ++ [closeTag]

-- ============================================================================
-- Type Reference to Mustache
-- ============================================================================

-- | Convert a TypeRef to Mustache template text
typeRefToMustache :: IR -> Int -> TypeRef -> Text
typeRefToMustache ir indent = \case
  -- Primitives render as the current value
  RefPrimitive _ _ -> "{{.}}"

  -- Named types: look up the definition
  RefNamed qn ->
    let name = qualifiedNameFull qn
    in case Map.lookup name (irTypes ir) of
      Just typeDef -> typeDefToMustache ir indent typeDef
      Nothing -> "{{.}}"  -- Fallback if type not found

  -- Arrays: iterate with section
  RefArray inner ->
    let itemTemplate = typeRefToMustache ir (indent + 1) inner
        ind = indentText indent
    in T.unlines
      [ ind <> "{{#.}}"
      , itemTemplate
      , ind <> "{{/.}}"
      ]

  -- Optionals: just render the inner type (mustache handles missing)
  RefOptional inner -> typeRefToMustache ir indent inner

  -- Any/Unknown: fall back to current value
  RefAny -> "{{.}}"
  RefUnknown -> "{{.}}"

-- ============================================================================
-- Type Definition to Mustache
-- ============================================================================

-- | Convert a TypeDef to Mustache template text
typeDefToMustache :: IR -> Int -> TypeDef -> Text
typeDefToMustache ir indent TypeDef{..} = case tdKind of
  -- Discriminated union: render ALL variants as sections (exhaustive)
  KindEnum _discriminator variants ->
    T.intercalate "\n" $ map (variantToMustache ir indent) variants

  -- Struct: render each field
  KindStruct fields ->
    generateStructTemplate ir indent fields

  -- Primitive: just the value
  KindPrimitive _ _ -> "{{.}}"

  -- Alias: follow to target
  KindAlias target -> typeRefToMustache ir indent target

-- ============================================================================
-- Struct Template Generation
-- ============================================================================

-- | Generate template for a struct
generateStructTemplate :: IR -> Int -> [FieldDef] -> Text
generateStructTemplate ir indent fields =
  let displayFields = filter (not . isInternalField) fields
      fieldLines = map (generateFieldLine ir indent) displayFields
  in T.unlines fieldLines

-- ============================================================================
-- Field Generation
-- ============================================================================

-- | Generate a labeled line for a field
generateFieldLine :: IR -> Int -> FieldDef -> Text
generateFieldLine ir indent field@FieldDef{..} =
  let ind = indentText indent
      label = fdName <> ": "
  in case fdType of
    -- For optional fields, show value or explicit "null"
    RefOptional inner ->
      let innerValue = generateOptionalInnerValue ir inner
      in ind <> label <> "{{#" <> fdName <> "}}" <> innerValue <> "{{/" <> fdName <> "}}{{^" <> fdName <> "}}null{{/" <> fdName <> "}}"
    _ ->
      let value = generateFieldValue ir fdType fdName field
      in ind <> label <> value

-- | Generate value for inside an optional section (no outer wrapper needed)
generateOptionalInnerValue :: IR -> TypeRef -> Text
generateOptionalInnerValue ir typeRef = case typeRef of
  RefNamed qn ->
    let name = qualifiedNameFull qn
    in case Map.lookup name (irTypes ir) of
      Just TypeDef{tdKind = KindStruct fields} ->
        let displayFields = filter (not . isInternalField) fields
            fieldRefs = map (\f -> fdName f <> "={{" <> fdName f <> "}}") displayFields
        in T.intercalate " " fieldRefs
      _ -> "{{.}}"
  RefPrimitive _ _ -> "{{.}}"
  _ -> "{{.}}"

-- | Generate the value portion of a field
generateFieldValue :: IR -> TypeRef -> Text -> FieldDef -> Text
generateFieldValue ir typeRef fieldName field = case typeRef of
  -- Primitives: use triple braces for content fields to avoid HTML escaping
  RefPrimitive _ _ ->
    if isContentField field
    then "{{{" <> fieldName <> "}}}"
    else "{{" <> fieldName <> "}}"

  -- Arrays: iterate
  RefArray inner -> case inner of
    RefPrimitive _ _ ->
      "{{#" <> fieldName <> "}}{{.}} {{/" <> fieldName <> "}}"
    RefNamed qn ->
      let name = qualifiedNameFull qn
      in case Map.lookup name (irTypes ir) of
        Just TypeDef{tdKind = KindStruct fields} ->
          -- For arrays of structs, expand each item's fields on its own line
          let displayFields = filter (not . isInternalField) fields
              fieldRefs = map (generateFieldRefInContext ir "") displayFields
          in "{{#" <> fieldName <> "}}\n    " <> T.intercalate " " fieldRefs <> "{{/" <> fieldName <> "}}"
        _ ->
          "{{#" <> fieldName <> "}}{{.}}{{/" <> fieldName <> "}}"
    _ ->
      "{{#" <> fieldName <> "}}{{.}}{{/" <> fieldName <> "}}"

  -- Named types: depends on what they are
  RefNamed qn ->
    let name = qualifiedNameFull qn
    in case Map.lookup name (irTypes ir) of
      Just TypeDef{tdKind = KindPrimitive _ _} ->
        "{{" <> fieldName <> "}}"
      Just TypeDef{tdKind = KindStruct fields} ->
        -- Expand struct fields with dot notation to avoid Haskell Show output
        let displayFields = filter (not . isInternalField) fields
            fieldRefs = map (\f -> fdName f <> "={{" <> fieldName <> "." <> fdName f <> "}}") displayFields
        in T.intercalate " " fieldRefs
      Just TypeDef{tdKind = KindEnum _ _} ->
        "{{{" <> fieldName <> "}}}"  -- Nested union: use triple braces for unescaped JSON
      _ ->
        "{{" <> fieldName <> "}}"

  -- Optional: wrap in section so null renders nothing
  RefOptional inner -> case inner of
    -- For optional structs, wrap expansion in a section
    RefNamed qn ->
      let name = qualifiedNameFull qn
      in case Map.lookup name (irTypes ir) of
        Just TypeDef{tdKind = KindStruct fields} ->
          let displayFields = filter (not . isInternalField) fields
              fieldRefs = map (\f -> fdName f <> "={{" <> fdName f <> "}}") displayFields
          in "{{#" <> fieldName <> "}}" <> T.intercalate " " fieldRefs <> "{{/" <> fieldName <> "}}"
        _ -> "{{#" <> fieldName <> "}}{{.}}{{/" <> fieldName <> "}}"
    -- For optional primitives, simple section
    _ -> "{{#" <> fieldName <> "}}{{.}}{{/" <> fieldName <> "}}"

  -- Fallback: use triple braces for content fields
  RefAny ->
    if isContentField field
    then "{{{" <> fieldName <> "}}}"
    else "{{" <> fieldName <> "}}"
  RefUnknown ->
    if isContentField field
    then "{{{" <> fieldName <> "}}}"
    else "{{" <> fieldName <> "}}"

-- ============================================================================
-- Field Reference Generation (for array contexts)
-- ============================================================================

-- | Generate a field reference in the context of an array iteration
-- Recursively expands struct fields using dot notation
-- prefix is the path prefix (empty for top-level array items)
generateFieldRefInContext :: IR -> Text -> FieldDef -> Text
generateFieldRefInContext ir prefix field =
  let name = fdName field
      fullPath = if T.null prefix then name else prefix <> "." <> name
  in case fdType field of
    -- Primitives: simple reference
    RefPrimitive _ _ ->
      name <> "={{" <> fullPath <> "}}"

    -- Named types: check if it's a struct that needs expansion
    RefNamed qn ->
      let typeName = qualifiedNameFull qn
      in case Map.lookup typeName (irTypes ir) of
        Just TypeDef{tdKind = KindStruct fields} ->
          -- Flatten nested struct fields using dot notation (no wrapper)
          let displayFields = filter (not . isInternalField) fields
              dotRefs = map (\f -> fdName f <> "={{" <> fullPath <> "." <> fdName f <> "}}") displayFields
          in T.intercalate " " dotRefs
        Just TypeDef{tdKind = KindPrimitive _ _} ->
          name <> "={{" <> fullPath <> "}}"
        _ ->
          -- Enum or unknown: just reference it
          name <> "={{" <> fullPath <> "}}"

    -- Arrays: indicate it's an array
    RefArray _ ->
      name <> "=[...]"

    -- Optional: same as inner type but may be null
    RefOptional inner -> case inner of
      RefNamed qn ->
        let typeName = qualifiedNameFull qn
        in case Map.lookup typeName (irTypes ir) of
          Just TypeDef{tdKind = KindStruct fields} ->
            -- Flatten nested struct fields using dot notation (no wrapper)
            let displayFields = filter (not . isInternalField) fields
                dotRefs = map (\f -> fdName f <> "={{" <> fullPath <> "." <> fdName f <> "}}") displayFields
            in T.intercalate " " dotRefs
          _ -> name <> "={{" <> fullPath <> "}}"
      _ -> name <> "={{" <> fullPath <> "}}"

    -- Fallback
    _ -> name <> "={{" <> fullPath <> "}}"

-- ============================================================================
-- Field Classification
-- ============================================================================

-- | Check if a field is a "content" field (for inline rendering)
isContentField :: FieldDef -> Bool
isContentField FieldDef{..} = fdName `elem` ["content", "message", "text", "data"]

-- | Check if a field is internal (shouldn't be displayed)
isInternalField :: FieldDef -> Bool
isInternalField FieldDef{..} = fdName `elem` ["type", "event"]

-- ============================================================================
-- Helpers
-- ============================================================================

-- | Generate indentation text
indentText :: Int -> Text
indentText n = T.replicate (n * 2) " "
