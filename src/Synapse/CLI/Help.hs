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

    -- * The method page (SYNAPSE-13)
  , MethodPage(..)
  , methodPage
  , renderMethodPage
  , renderExample
  , synthesiseExample
  , shellQuote

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

import Control.Applicative ((<|>))
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Plexus.Schema.Recursive (MethodExample(..), MethodSchema(..), paramValues)
import Synapse.IR.Types
import Synapse.Algebra.Render (deprecationMarker, formatDeprecationLine, fitText)
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

-- | Render complete help for a method: its page ('renderMethodPage'),
-- reached by @synapse <namespace> <method>@, from the IR alone.
renderMethodHelp :: IR -> MethodDef -> Text
renderMethodHelp ir md = renderMethodPage (methodPage md) ir md

-- | Render method help with custom style (the style no longer changes the
-- page; kept for callers of the earlier layout).
renderMethodHelpWith :: HelpStyle -> IR -> MethodDef -> Text
renderMethodHelpWith _ = renderMethodHelp

-- ============================================================================
-- The method page (SYNAPSE-13)
-- ============================================================================

-- | What a method's page needs beside the IR.
data MethodPage = MethodPage
  { mpProgram :: Text               -- ^ synapse and its global options (@synapse -P 4455@)
  , mpTarget  :: Text               -- ^ the backend and path to the method (@menger tickets set_status@)
  , mpSchema  :: Maybe MethodSchema -- ^ the method's schema: long description, examples, values, grant
  , mpCaller  :: Bool               -- ^ AUTH-8: in a caller's view (say what it requires)
  }
  deriving stock (Show, Eq)

-- | A page for a method known only from the IR: reached as
-- @synapse <namespace> <method>@.
methodPage :: MethodDef -> MethodPage
methodPage md = MethodPage
  { mpProgram = "synapse"
  , mpTarget = T.replace "." " " (mdFullPath md)
  , mpSchema = Nothing
  , mpCaller = False
  }

-- | SYNAPSE-13: a method's page in cargo's shape — the description; a
-- blank line and the long description; @Usage:@ with the required flags
-- spelled out; @Options:@ aligned, required first then optional in
-- [brackets], each with its type, description and allowed values;
-- @Example:@ — the schema's examples as command lines, each under its
-- note, else one synthesised from the required options; and, in a caller's
-- view, what it requires and where the caller holds it.
renderMethodPage :: MethodPage -> IR -> MethodDef -> Text
renderMethodPage MethodPage{..} ir md = T.unlines (dropTrailing body)
  where
    width = 80
    invoke = mpProgram <> " " <> mpTarget

    body = concat
      [ descLines
      , longLines
      , [""]
      , usageLines
      , optionsBlock
      , exampleBlock
      , requiresBlock
      ]

    descLines = case mdDescription md of
      Just d | not (T.null (T.strip d)) -> wrapWords width d
      _ -> [mdName md]

    longText = (mpSchema >>= methodLongDescription) <|> mdLongDescription md
    longLines = case longText of
      Just l | not (T.null (T.strip l)) ->
        "" : concatMap (\p -> if T.null p then [""] else wrapWords width p) (paragraphs l)
      _ -> []

    -- What a caller passes (a server-supplied param, REQ-8, is not a flag).
    (callerParams, serverParams) =
      ( [p | p <- params, not (serverSourced p)]
      , [p | p <- params, serverSourced p] )
    params = sortOn (not . pdRequired) (mdParams md)
    required = [p | p <- callerParams, pdRequired p]
    optional = [p | p <- callerParams, not (pdRequired p)]

    usageLines =
      [ "Usage: " <> T.unwords ([invoke] ++ map usageFlag required ++ ["[OPTIONS]" | not (null optional)]) ]
    usageFlag p = flagOf p <> " <" <> T.toUpper (pdName p) <> ">"

    optionsBlock
      | null callerParams && null serverParams = []
      | otherwise = ["", "Options:"] ++ concatMap optionRows callerParams ++ concatMap serverRows serverParams

    flagCol = 2 + min 28 (maximum (0 : map (T.length . flagText) callerParams ++ map (T.length . pdName) serverParams)) + 2

    flagText p =
      let f = flagDecorated p <> " <" <> shortType (pdType p) <> ">"
      in if pdRequired p then f else "[" <> f <> "]"
    flagDecorated p = case pdDeprecation p of
      Just _ -> deprecationMarker <> " " <> flagOf p
      Nothing -> flagOf p

    optionRows p = hangAt flagCol ("  " <> flagText p) (descOf p ++ extras p)
    serverRows p = hangAt flagCol ("  " <> pdName p) ["\x2190 " <> sourceLabel p]

    -- A type as a flag takes it: an optional one as its inner type (the
    -- brackets say it is optional), a named one by its own name.
    shortType = \case
      RefOptional inner -> shortType inner
      RefArray inner -> shortType inner <> "[]"
      t@(RefNamed qn) ->
        let full = renderTypeRef ir t
        in if full == qualifiedNameFull qn then qnLocalName qn else full
      t -> renderTypeRef ir t

    descW = max 20 (width - flagCol)
    descOf p = maybe [] (wrapWords descW) (pdDescription p)
    extras p = concat
      [ maybe [] (\di -> [formatDeprecationLine di]) (pdDeprecation p)
      , listLines p
      , maybe [] (\vs -> wrapWords descW ("[values: " <> T.intercalate ", " vs <> "]")) (valuesOf p)
      , if hsExpandEnums defaultHelpStyle then expandType ir (pdName p) (pdType p) else []
      ]

    listLines p = case listElement (pdType p) of
      Just inner
        | isScalarElement ir inner -> ["Repeat the flag or use commas: " <> flagOf p <> " a,b"]
        | otherwise -> ["Repeat the flag for each element"]
      Nothing -> []
    listElement = \case
      RefArray inner -> Just inner
      RefOptional inner -> listElement inner
      _ -> Nothing

    -- The schema's hint (x-plexus-values), else the type's own literals.
    valuesOf p = schemaValues (pdName p) <|> stringEnumValues ir (pdType p)
    schemaValues name = do
      ms <- mpSchema
      Object o <- methodParams ms
      Object props <- KM.lookup "properties" o
      prop <- KM.lookup (K.fromText name) props
      paramValues prop

    -- Examples: the schema's, else one from the required options.
    examples = case maybe [] methodExamples mpSchema of
      [] -> [MethodExample (synthesiseExample ir md (fmap methodParams mpSchema >>= id)) Nothing]
      es -> es
    exampleBlock =
      ["", if length examples == 1 then "Example:" else "Examples:"]
        ++ concat (zipWith exampleLines [0 :: Int ..] examples)
    exampleLines i ex =
      [ "" | i > 0 ]
        ++ maybe [] (\n -> ["  # " <> n]) (exampleNote ex)
        ++ ["  " <> renderExample mpProgram mpTarget md (exampleParams ex)]

    -- AUTH-8: in a caller's view, the grant and where it is held.
    requiresBlock = case mpSchema of
      Just ms | mpCaller -> maybe [] (\l -> ["", l]) (requiresLine ms)
      _ -> []
    requiresLine ms
      | methodPublic ms = Just "Requires: nothing (public)"
      | otherwise = do
          grant <- methodGrant ms
          let held = case methodHeldOn ms of
                Just hs@(_ : _) -> "you hold it on " <> T.intercalate ", " hs
                _ -> "you hold it nowhere"
          pure $ case methodTarget ms of
            Just t -> "Requires: " <> grant <> " on the scope of --" <> kebab t <> " (" <> held <> ")"
            Nothing -> "Requires: " <> grant <> ", answered from where it is held (" <> held <> ")"

    paragraphs = map (T.unwords . T.words) . splitBlank . T.lines
    splitBlank ls = case break (T.null . T.strip) ls of
      (p, []) -> [T.unlines p]
      (p, _ : rest) -> T.unlines p : "" : splitBlank rest

    dropTrailing = reverse . dropWhile (T.null . T.strip) . reverse

-- | A lead and lines that start in column @col@: the first beside the lead
-- when it fits (two spaces between), else all of them under it.
hangAt :: Int -> Text -> [Text] -> [Text]
hangAt col lead ds = case ds of
  [] -> [lead]
  (d : rest) | T.length lead <= col - 2 -> (T.justifyLeft col ' ' lead <> d) : map (pad <>) rest
  _ -> lead : map (pad <>) ds
  where pad = T.replicate col " "

flagOf :: ParamDef -> Text
flagOf p = "--" <> kebab (pdName p)

kebab :: Text -> Text
kebab = T.replace "_" "-"

-- | REQ-8: a param the server supplies (@x-plexus-source@ other than rpc).
serverSourced :: ParamDef -> Bool
serverSourced p = isJust (pdSource p) && sourceLabel p /= "rpc"

sourceLabel :: ParamDef -> Text
sourceLabel p = case pdSource p of
  Just (Object src) ->
    let str k = case KM.lookup k src of { Just (String t) -> Just t; _ -> Nothing }
    in case str "from" of
      Just "auth" -> maybe "auth" ("auth: " <>) (str "resolver")
      Just "cookie" -> "cookie" <> maybe "" (" " <>) (str "key")
      Just "header" -> "header" <> maybe "" (" " <>) (str "key")
      Just "query" -> "query" <> maybe "" (" " <>) (str "key")
      Just "derived" -> "server-derived"
      Just other -> other
      Nothing -> "server"
  _ -> "rpc"

-- | The literals a string-enum type takes, when it is one.
stringEnumValues :: IR -> TypeRef -> Maybe [Text]
stringEnumValues ir = \case
  RefOptional inner -> stringEnumValues ir inner
  RefNamed qn -> case Map.lookup (qualifiedNameFull qn) (irTypes ir) of
    Just TypeDef{tdKind = KindStringEnum vs} | not (null vs) -> Just vs
    _ -> Nothing
  _ -> Nothing

-- | SYNAPSE-13: one example from a method's required options: each the
-- value its description gives after @e.g.@, else the first of its values,
-- else a stand-in by type (@<ID>@ for a string).
synthesiseExample :: IR -> MethodDef -> Maybe Value -> Value
synthesiseExample ir md schemaParams = Object $ KM.fromList
  [ (K.fromText (pdName p), sample p) | p <- mdParams md, pdRequired p, not (serverSourced p) ]
  where
    sample p = fromMaybe (byType p (pdType p)) (fromDesc p <|> fromValues p)
    fromDesc p = do
      d <- pdDescription p
      let (_, after) = T.breakOn "e.g. " d
      guard' (not (T.null after))
      let tok = T.takeWhile (\c -> c `notElem` (" ,;)" :: String)) (T.drop 5 after)
          bare = T.dropAround (`elem` ("'\"`[]" :: String)) tok
      guard' (not (T.null bare))
      pure (String bare)
    fromValues p = do
      Object o <- schemaParams
      Object props <- KM.lookup "properties" o
      prop <- KM.lookup (K.fromText (pdName p)) props
      v : _ <- paramValues prop
      pure (String v)
    byType p = \case
      RefPrimitive "integer" _ -> Number 1
      RefPrimitive "number" _ -> Number 1
      RefPrimitive "boolean" _ -> Bool True
      RefArray inner -> Array (V.singleton (byType p inner))
      RefOptional inner -> byType p inner
      RefNamed qn -> case Map.lookup (qualifiedNameFull qn) (irTypes ir) of
        Just TypeDef{tdKind = KindStringEnum (v : _)} -> String v
        Just TypeDef{tdKind = KindAlias t} -> byType p t
        Just TypeDef{tdKind = KindPrimitive t _} -> byType p (RefPrimitive t Nothing)
        _ -> String ("<" <> T.toUpper (pdName p) <> ">")
      _ -> String ("<" <> T.toUpper (pdName p) <> ">")
    guard' b = if b then Just () else Nothing

-- | SYNAPSE-13: an example's params as a synapse command line: each param
-- its flag (required first, in the method's order), a list its values joined
-- by commas (an element holding one double-quoted, SYNAPSE-2), an object its dotted fields
-- (@--value.type choice@); params a flag cannot carry (a list inside an
-- object, SYNAPSE-9) go whole as @-p '{…}'@, before the backend.
renderExample :: Text -> Text -> MethodDef -> Value -> Text
renderExample program target md params = case params of
  Object o -> case mapM flagsOf (ordered o) of
    Just groups -> T.unwords ([program, target] ++ concat groups)
    Nothing -> whole
  _ -> whole
  where
    whole = T.unwords [program, "-p", shellQuote (TE.decodeUtf8 (LBS.toStrict (Aeson.encode params))), target]
    -- required first, then optional, each in the method's order
    order = zip (map pdName (sortOn (not . pdRequired) (mdParams md))) [0 :: Int ..]
    ordered o = sortOn (\(k, _) -> (fromMaybe maxBound (lookup (K.toText k) order), K.toText k)) (KM.toList o)
    flagsOf (k, v) = flags True ("--" <> kebab (K.toText k)) v
    flags _ _ Null = Just []
    -- an object's fields, its discriminator (@type@) first
    flags _ f (Object o) =
      concat <$> mapM (\(k, v) -> flags False (f <> "." <> kebab (K.toText k)) v)
        (sortOn (\(k, _) -> (K.toText k /= "type", K.toText k)) (KM.toList o))
    flags top f (Array xs)
      | not top = Nothing  -- a list inside an object: not as flags (SYNAPSE-9)
      | V.null xs = Just []
      | otherwise = do
          scalars <- mapM scalar (V.toList xs)
          -- an element holding a comma is double-quoted inside the value
          let element x = if T.isInfixOf "," x || T.isPrefixOf "\"" x || T.null x || T.strip x /= x
                then "\"" <> T.replace "\"" "\"\"" x <> "\""
                else x
          pure [f, shellQuote (T.intercalate "," (map element scalars))]
    flags _ f v = (\x -> [f, shellQuote x]) <$> scalar v
    scalar = \case
      String t -> Just t
      Number n -> Just (either (T.pack . show) (T.pack . show) (floatingOrInteger n :: Either Double Integer))
      Bool b -> Just (if b then "true" else "false")
      _ -> Nothing

-- | A word as the shell reads it: bare when it is plain, else in single
-- quotes.
shellQuote :: Text -> Text
shellQuote t
  | not (T.null t), T.all plain t = t
  | Just inner <- T.stripPrefix "<" t >>= T.stripSuffix ">", not (T.null inner), T.all plain inner = t
  | T.any (== '\'') t, not (T.any (`elem` ("\"$`\\!" :: String)) t) = "\"" <> t <> "\""
  | otherwise = "'" <> T.replace "'" "'\\''" t <> "'"
  where plain c = isAlphaNum c || c `elem` ("_@%+=:,./-" :: String)

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

      -- Description, wrapped in its column
      descLines = case pdDescription param of
        Just desc -> map (indent2 <>) (wrapWords (80 - T.length indent2) desc)
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

-- | Words wrapped greedily to a width (at least 20).
wrapWords :: Int -> Text -> [Text]
wrapWords w0 = go . T.words
  where
    w = max 20 w0
    go [] = []
    go (x : xs) = line x xs
    line acc [] = [acc]
    line acc (y : ys)
      | T.length acc + 1 + T.length y <= w = line (acc <> " " <> y) ys
      | otherwise = acc : line y ys

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
