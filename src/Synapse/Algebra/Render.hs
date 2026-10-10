-- | Render algebra for PluginSchema
module Synapse.Algebra.Render
  ( -- * Rendering Functions
    renderSchema
  , renderSchemaWith
  , renderMethod
  , renderMethodFull
  , renderChild
  , renderParams

    -- * Configuration
  , RenderStyle(..)
  , defaultStyle
  , compactStyle

    -- * Deprecation Rendering (IR-6)
  , deprecationMarker
  , formatDeprecationLine
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Control.Applicative ((<|>))
import Data.List (intersperse, sortOn)

import Synapse.Schema.Types
import Plexus.Schema.Recursive (DeprecationInfo(..), ParamSchema(..))

-- | Rendering style configuration
data RenderStyle = RenderStyle
  { rsIndent    :: !Int   -- ^ Spaces per indent level
  , rsShowHash  :: !Bool  -- ^ Show plugin/method hashes
  , rsShowTypes :: !Bool  -- ^ Show parameter types
  , rsCompact   :: !Bool  -- ^ Compact single-line format
  , rsWidth     :: !Int   -- ^ Columns to wrap descriptions at (SYNAPSE-12)
  }
  deriving stock (Show, Eq)

-- | Default style: readable multi-line output
defaultStyle :: RenderStyle
defaultStyle = RenderStyle
  { rsIndent    = 2
  , rsShowHash  = False
  , rsShowTypes = True
  , rsCompact   = False
  , rsWidth     = 80
  }

-- | Compact style: single-line descriptions
compactStyle :: RenderStyle
compactStyle = defaultStyle { rsCompact = True }

-- | Render a PluginSchema
renderSchema :: PluginSchema -> Text
renderSchema = renderSchemaWith defaultStyle

-- | Render with custom style
renderSchemaWith :: RenderStyle -> PluginSchema -> Text
renderSchemaWith style PluginSchema{..} = T.unlines (dropTrailingBlanks body)
  where
    width = rsWidth style

    -- SYNAPSE-12: one blank line between blocks, never more; the name and
    -- version, the description under it, then each section under a header.
    body = concat
      [ [heading]
      , deprecationNotice
      , map ("  " <>) (wrap (width - 2) psDescription)
      , requestBlock
      , section "activations" False childBlocks
      , section "methods" True methodBlocks
      ]

    heading = case psDeprecation of
      Just _  -> deprecationMarker <> " " <> psNamespace <> " v" <> psVersion
      Nothing -> psNamespace <> " v" <> psVersion

    deprecationNotice = case psDeprecation of
      Just di -> ["  " <> formatDeprecationLine di]
      Nothing -> []

    -- REQ-5: the activation's PlexusRequest schema.
    -- HEADERS-1 stub: plexus-protocol's PluginSchema doesn't yet expose
    -- psRequest. Nothing to render until the field lands in plexus-protocol;
    -- the renderer it had ("Request requirements:", the auth notice) is in
    -- this file's history before SYNAPSE-12.
    requestBlock = []

    section _ _ [] = []
    section title spaced blocks =
      "" : title : concat (if spaced then intersperse [""] blocks else blocks)

    children = maybe [] (sortOn csNamespace) psChildren
    methods = sortOn methodName psMethods

    -- The name column: wide enough for the longest name (capped), shared by
    -- both sections so their descriptions start in one column.
    nameWidth = min 18 $ maximum $ 8 : map T.length (map csNamespace children ++ map decoratedName methods)
    descCol = 2 + nameWidth + 2

    decoratedName m = case methodDeprecation m of
      Just _  -> deprecationMarker <> " " <> methodName m
      Nothing -> methodName m

    childBlocks = [ entry (csNamespace c) (csDescription c) | c <- children ]

    methodBlocks =
      [ entry (decoratedName m) (methodDescription m)
          ++ maybe [] (\di -> [T.replicate descCol " " <> formatDeprecationLine di]) (methodDeprecation m)
          ++ paramLines (methodParams m)
      | m <- methods ]

    -- A name and its description, the description wrapped in its column;
    -- a name too long for the column puts the description under it.
    entry name desc =
      hang descCol ("  " <> name) (wrap (width - descCol) desc)

    -- Parameters, under the method's name: required first, then
    -- optional (in [brackets]), each a flag column and a wrapped description.
    paramLines Nothing = []
    paramLines (Just (Object o)) = case KM.lookup "properties" o of
      Just (Object props) ->
        let reqList = case KM.lookup "required" o of
              Just (Array arr) -> [t | String t <- V.toList arr]
              _ -> []
            sorted = sortOn (\(k, _) -> (K.toText k `notElem` reqList, K.toText k)) (KM.toList props)
            rows = map (paramRow reqList) sorted
            flagW = min 26 (maximum (0 : [T.length f | (f, _) <- rows]))
            ind = 6  -- under the method's name, a step in
            col = ind + flagW + 2
            render (flag, desc) =
              hang col (T.replicate ind " " <> flag) (wrap (width - col) desc)
        in concatMap render rows
      _ -> []
    paramLines _ = []

    paramRow :: [Text] -> (K.Key, Value) -> (Text, Text)
    paramRow required (name, propSchema) =
      let nameText = K.toText name
          isReq = nameText `elem` required
          (typ, desc) = extractTypeDesc propSchema
      in case extractParamSourceAnnot propSchema of
           -- REQ-8: a param with x-plexus-source is server-extracted —
           -- clients can't supply it, so no --flag form; its name and
           -- where it comes from instead.
           Just annotation -> (nameText, "\x2190 " <> annotation)
           Nothing ->
             let flag = "--" <> T.replace "_" "-" nameText <> " <" <> typ <> ">"
             in (if isReq then flag else "[" <> flag <> "]", desc)

    -- REQ-8: format an x-plexus-source annotation as a short inline label.
    -- Returns 'Nothing' when the property has no annotation (i.e. it's an
    -- ordinary RPC param). Shape recognition matches REQ-6's wire vocabulary.
    extractParamSourceAnnot :: Value -> Maybe Text
    extractParamSourceAnnot propSchema = do
      Object po <- Just propSchema
      Object src <- KM.lookup "x-plexus-source" po
      let lookupStr k = case KM.lookup k src of
            Just (String t) -> Just t
            _               -> Nothing
      from <- lookupStr "from"
      case from of
        "auth" -> case lookupStr "resolver" of
          Just r  -> Just ("auth: " <> r)
          Nothing -> Just "auth"
        "cookie"  -> Just ("cookie" <> maybe "" (" " <>) (lookupStr "key"))
        "header"  -> Just ("header" <> maybe "" (" " <>) (lookupStr "key"))
        "query"   -> Just ("query" <> maybe "" (" " <>) (lookupStr "key"))
        "derived" -> Just "server-derived"
        "rpc"     -> Nothing
        other     -> Just other

-- | Words wrapped greedily to a width (at least 20); a word longer than the
-- width stands on its own line.
wrap :: Int -> Text -> [Text]
wrap w0 txt = go (T.words txt)
  where
    w = max 20 w0
    go [] = []
    go (x : xs) = line x xs
    line acc [] = [acc]
    line acc (y : ys)
      | T.length acc + 1 + T.length y <= w = line (acc <> " " <> y) ys
      | otherwise = acc : line y ys

-- | A lead and lines that start in column @col@: the first beside the lead
-- when it fits (two spaces between), else all of them under it.
hang :: Int -> Text -> [Text] -> [Text]
hang col lead ds = case ds of
  (d : rest) | T.length lead <= col - 2 -> (T.justifyLeft col ' ' lead <> d) : map (pad <>) rest
  _ -> lead : map (pad <>) ds
  where pad = T.replicate col " "

dropTrailingBlanks :: [Text] -> [Text]
dropTrailingBlanks = reverse . dropWhile (T.null . T.strip) . reverse

-- | Render a method (short form)
renderMethod :: MethodSchema -> Text
renderMethod = renderMethodWith defaultStyle

-- | Render a method with style
renderMethodWith :: RenderStyle -> MethodSchema -> Text
renderMethodWith RenderStyle{..} m =
  "  " <> padRight 16 (methodName m) <> methodDescription m
    <> if rsShowTypes then renderParams (methodParams m) else ""

-- | Render a method (full form with all params).
--
-- IR-14: per-parameter deprecation info carried on @methodParamSchemas@
-- is threaded into the param rendering so each deprecated parameter
-- receives the ⚠ marker and the formatted @DEPRECATED since …@ line.
-- Non-deprecated parameters render exactly as before.
renderMethodFull :: MethodSchema -> Text
renderMethodFull m = T.unlines $
  [ methodName m <> " - " <> methodDescription m
  , ""
  ] <> paramLines
  where
    paramLines = case methodParams m of
      Nothing -> ["  (no parameters)"]
      Just schema -> renderParamsFull (methodParamSchemas m) schema

-- | Render a child summary
renderChild :: ChildSummary -> Text
renderChild child =
  "  " <> padRight 16 (csNamespace child) <> csDescription child

-- | Render parameters inline
renderParams :: Maybe Value -> Text
renderParams Nothing = ""
renderParams (Just (Object o)) = case KM.lookup "properties" o of
  Just (Object props) ->
    let reqList = case KM.lookup "required" o of
          Just (Array arr) -> [t | String t <- foldr (:) [] arr]
          _ -> []
        propList = KM.toList props
        sorted = sortOn (\(k, _) -> (K.toText k `notElem` reqList, K.toText k)) propList
        rendered = map (renderParam reqList) sorted
    in if null rendered then "" else "\n" <> T.intercalate "\n" rendered
  _ -> ""
renderParams _ = ""

-- | Render a single parameter
renderParam :: [Text] -> (K.Key, Value) -> Text
renderParam required (name, propSchema) =
  let nameText = K.toText name
      isReq = nameText `elem` required
      flagName = T.replace "_" "-" nameText
      (typ, desc) = extractTypeDesc propSchema
      reqMarker = if isReq then "" else "?"
  in "      --" <> flagName <> " <" <> typ <> ">" <> reqMarker <> "  " <> desc

-- | Render parameters in full (for method help).
--
-- IR-14: the first argument carries optional per-parameter metadata
-- ('ParamSchema') advertised by IR-5 producers. Any parameter whose
-- 'paramDeprecation' is @Just@ is decorated with the ⚠ marker and a
-- trailing @DEPRECATED since … removed in … — …@ line. Parameters
-- without a matching entry render identically to pre-ticket output.
renderParamsFull :: Maybe [ParamSchema] -> Value -> [Text]
renderParamsFull paramSchemas (Object o) = case KM.lookup "properties" o of
  Just (Object props) ->
    let reqList = case KM.lookup "required" o of
          Just (Array arr) -> [t | String t <- foldr (:) [] arr]
          _ -> []
        propList = KM.toList props
        sorted = sortOn (\(k, _) -> (K.toText k `notElem` reqList, K.toText k)) propList
    in map (renderParamFull paramSchemas reqList) sorted
  _ -> []
renderParamsFull _ _ = []

-- | Look up per-parameter deprecation info by name, scanning the
--   optional ParamSchema list attached to the method.
lookupParamDeprecation :: Maybe [ParamSchema] -> Text -> Maybe DeprecationInfo
lookupParamDeprecation Nothing     _     = Nothing
lookupParamDeprecation (Just pss) pname =
  case filter ((== pname) . paramName) pss of
    (ps:_) -> paramDeprecation ps
    []     -> Nothing

renderParamFull :: Maybe [ParamSchema] -> [Text] -> (K.Key, Value) -> Text
renderParamFull paramSchemas required (name, propSchema) =
  let nameText = K.toText name
      isReq = nameText `elem` required
      (typ, desc) = extractTypeDesc propSchema
      reqText = if isReq then " (required)" else " (optional)"
      -- IR-14: deprecation decoration.  Marker prepended to the flag
      -- name, detail line appended after the description.  Pre-ticket
      -- non-deprecated output is preserved exactly when depInfo is
      -- Nothing.
      depInfo = lookupParamDeprecation paramSchemas nameText
      decoratedName = case depInfo of
        Just _  -> deprecationMarker <> " --" <> nameText
        Nothing -> "--" <> nameText
      depLine = case depInfo of
        Just di -> "\n      " <> formatDeprecationLine di
        Nothing -> ""
  in "  " <> decoratedName <> " <" <> typ <> ">" <> reqText
     <> "\n      " <> desc <> depLine

-- | Extract type and description from property schema.
--
-- SYNAPSE-12: the type a flag takes, as the schema says it — a nullable
-- type is its non-null part (@["string", "null"]@ → @string@), an array
-- its element's (@string[]@), a @$ref@ the type it names, a union its
-- alternatives (@a|b@); @any@ only when the schema says nothing.
extractTypeDesc :: Value -> (Text, Text)
extractTypeDesc v@(Object po) =
  ( schemaTypeName v
  , case KM.lookup "description" po of { Just (String d) -> d; _ -> "" }
  )
extractTypeDesc _ = ("any", "")

schemaTypeName :: Value -> Text
schemaTypeName (Object po)
  | Just (String ref) <- KM.lookup "$ref" po = last (T.splitOn "/" ref)
  | Just (String "array") <- KM.lookup "type" po = itemsName <> "[]"
  | Just (String t) <- KM.lookup "type" po = t
  | Just (Array ts) <- KM.lookup "type" po =
      case [t | String t <- V.toList ts, t /= "null"] of
        ["array"] -> itemsName <> "[]"
        [] -> "any"
        ts' -> T.intercalate "|" ts'
  | Just (Array alts) <- KM.lookup "anyOf" po <|> KM.lookup "oneOf" po =
      case filter (/= "null") (map schemaTypeName (V.toList alts)) of
        [] -> "any"
        ns -> T.intercalate "|" (dedupe ns)
  | Just (Array _) <- KM.lookup "enum" po = "string"
  | otherwise = "any"
  where
    itemsName = maybe "any" schemaTypeName (KM.lookup "items" po)
    dedupe = foldr (\x acc -> if x `elem` acc then acc else x : acc) []
schemaTypeName _ = "any"

-- | Pad text to a minimum width
padRight :: Int -> Text -> Text
padRight n t
  | T.length t >= n = t <> " "
  | otherwise = t <> T.replicate (n - T.length t) " "

-- ============================================================================
-- Deprecation Rendering (IR-6)
-- ============================================================================

-- | Visible marker for deprecated surfaces.
--
--   Plain UTF-8 — a TTY/color-aware renderer lives at the CLI boundary;
--   this module only emits text markers.
deprecationMarker :: Text
deprecationMarker = "\x26A0"  -- ⚠

-- | Format the one-line deprecation detail, e.g.
--   @DEPRECATED since 0.5, removed in 0.7 — use move_doc@.
formatDeprecationLine :: DeprecationInfo -> Text
formatDeprecationLine di =
  "DEPRECATED since " <> depSince di
    <> ", removed in " <> depRemovedIn di
    <> " \x2014 " <> depMessage di   -- em-dash

