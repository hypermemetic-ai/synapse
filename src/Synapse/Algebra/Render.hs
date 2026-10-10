-- | Render algebra for PluginSchema
module Synapse.Algebra.Render
  ( -- * Rendering Functions
    renderSchema
  , renderSchemaWith
  , renderSchemaFor
  , renderListing
  , Listing(..)
  , summarize
  , fitText
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
import Data.Char (toLower)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing)

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
renderSchemaWith style = renderSchemaFor style Nothing

-- | AUTH-8: render for a caller — the header line saying who it is
-- (@you are … · editor on MENGER@) under the description, when given.
-- SYNAPSE-13: the listing as 'renderListing' lays it out, reached by
-- @synapse <namespace>@.
renderSchemaFor :: RenderStyle -> Maybe Text -> PluginSchema -> Text
renderSchemaFor style callerLine ps =
  renderListing style (Listing ("synapse " <> psNamespace ps) callerLine) ps

-- | SYNAPSE-13: what a listing needs beside the schema.
data Listing = Listing
  { lsInvoke :: Text        -- ^ the command line that reaches this plugin (@synapse -P 4455 menger tickets@)
  , lsCaller :: Maybe Text  -- ^ AUTH-8: who the caller is, in a caller's view
  }
  deriving stock (Show, Eq)

-- | SYNAPSE-13: a plugin's listing in gh's shape — the name, version and
-- description; a usage line; then one line per activation and per method
-- (its name and summary, the first sentence of its description), the
-- methods in their groups (the schema's @group@, in the order they first
-- appear; alphabetical within each; one METHODS section when there are
-- none), and how to see a method's options. In a caller's view (AUTH-8)
-- who the caller is sits under the description, and each method's line
-- ends with its grant and where it is held, in two aligned columns.
renderListing :: RenderStyle -> Listing -> PluginSchema -> Text
renderListing style Listing{..} PluginSchema{..} = T.unlines (dropTrailingBlanks body)
  where
    width = rsWidth style
    authCols = isJust lsCaller || any (isJust . methodHeldOn) psMethods

    body = concat
      [ [heading]
      , deprecationNotice
      , map ("  " <>) (wrap (width - 2) psDescription)
      , maybe [] (\l -> ["  " <> l]) lsCaller
      , section "USAGE" (map ("  " <>) usage)
      , section "ACTIVATIONS" (map childRow children)
      , concat [ section title (concatMap methodRows ms) | (title, ms) <- methodSections ]
      , if null footer then [] else "" : footer
      ]

    heading = case psDeprecation of
      Just _  -> deprecationMarker <> " " <> psNamespace <> " v" <> psVersion
      Nothing -> psNamespace <> " v" <> psVersion

    deprecationNotice = case psDeprecation of
      Just di -> ["  " <> formatDeprecationLine di]
      Nothing -> []

    section _ [] = []
    section title rows = "" : title : rows

    children = maybe [] (sortOn csNamespace) psChildren

    usage = concat
      [ [lsInvoke <> " <activation> <method> [flags]" | not (null children)]
      , [lsInvoke <> " <method> [flags]" | not (null psMethods)]
      ]

    footer = concat
      [ ["Run '" <> lsInvoke <> " <activation>' for its methods." | not (null children)]
      , ["Run '" <> lsInvoke <> " <method> --help' for its options." | not (null psMethods)]
      ]

    -- The groups in the order they first appear, each alphabetical; the
    -- ungrouped last (as OTHER), or all of them as METHODS when no method
    -- says a group.
    methodSections =
      let groups = dedupe [g | Just g <- map methodGroup psMethods]
          inGroup g = sortOn methodName [m | m <- psMethods, methodGroup m == Just g]
          ungrouped = sortOn methodName [m | m <- psMethods, isNothing (methodGroup m)]
      in if null groups
           then [("METHODS", ungrouped) | not (null ungrouped)]
           else [(T.toUpper g, inGroup g) | g <- groups]
                  ++ [("OTHER", ungrouped) | not (null ungrouped)]
    dedupe = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) []

    decoratedName m = case methodDeprecation m of
      Just _  -> deprecationMarker <> " " <> methodName m
      Nothing -> methodName m

    -- The name column: wide enough for the longest name (capped), shared by
    -- every section so the summaries start in one column.
    nameW = min 24 $ maximum $ 8 : map T.length (map csNamespace children ++ map decoratedName psMethods)
    descCol = 2 + nameW + 2

    -- AUTH-8: the grant and where it is held, after the summary.
    grantOf m = fromMaybe "" (methodGrant m)
    heldOf m = maybe "" (T.intercalate ", ") (methodHeldOn m)
    grantW = maximum (0 : map (T.length . grantOf) psMethods)
    heldW = maximum (0 : map (T.length . heldOf) psMethods)
    summaryW
      | authCols = max 20 (width - descCol - 2 - grantW - 2 - heldW)
      | otherwise = width - descCol

    row name summary =
      T.stripEnd ("  " <> T.justifyLeft nameW ' ' name <> "  " <> summary)

    childRow c = row (csNamespace c) (fitText (width - descCol) (summarize (csDescription c)))

    methodRows m =
      let s = fitText summaryW (summarize (methodDescription m))
          line
            | authCols = row (decoratedName m)
                (T.justifyLeft summaryW ' ' s <> "  " <> T.justifyLeft grantW ' ' (grantOf m) <> "  " <> heldOf m)
            | otherwise = row (decoratedName m) s
      in line : maybe [] (\di -> [T.replicate descCol " " <> formatDeprecationLine di]) (methodDeprecation m)

-- | SYNAPSE-13: a description's summary — its first sentence: up to the
-- first @. @, @; @ or @: @ outside brackets (an @e.g.@ or @i.e.@ is not an
-- end), without the stop.
summarize :: Text -> Text
summarize desc = T.strip (go (0 :: Int) "" (T.unpack (T.strip desc)))
  where
    go _ acc [] = T.pack (reverse acc)
    go d acc (c : rest)
      | c `elem` ("([{" :: String) = go (d + 1) (c : acc) rest
      | c `elem` (")]}" :: String) = go (max 0 (d - 1)) (c : acc) rest
      | d == 0, c `elem` (".;:" :: String), startsSpace rest, not (abbrev c acc) = T.pack (reverse acc)
      | otherwise = go d (c : acc) rest
    startsSpace (' ' : _) = True
    startsSpace _ = False
    abbrev '.' acc = map toLower (reverse (takeWhile (/= ' ') acc)) `elem` ["e.g", "i.e", "etc", "vs", "cf"]
    abbrev _ _ = False

-- | SYNAPSE-13: a line's text in at most @w@ columns: whole when it fits;
-- else without its parentheticals; else up to the last @ — @ that fits;
-- else cut after a comma that keeps most of it, or at a word, ending in @…@.
fitText :: Int -> Text -> Text
fitText w t
  | T.length t <= w = t
  | T.length bare <= w = bare
  | Just c <- lastFitting 0 " \x2014 " = c
  | Just c <- lastFitting 1 ", ", T.length c * 10 >= w * 6 = c <> "\x2026"
  | otherwise = wordCut (T.words bare)
  where
    bare = tidy (dropParens t)
    -- the longest prefix ending before @sep@ that fits (with @room@ for an …)
    lastFitting room sep =
      case [p | (p, _) <- T.breakOnAll sep bare, T.length p + room <= w, not (T.null (T.strip p))] of
        [] -> Nothing
        ps -> Just (T.stripEnd (last ps))
    wordCut ws = go "" ws
      where
        go acc [] = acc
        go acc (x : xs)
          | T.length (join acc x) + 1 <= w = go (join acc x) xs
          | T.null acc = T.take (max 1 (w - 1)) x <> "\x2026"
          | otherwise = T.dropWhileEnd (`elem` (",;:" :: String)) acc <> "\x2026"
        join acc x = if T.null acc then x else acc <> " " <> x
    -- words rejoined, and no space left before a comma or a stop
    tidy x = foldr (\p -> T.replace (" " <> p) p) (T.unwords (T.words x)) [",", ".", ";", ":"]
    dropParens = T.pack . strip (0 :: Int) . T.unpack
      where
        strip _ [] = []
        strip d (c : rest)
          | c == '(' = strip (d + 1) rest
          | c == ')' && d > 0 = strip (d - 1) rest
          | d > 0 = strip d rest
          | otherwise = c : strip d rest

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

