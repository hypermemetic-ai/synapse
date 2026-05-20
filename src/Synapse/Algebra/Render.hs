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
import Data.List (intersperse, sortOn)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Synapse.Schema.Types
import Plexus.Schema.Recursive
  ( DeprecationInfo(..)
  , ParamSchema(..)
  , MethodRole(..)
  )

-- | Rendering style configuration
data RenderStyle = RenderStyle
  { rsIndent    :: !Int   -- ^ Spaces per indent level
  , rsShowHash  :: !Bool  -- ^ Show plugin/method hashes
  , rsShowTypes :: !Bool  -- ^ Show parameter types
  , rsCompact   :: !Bool  -- ^ Compact single-line format
  }
  deriving stock (Show, Eq)

-- | Default style: readable multi-line output
defaultStyle :: RenderStyle
defaultStyle = RenderStyle
  { rsIndent    = 2
  , rsShowHash  = False
  , rsShowTypes = True
  , rsCompact   = False
  }

-- | Compact style: single-line descriptions
compactStyle :: RenderStyle
compactStyle = defaultStyle { rsCompact = True }

-- | Render a PluginSchema
renderSchema :: PluginSchema -> Text
renderSchema = renderSchemaWith defaultStyle

-- | Render with custom style
renderSchemaWith :: RenderStyle -> PluginSchema -> Text
renderSchemaWith _style PluginSchema{..}
  | null psMethods && maybe True null psChildren = deprecationBanner <> headerText
  | otherwise = renderStrict $ layoutPretty layoutOpts doc
  where
    layoutOpts = LayoutOptions (AvailablePerLine 80 1.0)

    -- Activation-level deprecation banner (plain-text, rendered ahead
    -- of the compact header variant used for trivial schemas).
    deprecationBanner = case psDeprecation of
      Just di -> deprecationMarker <> " " <> formatDeprecationLine di <> "\n"
      Nothing -> ""

    headerText = psNamespace <> " v" <> psVersion <> "\n" <> psDescription <> "\n"

    -- Name line; decorated with the warning marker when the activation
    -- carries Just DeprecationInfo.
    namespaceHeading = case psDeprecation of
      Just _  -> pretty (deprecationMarker <> " " <> psNamespace)
                   <+> pretty ("v" <> psVersion)
      Nothing -> pretty psNamespace <+> pretty ("v" <> psVersion)

    -- Full deprecation notice rendered immediately below the heading
    -- when relevant, before the description block.
    deprecationNoticeDoc = case psDeprecation of
      Just di -> vsep
        [ emptyDoc
        , indent 2 $ pretty (formatDeprecationLine di)
        ]
      Nothing -> emptyDoc

    doc :: Doc ann
    doc = vsep
      [ namespaceHeading
      , deprecationNoticeDoc
      , emptyDoc
      , indent 2 $ align $ fillSep $ map pretty $ T.words psDescription
      , emptyDoc
      , requestDoc      -- REQ-5: render psRequest schema (auth requirements + request fields)
      , childrenDoc
      , methodsDoc
      ]

    -- REQ-5: render the activation's PlexusRequest schema.
    -- Walks the JSON Schema's properties and emits a "Request requirements:"
    -- block describing where each field comes from (cookie/header/query/derived).
    requestDoc = case psRequest of
      Nothing  -> emptyDoc
      Just req -> renderRequestSchemaDoc req

    renderRequestSchemaDoc :: Aeson.Value -> Doc ann
    renderRequestSchemaDoc schemaVal =
      let mProps = case schemaVal of
            Aeson.Object o -> KM.lookup "properties" o
            _              -> Nothing
          mReq = case schemaVal of
            Aeson.Object o -> KM.lookup "required" o
            _              -> Nothing
          requiredNames = case mReq of
            Just (Aeson.Array v) -> [n | Aeson.String n <- V.toList v]
            _                    -> []
          propPairs = case mProps of
            Just (Aeson.Object o) -> KM.toList o
            _                     -> []
          hasCookieAuth = any
            (\(k, v) -> case v of
              Aeson.Object pv ->
                let from = KM.lookup "x-plexus-source" pv >>= \src -> case src of
                      Aeson.Object sv -> KM.lookup "from" sv
                      _               -> Nothing
                    key = KM.lookup "x-plexus-source" pv >>= \src -> case src of
                      Aeson.Object sv -> KM.lookup "key" sv
                      _               -> Nothing
                in from == Just (Aeson.String "cookie")
                   && key == Just (Aeson.String "access_token")
                   && K.toText k `elem` requiredNames
              _ -> False)
            propPairs
          authNotice =
            if hasCookieAuth
              then [ pretty ("Authentication required" :: Text)
                       <+> pretty ("(use --token <jwt>, --cookie access_token=<jwt>, or SYNAPSE_TOKEN)" :: Text)
                   , emptyDoc ]
              else []
          fieldDocs = if null propPairs
                        then []
                        else [ pretty ("Request requirements:" :: Text)
                             , emptyDoc
                             , indent 2 $ vsep $ map (renderRequestFieldDoc requiredNames) propPairs
                             , emptyDoc ]
      in vsep (authNotice ++ fieldDocs)

    renderRequestFieldDoc :: [Text] -> (K.Key, Aeson.Value) -> Doc ann
    renderRequestFieldDoc requiredNames (k, v) =
      let name = K.toText k
          isReq = name `elem` requiredNames
          (label, sourceKeyText, isDerived) = case v of
            Aeson.Object pv ->
              let src = KM.lookup "x-plexus-source" pv
                  fromVal = src >>= \s -> case s of
                    Aeson.Object so -> KM.lookup "from" so
                    _               -> Nothing
                  keyVal = src >>= \s -> case s of
                    Aeson.Object so -> KM.lookup "key" so
                    _               -> Nothing
                  fromTxt = case fromVal of
                    Just (Aeson.String t) -> t
                    _                     -> "unknown"
                  keyTxt = case keyVal of
                    Just (Aeson.String t) -> " (" <> t <> ")"
                    _                     -> ""
                  lbl = case fromTxt of
                    "cookie"  -> "Cookie"
                    "header"  -> "Header"
                    "query"   -> "QueryParam"
                    "derived" -> "Server-derived"
                    _         -> "Unknown"
              in (lbl, keyTxt, fromTxt == "derived")
            _ -> ("Unknown", "", False)
          desc = case v of
            Aeson.Object pv -> case KM.lookup "description" pv of
              Just (Aeson.String t) -> " — " <> t
              _                     -> ""
            _ -> ""
          reqMark = if isReq && not isDerived then " required" else " optional"
      in pretty (label <> " " <> name <> sourceKeyText <> reqMark <> desc)

    childrenDoc = case psChildren of
      Nothing -> emptyDoc
      Just [] -> emptyDoc
      Just children -> vsep
        [ pretty ("activations" :: Text)
        , emptyDoc
        , indent 2 $ vsep $ map renderChildDoc (sortOn csNamespace children)
        , emptyDoc
        ]

    methodsDoc
      | null psMethods = emptyDoc
      | otherwise = vsep
        [ pretty ("methods" :: Text)
        , emptyDoc
        , indent 2 $ vsep $ intersperse emptyDoc $ map renderMethodDoc (sortOn methodName psMethods)
        ]

    renderChildDoc child = fillBreak 12 (pretty $ csNamespace child)
      <+> align (fillSep $ map pretty $ T.words $ csDescription child)

    -- SYN-2: render a dynamic-child gate as
    --
    --   body {name}  Look up a planet by name
    --                (dynamic child gate; list: list_planets, search: search_planets)
    --                --name <string>
    --
    -- Ordinary (MethodRoleRpc / MethodRoleStaticChild) methods render
    -- byte-identically to pre-SYN-2 — back-compat is preserved by
    -- branching on 'methodRole'.
    renderMethodDoc method =
      let (decoratedName, dynLines) = decorateForRole method
          nameText = case methodDeprecation method of
            Just _  -> deprecationMarker <> " " <> decoratedName
            Nothing -> decoratedName
          nameLine = fillBreak 12 (pretty nameText)
                       <+> align (fillSep $ map pretty $ T.words $ methodDescription method)
          depLines = case methodDeprecation method of
            Just di -> [indent 12 $ pretty (formatDeprecationLine di)]
            Nothing -> []
      in vsep $ [nameLine] ++ depLines ++ dynLines ++ paramsDocs (methodParams method)

    -- SYN-2: extract the role-specific decoration. Returns the
    -- decorated method-name token (e.g. @"body {name}"@) and any
    -- additional annotation lines to be inserted between the
    -- deprecation block and the param block.
    --
    -- For every role *other than* 'MethodRoleDynamicChild' the result
    -- is @(methodName, [])@ — the original pre-SYN-2 behavior, so
    -- existing non-hub and legacy-hub activations (Arbor, Cone,
    -- ClaudeCode, …) render byte-identically. The decoration is
    -- additive on top of dynamic gates only.
    decorateForRole method' =
      let baseName = methodName method'
      in case methodRole method' of
        MethodRoleDynamicChild { } ->
          let paramTok = "{" <> dynamicGateParamName method' <> "}"
              decorated = baseName <> " " <> paramTok
              hint = renderDynamicHint method'
          in (decorated, [indent 12 $ pretty hint])
        _ ->
          (baseName, [])

    -- SYN-2: build the one-line "(dynamic child gate; list: …,
    -- search: …)" annotation. Both fields are skipped when absent.
    renderDynamicHint method' =
      let suffix = case methodRole method' of
            MethodRoleDynamicChild lm sm ->
              let parts = case lm of { Just n -> ["list: " <> n]; Nothing -> [] }
                       <> case sm of { Just n -> ["search: " <> n]; Nothing -> [] }
              in if null parts
                   then ""
                   else "; " <> T.intercalate ", " parts
            _ -> ""
      in "(dynamic child gate" <> suffix <> ")"

    -- SYN-2: scan a method's JSON Schema params to pick a sensible
    -- placeholder name. We mirror the heuristic in
    -- 'Synapse.DynamicChildren.gateParamName' (single property →
    -- that property's key; else @"name"@). Inlined here so the
    -- render module remains a leaf — Render must not depend on the
    -- consumer module.
    dynamicGateParamName method' = case methodParams method' of
      Just (Object o) -> case KM.lookup "properties" o of
        Just (Object props) -> case KM.keys props of
          [k] -> K.toText k
          ks  -> case filter (/= "_self") (map K.toText ks) of
            [k] -> k
            _   -> "name"
        _ -> "name"
      _ -> "name"

    paramsDocs Nothing = []
    paramsDocs (Just (Object o)) = case KM.lookup "properties" o of
      Just (Object props) ->
        let reqList = case KM.lookup "required" o of
              Just (Array arr) -> [t | String t <- foldr (:) [] arr]
              _ -> []
            propList = KM.toList props
            sorted = sortOn (\(k, _) -> (K.toText k `notElem` reqList, K.toText k)) propList
        in [indent 12 $ vsep $ map (renderParamDoc reqList) sorted]
      _ -> []
    paramsDocs _ = []

    renderParamDoc :: [Text] -> (K.Key, Value) -> Doc ann
    renderParamDoc required (name, propSchema) =
      let nameText = K.toText name
          isReq = nameText `elem` required
          (typ, desc) = extractTypeDesc propSchema
          srcAnnot = extractParamSourceAnnot propSchema
          -- REQ-8: when a param has x-plexus-source, it's server-extracted —
          -- don't emit a --flag form (clients can't supply it). Show its name
          -- + source annotation inline instead.
      in case srcAnnot of
           Just annotation ->
             fillBreak 20 (pretty nameText)
               <+> align (pretty ("← " :: Text) <> pretty annotation)
           Nothing ->
             let flag = "--" <> T.replace "_" "-" nameText
                 typStr = " <" <> typ <> ">" <> if isReq then "" else "?"
                 descWords = if T.null desc then [] else map pretty (T.words desc)
             in fillBreak 20 (pretty flag <> pretty typStr)
                <+> align (fillSep descWords)

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

-- | Extract type and description from property schema
extractTypeDesc :: Value -> (Text, Text)
extractTypeDesc (Object po) =
  ( case KM.lookup "type" po of { Just (String t) -> t; _ -> "any" }
  , case KM.lookup "description" po of { Just (String d) -> d; _ -> "" }
  )
extractTypeDesc _ = ("any", "")

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

