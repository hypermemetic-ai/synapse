{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

-- | SYNAPSE-17: @synapse _find <backend> [activation…] <words…>@ — a fuzzy
-- search over a backend's methods and parameters, answered in the shape of
-- synapse's help (SYNAPSE-13): grouped, aligned, every line a command.
--
-- * The catalog is the backend's IR (the caller's view of its schema),
--   kept in synapse's IR cache under the root schema's hash: a search
--   costs one schema call — the hash check — when nothing has changed.
-- * A word matches a name exactly, as a prefix (of the name or of one of
--   its @snake_case@ segments), as a subsequence of segment prefixes
--   (@setpar@ → @set_parent@), or with a typo (edit distance ≤ 1 for 4–7
--   letters, ≤ 2 beyond); else as a word (or a word's prefix) of a
--   description. Every word must match.
-- * A hit whose words all match names is a METHOD or a PARAMETER; one that
--   needs a description is MENTIONED IN — unless its method is a named hit
--   already.
module Synapse.Find
  ( -- * The command
    FindOpts(..)
  , findInfo
  , defaultLimit

    -- * The catalog
  , loadCatalog
  , Entry(..)
  , EntryKind(..)
  , catalogEntries
  , narrowPath
  , entryLocation

    -- * Matching
  , MatchKind(..)
  , WordMatch(..)
  , matchName
  , matchDescription
  , typoLimit
  , kindScore

    -- * Search
  , Hit(..)
  , Where(..)
  , Group(..)
  , Only(..)
  , search
  , hitGroup
  , takenAs

    -- * The answer
  , Answer(..)
  , answer

    -- * Rendering
  , FindRender(..)
  , renderFind
  , renderNoMatch
  , nearest
  , hitJson
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value, object, (.=))
import Data.Char (isAlphaNum)
import Data.List (sortOn, foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

import Synapse.Algebra.Render (fitText, summarize)
import Synapse.CLI.Help (flagLabel, flagType, paramValuesOf, serverSourced, wrapWords)
import Synapse.CLI.Similarity (levenshteinDistance)
import Synapse.IR.Builder (buildIR)
import Synapse.IR.Cache (IRCache, insertIR, irCacheKey, lookupIR)
import Synapse.IR.Types
import Synapse.Monad (SynapseM, SynapseEnv(..))
import Synapse.Schema.Types (psHash)
import Synapse.Transport (fetchSchemaAt)

-- ============================================================================
-- The command
-- ============================================================================

-- | What @_find@ was asked.
data FindOpts = FindOpts
  { foBackend :: Text     -- ^ the backend, e.g. menger
  , foTerms   :: [Text]   -- ^ an activation path (optional), then the words
  , foLimit   :: Int      -- ^ at most this many hits
  , foMethods :: Bool     -- ^ only METHODS
  , foParams  :: Bool     -- ^ only PARAMETERS
  }
  deriving stock (Show, Eq)

defaultLimit :: Int
defaultLimit = 20

findInfo :: ParserInfo FindOpts
findInfo = info (p <**> helper)
  ( fullDesc
 <> header "synapse _find - search a backend's methods and parameters"
 <> progDesc "Every method and parameter whose name or description matches all the words, best first, in help's shape: METHODS, PARAMETERS, MENTIONED IN (a match only in a description). A word may be misspelled (one edit for 4-7 letters, two beyond), a prefix, or an abbreviation of a snake_case name (setpar finds set_parent). Activation names before the words narrow the search (_find menger tickets setpar). Global options (-H, -P, -j) go before _find; -j prints one JSON object per hit."
  )
  where
    p = FindOpts
      <$> (T.pack <$> strArgument (metavar "BACKEND" <> help "The hub's name, e.g. menger"))
      <*> some (T.pack <$> strArgument (metavar "WORDS..." <> help "[activation…] then the words to look for"))
      <*> option auto (long "limit" <> metavar "N" <> value defaultLimit <> showDefault <> help "At most N hits")
      <*> switch (long "methods" <> help "Only methods")
      <*> switch (long "params" <> help "Only parameters")

-- ============================================================================
-- The catalog
-- ============================================================================

-- | The backend's IR, as the caller's view of its schema has it: the root
-- schema is fetched (the hash check), and the IR kept under that hash —
-- qualified by the caller's view (AUTH-8) — is used when there is one;
-- else the tree is walked once and the IR kept for next time. A cache that
-- cannot be read or written is no cache.
loadCatalog :: Maybe IRCache -> SynapseM IR
loadCatalog mcache = do
  root <- fetchSchemaAt []
  view <- asks seViewKey
  let key = "find-" <> irCacheKey view (psHash root)
  cached <- case mcache of
    Nothing -> pure Nothing
    Just c -> liftIO (either (const Nothing) id <$> (try (lookupIR c key) :: IO (Either SomeException (Maybe IR))))
  case cached of
    Just ir -> pure ir
    Nothing -> do
      ir <- buildIR [] []
      case mcache of
        Nothing -> pure ()
        Just c -> liftIO (() <$ (try (insertIR c key ir) :: IO (Either SomeException ())))
      pure ir

data EntryKind = MethodEntry | ParamEntry
  deriving stock (Show, Eq, Ord)

-- | One thing a search can find: a method, or one of its parameters.
data Entry = Entry
  { enKind   :: EntryKind
  , enMethod :: MethodDef
  , enParam  :: Maybe ParamDef
  }
  deriving stock (Show, Eq)

-- | Every method of the IR, and every parameter a caller passes (one the
-- server supplies, REQ-8, is not a flag), under the activation path.
catalogEntries :: [Text] -> IR -> [Entry]
catalogEntries path ir =
  concat
    [ Entry MethodEntry m Nothing
        : [Entry ParamEntry m (Just p) | p <- mdParams m, not (serverSourced p)]
    | m <- Map.elems (irMethods ir)
    , under (mdNamespace m)
    ]
  where
    prefix = T.intercalate "." path
    under ns = null path || ns == prefix || (prefix <> ".") `T.isPrefixOf` ns

-- | The leading terms that name an activation (as help's path does), and
-- the words after them; at least one word is always left.
narrowPath :: IR -> [Text] -> ([Text], [Text])
narrowPath ir = go []
  where
    namespaces = Set.fromList (map mdNamespace (Map.elems (irMethods ir)))
    isPath segs =
      let p = T.intercalate "." segs
      in any (\ns -> ns == p || (p <> ".") `T.isPrefixOf` ns) (Set.toList namespaces)
    go acc (t : rest@(_ : _))
      | isPath (acc ++ [t]) = go (acc ++ [t]) rest
    go acc rest = (acc, rest)

-- | Where an entry is: its activation path and method, as typed
-- (@tickets snooze@).
entryLocation :: Entry -> Text
entryLocation = methodLocation . enMethod

methodLocation :: MethodDef -> Text
methodLocation m = T.unwords (filter (not . T.null) [T.replace "." " " (mdNamespace m), mdName m])

-- ============================================================================
-- Matching
-- ============================================================================

-- | How a word matched, best first.
data MatchKind
  = Exact            -- ^ the whole name
  | Prefix           -- ^ the start of the name
  | SegmentExact     -- ^ one @snake_case@ segment of it
  | SegmentPrefix    -- ^ the start of a segment
  | Subsequence      -- ^ prefixes of segments, in order (@setpar@)
  | Typo Int         -- ^ within this many edits of the name or a segment's start
  | InDescription    -- ^ a word of a description, or its start
  deriving stock (Show, Eq)

kindScore :: MatchKind -> Double
kindScore = \case
  Exact -> 1.0
  Prefix -> 0.9
  SegmentExact -> 0.85
  SegmentPrefix -> 0.8
  Subsequence -> 0.6
  Typo d -> 0.55 - 0.1 * fromIntegral (d - 1)
  InDescription -> 0.3

-- | A word's match: how, what it was taken as, and which characters of the
-- text it matched (offset, length).
data WordMatch = WordMatch
  { wmKind  :: MatchKind
  , wmTaken :: Text
  , wmSpans :: [(Int, Int)]
  }
  deriving stock (Show, Eq)

-- | The edits a word of this length may be off by: none under 4 letters,
-- one for 4–7, two beyond.
typoLimit :: Int -> Int
typoLimit n
  | n < 4 = 0
  | n <= 7 = 1
  | otherwise = 2

-- | A name's segments and where each starts (@set_parent@ → set@0, parent@4).
segments :: Text -> [(Int, Text)]
segments name = go 0 (T.unpack name)
  where
    go _ [] = []
    go off s =
      let (seg, rest) = break isSep s
          off' = off + length seg
      in case rest of
           [] -> [(off, T.pack seg) | not (null seg)]
           (_ : rest') -> [(off, T.pack seg) | not (null seg)] ++ go (off' + 1) rest'
    isSep c = c `elem` ("_-." :: String)

-- | Match a word (lower case) against a name.
matchName :: Text -> Text -> Maybe WordMatch
matchName w0 name0
  | T.null w = Nothing
  | w == name = Just (WordMatch Exact name0 [(0, T.length w)])
  | w `T.isPrefixOf` name = Just (WordMatch Prefix w [(0, T.length w)])
  | Just (off, _) <- find' (\(_, s) -> s == w) segs = Just (WordMatch SegmentExact w [(off, T.length w)])
  | Just (off, _) <- find' (\(_, s) -> w `T.isPrefixOf` s) segs = Just (WordMatch SegmentPrefix w [(off, T.length w)])
  | Just spans <- subsequence (T.unpack w) segs = Just (WordMatch Subsequence name0 spans)
  | otherwise = typo
  where
    w = normalise w0
    name = T.toLower name0
    segs = segments name
    find' f = listToMaybe . filter f

    -- prefixes of segments, in order, at least two of them
    subsequence ws ss = case pieces ws ss of
      Just sp@(_ : _ : _) -> Just sp
      _ -> Nothing
    pieces [] _ = Just []
    pieces _ [] = Nothing
    pieces ws ((off, s) : rest) =
      let common = length (takeWhile id (zipWith (==) ws (T.unpack s)))
          tryLen k = ((off, k) :) <$> pieces (drop k ws) rest
          viaThis = foldr (\k acc -> maybe acc Just (tryLen k)) Nothing [common, common - 1 .. 1]
      in case viaThis of
           Just sp -> Just sp
           Nothing -> pieces ws rest

    -- within the limit of the whole name or of a segment's start
    typo
      | k == 0 = Nothing
      | otherwise =
          case sortOn rank candidates of
            ((d, _, off, taken) : _) | d <= k -> Just (WordMatch (Typo d) (orig off (T.length taken)) [(off, T.length taken)])
            _ -> Nothing
      where
        n = T.length w
        k = typoLimit n
        candidates =
          [ (levenshteinDistance w p, T.length c - j, off, p)
          | (off, c) <- (0, name) : segs
          , j <- [max 1 (n - k) .. min (T.length c) (n + k)]
          , let p = T.take j c
          ]
        -- fewest edits; then the whole segment; then the longest
        rank (d, short, _, p) = (d, short, negate (T.length p))
    orig off len = T.take len (T.drop off name0)

-- | A word (lower case) as a word of a description, or a word's start:
-- the matched characters of the text. Words under 3 letters are not
-- looked for in descriptions.
matchDescription :: Text -> Text -> Maybe WordMatch
matchDescription w0 text
  | T.length w < 3 = Nothing
  | null spans = Nothing
  | otherwise = Just (WordMatch InDescription w spans)
  where
    w = normalise w0
    spans = descSpans w text

-- | Where a word starts a word of the text (offset, the word's length).
descSpans :: Text -> Text -> [(Int, Int)]
descSpans w text =
  [ (off, T.length w) | (off, tok) <- tokens text, w `T.isPrefixOf` T.toLower tok ]
  where
    -- a query word with an underscore matches across the text's underscores
    tokens t
      | T.any (== '_') w = wordsBy (\c -> isAlphaNum c || c == '_') t
      | otherwise = wordsBy isAlphaNum t

wordsBy :: (Char -> Bool) -> Text -> [(Int, Text)]
wordsBy keep t = go 0 (T.unpack t)
  where
    go _ [] = []
    go off s@(c : rest)
      | keep c = let (tok, rest') = span keep s
                 in (off, T.pack tok) : go (off + length tok) rest'
      | otherwise = go (off + 1) rest

normalise :: Text -> Text
normalise = T.replace "-" "_" . T.toLower

-- ============================================================================
-- Search
-- ============================================================================

-- | Which text a word matched.
data Where = OnMethodName | OnParamName | OnMethodDescription | OnParamDescription
  deriving stock (Show, Eq)

data Hit = Hit
  { hitEntry :: Entry
  , hitWords :: [(Text, Where, WordMatch)]   -- ^ per query word
  , hitScore :: Double
  }
  deriving stock (Show, Eq)

data Group = Methods | Parameters | Mentioned
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | A named hit is a method or a parameter; one that needed a description
-- is mentioned.
hitGroup :: Hit -> Group
hitGroup h
  | not (all named (hitWords h)) = Mentioned
  | enKind (hitEntry h) == MethodEntry = Methods
  | otherwise = Parameters
  where named (_, wh, _) = wh `elem` [OnMethodName, OnParamName]

-- | Which groups to keep.
data Only = OnlyAll | OnlyMethods | OnlyParams | OnlyBoth
  deriving stock (Show, Eq)

-- | Every hit for the words (all must match), best first: by score, then
-- the shorter path, then alphabetically. A mention of a method that is a
-- named hit already (itself or by a parameter) is left out.
search :: Only -> [Entry] -> [Text] -> [Hit]
search only entries ws = filter keep ranked
  where
    hits = mapMaybe (matchEntry (map normalise ws)) entries
    namedMethods = Set.fromList [mdFullPath (enMethod (hitEntry h)) | h <- hits, hitGroup h /= Mentioned]
    keep h = groupWanted (hitGroup h)
      && not (hitGroup h == Mentioned && Set.member (mdFullPath (enMethod (hitEntry h))) namedMethods)
    groupWanted g = case only of
      OnlyAll -> True
      OnlyMethods -> g == Methods
      OnlyParams -> g == Parameters
      OnlyBoth -> g /= Mentioned
    ranked = sortOn (\h -> (negate (hitScore h), T.length (key h), key h)) hits
    key h = entryLocation (hitEntry h) <> maybe "" ((" --" <>) . pdName) (enParam (hitEntry h))

matchEntry :: [Text] -> Entry -> Maybe Hit
matchEntry ws e = do
  matched <- mapM best ws
  let onParam = any (\(_, wh, _) -> wh `elem` [OnParamName, OnParamDescription]) matched
  if enKind e == ParamEntry && not onParam
    then Nothing
    else Just (Hit e matched (sum [kindScore (wmKind m) | (_, _, m) <- matched] / fromIntegral (length matched)))
  where
    m = enMethod e
    methodDescs = filter (not . T.null) [fromMaybe "" (mdDescription m), fromMaybe "" (mdLongDescription m)]
    best w = fmap (\(wh, wm) -> (w, wh, wm)) $ pick $ case enParam e of
      Nothing ->
        [ (OnMethodName, matchName w (mdName m)) ]
          ++ [ (OnMethodDescription, matchDescription w d) | d <- methodDescs ]
      Just p ->
        [ (OnParamName, matchName w (pdName p))
        , (OnMethodName, matchName w (mdName m)) ]
          ++ [ (OnParamDescription, matchDescription w d) | Just d <- [pdDescription p] ]
    -- the best-scoring; on a tie, the first (the parameter's own name)
    pick cs = foldl' better Nothing [(wh, x) | (wh, Just x) <- cs]
    better Nothing c = Just c
    better (Just a@(_, x)) c@(_, y)
      | kindScore (wmKind y) > kindScore (wmKind x) = Just c
      | otherwise = Just a

-- | The query as the top hit took it — a typo or an abbreviation said as
-- the name it matched — when it differs from what was typed.
takenAs :: [Hit] -> Maybe Text
takenAs [] = Nothing
takenAs (h : _)
  | taken == typed = Nothing
  | otherwise = Just (T.unwords taken)
  where
    typed = [w | (w, _, _) <- hitWords h]
    taken = [ case wmKind m of { Typo _ -> wmTaken m; Subsequence -> wmTaken m; _ -> w } | (w, _, m) <- hitWords h ]

-- ============================================================================
-- The answer
-- ============================================================================

-- | What @_find@ says: its page, or one JSON object per hit (@-j@).
data Answer = Answer
  { anMatched :: Bool      -- ^ anything found (else the page is the nearest names)
  , anText    :: Text      -- ^ the page
  , anJson    :: [Value]   -- ^ the hits, at most the limit, best first
  , anScope   :: Text      -- ^ where it looked (@menger tickets@)
  }

-- | The answer to a search: given synapse's command line (@synapse -P
-- 4455@), the backend, whether to embolden, and the IR (the caller's view).
answer :: Text -> Text -> Bool -> FindOpts -> IR -> Answer
answer program backend bold FindOpts{..} ir = Answer
  { anMatched = not (null hits)
  , anText = if null hits then renderNoMatch fr (nearest entries ws) else renderFind fr hits
  , anJson = map (hitJson ir) (take foLimit hits)
  , anScope = T.unwords (backend : path)
  }
  where
    (path, ws) = narrowPath ir foTerms
    entries = catalogEntries path ir
    only = case (foMethods, foParams) of
      (True, True) -> OnlyBoth
      (True, False) -> OnlyMethods
      (False, True) -> OnlyParams
      _ -> OnlyAll
    hits = search only entries ws
    fr = FindRender
      { frProgram = program
      , frBackend = backend
      , frPath = path
      , frQuery = ws
      , frLimit = foLimit
      , frBold = bold
      , frIR = ir
      }

-- ============================================================================
-- Rendering
-- ============================================================================

-- | What a rendering needs beside the hits.
data FindRender = FindRender
  { frProgram :: Text      -- ^ synapse and its global options (@synapse -P 4455@)
  , frBackend :: Text      -- ^ the backend (@menger@)
  , frPath    :: [Text]    -- ^ the activation path the search was narrowed to
  , frQuery   :: [Text]    -- ^ the words, as typed
  , frLimit   :: Int
  , frBold    :: Bool      -- ^ on a terminal: the matched characters in bold
  , frIR      :: IR
  }

width :: Int
width = 80

-- | A description keeps at least this many columns, past the 80 when the
-- columns before it are wide: a line too short to say anything is no help.
minDesc :: Int
minDesc = 36

-- | SYNAPSE-17: the hits in help's shape — a header (how many, for what,
-- as what), METHODS · PARAMETERS · MENTIONED IN with the locations in one
-- column and the flags in the next, the descriptions fitted to the line,
-- how many more the limit held back, and how to see the top hit's options.
renderFind :: FindRender -> [Hit] -> Text
renderFind FindRender{..} hits = T.unlines (dropTrailingBlanks body)
  where
    total = length hits
    shown = take (max 0 frLimit) hits
    scope = T.unwords (frBackend : frPath)
    query = T.unwords frQuery
    header =
      T.pack (show total) <> (if total == 1 then " match" else " matches")
        <> " for '" <> query <> "'"
        <> maybe "" (\t -> " (as '" <> t <> "')") (takenAs hits)
        <> " in " <> scope

    body = concat
      [ [header]
      , concat [ section g [h | h <- shown, hitGroup h == g] | g <- [minBound .. maxBound] ]
      , [ "" | total > length shown ]
      , [ T.pack (show (total - length shown)) <> " more: '--limit " <> T.pack (show total) <> "' shows them all."
        | total > length shown ]
      , case shown of
          (h : _) -> ["", "Run '" <> frProgram <> " " <> frBackend <> " " <> entryLocation (hitEntry h) <> " --help' for its options."]
          [] -> []
      ]

    section _ [] = []
    section g hs = "" : groupTitle g : concatMap row hs

    locW = maximum (0 : map (T.length . entryLocation . hitEntry) shown)
    col1 = 2 + locW + 4
    paramLabels = [flagLabel frIR p | h <- shown, Just p <- [enParam (hitEntry h)]]
    flagW = maximum (0 : map T.length paramLabels)
    col2 = col1 + flagW + 4

    row h = case enParam (hitEntry h) of
      Nothing ->
        let descW = max minDesc (width - col1)
        in [ T.stripEnd ("  " <> locCell h <> descCell h descW (methodText (enMethod (hitEntry h)))) ]
      Just p ->
        let descW = max minDesc (width - col2)
            label = flagLabel frIR p
            flagOff = T.length (fst (T.breakOn "--" label)) + 2
            flagSpans = [ (flagOff + o, l) | (_, OnParamName, m) <- hitWords h, (o, l) <- wmSpans m ]
            flag = styled frBold label flagSpans <> T.replicate (flagW + 4 - T.length label) " "
            values = case paramValuesOf frIR p of
              Just vs@(_ : _) -> map (T.replicate col2 " " <>) (wrapWords descW ("[values: " <> T.intercalate ", " vs <> "]"))
              _ -> []
        in T.stripEnd ("  " <> locCell h <> flag <> descCell h descW (fromMaybe "" (pdDescription p))) : values

    -- the location, the method's matched characters in bold
    locCell h =
      let e = hitEntry h
          loc = entryLocation e
          nameOff = T.length loc - T.length (mdName (enMethod e))
          spans = [ (nameOff + o, l) | (_, OnMethodName, m) <- hitWords h, (o, l) <- wmSpans m ]
      in styled frBold loc spans <> T.replicate (locW + 4 - T.length loc) " "

    -- a named hit's summary; a mention's shows where the word is
    descCell h w text =
      let descWords = [ wd | (wd, wh, _) <- hitWords h, wh `elem` [OnMethodDescription, OnParamDescription] ]
          shownText = if hitGroup h == Mentioned then snippet w descWords text (matchSource h text) else fitText w (summarize text)
          spans = concat [ descSpans (normalise wd) shownText | wd <- descWords ]
      in styled frBold shownText spans

    methodText m = case mdDescription m of
      Just d | not (T.null (T.strip d)) -> d
      _ -> fromMaybe "" (mdLongDescription m)

    -- the text a mention was found in: the one shown, else (a method's)
    -- its long description
    matchSource h shown = case (enParam (hitEntry h), mdLongDescription (enMethod (hitEntry h))) of
      (Nothing, Just long) -> shown <> "\n" <> long
      _ -> shown

groupTitle :: Group -> Text
groupTitle = \case
  Methods -> "METHODS"
  Parameters -> "PARAMETERS"
  Mentioned -> "MENTIONED IN"

-- | A description cut to show a word it was found by (in @source@, the
-- description and what else was searched): the summary when the word is in
-- it, else as much of the summary as fits, then @… word …@.
snippet :: Int -> [Text] -> Text -> Text -> Text
snippet w ws text source
  | null ws = fitText w summary
  | any (\wd -> not (null (descSpans (normalise wd) fitted))) ws = fitted
  | otherwise = case firstToken of
      Nothing -> fitted
      Just tok ->
        let tailT = " \x2026 " <> tok <> " \x2026"
            room = w - T.length tailT
            lead = T.dropWhileEnd (== '\x2026') (fitText room summary)
        in if room >= 12 then T.stripEnd lead <> tailT else fitText w ("\x2026 " <> tok <> " \x2026")
  where
    summary = summarize text
    fitted = fitText w summary
    -- the text's word (as written) where the first of the words is found
    firstToken = listToMaybe
      [ T.dropAround (`elem` ("()[]{},.;:'\"`" :: String)) whole
      | wd <- ws
      , (off, _) <- take 1 (descSpans (normalise wd) source)
      , let whole = wordAt off source
      ]
    wordAt off t =
      let before = T.takeWhileEnd (not . isSpace') (T.take off t)
          after = T.takeWhile (not . isSpace') (T.drop off t)
          isSpace' c = c == ' ' || c == '\n'
      in before <> after

-- | Text with the characters at the spans in bold (when asked).
styled :: Bool -> Text -> [(Int, Int)] -> Text
styled False t _ = t
styled True t spans
  | null marks = t
  | otherwise = T.pack (go 0 (T.unpack t))
  where
    marks = Set.fromList [ i | (o, l) <- spans, i <- [o .. o + l - 1], i >= 0, i < T.length t ]
    go _ [] = []
    go i s@(_ : _)
      | Set.member i marks =
          let (run, rest) = splitAt (runLen i) s
          in "\ESC[1m" ++ run ++ "\ESC[22m" ++ go (i + length run) rest
      | otherwise = let (c : rest) = s in c : go (i + 1) rest
    runLen i = length (takeWhile (`Set.member` marks) [i ..])

dropTrailingBlanks :: [Text] -> [Text]
dropTrailingBlanks = reverse . dropWhile (T.null . T.strip) . reverse

-- | The three methods whose names are nearest the query (by edit distance
-- to the name or to one of its segments).
nearest :: [Entry] -> [Text] -> [Entry]
nearest entries ws = take 3 (sortOn rank candidates)
  where
    methods = [e | e <- entries, enKind e == MethodEntry]
    -- the introspection every hub has (schema, hashes) is never what was meant
    candidates = case filter ((`notElem` ["schema", "hashes"]) . mdName . enMethod) methods of
      [] -> methods
      cs -> cs
    q = T.intercalate "_" (map normalise ws)
    dist e =
      let name = T.toLower (mdName (enMethod e))
      in minimum (levenshteinDistance q name : [levenshteinDistance q s | (_, s) <- segments name])
    rank e = (dist e, T.length (entryLocation e), entryLocation e)

-- | Nothing matched: say so, then the nearest names.
renderNoMatch :: FindRender -> [Entry] -> Text
renderNoMatch FindRender{..} near = T.unlines $
  [ "No match for '" <> T.unwords frQuery <> "' in " <> T.unwords (frBackend : frPath) <> "." ]
  ++ if null near then [] else
       [ "", "Nearest:" ]
       ++ [ T.stripEnd ("  " <> T.justifyLeft (locW + 4) ' ' (entryLocation e) <> fitText (max minDesc (width - (2 + locW + 4))) (summarize (desc e)))
          | e <- near ]
  where
    locW = maximum (0 : map (T.length . entryLocation) near)
    desc e = fromMaybe "" (mdDescription (enMethod e))

-- | One hit as JSON, for @-j@.
hitJson :: IR -> Hit -> Value
hitJson ir h = object $
  [ "kind" .= (case enKind e of { MethodEntry -> "method"; ParamEntry -> "param" } :: Text)
  , "path" .= mdFullPath m
  ]
  ++ case enParam e of
       Just p ->
         [ "param" .= pdName p
         , "type" .= flagType ir (pdType p)
         , "required" .= pdRequired p
         , "description" .= fromMaybe "" (pdDescription p)
         ] ++ maybe [] (\vs -> ["values" .= vs]) (paramValuesOf ir p)
       Nothing -> [ "description" .= fromMaybe "" (mdDescription m) ]
  ++
  [ "group" .= (case hitGroup h of { Methods -> "methods"; Parameters -> "parameters"; Mentioned -> "mentioned" } :: Text)
  , "score" .= (fromIntegral (round (hitScore h * 100) :: Int) / 100 :: Double)
  , "matched" .= T.unwords [ case wmKind wm of { Typo _ -> wmTaken wm; Subsequence -> wmTaken wm; _ -> w } | (w, _, wm) <- hitWords h ]
  ]
  where
    e = hitEntry h
    m = enMethod e
