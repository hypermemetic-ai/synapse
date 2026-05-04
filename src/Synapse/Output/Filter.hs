{-# LANGUAGE PatternSynonyms #-}

-- | Stream-shaping renderer (OUTPUT-2).
--
-- The @ShapedJson@ renderer is the first non-default 'StreamRenderer' in
-- the dispatch established by "Synapse.Output.Renderer". It plugs in
-- when any of the new shaping flags are set:
--
-- * @--select foo,bar@   — projection
-- * @--filter EXPR@      — predicate filter
-- * @--head N@           — keep the first N data items
-- * @--tail N@           — keep the last N data items (bounded ring)
-- * @--count@            — drop all data items, emit @{"count": N}@ at end
--
-- All five shape only 'HubData' items. Non-data items
-- ('HubProgress', 'HubError', 'HubDone', 'HubRequest', 'HubGuidance')
-- pass through unchanged so progress/error/done semantics are preserved.
--
-- == Strong-typed contracts
--
-- 'FieldName' and 'FilterExpr' are newtypes over 'Text' so that the
-- distinction between "raw user input" and "validated CLI value" stays
-- visible at type sites.
--
-- 'Format' is the dispatch tag the CLI parser produces. Adding a new
-- format (e.g. OUTPUT-3's @Table@) is additive — new constructor, new
-- module, no changes here.
--
-- == Filter grammar (subset of jq-style)
--
-- > expr     ::= and_expr ('||' and_expr)*
-- > and_expr ::= cmp ('&&' cmp)*
-- > cmp      ::= field op literal | '(' expr ')' | '!' expr
-- > op       ::= '==' | '!='
-- > literal  ::= '"text"' | number | true | false | null
-- > field    ::= identifier ('.' identifier)*
--
-- Anything outside this grammar is a CLI parse error pointing at the
-- column of the failure. Suggested workaround: pipe to @jq@.
module Synapse.Output.Filter
  ( -- * Strong-typed contracts
    FieldName(..)
  , FilterExpr(..)
  , ShapeOpts(..)
  , defaultShapeOpts
  , Format(..)

    -- * Filter AST and parser
  , Predicate(..)
  , Cmp(..)
  , CmpOp(..)
  , Literal(..)
  , parseFilter
  , evalPredicate
  , ParseFilterError(..)
  , renderParseFilterError

    -- * Renderer
  , shapedJsonRenderer

    -- * Default tail buffer cap
  , defaultTailCap

    -- * Projection (exposed for tests)
  , projectFields
  ) where

import Control.Monad (when)
import Data.Aeson (Value(..), encode, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.IORef
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import Synapse.Schema.Types
  ( HubStreamItem
  , pattern HubData
  )

import Synapse.Output.Renderer (StreamRenderer(..))

-- ============================================================================
-- Strong-typed contracts
-- ============================================================================

-- | A JSON field name referenced by @--select@ or in a @--filter@ predicate.
--
-- Wrapping in a newtype keeps "field path passed by the user" distinct
-- from arbitrary 'Text' at type sites.
newtype FieldName = FieldName { unFieldName :: Text }
  deriving stock (Show, Eq, Ord, Generic)

-- | The raw text of the predicate passed to @--filter@.
--
-- This is the unparsed form. Parse it with 'parseFilter' to get a
-- 'Predicate' AST.
newtype FilterExpr = FilterExpr { unFilterExpr :: Text }
  deriving stock (Show, Eq, Generic)

-- | Stream-shaping options bundled together.
--
-- 'soSelect' is empty when @--select@ is absent. 'soFilter' is
-- 'Nothing' when @--filter@ is absent. 'soHead' and 'soTail' are
-- mutually exclusive and validated at the CLI parser layer.
-- 'soCount' is mutually exclusive with all of the above.
data ShapeOpts = ShapeOpts
  { soSelect  :: ![FieldName]       -- ^ Projection field list (empty = no projection)
  , soFilter  :: !(Maybe Predicate) -- ^ Parsed filter predicate
  , soHead    :: !(Maybe Int)       -- ^ Limit: keep first N data items
  , soTail    :: !(Maybe Int)       -- ^ Limit: keep last N data items
  , soCount   :: !Bool              -- ^ Aggregate: emit only {"count": N} at end
  , soTailCap :: !Int               -- ^ Bounded-ring upper bound for --tail
  }
  deriving stock (Show, Eq, Generic)

-- | Default shape options: pass-through.
defaultShapeOpts :: ShapeOpts
defaultShapeOpts = ShapeOpts
  { soSelect  = []
  , soFilter  = Nothing
  , soHead    = Nothing
  , soTail    = Nothing
  , soCount   = False
  , soTailCap = defaultTailCap
  }

-- | Default cap for the @--tail@ ring buffer. Items beyond this cap are
-- still tracked; the buffer keeps the most-recent 'defaultTailCap'
-- items and emits a one-shot warning on stderr the first time the cap
-- is exceeded.
defaultTailCap :: Int
defaultTailCap = 10000

-- | The output format dispatch tag.
--
-- New formats (e.g. @Table@) extend this sum without modifying any of
-- the renderers in this module — they live in their own modules and
-- produce a 'StreamRenderer' via their own builder.
data Format
  = FormatJson                  -- ^ Existing NDJSON pass-through
  | FormatShapedJson !ShapeOpts -- ^ NDJSON shaped by select/filter/head/tail/count
  | FormatRaw                   -- ^ Raw content JSON (existing --raw)
  | FormatTemplate              -- ^ Template-based (existing default)
  deriving stock (Show, Eq, Generic)

-- ============================================================================
-- Filter AST
-- ============================================================================

-- | A boolean predicate over a 'HubData' content value.
data Predicate
  = PCmp !Cmp                    -- ^ Atomic comparison
  | PNot !Predicate              -- ^ Negation
  | PAnd !Predicate !Predicate   -- ^ Conjunction
  | POr  !Predicate !Predicate   -- ^ Disjunction
  deriving stock (Show, Eq, Generic)

-- | Atomic field-vs-literal comparison.
data Cmp = Cmp
  { cmpField :: ![Text]   -- ^ Dotted field path (e.g. @["foo","bar"]@)
  , cmpOp    :: !CmpOp
  , cmpLit   :: !Literal
  }
  deriving stock (Show, Eq, Generic)

-- | Comparison operator.
data CmpOp = OpEq | OpNeq
  deriving stock (Show, Eq, Generic)

-- | Literal values that may appear on the right-hand side of a comparison.
data Literal
  = LitString !Text
  | LitNumber !Double
  | LitBool   !Bool
  | LitNull
  deriving stock (Show, Eq, Generic)

-- ============================================================================
-- Filter evaluation
-- ============================================================================

-- | Evaluate a predicate against a 'HubData' content value.
--
-- Field-not-present at the requested path is treated as @null@ on the
-- left-hand side, so e.g. @missing == "x"@ is false and
-- @missing != "x"@ is true.
evalPredicate :: Predicate -> Value -> Bool
evalPredicate p v = case p of
  PCmp c    -> evalCmp c v
  PNot q    -> not (evalPredicate q v)
  PAnd a b  -> evalPredicate a v && evalPredicate b v
  POr  a b  -> evalPredicate a v || evalPredicate b v

evalCmp :: Cmp -> Value -> Bool
evalCmp Cmp{..} v =
  let lhs = lookupPath cmpField v
      eq  = literalEqValue cmpLit lhs
  in case cmpOp of
       OpEq  -> eq
       OpNeq -> not eq

-- | Walk a dotted field path through a JSON value.
-- Returns 'Null' if any segment is missing or the parent is non-object.
lookupPath :: [Text] -> Value -> Value
lookupPath []     v = v
lookupPath (k:ks) (Object o) =
  case KM.lookup (K.fromText k) o of
    Just v  -> lookupPath ks v
    Nothing -> Null
lookupPath _ _ = Null

literalEqValue :: Literal -> Value -> Bool
literalEqValue lit val = case (lit, val) of
  (LitNull,     Null)       -> True
  (LitBool b,   Bool b')    -> b == b'
  (LitString s, String s')  -> s == s'
  (LitNumber n, Number sci) ->
    -- Compare via Double for predictable behavior with literal numbers.
    let d = toRealFloat sci :: Double
    in n == d
  _                         -> False

-- ============================================================================
-- Field projection
-- ============================================================================

-- | Project a content 'Value' against a list of selected fields.
--
-- * If the value is a JSON object, return a new object containing each
--   selected field with its original value, or 'Null' if absent.
-- * Non-object content (string, number, array, null, bool) passes
--   through unchanged.
-- * If the selection list is empty, the value passes through unchanged.
projectFields :: [FieldName] -> Value -> Value
projectFields []  v = v
projectFields fns v = case v of
  Object o -> Object $ KM.fromList
    [ ( K.fromText (unFieldName f)
      , maybe Null id (KM.lookup (K.fromText (unFieldName f)) o)
      )
    | f <- fns
    ]
  other -> other

-- ============================================================================
-- Filter grammar parser
-- ============================================================================

-- | A parse error in a @--filter@ expression. Carries the 1-based
-- column at which the failure was detected and a short message.
data ParseFilterError = ParseFilterError
  { pfeColumn  :: !Int
  , pfeMessage :: !Text
  }
  deriving stock (Show, Eq)

-- | Render a 'ParseFilterError' as a human-readable string with a
-- one-line example pointing at the failing column.
renderParseFilterError :: Text -> ParseFilterError -> String
renderParseFilterError src ParseFilterError{..} = unlines
  [ "Filter parse error at column " <> show pfeColumn <> ": " <> T.unpack pfeMessage
  , "  " <> T.unpack src
  , "  " <> replicate (max 0 (pfeColumn - 1)) ' ' <> "^"
  , "Supported grammar: field == \"value\" | field != value | && || ! ( )"
  , "For richer expressions, pipe NDJSON to `jq`."
  ]

-- | Parser state: the remaining input and the absolute 1-based column
-- of its first character relative to the original source.
data PState = PState { psInput :: !Text, psCol :: !Int }
  deriving stock (Show)

-- | Parse a @--filter@ expression. Consumes the entire input.
parseFilter :: Text -> Either ParseFilterError Predicate
parseFilter src =
  let s0 = PState src 1
      s1 = skipSpaces s0
  in case parseExpr s1 of
       Left e -> Left e
       Right (p, s2) ->
         let s3 = skipSpaces s2
         in if T.null (psInput s3)
              then Right p
              else Left $ ParseFilterError (psCol s3) "unexpected trailing input"

-- | expr ::= and_expr ('||' and_expr)*
parseExpr :: PState -> Either ParseFilterError (Predicate, PState)
parseExpr s0 = do
  (lhs, s1) <- parseAndExpr s0
  go lhs s1
  where
    go acc s =
      let s' = skipSpaces s
      in case T.stripPrefix "||" (psInput s') of
           Just rest -> do
             let s2 = PState rest (psCol s' + 2)
             (rhs, s3) <- parseAndExpr s2
             go (POr acc rhs) s3
           Nothing -> Right (acc, s)

-- | and_expr ::= cmp ('&&' cmp)*
parseAndExpr :: PState -> Either ParseFilterError (Predicate, PState)
parseAndExpr s0 = do
  (lhs, s1) <- parseCmpAtom s0
  go lhs s1
  where
    go acc s =
      let s' = skipSpaces s
      in case T.stripPrefix "&&" (psInput s') of
           Just rest -> do
             let s2 = PState rest (psCol s' + 2)
             (rhs, s3) <- parseCmpAtom s2
             go (PAnd acc rhs) s3
           Nothing -> Right (acc, s)

-- | cmp_atom ::= '!' cmp_atom | '(' expr ')' | field op literal
parseCmpAtom :: PState -> Either ParseFilterError (Predicate, PState)
parseCmpAtom s0 =
  let s = skipSpaces s0
      input = psInput s
  in case T.uncons input of
       Nothing -> Left $ ParseFilterError (psCol s) "expected expression"
       Just ('!', rest) -> do
         (sub, s2) <- parseCmpAtom (PState rest (psCol s + 1))
         Right (PNot sub, s2)
       Just ('(', rest) -> do
         (sub, s2) <- parseExpr (PState rest (psCol s + 1))
         let s3 = skipSpaces s2
         case T.uncons (psInput s3) of
           Just (')', rest2) -> Right (sub, PState rest2 (psCol s3 + 1))
           _ -> Left $ ParseFilterError (psCol s3) "expected ')'"
       _ -> do
         (fld, s1) <- parseField s
         let s2 = skipSpaces s1
         (op, s3) <- parseOp s2
         let s4 = skipSpaces s3
         (lit, s5) <- parseLiteral s4
         Right (PCmp (Cmp fld op lit), s5)

-- | identifier ('.' identifier)*
parseField :: PState -> Either ParseFilterError ([Text], PState)
parseField s0 = do
  (ident, s1) <- parseIdentifier s0
  go [ident] s1
  where
    go acc s =
      case T.uncons (psInput s) of
        Just ('.', rest) -> do
          (ident, s') <- parseIdentifier (PState rest (psCol s + 1))
          go (acc ++ [ident]) s'
        _ -> Right (acc, s)

-- | identifier ::= [A-Za-z_][A-Za-z0-9_]*
parseIdentifier :: PState -> Either ParseFilterError (Text, PState)
parseIdentifier s0 =
  case T.uncons (psInput s0) of
    Just (c, _) | isIdentStart c ->
      let (ident, rest) = T.span isIdentChar (psInput s0)
          col' = psCol s0 + T.length ident
      in Right (ident, PState rest col')
    _ -> Left $ ParseFilterError (psCol s0) "expected field identifier"
  where
    isIdentStart c = isAlpha c || c == '_'
    isIdentChar c  = isAlphaNum c || c == '_'

-- | op ::= '==' | '!='
parseOp :: PState -> Either ParseFilterError (CmpOp, PState)
parseOp s0
  | Just rest <- T.stripPrefix "==" (psInput s0)
      = Right (OpEq,  PState rest (psCol s0 + 2))
  | Just rest <- T.stripPrefix "!=" (psInput s0)
      = Right (OpNeq, PState rest (psCol s0 + 2))
  | otherwise
      = Left $ ParseFilterError (psCol s0) "expected '==' or '!='"

-- | literal ::= '"text"' | number | true | false | null
parseLiteral :: PState -> Either ParseFilterError (Literal, PState)
parseLiteral s0 =
  case T.uncons (psInput s0) of
    Nothing -> Left $ ParseFilterError (psCol s0) "expected literal"
    Just ('"', rest) -> parseStringLit rest (psCol s0 + 1)
    Just (c, _)
      | isDigit c || c == '-' -> parseNumberLit s0
      | otherwise -> parseKeywordLit s0

parseStringLit :: Text -> Int -> Either ParseFilterError (Literal, PState)
parseStringLit input col0 = go [] input col0
  where
    go acc t col = case T.uncons t of
      Nothing -> Left $ ParseFilterError col "unterminated string literal"
      Just ('"', rest) ->
        Right (LitString (T.pack (reverse acc)), PState rest (col + 1))
      Just ('\\', rest) -> case T.uncons rest of
        Nothing -> Left $ ParseFilterError (col + 1) "trailing backslash in string"
        Just (e, rest2) ->
          let unescaped = case e of
                'n'  -> '\n'
                't'  -> '\t'
                '"'  -> '"'
                '\\' -> '\\'
                _    -> e
          in go (unescaped : acc) rest2 (col + 2)
      Just (c, rest) -> go (c : acc) rest (col + 1)

parseNumberLit :: PState -> Either ParseFilterError (Literal, PState)
parseNumberLit s0 =
  let (numTxt, rest) = T.span isNumChar (psInput s0)
      col' = psCol s0 + T.length numTxt
  in case reads (T.unpack numTxt) :: [(Double, String)] of
       [(d, "")] -> Right (LitNumber d, PState rest col')
       _         -> Left $ ParseFilterError (psCol s0) "expected number literal"
  where
    isNumChar c = isDigit c || c == '.' || c == '-' || c == 'e' || c == 'E' || c == '+'

parseKeywordLit :: PState -> Either ParseFilterError (Literal, PState)
parseKeywordLit s0
  | Just rest <- T.stripPrefix "true"  inp
  , notFollowedByIdent rest
      = Right (LitBool True,  PState rest (psCol s0 + 4))
  | Just rest <- T.stripPrefix "false" inp
  , notFollowedByIdent rest
      = Right (LitBool False, PState rest (psCol s0 + 5))
  | Just rest <- T.stripPrefix "null"  inp
  , notFollowedByIdent rest
      = Right (LitNull,       PState rest (psCol s0 + 4))
  | otherwise
      = Left $ ParseFilterError (psCol s0)
              "expected literal: \"text\", number, true, false, or null"
  where
    inp = psInput s0
    notFollowedByIdent t = case T.uncons t of
      Just (c, _) -> not (isAlphaNum c || c == '_')
      Nothing     -> True

skipSpaces :: PState -> PState
skipSpaces s =
  let (ws, rest) = T.span isSpace (psInput s)
  in PState rest (psCol s + T.length ws)

-- ============================================================================
-- Renderer: ShapedJson
-- ============================================================================

-- | Build the @ShapedJson@ stream renderer.
--
-- Apply order on each 'HubData' item:
--
--   1. Filter (predicate). If false, drop.
--   2. Projection (@--select@). On non-object content, this is a no-op.
--   3. Limiting:
--        * @--head N@ — count emitted items; once N reached, drop further.
--        * @--tail N@ — buffer items; emit the last N at finalization.
--        * @--count@  — never emit per-item; emit @{"count": N}@ at finalization.
--
-- Non-data items pass through unchanged regardless of options
-- (so progress/error/done semantics aren't broken).
shapedJsonRenderer :: ShapeOpts -> IO StreamRenderer
shapedJsonRenderer opts@ShapeOpts{..} = do
  -- Per-item counter for --head and --count.
  emittedRef   <- newIORef (0 :: Int)
  -- Ring buffer for --tail. The buffer is bounded by 'soTailCap';
  -- exceeding the cap emits a one-shot stderr warning.
  tailBufRef   <- newIORef ([] :: [Value])
  tailLenRef   <- newIORef (0 :: Int)
  capWarnedRef <- newIORef False

  let onItem item = case item of
        HubData h prov ct content ->
          let keep = case soFilter of
                Nothing -> True
                Just p  -> evalPredicate p content
          in if not keep
              then pure ()
              else do
                let projected = projectFields soSelect content
                    rebuilt   = HubData h prov ct projected
                if soCount
                  then bumpCount emittedRef
                  else case (soTail, soHead) of
                    (Just n, _) ->
                      pushTail opts tailBufRef tailLenRef capWarnedRef rebuilt n
                    (Nothing, Just n) -> do
                      n' <- readIORef emittedRef
                      when (n' < n) $ do
                        emitItem rebuilt
                        modifyIORef' emittedRef (+ 1)
                    (Nothing, Nothing) ->
                      emitItem rebuilt
        -- Non-data items always pass through unchanged.
        other -> emitItem other

      onEnd = do
        when soCount $ do
          n <- readIORef emittedRef
          LBS.putStrLn $ encode $ object [ K.fromText "count" .= n ]
          hFlush stdout
        case soTail of
          Just _ -> flushTail tailBufRef
          Nothing -> pure ()

  pure StreamRenderer
    { rrOnItem = onItem
    , rrOnEnd  = onEnd
    }

emitItem :: HubStreamItem -> IO ()
emitItem item = do
  LBS.putStrLn $ encode item
  hFlush stdout

bumpCount :: IORef Int -> IO ()
bumpCount ref = modifyIORef' ref (+ 1)

-- | Push a 'HubData' (already filtered + projected) onto the tail ring
-- buffer of length @n@. If the buffer would exceed 'soTailCap',
-- warn once on stderr and continue with truncation.
pushTail
  :: ShapeOpts
  -> IORef [Value]   -- buffer (most recent at head)
  -> IORef Int       -- buffer length
  -> IORef Bool      -- have we warned about cap exceedance?
  -> HubStreamItem
  -> Int             -- requested tail size (N)
  -> IO ()
pushTail ShapeOpts{soTailCap = cap} bufRef lenRef warnedRef item n = do
  let effective = min n cap
  when (n > cap) $ do
    warned <- readIORef warnedRef
    when (not warned) $ do
      hPutStrLn stderr $ "[synapse] --tail " <> show n <>
        " exceeds buffer cap (" <> show cap <> "); keeping the most-recent " <>
        show cap <> " items only."
      writeIORef warnedRef True
  let asValue = toJSON item
  buf <- readIORef bufRef
  ln  <- readIORef lenRef
  let buf' = asValue : take (effective - 1) buf
      ln'  = min (ln + 1) effective
  writeIORef bufRef buf'
  writeIORef lenRef ln'

-- | Flush the tail ring buffer to stdout. Items were pushed
-- most-recent-first so we reverse to emit in arrival order.
flushTail :: IORef [Value] -> IO ()
flushTail bufRef = do
  buf <- readIORef bufRef
  mapM_ (\v -> LBS.putStrLn (encode v)) (reverse buf)
  hFlush stdout
