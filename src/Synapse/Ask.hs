{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ASK-3: synapse answers a proposal (ASK-1's @needs_input@).
--
-- A method that asks finishes its call with a data item whose content is
-- @{"type":"needs_input","summary","effects","fields","token","expires"}@
-- and changes nothing. The answer is the same call — the same method, the
-- same params — with each field's value under its own name and
-- @confirm: token@. synapse never calls a method the server names.
--
-- * On a terminal: the summary and the effects, @[y/N]@ when there are no
--   fields, a prompt per field otherwise (a menu when it has options, its
--   default shown) and then @[y/N]@; yes makes the answer call, anything
--   else is "nothing done", exit 0.
-- * With no terminal: the proposal printed (its line under @-j@), and on
--   stderr the command that confirms it (@--confirm TOKEN@, @--FIELD ???@
--   for a field with no default), exit 'exitNeedsInput'.
-- * @--yes@ (before the service name): the answer call with each field's
--   default, when every field has one; else as with no terminal, the
--   field named.
module Synapse.Ask
  ( exitNeedsInput
  , onTerminal
  , renderProposal
  , readyCommand
  , renderValue
  , readAnswer
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (Value(..), decodeStrict, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isDigit, toLower)
import Data.List (findIndex)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO (Handle, hFlush, stdin)

import Plexus.NeedsInput (NeedsInput(..), NeedsInputField(..), answerParams)
import Synapse.CLI.Help (shellQuote)
import Synapse.Output (promptHandle, exitNeedsInput)

-- | Ask on the terminal (prompts on 'promptHandle', answers from stdin):
-- the values of the fields when the answer is yes, 'Nothing' when it is
-- no, empty or the input ended. The flag says the plan moved since the
-- last answer.
onTerminal :: Bool -> NeedsInput -> IO (Maybe [(Text, Value)])
onTerminal moved ni = do
  h <- promptHandle
  let say = TIO.hPutStrLn h
  if moved then say "synapse: the plan moved since it was proposed; this is what it is now:" else pure ()
  case niFields ni of
    [] -> do
      mapM_ (say . effectLine) (niEffects ni)
      yes <- confirm h (niSummary ni)
      pure (if yes then Just [] else Nothing)
    fields -> do
      say (niSummary ni)
      mapM_ (say . effectLine) (niEffects ni)
      vals <- askFields h fields
      case vals of
        Nothing -> pure Nothing
        Just vs -> do
          yes <- confirm h ("go ahead with " <> T.intercalate ", " [ k <> " = " <> renderValue v | (k, v) <- vs ])
          pure (if yes then Just vs else Nothing)

-- | @? MESSAGE  [y/N] @ — yes is @y@ or @yes@, anything else (and the end
-- of the input) is no.
confirm :: Handle -> Text -> IO Bool
confirm h msg = do
  TIO.hPutStr h ("? " <> msg <> "  [y/N] ")
  hFlush h
  l <- readLine
  pure $ case l of
    Just t -> map toLower (T.unpack (T.strip t)) `elem` ["y", "yes"]
    Nothing -> False

askFields :: Handle -> [NeedsInputField] -> IO (Maybe [(Text, Value)])
askFields h = go []
  where
    go acc [] = pure (Just (reverse acc))
    go acc (f : fs) = do
      v <- askField h f
      case v of
        Nothing -> pure Nothing
        Just x -> go ((nifName f, x) : acc) fs

-- | One field: a menu when it has options (a number or the option itself;
-- empty is the default), else a line (empty is the default; a field with
-- none is asked again). The end of the input is 'Nothing'.
askField :: Handle -> NeedsInputField -> IO (Maybe Value)
askField h f = case nifOptions f of
  Just opts@(_ : _) -> do
    TIO.hPutStrLn h ("  " <> nifName f <> maybe "" (" \x2014 " <>) (nifHelp f) <> ":")
    let defIx = nifDefault f >>= \d -> findIndex (== d) opts
    mapM_ (\(i, o) -> TIO.hPutStrLn h ("    " <> T.pack (show i) <> ") " <> renderValue o
                                        <> (if Just (i - 1) == defIx then " (default)" else "")))
          (zip [1 :: Int ..] opts)
    let prompt = "  " <> nifName f <> maybe "" (\i -> " [" <> T.pack (show (i + 1)) <> "]") defIx <> ": "
        loop = do
          TIO.hPutStr h prompt
          hFlush h
          readLine >>= \case
            Nothing -> pure Nothing
            Just t -> case pickOption opts (nifDefault f) (T.strip t) of
              Just v -> pure (Just v)
              Nothing -> TIO.hPutStrLn h ("  one of 1.." <> T.pack (show (length opts))) >> loop
    loop
  _ -> do
    let prompt = "  " <> nifName f <> maybe "" (\hp -> " (" <> hp <> ")") (nifHelp f)
                   <> maybe "" (\d -> " [" <> renderValue d <> "]") (nifDefault f) <> ": "
        loop = do
          TIO.hPutStr h prompt
          hFlush h
          readLine >>= \case
            Nothing -> pure Nothing
            Just t
              | T.null (T.strip t), Just d <- nifDefault f -> pure (Just d)
              | T.null (T.strip t) -> TIO.hPutStrLn h ("  " <> nifName f <> " has no default: give a value") >> loop
              | otherwise -> pure (Just (readAnswer (nifDefault f) (T.strip t)))
    loop

-- | A menu's answer: empty is the default, a number its option, else the
-- option written out.
pickOption :: [Value] -> Maybe Value -> Text -> Maybe Value
pickOption opts def t
  | T.null t = def
  | T.all isDigit t, n <- read (T.unpack t), n >= 1, n <= length opts = Just (opts !! (n - 1))
  | otherwise = case filter ((== t) . renderValue) opts of
      v : _ -> Just v
      [] -> Nothing

-- | A typed answer as a value: JSON when the field's default is not a
-- string (a number, a boolean, a list), else the text as it is.
readAnswer :: Maybe Value -> Text -> Value
readAnswer def t = case def of
  Just (String _) -> String t
  Just _ | Just v <- decodeStrict (TE.encodeUtf8 t) -> v
  _ -> String t

readLine :: IO (Maybe Text)
readLine = either (\(_ :: IOException) -> Nothing) Just <$> try (TIO.hGetLine stdin)

-- | A value as a person reads it: a string bare, anything else JSON.
renderValue :: Value -> Text
renderValue = \case
  String t -> t
  v -> TE.decodeUtf8 (LBS.toStrict (encode v))

effectLine :: Value -> Text
effectLine v = "  \x00b7 " <> renderValue v

-- | The proposal as the plain printer shows it, when no one can be asked.
renderProposal :: NeedsInput -> Text
renderProposal ni = T.unlines $
  [ "needs input: " <> niSummary ni ]
  ++ map effectLine (niEffects ni)
  ++ concatMap field (niFields ni)
  ++ [ "expires " <> e | Just e <- [niExpires ni] ]
  where
    field f =
      [ "  --" <> kebab (nifName f) <> " "
          <> maybe "" (\os -> T.intercalate "|" (map renderValue os) <> " ") (nifOptions f)
          <> maybe "(no default)" (\d -> "(default " <> renderValue d <> ")") (nifDefault f)
          <> maybe "" (" \x2014 " <>) (nifHelp f) ]

-- | The command that confirms the proposal: this run's own arguments
-- with @--confirm TOKEN@ and each field not already given — its default,
-- or @???@ where it has none. A run that gave its params with @-p@ gets
-- them back whole in its @-p@, the token and the fields in them. An
-- earlier @--confirm@ (a stale token) is replaced.
readyCommand :: Text -> [Text] -> Value -> NeedsInput -> Text
readyCommand prog argv params ni = T.unwords (prog : rendered)
  where
    rendered = case splitParams (dropConfirm argv) of
      Just (before, after) ->
        let fieldVals = [ (nifName f, maybe (String "???") id (nifDefault f)) | f <- niFields ni ]
            json = TE.decodeUtf8 (LBS.toStrict (encode (answerParams params fieldVals (niToken ni))))
        in map shellQuote before ++ ["-p", shellQuote json] ++ map shellQuote after
      Nothing ->
        map shellQuote (dropConfirm argv)
          ++ ["--confirm", shellQuote (niToken ni)]
          ++ concat [ ["--" <> kebab (nifName f), maybe "???" (shellQuote . renderValue) (nifDefault f)]
                    | f <- niFields ni, not (given (nifName f)) ]
    given name = any (\a -> a `elem` flags name || any (`T.isPrefixOf` a) (map (<> "=") (flags name))) argv
    flags name = ["--" <> kebab name, "--" <> name]

-- | The arguments without a @--confirm@ (and its value).
dropConfirm :: [Text] -> [Text]
dropConfirm = \case
  [] -> []
  "--confirm" : _ : rest -> dropConfirm rest
  a : rest | "--confirm=" `T.isPrefixOf` a -> dropConfirm rest
  a : rest -> a : dropConfirm rest

-- | The arguments before and after a @-p@ / @--params@ and its value.
splitParams :: [Text] -> Maybe ([Text], [Text])
splitParams = go []
  where
    go _ [] = Nothing
    go acc (a : v : rest) | a `elem` ["-p", "--params"] = Just (reverse acc, rest) <* Just v
    go acc (a : rest)
      | "--params=" `T.isPrefixOf` a = Just (reverse acc, rest)
      | "-p" `T.isPrefixOf` a, T.length a > 2, not ("--" `T.isPrefixOf` a) = Just (reverse acc, rest)
      | otherwise = go (a : acc) rest

kebab :: Text -> Text
kebab = T.replace "_" "-"
