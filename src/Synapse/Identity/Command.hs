{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | IDENTITY-43: the command lines.
--
-- @
-- synapse _key new [NAME] [--store auto|file|keychain]   -- NAME defaults to "default"; never overwrites
-- synapse _key show [NAME]                                -- key:\<hex\>
-- synapse _key list
-- synapse [-H HOST] -P PORT _login BACKEND [--key NAME] --as AGENT [--scope role\@SCOPE …] [--ttl SECS]
-- synapse [-H HOST] -P PORT _logout BACKEND [--as AGENT]
-- @
--
-- @_key@ is local and runs here; @_login@ / @_logout@ are parsed here and
-- run by the executable, which knows how to reach a backend.
module Synapse.Identity.Command
  ( -- * _key
    KeyCommand(..)
  , keyCommandInfo
  , runKeyCommand

    -- * _login / _logout
  , LoginOpts(..)
  , loginInfo
  , LogoutOpts(..)
  , logoutInfo
  , splitScopes
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit (ExitCode(..))
import System.IO (stderr)

import Synapse.Identity.Proof
import Synapse.Identity.Store

-- ============================================================================
-- _key
-- ============================================================================

data KeyCommand
  = KeyNew Text StorePref
  | KeyShow Text
  | KeyList
  deriving (Eq, Show)

keyCommandInfo :: ParserInfo KeyCommand
keyCommandInfo = info (parser <**> helper)
  ( fullDesc
 <> header "synapse _key - the Ed25519 keys an agent proves who it is with"
 <> progDesc "Kept in the OS keychain when one answers, else ~/.plexus/keys/NAME.key (0600). Read in the order: PLEXUS_KEY (base64 seed), keychain, $CREDENTIALS_DIRECTORY/NAME.key, ~/.plexus/keys/NAME.key."
  )
  where
    parser = hsubparser
      ( command "new" (info newP (progDesc "Make a key (NAME defaults to 'default'); prints key:<hex>. Never overwrites."))
     <> command "show" (info showP (progDesc "Print a key's principal, key:<hex>"))
     <> command "list" (info (pure KeyList) (progDesc "Every key synapse can find, and where"))
      )
    nameArg = T.pack <$> strArgument (metavar "NAME" <> value "default" <> showDefault)
    newP = KeyNew <$> nameArg <*> option storeR
      ( long "store" <> metavar "auto|file|keychain" <> value StoreAuto
     <> help "Where to keep it (default auto: the keychain when it answers, else the file)" )
    showP = KeyShow <$> nameArg
    storeR = eitherReader $ \case
      "auto" -> Right StoreAuto
      "file" -> Right StoreFile
      "keychain" -> Right StoreKeychain
      s -> Left ("--store is auto, file or keychain, not " <> s)

runKeyCommand :: StoreEnv -> KeyCommand -> IO ExitCode
runKeyCommand st = \case
  KeyNew name pref
    | not (validKeyName name) -> bad ("a key name is 1-64 of A-Z a-z 0-9 . _ - (not '" <> name <> "')")
    | otherwise -> keyExists st name >>= \case
        Just src -> bad ("key " <> name <> " already exists (" <> renderKeySource src <> "); synapse never overwrites a key")
        Nothing -> do
          seed <- generateSeed
          writeKey st pref name seed >>= \case
            Left e -> bad e
            Right src -> do
              TIO.putStrLn (principalOf seed)
              TIO.hPutStrLn stderr ("synapse: key " <> name <> " kept in " <> renderKeySource src)
              pure ExitSuccess
  KeyShow name -> lookupKey st name >>= \case
    Left e -> bad e
    Right Nothing -> bad ("no key " <> name <> " (synapse _key new " <> name <> ")")
    Right (Just (seed, _)) -> ExitSuccess <$ TIO.putStrLn (principalOf seed)
  KeyList -> do
    keys <- listKeys st
    rows <- mapM describe keys
    if null rows
      then TIO.hPutStrLn stderr "synapse: no keys (synapse _key new)"
      else mapM_ TIO.putStrLn (align' rows)
    pure ExitSuccess
  where
    bad msg = ExitFailure 1 <$ TIO.hPutStrLn stderr ("synapse: " <> msg)
    describe (name, src) = do
      -- the principal from the store that listed it (PLEXUS_KEY would
      -- answer for every name, so it is not asked here)
      p <- lookupKey st { stEnv = \k -> if k == "PLEXUS_KEY" then pure Nothing else stEnv st k } name
      let principal = case p of
            Right (Just (seed, _)) -> principalOf seed
            Right Nothing -> "?"
            Left e -> "(" <> e <> ")"
      pure (name, principal, renderKeySource src)
    align' rows =
      let w = maximum [T.length n | (n, _, _) <- rows]
      in [T.justifyLeft w ' ' n <> "  " <> p <> "  " <> s | (n, p, s) <- rows]

-- ============================================================================
-- _login / _logout
-- ============================================================================

data LoginOpts = LoginOpts
  { loBackend :: Text
  , loKey     :: Text
  , loAs      :: Maybe Text
  , loScope   :: [Text]
  , loTtl     :: Maybe Integer
  } deriving (Eq, Show)

loginInfo :: ParserInfo LoginOpts
loginInfo = info (p <**> helper)
  ( fullDesc
 <> header "synapse _login - log in from a key; later calls to this backend carry the session by themselves"
 <> progDesc "Proves the key (a plexus-key proof at the upgrade), asks session.login, and keeps the session (keychain, else ~/.plexus/sessions/HOST_PORT/AGENT.json). The agent becomes this backend's default. Global options (-H, -P) go before _login."
  )
  where
    p = LoginOpts
      <$> (T.pack <$> strArgument (metavar "BACKEND" <> help "The hub's name, e.g. menger"))
      <*> (T.pack <$> strOption (long "key" <> short 'k' <> metavar "NAME" <> value "default" <> showDefault <> help "The key to prove"))
      <*> optional (T.pack <$> strOption (long "as" <> metavar "AGENT" <> help "The agent id to log in as (else SYNAPSE_AS)"))
      <*> (splitScopes <$> many (T.pack <$> strOption (long "scope" <> metavar "ROLE@SCOPE" <> help "The session's ceiling, e.g. editor@MENGER-12 (repeat, or commas); default: the key's own grants")))
      <*> optional (option auto (long "ttl" <> metavar "SECS" <> help "Session lifetime in seconds (the server's default 12 h, at most 24 h)"))

data LogoutOpts = LogoutOpts
  { lgBackend :: Text
  , lgAs      :: Maybe Text
  } deriving (Eq, Show)

logoutInfo :: ParserInfo LogoutOpts
logoutInfo = info (p <**> helper)
  ( fullDesc
 <> header "synapse _logout - end a session (session.logout) and forget it"
  )
  where
    p = LogoutOpts
      <$> (T.pack <$> strArgument (metavar "BACKEND" <> help "The hub's name, e.g. menger"))
      <*> optional (T.pack <$> strOption (long "as" <> metavar "AGENT" <> help "Which agent's session (else SYNAPSE_AS, else the backend's default)"))

-- | @--scope a,b --scope c@ → @[a, b, c]@.
splitScopes :: [Text] -> [Text]
splitScopes = filter (not . T.null) . map T.strip . concatMap (T.splitOn ",")
