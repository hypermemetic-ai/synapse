{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IDENTITY-43: where an agent's key and its sessions are kept — IDENTITY-40's
-- "Stores (synapse)", exactly.
--
-- Key @NAME@, read in this order:
--
-- 1. env @PLEXUS_KEY@ (base64 of the 32-byte seed)
-- 2. the OS keychain (@secret-tool@ attributes @service=plexus-key name=NAME@;
--    macOS @security … -s plexus-key -a NAME@)
-- 3. @$CREDENTIALS_DIRECTORY/NAME.key@ (a systemd service's credentials)
-- 4. @~\/.plexus\/keys\/NAME.key@ (0600)
--
-- Session for (backend @HOST:PORT@, agent):
--
-- 1. env @PLEXUS_SESSION@ (the token itself)
-- 2. the keychain (@service=plexus-session backend=HOST:PORT agent=AGENT@)
-- 3. @~\/.plexus\/sessions\/HOST_PORT\/AGENT.json@ (0600)
--
-- SCAFFOLD-21: a backend reached any other way than plain
-- @ws://HOST:PORT/@ is keyed by its URL — @backend=wss://HOST:443/ws@ in the
-- keychain, @~\/.plexus\/sessions\/wss_HOST_443_ws\/AGENT.json@ on disk —
-- so a ws and a wss backend on one host keep sessions apart, and every
-- session stored before is found where it was.
--
-- Writes go to the keychain when it answers, else the file. The keychain
-- is asked through its command-line tool with a 2 s limit, and a host with
-- no session bus (a server) has none — so a headless machine falls through
-- to the files at once. @PLEXUS_KEYCHAIN=off@ skips it outright.
--
-- Everything is reached through a 'StoreEnv', so tests give it a directory,
-- an environment and a keychain of their own.
module Synapse.Identity.Store
  ( -- * The environment
    StoreEnv(..)
  , systemStoreEnv
  , Keychain(..)
  , KcResult(..)
  , systemKeychain
  , memoryKeychain

    -- * Keys
  , KeySource(..)
  , renderKeySource
  , lookupKey
  , StorePref(..)
  , writeKey
  , keyExists
  , listKeys
  , keyFilePath

    -- * Sessions
  , SessionRec(..)
  , SessionSource(..)
  , lookupSession
  , writeSession
  , removeSession
  , sessionFilePath
  , addressKey
  , readDefaultAgent
  , writeDefaultAgent
  , clearDefaultAgent

  , withSessionLock

    -- * Files
  , writePrivateFile
  ) where

import Control.Exception (SomeException, try, catch, finally, onException)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), (.!=), object, withObject)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.List (nub, sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeExtension, dropExtension, takeFileName)
import System.IO (hClose, openBinaryTempFile)
import qualified System.Info as Info
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Files (setFileMode)
import System.Posix.IO (createFile, closeFd)
import Foreign.C.Types (CInt(..))
import Foreign.C.Error (throwErrnoIfMinus1RetryMayBlock_)
import System.Process (proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

import Synapse.Identity.Proof
import Synapse.Endpoint (Endpoint)
import qualified Synapse.Endpoint as E

-- ============================================================================
-- Environment
-- ============================================================================

-- | What the stores read and write through.
data StoreEnv = StoreEnv
  { stEnv      :: String -> IO (Maybe String)  -- ^ the process environment
  , stPlexus   :: FilePath                     -- ^ @~/.plexus@
  , stKeychain :: Maybe Keychain               -- ^ the OS keychain, when there is one
  }

-- | The real thing: the process environment, @~/.plexus@ and the system
-- keychain (if it is there).
systemStoreEnv :: IO StoreEnv
systemStoreEnv = do
  home <- getHomeDirectory
  kc <- systemKeychain
  pure StoreEnv { stEnv = lookupEnv, stPlexus = home </> ".plexus", stKeychain = kc }

-- | A keychain item is a service and the attributes that name it.
data KcResult = KcFound Text | KcAbsent | KcUnavailable Text
  deriving (Eq, Show)

data Keychain = Keychain
  { kcLabel  :: Text
  , kcLookup :: Text -> [(Text, Text)] -> IO KcResult
  , kcStore  :: Text -> Text -> [(Text, Text)] -> Text -> IO (Either Text ())  -- ^ label, service, attrs, secret
  , kcClear  :: Text -> [(Text, Text)] -> IO ()
  , kcNames  :: Text -> Text -> IO [Text]  -- ^ service, attribute → the values items hold
  }

-- | Once the keychain has failed to answer in this process, it is not
-- asked again (each ask may cost the 2 s limit).
{-# NOINLINE keychainDown #-}
keychainDown :: IORef Bool
keychainDown = unsafePerformIO (newIORef False)

-- | The system keychain, or none: none when @PLEXUS_KEYCHAIN@ is off, when
-- its tool is not installed, or (Linux) when there is no session bus to
-- reach a secret service on.
systemKeychain :: IO (Maybe Keychain)
systemKeychain = do
  off <- lookupEnv "PLEXUS_KEYCHAIN"
  if fmap (T.toLower . T.pack) off `elem` map Just ["0", "off", "no", "false", "none", "file"]
    then pure Nothing
    else case Info.os of
      "darwin" -> fmap (const macKeychain) <$> findExecutable "security"
      "linux" -> do
        tool <- findExecutable "secret-tool"
        bus <- sessionBus
        pure $ if isJust tool && bus then Just secretTool else Nothing
      _ -> pure Nothing
  where
    sessionBus = do
      addr <- lookupEnv "DBUS_SESSION_BUS_ADDRESS"
      case addr of
        Just a | not (null a) -> pure True
        _ -> lookupEnv "XDG_RUNTIME_DIR" >>= \case
          Just d -> doesPathExist (d </> "bus")
          Nothing -> pure False

-- | Run a keychain tool with the 2 s limit: its exit, stdout and stderr, or
-- why it did not answer.
runTool :: FilePath -> [String] -> String -> IO (Either Text (ExitCode, String, String))
runTool exe args input = do
  down <- readIORef keychainDown
  if down then pure (Left "the keychain did not answer earlier") else do
    r <- try (timeout 2000000 (readCreateProcessWithExitCode (proc exe args) input))
    case r of
      Left (e :: SomeException) -> markDown ("could not run " <> T.pack exe <> ": " <> T.pack (show e))
      Right Nothing -> markDown (T.pack exe <> " did not answer within 2 s")
      Right (Just out) -> pure (Right out)
  where
    markDown msg = writeIORef keychainDown True >> pure (Left msg)

attrArgs :: Text -> [(Text, Text)] -> [String]
attrArgs service attrs = concat [[T.unpack k, T.unpack v] | (k, v) <- ("service", service) : attrs]

-- | Linux: @secret-tool@ over the Secret Service.
secretTool :: Keychain
secretTool = Keychain
  { kcLabel = "keychain (secret-tool)"
  , kcLookup = \service attrs -> do
      r <- runTool "secret-tool" ("lookup" : attrArgs service attrs) ""
      pure $ case r of
        Left msg -> KcUnavailable msg
        Right (ExitSuccess, out, _) -> KcFound (T.strip (T.pack out))
        -- exit 1 with nothing said: the service answered, no such item
        Right (ExitFailure 1, _, err) | all (`elem` (" \n\t" :: String)) err -> KcAbsent
        Right (_, _, err) -> KcUnavailable (T.strip (T.pack err))
  , kcStore = \label service attrs secret -> do
      r <- runTool "secret-tool" (["store", "--label=" <> T.unpack label] <> attrArgs service attrs) (T.unpack secret)
      pure $ case r of
        Left msg -> Left msg
        Right (ExitSuccess, _, _) -> Right ()
        Right (_, _, err) -> Left (T.strip (T.pack err))
  , kcClear = \service attrs -> () <$ runTool "secret-tool" ("clear" : attrArgs service attrs) ""
  , kcNames = \service attr -> do
      r <- runTool "secret-tool" ["search", "--all", "service", T.unpack service] ""
      pure $ case r of
        Right (ExitSuccess, out, err) ->
          -- secret-tool prints the attributes on stdout or stderr by version
          [ T.strip v
          | l <- T.lines (T.pack out <> "\n" <> T.pack err)
          , Just v <- [T.stripPrefix ("attribute." <> attr <> " = ") (T.strip l)]
          ]
        _ -> []
  }

-- | macOS: @security@ generic passwords; the account is the attribute
-- values joined with @/@ (a key's: its name).
macKeychain :: Keychain
macKeychain = Keychain
  { kcLabel = "keychain (macOS)"
  , kcLookup = \service attrs -> do
      r <- runTool "security" ["find-generic-password", "-s", T.unpack service, "-a", account attrs, "-w"] ""
      pure $ case r of
        Left msg -> KcUnavailable msg
        Right (ExitSuccess, out, _) -> KcFound (T.strip (T.pack out))
        Right (ExitFailure 44, _, _) -> KcAbsent
        Right (_, _, err) -> KcUnavailable (T.strip (T.pack err))
  , kcStore = \label service attrs secret -> do
      r <- runTool "security" ["add-generic-password", "-U", "-l", T.unpack label, "-s", T.unpack service, "-a", account attrs, "-w", T.unpack secret] ""
      pure $ case r of
        Left msg -> Left msg
        Right (ExitSuccess, _, _) -> Right ()
        Right (_, _, err) -> Left (T.strip (T.pack err))
  , kcClear = \service attrs -> () <$ runTool "security" ["delete-generic-password", "-s", T.unpack service, "-a", account attrs] ""
  , kcNames = \_ _ -> pure []  -- `security` has no cheap listing; keys in files are still listed
  }
  where account = T.unpack . T.intercalate "/" . map snd

-- | An in-memory keychain, for tests.
memoryKeychain :: IO (Keychain, IORef (Map.Map (Text, [(Text, Text)]) Text))
memoryKeychain = do
  ref <- newIORef Map.empty
  let kc = Keychain
        { kcLabel = "keychain (memory)"
        , kcLookup = \s a -> maybe KcAbsent KcFound . Map.lookup (s, a) <$> readIORef ref
        , kcStore = \_ s a v -> Right () <$ modifyIORef' ref (Map.insert (s, a) v)
        , kcClear = \s a -> modifyIORef' ref (Map.delete (s, a))
        , kcNames = \s attr -> do
            m <- readIORef ref
            pure [v | ((s', as), _) <- Map.toList m, s' == s, Just v <- [lookup attr as]]
        }
  pure (kc, ref)

-- ============================================================================
-- Keys
-- ============================================================================

data KeySource = KeyFromEnv | KeyFromKeychain | KeyFromCredentials FilePath | KeyFromFile FilePath
  deriving (Eq, Show)

renderKeySource :: KeySource -> Text
renderKeySource = \case
  KeyFromEnv -> "PLEXUS_KEY"
  KeyFromKeychain -> "keychain"
  KeyFromCredentials p -> "credentials " <> T.pack p
  KeyFromFile p -> "file " <> T.pack p

keyAttrs :: Text -> [(Text, Text)]
keyAttrs name = [("name", name)]

keyFilePath :: StoreEnv -> Text -> FilePath
keyFilePath st name = stPlexus st </> "keys" </> (T.unpack name <> ".key")

-- | Key @NAME@ in the spec's order, or why it is not there (Left when a
-- store holds something that is not a key).
lookupKey :: StoreEnv -> Text -> IO (Either Text (Maybe (Seed, KeySource)))
lookupKey st name = do
  envKey <- stEnv st "PLEXUS_KEY"
  case envKey of
    Just v | not (null v) -> pure $ either (Left . ("PLEXUS_KEY: " <>)) (\s -> Right (Just (s, KeyFromEnv))) (decodeSeedText (T.pack v))
    _ -> do
      fromKc <- case stKeychain st of
        Nothing -> pure Nothing
        Just kc -> kcLookup kc "plexus-key" (keyAttrs name) >>= \case
          KcFound v -> pure (Just (decodeSeedText v, KeyFromKeychain))
          _ -> pure Nothing
      case fromKc of
        Just (Right s, src) -> pure (Right (Just (s, src)))
        Just (Left e, _) -> pure (Left ("the keychain's plexus-key " <> name <> ": " <> e))
        Nothing -> do
          creds <- stEnv st "CREDENTIALS_DIRECTORY"
          fromCreds <- case creds of
            Just d | not (null d) -> readKeyFile (d </> (T.unpack name <> ".key")) KeyFromCredentials
            _ -> pure (Right Nothing)
          case fromCreds of
            Right Nothing -> readKeyFile (keyFilePath st name) KeyFromFile
            other -> pure other
  where
    readKeyFile p mk = do
      exists <- doesFileExist p
      if not exists then pure (Right Nothing) else do
        bytes <- BS.readFile p
        -- base64 text, or the 32 raw bytes
        let parsed = case decodeSeedText (TE.decodeLatin1 bytes) of
              Right s -> Right s
              Left _ -> seedFromBytes bytes
        pure $ case parsed of
          Right s -> Right (Just (s, mk p))
          Left e -> Left (T.pack p <> ": " <> e)

-- | Where a new key goes.
data StorePref = StoreAuto | StoreFile | StoreKeychain
  deriving (Eq, Show)

-- | Is there already a key by this name in a store synapse writes (the
-- keychain or the file)?
keyExists :: StoreEnv -> Text -> IO (Maybe KeySource)
keyExists st name = do
  inKc <- case stKeychain st of
    Just kc -> kcLookup kc "plexus-key" (keyAttrs name) >>= \case
      KcFound _ -> pure True
      _ -> pure False
    Nothing -> pure False
  if inKc then pure (Just KeyFromKeychain) else do
    let p = keyFilePath st name
    e <- doesFileExist p
    pure (if e then Just (KeyFromFile p) else Nothing)

-- | Keep a key: the keychain when it answers (and is wanted), else the file.
writeKey :: StoreEnv -> StorePref -> Text -> Seed -> IO (Either Text KeySource)
writeKey st pref name seed = do
  let secret = encodeSeedText seed
      toFile = do
        let p = keyFilePath st name
        writePrivateFile p (TE.encodeUtf8 (secret <> "\n"))
        pure (Right (KeyFromFile p))
  case (pref, stKeychain st) of
    (StoreFile, _) -> toFile
    (StoreKeychain, Nothing) -> pure (Left "no keychain answers here")
    (_, Just kc) -> do
      r <- kcStore kc ("plexus key " <> name) "plexus-key" (keyAttrs name) secret
      case r of
        Right () -> pure (Right KeyFromKeychain)
        Left e | pref == StoreKeychain -> pure (Left ("the keychain refused: " <> e))
               | otherwise -> toFile
    (StoreAuto, Nothing) -> toFile

-- | Every key synapse can see by name: (name, where).
listKeys :: StoreEnv -> IO [(Text, KeySource)]
listKeys st = do
  kcNamesFound <- case stKeychain st of
    Just kc -> kcNames kc "plexus-key" "name"
    Nothing -> pure []
  creds <- stEnv st "CREDENTIALS_DIRECTORY"
  credNames <- case creds of
    Just d | not (null d) -> keyFilesIn d
    _ -> pure []
  fileNames <- keyFilesIn (stPlexus st </> "keys")
  let kcs = [(n, KeyFromKeychain) | n <- nub kcNamesFound]
      cs = [(n, KeyFromCredentials p) | (n, p) <- credNames, n `notElem` map fst kcs]
      fs = [(n, KeyFromFile p) | (n, p) <- fileNames, n `notElem` map fst kcs, n `notElem` map fst cs]
  pure (kcs <> cs <> fs)
  where
    keyFilesIn d = do
      e <- doesDirectoryExist d
      if not e then pure [] else do
        fs <- sort <$> listDirectory d
        pure [(T.pack (dropExtension f), d </> f) | f <- fs, takeExtension f == ".key"]

-- ============================================================================
-- Sessions
-- ============================================================================

-- | A session as synapse keeps it: the token, who it is, until when, and
-- what it was made from (the key's name, the scope and ttl asked) so it
-- can be made again.
data SessionRec = SessionRec
  { srToken     :: Text
  , srPrincipal :: Text
  , srAgent     :: Text
  , srExpiresAt :: Maybe Integer  -- ^ unix seconds
  , srKey       :: Text           -- ^ the key's name in the key store
  , srScope     :: [Text]         -- ^ as asked at login (empty: the key's own)
  , srTtl       :: Maybe Integer  -- ^ as asked at login
  , srBackend   :: Text           -- ^ the hub's name (its @<name>.call@)
  } deriving (Eq, Show)

instance ToJSON SessionRec where
  toJSON s = object $
    [ "token" .= srToken s
    , "principal" .= srPrincipal s
    , "agent" .= srAgent s
    , "expires_at" .= srExpiresAt s
    , "key" .= srKey s
    , "scope" .= srScope s
    , "backend" .= srBackend s
    ] <> maybe [] (\t -> ["ttl_secs" .= t]) (srTtl s)

instance FromJSON SessionRec where
  parseJSON = withObject "session" $ \o -> SessionRec
    <$> o .: "token"
    <*> o .:? "principal" .!= ""
    <*> o .: "agent"
    <*> o .:? "expires_at"
    <*> o .:? "key" .!= "default"
    <*> o .:? "scope" .!= []
    <*> o .:? "ttl_secs"
    <*> o .:? "backend" .!= ""

data SessionSource = SessionFromEnv | SessionFromKeychain | SessionFromFile FilePath
  deriving (Eq, Show)

-- | @HOST:PORT@, the backend a session belongs to (SCAFFOLD-21: the
-- whole URL for an address that is not plain @ws://HOST:PORT/@ — see
-- 'Synapse.Endpoint.addressKey').
addressKey :: Endpoint -> Text
addressKey = E.addressKey

sessionDir :: StoreEnv -> Endpoint -> FilePath
sessionDir st ep = stPlexus st </> "sessions" </> E.dirName ep

sessionFilePath :: StoreEnv -> Endpoint -> Text -> FilePath
sessionFilePath st ep agent = sessionDir st ep </> (T.unpack agent <> ".json")

sessionAttrs :: Endpoint -> Text -> [(Text, Text)]
sessionAttrs ep agent = [("backend", addressKey ep), ("agent", agent)]

-- | The session for this backend and agent. @PLEXUS_SESSION@ is the token
-- alone: no record, nothing to sign in again from.
lookupSession :: StoreEnv -> Endpoint -> Text -> IO (Maybe (Either Text SessionRec, SessionSource))
lookupSession st ep agent = do
  envTok <- stEnv st "PLEXUS_SESSION"
  case envTok of
    Just t | not (null t) -> pure (Just (Left (T.strip (T.pack t)), SessionFromEnv))
    _ -> do
      fromKc <- case stKeychain st of
        Nothing -> pure Nothing
        Just kc -> kcLookup kc "plexus-session" (sessionAttrs ep agent) >>= \case
          KcFound v -> pure (A.decodeStrict (TE.encodeUtf8 v))
          _ -> pure Nothing
      case fromKc of
        Just r -> pure (Just (Right r, SessionFromKeychain))
        Nothing -> do
          let p = sessionFilePath st ep agent
          e <- doesFileExist p
          if not e then pure Nothing else do
            r <- A.decodeFileStrict p
            pure ((\s -> (Right s, SessionFromFile p)) <$> r)

-- | Keep a session: the keychain when it answers, else the file.
writeSession :: StoreEnv -> Endpoint -> SessionRec -> IO SessionSource
writeSession st ep s = do
  let json = TE.decodeUtf8 (LBS.toStrict (A.encode s))
      p = sessionFilePath st ep (srAgent s)
      toFile = do
        writePrivateFile p (LBS.toStrict (A.encode s))
        pure (SessionFromFile p)
  case stKeychain st of
    Nothing -> toFile
    Just kc -> do
      r <- kcStore kc ("plexus session " <> addressKey ep <> " " <> srAgent s)
             "plexus-session" (sessionAttrs ep (srAgent s)) json
      case r of
        Right () -> do
          -- a stale file would shadow nothing (the keychain is read first)
          -- but would outlive a logout's intent; drop it
          removeIfExists p
          pure SessionFromKeychain
        Left _ -> toFile

-- | Forget a session, wherever it is kept.
removeSession :: StoreEnv -> Endpoint -> Text -> IO ()
removeSession st ep agent = do
  mapM_ (\kc -> kcClear kc "plexus-session" (sessionAttrs ep agent)) (stKeychain st)
  removeIfExists (sessionFilePath st ep agent)

defaultAgentPath :: StoreEnv -> Endpoint -> FilePath
defaultAgentPath st ep = sessionDir st ep </> "default"

-- | The agent a backend's calls go as when none is named.
readDefaultAgent :: StoreEnv -> Endpoint -> IO (Maybe Text)
readDefaultAgent st ep = do
  let p = defaultAgentPath st ep
  e <- doesFileExist p
  if not e then pure Nothing else do
    t <- T.strip <$> TIO.readFile p
    pure (if validAgent t then Just t else Nothing)

writeDefaultAgent :: StoreEnv -> Endpoint -> Text -> IO ()
writeDefaultAgent st ep agent =
  writePrivateFile (defaultAgentPath st ep) (TE.encodeUtf8 (agent <> "\n"))

-- | Drop the default when it names this agent.
clearDefaultAgent :: StoreEnv -> Endpoint -> Text -> IO ()
clearDefaultAgent st ep agent = do
  cur <- readDefaultAgent st ep
  when (cur == Just agent) $ removeIfExists (defaultAgentPath st ep)

-- ============================================================================
-- Files
-- ============================================================================

-- | Write a file only its owner can read: its directory 0700, the file
-- 0600 from the moment it exists, replaced atomically.
--
-- SYNAPSE-15: each writer has a temporary file of its own
-- (@NAME.tmpNNN@, created with O_EXCL and 0600 by 'openBinaryTempFile'),
-- renamed over the target; parallel writers never touch each other's
-- file, and the last rename wins. Before, every writer used @NAME.tmp@,
-- and one removed another's between its write and its rename.
writePrivateFile :: FilePath -> BS.ByteString -> IO ()
writePrivateFile p bytes = do
  let dir = takeDirectory p
  privateDir dir
  (tmp, h) <- openBinaryTempFile dir (takeFileName p <> ".tmp")
  (do setFileMode tmp 0o600
      BS.hPut h bytes
      hClose h
      renameFile tmp p)
    `onException` (hClose h >> removeIfExists tmp)

foreign import ccall safe "sys/file.h flock" c_flock :: CInt -> CInt -> IO CInt

-- | flock's LOCK_EX.
lockEx :: CInt
lockEx = 2

-- | A directory only its owner can enter, made if missing.
privateDir :: FilePath -> IO ()
privateDir dir = do
  existed <- doesDirectoryExist dir
  createDirectoryIfMissing True dir
  unless existed $ setFileMode dir 0o700

-- | SYNAPSE-15: hold the agent's session lock at this backend (an advisory
-- lock on @AGENT.lock@ beside its session) for the action — so of the
-- processes that find the session stale at once, one signs in again and
-- the others, waiting here, find its new session when they read again.
withSessionLock :: StoreEnv -> Endpoint -> Text -> IO a -> IO a
withSessionLock st ep agent act = do
  let lockPath = sessionDir st ep </> (T.unpack agent <> ".lock")
  -- flock on a descriptor of our own (not a GHC handle: GHC refuses a
  -- second writer of one file within a process): processes and threads
  -- alike wait their turn; closing the descriptor lets the next one in
  opened <- try (privateDir (takeDirectory lockPath) >> createFile lockPath 0o600)
  case opened of
    -- a lock that cannot be had (a read-only store) is no reason to fail
    -- the call: it goes on unserialised, as before
    Left (_ :: SomeException) -> act
    Right fd -> flip finally (closeFd fd) $ do
      throwErrnoIfMinus1RetryMayBlock_ "flock" (c_flock (fromIntegral fd) lockEx) (pure ())
      act

removeIfExists :: FilePath -> IO ()
removeIfExists p = removeFile p `catch` \(_ :: SomeException) -> pure ()
