{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

-- | IDENTITY-43: logging in from a key, and the session every later call
-- carries by itself.
--
-- * 'loginWithKey' opens a connection presenting a fresh proof
--   (@Authorization: Bearer plexus-key.…@) and asks @session.login@ on it.
-- * 'sessionFor' finds the session a call to @HOST:PORT@ goes with — the
--   agent named (@--as@ / @SYNAPSE_AS@) or the backend's default — and, when
--   its @expires_at@ has passed, logs in again from the key it was made
--   from before the call is sent.
-- * 'reattest' does that on demand, when the server refuses the session
--   (a 401 at the upgrade, or the "you have signed out" error): the caller
--   retries once.
--
-- The session found for an address is remembered for the rest of the
-- process, so the discovery probe and the call carry the same token.
module Synapse.Identity.Session
  ( -- * Login / logout
    LoginResult(..)
  , loginWithKey
  , logoutWithToken

    -- * The session a call carries
  , ActiveSession(..)
  , sessionFor
  , reattest
  , canReattest
  , bearer

    -- * Recognising an ended session
  , isSessionEndedItem
  , isSessionEndedText
  , isUpgradeRefused

    -- * Within one call
  , markSessionEnded
  , takeSessionEnded
  , notePrinted
  , printedAny
  ) where

import Control.Monad (when)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as V
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)

import Synapse.Identity.Proof
import Synapse.Identity.Store
import qualified Synapse.Log as Log
import Synapse.Monad
import Synapse.Schema.Types (HubStreamItem, pattern HubData, pattern HubError)
import Synapse.Transport (invoke)

-- ============================================================================
-- Login
-- ============================================================================

-- | What @session.login@ answered.
data LoginResult = LoginResult
  { lrToken     :: Text
  , lrPrincipal :: Text
  , lrAgent     :: Text
  , lrExpiresAt :: Maybe Integer
  , lrScope     :: [Text]
  } deriving (Eq, Show)

nowSecs :: IO Integer
nowSecs = round <$> getPOSIXTime

-- | Log in to the hub @backend@ at @HOST:PORT@ as @agent@, proving the key:
-- a connection opened with the proof, and @session.login { ttl_secs?,
-- scope? }@ called on it.
loginWithKey :: Log.Logger -> Text -> Int -> Text -> Seed -> Text -> [Text] -> Maybe Integer -> IO (Either Text LoginResult)
loginWithKey logger host port backend seed agent scope ttl
  | not (validAgent agent) = pure (Left ("an agent id is 1-64 of A-Z a-z 0-9 . _ - (not '" <> agent <> "')"))
  | otherwise = do
      nonce <- newNonce
      ts <- nowSecs
      let proof = buildProof seed (wsAudience host port) agent nonce ts
          params = object $
            maybe [] (\t -> ["ttl_secs" .= t]) ttl
            <> (if null scope then [] else ["scope" .= scope])
      items <- call logger host port backend proof "login" params
      pure (items >>= loginItem)
  where
    loginItem items =
      case [o | HubData _ _ _ (Object o) <- items, KM.lookup "type" o == Just (String "login")] of
        (o : _) | Just (String tok) <- KM.lookup "token" o -> Right LoginResult
          { lrToken = tok
          , lrPrincipal = str "principal" o
          , lrAgent = let a = str "agent" o in if T.null a then agent else a
          , lrExpiresAt = KM.lookup "expires_at" o >>= int
          , lrScope = case KM.lookup "scope" o of
              Just (Array xs) -> [s | String s <- V.toList xs]
              _ -> []
          }
        _ -> Left (answerError items "session.login answered no login")

-- | @session.logout@ on a connection carrying this token.
logoutWithToken :: Log.Logger -> Text -> Int -> Text -> Text -> IO (Either Text ())
logoutWithToken logger host port backend token = do
  items <- call logger host port backend token "logout" (object [])
  pure $ items >>= \is ->
    if any isLoggedOut is then Right () else Left (answerError is "session.logout answered nothing")
  where
    isLoggedOut = \case
      HubData _ _ _ (Object o) -> KM.lookup "type" o == Just (String "logged_out")
      _ -> False

-- | One @session.\<method\>@ call on a fresh connection presenting @cred@ as
-- @Authorization: Bearer@.
call :: Log.Logger -> Text -> Int -> Text -> Text -> Text -> Value -> IO (Either Text [HubStreamItem])
call logger host port backend cred method params = do
  env0 <- initEnv host port backend logger Nothing
  let env = withRequestContext [] [bearer cred] env0
  r <- runSynapseM env (invoke ["session"] method params)
  pure $ case r of
    Right items -> Right items
    Left (TransportErrorContext ctx)
      | isUpgradeRefused (tcMessage ctx) -> Left (addressKey host port <> " refused the credential at the upgrade (401)")
      | otherwise -> Left (firstLine (tcMessage ctx))
    Left (TransportError m) -> Left (firstLine m)
    Left e -> Left (T.pack (show e))

firstLine :: Text -> Text
firstLine = T.strip . T.takeWhile (/= '\n')

-- | The upgrade header a credential travels in.
bearer :: Text -> (Text, Text)
bearer cred = ("Authorization", "Bearer " <> cred)

answerError :: [HubStreamItem] -> Text -> Text
answerError items fallback =
  case [m | HubData _ _ _ (Object o) <- items, KM.lookup "type" o == Just (String "error"), Just (String m) <- [KM.lookup "message" o]]
       <> [e | HubError _ _ e _ <- items] of
    (m : _) -> m
    [] -> fallback

str :: KM.Key -> KM.KeyMap Value -> Text
str k o = case KM.lookup k o of
  Just (String s) -> s
  _ -> ""

int :: Value -> Maybe Integer
int = \case
  Number n -> fromIntegral <$> (toBoundedInteger n :: Maybe Int)
  String s | [(i, "")] <- reads (T.unpack s) -> Just i
  _ -> Nothing

-- ============================================================================
-- The session a call carries
-- ============================================================================

data ActiveSession = ActiveSession
  { asHost  :: Text
  , asPort  :: Int
  , asToken :: Text
  , asRec   :: Maybe SessionRec  -- ^ Nothing: PLEXUS_SESSION, nothing to log in again from
  } deriving (Eq, Show)

{-# NOINLINE memo #-}
memo :: IORef (Map.Map (Text, Int) (Maybe ActiveSession))
memo = unsafePerformIO (newIORef Map.empty)

-- | Addresses already logged in again in this process (at most once each).
{-# NOINLINE reattested #-}
reattested :: IORef [(Text, Int)]
reattested = unsafePerformIO (newIORef [])

-- | The session a call to @HOST:PORT@ carries, if any: @PLEXUS_SESSION@,
-- else the stored one for the agent asked (or the backend's default agent).
-- One past its @expires_at@ is made again from its key first.
sessionFor :: Log.Logger -> StoreEnv -> Maybe Text -> Text -> Int -> IO (Maybe ActiveSession)
sessionFor logger st asked host port = do
  known <- Map.lookup (host, port) <$> readIORef memo
  case known of
    Just s -> pure s
    Nothing -> do
      s <- find
      modifyIORef' memo (Map.insert (host, port) s)
      pure s
  where
    find = do
      envTok <- stEnv st "PLEXUS_SESSION"
      case envTok of
        Just t | not (null t) -> pure (Just (ActiveSession host port (T.strip (T.pack t)) Nothing))
        _ -> do
          agent <- maybe (readDefaultAgent st host port) (pure . Just) asked
          case agent of
            Nothing -> pure Nothing
            Just a -> lookupSession st host port a >>= \case
              Just (Right rec, _) -> do
                now <- nowSecs
                let active = ActiveSession host port (srToken rec) (Just rec)
                if maybe False (<= now + 5) (srExpiresAt rec)
                  then reattestWith logger st active "had expired"
                  else pure (Just active)
              Just (Left tok, _) -> pure (Just (ActiveSession host port tok Nothing))
              Nothing -> do
                when (isJust asked) $ TIO.hPutStrLn stderr $
                  "synapse: no session for " <> a <> " at " <> addressKey host port
                    <> " (synapse -P " <> T.pack (show port) <> " _login <backend> --key NAME --as " <> a <> ")"
                pure Nothing

-- | Can the session in use at this address be made again from its key?
canReattest :: Text -> Int -> IO Bool
canReattest host port = do
  s <- Map.lookup (host, port) <$> readIORef memo
  done <- elem (host, port) <$> readIORef reattested
  pure $ case s of
    Just (Just ActiveSession{asRec = Just _}) -> not done
    _ -> False

-- | The server refused the session in use at this address: log in again
-- from its key (once per process). The new session, or Nothing when there
-- was none to remake or the login failed (both said on stderr).
reattest :: Log.Logger -> StoreEnv -> Text -> Int -> IO (Maybe ActiveSession)
reattest logger st host port = do
  ok <- canReattest host port
  s <- Map.lookup (host, port) <$> readIORef memo
  case s of
    Just (Just active) | ok -> do
      r <- reattestWith logger st active "has ended"
      pure $ case r of
        Just new | asToken new /= asToken active -> Just new
        _ -> Nothing
    _ -> pure Nothing

reattestWith :: Log.Logger -> StoreEnv -> ActiveSession -> Text -> IO (Maybe ActiveSession)
reattestWith logger st active why = case asRec active of
  Nothing -> pure (Just active)
  Just rec -> do
    let host = asHost active
        port = asPort active
        who = srAgent rec <> " at " <> addressKey host port
    modifyIORef' reattested ((host, port) :)
    key <- lookupKey st (srKey rec)
    case key of
      Right (Just (seed, _)) -> do
        r <- loginWithKey logger host port (srBackend rec) seed (srAgent rec) (srScope rec) (srTtl rec)
        case r of
          Right lr -> do
            let rec' = rec { srToken = lrToken lr
                           , srPrincipal = if T.null (lrPrincipal lr) then srPrincipal rec else lrPrincipal lr
                           , srExpiresAt = lrExpiresAt lr
                           }
                active' = active { asToken = lrToken lr, asRec = Just rec' }
            _ <- writeSession st host port rec'
            modifyIORef' memo (Map.insert (host, port) (Just active'))
            TIO.hPutStrLn stderr $ "synapse: the session for " <> who <> " " <> why
              <> "; signed in again from key " <> srKey rec
            pure (Just active')
          Left e -> do
            TIO.hPutStrLn stderr $ "synapse: the session for " <> who <> " " <> why
              <> " and signing in again from key " <> srKey rec <> " failed: " <> e
            pure (Just active)
      Right Nothing -> do
        TIO.hPutStrLn stderr $ "synapse: the session for " <> who <> " " <> why
          <> ", and key " <> srKey rec <> " is in no store to sign in again from"
        pure (Just active)
      Left e -> do
        TIO.hPutStrLn stderr $ "synapse: the session for " <> who <> " " <> why <> "; key " <> srKey rec <> ": " <> e
        pure (Just active)

-- ============================================================================
-- Recognising an ended session
-- ============================================================================

-- | The in-band error a hub answers a call whose session has ended with.
isSessionEndedItem :: HubStreamItem -> Bool
isSessionEndedItem = \case
  HubData _ _ _ (Object o)
    | KM.lookup "type" o == Just (String "error")
    , Just (String m) <- KM.lookup "message" o -> isSessionEndedText m
  HubError _ _ e _ -> isSessionEndedText e
  _ -> False

isSessionEndedText :: Text -> Bool
isSessionEndedText m = any (`T.isInfixOf` T.toLower m)
  ["you have signed out", "your sign-in has expired"]

-- | A transport failure that is the upgrade's 401.
isUpgradeRefused :: Text -> Bool
isUpgradeRefused m = any (`T.isInfixOf` m) ["responseCode = 401", " 401 ", "(401)", "Unauthorized"]

-- ============================================================================
-- Within one call: was the session refused before anything was shown?
-- ============================================================================

{-# NOINLINE endedRef #-}
endedRef :: IORef (Maybe Text)
endedRef = unsafePerformIO (newIORef Nothing)

{-# NOINLINE printedRef #-}
printedRef :: IORef Bool
printedRef = unsafePerformIO (newIORef False)

-- | Hold back the server's refusal (its words) for the retry.
markSessionEnded :: Text -> IO ()
markSessionEnded = writeIORef endedRef . Just

-- | Was the session refused, and the refusal held back? Its words; resets.
takeSessionEnded :: IO (Maybe Text)
takeSessionEnded = atomicModifyIORef' endedRef (\b -> (Nothing, b))

notePrinted :: IO ()
notePrinted = writeIORef printedRef True

printedAny :: IO Bool
printedAny = readIORef printedRef
