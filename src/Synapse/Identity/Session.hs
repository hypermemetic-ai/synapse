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

    -- * A person (AUTH-8: the loopback flow)
  , loginPerson
  , whoamiWith
  , whoamiM

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
  , noteRefused
  , wasRefused
  , currentCeiling
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

import Control.Monad.Except (catchError)


import Synapse.Auth (SessionGrant(..), Whoami, isMissingAuth, parseSessionGrant, parseWhoami)
import qualified Synapse.Auth.Loopback as Loopback
import Synapse.Identity.Proof
import Synapse.Identity.Store
import qualified Synapse.Log as Log
import Synapse.Monad
import Synapse.Schema.Types (HubStreamItem, pattern HubData, pattern HubError, pattern HubGuidance)
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
-- a connection opened with the proof, and on it AUTH-5's @auth.login
-- {flow: {session: {ttl_secs?, ceiling?}}}@ — or, on a hub with no @auth@
-- activation (every hub before AUTH), IDENTITY-42's @session.login {ttl_secs?,
-- scope?}@ on a second connection with a fresh proof (a proof works once).
loginWithKey :: Log.Logger -> Text -> Int -> Text -> Seed -> Text -> [Text] -> Maybe Integer -> IO (Either Text LoginResult)
loginWithKey logger host port backend seed agent scope ttl
  | not (validAgent agent) = pure (Left ("an agent id is 1-64 of A-Z a-z 0-9 . _ - (not '" <> agent <> "')"))
  | otherwise = do
      viaAuth <- do
        proof <- freshProof
        let flow = object $
              maybe [] (\t -> ["ttl_secs" .= t]) ttl
              <> (if null scope then [] else ["ceiling" .= scope])
        items <- call logger host port backend (Just proof) "auth" "login" (object ["flow" .= object ["session" .= flow]])
        pure (items >>= grantItem "auth.login")
      case viaAuth of
        Left e | isMissingAuth e -> do
          proof <- freshProof
          let params = object $
                maybe [] (\t -> ["ttl_secs" .= t]) ttl
                <> (if null scope then [] else ["scope" .= scope])
          items <- call logger host port backend (Just proof) "session" "login" params
          pure (items >>= loginItem)
        other -> pure other
  where
    freshProof = do
      nonce <- newNonce
      ts <- nowSecs
      pure (buildProof seed (wsAudience host port) agent nonce ts)
    grantItem what items =
      case [g | HubData _ _ _ v <- items, Just g <- [parseSessionGrant v]] of
        (g : _) -> Right LoginResult
          { lrToken = sgToken g
          , lrPrincipal = sgPrincipal g
          , lrAgent = maybe agent id (sgAgent g)
          , lrExpiresAt = sgExpiresAt g
          , lrScope = sgCeiling g
          }
        [] -> Left (answerError items (what <> " answered no session"))
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

-- | AUTH-8: a person signs in through the browser (the loopback flow,
-- AUTH-1 Q3). synapse listens on 127.0.0.1 (a port the OS picks), asks
-- @auth.login {flow: {browser: {redirect: http://127.0.0.1:PORT/cb}}}@ for
-- @{type: login_url, url, state}@, opens the URL (and says it, for a
-- browser elsewhere), waits up to @secs@ for @GET /cb?code=…&state=…@ with
-- that state, and trades the code: @auth.login {flow: {code: {code,
-- state}}}@ → the session. Every call here is anonymous.
loginPerson :: Log.Logger -> Text -> Int -> Text -> Int -> (Text -> IO ()) -> IO (Either Text SessionGrant)
loginPerson logger host port backend secs say =
  Loopback.withLoopback $ \lb -> do
    let redirect = Loopback.loopbackRedirect lb
        browserFlow = object ["flow" .= object ["browser" .= object ["redirect" .= redirect]]]
    first' <- call logger host port backend Nothing "auth" "login" browserFlow
    case first' >>= loginUrl of
      Left e
        | isMissingAuth e -> pure (Left (addressKey host port <> " has no auth activation, so no browser sign-in"
                                     <> " (an agent signs in with its key: _login " <> backend <> " --key NAME --as AGENT)"))
        | otherwise -> pure (Left e)
      Right (url, state) -> do
        opened <- Loopback.openBrowser url
        say $ (if opened then "opening your browser to sign in; if it does not open, visit:"
                         else "to sign in, visit:")
        say ("  " <> url)
        say ("waiting for the browser on " <> redirect <> " (" <> Loopback.durationText secs <> ") \x2026")
        back <- Loopback.awaitCallback lb state secs
        case back of
          Left why -> pure (Left why)
          Right code -> do
            let codeFlow = object ["flow" .= object ["code" .= object ["code" .= code, "state" .= state]]]
            done <- call logger host port backend Nothing "auth" "login" codeFlow
            pure $ done >>= \items ->
              case [g | HubData _ _ _ v <- items, Just g <- [parseSessionGrant v]] of
                (g : _) -> Right g
                [] -> Left (answerError items "auth.login answered no session for the code")
  where
    loginUrl items =
      case [o | HubData _ _ _ (Object o) <- items, KM.lookup "type" o `elem` [Just (String "login_url"), Just (String "url")]] of
        (o : _) | Just (String u) <- KM.lookup "url" o, not (T.null u) ->
          case KM.lookup "state" o of
            Just (String st) | not (T.null st) -> Right (u, st)
            _ -> Left "auth.login gave a sign-in URL with no state; refused (the state is what binds the browser's answer to this sign-in)"
        _ -> Left (answerError items "auth.login answered no sign-in URL")

-- | AUTH-8: who a token is — @auth.whoami@, else (a hub without the auth
-- activation) @session.whoami@; Nothing when neither says.
whoamiWith :: Log.Logger -> Text -> Int -> Text -> Text -> IO (Maybe Whoami)
whoamiWith logger host port backend token = do
  a <- call logger host port backend (Just token) "auth" "whoami" (object [])
  case a >>= firstWhoami of
    Right w -> pure (Just w)
    Left _ -> do
      b <- call logger host port backend (Just token) "session" "whoami" (object [])
      pure (either (const Nothing) Just (b >>= firstWhoami))

-- | 'whoamiWith' on the call's own connection (its credential, whatever it
-- is): @auth.whoami@, else @session.whoami@, else Nothing. Never fails.
whoamiM :: SynapseM (Maybe Whoami)
whoamiM = do
  a <- (Just <$> invoke ["auth"] "whoami" (object [])) `catchError` \_ -> pure Nothing
  case a >>= either (const Nothing) Just . firstWhoami of
    Just w -> pure (Just w)
    Nothing -> do
      b <- (Just <$> invoke ["session"] "whoami" (object [])) `catchError` \_ -> pure Nothing
      pure (b >>= either (const Nothing) Just . firstWhoami)

firstWhoami :: [HubStreamItem] -> Either Text Whoami
firstWhoami items = case [w | HubData _ _ _ v <- items, Just w <- [parseWhoami v]] of
  (w : _) -> Right w
  [] -> Left (answerError items "no whoami")

-- | End the session this token is: @auth.logout@ (AUTH-5), else — a hub
-- with no auth activation — @session.logout@.
logoutWithToken :: Log.Logger -> Text -> Int -> Text -> Text -> IO (Either Text ())
logoutWithToken logger host port backend token = do
  a <- call logger host port backend (Just token) "auth" "logout" (object [])
  case a >>= ended "auth.logout" of
    Left e | isMissingAuth e -> do
      items <- call logger host port backend (Just token) "session" "logout" (object [])
      pure (items >>= ended "session.logout")
    other -> pure other
  where
    -- an answer with no error is a logout; one that says so, certainly
    ended what is
      | any isLoggedOut is = Right ()
      | null [() | HubError{} <- is] && null (errorEvents is) && not (null [() | HubData{} <- is]) = Right ()
      | otherwise = Left (answerError is (what <> " answered nothing"))
    isLoggedOut = \case
      HubData _ _ _ (Object o) -> KM.lookup "type" o `elem` [Just (String "logged_out"), Just (String "logout")]
      _ -> False
    errorEvents is = [() | HubData _ _ _ (Object o) <- is, KM.lookup "type" o == Just (String "error")]

-- | One @\<ns\>.\<method\>@ call on a fresh connection presenting @cred@
-- as @Authorization: Bearer@ (none: anonymous).
call :: Log.Logger -> Text -> Int -> Text -> Maybe Text -> Text -> Text -> Value -> IO (Either Text [HubStreamItem])
call logger host port backend cred ns method params = do
  env0 <- initEnv host port backend logger Nothing
  let env = withRequestContext [] (maybe [] (\c -> [bearer c]) cred) env0
  r <- runSynapseM env (invoke [ns] method params)
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
       <> [e | HubError _ _ e _ <- items]
       <> [T.pack (show g) | HubGuidance _ _ g _ _ _ <- items] of
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
    -- AUTH-8: a person's session (no key) is made again only by signing in
    Just (Just ActiveSession{asRec = Just r}) -> not done && not (T.null (srKey r))
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
  Just rec | T.null (srKey rec) -> do
    -- AUTH-8: a person's session: there is no key to make it again from
    TIO.hPutStrLn stderr $ "synapse: the session for " <> srAgent rec <> " at "
      <> addressKey (asHost active) (asPort active) <> " " <> why
      <> "; sign in again: synapse -P " <> T.pack (show (asPort active)) <> " _login " <> srBackend rec
    pure (Just active)
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

-- | AUTH-8: a refusal was shown (the call exits non-zero).
{-# NOINLINE refusedRef #-}
refusedRef :: IORef Bool
refusedRef = unsafePerformIO (newIORef False)

noteRefused :: IO ()
noteRefused = writeIORef refusedRef True

wasRefused :: IO Bool
wasRefused = readIORef refusedRef

-- | AUTH-8: the ceiling of the session this process carries, as asked at
-- login (@editor\@MENGER-12@ each) — what a refusal that names none is
-- said to be within. Empty when no stored session is carried.
currentCeiling :: IO [Text]
currentCeiling = do
  m <- readIORef memo
  pure $ case [srScope r | Just ActiveSession{asRec = Just r} <- Map.elems m] of
    (c : _) -> c
    [] -> []
