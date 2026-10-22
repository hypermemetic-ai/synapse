{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SYNAPSE-28: a call costs one round trip and the server's time.
--
-- Every call synapse makes goes through plexus-protocol's
-- 'ST.setCallRouter' hook to one of two places:
--
-- * __the holder__ — a process of synapse's own (@synapse _hold@), one per
--   backend address and agent, in the manner of ssh's ControlMaster. It
--   keeps the signed-in WebSocket open between runs and listens on a unix
--   socket (@$XDG_RUNTIME_DIR/synapse/ADDRESS\@AGENT-VERSION.sock@, 0600,
--   in a 0700 directory). A run hands it each request and reads the reply
--   back, so a warm call costs local IPC and one round trip. The first run
--   starts it; it exits after @--hold-for@ (10 minutes) with nothing to do.
-- * __the multiplexer__ in this process ('Mux') — what the holder itself
--   runs, and what a run uses with @--no-hold@ or when no holder can be
--   had: one connection per address and credential, every call of the
--   run on it (JSON-RPC carries many by id), never one per call.
--
-- Either way a failure reads as it did before: a lost connection is
-- 'LostConnection' (with whether the request had gone out), a failed dial
-- the error call 'Plexus.Client.connect' makes — so SYNAPSE-20's rules
-- (a read sent again, a write never resent blind, exit 69 and 75) and the
-- re-login from the key hold unchanged on top of it. A credential that
-- changed (a session made again from the key) is a new connection in the
-- holder, beside the old one until it is idle; the holder never signs in
-- by itself.
module Synapse.Hold
  ( -- * The multiplexer
    Mux
  , newMux
  , muxCall
  , muxActive
  , muxConnections

    -- * The holder
  , HoldOpts(..)
  , runHolder
  , socketPathFor
  , defaultHoldFor

    -- * Through the holder
  , holderCall
  , HolderGone(..)

    -- * The router
  , RouterOpts(..)
  , installRouter
  , routedCall
  ) where

import Control.Concurrent (forkIO, threadDelay, myThreadId)
import Control.Concurrent.Async (async, cancel, waitCatch, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, unless, void, when)
import Data.Aeson hiding (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.Types (CInt(..))
import GHC.Clock (getMonotonicTime)
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import qualified Streaming.Prelude as S
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, removeFile)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (exitSuccess)
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, IOMode(..), openFile)
import System.Posix.Files (setFileMode, getFileStatus, fileID, fileOwner, fileMode)
import Data.Bits ((.&.))
import System.Posix.IO (closeFd, createFile)
import System.Posix.Signals (installHandler, sigTERM, sigHUP, Handler(..))
import System.Posix.User (getRealUserID)
import System.Process (CreateProcess(..), StdStream(..), createProcess, proc)
import System.Timeout (timeout)
import Foreign.C.Error (throwErrnoIfMinus1RetryMayBlock_)

import Plexus.Client (SubstrateConfig(..), SubstrateConnection, LostConnection(..), TlsOptions(..), connect, disconnect, connectionLost, substrateRpcSub, unsubscribe)
import qualified Plexus.Transport as ST
import Plexus.Types (PlexusStreamItem(..))

-- ============================================================================
-- The multiplexer
-- ============================================================================

-- | Where and as whom: a config with its backend left out (the backend
-- names methods; it does not change the connection).
type ConnKey = SubstrateConfig

keyOf :: SubstrateConfig -> ConnKey
keyOf c = c { substrateBackend = "" }

-- | One connection and the calls on it.
data Upstream = Upstream
  { upConn     :: SubstrateConnection
  , upInflight :: TVar Int
  , upLastUsed :: IORef Double
  , upSerial   :: Int
  }

-- | Connections by address and credential, shared by every call.
data Mux = Mux
  { muxUps     :: MVar (Map.Map ConnKey [Upstream])
  , muxDialing :: MVar (Map.Map ConnKey (MVar ()))
  , muxBusy    :: TVar Int        -- ^ calls under way
  , muxLast    :: IORef Double    -- ^ when the last call ended (or began)
  , muxSerial  :: IORef Int       -- ^ connections opened (for tests, and the holder's log)
  , muxShared  :: MVar (Map.Map (ConnKey, Text, BS.ByteString) (MVar (Either SomeException [PlexusStreamItem])))
    -- ^ schema and _info reads under way, each asked once however many wait
  }

newMux :: IO Mux
newMux = do
  now <- getMonotonicTime
  Mux <$> newMVar Map.empty <*> newMVar Map.empty <*> newTVarIO 0 <*> newIORef now <*> newIORef 0 <*> newMVar Map.empty

-- | Calls under way, and when the last one ended.
muxActive :: Mux -> IO (Int, Double)
muxActive m = (,) <$> readTVarIO (muxBusy m) <*> readIORef (muxLast m)

-- | How many connections this multiplexer has opened.
muxConnections :: Mux -> IO Int
muxConnections = readIORef . muxSerial

-- | Calls one connection carries before another is opened beside it
-- (menger refuses past 256 open calls on one connection, ACCESS-29).
perConnection :: Int
perConnection = 200

-- | One call on the shared connection for its address and credential —
-- dialed when there is none (one dial at a time per key), dropped when it
-- is found dead. A call this side abandons (its thread cancelled, or its
-- consumer failing) is unsubscribed, so a held connection never fills up
-- with streams no one reads.
muxCall :: Mux -> SubstrateConfig -> ST.CallRoute
muxCall mux cfg onSub method params onItem
  | shareable method params = shared mux cfg onSub method params onItem
  | otherwise = muxCall' mux cfg onSub method params onItem

-- | A read every run asks the same of, and asks first: a schema
-- (@<backend>.call@ of @….schema@) or @_info@. Twenty runs starting at once
-- (a cold cache) wait on one answer — menger's @tickets@ schema is 2.6 MB —
-- where each would otherwise fetch its own.
shareable :: Text -> Value -> Bool
shareable method params = method == "_info" || case params of
  Object o | ".call" `T.isSuffixOf` method, Just (String m) <- KM.lookup "method" o -> ".schema" `T.isSuffixOf` m
  _ -> False

-- | The read asked once: the first asks it, the rest are given what it was
-- answered (or how it failed), each told it was taken first.
shared :: Mux -> SubstrateConfig -> ST.CallRoute
shared mux cfg onSub method params onItem = do
  let key = (keyOf cfg, method, LBS.toStrict (encode params))
  mine <- newEmptyMVar
  existing <- modifyMVar (muxShared mux) $ \m -> case Map.lookup key m of
    Just w -> pure (m, Just w)
    Nothing -> pure (Map.insert key mine m, Nothing)
  case existing of
    Just waiting -> readMVar waiting >>= \case
      Right items -> onSub >> mapM_ onItem items
      -- the first run went away (its call cancelled): ask for this one
      Left e | Just (_ :: SomeAsyncException) <- fromException e -> muxCall' mux cfg onSub method params onItem
      Left e -> throwIO e
    Nothing -> do
      acc <- newIORef []
      r <- try (muxCall' mux cfg onSub method params (\i -> modifyIORef' acc (i :) >> onItem i))
      items <- reverse <$> readIORef acc
      modifyMVar_ (muxShared mux) (pure . Map.delete key)
      putMVar mine (either Left (const (Right items)) r)
      either throwIO pure r

muxCall' :: Mux -> SubstrateConfig -> ST.CallRoute
muxCall' mux cfg onSub method params onItem = do
  now <- getMonotonicTime
  writeIORef (muxLast mux) now
  bracket_ (atomically (modifyTVar' (muxBusy mux) (+ 1)))
           (do atomically (modifyTVar' (muxBusy mux) (subtract 1))
               getMonotonicTime >>= writeIORef (muxLast mux)) $
    attempt True
  where
    key = keyOf cfg
    attempt again = do
      up <- acquire mux key cfg
      r <- try (onUpstream up) `finally` release up
      case r of
        -- the connection had died idle: nothing was sent, so once more on a
        -- fresh one (plexus-protocol's freshIfUnsent, here)
        Left (LostConnection _ False) | again -> drop' mux key up >> attempt False
        Left e -> do
          gone <- connectionLost (upConn up)
          when (isJust gone) $ drop' mux key up
          throwIO e
        Right () -> pure ()
    onUpstream up = do
      sub <- newIORef Nothing
      finished <- newIORef False
      let onS sid = writeIORef sub (Just sid) >> onSub
          deliver i = do
            case i of
              StreamDone{} -> writeIORef finished True
              StreamError{} -> writeIORef finished True
              _ -> pure ()
            onItem i
      S.mapM_ deliver (substrateRpcSub (upConn up) onS method params)
        `onException` do
          s <- readIORef sub
          f <- readIORef finished
          case s of
            Just sid | not f -> unsubscribe (upConn up) (method <> "_unsub") sid
                                  `catch` \(_ :: SomeException) -> pure ()
            _ -> pure ()

-- | A live connection for the key with room on it, or a new one.
acquire :: Mux -> ConnKey -> SubstrateConfig -> IO Upstream
acquire mux key cfg = do
  picked <- pick
  case picked of
    Just up -> pure up
    Nothing -> do
      lock <- modifyMVar (muxDialing mux) $ \m -> case Map.lookup key m of
        Just l -> pure (m, l)
        Nothing -> do
          l <- newMVar ()
          pure (Map.insert key l m, l)
      withMVar lock $ \() -> do
        again <- pick
        case again of
          Just up -> pure up
          Nothing -> do
            conn <- connect cfg { substrateBackend = "" }
            n <- atomicModifyIORef' (muxSerial mux) (\k -> (k + 1, k + 1))
            inflight <- newTVarIO 1
            used <- newIORef =<< getMonotonicTime
            let up = Upstream conn inflight used n
            modifyMVar_ (muxUps mux) (pure . Map.insertWith (flip (++)) key [up])
            pure up
  where
    pick = do
      ups <- Map.findWithDefault [] key <$> readMVar (muxUps mux)
      alive <- filterAlive ups
      when (length alive /= length ups) $
        modifyMVar_ (muxUps mux) (pure . Map.adjust (const alive) key)
      let try' [] = pure Nothing
          try' (u : us) = do
            ok <- atomically $ do
              n <- readTVar (upInflight u)
              if n < perConnection then True <$ writeTVar (upInflight u) (n + 1) else pure False
            if ok then pure (Just u) else try' us
      try' alive
    filterAlive = fmap concat . mapM (\u -> do
      g <- connectionLost (upConn u)
      pure [u | not (isJust g)])

release :: Upstream -> IO ()
release up = do
  atomically (modifyTVar' (upInflight up) (subtract 1))
  getMonotonicTime >>= writeIORef (upLastUsed up)

drop' :: Mux -> ConnKey -> Upstream -> IO ()
drop' mux key up = do
  modifyMVar_ (muxUps mux) (pure . Map.adjust (filter ((/= upSerial up) . upSerial)) key)
  disconnect (upConn up) `catch` \(_ :: SomeException) -> pure ()

-- | Close the connections nothing uses: idle ones for an address whose
-- credential has since changed (a session made again), and any idle for
-- longer than @maxIdle@.
sweep :: Mux -> Double -> IO ()
sweep mux maxIdle = do
  now <- getMonotonicTime
  ups <- readMVar (muxUps mux)
  -- per address, the credential used last is kept
  stamped <- mapM (\(k, us) -> do
                     ls <- mapM (readIORef . upLastUsed) us
                     pure (k, us, if null ls then 0 else maximum ls)) (Map.toList ups)
  let place k = (substrateHost k, substratePort k, substratePath k, substrateTls k)
      latest = Map.fromListWith max [(place k, t) | (k, _, t) <- stamped]
  mapM_ (\(k, us, t) -> mapM_ (\u -> do
           n <- readTVarIO (upInflight u)
           l <- readIORef (upLastUsed u)
           let superseded = Map.lookup (place k) latest > Just t && now - l > 5
           when (n == 0 && (superseded || now - l > maxIdle)) $ drop' mux k u) us) stamped

-- ============================================================================
-- The wire between a run and its holder
-- ============================================================================
--
-- One request per unix connection, a line of JSON each way:
--
--   run → holder   {"host","port","path","tls","ca","headers","method","params"}
--   holder → run   {"t":"taken"}            the request is read; it goes out now
--                  {"t":"sub"}              the server took the call
--                  {"t":"item","i":ITEM}    each item, as the server sent it
--                  {"t":"end"}              the call ended
--                  {"t":"lost","why","sent"} the connection went (LostConnection)
--                  {"t":"error","why"}      any other failure (a refused dial)
--
-- A run that goes away mid-call closes its socket; the holder cancels the
-- call (unsubscribing it).

encodeRequest :: SubstrateConfig -> Text -> Value -> Value
encodeRequest c method params = object
  [ "host" .= substrateHost c, "port" .= substratePort c, "path" .= substratePath c
  , "tls" .= isJust (substrateTls c)
  , "ca" .= maybe [] tlsCaFiles (substrateTls c)
  , "headers" .= [ [latin1 (CI.original k), latin1 v] | (k, v) <- substrateHeaders c ]
  , "method" .= method, "params" .= params ]
  where latin1 = T.pack . BS8.unpack

decodeRequest :: Value -> Maybe (SubstrateConfig, Text, Value)
decodeRequest = \case
  Object o -> do
    String host <- KM.lookup "host" o
    port <- num =<< KM.lookup "port" o
    String path <- KM.lookup "path" o
    Bool tls <- KM.lookup "tls" o
    ca <- resultMaybe . fromJSON =<< KM.lookup "ca" o
    hs <- resultMaybe . fromJSON =<< KM.lookup "headers" o
    String method <- KM.lookup "method" o
    let params = maybe Null id (KM.lookup "params" o)
        bytes = BS8.pack . T.unpack
    headers <- mapM (\case { [k, v] -> Just (CI.mk (bytes k), bytes v); _ -> Nothing }) (hs :: [[Text]])
    pure ( SubstrateConfig
             { substrateHost = T.unpack host, substratePort = port, substratePath = T.unpack path
             , substrateBackend = "", substrateHeaders = headers
             , substrateTls = if tls then Just (TlsOptions ca) else Nothing }
         , method, params )
  _ -> Nothing
  where
    num = \case
      Number n -> Just (round n)
      _ -> Nothing
    resultMaybe = \case
      Success a -> Just a
      Error _ -> Nothing

-- | Lines from a socket.
data LineReader = LineReader N.Socket (IORef BS.ByteString)

newLineReader :: N.Socket -> IO LineReader
newLineReader s = LineReader s <$> newIORef BS.empty

-- | The next line, or Nothing at the end.
readLine :: LineReader -> IO (Maybe BS.ByteString)
readLine (LineReader s buf) = go
  where
    go = do
      b <- readIORef buf
      case BS8.elemIndex '\n' b of
        Just i -> do
          writeIORef buf (BS.drop (i + 1) b)
          pure (Just (BS.take i b))
        Nothing -> do
          chunk <- NB.recv s 65536 `catch` \(_ :: IOException) -> pure BS.empty
          if BS.null chunk
            then pure Nothing
            else writeIORef buf (b <> chunk) >> go

sendLine :: N.Socket -> Value -> IO ()
sendLine s v = NB.sendAll s (LBS.toStrict (encode v) <> "\n")

-- ============================================================================
-- The holder
-- ============================================================================

data HoldOpts = HoldOpts
  { hoSocket  :: FilePath
  , hoHoldFor :: Double   -- ^ seconds with nothing to do before it exits
  }

-- | Ten minutes.
defaultHoldFor :: Double
defaultHoldFor = 600

-- | Serve the socket until idle for 'hoHoldFor', then remove it and exit.
runHolder :: HoldOpts -> IO ()
runHolder HoldOpts{..} = do
  mux <- newMux
  clients <- newTVarIO (0 :: Int)
  lastSeen <- newIORef =<< getMonotonicTime
  lsock <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
  privateDir (takeDirectory hoSocket)
  removeIfExists hoSocket
  N.bind lsock (N.SockAddrUnix hoSocket)
  setFileMode hoSocket 0o600
  N.listen lsock 128
  ours <- fileID <$> getFileStatus hoSocket
  done <- newEmptyMVar
  let shutdown = do
        withLock (hoSocket <> ".lock") $ do
          N.close lsock `catch` \(_ :: SomeException) -> pure ()
          -- only our own socket: another holder may have replaced it
          st <- try (getFileStatus hoSocket)
          case st of
            Right s | fileID s == ours -> removeIfExists hoSocket
            Left (_ :: SomeException) -> pure ()
            _ -> pure ()
        -- what was taken before the close is finished first
        atomically $ readTVar clients >>= \n -> check (n == 0)
        void (tryPutMVar done ())
  main' <- myThreadId
  _ <- installHandler sigTERM (Catch (void (forkIO shutdown))) Nothing
  _ <- installHandler sigHUP (Catch (void (forkIO shutdown))) Nothing
  _ <- forkIO $ forever $ do
    r <- try (N.accept lsock)
    case r of
      Left (_ :: SomeException) -> threadDelay 100000
      Right (s, _) -> do
        atomically (modifyTVar' clients (+ 1))
        getMonotonicTime >>= writeIORef lastSeen
        void $ forkIO $
          serveClient mux s `finally` do
            N.close s `catch` \(_ :: SomeException) -> pure ()
            getMonotonicTime >>= writeIORef lastSeen
            atomically (modifyTVar' clients (subtract 1))
  _ <- forkIO $ forever $ do
    threadDelay 1000000
    sweep mux hoHoldFor
    n <- readTVarIO clients
    (busy, lastCall) <- muxActive mux
    seen <- readIORef lastSeen
    now <- getMonotonicTime
    when (n == 0 && busy == 0 && now - max seen lastCall >= hoHoldFor) shutdown
  _ <- pure main'
  takeMVar done
  exitSuccess

-- | One run's request: read it, say it is taken, relay the call, say how
-- it ended. The run closing its end cancels the call.
serveClient :: Mux -> N.Socket -> IO ()
serveClient mux s = do
  rd <- newLineReader s
  line <- timeout 30000000 (readLine rd)
  case line >>= id >>= decodeStrict of
    Nothing -> pure ()
    Just v -> case decodeRequest v of
      Nothing -> sendLine s (object ["t" .= ("error" :: Text), "why" .= ("synapse holder: a request it cannot read" :: Text)])
      Just (cfg, method, params) -> do
        sendLine s (object ["t" .= ("taken" :: Text)])
        let call = muxCall mux cfg
              (sendLine s (object ["t" .= ("sub" :: Text)]))
              method params
              (\i -> sendLine s (object ["t" .= ("item" :: Text), "i" .= i]))
        withAsync call $ \a -> do
          -- the run going away (its end closed) ends the call
          watcher <- async $ do
            _ <- readLine rd
            cancel a
          r <- waitCatch a
          cancel watcher
          let reply = case r of
                Right () -> object ["t" .= ("end" :: Text)]
                Left e | Just (LostConnection why sent) <- fromException e ->
                  object ["t" .= ("lost" :: Text), "why" .= why, "sent" .= sent]
                Left e -> object ["t" .= ("error" :: Text), "why" .= errorText e]
          sendLine s reply `catch` \(_ :: SomeException) -> pure ()

-- | An exception as the run should rethrow it: an error call's message as
-- it is ('connect' reports a failed dial as one), anything else shown.
errorText :: SomeException -> Text
errorText e = case fromException e of
  Just (ErrorCall m) -> T.pack m
  Nothing -> T.pack (show e)

-- ============================================================================
-- A run's side
-- ============================================================================

-- | The holder went away before it took the request: nothing was sent.
data HolderGone = HolderGone Text
  deriving Show

instance Exception HolderGone

-- | One call through the holder at the socket. Throws 'HolderGone' when
-- the holder cannot be reached or closes before taking the request (the
-- request did not go out); after that, what the holder reports.
holderCall :: FilePath -> SubstrateConfig -> ST.CallRoute
holderCall path cfg onSub method params onItem = do
  s <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
  flip finally (N.close s `catch` \(_ :: SomeException) -> pure ()) $ do
    c <- try (N.connect s (N.SockAddrUnix path))
    case c of
      Left (e :: SomeException) -> throwIO (HolderGone (T.pack (show e)))
      Right () -> pure ()
    sent <- try (sendLine s (encodeRequest cfg method params))
    case sent of
      Left (e :: SomeException) -> throwIO (HolderGone (T.pack (show e)))
      Right () -> pure ()
    rd <- newLineReader s
    let loop taken = do
          line <- readLine rd
          case line >>= decodeStrict of
            Nothing
              | taken -> throwIO (LostConnection "the connection holder went away" True)
              | otherwise -> throwIO (HolderGone "the holder closed before taking the request")
            Just (Object o) -> case KM.lookup "t" o of
              Just (String "taken") -> loop True
              Just (String "sub") -> onSub >> loop True
              Just (String "item") -> do
                case fromJSON <$> KM.lookup "i" o of
                  Just (Success i) -> onItem i
                  _ -> pure ()
                loop True
              Just (String "end") -> pure ()
              Just (String "lost") ->
                let why = case KM.lookup "why" o of { Just (String w) -> w; _ -> "the connection was lost" }
                    wasSent = KM.lookup "sent" o /= Just (Bool False)
                in throwIO (LostConnection why wasSent)
              Just (String "error") ->
                throwIO (ErrorCall (case KM.lookup "why" o of { Just (String w) -> T.unpack w; _ -> "the holder failed" }))
              _ -> loop taken
            Just _ -> loop taken
    loop False

-- | The holder's socket for an address and agent:
-- @$XDG_RUNTIME_DIR/synapse/DIR\@AGENT-VERSION.sock@ (a temporary
-- directory of the user's own without one). A name too long for a unix
-- socket is shortened to a hash of itself.
socketPathFor :: Text -> FilePath -> Text -> IO FilePath
socketPathFor version addrDir agent = do
  base <- runtimeDir
  let name = addrDir <> "@" <> T.unpack (safe agent) <> "-" <> T.unpack version <> ".sock"
      full = base </> name
  pure $ if length full < 100 then full else base </> (show (fnv (T.pack name)) <> ".sock")
  where
    safe = T.map (\ch -> if ch `elem` ("/\\\0" :: String) then '_' else ch)
    -- a stable hash of the name (polynomial, mod 2^61-1): only its length matters here
    fnv :: Text -> Integer
    fnv = T.foldl' (\h ch -> (h * 131 + fromIntegral (fromEnum ch)) `mod` 2305843009213693951) 7

runtimeDir :: IO FilePath
runtimeDir = do
  x <- lookupEnv "XDG_RUNTIME_DIR"
  case x of
    Just d | not (null d) -> pure (d </> "synapse")
    _ -> do
      uid <- getRealUserID
      tmp <- getTemporaryDirectory
      pure (tmp </> ("synapse-" <> show uid))

-- | Start the holder for the socket unless one answers there, under the
-- socket's lock (so parallel first runs start one), and wait for it.
ensureHolder :: FilePath -> Double -> IO Bool
ensureHolder path holdFor = do
  privateDir (takeDirectory path)
  withLock (path <> ".lock") $ do
    up <- answers path
    if up then pure True else do
      removeIfExists path
      exe <- getExecutablePath
      devnull <- openFile "/dev/null" ReadWriteMode
      logH <- openFile (path <> ".log") WriteMode
      setFileMode (path <> ".log") 0o600
      procfs <- doesDirectoryExist "/proc/self/fd"
      let args = ["_hold", "--socket", path, "--hold-for", show holdFor]
          -- the holder keeps nothing of the run's but its own socket: every
          -- descriptor past stderr is closed before it starts. close_fds
          -- closes each one the limit allows (a million here: 0.3 s); on
          -- Linux, a shell closes only those /proc says are open, then execs
          spawn
            | procfs = (proc "/bin/sh" (["-c", closeOpenFds, "synapse-hold", exe] ++ args)) { close_fds = False }
            | otherwise = (proc exe args) { close_fds = True }
      _ <- createProcess spawn
        { std_in = UseHandle devnull, std_out = UseHandle devnull, std_err = UseHandle logH
        , new_session = True }
      hClose devnull
      hClose logH
      let wait (0 :: Int) = pure False
          wait n = do
            ok <- answers path
            if ok then pure True else threadDelay 5000 >> wait (n - 1)
      wait 600   -- 3 s

-- | A shell's start for the holder: close every descriptor past stderr
-- that is open, then exec the holder (the arguments after @$0@).
closeOpenFds :: String
closeOpenFds = concat
  [ "for f in /proc/$$/fd/*; do n=${f##*/}; "
  , "case $n in 0|1|2|*[!0-9]*) ;; *) eval \"exec $n>&-\" 2>/dev/null ;; esac; done; "
  , "exec \"$@\"" ]

-- | Does something accept on the socket?
answers :: FilePath -> IO Bool
answers path = do
  s <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
  r <- try (N.connect s (N.SockAddrUnix path))
  N.close s `catch` \(_ :: SomeException) -> pure ()
  pure (either (\(_ :: SomeException) -> False) (const True) r)

-- ============================================================================
-- The router
-- ============================================================================

data RouterOpts = RouterOpts
  { roHold    :: Maybe (SubstrateConfig -> IO (Maybe FilePath))
    -- ^ the holder's socket for a call's address (and the run's agent), or
    -- Nothing for an address this run does not hold (a registry it only
    -- glances at); Nothing at all: this process's multiplexer alone
    -- (@--no-hold@)
  , roHoldFor :: Double
  }

-- | Route every call of this process: through the holder for its address
-- (started when there is none), else on this process's own shared
-- connection.
installRouter :: RouterOpts -> IO Mux
installRouter ro = do
  mux <- newMux
  broken <- newIORef False
  paths <- newIORef Map.empty
  ST.setCallRouter (\cfg -> Just (routedCall ro mux broken paths cfg))
  pure mux

routedCall :: RouterOpts -> Mux -> IORef Bool -> IORef (Map.Map ConnKey (Maybe FilePath)) -> SubstrateConfig -> ST.CallRoute
routedCall ro mux broken paths cfg onSub method params onItem = do
  b <- readIORef broken
  held <- case roHold ro of
    Just socketFor | not b -> do
      known <- Map.lookup (keyOf cfg) <$> readIORef paths
      p <- maybe (socketFor cfg) pure known
      modifyIORef' paths (Map.insert (keyOf cfg) p)
      pure p
    _ -> pure Nothing
  -- the socket's directory must be this user's alone (0700, owned by it)
  ours <- maybe (pure False) (dirIsOurs . takeDirectory) held
  case held of
    Just path | ours -> do
      let via = holderCall path cfg onSub method params onItem
      r <- try via
      case r of
        Right () -> pure ()
        Left (HolderGone _) -> do
          ok <- ensureHolder path (roHoldFor ro)
          if not ok then local' else do
            r' <- try via
            case r' of
              Right () -> pure ()
              Left (HolderGone _) -> local'
    _ -> muxCall mux cfg onSub method params onItem
  where
    -- no holder to be had: this process's own connection, for the rest of it
    local' = do
      writeIORef broken True
      muxCall mux cfg onSub method params onItem

-- ============================================================================
-- Files
-- ============================================================================

foreign import ccall safe "sys/file.h flock" c_flock :: CInt -> CInt -> IO CInt

-- | Hold an advisory lock on the file for the action.
withLock :: FilePath -> IO a -> IO a
withLock lockPath act = do
  opened <- try (createFile lockPath 0o600)
  case opened of
    Left (_ :: SomeException) -> act
    Right fd -> flip finally (closeFd fd) $ do
      throwErrnoIfMinus1RetryMayBlock_ "flock" (c_flock (fromIntegral fd) 2) (pure ())
      act

-- | The directory exists, is this user's, and no one else may enter it.
dirIsOurs :: FilePath -> IO Bool
dirIsOurs dir = do
  r <- try $ do
    privateDir dir
    st <- getFileStatus dir
    uid <- getRealUserID
    pure (fileOwner st == uid && (fileMode st .&. 0o077) == 0)
  pure (either (\(_ :: SomeException) -> False) id r)

privateDir :: FilePath -> IO ()
privateDir dir = do
  existed <- doesDirectoryExist dir
  createDirectoryIfMissing True dir
  unless existed $ setFileMode dir 0o700

removeIfExists :: FilePath -> IO ()
removeIfExists p = removeFile p `catch` \(_ :: SomeException) -> pure ()
