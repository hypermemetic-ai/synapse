{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SYNAPSE-28: a call costs one round trip and the server's time.
-- SYNAPSE-40: and one process holds every connection a user's runs keep.
--
-- Every call synapse makes goes through plexus-protocol's
-- 'ST.setCallRouter' hook to one of two places:
--
-- * __the holder__ — a process of synapse's own (@synapse _hold@), __one
--   per user__ (and per 'holdProtocol'), in the manner of ssh's
--   ControlMaster. It keeps signed-in WebSockets open between runs, one per
--   address, agent and credential, and listens on a unix socket
--   (@$XDG_RUNTIME_DIR/synapse/holder-2.sock@, 0600, in a 0700 directory).
--   A run hands it each request, with the agent it calls as, and reads the
--   reply back, so a warm call costs local IPC and one round trip. The
--   first run starts it. A connection whose server has gone is let go at
--   once (within the second); one idle for @--hold-for@ (10 minutes) is
--   closed; past 'defaultCap' connections the one idle longest is closed
--   first; and the holder exits once it has held nothing and served nothing
--   for @--hold-for@.
-- * __the multiplexer__ in this process ('Mux') — what the holder itself
--   runs, and what a run uses with @--no-hold@ or when no holder can be
--   had: one connection per address, agent and credential, every call of
--   the run on it (JSON-RPC carries many by id), never one per call.
--
-- Either way a failure reads as it did before: a lost connection is
-- 'LostConnection' (with whether the request had gone out), a failed dial
-- the error call 'Plexus.Client.connect' makes — so SYNAPSE-20's rules
-- (a read sent again, a write never resent blind, exit 69 and 75) and the
-- re-login from the key hold unchanged on top of it. A credential that
-- changed (a session made again from the key) is a new connection in the
-- holder, beside the old one until it is idle; the holder never signs in
-- by itself.
--
-- SYNAPSE-28's rule that a call never rides another agent's connection
-- holds by the key: the agent is part of it, so two agents with the same
-- credential (none at all, say) still have a connection each.
--
-- Older synapse binaries (3.16.0 to 3.17.3) keep a holder per address and
-- agent at @ADDRESS\@AGENT-VERSION.sock@ in the same directory; nothing
-- here opens, removes or answers those, so they go on working beside it.
module Synapse.Hold
  ( -- * The multiplexer
    Mux
  , newMux
  , newMuxWith
  , muxCall
  , muxCallAs
  , muxActive
  , muxConnections
  , Held(..)
  , muxHolding

    -- * The holder
  , HoldOpts(..)
  , holdOpts
  , runHolder
  , holdProtocol
  , holderSocketPath
  , defaultHoldFor
  , defaultCap

    -- * Through the holder
  , holderCall
  , holderStatus
  , HolderGone(..)

    -- * The router
  , RouterOpts(..)
  , installRouter
  , routedCall
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, cancel, wait, waitCatch, withAsync)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM, forM_, forever, unless, void, when)
import Data.Aeson hiding (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.Error (Errno(..), eAGAIN, throwErrnoIfMinus1RetryMayBlock_)
import Foreign.C.Types (CInt(..))
import GHC.Clock (getMonotonicTime)
import GHC.IO.Exception (IOException(..))
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import qualified Streaming.Prelude as S
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, makeAbsolute, removeFile)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv)
import System.Exit (exitSuccess)
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, IOMode(..), openFile)
import System.Posix.Files (setFileMode, getFileStatus, fileID, fileOwner, fileMode)
import Data.Bits ((.&.))
import System.Posix.IO (closeFd, createFile)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (installHandler, sigTERM, sigHUP, Handler(..))
import System.Posix.User (getRealUserID)
import System.Process (CreateProcess(..), StdStream(..), createProcess, getProcessExitCode, proc)
import System.Timeout (timeout)

import Plexus.Client (SubstrateConfig(..), SubstrateConnection, LostConnection(..), TlsOptions(..), connect, disconnect, connectionLost, substrateRpcSub, unsubscribe)
import qualified Plexus.Transport as ST
import Plexus.Types (PlexusStreamItem(..))

-- ============================================================================
-- The multiplexer
-- ============================================================================

-- | Where, as whom and with what: the agent the call is made as, and the
-- config with its backend left out (the backend names methods; it does not
-- change the connection) — its address and the credential its headers
-- carry. SYNAPSE-40: the agent is in the key, so a call as one agent never
-- rides another agent's connection, however alike their credentials.
data ConnKey = ConnKey
  { ckAgent  :: Text
  , ckConfig :: SubstrateConfig
  }
  deriving (Eq, Ord)

keyOf :: Text -> SubstrateConfig -> ConnKey
keyOf agent c = ConnKey agent c { substrateBackend = "" }

-- | One connection and the calls on it.
data Upstream = Upstream
  { upConn     :: SubstrateConnection
  , upInflight :: TVar Int
  , upClosing  :: TVar Bool       -- ^ SYNAPSE-40: being let go; no call picks it
  , upLastUsed :: IORef Double
  , upSerial   :: Int
  }

-- | Connections by agent, address and credential, shared by every call.
data Mux = Mux
  { muxUps     :: MVar (Map.Map ConnKey [Upstream])
  , muxDialing :: MVar (Map.Map ConnKey (MVar (), Int))
    -- ^ one dial at a time per key; an entry lives while someone dials or waits
  , muxBusy    :: TVar Int        -- ^ calls under way
  , muxLast    :: IORef Double    -- ^ when the last call ended (or began)
  , muxSerial  :: IORef Int       -- ^ connections opened (for tests, and the holder's status)
  , muxShared  :: MVar (Map.Map (ConnKey, Text, BS.ByteString) (MVar (Either SomeException [PlexusStreamItem])))
    -- ^ schema and _info reads under way, each asked once however many wait
  , muxCap     :: Int
    -- ^ SYNAPSE-40: the most connections kept at once (0: no cap)
  }

-- | A multiplexer with the default cap.
newMux :: IO Mux
newMux = newMuxWith defaultCap

-- | A multiplexer keeping at most this many connections (0: no cap).
newMuxWith :: Int -> IO Mux
newMuxWith cap = do
  now <- getMonotonicTime
  Mux <$> newMVar Map.empty <*> newMVar Map.empty <*> newTVarIO 0 <*> newIORef now <*> newIORef 0
      <*> newMVar Map.empty <*> pure (max 0 cap)

-- | SYNAPSE-40: the most connections the holder keeps. A live connection
-- costs it about 96 kB (measured: 403 held to live services, 17.2 MB with
-- 3 → 55.8 MB, hold-test's "what its cap is set by"), so at 256 the holder
-- stays under about 45 MB — three of SYNAPSE-28's holders, where a loop
-- left 1,634 of them — while 256 is twenty times the 13 live connections
-- this machine's agents held when SYNAPSE-40 was filed. Past it, the
-- connection idle longest is closed first; one carrying a call never is,
-- so a burst of open calls may pass the cap until they end.
defaultCap :: Int
defaultCap = 256

-- | Calls under way, and when the last one ended.
muxActive :: Mux -> IO (Int, Double)
muxActive m = (,) <$> readTVarIO (muxBusy m) <*> readIORef (muxLast m)

-- | How many connections this multiplexer has opened.
muxConnections :: Mux -> IO Int
muxConnections = readIORef . muxSerial

-- | SYNAPSE-40: a connection held, as the holder's status shows it — its
-- address and agent, never its credential.
data Held = Held
  { heldConfig :: SubstrateConfig   -- ^ with its headers left out
  , heldAgent  :: Text
  , heldCalls  :: Int
  , heldIdle   :: Double            -- ^ seconds since a call last ended on it
  }

-- | The connections held now.
muxHolding :: Mux -> IO [Held]
muxHolding mux = do
  now <- getMonotonicTime
  ups <- readMVar (muxUps mux)
  forM [ (k, u) | (k, us) <- Map.toList ups, u <- us ] $ \(k, u) -> do
    n <- readTVarIO (upInflight u)
    l <- readIORef (upLastUsed u)
    pure Held { heldConfig = (ckConfig k) { substrateHeaders = [] }, heldAgent = ckAgent k
              , heldCalls = n, heldIdle = max 0 (now - l) }

-- | Calls one connection carries before another is opened beside it
-- (menger refuses past 256 open calls on one connection, ACCESS-29).
perConnection :: Int
perConnection = 200

-- | One call on the shared connection for its address and credential, as
-- no agent in particular (a run's own multiplexer: the run is one agent).
muxCall :: Mux -> SubstrateConfig -> ST.CallRoute
muxCall mux = muxCallAs mux ""

-- | One call, as the agent, on the shared connection for its address and
-- credential — dialed when there is none (one dial at a time per key),
-- dropped when it is found dead. A call this side abandons (its thread
-- cancelled, or its consumer failing) is unsubscribed, so a held
-- connection never fills up with streams no one reads.
muxCallAs :: Mux -> Text -> SubstrateConfig -> ST.CallRoute
muxCallAs mux agent cfg onSub method params onItem
  | shareable method params = shared mux key onSub method params onItem
  | otherwise = muxCall' mux key onSub method params onItem
  where key = keyOf agent cfg

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
shared :: Mux -> ConnKey -> ST.CallRoute
shared mux ck onSub method params onItem = do
  let key = (ck, method, LBS.toStrict (encode params))
  mine <- newEmptyMVar
  existing <- modifyMVar (muxShared mux) $ \m -> case Map.lookup key m of
    Just w -> pure (m, Just w)
    Nothing -> pure (Map.insert key mine m, Nothing)
  case existing of
    Just waiting -> readMVar waiting >>= \case
      Right items -> onSub >> mapM_ onItem items
      -- the first run went away (its call cancelled): ask for this one
      Left e | Just (_ :: SomeAsyncException) <- fromException e -> muxCall' mux ck onSub method params onItem
      Left e -> throwIO e
    Nothing -> do
      acc <- newIORef []
      r <- try (muxCall' mux ck onSub method params (\i -> modifyIORef' acc (i :) >> onItem i))
      items <- reverse <$> readIORef acc
      modifyMVar_ (muxShared mux) (pure . Map.delete key)
      putMVar mine (either Left (const (Right items)) r)
      either throwIO pure r

muxCall' :: Mux -> ConnKey -> ST.CallRoute
muxCall' mux key onSub method params onItem = do
  now <- getMonotonicTime
  writeIORef (muxLast mux) now
  bracket_ (atomically (modifyTVar' (muxBusy mux) (+ 1)))
           (do atomically (modifyTVar' (muxBusy mux) (subtract 1))
               getMonotonicTime >>= writeIORef (muxLast mux)) $
    attempt True
  where
    -- the slot taken on a connection is given back however the call ends
    -- (bracket: never a connection counted busy for ever, so never idle)
    attempt again = do
      (up, r) <- bracket (acquire mux key) release (\up -> (,) up <$> try (onUpstream up))
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

-- | A live connection for the key with room on it, or a new one — room
-- made for it first when the multiplexer holds its cap.
acquire :: Mux -> ConnKey -> IO Upstream
acquire mux key = do
  picked <- pick
  case picked of
    Just up -> pure up
    Nothing -> withDialLock mux key $ do
      again <- pick
      case again of
        Just up -> pure up
        Nothing -> mask $ \restore -> do
          when (muxCap mux > 0) $ restore (trimTo mux (muxCap mux - 1))
          conn <- restore (dialKept (ckConfig key))
          -- held from the moment it is made: nothing between the dial and
          -- the map may leave it open and held by no one
          uninterruptibleMask_ $ do
            n <- atomicModifyIORef' (muxSerial mux) (\k -> (k + 1, k + 1))
            inflight <- newTVarIO 1
            closing <- newTVarIO False
            used <- newIORef =<< getMonotonicTime
            let up = Upstream conn inflight closing used n
            modifyMVar_ (muxUps mux) (pure . Map.insertWith (flip (++)) key [up])
            pure up
  where
    pick = do
      ups <- Map.findWithDefault [] key <$> readMVar (muxUps mux)
      let try' [] = pure Nothing
          try' (u : us) = do
            gone <- connectionLost (upConn u)
            when (isJust gone) $ drop' mux key u
            ok <- if isJust gone then pure False else atomically $ do
              closing <- readTVar (upClosing u)
              n <- readTVar (upInflight u)
              if not closing && n < perConnection then True <$ writeTVar (upInflight u) (n + 1) else pure False
            if ok then pure (Just u) else try' us
      try' ups

-- | Dial in a thread of its own. A call cancelled while its connection is
-- being made (its run went away) leaves the dial to finish and the
-- connection closed — never open, answering pings, and held by no one.
dialKept :: SubstrateConfig -> IO SubstrateConnection
dialKept cfg = do
  d <- async (connect cfg)
  wait d `onException` void (forkIO (waitCatch d >>= either (const (pure ()))
    (\c -> disconnect c `catch` \(_ :: SomeException) -> pure ())))

-- | One dial at a time for the key; the lock is forgotten once no one
-- holds or waits for it (a holder serving a thousand throwaway ports keeps
-- no trace of them).
withDialLock :: Mux -> ConnKey -> IO a -> IO a
withDialLock mux key act = do
  lock <- modifyMVar (muxDialing mux) $ \m -> case Map.lookup key m of
    Just (l, n) -> pure (Map.insert key (l, n + 1) m, l)
    Nothing -> do
      l <- newMVar ()
      pure (Map.insert key (l, 1) m, l)
  withMVar lock (const act) `finally`
    modifyMVar_ (muxDialing mux)
      (pure . Map.update (\(l, n) -> if n <= 1 then Nothing else Just (l, n - 1)) key)

release :: Upstream -> IO ()
release up = do
  atomically (modifyTVar' (upInflight up) (subtract 1))
  getMonotonicTime >>= writeIORef (upLastUsed up)

-- | Let the connection go: no call picks it from now on, it leaves the map
-- (and its key, when it was the last), and it is closed off this thread (a
-- close never holds the holder up).
drop' :: Mux -> ConnKey -> Upstream -> IO ()
drop' mux key up = do
  atomically (writeTVar (upClosing up) True)
  modifyMVar_ (muxUps mux) $ pure . Map.update
    (\us -> case filter ((/= upSerial up) . upSerial) us of { [] -> Nothing; rest -> Just rest }) key
  void $ forkIO $ disconnect (upConn up) `catch` \(_ :: SomeException) -> pure ()

-- | Claim a connection nothing is on, to close it: a call that picks it at
-- the same moment wins, and it is kept.
claimIdle :: Upstream -> IO Bool
claimIdle up = atomically $ do
  n <- readTVar (upInflight up)
  closing <- readTVar (upClosing up)
  if n == 0 && not closing then True <$ writeTVar (upClosing up) True else pure False

-- | Close idle connections, the one idle longest first, until at most
-- @limit@ are held (a connection carrying a call is never closed for it).
trimTo :: Mux -> Int -> IO ()
trimTo mux limit = do
  ups <- readMVar (muxUps mux)
  let held = [ (k, u) | (k, us) <- Map.toList ups, u <- us ]
      excess = length held - max 0 limit
  when (excess > 0) $ do
    stamped <- forM held $ \(k, u) -> (\l -> (l, k, u)) <$> readIORef (upLastUsed u)
    let close' 0 _ = pure ()
        close' _ [] = pure ()
        close' n ((_, k, u) : rest) = do
          ok <- claimIdle u
          if ok then drop' mux k u >> close' (n - 1 :: Int) rest else close' n rest
    close' excess (sortOn (\(l, _, _) -> l) stamped)

-- | Let go what is not worth holding: at once, a connection whose server
-- has gone (closed, reset, or silent past the keepalive: plexus-protocol
-- has filled its 'connectionLost'); when idle, one superseded by a newer
-- credential of the same agent at the same address (a session made again),
-- and one idle for longer than @maxIdle@; and past the cap, the idle
-- longest first.
sweep :: Mux -> Double -> IO ()
sweep mux maxIdle = do
  now <- getMonotonicTime
  ups <- readMVar (muxUps mux)
  facts <- forM [ (k, u) | (k, us) <- Map.toList ups, u <- us ] $ \(k, u) -> do
    gone <- connectionLost (upConn u)
    l <- readIORef (upLastUsed u)
    pure (k, u, isJust gone, l)
  -- per agent and address, the credential used last is kept
  let place k = let c = ckConfig k
                in (ckAgent k, substrateHost c, substratePort c, substratePath c, substrateTls c)
      lastOfKey = Map.fromListWith max [ (k, l) | (k, _, _, l) <- facts ]
      latest = Map.fromListWith max [ (place k, l) | (k, l) <- Map.toList lastOfKey ]
  forM_ facts $ \(k, u, dead, l) ->
    if dead then drop' mux k u else do
      let superseded = Map.lookup (place k) latest > Map.lookup k lastOfKey && now - l > 5
      when (superseded || now - l > maxIdle) $ do
        ok <- claimIdle u
        when ok $ drop' mux k u
  when (muxCap mux > 0) $ trimTo mux (muxCap mux)

-- | How many connections are held.
heldCount :: Mux -> IO Int
heldCount mux = sum . map length . Map.elems <$> readMVar (muxUps mux)

-- ============================================================================
-- The wire between a run and its holder
-- ============================================================================
--
-- One request per unix connection, a line of JSON each way:
--
--   run → holder   {"agent","host","port","path","tls","ca","headers","method","params"}
--   holder → run   {"t":"taken"}            the request is read; it goes out now
--                  {"t":"sub"}              the server took the call
--                  {"t":"item","i":ITEM}    each item, as the server sent it
--                  {"t":"end"}              the call ended
--                  {"t":"lost","why","sent"} the connection went (LostConnection)
--                  {"t":"error","why"}      any other failure (a refused dial)
--
--   run → holder   {"status":true}
--   holder → run   {"t":"status","protocol","version","pid","cap","hold_for",
--                   "clients","calls","held":[{"host","port","path","tls",
--                   "agent","calls","idle"}, …]}   never a credential
--
-- A run that goes away mid-call closes its socket; the holder cancels the
-- call (unsubscribing it).

-- | SYNAPSE-40: the holder's protocol — the wire above and what the holder
-- does with a request. It names the socket, so a run only ever talks to a
-- holder that reads it as it means it; bump it with any change to either
-- (the relay of items included: a holder built against another
-- plexus-protocol relays its 'PlexusStreamItem'). 1 was SYNAPSE-28's holder
-- per address and agent (@ADDRESS\@AGENT-VERSION.sock@, synapse 3.16.0 to
-- 3.17.3); 2 is one holder per user, the agent in each request.
holdProtocol :: Int
holdProtocol = 2

encodeRequest :: Text -> SubstrateConfig -> Text -> Value -> Value
encodeRequest agent c method params = object
  [ "agent" .= agent
  , "host" .= substrateHost c, "port" .= substratePort c, "path" .= substratePath c
  , "tls" .= isJust (substrateTls c)
  , "ca" .= maybe [] tlsCaFiles (substrateTls c)
  , "headers" .= [ [latin1 (CI.original k), latin1 v] | (k, v) <- substrateHeaders c ]
  , "method" .= method, "params" .= params ]
  where latin1 = T.pack . BS8.unpack

decodeRequest :: Value -> Maybe (Text, SubstrateConfig, Text, Value)
decodeRequest = \case
  Object o -> do
    let agent = case KM.lookup "agent" o of { Just (String a) -> a; _ -> "" }
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
    pure ( agent
         , SubstrateConfig
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

isStatusRequest :: Value -> Bool
isStatusRequest = \case
  Object o -> KM.lookup "status" o == Just (Bool True)
  _ -> False

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
  , hoHoldFor :: Double   -- ^ seconds holding nothing, with nothing to do, before it exits
  , hoCap     :: Int      -- ^ the most connections kept at once (0: no cap)
  , hoVersion :: Text     -- ^ the synapse it is (its status says so)
  }

-- | A holder at the socket, idle for the seconds, with the default cap.
holdOpts :: FilePath -> Double -> HoldOpts
holdOpts sock secs = HoldOpts { hoSocket = sock, hoHoldFor = secs, hoCap = defaultCap, hoVersion = "" }

-- | Ten minutes.
defaultHoldFor :: Double
defaultHoldFor = 600

-- | Serve the socket until it has held nothing, with no run on it, for
-- 'hoHoldFor'; then remove it and exit. SIGTERM / SIGHUP: stop taking
-- runs at once, give the calls under way ten seconds to end, and go (a
-- stream still open then is told its connection was lost, and resumes as
-- SYNAPSE-20 says).
runHolder :: HoldOpts -> IO ()
runHolder HoldOpts{..} = do
  mux <- newMuxWith hoCap
  clients <- newTVarIO (0 :: Int)
  lastSeen <- newIORef =<< getMonotonicTime
  lsock <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
  privateDir (takeDirectory hoSocket)
  removeIfExists hoSocket
  N.bind lsock (N.SockAddrUnix hoSocket)
  setFileMode hoSocket 0o600
  -- one holder takes every run of the user's: a long queue, so a burst of
  -- runs never finds it full (and takes it for gone)
  N.listen lsock (max 128 N.maxListenQueue)
  ours <- fileID <$> getFileStatus hoSocket
  pid <- getProcessID
  done <- newEmptyMVar
  let close' = withLock (hoSocket <> ".lock") $ do
        N.close lsock `catch` \(_ :: SomeException) -> pure ()
        -- only our own socket: another holder may have replaced it
        st <- try (getFileStatus hoSocket)
        case st of
          Right s | fileID s == ours -> removeIfExists hoSocket
          Left (_ :: SomeException) -> pure ()
          _ -> pure ()
      -- what was taken before the close is finished first: however long
      -- that takes when idle, ten seconds when told to go
      shutdown grace = do
        close'
        let drained = atomically $ readTVar clients >>= \n -> check (n == 0)
        maybe drained (\secs -> void (timeout (round (secs * 1000000 :: Double)) drained)) grace
        void (tryPutMVar done ())
      status = do
        held <- muxHolding mux
        n <- readTVarIO clients
        (busy, _) <- muxActive mux
        pure $ object
          [ "t" .= ("status" :: Text), "protocol" .= holdProtocol, "version" .= hoVersion
          , "pid" .= (fromIntegral pid :: Int), "cap" .= hoCap, "hold_for" .= hoHoldFor
          , "clients" .= (n - 1 :: Int)   -- the one asking is not counted
          , "calls" .= busy
          , "held" .= [ object [ "host" .= substrateHost c, "port" .= substratePort c, "path" .= substratePath c
                               , "tls" .= isJust (substrateTls c), "agent" .= heldAgent h
                               , "calls" .= heldCalls h, "idle" .= (fromIntegral (round (heldIdle h * 10) :: Int) / 10 :: Double) ]
                      | h <- held, let c = heldConfig h ] ]
  _ <- installHandler sigTERM (Catch (void (forkIO (shutdown (Just 10))))) Nothing
  _ <- installHandler sigHUP (Catch (void (forkIO (shutdown (Just 10))))) Nothing
  _ <- forkIO $ forever $ do
    r <- try (N.accept lsock)
    case r of
      Left (_ :: SomeException) -> threadDelay 100000
      Right (s, _) -> do
        atomically (modifyTVar' clients (+ 1))
        getMonotonicTime >>= writeIORef lastSeen
        void $ forkIO $
          serveClient mux status s `finally` do
            N.close s `catch` \(_ :: SomeException) -> pure ()
            getMonotonicTime >>= writeIORef lastSeen
            atomically (modifyTVar' clients (subtract 1))
  _ <- forkIO $ forever $ do
    threadDelay 1000000
    sweep mux hoHoldFor
    n <- readTVarIO clients
    (busy, lastCall) <- muxActive mux
    held <- heldCount mux
    seen <- readIORef lastSeen
    now <- getMonotonicTime
    when (n == 0 && busy == 0 && held == 0 && now - max seen lastCall >= hoHoldFor) (shutdown Nothing)
  takeMVar done
  exitSuccess

-- | One run's request: read it, say it is taken, relay the call, say how
-- it ended — or, asked for it, the status. The run closing its end
-- cancels the call.
serveClient :: Mux -> IO Value -> N.Socket -> IO ()
serveClient mux status s = do
  rd <- newLineReader s
  line <- timeout 30000000 (readLine rd)
  case line >>= id >>= decodeStrict of
    Nothing -> pure ()
    Just v | isStatusRequest v -> status >>= sendLine s
    Just v -> case decodeRequest v of
      Nothing -> sendLine s (object ["t" .= ("error" :: Text), "why" .= ("synapse holder: a request it cannot read" :: Text)])
      Just (agent, cfg, method, params) -> do
        sendLine s (object ["t" .= ("taken" :: Text)])
        let call = muxCallAs mux agent cfg
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

-- | A connection to the holder's socket. A queue that is full answers
-- EAGAIN: that is a holder, busy, so it is tried again (for a second)
-- rather than taken for none.
openHolder :: FilePath -> IO N.Socket
openHolder path = go (50 :: Int)
  where
    go n = do
      s <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
      r <- try (N.connect s (N.SockAddrUnix path))
      case r of
        Right () -> pure s
        Left (e :: SomeException) -> do
          N.close s `catch` \(_ :: SomeException) -> pure ()
          if busy e && n > 0 then threadDelay 20000 >> go (n - 1) else throwIO e

-- | EAGAIN: the listener's queue is full.
busy :: SomeException -> Bool
busy e = case fromException e of
  Just io -> fmap Errno (ioe_errno io) == Just eAGAIN
  Nothing -> False

-- | One call through the holder at the socket, as the agent. Throws
-- 'HolderGone' when the holder cannot be reached or closes before taking
-- the request (the request did not go out); after that, what the holder
-- reports.
holderCall :: FilePath -> Text -> SubstrateConfig -> ST.CallRoute
holderCall path agent cfg0 onSub method params onItem = do
  -- a CA file named relative to this run's directory is named whole: the
  -- holder runs in / and serves every run
  cfg <- case substrateTls cfg0 of
    Just t -> (\fs -> cfg0 { substrateTls = Just t { tlsCaFiles = fs } }) <$> mapM makeAbsolute (tlsCaFiles t)
    Nothing -> pure cfg0
  c <- try (openHolder path)
  s <- case c of
    Left (e :: SomeException) -> throwIO (HolderGone (T.pack (show e)))
    Right s -> pure s
  flip finally (N.close s `catch` \(_ :: SomeException) -> pure ()) $ do
    sent <- try (sendLine s (encodeRequest agent cfg method params))
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

-- | SYNAPSE-40: what the holder at the socket holds, as it says
-- (@{"t":"status", …}@), or Nothing when no holder answers there.
holderStatus :: FilePath -> IO (Maybe Value)
holderStatus path = do
  r <- try $ bracket (openHolder path) N.close $ \s -> do
    sendLine s (object ["status" .= True])
    rd <- newLineReader s
    timeout 5000000 (readLine rd)
  pure $ case r of
    Right (Just (Just line)) | Just v@(Object o) <- decodeStrict line
                             , KM.lookup "t" o == Just (String "status") -> Just v
    Left (_ :: SomeException) -> Nothing
    _ -> Nothing

-- | SYNAPSE-40: the one holder's socket:
-- @$XDG_RUNTIME_DIR/synapse/holder-2.sock@ (a temporary directory of the
-- user's own without one) — one per user and 'holdProtocol'.
holderSocketPath :: IO FilePath
holderSocketPath = (</> ("holder-" <> show holdProtocol <> ".sock")) <$> runtimeDir

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
-- socket's lock (so parallel first runs start one), and wait for it — up
-- to 3 s, or until it has exited without answering (a socket it could not
-- bind), so a run never waits on a holder that is not coming.
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
      -- SYNAPSE-40: the one holder outlives the run that starts it, so it
      -- takes nothing of that run's it does not need: not its directory (a
      -- worktree it would keep in use), nor a secret in its environment
      env <- filter ((`notElem` secrets) . fst) <$> getEnvironment
      let args = ["_hold", "--socket", path, "--hold-for", show holdFor]
          -- the holder keeps nothing of the run's but its own socket: every
          -- descriptor past stderr is closed before it starts. close_fds
          -- closes each one the limit allows (a million here: 0.3 s); on
          -- Linux, a shell closes only those /proc says are open, then execs
          spawn
            | procfs = (proc "/bin/sh" (["-c", closeOpenFds, "synapse-hold", exe] ++ args)) { close_fds = False }
            | otherwise = (proc exe args) { close_fds = True }
      (_, _, _, ph) <- createProcess spawn
        { std_in = UseHandle devnull, std_out = UseHandle devnull, std_err = UseHandle logH
        , new_session = True, cwd = Just "/", env = Just env }
      hClose devnull
      hClose logH
      let wait (0 :: Int) = pure False
          wait n = do
            ok <- answers path
            if ok then pure True else do
              ended <- getProcessExitCode ph
              case ended of
                Just _ -> pure False
                Nothing -> threadDelay 5000 >> wait (n - 1)
      wait 600   -- 3 s

-- | What a run may carry in its environment that the holder never needs:
-- a key's seed, a session, a token (the holder signs nothing in; every
-- request brings its credential).
secrets :: [String]
secrets = ["PLEXUS_KEY", "PLEXUS_SESSION", "SYNAPSE_TOKEN"]

-- | A shell's start for the holder: close every descriptor past stderr
-- that is open, then exec the holder (the arguments after @$0@).
closeOpenFds :: String
closeOpenFds = concat
  [ "for f in /proc/$$/fd/*; do n=${f##*/}; "
  , "case $n in 0|1|2|*[!0-9]*) ;; *) eval \"exec $n>&-\" 2>/dev/null ;; esac; done; "
  , "exec \"$@\"" ]

-- | Does a holder accept on the socket? One whose queue is full (EAGAIN
-- past 'openHolder''s second) is there, busy: never replaced for it.
answers :: FilePath -> IO Bool
answers path = do
  r <- try (openHolder path)
  case r of
    Right s -> True <$ (N.close s `catch` \(_ :: SomeException) -> pure ())
    Left e -> pure (busy e)

-- ============================================================================
-- The router
-- ============================================================================

data RouterOpts = RouterOpts
  { roHold    :: Maybe (SubstrateConfig -> IO (Maybe Text))
    -- ^ the agent a call to this address is made as, through the holder,
    -- or Nothing for an address this run does not hold (a registry it only
    -- glances at); Nothing at all: this process's multiplexer alone
    -- (@--no-hold@)
  , roSocket  :: FilePath   -- ^ the holder's socket ('holderSocketPath')
  , roHoldFor :: Double
  }

-- | Route every call of this process: through the holder (started when
-- there is none), else on this process's own shared connection.
installRouter :: RouterOpts -> IO Mux
installRouter ro = do
  mux <- newMux
  broken <- newIORef False
  agents <- newIORef Map.empty
  ST.setCallRouter (\cfg -> Just (routedCall ro mux broken agents cfg))
  pure mux

routedCall :: RouterOpts -> Mux -> IORef Bool -> IORef (Map.Map ConnKey (Maybe Text)) -> SubstrateConfig -> ST.CallRoute
routedCall ro mux broken agents cfg onSub method params onItem = do
  b <- readIORef broken
  agent <- case roHold ro of
    Just agentFor | not b -> do
      let k = keyOf "" cfg
      known <- Map.lookup k <$> readIORef agents
      a <- maybe (agentFor cfg) pure known
      modifyIORef' agents (Map.insert k a)
      pure a
    _ -> pure Nothing
  -- the socket's directory must be this user's alone (0700, owned by it),
  -- and the path short enough for a unix socket (107 bytes)
  let path = roSocket ro
      fits = BS.length (TE.encodeUtf8 (T.pack path)) < 108
  ours <- if fits then maybe (pure False) (const (dirIsOurs (takeDirectory path))) agent else pure False
  case agent of
    Just a | ours -> do
      let via = holderCall path a cfg onSub method params onItem
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
