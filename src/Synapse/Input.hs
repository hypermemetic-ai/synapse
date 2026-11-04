{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | BIDIR-18: synapse calls a method with an input half (@Input<T>@,
-- plexus-wire spec/10).
--
-- stdin (@--input -@) or a file (@--input \@path@) is the stream in:
--
-- * a string item: one line, one item;
-- * a bytes item (@"x-plexus-binary": true@, @Input<Bytes>@): the bytes cut
--   into @--chunk N@ (4096 unless said), each one WebSocket binary frame;
-- * anything else: NDJSON, one JSON value a line — a line that does not
--   decode is an error naming its line number, and no frame carries it.
--
-- Items are read only while the callee's credit allows: with no credit,
-- nothing more is read, so the pipe in front of synapse is the buffer and
-- synapse's memory stays flat however far the writer runs ahead. What is
-- ready goes out together, up to 256 items (or about 1 MiB) a frame. At
-- the end of the input, @<ns>.end@; the output prints until its terminal
-- item, as any call's.
--
-- Nothing is sent twice (spec/10: no resume; the owner, 2026-11-01:
-- buffer locally, delete only as a last resort, always warn on a
-- disconnect). A connection lost during the call is said on stderr every
-- time, with what was sent and what the callee acknowledged, and the run
-- exits 69. A typed close exits by its kind: @past_credit@ 70 (synapse's
-- own bug), @invalid_input@, @out_of_sequence@, @too_much_input@ 65, a
-- revoked or ended sign-in 77.
module Synapse.Input
  ( -- * The flags
    InputNames(..)
  , inputNames
  , InputFlags(..)
  , takeInputFlags
  , defaultChunk

    -- * The source
  , InputSource(..)
  , sourceOf
  , ItemKind(..)
  , itemKind

    -- * Feeding a call
  , Tally(..)
  , Fed(..)
  , withInputFeed
  , renderTally
  , closeExit
  , inputRefusal

    -- * A dry run
  , dryRunFrame
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, cancel, waitCatch, waitEitherCatch, withAsync)
import Control.Concurrent.STM
import Control.Exception (Exception, SomeException, IOException, catch, fromException, throwIO, try)
import Control.Monad (forM_, unless, void, when)
import Data.Aeson (Value(..), eitherDecodeStrict', encode, object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isSpace)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Word (Word64)
import Numeric (showHex)
import System.FilePath ((</>), isRelative)
import System.IO (Handle, IOMode(..), hClose, hFlush, hReady, hSetBinaryMode, openBinaryFile, stderr, stdin)

import Plexus.Client (LostConnection(..))
import Plexus.Input
import Plexus.Types (PlexusStreamItem(..))
import qualified Plexus.RemoteError as RE

-- ---------------------------------------------------------------------------
-- The flags

-- | What the input's flags are called for a method: @--input@, @--chunk@,
-- @--progress@ — or, when a parameter of the method already has one of
-- those names, @--input-stream@, @--input-chunk@, @--input-progress@ (the
-- parameter keeps its flag).
data InputNames = InputNames
  { inStream   :: Text
  , inChunk    :: Text
  , inProgress :: Text
  } deriving (Eq, Show)

-- | The names, given the method's parameter names.
inputNames :: [Text] -> InputNames
inputNames params = InputNames
  { inStream = pick "input" "input-stream"
  , inChunk = pick "chunk" "input-chunk"
  , inProgress = pick "progress" "input-progress"
  }
  where
    pick plain other = if plain `elem` params then other else plain

-- | What the input's flags said.
data InputFlags = InputFlags
  { ifSource   :: Maybe Text   -- ^ @-@ or @\@path@ (Nothing: not given)
  , ifChunk    :: Maybe Int
  , ifProgress :: Bool
  } deriving (Eq, Show)

-- | 4096 bytes a binary frame unless @--chunk@ says.
defaultChunk :: Int
defaultChunk = 4096

-- | Take the input's flags out of a method's flag arguments (as
-- 'Synapse.CLI.Parse.parsePathAndParams' gave them): what they said, and
-- the arguments left for the method's parameters. @--input@ and @--chunk@
-- take the next argument whatever it begins with (@-@ is stdin), or
-- @=value@; @--progress@ is a switch (@=true@ / @=false@ too).
takeInputFlags :: InputNames -> [Text] -> Either Text (InputFlags, [Text])
takeInputFlags names = go (InputFlags Nothing Nothing False) []
  where
    go acc rest [] = Right (acc, reverse rest)
    go acc rest (x : xs) = case T.stripPrefix "--" x of
      Just flag
        | (key, eq) <- T.breakOn "=" flag, norm key == norm (inStream names) ->
            withValue key eq xs $ \v xs' -> go acc { ifSource = Just v } rest xs'
        | (key, eq) <- T.breakOn "=" flag, norm key == norm (inChunk names) ->
            withValue key eq xs $ \v xs' -> case reads (T.unpack v) of
              [(n, "")] | n > 0 && n <= 64 * 1024 * 1024 -> go acc { ifChunk = Just n } rest xs'
              _ -> Left ("--" <> key <> " takes a number of bytes (1 to 67108864), not '" <> v <> "'")
        | (key, eq) <- T.breakOn "=" flag, norm key == norm (inProgress names) ->
            case T.drop 1 eq of
              _ | T.null eq -> go acc { ifProgress = True } rest xs
              v | v `elem` ["true", "1"] -> go acc { ifProgress = True } rest xs
                | v `elem` ["false", "0"] -> go acc { ifProgress = False } rest xs
                | otherwise -> Left ("--" <> key <> " is a switch (true or false), not '" <> v <> "'")
      _ -> go acc (x : rest) xs
    withValue key eq xs k
      | not (T.null eq) = k (T.drop 1 eq) xs
      | (v : xs') <- xs = k v xs'
      | otherwise = Left ("--" <> key <> " takes a value: - (stdin) or @FILE")
    norm = T.replace "_" "-"

-- ---------------------------------------------------------------------------
-- The source

-- | Where the items come from.
data InputSource = FromStdin | FromFile FilePath
  deriving (Eq, Show)

-- | @-@ is stdin, @\@path@ a file (@~/@ and a relative path resolved as
-- SYNAPSE-10's @\@file@ are); anything else is refused, naming the flag.
sourceOf :: Text -> FilePath -> Maybe FilePath -> Text -> Either Text InputSource
sourceOf flag cwd home v
  | v == "-" = Right FromStdin
  | Just p <- T.stripPrefix "@" v, not (T.null p), not ("@" `T.isPrefixOf` p) = Right (FromFile (resolve (T.unpack p)))
  | otherwise = Left ("--" <> flag <> " takes - (stdin) or @FILE, not '" <> v <> "'")
  where
    resolve ('~' : '/' : rest) | Just h <- home = h </> rest
    resolve p | isRelative p = cwd </> p
              | otherwise = p

-- | Items read from a handle.
data Reader = Reader
  { rdNext  :: IO (Maybe Item)  -- ^ the next item, blocking; Nothing at the end
  , rdReady :: IO Bool          -- ^ another can be read without waiting
  }

data Item = JsonItem Value Int | BytesItem BS.ByteString

-- | A line that is not an item: its number and why.
data BadLine = BadLine Int Text
  deriving Show

instance Exception BadLine

newReader :: ItemKind -> Int -> Handle -> IO Reader
newReader kind chunk h = do
  hSetBinaryMode h True
  lineNo <- newIORef (0 :: Int)
  let ready = hReady h `catch` \(_ :: IOException) -> pure True   -- at the end: the next read says so
      line = do
        r <- try (BS8.hGetLine h)
        case r of
          Left (_ :: IOException) -> pure Nothing
          Right l -> do
            n <- atomicModifyIORef' lineNo (\k -> (k + 1, k + 1))
            pure (Just (n, stripCr l))
      nextJson = line >>= \case
        Nothing -> pure Nothing
        Just (n, l)
          | BS8.all isSpace l -> nextJson   -- a blank line between values is none
          | otherwise -> case eitherDecodeStrict' l of
              Right v -> pure (Just (JsonItem v (BS.length l)))
              Left e -> throwIO (BadLine n (T.pack e))
      nextString = line >>= \case
        Nothing -> pure Nothing
        Just (n, l) -> case TE.decodeUtf8' l of
          Right t -> pure (Just (JsonItem (String t) (BS.length l)))
          Left _ -> throwIO (BadLine n "not UTF-8 text")
      nextBytes = do
        b <- BS.hGet h chunk
        pure (if BS.null b then Nothing else Just (BytesItem b))
  pure Reader
    { rdNext = case kind of
        ItemBytes -> nextBytes
        ItemString -> nextString
        ItemJson -> nextJson
    , rdReady = ready }
  where
    stripCr l | not (BS.null l), BS8.last l == '\r' = BS.init l
              | otherwise = l

openSource :: InputSource -> IO Handle
openSource = \case
  FromStdin -> pure stdin
  FromFile p -> openBinaryFile p ReadMode

-- ---------------------------------------------------------------------------
-- Feeding a call

-- | What the input half did: what went out, and what the callee said.
data Tally = Tally
  { tItems    :: !Integer        -- ^ items sent
  , tFrames   :: !Integer        -- ^ frames sent (JSON and binary)
  , tAcked    :: !(Maybe Integer)  -- ^ the last frame the callee consumed
  , tUnacked  :: !Integer        -- ^ items sent and not yet acknowledged
  , tCredit   :: !Integer        -- ^ credit left
  , tEnded    :: !Bool           -- ^ @end@ went out
  , tClosed   :: !(Maybe (Text, Maybe Integer))  -- ^ @input_closed {reason, ack}@
  } deriving (Eq, Show)

-- | How the input half ended, beside the call's own ending.
data Fed
  = FedAll                 -- ^ the input ended and @end@ went out, or the callee closed it
  | FedBadLine Int Text    -- ^ a line that is not an item: nothing carried it; the call was ended
  | FedFailed Text         -- ^ the source could not be read
  deriving (Eq, Show)

data FeedState = FeedState
  { fsPort    :: TMVar InputPort
  , fsCredit  :: TVar Integer
  , fsClosed  :: TVar (Maybe (Text, Maybe Integer))
  , fsAcked   :: TVar (Maybe Integer)
  , fsItems   :: TVar Integer
  , fsFrames  :: TVar Integer
  , fsThrough :: TVar (Map.Map Integer Integer)  -- ^ frame seq → items sent through it, for frames not yet acked
  , fsAckedItems :: TVar Integer                -- ^ items in the frames acked
  , fsEnded   :: TVar Bool
  , fsDone    :: TVar Bool                      -- ^ the call is over: stop
  , fsGranted :: TVar Bool                      -- ^ a credit came: the source may have been read
  }

-- | The most items, and about the most bytes, one @<ns>.input@ carries.
frameItems, frameBytes :: Int
frameItems = 256
frameBytes = 1024 * 1024

-- | Run a call with its input fed from the source. @call@ makes the call
-- with the half it is given (through a route that hands the half its port
-- once the server has taken the call). The source is opened first (a file
-- that cannot be read is refused before anything is sent). Returns how the
-- call ended (its own exception kept), how the feeding ended, and the
-- tally. A bad line cancels the call. @call@ is also given whether the
-- source is untouched yet (no credit has come, so nothing was read): a
-- call refused then may be made again without losing an item.
withInputFeed
  :: ItemKind -> Int -> Bool -> InputSource
  -> ((InputHalf, IO Bool) -> IO a)
  -> IO (Either SomeException a, Fed, Tally)
withInputFeed kind chunk progress src call = do
  h <- openSource src
  rd <- newReader kind chunk h
  st <- FeedState <$> newEmptyTMVarIO <*> newTVarIO 0 <*> newTVarIO Nothing <*> newTVarIO Nothing
                  <*> newTVarIO 0 <*> newTVarIO 0 <*> newTVarIO Map.empty <*> newTVarIO 0 <*> newTVarIO False <*> newTVarIO False <*> newTVarIO False
  let half = InputHalf
        { ihPort = atomically . void . tryPutTMVar (fsPort st)
        , ihSignal = signal st }
  feeder <- async (feed kind rd st)
  ticker <- if progress then Just <$> async (tick st) else pure Nothing
  callA <- async (call (half, not <$> readTVarIO (fsGranted st)))
  first <- waitEitherCatch feeder callA
  (result, fed) <- case first of
    -- the call ended first: the feeder may be blocked reading the source,
    -- which no exception interrupts — it is let go, not waited for
    Right r -> do
      atomically (writeTVar (fsDone st) True)
      void (forkIO (cancel feeder))
      pure (r, FedAll)
    Left (Right ()) -> do
      r <- waitCatch callA
      pure (r, FedAll)
    Left (Left e)
      | Just (BadLine n why) <- fromException e -> do
          -- nothing carried the line: the call ends here, unsubscribed
          cancel callA
          r <- waitCatch callA
          pure (r, FedBadLine n why)
      | Just (_ :: LostConnection) <- fromException e -> do
          -- a frame the connection would not take: the call reports why
          r <- waitCatch callA
          pure (r, FedAll)
      | otherwise -> do
          cancel callA
          r <- waitCatch callA
          pure (r, FedFailed (T.pack (show e)))
  atomically (writeTVar (fsDone st) True)
  mapM_ cancel ticker
  case src of
    FromFile _ -> hClose h `catch` \(_ :: SomeException) -> pure ()
    FromStdin -> pure ()
  t <- tallyOf st
  pure (result, fed, t)

-- | What the callee said, kept for the feeder.
signal :: FeedState -> InputSignal -> IO ()
signal st = \case
  InputCredit n -> atomically $ do
    writeTVar (fsGranted st) True
    modifyTVar' (fsCredit st) (+ max 0 n)
  InputAck sq -> atomically (acked sq)
  InputClosed reason ack -> atomically $ do
    writeTVar (fsClosed st) (Just (reason, ack))
    forM_ ack acked
  where
    -- every frame through sq consumed: its items counted, its entry let go
    acked sq = do
      writeTVar (fsAcked st) (Just sq)
      through <- readTVar (fsThrough st)
      forM_ (Map.lookupLE sq through) $ \(_, n) -> modifyTVar' (fsAckedItems st) (max n)
      writeTVar (fsThrough st) (snd (Map.split sq through))

-- | The feeding: wait for the port and for credit; read what credit
-- allows (the first item waits for the source, the rest only what is
-- ready); send it as one frame (a bytes item as a binary frame of its
-- own); at the end of the source, @end@. The callee closing the half stops
-- it, reading no more.
feed :: ItemKind -> Reader -> FeedState -> IO ()
feed _kind rd st = do
  port <- atomically (readTMVar (fsPort st))
  let loop sq = do
        go' <- atomically $ do
          done <- readTVar (fsDone st)
          closed <- readTVar (fsClosed st)
          if done || isJust closed then pure False else do
            c <- readTVar (fsCredit st)
            check (c > 0)
            pure True
        when go' $ do
          first <- rdNext rd
          case first of
            Nothing -> end port sq
            Just (BytesItem b) -> do
              stillOpen <- open'
              when stillOpen $ do
                sent sq 1
                ipBinary port sq b
                loop (sq + 1)
            Just (JsonItem v n) -> gather port sq [v] 1 n
      -- more of what is ready, within the credit and the frame's size
      gather port sq acc count size = do
        c <- readTVarIO (fsCredit st)
        more <- if c > fromIntegral count && count < frameItems && size < frameBytes then rdReady rd else pure False
        if not more then flush port sq acc count >> loop (sq + 1) else do
          -- the items before a bad line go out as read; the bad one never does
          next <- rdNext rd `catch` \(e :: BadLine) -> flush port sq acc count >> throwIO e
          case next of
            Just (JsonItem v n) -> gather port sq (v : acc) (count + 1) (size + n)
            Just (BytesItem _) -> flush port sq acc count >> loop (sq + 1)   -- never: one kind a source
            Nothing -> do
              flush port sq acc count
              end port (sq + 1)
      flush port sq acc count = do
        stillOpen <- open'
        when stillOpen $ do
          sent sq count
          ipItems port sq (reverse acc)
      end port sq = do
        stillOpen <- open'
        when stillOpen $ do
          ipEnd port sq
          atomically (writeTVar (fsEnded st) True)
      open' = atomically $ (&&) <$> (not <$> readTVar (fsDone st)) <*> (not . isJust <$> readTVar (fsClosed st))
      -- counted as it goes out (before the send: an ack can only follow
      -- it, and a frame whose send failed may have gone, so it is unacked)
      sent sq count = atomically $ do
        modifyTVar' (fsCredit st) (subtract (fromIntegral count))
        total <- (+ fromIntegral count) <$> readTVar (fsItems st)
        writeTVar (fsItems st) total
        modifyTVar' (fsFrames st) (+ 1)
        modifyTVar' (fsThrough st) (Map.insert (toInteger sq) total)
  loop (0 :: Word64)

tallyOf :: FeedState -> IO Tally
tallyOf st = atomically $ do
  items <- readTVar (fsItems st)
  frames <- readTVar (fsFrames st)
  acked <- readTVar (fsAcked st)
  ackedItems <- readTVar (fsAckedItems st)
  credit <- readTVar (fsCredit st)
  ended <- readTVar (fsEnded st)
  closed <- readTVar (fsClosed st)
  pure Tally { tItems = items, tFrames = frames, tAcked = acked
             , tUnacked = max 0 (items - ackedItems)
             , tCredit = credit, tEnded = ended, tClosed = closed }

-- | @--progress@: the tally on stderr once a second.
tick :: FeedState -> IO ()
tick st = loop
  where
    loop = do
      threadDelay 1000000
      t <- tallyOf st
      TIO.hPutStrLn stderr (renderTally t <> " · credit " <> commas (tCredit t))
      hFlush stderr
      loop

-- | @input: sent 1,204 items in 1,204 frames · acked through seq 1,179 · 25 unacked@
renderTally :: Tally -> Text
renderTally t = "input: sent " <> commas (tItems t) <> " item" <> plural (tItems t)
  <> " in " <> commas (tFrames t) <> " frame" <> plural (tFrames t)
  <> " · " <> maybe "nothing acked" (\a -> "acked through seq " <> commas a) (tAcked t)
  <> " · " <> commas (tUnacked t) <> " unacked"
  where plural n = if n == 1 then "" else "s"

commas :: Integer -> Text
commas n
  | n < 0 = "-" <> commas (negate n)
  | otherwise = T.reverse (T.intercalate "," (T.chunksOf 3 (T.reverse (T.pack (show n)))))

-- | A typed close of the input half on an error item: its @error_kind@ and
-- the frame it names.
inputRefusal :: PlexusStreamItem -> Maybe (Text, Maybe Integer)
inputRefusal = \case
  StreamErrorWith _ _ err _ _ beside
    | Just kind <- kindOf err beside, kind `elem` inputKinds -> Just (kind, seqOf err beside)
  _ -> Nothing
  where
    inputKinds = ["past_credit", "invalid_input", "out_of_sequence", "too_much_input"]
    kindOf err beside = case KM.lookup "error_kind" beside of
      Just (String k) -> Just k
      _ -> case err of
        RE.Other k _ -> Just k
        _ -> Nothing
    seqOf err beside = case KM.lookup "seq" beside of
      Just (Number n) -> Just (round n)
      _ -> case err of
        RE.Other _ fields | Just (Number n) <- KM.lookup "seq" fields -> Just (round n)
        _ -> Nothing

-- | The exit a close calls for: @past_credit@ 70 (EX_SOFTWARE: synapse
-- sent past what it was granted — its own bug); @invalid_input@,
-- @out_of_sequence@, @too_much_input@ 65 (EX_DATAERR); an input closed
-- because the sign-in was revoked, signed out or expired 77.
closeExit :: Maybe (Text, Maybe Integer) -> Maybe (Text, Maybe Integer) -> Maybe (Int, Text)
closeExit refusal closed = case (refusal, closed) of
  (Just ("past_credit", sq), _) -> Just (70, "the service says synapse sent past its credit" <> at sq <> " — a bug in synapse")
  (Just (kind, sq), _) -> Just (65, "the service refused the input (" <> kind <> ")" <> at sq)
  (Nothing, Just (reason, _))
    | reason `elem` ["revoked", "signed_out", "expired", "forbidden", "unauthenticated"] ->
        Just (77, "the service closed the input: " <> reason <> " — sign in again (_login)")
  _ -> Nothing
  where
    at = maybe "" (\s -> " at frame " <> commas s)

-- ---------------------------------------------------------------------------
-- A dry run

-- | @-n@: the first input frame the call would send — a JSON one with its
-- items (the subscription @null@: the server names it when it takes the
-- call), a binary one as its header in hex and its length — or @end@ for
-- an empty source. Reads one item of the source.
dryRunFrame :: Text -> ItemKind -> Int -> InputSource -> IO (Either Text Text)
dryRunFrame ns kind chunk src = do
  h <- openSource src
  rd <- newReader kind chunk h
  r <- try (rdNext rd)
  case src of
    FromFile _ -> hClose h `catch` \(_ :: SomeException) -> pure ()
    FromStdin -> pure ()
  pure $ case r of
    Left (BadLine n why) -> Left (lineError n why)
    Right Nothing -> Right (frame "end" [])
    Right (Just (JsonItem v _)) -> Right (frame "input" ["items" .= [v]])
    Right (Just (BytesItem b)) -> Right $
      "binary: " <> hex 0x01 2 <> " " <> hex 0 16 <> " " <> hex 0 16 <> " + " <> commas (toInteger (BS.length b)) <> " bytes"
        <> " ([0x01][subscription u64][seq u64][payload]; the subscription is the call's)"
  where
    -- the subscription null: the server names it when it takes the call
    frame verb extra = TE.decodeUtf8 $ LBS8.toStrict $ encode $ object
      [ "jsonrpc" .= ("2.0" :: Text), "method" .= (ns <> "." <> verb)
      , "params" .= object (["subscription" .= Null, "seq" .= (0 :: Int)] ++ extra) ]
    hex :: Integer -> Int -> Text
    hex n w = let s = T.pack (showHex n "") in T.replicate (w - T.length s) "0" <> s

-- | How a bad line is said.
lineError :: Int -> Text -> Text
lineError n why = "input line " <> commas (toInteger n) <> " is not an item: " <> why
