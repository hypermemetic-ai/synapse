{-# LANGUAGE RecordWildCards #-}

-- | IR caching with hash-based invalidation
--
-- Caches built IR keyed by plugin hash to avoid rebuilding when schemas haven't changed.
-- The plugin hash changes whenever any method or child activation changes, making it
-- perfect for cache invalidation.
module Synapse.IR.Cache
  ( IRCache
  , CacheConfig(..)
  , defaultCacheConfig
  , newIRCache
  , lookupIR
  , insertIR
  , irCacheKey
  , clearCache
  , getCacheStats
  , CacheStats(..)
  ) where

import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>))
import Data.Aeson (encode, eitherDecodeFileStrict')
import qualified Data.ByteString.Lazy as LBS

import Synapse.IR.Types (IR)

-- | Cache configuration
data CacheConfig = CacheConfig
  { cacheEnabled :: Bool       -- ^ Whether caching is enabled
  , cacheMaxSize :: Int        -- ^ Max number of cached IRs (0 = unlimited)
  , cachePersist :: Bool       -- ^ Persist cache to disk
  , cachePath    :: FilePath   -- ^ Path to cache directory
  }
  deriving stock (Show, Eq)

-- | Default cache configuration
-- - Enabled by default
-- - Max 100 cached IRs
-- - Persist to ~/.cache/synapse/ir
defaultCacheConfig :: FilePath -> CacheConfig
defaultCacheConfig homeDir = CacheConfig
  { cacheEnabled = True
  , cacheMaxSize = 100
  , cachePersist = True
  , cachePath = homeDir </> ".cache" </> "synapse" </> "ir"
  }

-- | Cache statistics
data CacheStats = CacheStats
  { statsHits   :: Int  -- ^ Cache hits
  , statsMisses :: Int  -- ^ Cache misses
  , statsSize   :: Int  -- ^ Current cache size
  }
  deriving stock (Show, Eq)

-- | In-memory IR cache
data IRCache = IRCache
  { cacheConfig  :: CacheConfig
  , cacheStore   :: MVar (Map Text IR)
  , cacheStats   :: MVar CacheStats
  }

-- | Create a new IR cache
newIRCache :: CacheConfig -> IO IRCache
newIRCache config = do
  when (cachePersist config) $
    createDirectoryIfMissing True (cachePath config)

  store <- newMVar Map.empty
  stats <- newMVar (CacheStats 0 0 0)

  pure IRCache
    { cacheConfig = config
    , cacheStore = store
    , cacheStats = stats
    }

-- | AUTH-8: the key an IR is kept under — the plugin hash, qualified by
-- the caller's view when the hub has one (@VIEW_HASH@ joined with @-@, a
-- file name). Callers pass this to 'lookupIR' / 'insertIR' so two
-- callers' views of one hub never share an IR on disk.
irCacheKey :: Maybe Text -> Text -> Text
irCacheKey Nothing hash = hash
irCacheKey (Just v) hash
  | T.null v = hash
  | otherwise = T.map safe v <> "-" <> hash
  where
    safe c = if c `elem` ("/\\:" :: String) then '_' else c

-- | Look up IR by hash (see 'irCacheKey' for a view's)
lookupIR :: IRCache -> Text -> IO (Maybe IR)
lookupIR IRCache{..} hash
  | not (cacheEnabled cacheConfig) = pure Nothing
  | otherwise = do
      -- Try memory cache first
      mIR <- Map.lookup hash <$> readMVar cacheStore
      case mIR of
        Just ir -> do
          recordHit
          pure (Just ir)
        Nothing
          | cachePersist cacheConfig -> do
              -- Try disk cache
              mDiskIR <- loadFromDisk hash
              case mDiskIR of
                Just ir -> do
                  -- Load into memory cache
                  modifyMVar_ cacheStore $ \store ->
                    pure $ Map.insert hash ir store
                  recordHit
                  pure (Just ir)
                Nothing -> do
                  recordMiss
                  pure Nothing
          | otherwise -> do
              recordMiss
              pure Nothing
  where
    recordHit = modifyMVar_ cacheStats $ \stats ->
      pure $ stats { statsHits = statsHits stats + 1 }

    recordMiss = modifyMVar_ cacheStats $ \stats ->
      pure $ stats { statsMisses = statsMisses stats + 1 }

    loadFromDisk hash = do
      let path = cachePath cacheConfig </> T.unpack hash <> ".json"
      exists <- doesFileExist path
      if exists
        then do
          result <- eitherDecodeFileStrict' path
          case result of
            Right ir -> pure (Just ir)
            Left _   -> pure Nothing  -- Corrupted cache, ignore
        else pure Nothing

-- | Insert IR into cache
insertIR :: IRCache -> Text -> IR -> IO ()
insertIR IRCache{..} hash ir
  | not (cacheEnabled cacheConfig) = pure ()
  | otherwise = do
      -- Insert into memory cache
      modifyMVar_ cacheStore $ \store -> do
        let newStore = Map.insert hash ir store
        -- Evict if over max size (simple LRU: remove first element)
        let finalStore = if cacheMaxSize cacheConfig > 0 && Map.size newStore > cacheMaxSize cacheConfig
              then Map.deleteMin newStore
              else newStore
        pure finalStore

      -- Update stats
      modifyMVar_ cacheStats $ \stats ->
        pure $ stats { statsSize = statsSize stats + 1 }

      -- Persist to disk if enabled
      when (cachePersist cacheConfig) $ do
        let path = cachePath cacheConfig </> T.unpack hash <> ".json"
        LBS.writeFile path (encode ir)

-- | Clear the cache
clearCache :: IRCache -> IO ()
clearCache IRCache{..} = do
  modifyMVar_ cacheStore $ \_ -> pure Map.empty
  modifyMVar_ cacheStats $ \_ -> pure (CacheStats 0 0 0)

-- | Get cache statistics
getCacheStats :: IRCache -> IO CacheStats
getCacheStats IRCache{..} = readMVar cacheStats
