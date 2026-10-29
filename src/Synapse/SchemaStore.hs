{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | SYNAPSE-28: what a call needs from a backend's schema, kept on disk so
-- a call sends first and checks only on failure.
--
-- Under @$XDG_CACHE_HOME/synapse/schemas@ (@~/.cache/synapse/schemas@):
--
-- @
-- routes/CONNECTION-POINT/BACKEND.json      where BACKEND is reached from there
-- ADDRESS/AGENT/BACKEND/info.json           its _info, as this agent was answered
-- ADDRESS/AGENT/BACKEND/PATH\@VERSION-HASH.json   an activation's schema (PATH: _root, tickets, tickets.inputs …)
-- ADDRESS/AGENT/BACKEND/PATH.pin            the file PATH resolves to now
-- ADDRESS/AGENT/BACKEND/plans/METHOD.json   one method's call plan: its schema, its
--                                           activation's deprecation, the IR its flags parse by
-- @
--
-- A schema is per agent because a caller's view of it is (AUTH-8). Old
-- versions stay beside the pin (SYNAPSE-29 adds pinning by version and
-- pruning); a plan is what a warm call reads — a few kilobytes, where
-- menger's @tickets@ schema is 2.6 MB.
module Synapse.SchemaStore
  ( Store(..)
  , storeFor
  , schemasRoot

    -- * Routes
  , readRoute
  , writeRoute

    -- * _info
  , readInfo
  , writeInfo

    -- * Schemas
  , readSchema
  , writeSchema

    -- * Plans
  , Plan(..)
  , planKey
  , readPlan
  , writePlan
  , planIR

    -- * Forgetting
  , forget
  ) where

import Control.Exception (SomeException, catch, try)
import Control.Monad (forM_, when)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesDirectoryExist, getHomeDirectory, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension, takeFileName)

import Plexus.Auth (BackendInfo, parseBackendInfo)
import Plexus.Schema.Recursive (PluginSchema(..), MethodSchema, DeprecationInfo)
import Synapse.Endpoint (Endpoint(..), dirName)
import Synapse.Identity.Store (writePrivateFile)
import Synapse.IR.Types

-- | One backend's cache, as one agent sees it at one address.
newtype Store = Store { storeDir :: FilePath }
  deriving (Eq, Show)

-- | @$XDG_CACHE_HOME/synapse/schemas@, else @~/.cache/synapse/schemas@.
schemasRoot :: IO FilePath
schemasRoot = do
  x <- lookupEnv "XDG_CACHE_HOME"
  base <- case x of
    Just d | not (null d) -> pure d
    _ -> (</> ".cache") <$> getHomeDirectory
  pure (base </> "synapse" </> "schemas")

storeFor :: Endpoint -> Text -> Text -> IO Store
storeFor ep agent backend = do
  root <- schemasRoot
  pure (Store (root </> dirName ep </> safe agent </> safe backend))

-- | A name as a file name: what is not @[A-Za-z0-9._-]@ becomes @_@.
safe :: Text -> FilePath
safe t = case T.unpack (T.map (\c -> if isAlphaNum c || c `elem` ("._-" :: String) then c else '_') t) of
  "" -> "_"
  "." -> "_"
  ".." -> "__"
  s -> s

readJson :: FromJSON a => FilePath -> IO (Maybe a)
readJson p = do
  r <- try (BS.readFile p)
  pure $ case r of
    Left (_ :: SomeException) -> Nothing
    Right bs -> decodeStrict bs

-- | Written whole and renamed into place (0600), so a reader never sees
-- half a file; a cache that cannot be written is no failure of the call.
writeJson :: ToJSON a => FilePath -> a -> IO ()
writeJson p v = writePrivateFile p (LBS.toStrict (encode v)) `catch` \(_ :: SomeException) -> pure ()

-- ---------------------------------------------------------------------------
-- Routes

routePath :: Endpoint -> Text -> IO FilePath
routePath cp backend = do
  root <- schemasRoot
  pure (root </> "routes" </> dirName cp </> (safe backend <> ".json"))

-- | Where the backend was found from this connection point (itself, or a
-- registry's entry): the address calls go to.
readRoute :: Endpoint -> Text -> IO (Maybe Endpoint)
readRoute cp backend = do
  p <- routePath cp backend
  v <- readJson p
  pure $ case v of
    Just (Object o)
      | Just (String h) <- KM.lookup "host" o
      , Just (Number port) <- KM.lookup "port" o
      , Just (Bool tls) <- KM.lookup "tls" o
      , Just (String path) <- KM.lookup "path" o
      -> Just Endpoint { epHost = h, epPort = round port, epTls = tls, epPath = path
                       , epCaFiles = if h == epHost cp && round port == epPort cp then epCaFiles cp else [] }
    _ -> Nothing

writeRoute :: Endpoint -> Text -> Endpoint -> IO ()
writeRoute cp backend at = do
  p <- routePath cp backend
  writeJson p (object ["host" .= epHost at, "port" .= epPort at, "tls" .= epTls at, "path" .= epPath at])

-- ---------------------------------------------------------------------------
-- _info

readInfo :: Store -> IO (Maybe BackendInfo)
readInfo st = (>>= parseBackendInfo) <$> readJson (storeDir st </> "info.json")

writeInfo :: Store -> BackendInfo -> IO ()
writeInfo st = writeJson (storeDir st </> "info.json")

-- ---------------------------------------------------------------------------
-- Schemas

pathName :: [Text] -> FilePath
pathName [] = "_root"
pathName segs = safe (T.intercalate "." segs)

readSchema :: Store -> [Text] -> IO (Maybe PluginSchema)
readSchema st segs = do
  pin <- try (BS.readFile (storeDir st </> (pathName segs <> ".pin")))
  case pin of
    Left (_ :: SomeException) -> pure Nothing
    Right f -> readJson (storeDir st </> takeFileName (T.unpack (T.strip (TE.decodeUtf8 f))))

-- | Kept as @PATH\@VERSION-HASH.json@ and pinned.
writeSchema :: Store -> [Text] -> PluginSchema -> IO ()
writeSchema st segs schema = do
  let file = pathName segs <> "@" <> safe (psVersion schema) <> "-" <> safe (psHash schema) <> ".json"
  writeJson (storeDir st </> file) schema
  writePrivateFile (storeDir st </> (pathName segs <> ".pin")) (TE.encodeUtf8 (T.pack file))
    `catch` \(_ :: SomeException) -> pure ()

-- ---------------------------------------------------------------------------
-- Plans

-- | What a warm call reads instead of its activation's schema.
data Plan = Plan
  { plMethod      :: MethodSchema
  , plPath        :: [Text]                  -- ^ the method's path as navigation resolved it
  , plDeprecation :: Maybe DeprecationInfo   -- ^ its activation's
  , plIR          :: Maybe IR                -- ^ the method and the types its params name; Nothing when it takes none
  } deriving (Eq, Show)

instance ToJSON Plan where
  toJSON Plan{..} = object
    [ "method" .= plMethod, "path" .= plPath, "deprecation" .= plDeprecation, "ir" .= plIR ]

instance FromJSON Plan where
  parseJSON = withObject "Plan" $ \o -> Plan
    <$> o .: "method" <*> o .: "path" <*> o .:? "deprecation" <*> o .:? "ir"

-- | The plan's file name for the path as the command line gave it.
planKey :: [Text] -> FilePath
planKey segs = pathName segs <> ".json"

-- | SYNAPSE-CC-8: a plan whose IR another synapse wrote (an older IR
-- version: 3.16.0's 2.0) is not this one's; it is read as no plan, so the
-- call asks the server and writes a plan of its own.
readPlan :: Store -> [Text] -> IO (Maybe Plan)
readPlan st segs = do
  plan <- readJson (storeDir st </> "plans" </> planKey segs)
  pure $ case plan of
    Just p | maybe False ((/= currentIrVersion) . irVersion) (plIR p) -> Nothing
    other -> other

writePlan :: Store -> [Text] -> Plan -> IO ()
writePlan st segs = writeJson (storeDir st </> "plans" </> planKey segs)

-- | The IR a plan keeps: the one method, and the types its params reach.
planIR :: Text -> IR -> IR
planIR fullPath ir = ir
  { irMethods = Map.filterWithKey (\k _ -> k == fullPath) (irMethods ir)
  , irTypes = Map.restrictKeys (irTypes ir) reached
  , irPlugins = Map.empty
  }
  where
    roots = [pdType p | Just md <- [Map.lookup fullPath (irMethods ir)], p <- mdParams md]
    reached = go Set.empty (concatMap names roots)
    go seen [] = seen
    go seen (n : rest)
      | n `Set.member` seen = go seen rest
      | otherwise = go (Set.insert n seen) (more n ++ rest)
    more n = maybe [] (kindNames . tdKind) (Map.lookup n (irTypes ir))
    kindNames = \case
      KindStruct fs -> concatMap (names . fdType) fs
      KindEnum _ vs _ -> concatMap (\v -> concatMap (names . fdType) (vdFields v) ++ maybe [] names (vdRef v)) vs
      KindAlias t -> names t
      _ -> []
    names = \case
      RefNamed qn -> [qualifiedNameFull qn, qnLocalName qn]
      t -> concatMap names (typeRefChildren t)

-- ---------------------------------------------------------------------------
-- Forgetting

-- | A call the cache misled (a method renamed, a parameter changed): its
-- plans and pins go, so every call after it reads what the server says
-- now. The schema files stay (the versions SYNAPSE-29 pins between).
forget :: Store -> IO ()
forget st = do
  let dir = storeDir st
  ok <- doesDirectoryExist dir
  when ok $ do
    fs <- listDirectory dir `catch` \(_ :: SomeException) -> pure []
    forM_ [f | f <- fs, takeExtension f == ".pin"] $ \f -> rm (dir </> f)
    let plans = dir </> "plans"
    pk <- doesDirectoryExist plans
    when pk $ do
      ps <- listDirectory plans `catch` \(_ :: SomeException) -> pure []
      forM_ ps $ \f -> rm (plans </> f)
  where
    rm p = removeFile p `catch` \(_ :: SomeException) -> pure ()
