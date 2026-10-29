{-# LANGUAGE OverloadedStrings #-}

-- | SYNAPSE-CC-8: menger's activation schemas of 2026-10-29, offline.
--
-- @test/fixtures/ir-coverage/menger-2026-10-29.schemas.jsonl.gz@ holds the
-- root and its 20 activations, one line each: @{"path": [...], "schema":
-- <PluginSchema>}@, as @<ns>.schema@ answered on app.menger.sh. They are
-- handed to the IR builder through the run's schema memo ('seFetched'), so
-- 'buildIR' walks them with no network: the builder alone.
module IRFixture
  ( menger20261029
  , loadSchemas
  , envWithSchemas
  , buildFixtureIR
  , fixtureIR
  ) where

import qualified Codec.Compression.GZip as GZ
import Data.Aeson (FromJSON(..), eitherDecode, withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef (writeIORef)
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Katip

import Synapse.IR.Builder (buildIR)
import Synapse.IR.Types (IR)
import qualified Synapse.Log as Log
import Synapse.Monad
import Synapse.Schema.Types (Path, PluginSchema)

menger20261029 :: FilePath
menger20261029 = "test/fixtures/ir-coverage/menger-2026-10-29.schemas.jsonl.gz"

data Row = Row Path PluginSchema

instance FromJSON Row where
  parseJSON = withObject "Row" $ \o -> Row <$> o .: "path" <*> o .: "schema"

-- | Every schema of a fixture, by path: a @.jsonl.gz@ one row a line
--   (gunzipped), a @.json@ an array of rows.
loadSchemas :: FilePath -> IO [(Path, PluginSchema)]
loadSchemas file = do
  raw <- BL.readFile file
  if ".json" `isSuffixOf` file
    then case eitherDecode raw of
      Right rows -> pure [ (p, s) | Row p s <- rows ]
      Left e -> fail ("IRFixture: " <> file <> ": " <> e)
    else do
      let body = if ".gz" `isSuffixOf` file then GZ.decompress raw else raw
      mapM row (filter (not . BL.null) (BL.lines body))
  where
    row l = case eitherDecode l of
      Right (Row p s) -> pure (p, s)
      Left e -> fail ("IRFixture: " <> file <> ": " <> e)

-- | An environment whose schemas are these, already fetched.
envWithSchemas :: Text -> [(Path, PluginSchema)] -> IO SynapseEnv
envWithSchemas backend schemas = do
  logger <- Log.makeLogger Katip.ErrorS
  env <- initEnv "127.0.0.1" 1 backend logger Nothing
  writeIORef (seFetched env) (Map.fromList schemas)
  pure env

-- | The IR of the tree under a path, built from the memo alone.
buildFixtureIR :: SynapseEnv -> Path -> IO IR
buildFixtureIR env path = runSynapseM env (buildIR [] path) >>= either (fail . show) pure

-- | The IR of a fixture file, built from the tree under a path.
fixtureIR :: FilePath -> Path -> IO IR
fixtureIR file path = do
  schemas <- loadSchemas file
  env <- envWithSchemas "menger" schemas
  buildFixtureIR env path
