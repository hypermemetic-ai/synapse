{-# LANGUAGE RecordWildCards #-}

-- | Parameter transformation layer for CLI
--
-- Provides programmable transformations for command-line parameters before
-- they're parsed by the IR-driven parser. This allows context-aware expansions
-- like path resolution and environment variable substitution.
--
-- = Architecture
--
-- @
-- Command line args
--   ↓
-- parsePathAndParams (find the path and the flag arguments)
--   ↓
-- pairFlags (pair --key value / --key=value by parameter type)
--   ↓
-- [(Text, Text)] raw params
--   ↓
-- transformParams (THIS MODULE - middleware chain)
--   ↓
-- [(Text, Text)] transformed params
--   ↓
-- parseParams (IR-driven type parsing)
--   ↓
-- Value (JSON sent to backend)
-- @
--
-- = Example
--
-- @
-- -- User runs: synapse claudecode create --name test --working_dir .
-- -- Input: [("name", "test"), ("working_dir", ".")]
-- env <- mkTransformEnv
-- transformed <- transformParams env defaultTransformers params
-- -- Output: [("name", "test"), ("working_dir", "/absolute/cwd")]
-- @
module Synapse.CLI.Transform
  ( -- * Transformation Pipeline
    TransformEnv(..)
  , Transformer
  , transformParams
  , mkTransformEnv

    -- * Built-in Transformers
  , pathExpansion
  , envExpansion
  , defaultTransformers

    -- * Smart Defaults (Type-Aware)
  , injectSmartDefaults
  , injectBooleanDefaults
  , isPathParamByType
  , getSmartDefault

    -- * Utilities
  , expandPath
  , isPathParam
  ) where

import Control.Exception (catch, IOException)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (getCurrentDirectory, makeAbsolute, getHomeDirectory)
import System.Environment (getEnvironment)
import System.FilePath (isRelative, (</>))

-- For type-aware smart defaults
import Synapse.IR.Types (IR, MethodDef(..), ParamDef(..), TypeRef(..))

-- ============================================================================
-- Types
-- ============================================================================

-- | Environment available to transformers
data TransformEnv = TransformEnv
  { teCwd  :: FilePath            -- ^ Current working directory
  , teHome :: Maybe FilePath      -- ^ Home directory
  , teEnv  :: [(String, String)]  -- ^ Environment variables
  }
  deriving (Show, Eq)

-- | A transformer: inspects/modifies parameter pairs
--
-- Transformers are fail-safe: if transformation fails, return original value.
-- The server has authoritative validation, so it's better to pass through
-- potentially-wrong values than to fail CLI invocation.
type Transformer = TransformEnv -> (Text, Text) -> IO (Text, Text)

-- ============================================================================
-- Transformation Pipeline
-- ============================================================================

-- | Apply a chain of transformers to parameter pairs
--
-- Each transformer in the list runs sequentially. Transformers can:
-- - Modify values based on keys
-- - Leave unmatched parameters unchanged
-- - Handle errors gracefully by returning original values
transformParams :: TransformEnv -> [Transformer] -> [(Text, Text)] -> IO [(Text, Text)]
transformParams env transformers params =
  foldl applyTransformer (pure params) transformers
  where
    applyTransformer :: IO [(Text, Text)] -> Transformer -> IO [(Text, Text)]
    applyTransformer mParams transformer = do
      ps <- mParams
      mapM (transformer env) ps

-- | Create transformation environment from system state
mkTransformEnv :: IO TransformEnv
mkTransformEnv = do
  cwd <- getCurrentDirectory
  home <- (Just <$> getHomeDirectory) `catch` \(_ :: IOException) -> pure Nothing
  env <- getEnvironment
  pure TransformEnv
    { teCwd = cwd
    , teHome = home
    , teEnv = env
    }

-- | Default transformer chain
--
-- Applied to all CLI invocations unless disabled.
-- Order matters: earlier transformers run first.
defaultTransformers :: [Transformer]
defaultTransformers =
  [ pathExpansion
  , envExpansion
  ]

-- ============================================================================
-- Path Expansion Transformer
-- ============================================================================

-- | Transform path parameters to absolute paths
--
-- Handles:
-- - @--path .@ → @--path \/absolute\/cwd@
-- - @--path ~\/foo@ → @--path \/home\/user\/foo@
-- - @--working_dir relative@ → @--working_dir \/absolute\/path@
-- - Leaves absolute paths unchanged
--
-- Fail-safe: Returns original value if expansion fails.
pathExpansion :: Transformer
pathExpansion env@TransformEnv{..} (key, val)
  | isPathParam key = do
      expanded <- expandPath teCwd teHome val
      pure (key, expanded)
  | otherwise = pure (key, val)

-- | Check if a parameter name represents a filesystem path
--
-- Add parameter names to this list to enable path expansion for them.
isPathParam :: Text -> Bool
isPathParam k = k `elem`
  [ "path"
  , "working_dir"
  , "output_dir"
  , "file_path"
  , "dir"
  , "directory"
  , "workdir"
  ]

-- | Expand a path string to absolute form
--
-- Handles:
-- - @.@ → current working directory
-- - @~\/foo@ → home directory + foo
-- - @relative\/path@ → cwd + relative\/path
-- - @\/absolute@ → unchanged
--
-- Fail-safe: Returns original path text if expansion fails.
expandPath :: FilePath -> Maybe FilePath -> Text -> IO Text
expandPath cwd mHome pathText = do
  let path = T.unpack pathText
  expanded <- tryExpand path `catch` \(_ :: IOException) -> pure path
  pure $ T.pack expanded
  where
    tryExpand :: FilePath -> IO FilePath
    tryExpand "." = makeAbsolute cwd
    tryExpand ('~':'/':rest) = case mHome of
      Just home -> makeAbsolute (home </> rest)
      Nothing -> pure ('~':'/':rest)  -- Can't expand, return as-is
    tryExpand t@('~':_) = pure t  -- ~user form not supported
    tryExpand p
      | isRelative p = makeAbsolute (cwd </> p)
      | otherwise = pure p  -- Already absolute

-- ============================================================================
-- Environment Variable Expansion
-- ============================================================================

-- | Expand environment variables in parameter values
--
-- Handles:
-- - @--value $VAR@ → @--value \<env_value\>@
-- - @--message \"Hello $USER\"@ → @--message \"Hello alice\"@
--
-- Simple implementation: only expands $VARNAME format (not ${VAR}).
-- Fail-safe: Returns original value if variable not found.
envExpansion :: Transformer
envExpansion TransformEnv{..} (key, val)
  | T.any (== '$') val = do
      let expanded = expandEnvVars teEnv val
      pure (key, expanded)
  | otherwise = pure (key, val)

-- | Expand $VAR references in text
--
-- Uses simple string replacement. Does not handle:
-- - ${VAR} syntax
-- - Default values (${VAR:-default})
-- - Command substitution
--
-- Fail-safe: Leaves $VAR unchanged if not found in environment.
expandEnvVars :: [(String, String)] -> Text -> Text
expandEnvVars env text =
  -- Split on '$' and process each potential variable reference
  let parts = T.splitOn "$" text
  in case parts of
    [] -> text
    (first:rest) -> first <> T.concat (map expandPart rest)
  where
    -- Try to expand a part that comes after '$'
    expandPart :: Text -> Text
    expandPart part =
      let (varName, suffix) = T.span isVarChar part
      in if T.null varName
         then "$" <> part  -- No valid var name, keep $
         else case lookup (T.unpack varName) env of
           Just value -> T.pack value <> suffix
           Nothing -> "$" <> part  -- Var not found, keep as-is

    -- Characters allowed in environment variable names
    isVarChar :: Char -> Bool
    isVarChar c = c `elem` (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_")

-- ============================================================================
-- Smart Defaults (Type-Aware)
-- ============================================================================

-- | Inject smart defaults for missing required parameters
--
-- This is type-aware: it inspects the method's parameter definitions
-- to determine which params are required and what types need defaults.
--
-- Currently supports:
-- - Path parameters (working_dir, path, etc.): Default to current working directory
--
-- Only injects defaults for parameters that are:
-- 1. Required (not optional)
-- 2. Not provided by the user
-- 3. Have a known smart default for their type
--
-- = Example
--
-- @
-- -- User runs: synapse claudecode create --name test --model opus
-- -- Method requires: name, working_dir, model
-- -- User provided: name, model
-- -- Missing: working_dir
--
-- injectSmartDefaults env ir methodDef [(\"name\", \"test\"), (\"model\", \"opus\")]
-- -- Returns: [(\"name\", \"test\"), (\"model\", \"opus\"), (\"working_dir\", \"\/cwd\")]
-- @
injectSmartDefaults :: TransformEnv -> IR -> MethodDef -> [(Text, Text)] -> IO [(Text, Text)]
injectSmartDefaults env _ir methodDef existingParams = do
  let providedKeys = map fst existingParams
      requiredParams = filter pdRequired (mdParams methodDef)
      missingRequired = filter (\p -> pdName p `notElem` providedKeys) requiredParams

  -- For each missing required param, try to inject a smart default
  defaults <- mapM (tryInjectDefault env) missingRequired
  pure $ existingParams ++ catMaybes defaults
  where
    tryInjectDefault :: TransformEnv -> ParamDef -> IO (Maybe (Text, Text))
    tryInjectDefault e param = do
      mDefault <- getSmartDefault e param
      case mDefault of
        Just val -> pure $ Just (pdName param, val)
        Nothing -> pure Nothing

-- | Inject "true" for boolean flags that were present but had no value
--
-- A bare @--force@ is already @("force", "true")@ from @pairFlags@; @--force=@
-- arrives as @("force", "")@. This function looks at the method definition to determine
-- which empty-value params are actually booleans, and fills them with "true".
--
-- Non-boolean params with empty values are left as-is and will fail validation
-- later with a clear error message.
--
-- = Example
--
-- @
-- -- User runs: synapse cone delete --identifier.type by_name --identifier.name test --force
-- -- Parsed: [("identifier.type", "by_name"), ("identifier.name", "test"), ("force", "")]
-- injectBooleanDefaults ir methodDef params
-- -- Returns: [("identifier.type", "by_name"), ("identifier.name", "test"), ("force", "true")]
-- @
injectBooleanDefaults :: IR -> MethodDef -> [(Text, Text)] -> [(Text, Text)]
injectBooleanDefaults _ir methodDef params =
  map fillBooleanDefault params
  where
    -- Create a map of param names to their definitions for quick lookup
    paramDefMap = [(pdName p, p) | p <- mdParams methodDef]

    fillBooleanDefault :: (Text, Text) -> (Text, Text)
    fillBooleanDefault (key, val)
      -- If value is empty (flag with no value) and param is boolean, fill with "true"
      | val == "", Just paramDef <- lookup key paramDefMap, isBooleanParam paramDef =
          (key, "true")
      -- Otherwise keep as-is
      | otherwise = (key, val)

    -- Check if a parameter is a boolean type (including optional booleans)
    isBooleanParam :: ParamDef -> Bool
    isBooleanParam param = isBooleanType (pdType param)

    isBooleanType :: TypeRef -> Bool
    isBooleanType (RefPrimitive "boolean" _) = True
    isBooleanType (RefOptional inner) = isBooleanType inner
    isBooleanType _ = False

-- | Check if a parameter represents a filesystem path based on type info
--
-- Uses both name heuristics AND type information:
-- - Name matches known path params (working_dir, path, etc.)
-- - Type is string (not UUID, not enum, etc.)
--
-- This is more robust than just checking names, as it avoids false positives
-- like a parameter named \"path\" that's actually a URL or other non-filesystem type.
isPathParamByType :: ParamDef -> Bool
isPathParamByType param =
  let nameMatches = pdName param `elem` pathParamNames
      typeIsString = case pdType param of
        RefPrimitive "string" _ -> True
        RefOptional (RefPrimitive "string" _) -> True
        _ -> False
  in nameMatches && typeIsString
  where
    pathParamNames =
      [ "path"
      , "working_dir"
      , "output_dir"
      , "file_path"
      , "dir"
      , "directory"
      , "workdir"
      ]

-- | Get smart default value for a parameter based on its type
--
-- Returns Nothing if no smart default is available.
-- This is the extensibility point for adding new smart defaults.
--
-- = Current Defaults
--
-- - Path parameters → current working directory
--
-- = Future Extensions
--
-- Could add:
-- - UUID parameters → generated UUID
-- - Timestamp parameters → current time
-- - User name parameters → $USER from environment
getSmartDefault :: TransformEnv -> ParamDef -> IO (Maybe Text)
getSmartDefault env param
  -- Path parameters: use current working directory
  | isPathParamByType param =
      pure $ Just (T.pack $ teCwd env)

  -- Future: UUID parameters could generate fresh UUIDs
  -- | isUuidParam param = Just <$> generateUUID

  -- Future: Timestamp parameters could use current time
  -- | isTimestampParam param = Just <$> getCurrentTimestamp

  -- No smart default available
  | otherwise = pure Nothing
