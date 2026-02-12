{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Synapse CLI - Algebraic Implementation
--
-- This executable uses the categorical machinery:
-- - Effect stack (SynapseM) with caching and cycle detection
-- - Reified algebras for navigation, rendering, completion
-- - Proper error handling
--
-- CLI Structure:
--   synapse [OPTIONS] <backend> <path...> [--param value ...]
--
-- Options must appear BEFORE the backend. Everything after the backend
-- is passed through for method invocation.
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Options.Applicative
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, hFlush, stdout)


import Synapse.Schema.Types
import Synapse.Monad (SynapseM, SynapseEnv(..), SynapseError(..), BackendErrorType(..), TransportContext(..), TransportErrorCategory(..), initEnv, runSynapseM, throwNav, throwTransport, throwParse, throwBackend)
import Synapse.Algebra.Navigate
import Synapse.Algebra.Render (renderSchema)
import Synapse.CLI.Help (renderMethodHelp)
import Synapse.CLI.Parse (parseParams)
import qualified Synapse.CLI.Parse as Parse
import Synapse.IR.Types (IR, irMethods, MethodDef)
import qualified Data.Map.Strict as Map
import qualified Synapse.CLI.Template as TemplateIR
import Synapse.Transport
import Synapse.Bidir (BidirMode(..), parseBidirMode, detectBidirMode, handleBidirRequest)
import Synapse.CLI.Transform (mkTransformEnv, transformParams, defaultTransformers, injectBooleanDefaults, injectSmartDefaults)
import Synapse.IR.Builder (buildIR)
import Synapse.Renderer (RendererConfig, defaultRendererConfig, renderItem, prettyValue, withMethodPath)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import qualified Synapse.Self.Commands as Self
import Synapse.Backend.Discovery (Backend(..), BackendDiscovery(..), registryDiscovery, pingBackends, getBackendAt)

-- ============================================================================
-- Types
-- ============================================================================

-- | Synapse-level options (must appear before backend)
-- Controls connection settings and output format
data SynapseOpts = SynapseOpts
  { soHost          :: Text          -- ^ Registry host for discovery
  , soPort          :: Int           -- ^ Registry port for discovery
  , soJson          :: Bool          -- ^ Output raw JSON stream items
  , soRaw           :: Bool          -- ^ Output raw content (no templates)
  , soDryRun        :: Bool          -- ^ Show request without sending
  , soSchema        :: Bool          -- ^ Fetch raw schema JSON
  , soGenerate      :: Bool          -- ^ Generate templates from IR
  , soEmitIR        :: Bool          -- ^ Emit IR for code generation
  , soParams        :: Maybe Text    -- ^ JSON params via -p
  , soRpc           :: Maybe Text    -- ^ Raw JSON-RPC passthrough
  , soGeneratorInfo :: [Text]        -- ^ Generator tool info (tool:version pairs)
  , soBidirMode     :: Maybe Text    -- ^ Bidirectional mode override
  }
  deriving Show

-- | Full CLI arguments after two-phase parsing
data Args = Args
  { argOpts    :: SynapseOpts   -- ^ Synapse-level options
  , argBackend :: Maybe Text    -- ^ Backend name (first positional)
  , argPath    :: [Text]        -- ^ Path segments and --key value params (raw)
  }
  deriving Show


-- ============================================================================
-- Constants
-- ============================================================================

defaultHost :: Text
defaultHost = "127.0.0.1"

defaultPort :: Int
defaultPort = 4444

-- ============================================================================
-- Argument Splitting
-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  args <- execParser argsInfo
  let opts = argOpts args
  -- Use specified host/port for registry discovery
  let discovery = registryDiscovery (soHost opts) (soPort opts)

  -- Get the backend at the connection point (host:port)
  -- If it has a registry plugin, that's used for discovering other backends
  maybeBackend <- getBackendAt (soHost opts) (soPort opts)
  let hostBackend = maybe "plexus" id maybeBackend

  case argBackend args of
    Nothing
      -- Suppress banner for data output modes
      | soEmitIR opts || soSchema opts || soJson opts || soRaw opts ->
          runWithDiscovery discovery hostBackend args
      | otherwise -> do
          -- No backend specified, show available backends
          TIO.putStr cliHeader
          backends <- discoverBackends discovery
          -- Ping backends to check if they're reachable
          backendsWithStatus <- pingBackends backends
          TIO.putStrLn "\nAvailable backends:"
          mapM_ printBackend backendsWithStatus
          TIO.putStrLn "\nUsage: synapse <backend> [command...]"
    Just backend
      -- Handle --help/-h if it somehow ends up as the backend
      | backend `elem` ["--help", "-h"] -> TIO.putStr cliHeader
      -- Handle _self meta-commands (use discovered primary backend)
      | backend == "_self" -> runWithDiscovery discovery hostBackend args
      | otherwise -> runWithDiscovery discovery backend args

-- | Print a backend in the list
printBackend :: Backend -> IO ()
printBackend b = do
  let nameField = T.justifyLeft 15 ' ' (backendName b)
  let hostPort = backendHost b <> ":" <> T.pack (show (backendPort b))
  let status = case backendReachable b of
        Just True  -> " [OK]"
        Just False -> " [UNREACHABLE]"
        Nothing    -> ""
  let desc = if T.null (backendDescription b)
             then ""
             else " - " <> backendDescription b
  TIO.putStrLn $ "  " <> nameField <> hostPort <> status <> desc

-- | Run a Hub command with backend discovery
runWithDiscovery :: BackendDiscovery -> Text -> Args -> IO ()
runWithDiscovery discovery backendName args = do
  let opts = argOpts args
  -- Try to discover backend info
  maybeBackend <- getBackendInfo discovery backendName

  case maybeBackend of
    Just backend -> do
      -- Always use discovered host/port from registry
      -- The -H/-P flags specify where to find the registry, not the target backend
      run backendName (backendHost backend) (backendPort backend) args
    Nothing -> do
      -- Backend not found in registry - gather available backends and show error
      backends <- discoverBackends discovery
      backendsWithStatus <- pingBackends backends
      env <- initEnv (soHost opts) (soPort opts) backendName
      result <- runSynapseM env (throwBackend (BackendNotFound backendName) backendsWithStatus)
      case result of
        Left err -> do
          hPutStrLn stderr $ renderError err
          exitFailure
        Right () -> exitSuccess

-- | Run a Hub command with specified backend and connection details
run :: Text -> Text -> Int -> Args -> IO ()
run backend host port args = do
  env <- initEnv host port backend
  rendererCfg <- defaultRendererConfig
  result <- runSynapseM env (dispatch args rendererCfg)
  case result of
    Left err -> do
      hPutStrLn stderr $ renderError err
      exitFailure
    Right () -> exitSuccess

-- | Dispatch based on navigation result
dispatch :: Args -> RendererConfig -> SynapseM ()
dispatch Args{argOpts = SynapseOpts{..}, argBackend, argPath} rendererCfg = do
  -- Check for _self commands first (before anything else)
  case argBackend of
    Just "_self" -> do
      -- _self meta-command: parse subcommand and rest from argPath
      let (pathSegs, rawParams, _) = parsePathAndParams argPath
      case pathSegs of
        (subcommand : rest) -> do
          Self.dispatch subcommand rest rawParams
          return ()
        [] -> do
          -- No subcommand provided, show help
          Self.showHelp
          return ()

    -- Normal dispatch
    _ -> do
      -- Mode 1: Raw JSON-RPC passthrough
      case soRpc of
        Just rpcJson -> do
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 rpcJson) of
            Left err -> throwParse $ T.pack err
            Right rpcReq -> do
              items <- invokeRawRpc rpcReq
              liftIO $ mapM_ (printResult soJson soRaw rendererCfg) items
          return ()

        Nothing -> do
          -- Parse path and inline params (--key value pairs)
          let (pathSegs, rawParams, helpRequested) = parsePathAndParams argPath

          -- Normal Plexus RPC routing
          -- Apply parameter transformations (path expansion, env vars)
          transformEnv <- liftIO mkTransformEnv
          inlineParams <- liftIO $ transformParams transformEnv defaultTransformers rawParams

          -- Mode 2: Schema request
          if soSchema
            then do
              -- Try to navigate and determine if last segment is a method
              schemaResult <- fetchSchemaForPath pathSegs
              case schemaResult of
                Left err -> throwNav $ FetchError err pathSegs
                Right val -> liftIO $ LBS.putStrLn $ encode val

            -- Mode 3: Generate templates (IR-driven approach)
            else if soGenerate
              then do
                homeDir <- liftIO getHomeDirectory
                let baseDir = homeDir </> ".config" </> "synapse" </> "templates"
                    writeAndLog gt = do
                      writeGeneratedTemplateIR baseDir gt
                      TIO.putStrLn $ "  " <> T.pack (TemplateIR.gtPath gt)
                liftIO $ TIO.putStrLn $ "Generating templates in " <> T.pack baseDir <> "..."
                count <- TemplateIR.generateAllTemplatesWithCallback writeAndLog pathSegs
                liftIO $ TIO.putStrLn $ "Generated " <> T.pack (show count) <> " templates"

              -- Mode 4: Emit IR for code generation
              else if soEmitIR
                then do
                  ir <- buildIR soGeneratorInfo pathSegs
                  liftIO $ LBS.putStrLn $ encode ir

                else do
                  -- Mode 5: Normal navigation
                  if null pathSegs
                    then do
                      rootSchema <- navigate []
                      case rootSchema of
                        ViewPlugin schema _ -> liftIO $ do
                          TIO.putStr cliHeader
                          TIO.putStr "\n\n"
                          TIO.putStr $ renderSchema schema
                        _ -> pure ()
                    else do
                      -- Navigate to target
                      view <- navigate pathSegs
                      case view of
                        -- Landed on a plugin: show help
                        ViewPlugin schema _ ->
                          liftIO $ TIO.putStr $ renderSchema schema

                        -- Landed on a method: invoke or show help
                        ViewMethod method path -> do
                          -- Build IR for this method's namespace
                          ir <- buildIR soGeneratorInfo (init path)
                          let fullPath = T.intercalate "." path

                          -- If --help was explicitly requested, show help and exit
                          if helpRequested
                            then do
                              case Map.lookup fullPath (irMethods ir) of
                                Just methodDef ->
                                  liftIO $ TIO.putStr $ renderMethodHelp ir methodDef
                                Nothing ->
                                  liftIO $ TIO.putStrLn $ T.intercalate "." path <> " - " <> methodDescription method
                            else do
                              -- Build params: use IR-driven parsing for inline params
                              let schemaDefaults = extractSchemaDefaults method
                              userParams <- case soParams of
                                -- -p JSON: parse as raw JSON (bypass IR parsing)
                                Just jsonStr ->
                                  case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 jsonStr) of
                                    Left err -> throwParse $ T.pack err
                                    Right p -> pure p
                                Nothing
                                  | not (null inlineParams) ->
                                      -- Use IR-driven parsing for inline params
                                      case Map.lookup fullPath (irMethods ir) of
                                        Just methodDef -> do
                                          -- Inject boolean defaults for flags without values
                                          let paramsWithBools = injectBooleanDefaults ir methodDef inlineParams
                                          -- Inject smart defaults for missing required params (e.g., --path defaults to cwd)
                                          paramsWithDefaults <- liftIO $ injectSmartDefaults transformEnv ir methodDef paramsWithBools
                                          case parseParams ir methodDef paramsWithDefaults of
                                            Right p -> pure p
                                            Left errs -> do
                                              liftIO $ mapM_ (hPutStrLn stderr . renderParseError) errs
                                              throwParse "Parameter parsing failed"
                                        Nothing ->
                                          -- Fallback to flat object if method not in IR
                                          pure $ buildParamsObject inlineParams
                                  | otherwise -> pure $ object []
                              -- Merge: user params override schema defaults
                              let params = mergeParams schemaDefaults userParams

                              if soDryRun
                                then do
                                  backend <- asks seBackend
                                  liftIO $ LBS.putStrLn $ encodeDryRun backend (init path) (last path) params
                                -- Show help when: method has required params AND user provided no params
                                else if hasRequiredParams method && userParams == object [] && null inlineParams
                                  then do
                                    -- Render help from IR
                                    case Map.lookup fullPath (irMethods ir) of
                                      Just methodDef ->
                                        liftIO $ TIO.putStr $ renderMethodHelp ir methodDef
                                      Nothing ->
                                        -- Fallback: method not in IR, use basic info
                                        liftIO $ TIO.putStrLn $ T.intercalate "." path <> " - " <> methodDescription method
                                  else invokeMethod path params
  where
    invokeMethod path params = do
      let namespacePath = init path  -- path without method name
      let methodName' = last path
      -- Set method path hint for template resolution
      let rendererCfg' = withMethodPath rendererCfg path
      -- Determine bidirectional mode
      bidirMode <- liftIO $ case soBidirMode of
        Just modeStr -> case parseBidirMode modeStr of
          Just mode -> pure mode
          Nothing -> do
            TIO.hPutStrLn stderr $ "[synapse] Unknown --bidir-mode: " <> modeStr <> ", using auto-detect"
            detectBidirMode
        Nothing -> detectBidirMode
      -- Use bidirectional streaming handler
      invokeStreamingWithBidir
        namespacePath
        methodName'
        params
        (printResult soJson soRaw rendererCfg')
        (handleBidirRequest bidirMode)

    -- Extract default values from JSON Schema properties
    -- Schema format: {"properties": {"key": {"default": value, ...}, ...}, ...}
    extractSchemaDefaults :: MethodSchema -> Value
    extractSchemaDefaults m = case methodParams m of
      Nothing -> object []
      Just (Object o) -> case KM.lookup "properties" o of
        Just (Object props) -> object
          [ (k, defaultVal)
          | (k, propSchema) <- KM.toList props
          , Object propObj <- [propSchema]
          , Just defaultVal <- [KM.lookup "default" propObj]
          ]
        _ -> object []
      Just _ -> object []

    -- Merge two JSON objects: right takes precedence over left
    mergeParams :: Value -> Value -> Value
    mergeParams (Object defaults) (Object user) =
      Object (KM.union user defaults)  -- union prefers first arg on conflict
    mergeParams _ user = user  -- if defaults aren't an object, just use user params

    -- Check if method has required parameters
    hasRequiredParams :: MethodSchema -> Bool
    hasRequiredParams m = case methodParams m of
      Nothing -> False
      Just (Object o) -> case KM.lookup "required" o of
        Just (Array arr) -> not (null arr)
        _ -> False
      Just _ -> False

    -- Fetch schema for a path, detecting if last segment is a method
    -- Uses navigate to determine what type of schema to fetch
    fetchSchemaForPath :: [Text] -> SynapseM (Either Text Value)
    fetchSchemaForPath segs = do
      view <- navigate segs
      case view of
        ViewPlugin schema _ -> pure $ Right $ toJSON schema
        ViewMethod method path -> do
          -- Use method-specific schema query for detailed method info
          let parentPath = init path
              methodName' = last path
          detailedMethod <- fetchMethodSchema parentPath methodName'
          pure $ Right $ toJSON detailedMethod

    -- Invoke raw JSON-RPC request
    invokeRawRpc :: Value -> SynapseM [HubStreamItem]
    invokeRawRpc rpcReq = do
      case rpcReq of
        Object o -> case (KM.lookup "method" o, KM.lookup "params" o) of
          (Just (String method), Just params) -> invokeRaw method params
          (Just (String method), Nothing) -> invokeRaw method (object [])
          _ -> throwParse "JSON-RPC must have 'method' field"
        _ -> throwParse "JSON-RPC must be an object"

-- | Parse path segments and --key value params
-- Returns (path segments, [(key, value)] params, help requested)
-- Example: ["echo", "once", "--message", "hello", "--count", "3"]
--       -> (["echo", "once"], [("message", "hello"), ("count", "3")], False)
-- Example: ["echo", "once", "--help"]
--       -> (["echo", "once"], [], True)
parsePathAndParams :: [Text] -> ([Text], [(Text, Text)], Bool)
parsePathAndParams = go [] [] False
  where
    go path params helpReq [] = (reverse path, reverse params, helpReq)
    go path params helpReq (x:xs)
      -- --help flag: mark help requested, don't add as param
      | x == "--help" || x == "-h" =
          go path params True xs
      -- --key value pair (value must not start with --)
      | Just key <- T.stripPrefix "--" x
      , not (T.null key)
      , (val:rest) <- xs
      , not (T.isPrefixOf "--" val) =
          let normalizedKey = T.replace "-" "_" key  -- Normalize kebab-case to snake_case
          in go path ((normalizedKey, val) : params) helpReq rest
      -- --key with no value or next arg is another flag (boolean flag)
      | Just key <- T.stripPrefix "--" x
      , not (T.null key) =
          let normalizedKey = T.replace "-" "_" key  -- Normalize kebab-case to snake_case
          in go path ((normalizedKey, "") : params) helpReq xs
      -- Regular path segment - split on dots to support Plexus RPC path syntax (e.g., cone.chat)
      | otherwise =
          let segments = map (T.replace "-" "_") $ filter (not . T.null) $ T.splitOn "." x
          in go (reverse segments ++ path) params helpReq xs

-- | Build JSON object from key-value pairs
buildParamsObject :: [(Text, Text)] -> Value
buildParamsObject pairs = object
  [ (K.fromText k, inferValue v) | (k, v) <- pairs ]
  where
    -- Try to infer the JSON type from the string value
    inferValue :: Text -> Value
    inferValue t
      | t == "true" = Bool True
      | t == "false" = Bool False
      | Just n <- readMaybe (T.unpack t) :: Maybe Integer = Number (fromInteger n)
      | Just n <- readMaybe (T.unpack t) :: Maybe Double = Number (realToFrac n)
      | otherwise = String t

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

-- | ASCII splash logo
splash :: Text
splash = T.unlines
  [ ""
  , "███████╗██╗   ██╗███╗   ██╗ █████╗ ██████╗ ███████╗███████╗"
  , "██╔════╝╚██╗ ██╔╝████╗  ██║██╔══██╗██╔══██╗██╔════╝██╔════╝"
  , "███████╗ ╚████╔╝ ██╔██╗ ██║███████║██████╔╝███████╗█████╗  "
  , "╚════██║  ╚██╔╝  ██║╚██╗██║██╔══██║██╔═══╝ ╚════██║██╔══╝  "
  , "███████║   ██║   ██║ ╚████║██║  ██║██║     ███████║███████╗"
  , "╚══════╝   ╚═╝   ╚═╝  ╚═══╝╚═╝  ╚═╝╚═╝     ╚══════╝╚══════╝"
  , ""
  ]

-- | Get CLI help text from optparse-applicative
cliHeader :: Text
cliHeader = splash <> T.pack (fst $ renderFailure failure "synapse") <> selfHelp
  where
    failure = parserFailure defaultPrefs argsInfo (ShowHelpText Nothing) mempty
    selfHelp = T.unlines
      [ ""
      , "Meta-commands (local, no RPC):"
      , ""
      , "  synapse _self template"
      , "      Manage Mustache templates (CRUD operations)"
      , ""
      , "      Subcommands:"
      , "        list [pattern]      - List existing templates"
      , "        show <method>       - Display template content"
      , "        generate [pattern]  - Generate new templates from IR"
      , "        delete <pattern>    - Delete templates"
      , "        reload              - Clear template cache"
      , ""
      , "      Examples:"
      , "        synapse _self template list"
      , "        synapse _self template show cone.chat"
      , "        synapse _self template generate 'plexus.cone.*'"
      , ""
      ]

-- | Render an error for display
renderError :: SynapseError -> String
renderError = \case
  NavError (NotFound seg path maybeSchema) ->
    let baseMsg = "Command not found: '" <> T.unpack seg <> "' at " <> showPath path
    in case maybeSchema of
      Nothing -> baseMsg
      Just schema ->
        let methods = psMethods schema
            children = pluginChildren schema
            suggestions = if null methods && null children
                         then ""
                         else "\n\nAvailable commands:"
                              <> renderMethods methods
                              <> renderChildren children
        in baseMsg <> suggestions
  NavError (MethodNotTerminal seg path) ->
    "Method '" <> T.unpack seg <> "' cannot have subcommands at " <> showPath path
  NavError (Cycle hash path) ->
    "Cycle detected: hash " <> T.unpack hash <> " at " <> showPath path
  NavError (FetchError msg path) ->
    "Fetch error at " <> showPath path <> ": " <> T.unpack msg
  TransportError msg ->
    "Transport error: " <> T.unpack msg
  TransportErrorContext ctx ->
    renderTransportError ctx
  ParseError msg ->
    "Parse error: " <> T.unpack msg
  ValidationError msg ->
    "Validation error: " <> T.unpack msg
  BackendError errorType backends ->
    case errorType of
      BackendNotFound name ->
        "Backend not found: '" <> T.unpack name <> "'"
        <> renderBackendList backends
      BackendUnreachable name ->
        "Backend unreachable: '" <> T.unpack name <> "'"
        <> renderBackendList backends
      NoBackendsAvailable ->
        "No backends available"
        <> renderBackendList backends
  where
    showPath [] = "root"
    showPath p = T.unpack $ T.intercalate "." p

    renderMethods [] = ""
    renderMethods methods =
      let header = "\n\n  Methods:"
          methodLines = map renderMethod methods
      in header <> concat methodLines

    renderMethod method =
      let name = T.unpack $ methodName method
          desc = T.unpack $ methodDescription method
          padding = 20
          namePadded = name <> replicate (max 1 (padding - length name)) ' '
      in "\n    " <> namePadded <> desc

    renderChildren [] = ""
    renderChildren children =
      let header = "\n\n  Child plugins:"
          childLines = map renderChild children
      in header <> concat childLines

    renderChild child =
      let name = T.unpack $ csNamespace child
          desc = T.unpack $ csDescription child
          padding = 20
          namePadded = name <> replicate (max 1 (padding - length name)) ' '
      in "\n    " <> namePadded <> desc

    renderBackendList [] = "\n\nNo backends available."
    renderBackendList backends =
      let header = "\n\nAvailable backends:"
          backendLines = map renderBackendLine backends
          usageHint = "\n\nUsage: synapse <backend> [command...]"
      in header <> concat backendLines <> usageHint

    renderBackendLine backend =
      let name = T.unpack $ backendName backend
          nameField = name <> replicate (max 1 (15 - length name)) ' '
          hostPort = T.unpack $ backendHost backend <> ":" <> T.pack (show (backendPort backend))
          status = case backendReachable backend of
                     Just True  -> " [OK]"
                     Just False -> " [UNREACHABLE]"
                     Nothing    -> ""
          desc = if T.null (backendDescription backend)
                 then ""
                 else " - " <> T.unpack (backendDescription backend)
      in "\n  " <> nameField <> hostPort <> status <> desc

-- | Render transport error with context
renderTransportError :: TransportContext -> String
renderTransportError TransportContext{..} = case tcCategory of
  ConnectionRefused ->
    "Connection refused\n\n" <>
    "Backend: " <> T.unpack tcBackend <> "\n" <>
    "Address: " <> T.unpack tcHost <> ":" <> show tcPort <> "\n" <>
    "Path: " <> showPath tcPath <> "\n\n" <>
    "Troubleshooting:\n" <>
    "  - Check if the backend is running\n" <>
    "  - Verify host (-H) and port (-P) settings\n" <>
    "  - Run 'synapse' (no args) to list backends"

  ConnectionTimeout ->
    "Connection timeout\n\n" <>
    "Backend: " <> T.unpack tcBackend <> " @ " <>
    T.unpack tcHost <> ":" <> show tcPort <> "\n\n" <>
    "The server may be overloaded or network latency is high\n" <>
    "Error: " <> T.unpack tcMessage

  ProtocolError ->
    "Protocol error\n\n" <>
    "Backend: " <> T.unpack tcBackend <> "\n" <>
    "This may indicate a version mismatch\n\n" <>
    "Error: " <> T.unpack tcMessage

  UnknownTransportError ->
    "Transport error: " <> T.unpack tcMessage <> "\n\n" <>
    "Backend: " <> T.unpack tcBackend <> " @ " <>
    T.unpack tcHost <> ":" <> show tcPort
  where
    showPath [] = "root"
    showPath p = T.unpack $ T.intercalate "." p

-- | Render a parse error for display
renderParseError :: Parse.ParseError -> String
renderParseError = \case
  Parse.UnknownParam name suggestions ->
    "Unknown parameter: --" <> T.unpack name <>
    case suggestions of
      [] -> ""
      [s] -> "\n\nDid you mean: --" <> T.unpack s <> "?"
      ss -> "\n\nDid you mean one of:\n" <>
            unlines ["  --" <> T.unpack s | s <- ss]
  Parse.MissingRequired name ->
    "Missing required parameter: --" <> T.unpack name
  Parse.InvalidValue name reason ->
    "Invalid value for --" <> T.unpack name <> ": " <> T.unpack reason
  Parse.AmbiguousVariant name variants ->
    "Ambiguous variant for --" <> T.unpack name <> ": could be one of " <> show (map T.unpack variants)
  Parse.MissingDiscriminator param field ->
    "Missing discriminator for --" <> T.unpack param <> ": need --" <> T.unpack param <> "." <> T.unpack field
  Parse.UnknownVariant param value valid suggestions ->
    "Unknown variant '" <> T.unpack value <> "' for --" <> T.unpack param <>
    "\n\nValid variants: " <> T.unpack (T.intercalate ", " valid) <>
    case suggestions of
      [] -> ""
      [s] -> "\n\nDid you mean: " <> T.unpack s <> "?"
      ss -> "\n\nDid you mean one of: " <> T.unpack (T.intercalate ", " ss) <> "?"
  Parse.TypeNotFound name ->
    "Type not found in IR: " <> T.unpack name


-- | Write a generated template to disk (no logging)
writeGeneratedTemplateIR :: FilePath -> TemplateIR.GeneratedTemplate -> IO ()
writeGeneratedTemplateIR baseDir gt = do
  let fullPath = baseDir </> TemplateIR.gtPath gt
  createDirectoryIfMissing True (baseDir </> T.unpack (TemplateIR.gtNamespace gt))
  TIO.writeFile fullPath (TemplateIR.gtTemplate gt)

-- | Encode a dry-run request
encodeDryRun :: Text -> [Text] -> Text -> Value -> LBS.ByteString
encodeDryRun backend namespacePath method params =
  let fullPath = if null namespacePath then [backend] else namespacePath
      dotPath = T.intercalate "." (fullPath ++ [method])
  in encode $ object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id" .= (1 :: Int)
    , "method" .= (backend <> ".call")
    , "params" .= object
        [ "method" .= dotPath
        , "params" .= params
        ]
    ]

-- | Print a stream result
-- soJson: output raw JSON stream items
-- soRaw: skip template rendering, just output content JSON
-- otherwise: try template rendering, fall back to content JSON
printResult :: Bool -> Bool -> RendererConfig -> HubStreamItem -> IO ()
printResult True _ _ item = LBS.putStrLn $ encode item
printResult _ True _ item = case item of
  -- Raw mode: just output the content
  HubData _ _ _ dat -> do
    LBS.putStrLn $ encode dat
    hFlush stdout
  HubProgress _ _ msg _ -> do
    TIO.putStr msg
    TIO.putStr "\r"
    hFlush stdout
  HubError _ _ err _ ->
    hPutStrLn stderr $ "Error: " <> T.unpack err
  _ -> pure ()
printResult _ _ cfg item = do
  -- Template mode: try to render with template
  mRendered <- renderItem cfg item
  case mRendered of
    Just text -> do
      TIO.putStrLn text
      hFlush stdout
    Nothing -> case item of
      -- Fallback to pretty-printed content
      HubData _ _ _ dat -> do
        TIO.putStrLn $ prettyValue dat
        hFlush stdout
      HubProgress _ _ msg _ -> do
        TIO.putStr msg
        TIO.putStr "\r"
        hFlush stdout
      HubError _ _ err _ ->
        hPutStrLn stderr $ "Error: " <> T.unpack err
      _ -> pure ()

-- ============================================================================
-- Argument Parsing (Synapse Options Only)
-- ============================================================================

-- | Parser for synapse-level options only
-- Backend and path are handled separately after arg splitting
argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> helper)
  ( fullDesc
 <> header "synapse - Algebraic CLI for Hub"
 <> progDesc "synapse [OPTIONS] <backend> <path...> [--param value ...]"
 <> noIntersperse  -- Stop option parsing at first positional arg
  )

argsParser :: Parser Args
argsParser = Args <$> optsParser <*> backendParser <*> restParser
  where
    backendParser = optional $ T.pack <$> argument str
      ( metavar "BACKEND"
     <> help "Backend name (e.g., plexus, registry-hub)" )

    restParser = many $ T.pack <$> argument str
      ( metavar "PATH... [--param value ...]"
     <> help "Path and method parameters (passed through)" )

optsParser :: Parser SynapseOpts
optsParser = do
  soHost <- T.pack <$> strOption
    ( long "host" <> short 'H' <> metavar "HOST"
   <> value (T.unpack defaultHost)
   <> help "Registry/discovery host (default: 127.0.0.1)" )
  soPort <- option auto
    ( long "port" <> short 'P' <> metavar "PORT"
   <> value defaultPort
   <> help "Registry/discovery port (default: 4444)" )
  soJson <- switch
    ( long "json" <> short 'j'
   <> help "Output raw JSON stream items" )
  soRaw <- switch
    ( long "raw"
   <> help "Output raw content JSON (skip templates)" )
  soDryRun <- switch
    ( long "dry-run" <> short 'n'
   <> help "Show JSON-RPC request without sending" )
  soSchema <- switch
    ( long "schema" <> short 's'
   <> help "Fetch raw schema JSON for path" )
  soGenerate <- switch
    ( long "generate-templates" <> short 'g'
   <> help "Generate mustache templates from IR" )
  soEmitIR <- switch
    ( long "emit-ir" <> short 'i'
   <> help "Emit IR for code generation (JSON)" )
  soParams <- optional $ T.pack <$> strOption
    ( long "params" <> short 'p' <> metavar "JSON"
   <> help "Method parameters as JSON object" )
  soRpc <- optional $ T.pack <$> strOption
    ( long "rpc" <> short 'r' <> metavar "JSON"
   <> help "Raw JSON-RPC request (bypass navigation)" )
  soGeneratorInfo <- many $ T.pack <$> strOption
    ( long "generator-info" <> metavar "TOOL:VERSION"
   <> help "Generator tool version info (can be specified multiple times)" )
  soBidirMode <- optional $ T.pack <$> strOption
    ( long "bidir-mode" <> short 'b' <> metavar "MODE"
   <> help "Bidirectional mode: interactive (TTY), json (pipe protocol), auto-cancel, defaults" )
  pure SynapseOpts{..}
