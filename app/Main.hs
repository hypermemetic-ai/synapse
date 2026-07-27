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

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List (stripPrefix)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Options.Applicative
import System.Environment (getEnvironment)
import Data.Version (showVersion)
import qualified Paths_plexus_synapse as Meta
import System.Directory (getHomeDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, hFlush, stdout)


import Synapse.Schema.Types
import Synapse.Monad (SynapseM, SynapseEnv(..), SynapseError(..), BackendErrorType(..), TransportContext(..), TransportErrorCategory(..), initEnv, runSynapseM, throwNav, throwTransport, throwParse, throwBackend, withRequestContext)
import qualified Synapse.Self as Self
  ( CredentialRef(..)
  , RefreshOutcome(..)
  , ResolveError(..)
  , ResolvedDefaults(..)
  , defaultRegistry
  , ensureFreshLogin
  , loadDefaults
  , merge
  , resolveAll
  )
import qualified Control.Exception as E
import qualified Synapse.Log as Log
import qualified Katip
import Synapse.Algebra.Navigate
import qualified Synapse.Connectome.Nav as CNav
import Synapse.Wire (readWireLog, renderWireLog)
import Synapse.Monad (getWireLog)
import qualified Plexus.Connectome.Types as C
import Synapse.Algebra.Render (renderSchema)
import Synapse.CLI.Help (renderMethodHelp)
import qualified Synapse.DynamicChildren as DC
import Synapse.Transport (getConfig)
import Synapse.CLI.Parse (parseParams)
import qualified Synapse.CLI.Parse as Parse
import Synapse.IR.Types (IR, irMethods, MethodDef)
import qualified Data.Map.Strict as Map
import qualified Synapse.CLI.Template as TemplateIR
import Synapse.Deprecation (emitActivationWarning, emitMethodWarning)
import Synapse.Transport
import Synapse.Bidir (BidirMode(..), parseBidirMode, detectBidirMode, handleBidirRequest)
import Plexus.Types (Response(..))
import Synapse.CLI.Transform (mkTransformEnv, transformParams, defaultTransformers, injectBooleanDefaults, injectSmartDefaults)
import Synapse.IR.Builder (buildIR)
import Synapse.Renderer (RendererConfig, defaultRendererConfig, renderItem, prettyValue, withMethodPath)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import qualified Synapse.Self.Commands as Self
import qualified Synapse.Self.Command as SelfCmd
import Synapse.Backend.Discovery
  ( Backend(..)
  , BackendDiscovery(..)
  , registryDiscovery
  , directDiscovery
  , registryEndpointFromEnv
  , registryUrlEnvVar
  , deactivateBackend
  , pingBackends
  , getBackendAt
  )
import System.Exit (ExitCode(..), exitWith)

-- ============================================================================
-- Types
-- ============================================================================

-- | Synapse-level options (must appear before backend)
-- Controls connection settings and output format
data SynapseOpts = SynapseOpts
  { soHost          :: Text          -- ^ Connect-target host (-H)
  , soPort          :: Int           -- ^ Connect-target port (-P)
  , soExplicitConn  :: Bool          -- ^ Z2H-6: -H/-P given explicitly → direct connect, registry bypassed
  , soJson          :: Bool          -- ^ Output raw JSON stream items
  , soRaw           :: Bool          -- ^ Output raw content (no templates)
  , soDryRun        :: Bool          -- ^ Show request without sending
  , soSchema        :: Bool          -- ^ Fetch raw schema JSON
  , soConnectome    :: Bool          -- ^ PLX-121: navigate over the Connectome document
  , soGenerate      :: Bool          -- ^ Generate templates from IR
  , soEmitIR        :: Bool          -- ^ Emit IR for code generation
  , soParams        :: Maybe Text    -- ^ JSON params via -p
  , soRpc           :: Maybe Text    -- ^ Raw JSON-RPC passthrough
  , soGeneratorInfo :: [Text]        -- ^ Generator tool info (tool:version pairs)
  , soBidirMode     :: Maybe Text    -- ^ Bidirectional mode override
  , soBidirCmd      :: Maybe Text    -- ^ Bidirectional subprocess command (--bidir-cmd)
  , soNoCache       :: Bool          -- ^ Disable IR caching
  , soLogLevel      :: Maybe Text    -- ^ Log level: info, debug, trace
  , soLogSubsystems :: [Text]        -- ^ Filter logs by subsystems (empty = all)
  , soToken         :: Maybe Text    -- ^ JWT sent as Cookie: access_token=<jwt>
  , soTokenFile     :: Maybe Text    -- ^ Path to token file (overridden by --token)
  , soCookies       :: [(Text, Text)]  -- ^ SAFE-S04/REQ-5: extra --cookie KEY=VALUE entries
  , soHeaders       :: [(Text, Text)]  -- ^ SAFE-S04/REQ-5: extra --header KEY=VALUE entries
  , soNoDeprecationWarnings :: Bool  -- ^ Suppress invocation-time deprecation warnings (IR-15)
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
-- Logging Helper
-- ============================================================================

-- | Create a logger from command-line options
-- Default: ErrorS (only show errors)
makeLoggerFromOpts :: SynapseOpts -> IO Log.Logger
makeLoggerFromOpts opts = do
  let level = case soLogLevel opts of
        Nothing -> Katip.ErrorS  -- Default: only errors
        Just levelStr -> parseLogLevel levelStr
  Log.makeLogger level
  where
    parseLogLevel "error" = Katip.ErrorS
    parseLogLevel "warn" = Katip.WarningS
    parseLogLevel "info" = Katip.InfoS
    parseLogLevel "debug" = Katip.DebugS
    parseLogLevel _ = Katip.ErrorS  -- Default to error

-- ============================================================================
-- Token Resolution
-- ============================================================================

-- | Build the request-context (extra cookies/headers) for the WS upgrade,
-- combining --cookie/--header flags with SYNAPSE_COOKIE_*/SYNAPSE_HEADER_*
-- env vars. Env keys are lowercased.
collectRequestContext :: SynapseOpts -> IO ([(Text, Text)], [(Text, Text)])
collectRequestContext opts = do
  envVars <- getEnvironment
  let envCookies = [ (T.toLower (T.pack k), T.pack v)
                   | (k0, v) <- envVars
                   , Just k <- [stripPrefix "SYNAPSE_COOKIE_" k0]
                   ]
      envHeaders = [ (T.toLower (T.pack k), T.pack v)
                   | (k0, v) <- envVars
                   , Just k <- [stripPrefix "SYNAPSE_HEADER_" k0]
                   ]
  pure (soCookies opts ++ envCookies, soHeaders opts ++ envHeaders)

-- | Resolve the auth token to use for this invocation from CLI-only
-- sources.
--
-- Priority:
--   1. --token <jwt>           (explicit, highest priority)
--   2. --token-file <path>     (explicit file)
--
-- Any persistent per-backend default (including the former
-- @~\/.plexus\/tokens\/\<backend\>@, now auto-migrated by
-- 'Synapse.Self.loadDefaults' on first read) lives in
-- @~\/.plexus\/\<backend\>\/defaults.json@ as a
-- @cookies.access_token@ ref and is loaded through the SELF-2 pipeline
-- in 'buildEnv'. This function therefore only consults
-- invocation-scoped flags; returning 'Nothing' means "no CLI override",
-- not "no token available".
--
-- Token files contain just the raw JWT, optionally with a trailing
-- newline.
resolveToken :: SynapseOpts -> Text -> IO (Maybe Text)
resolveToken opts _backend =
  case soToken opts of
    Just tok -> pure (Just tok)
    Nothing  -> case soTokenFile opts of
      Nothing   -> pure Nothing
      Just path -> do
        contents <- TIO.readFile (T.unpack path)
        let tok = T.strip contents
        pure $ if T.null tok then Nothing else Just tok

-- ============================================================================
-- Env Construction (SELF-2 read path)
-- ============================================================================

-- | Render a 'Self.ResolveError' as a human-readable message naming the
-- failing URI and the underlying reason.
renderResolveError :: Self.ResolveError -> String
renderResolveError = \case
  Self.ResolveUnknownScheme scheme ->
    "unknown credential scheme: " <> T.unpack scheme
    <> " (no resolver registered; known schemes: literal, env, file)"
  Self.ResolveNotFound (Self.CredentialRef uri) ->
    "credential not found: " <> T.unpack uri
  Self.ResolveBackendError (Self.CredentialRef uri) msg ->
    "credential backend error for " <> T.unpack uri <> ": " <> T.unpack msg
  Self.ResolveParseError msg ->
    "malformed credential reference: " <> T.unpack msg

-- | Build a fully-wired 'SynapseEnv' for a backend invocation.
--
-- SELF-2 read path:
--
-- 1. CLI token resolution (--token / --token-file; persistent defaults
--    live in ~/.plexus/<b>/defaults.json and are loaded in step 3)
-- 2. Collect CLI --cookie / --header flags and SYNAPSE_COOKIE_*/HEADER_* env
-- 3. Load stored defaults from @~\/.plexus\/\<backend\>\/defaults.json@
-- 4. Resolve every 'CredentialRef' through the default registry
--    (literal / env / file)
-- 5. Wrap the legacy token (if any) as a CLI @access_token@ cookie so it
--    flows through the same 'Self.merge' and wins over any stored default
--    — preserving the pre-SELF-2 precedence of the --token flag.
-- 6. Merge CLI overrides onto resolved stored defaults (CLI wins per key).
--
-- On resolve failure: print a clear message and 'exitFailure'. We never
-- build a half-authenticated 'SynapseEnv'.
buildEnv :: Text -> Int -> Text -> Log.Logger -> SynapseOpts -> IO SynapseEnv
buildEnv host port backend logger opts = do
  -- Legacy token path (unchanged semantically; SELF-3 consolidates).
  mToken <- resolveToken opts backend
  (cliCookieList, cliHeaderList) <- collectRequestContext opts

  -- SELF-2: load + resolve + merge.
  stored0 <- E.catch (Self.loadDefaults backend) $ \e -> do
    hPutStrLn stderr $ "Error: " <> show (e :: E.IOException)
    exitFailure

  -- UT-3 transparent refresh: when no invocation-scoped token override
  -- exists, an expired/missing stored access token with a stored refresh
  -- path is renewed in place before this call dispatches. CLI-provided
  -- tokens (--token / --token-file / --cookie access_token=...) skip
  -- this entirely — explicit material is never second-guessed.
  let cliProvidesToken =
        mToken /= Nothing || any ((== "access_token") . fst) cliCookieList
  stored <-
    if cliProvidesToken
      then pure stored0
      else do
        (stored', outcome) <- Self.ensureFreshLogin backend stored0
        case outcome of
          Self.Refreshed ->
            Log.logInfo logger Log.SubsystemCLI $
              "access token for `" <> backend
              <> "` was expired; refreshed via stored refresh token"
          Self.RefreshFailed err ->
            -- Loud but non-fatal: the call proceeds with the stale token
            -- and the backend's auth error names the real failure; this
            -- warning explains *why* it is about to happen.
            hPutStrLn stderr $ "Warning: stored access token for '"
              <> T.unpack backend <> "' is expired and auto-refresh failed: "
              <> T.unpack err
              <> "\n  Run: synapse _self " <> T.unpack backend <> " login"
          _ -> pure ()
        pure stored'

  resolved <- Self.resolveAll Self.defaultRegistry stored []
  resolvedDefaults <- case resolved of
    Left err -> do
      hPutStrLn stderr $ "Error resolving defaults for backend '"
        <> T.unpack backend <> "': " <> renderResolveError err
      exitFailure
    Right rd -> pure rd

  -- Fold CLI flags (and the legacy --token / SYNAPSE_TOKEN / token file)
  -- into the CLI-side maps so the unified 'Self.merge' produces the final
  -- cookie + header set.
  let cliCookiesBase = Map.fromList cliCookieList
      cliCookies = case mToken of
        Just tok -> Map.insert "access_token" tok cliCookiesBase
        Nothing  -> cliCookiesBase
      cliHeaders = Map.fromList cliHeaderList
      (mergedCookies, mergedHeaders) =
        Self.merge resolvedDefaults cliCookies cliHeaders

  -- 'seToken = Nothing' here: any access_token cookie already sits in the
  -- merged cookie map. Transport's mergeUpgradeHeaders joins them into a
  -- single Cookie header exactly as before.
  env0 <- initEnv host port backend logger Nothing
  pure $ withRequestContext (Map.toList mergedCookies)
                             (Map.toList mergedHeaders)
                             env0

-- ============================================================================
-- Argument Splitting
-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  args <- execParser argsInfo
  let opts = argOpts args

  -- Initialize logger (default: ErrorS, only show errors)
  logger <- makeLoggerFromOpts opts

  Log.logDebug logger Log.SubsystemCLI "synapse starting..."

  -- Z2H-6 resolution split (Z2H-S3 §3):
  --   explicit -H/-P  → direct connect target, registry bypassed entirely
  --   bare name       → resolve through the registry endpoint
  --                     (PLEXUS_REGISTRY_URL, default ws://127.0.0.1:4444)
  (regHost, regPort) <- registryEndpointFromEnv
  let explicit = soExplicitConn opts
      (probeHost, probePort) =
        if explicit then (soHost opts, soPort opts) else (regHost, regPort)
      discovery =
        if explicit
          then directDiscovery probeHost probePort
          else registryDiscovery regHost regPort

  -- Identify whatever backend answers at the probe point (the explicit
  -- target, or the registry endpoint). Used as the fallback backend for
  -- backend-less invocations and for the _self legacy path.
  Log.logDebug logger Log.SubsystemDiscovery $
    "Checking backend at " <> probeHost <> ":" <> T.pack (show probePort)

  maybeBackend <- getBackendAt probeHost probePort

  Log.logDebug logger Log.SubsystemDiscovery $
    "Backend check complete: " <> T.pack (show maybeBackend)
  let hostBackend = maybe "plexus" id maybeBackend

  -- NOTE: the pre-Z2H-6 fire-and-forget client-side auto-registration
  -- that lived here is gone. Backends now register THEMSELVES at boot
  -- (plexus-transport, Z2H-S3 protocol A); the CLI only reads.

  let mRegistry = if explicit then Nothing else Just (regHost, regPort)
      runDisc   = runWithDiscovery mRegistry (probeHost, probePort) discovery

  case argBackend args of
    Nothing
      -- Suppress banner for data output modes
      | soEmitIR opts || soSchema opts || soJson opts || soRaw opts || soConnectome opts ->
          runDisc hostBackend args
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
      -- Handle _self meta-commands
      | backend == "_self" ->
          -- SELF-4 verbs (synapse _self <backend> <verb>) dispatch through
          -- a dedicated parser on the raw argPath. Legacy _self
          -- subcommands (template / scan / debug / validate / test) keep
          -- the original path through runWithDiscovery — EXCEPT the
          -- purely-local ones (template / scan / help), which are
          -- short-circuited straight to the local dispatcher BEFORE any
          -- backend discovery (defect C2). Resolving a "plexus" backend
          -- over the network for a local-only command was both wrong and
          -- a needless failure point.
          if isSelfV4Invocation (argPath args)
            then runSelfV4 (argPath args)
            else if isLocalOnlySelfCommand (argPath args)
              then runSelfLegacyLocal probeHost probePort hostBackend opts args
              else runDisc hostBackend args
      | otherwise -> runDisc backend args

-- | Legacy @_self@ subcommands that predate SELF-4. These keep their
-- original dispatch path through 'Synapse.Self.Commands.dispatch'.
--
-- Anything else under @_self@ is treated as SELF-4 (@_self \<backend\>
-- ...@) and routes through 'Synapse.Self.Command.runSelfCommand'.
legacySelfSubcommands :: [Text]
legacySelfSubcommands =
  [ "template", "scan", "debug", "validate", "test"
  , "--help", "-h"  -- Legacy help path is also handled by the old dispatcher
  ]

-- | True when @_self@ was invoked in the SELF-4 shape — anything other
-- than a legacy subcommand as the first positional. The SELF-4 parser
-- handles its own help / error messaging from there, so even
-- @_self \<backend\>@ (no verb) routes here: optparse emits a
-- "Missing: COMMAND" message listing the SELF-4 verbs.
isSelfV4Invocation :: [Text] -> Bool
isSelfV4Invocation []          = False  -- No args at all: legacy help.
isSelfV4Invocation (first : _) = first `notElem` legacySelfSubcommands

-- | Legacy @_self@ subcommands that are purely local — they never touch
-- a backend (defect C2). @template@ is CRUD over local files; @scan@
-- probes ports the user names; help just prints text. These must NOT go
-- through 'runWithDiscovery' (which would resolve a "plexus" backend
-- over the network first). @debug@ / @validate@ / @test@ are NOT here:
-- they intentionally contact a host/port.
localOnlySelfSubcommands :: [Text]
localOnlySelfSubcommands = ["template", "scan", "--help", "-h"]

-- | True when the legacy @_self@ invocation is a purely-local command
-- (see 'localOnlySelfSubcommands'). An empty arg list is the legacy help
-- path, which is also local.
isLocalOnlySelfCommand :: [Text] -> Bool
isLocalOnlySelfCommand []          = True  -- bare `_self`: local help
isLocalOnlySelfCommand (first : _) = first `elem` localOnlySelfSubcommands

-- | Run a purely-local legacy @_self@ command without contacting any
-- backend (defect C2). Builds an env pointed at the probe endpoint (so
-- @scan@'s @asks seHost@ still works) and dispatches directly, skipping
-- 'runWithDiscovery' entirely.
runSelfLegacyLocal :: Text -> Int -> Text -> SynapseOpts -> Args -> IO ()
runSelfLegacyLocal host port backend opts args = do
  logger <- makeLoggerFromOpts opts
  env <- buildEnv host port backend logger opts
  rendererCfg <- defaultRendererConfig
  result <- runSynapseM env (dispatch args rendererCfg)
  case result of
    Left err -> do
      hPutStrLn stderr $ renderError err
      exitFailure
    Right () -> exitSuccess

-- | Re-parse the post-@_self@ token stream through the SELF-4
-- optparse-applicative parser, then run the resulting command.
runSelfV4 :: [Text] -> IO ()
runSelfV4 rest = do
  let argv = map T.unpack rest
      parserInfo = SelfCmd.selfCommandInfo
      pr = execParserPure defaultPrefs parserInfo argv
  cmd <- handleParseResult pr
  code <- SelfCmd.runSelfCommand cmd
  case code of
    ExitSuccess   -> pure ()
    ExitFailure _ -> exitWith code

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

-- | Run a Hub command with backend discovery.
--
-- @mRegistry@ is @Just (regHost, regPort)@ when resolving through the
-- registry (bare-name invocation) and @Nothing@ on the explicit @-H/-P@
-- direct path. @(probeHost, probePort)@ is the point we probed: the
-- explicit target, or the registry endpoint.
runWithDiscovery :: Maybe (Text, Int) -> (Text, Int) -> BackendDiscovery -> Text -> Args -> IO ()
runWithDiscovery mRegistry (probeHost, probePort) discovery backendName args = do
  let opts = argOpts args
  -- Try to discover backend info
  maybeBackend <- getBackendInfo discovery backendName

  case maybeBackend of
    Just backend -> do
      -- Verify the resolved backend actually identifies as the requested name.
      -- This catches cases where -P points to a different backend than expected,
      -- or the registry has stale entries.
      actualName <- getBackendAt (backendHost backend) (backendPort backend)
      case actualName of
        Just name | name /= backendName -> do
          -- Z2H-6 lazy eviction: a registry entry that resolves to an
          -- endpoint now serving a DIFFERENT backend is stale — flip it
          -- inactive so the next resolve fails fast.
          evictionNote <- evictStaleEntry
          hPutStrLn stderr $ "Backend mismatch: requested '" <> T.unpack backendName
            <> "' but " <> T.unpack (backendHost backend) <> ":" <> show (backendPort backend)
            <> " identifies as '" <> T.unpack name <> "'"
            <> evictionNote
          exitFailure
        Nothing -> case mRegistry of
          Just _ -> do
            -- Z2H-6 lazy eviction (the SIGKILL/crash half of
            -- stop-invalidation): the registry resolved a backend whose
            -- endpoint no longer answers _info. Deactivate the entry and
            -- error actionably.
            evictionNote <- evictStaleEntry
            hPutStrLn stderr $ "Backend '" <> T.unpack backendName
              <> "' is registered at " <> T.unpack (backendHost backend) <> ":"
              <> show (backendPort backend) <> " but unreachable (did it crash?)."
              <> evictionNote
              <> "\n\nTo fix: start the backend — it re-registers itself at startup."
            exitFailure
          Nothing -> do
            -- Direct path: protocol handshake failed when verifying backend
            logger <- makeLoggerFromOpts opts
            env <- buildEnv probeHost probePort backendName logger opts
            result <- runSynapseM env (throwBackend (ProtocolHandshakeFailed (backendHost backend) (backendPort backend)) [])
            case result of
              Left err -> do
                hPutStrLn stderr $ renderError err
                exitFailure
              Right () -> exitSuccess
        Just _ ->
          -- Match confirmed
          run backendName (backendHost backend) (backendPort backend) args
    Nothing -> do
      -- Backend not resolved (not at the probe point; in registry mode,
      -- also not registered). Distinguish handshake failure from a true
      -- not-registered miss.
      maybeDirectBackend <- getBackendAt probeHost probePort
      logger <- makeLoggerFromOpts opts
      env <- buildEnv probeHost probePort backendName logger opts
      case maybeDirectBackend of
        Nothing | mRegistry == Nothing -> do
          -- Protocol handshake failed at the explicit host:port
          result <- runSynapseM env (throwBackend (ProtocolHandshakeFailed probeHost probePort) [])
          case result of
            Left err -> do
              hPutStrLn stderr $ renderError err
              exitFailure
            Right () -> exitSuccess
        Just discoveredName | discoveredName == backendName ->
          -- Backend discovered successfully - should not reach here, but proceed
          run backendName probeHost probePort args
        _ -> do
          -- Z2H-6: not registered (or, in registry mode, registry down) —
          -- actionable error naming the registration step (rendered by
          -- the BackendNotFound case of renderError).
          backends <- discoverBackends discovery
          backendsWithStatus <- pingBackends backends
          result <- runSynapseM env (throwBackend (BackendNotFound backendName) backendsWithStatus)
          case result of
            Left err -> do
              hPutStrLn stderr $ renderError err
              exitFailure
            Right () -> exitSuccess
  where
    -- Deactivate the requested name's registry entry (registry mode only).
    -- Returns a note for the error message; never fails the invocation.
    evictStaleEntry :: IO String
    evictStaleEntry = case mRegistry of
      Nothing -> pure ""
      Just (regHost, regPort) -> do
        evicted <- deactivateBackend regHost regPort backendName
        pure $ if evicted
          then "\nThe stale registry entry for '" <> T.unpack backendName
            <> "' has been deactivated."
          else ""

-- | Run a Hub command with specified backend and connection details
run :: Text -> Text -> Int -> Args -> IO ()
run backend host port args = do
  let opts = argOpts args
  logger <- makeLoggerFromOpts opts
  Log.logInfo logger Log.SubsystemCLI $ "Synapse starting: backend=" <> backend <> ", host=" <> host <> ", port=" <> T.pack (show port)
  env <- buildEnv host port backend logger opts
  rendererCfg <- defaultRendererConfig
  Log.logDebug logger Log.SubsystemCLI "Running dispatch..."
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
      -- _self meta-command: parse subcommand and rest from argPath.
      -- NOTE: legacy _self positionals are NOT dot-split/dash-replaced
      -- (defect C1). `_self debug 127.0.0.1 4444 foo` must keep the host
      -- as a single token "127.0.0.1" rather than ["127","0","0","1"].
      let (pathSegs, rawParams, _) = parseRawPathAndParams argPath
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

          -- Mode 1.5: Respond subcommand (agent sends a response to a pending bidir request)
          if not (null pathSegs) && head pathSegs == "respond"
            then handleRespondCommand rawParams

          -- Mode 1.75 (PLX-121): navigate over the ONE Connectome document.
          -- With a path: navigate to it. With no path: walk the whole tree.
          -- Either way the wire log is dumped to stderr, because "the round
          -- trip class is gone" is a claim about counted round trips.
          else if soConnectome
            then runConnectomeMode pathSegs

          -- Mode 2: Schema request
          else if soSchema
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
                        ViewPlugin schema visited -> do
                          -- GAP c8b4555d: `synapse <backend>` (no path) lists the
                          -- backend's activations — the natural discovery gesture —
                          -- rather than dumping the global usage banner. Mirrors the
                          -- non-empty-path ViewPlugin branch below.
                          liftIO $ TIO.putStr $ renderSchema schema
                          -- SYN-2: live-enumerate dynamic children for the rendered hub
                          renderDynamicChildrenBlock visited schema
                        _ -> pure ()
                    else do
                      -- Navigate to target
                      view <- navigate pathSegs
                      case view of
                        -- Landed on a plugin: show help
                        ViewPlugin schema visited -> do
                          liftIO $ TIO.putStr $ renderSchema schema
                          -- SYN-2: live-enumerate dynamic children for the rendered hub
                          renderDynamicChildrenBlock visited schema

                        -- Landed on a method: invoke or show help
                        ViewMethod method parentPlugin path -> do
                          let fullPath = T.intercalate "." path

                          -- IR-15: emit stderr deprecation warnings right before any
                          -- real invocation. Dedupe is per-process-lifetime, so this
                          -- action is idempotent on repeat calls. We bind it as a
                          -- local SynapseM action and invoke it at both invocation
                          -- sites below (fast path and slow path). --help, --dry-run,
                          -- --schema, and --emit-ir do not invoke, and intentionally
                          -- do not fire these warnings.
                          let fireDeprecationWarnings :: SynapseM ()
                              fireDeprecationWarnings = do
                                liftIO $ emitMethodWarning
                                  soNoDeprecationWarnings
                                  fullPath
                                  (methodDeprecation method)
                                let namespacePath = init path
                                when (not (null namespacePath)) $ do
                                  let activationNs = T.intercalate "." namespacePath
                                  -- PLX-143: `parentPlugin` comes from the
                                  -- ViewMethod itself. It is the schema at
                                  -- `namespacePath` that navigation already
                                  -- fetched and found this method in, so the
                                  -- `fetchSchemaAt namespacePath` that used to
                                  -- stand here was a second round trip for a
                                  -- value the caller was already holding.
                                  liftIO $ emitActivationWarning
                                    soNoDeprecationWarnings
                                    activationNs
                                    (psDeprecation parentPlugin)

                          -- Optimization: Skip IR construction for parameter-less methods
                          -- IR is only needed for:
                          -- 1. Help rendering (--help flag)
                          -- 2. Parameter parsing (methods with params)
                          let needsIR = helpRequested
                                     || isJust (methodParams method)
                                     || isJust soParams
                                     || not (null inlineParams)

                          -- Fast path: parameter-less method with no help requested
                          if not needsIR
                            then fireDeprecationWarnings >> invokeMethod path (object [])
                            else do
                              -- Build IR for this method's namespace
                              ir <- buildIR soGeneratorInfo (init path)

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
                                      -- IR-15: fire stderr deprecation warnings before real invocation.
                                      else fireDeprecationWarnings >> invokeMethod path params
  where
    handleRespondCommand :: [(Text, Text)] -> SynapseM ()
    handleRespondCommand params = do
      let mRequestId = lookup "request_id" params
          mResponseStr = lookup "response" params
      case (mRequestId, mResponseStr) of
        (Nothing, _) -> throwParse "Missing --request-id for respond subcommand"
        (_, Nothing) -> throwParse "Missing --response for respond subcommand"
        (Just requestId, Just responseStr) ->
          case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 responseStr) of
            Left err -> throwParse $ "Invalid --response JSON: " <> T.pack err
            Right (resp :: Response Value) -> sendResponse requestId resp

    invokeMethod path params = do
      let namespacePath = init path  -- path without method name
      let methodName' = last path
      -- Set method path hint for template resolution
      let rendererCfg' = withMethodPath rendererCfg path
      -- Determine bidirectional mode (--bidir-cmd takes precedence)
      bidirMode <- liftIO $ case soBidirCmd of
        Just cmd -> pure $ BidirCmd cmd
        Nothing  -> case soBidirMode of
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
        (handleBidirRequest bidirMode Nothing)

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
        -- PLX-143: this used to call `fetchMethodSchema (init path) (last path)`,
        -- which fetches `<init path>.schema` and then `find`s the method by name
        -- in `psMethods`. That is the same request and the same selection that
        -- `navigate` performed one line above (Navigate.findMethod over the
        -- schema at `init path`), so it returned the very `MethodSchema` already
        -- bound here. Same value, one fewer round trip.
        ViewMethod method _parent _path -> pure $ Right $ toJSON method

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

-- | Like 'parsePathAndParams' but treats every positional as a single
-- raw token: no dot-splitting and no kebab→snake dash-replacement on
-- path segments. Used only by the legacy @_self@ path (defect C1) where
-- positionals are literal values (e.g. an IPv4 host @127.0.0.1@, a port,
-- a backend name) rather than Plexus RPC path syntax. Flag parsing is
-- unchanged: @--key value@ / boolean flags / @--help@ behave identically.
parseRawPathAndParams :: [Text] -> ([Text], [(Text, Text)], Bool)
parseRawPathAndParams = go [] [] False
  where
    go path params helpReq [] = (reverse path, reverse params, helpReq)
    go path params helpReq (x:xs)
      | x == "--help" || x == "-h" =
          go path params True xs
      | Just key <- T.stripPrefix "--" x
      , not (T.null key)
      , (val:rest) <- xs
      , not (T.isPrefixOf "--" val) =
          let normalizedKey = T.replace "-" "_" key
          in go path ((normalizedKey, val) : params) helpReq rest
      | Just key <- T.stripPrefix "--" x
      , not (T.null key) =
          let normalizedKey = T.replace "-" "_" key
          in go path ((normalizedKey, "") : params) helpReq xs
      -- Raw positional: keep the whole token (no dot-split, no dash-replace).
      | otherwise =
          go (x : path) params helpReq xs

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
        "Backend not found: '" <> T.unpack name <> "' is not registered.\n\n"
        <> "Backends self-register with the registry when they start (Z2H-6).\n"
        <> "To fix:\n"
        <> "  - start the backend: it registers itself at startup\n"
        <> "    (registry endpoint: $" <> registryUrlEnvVar
        <> ", default ws://127.0.0.1:4444)\n"
        <> "  - or connect directly, bypassing the registry:\n"
        <> "    synapse -H <host> -P <port> " <> T.unpack name <> " ..."
        <> renderBackendList backends
      BackendUnreachable name ->
        "Backend unreachable: '" <> T.unpack name <> "'"
        <> renderBackendList backends
      NoBackendsAvailable ->
        "No backends available"
        <> renderBackendList backends
      ProtocolHandshakeFailed host port ->
        "Protocol handshake failed at " <> T.unpack host <> ":" <> show port <> "\n"
        <> "The _info request did not complete successfully within 2 seconds.\n"
        <> "This typically means:\n"
        <> "  - The server is not responding to Plexus RPC protocol messages\n"
        <> "  - The server is not sending required StreamDone messages\n"
        <> "  - The connection is timing out"
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
    Just text
      | T.null (T.strip text) -> pure ()
      | otherwise -> do
          TIO.putStr text
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
-- SYN-2: Live dynamic-children enumeration
-- ============================================================================

-- | After rendering a hub schema, walk its 'MethodRoleDynamicChild'
--   gates and call the sibling @list_method@ (when present) to
--   enumerate currently-available children. Each list call:
--
--   * is timed-out at 'DC.defaultInvocationTimeoutMicros' (2s);
--   * is NOT cached. PLX-121 deleted the @(namespace, listMethodName)@
--     + 5-second-TTL cache that used to sit here. It was a mutable key
--     invalidated by a clock, it never produced a hit (the cache was
--     rebuilt on every call to this function), and the Connectome
--     carries no content identity for a list of live instances to
--     replace it with — see "Synapse.DynamicChildren";
--   * gracefully degrades to "no completions" on any failure (timeout,
--     transport error, method-not-found, malformed response).
--
--   Output is appended directly to stdout under a "Dynamic children:"
--   header. When the hub has NO dynamic gates, this is a no-op and
--   prints nothing — preserving byte-identical output for non-hub /
--   legacy-hub activations.
--
--   The 'visited' path is the path used by 'fetchSchemaAt' to reach
--   this schema — we reuse it as the @<ns>@ for the @<ns>.<fn>@ call.
renderDynamicChildrenBlock :: [Text] -> PluginSchema -> SynapseM ()
renderDynamicChildrenBlock visited schema = do
  let gates = DC.dynamicChildGates schema
  case gates of
    [] -> pure ()   -- no dynamic gates — preserve byte-identical output
    _  -> do
      cfg   <- getConfig
      let nsPath = if null visited then [psNamespace schema] else visited
      -- For each gate with a list_method, fetch the children and render.
      liftIO $ do
        TIO.putStrLn ""
        TIO.putStrLn "Dynamic children:"
        mapM_ (renderOneGate cfg nsPath) gates
  where
    renderOneGate cfg nsPath gate = case DC.gateListMethod gate of
      Nothing -> do
        -- Gate exists but no list opt-in. AC #4: render the gate
        -- header alone, no enumeration, no error.
        TIO.putStrLn $ "  " <> DC.gateMethodName gate
          <> " {" <> DC.gateParam gate <> "}  (no list_method advertised)"
      Just listFn -> do
        names <- DC.listChildren cfg nsPath listFn
        TIO.putStrLn $ "  " <> DC.gateMethodName gate
          <> " {" <> DC.gateParam gate <> "}"
        case names of
          [] -> TIO.putStrLn "    (no children)"
          _  -> mapM_ (\n -> TIO.putStrLn ("    - " <> n)) names

-- ============================================================================
-- PLX-121: navigation over the ONE Connectome document
-- ============================================================================

-- | Drive 'Synapse.Connectome.Nav' and dump the wire log.
--
-- The point of the mode is the log. PLX-121's exit gate is "a wire log
-- (captured, not described) shows a full-tree navigation issuing ONE document
-- fetch plus K Dynamic-edge fetches, with K explained", and a mode that
-- printed a pretty tree without a fetch count would prove nothing.
--
-- With no path this walks the whole tree, then navigates twice more to
-- previously-seen subtrees so the log shows what the second and third
-- navigations cost. That is the measurement, not decoration: on the legacy
-- wire each of those cost a @{ns}.schema@ round trip per level.
runConnectomeMode :: [Text] -> SynapseM ()
runConnectomeMode pathSegs = do
  if null pathSegs
    then do
      result <- CNav.walkTree
      case result of
        Left err -> liftIO $ TIO.hPutStrLn stderr (CNav.renderCNavError err)
        Right entries -> do
          liftIO $ TIO.putStrLn "connectome walk (one document, every edge followed once):"
          liftIO $ mapM_ (TIO.putStrLn . renderWalkEntry) entries
          -- Second and third navigations in the SAME process: the tree is
          -- already in hand, so these must issue no fetch at all.
          let revisit segs = do
                r <- CNav.navigateC segs
                liftIO $ TIO.putStrLn $
                  "revisit " <> T.intercalate "." segs <> ": " <> renderLanding r
          mapM_ revisit (take 2 (map CNav.wePathSegs (filter ((/= []) . CNav.wePathSegs) entries)))
    else do
      result <- CNav.navigateC pathSegs
      liftIO $ TIO.putStrLn (renderLanding result)
      -- Repeat the identical navigation: proves the second one is free.
      result2 <- CNav.navigateC pathSegs
      liftIO $ TIO.putStrLn ("repeat: " <> renderLanding result2)
  dumpWireLog

-- | Emit the captured transcript on stderr so stdout stays pipeable.
dumpWireLog :: SynapseM ()
dumpWireLog = do
  wl <- getWireLog
  evs <- liftIO $ readWireLog wl
  liftIO $ do
    TIO.hPutStrLn stderr ""
    TIO.hPutStrLn stderr "--- wire log ---"
    TIO.hPutStr stderr (renderWireLog evs)

renderLanding :: Either CNav.CNavError CNav.CView -> Text
renderLanding = \case
  Left err -> "ERROR: " <> CNav.renderCNavError err
  Right (CNav.CViewActivation act path) ->
    "activation " <> renderSegs path
      <> " (hash " <> C.unHash (C.aHash act)
      <> ", " <> T.pack (show (length (C.aMethods act))) <> " methods"
      <> ", " <> T.pack (show (length (C.aChildren act))) <> " children)"
  Right (CNav.CViewMethod m path) ->
    "method " <> renderSegs path
      <> " (hash " <> C.unHash (C.mHash m) <> ")"

renderWalkEntry :: CNav.WalkEntry -> Text
renderWalkEntry e =
  "  " <> pad (renderSegs (CNav.wePathSegs e))
    <> "  edge=" <> CNav.weEdge e
    <> " methods=" <> T.pack (show (CNav.weMethodCount e))
    <> " hash=" <> CNav.weActivationHash e
    <> maybe "" (\err -> "\n      UNREACHABLE: " <> CNav.renderCNavError err) (CNav.weUnreachable e)
  where
    pad t = t <> T.replicate (max 0 (34 - T.length t)) " "

renderSegs :: [Text] -> Text
renderSegs [] = "<root>"
renderSegs p = T.intercalate "." p

-- ============================================================================
-- Argument Parsing (Synapse Options Only)
-- ============================================================================

-- | Parser for synapse-level options only
-- Backend and path are handled separately after arg splitting
argsInfo :: ParserInfo Args
argsInfo = info (argsParser <**> versionOption <**> helper)
  ( fullDesc
 <> header "synapse - Algebraic CLI for Hub"
 <> progDesc "synapse [OPTIONS] <backend> <path...> [--param value ...]"
 <> noIntersperse  -- Stop option parsing at first positional arg
  )
  where
    versionOption = infoOption (showVersion Meta.version)
      ( long "version" <> short 'V' <> help "Show version" )

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
  -- Z2H-6: -H/-P are the DIRECT connect target. When either is given,
  -- the registry is bypassed entirely (the minute-one, no-registry
  -- path). When neither is given, backend names resolve through the
  -- registry endpoint (PLEXUS_REGISTRY_URL, default ws://127.0.0.1:4444).
  mHost <- optional $ T.pack <$> strOption
    ( long "host" <> short 'H' <> metavar "HOST"
   <> help "Connect directly to HOST, bypassing the registry (default: resolve via registry)" )
  mPort <- optional $ option auto
    ( long "port" <> short 'P' <> metavar "PORT"
   <> help "Connect directly to PORT, bypassing the registry (default: resolve via registry)" )
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
  soConnectome <- switch
    ( long "connectome"
   <> help "PLX-121: navigate over the ONE Connectome document ({ns}.connectome). \
           \With a path, navigate to it; with no path, walk the whole tree. \
           \The wire log (fetches, local edges, hash-keyed cache hits) goes to stderr." )
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
   <> help "Bidirectional mode: interactive (TTY), json (pipe protocol), auto-cancel, defaults, respond" )
  soBidirCmd <- optional $ T.pack <$> strOption
    ( long "bidir-cmd" <> metavar "CMD"
   <> help "Shell command to handle bidir requests (stdin=JSON request, stdout=JSON response)" )
  soNoCache <- switch
    ( long "no-cache"
   <> help "Disable IR caching (force rebuild from schema)" )
  soLogLevel <- optional $ T.pack <$> strOption
    ( long "log-level" <> metavar "LEVEL"
   <> help "Log level: info, debug, trace (default: disabled)" )
  soLogSubsystems <- many $ T.pack <$> strOption
    ( long "log-subsystem" <> metavar "SUBSYSTEM"
   <> help "Filter logs by subsystem: discovery, transport, rpc, schema, cache, navigation (can specify multiple, default: all)" )
  soToken <- optional $ T.pack <$> strOption
    ( long "token" <> short 't' <> metavar "JWT"
   <> help "JWT sent as Cookie: access_token=<jwt> on WebSocket upgrade (overrides any access_token in ~/.plexus/<backend>/defaults.json)" )
  soTokenFile <- optional $ T.pack <$> strOption
    ( long "token-file" <> metavar "PATH"
   <> help "Path to file containing a raw JWT (overrides any access_token in ~/.plexus/<backend>/defaults.json)" )
  soCookies <- many (parseKv <$> strOption
    ( long "cookie" <> metavar "KEY=VALUE"
   <> help "Extra cookie attached to WS upgrade (repeatable; SAFE-S04/REQ-5)" ))
  soHeaders <- many (parseKv <$> strOption
    ( long "header" <> metavar "KEY=VALUE"
   <> help "Extra HTTP header attached to WS upgrade (repeatable; SAFE-S04/REQ-5)" ))
  soNoDeprecationWarnings <- switch
    ( long "no-deprecation-warnings"
   <> help "Suppress invocation-time deprecation warnings on stderr (IR-15)" )
  pure (let soHost         = maybe defaultHost id mHost
            soPort         = maybe defaultPort id mPort
            soExplicitConn = isJust mHost || isJust mPort
        in SynapseOpts{..})

-- | Parse a KEY=VALUE pair (used by --cookie/--header). Treats the first '='
-- as the separator; everything before is the key, everything after is the value.
parseKv :: String -> (Text, Text)
parseKv s =
  let (k, rest) = break (== '=') s
      v = case rest of
            '=':vv -> vv
            _      -> ""
  in (T.pack k, T.pack v)
