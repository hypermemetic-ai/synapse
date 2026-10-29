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

import Control.Monad (filterM, unless, when)
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
import qualified Options.Applicative.Help.Pretty as Pretty
import System.Environment (getEnvironment)
import Data.Version (showVersion)
import qualified Paths_plexus_synapse as Meta
import System.Directory (getHomeDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, hFlush, stdout, hIsTerminalDevice)


import Synapse.Schema.Types
import Synapse.Monad (SynapseM, SynapseEnv(..), SynapseError(..), BackendErrorType(..), TransportContext(..), TransportErrorCategory(..), initEnvAt, runSynapseM, throwNav, throwTransport, throwParse, throwBackend, withRequestContext, withViewKey, SchemaSource(..), withSchemaSource, isCached)
import qualified Synapse.SchemaStore as SS
import qualified Synapse.Hold as Hold
import Plexus.Client (SubstrateConfig(..), TlsOptions(..))
import Plexus.Auth (Refusal(..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromMaybe, isNothing)
import qualified Synapse.Self as Self
  ( CredentialRef(..)
  , ResolveError(..)
  , ResolvedDefaults(..)
  , defaultRegistry
  , loadDefaults
  , merge
  , resolveAll
  )
import qualified Control.Exception as E
import qualified Synapse.Log as Log
import qualified Katip
import Synapse.Algebra.Navigate
import Synapse.Algebra.Render (Listing(..), renderListing, defaultStyle)
import Synapse.Auth (AnonPage(..), anonymousMethods, renderAnonymousPage, renderCallerHeader, renderRefusal, SessionGrant(..))
import Plexus.Auth (BackendInfo(..), View(..), viewText)
import Plexus.Types (itemRefusal)
import Control.Monad.Except (catchError, throwError)
import Synapse.CLI.Help (MethodPage(..), renderMethodPage)
import Synapse.CLI.Parse (parseParams, parsePathAndParams, pairFlags)
import qualified Synapse.CLI.Parse as Parse
import Synapse.IR.Types (IR, irMethods, irRefusals, MethodDef)
import qualified Synapse.IR.Types as IRT
import Plexus.Schema.Recursive (DeprecationInfo)
import qualified Data.Map.Strict as Map
import qualified Synapse.CLI.Template as TemplateIR
import Synapse.Deprecation (emitActivationWarning, emitMethodWarning)
import Synapse.Transport
import Synapse.Bidir (BidirMode(..), parseBidirMode, detectBidirMode, handleBidirRequest)
import Plexus.Types (Response(..))
import Synapse.CLI.Transform (TransformEnv(..), mkTransformEnv, transformParams, defaultTransformers, injectBooleanDefaults, injectSmartDefaults)
import Synapse.CLI.Source (SourceEnv, mkSourceEnv, expandSources, expandValue, stdinWasRead, renderSourceError)
import Synapse.IR.Builder (buildIR)
import Synapse.Renderer (RendererConfig, defaultRendererConfig, renderItem, prettyValue, withMethodPath)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import qualified Synapse.Self.Commands as Self
import qualified Synapse.Self.Command as SelfCmd
import Synapse.Backend.Discovery (Backend(..), BackendDiscovery(..), registryDiscoveryAt, endpointFor, pingBackendsVia, getBackendAt, getBackendAtEp, probeBackendAtEp, registerWithRegistry, probedInfoAt, probeFailureAt, probedNameAt, rememberInfo)
import Synapse.Endpoint (Endpoint(..), plainAt, isPlain, parseUrl, normalisePath, cliOptions)
import qualified Synapse.Endpoint as E
import Control.Applicative ((<|>))
import System.Environment (lookupEnv)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Synapse.Identity.Command as IdCmd
import qualified Synapse.Identity.Proof as Proof
import qualified Synapse.Identity.Session as Session
import qualified Synapse.Reconnect as Reconnect
import qualified Synapse.Identity.Store as Store
import System.Exit (ExitCode(..), exitWith)
import qualified Synapse.Find as Find
import Synapse.IR.Cache (newIRCache, defaultCacheConfig)

-- ============================================================================
-- Types
-- ============================================================================

-- | Synapse-level options (must appear before backend)
-- Controls connection settings and output format
data SynapseOpts = SynapseOpts
  { soHost          :: Text          -- ^ Registry host for discovery
  , soPort          :: Int           -- ^ Registry port for discovery
  , soHostFlag      :: Maybe Text    -- ^ SCAFFOLD-21: -H as given
  , soPortFlag      :: Maybe Int     -- ^ SCAFFOLD-21: -P as given
  , soUrl           :: Maybe Text    -- ^ SCAFFOLD-21: --url ws[s]://HOST[:PORT][/PATH]
  , soTls           :: Bool          -- ^ SCAFFOLD-21: --tls — wss to -H / -P
  , soPath          :: Maybe Text    -- ^ SCAFFOLD-21: --path — the socket's path
  , soCaCerts       :: [Text]        -- ^ SCAFFOLD-21: --cacert FILE — trusted beside the system's
  , soEndpoint      :: Endpoint      -- ^ SCAFFOLD-21: the connection point, resolved from the above
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
  , soBidirCmd      :: Maybe Text    -- ^ Bidirectional subprocess command (--bidir-cmd)
  , soNoCache       :: Bool          -- ^ Disable IR caching
  , soLogLevel      :: Maybe Text    -- ^ Log level: info, debug, trace
  , soLogSubsystems :: [Text]        -- ^ Filter logs by subsystems (empty = all)
  , soToken         :: Maybe Text    -- ^ JWT sent as Cookie: access_token=<jwt>
  , soTokenFile     :: Maybe Text    -- ^ Path to token file (overridden by --token)
  , soCookies       :: [(Text, Text)]  -- ^ SAFE-S04/REQ-5: extra --cookie KEY=VALUE entries
  , soHeaders       :: [(Text, Text)]  -- ^ SAFE-S04/REQ-5: extra --header KEY=VALUE entries
  , soNoDeprecationWarnings :: Bool  -- ^ Suppress invocation-time deprecation warnings (IR-15)
  , soAs            :: Maybe Text    -- ^ IDENTITY-43: which stored agent session to carry
  , soReconnectFor  :: Maybe Text    -- ^ SYNAPSE-20: how long to ride out an outage (10m; 0 = never)
  , soNoHold        :: Bool          -- ^ SYNAPSE-28: no connection held between runs
  , soHoldFor       :: Maybe Text    -- ^ SYNAPSE-28: how long the holder waits with nothing to do (10m)
  }
  deriving Show

-- | Full CLI arguments after two-phase parsing
data Args = Args
  { argOpts    :: SynapseOpts   -- ^ Synapse-level options
  , argBackend :: Maybe Text    -- ^ Backend name (first positional)
  , argPath    :: [Text]        -- ^ Path segments and --key value params (raw)
  , argFind    :: Maybe Find.FindOpts  -- ^ SYNAPSE-17: a search (_find), not a call
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

-- | IDENTITY-16: the upgrade headers a discovery probe carries — the token
-- (as the access_token cookie), --cookie and --header, as the command line
-- and its env forms give them. Stored per-backend defaults are not read
-- here: the probe runs before the backend is named.
--
-- (No signature: the result is the websockets package's header list, which
-- this executable does not depend on by name.)
probeHeaders opts ep = do
  mToken <- resolveToken opts ""
  (cookies, headers) <- collectRequestContext opts
  sess <- sessionHeaders opts ep (isJust mToken || hasCredential cookies headers)
  pure (mergeUpgradeHeaders mToken cookies (headers ++ sess))

-- | Does this cookie/header set already carry a credential (an
-- access_token cookie or an Authorization header)? Then no stored session
-- is added to it.
hasCredential :: [(Text, Text)] -> [(Text, Text)] -> Bool
hasCredential cookies headers =
  any ((== "access_token") . T.toLower . fst) cookies
    || any ((== "authorization") . T.toLower . fst) headers

-- | IDENTITY-43: the agent asked for — @--as@, else @SYNAPSE_AS@.
askedAgent :: SynapseOpts -> IO (Maybe Text)
askedAgent opts = do
  env <- lookupEnv "SYNAPSE_AS"
  pure (soAs opts <|> (T.pack <$> mfilter' (not . null) env))
  where mfilter' f m = m >>= \x -> if f x then Just x else Nothing

-- | IDENTITY-43: the stored session's @Authorization: Bearer@ for a call to
-- the address — none when the call already carries a credential (-t,
-- --token-file, an access_token cookie, an Authorization header).
sessionHeaders :: SynapseOpts -> Endpoint -> Bool -> IO [(Text, Text)]
sessionHeaders opts ep explicit
  | explicit = pure []
  | otherwise = do
      st <- Store.systemStoreEnv
      asked <- askedAgent opts
      logger <- makeLoggerFromOpts opts
      maybe [] (\s -> [Session.bearer (Session.asToken s)]) <$> Session.sessionFor logger st asked ep

-- | IDENTITY-43: the _info probe, logging in again from the key once when
-- the upgrade is refused (401) while a stored session is carried.
probeWithSession :: SynapseOpts -> Endpoint -> IO (Maybe Text)
probeWithSession = probeWithSession' False

-- | SYNAPSE-20: with @waits@, a probe that fails as an outage does is
-- tried again for the reconnect window.
probeWithSession' :: Bool -> SynapseOpts -> Endpoint -> IO (Maybe Text)
probeWithSession' waits opts ep = probedNameAt ep >>= \case
  -- SYNAPSE-28: this run has asked already
  Just name -> pure (Just name)
  Nothing -> probeWithSession'' waits opts ep

probeWithSession'' :: Bool -> SynapseOpts -> Endpoint -> IO (Maybe Text)
probeWithSession'' waits opts ep = do
  probe <- probeHeaders opts ep
  let once = probeBackendAtEp probe ep
      whereTo = Store.addressKey ep
  r <- if waits
    then Reconnect.awaitBackend
           (\why -> Reconnect.isOutageText why && not (Session.isUpgradeRefused why))
           whereTo once
    else once
  case r of
    Right name -> pure (Just name)
    Left why
      | Session.isUpgradeRefused why -> do
          can <- Session.canReattest ep
          if not can then pure Nothing else do
            st <- Store.systemStoreEnv
            logger <- makeLoggerFromOpts opts
            new <- Session.reattest logger st ep
            case new of
              Nothing -> pure Nothing
              Just _ -> do
                probe' <- probeHeaders opts ep
                getBackendAtEp probe' ep
      | otherwise -> pure Nothing

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
buildEnv :: Endpoint -> Text -> Log.Logger -> SynapseOpts -> IO SynapseEnv
buildEnv ep backend logger opts = do
  -- Legacy token path (unchanged semantically; SELF-3 consolidates).
  mToken <- resolveToken opts backend
  (cliCookieList, cliHeaderList) <- collectRequestContext opts

  -- SELF-2: load + resolve + merge.
  stored <- E.catch (Self.loadDefaults backend) $ \e -> do
    hPutStrLn stderr $ "Error: " <> show (e :: E.IOException)
    exitFailure
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

  -- IDENTITY-43: no credential given or stored for this backend — carry
  -- the agent's stored session, if there is one.
  sess <- sessionHeaders opts ep
            (hasCredential (Map.toList mergedCookies) (Map.toList mergedHeaders))

  -- 'seToken = Nothing' here: any access_token cookie already sits in the
  -- merged cookie map. Transport's mergeUpgradeHeaders joins them into a
  -- single Cookie header exactly as before.
  env0 <- initEnvAt ep backend logger Nothing
  pure $ withRequestContext (Map.toList mergedCookies)
                             (Map.toList mergedHeaders ++ sess)
                             env0

-- ============================================================================
-- Argument Splitting
-- ============================================================================
-- Main
-- ============================================================================

main :: IO ()
main = do
  args0 <- execParser argsInfo
  -- SYNAPSE-28: the connection holder a run started (synapse _hold …)
  when (argBackend args0 == Just "_hold") $ runHold (argPath args0)
  -- SCAFFOLD-21: where the connection point is — --url, or -H / -P with
  -- --tls / --path — resolved once, before anything dials
  opts <- either (\e -> hPutStrLn stderr ("synapse: " <> T.unpack e) >> exitWith (ExitFailure 2)) pure
            =<< resolveConnection (argOpts args0)
  let args = args0 { argOpts = opts }
      ep = soEndpoint opts

  -- SYNAPSE-20: the reconnect window, before anything dials
  envWindow <- lookupEnv "SYNAPSE_RECONNECT_FOR"
  case soReconnectFor opts <|> fmap T.pack envWindow of
    Nothing -> pure ()
    Just w -> case Reconnect.parseDuration w of
      Just secs -> Reconnect.setReconnectWindow secs
      Nothing -> do
        hPutStrLn stderr $ "synapse: --reconnect-for takes a duration (10m, 90s, 1h, 0), not '" <> T.unpack w <> "'"
        exitWith (ExitFailure 2)

  -- IDENTITY-43: keys are local; _login / _logout dial the backend
  -- themselves (a proof opens exactly one connection — no probe first).
  when (argBackend args == Just "_key") $ runKey (argPath args)
  when (argBackend args == Just "_login") $ runLogin opts (argPath args)
  when (argBackend args == Just "_logout") $ runLogout opts (argPath args)

  -- SYNAPSE-28: every request of this run goes over one connection — the
  -- holder's, kept between runs, or (--no-hold) this process's own
  installCallRouter opts

  -- SYNAPSE-28: a call to a backend the cache knows is sent at once — no
  -- probe, no schema fetch; they run only when the call fails in a way
  -- the schema explains
  case argBackend args of
    Just b | sendsFirst args b -> sendFirst args b
    _ -> pure ()
  slowPath args

-- | The run as it always went: the probe, the backend found, the schema
-- fetched (SYNAPSE-28: once each, on the run's one connection, and kept
-- for the next run).
slowPath :: Args -> IO ()
slowPath args = do
  let opts = argOpts args
      ep = soEndpoint opts

  -- Initialize logger (default: ErrorS, only show errors)
  logger <- makeLoggerFromOpts opts

  Log.logDebug logger Log.SubsystemCLI "synapse starting..."

  -- Use specified host/port for registry discovery
  let discovery = registryDiscoveryAt ep

  -- Get the backend at the connection point (host:port)
  -- If it has a registry plugin, that's used for discovering other backends
  Log.logDebug logger Log.SubsystemDiscovery $
    "Checking backend at " <> Store.addressKey ep

  -- IDENTITY-16: the probe carries what the command line supplied (-t,
  -- --token-file, --cookie, --header and their env forms), so a backend that
  -- refuses anonymous upgrades still answers it.
  -- IDENTITY-43: and the agent's stored session, made again from its key
  -- if the upgrade refuses it.
 -- SYNAPSE-20: a call to a named backend waits out an outage at start
  -- too (a promote's restart); the registry listing and _self never wait.
  let waits = case argBackend args of
        Just b -> b == "_find" || not ("_" `T.isPrefixOf` b || "-" `T.isPrefixOf` b)
        Nothing -> False
  maybeBackend <- probeWithSession' waits opts ep

  Log.logDebug logger Log.SubsystemDiscovery $
    "Backend check complete: " <> T.pack (show maybeBackend)
  let hostBackend = maybe "plexus" id maybeBackend

  -- Auto-register: if we're connecting to a non-default port, push the
  -- discovered backend to the registry at the default port (if reachable)
  let targetPort = soPort opts
      targetHost = soHost opts
  -- SCAFFOLD-21: a registry records ws://HOST:PORT; an address behind TLS
  -- or on a path is not registered
  when (targetPort /= defaultPort && isPlain ep) $ do
    case maybeBackend of
      Just name -> do
        -- Check if the registry is at the default port
        maybeRegistry <- getBackendAt targetHost defaultPort
        case maybeRegistry of
          Just registryName ->
            registerWithRegistry targetHost defaultPort registryName
              name targetHost targetPort
          Nothing -> pure ()
      Nothing -> pure ()

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
          backendsWithStatus <- pingBackendsVia ep backends
          TIO.putStrLn "\nAvailable backends:"
          mapM_ (printBackend ep) backendsWithStatus
          TIO.putStrLn "\nUsage: synapse <backend> [command...]"
    Just backend
      -- Handle --help/-h if it somehow ends up as the backend
      | backend `elem` ["--help", "-h"] -> TIO.putStr cliHeader
      -- SYNAPSE-17: _find BACKEND [activation…] WORDS… — a search over the
      -- backend's schema, reached as a call to that backend is
      | backend == "_find" -> do
          fo <- parseBuiltin "_find" Find.findInfo (argPath args)
          runWithDiscovery discovery (Find.foBackend fo)
            args { argBackend = Just (Find.foBackend fo), argPath = [], argFind = Just fo }
      -- Handle _self meta-commands
      | backend == "_self" ->
          -- SELF-4 verbs (synapse _self <backend> <verb>) dispatch through
          -- a dedicated parser on the raw argPath. Legacy _self
          -- subcommands (template / scan / debug / validate / test) keep
          -- the original path through runWithDiscovery.
          if isSelfV4Invocation (argPath args)
            then runSelfV4 (argPath args)
            else runWithDiscovery discovery hostBackend args
      | otherwise -> runWithDiscovery discovery backend args

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

-- | A builtin's options, its usage said as @synapse NAME …@.
parseBuiltin :: String -> ParserInfo a -> [Text] -> IO a
parseBuiltin name pinfo rest =
  case execParserPure defaultPrefs pinfo (map T.unpack rest) of
    Failure f -> do
      let (msg, code) = renderFailure f ("synapse " <> name)
      case code of
        ExitSuccess -> putStrLn msg >> exitSuccess
        _ -> hPutStrLn stderr msg >> exitWith code
    r -> handleParseResult r

-- | IDENTITY-43: @synapse _key new|show|list@ — local.
runKey :: [Text] -> IO ()
runKey rest = do
  cmd <- handleParseResult (execParserPure defaultPrefs IdCmd.keyCommandInfo (map T.unpack rest))
  st <- Store.systemStoreEnv
  IdCmd.runKeyCommand st cmd >>= exitWith

-- | IDENTITY-43: where @_login@ / @_logout@ dial — the connection point
-- named with @-P@, else what the registry says of the backend, else the
-- connection point.
loginAddress :: SynapseOpts -> Text -> IO Endpoint
loginAddress opts backendName
  | soPort opts /= defaultPort || not (isPlain ep) = pure ep
  | otherwise = do
      registered <- getBackendInfo (registryDiscoveryAt ep) backendName
      pure $ maybe ep (endpointFor ep) registered
  where ep = soEndpoint opts

dieWith :: Text -> IO a
dieWith msg = TIO.hPutStrLn stderr ("synapse: " <> msg) >> exitFailure

-- | IDENTITY-43: @synapse -P PORT _login BACKEND --key NAME --as AGENT
-- [--scope role\@SCOPE …] [--ttl SECS]@.
runLogin :: SynapseOpts -> [Text] -> IO ()
runLogin opts rest = do
  lo <- handleParseResult (execParserPure defaultPrefs IdCmd.loginInfo (map T.unpack rest))
  asked <- askedAgent opts
  -- AUTH-8: no key and no agent named — a person, through the browser
  when (IdCmd.loKey lo == Nothing && IdCmd.loAs lo == Nothing && asked == Nothing) $
    runLoginPerson opts lo
  agent <- maybe (dieWith "name the agent to log in as: --as AGENT (or SYNAPSE_AS)") pure (IdCmd.loAs lo <|> asked)
  let keyName = maybe "default" id (IdCmd.loKey lo)
  st <- Store.systemStoreEnv
  seed <- Store.lookupKey st keyName >>= \case
    Left e -> dieWith e
    Right Nothing -> dieWith ("no key " <> keyName <> " (synapse _key new " <> keyName <> ")")
    Right (Just (s, _)) -> pure s
  ep <- loginAddress opts (IdCmd.loBackend lo)
  logger <- makeLoggerFromOpts opts
  r <- Session.loginWithKey logger ep (IdCmd.loBackend lo) seed agent (IdCmd.loScope lo) (IdCmd.loTtl lo)
  lr <- either (\e -> dieWith ("login to " <> IdCmd.loBackend lo <> " at " <> Store.addressKey ep <> " failed: " <> e)) pure r
  let rec = Store.SessionRec
        { Store.srToken = Session.lrToken lr
        , Store.srPrincipal = if T.null (Session.lrPrincipal lr) then Proof.principalOf seed else Session.lrPrincipal lr
        , Store.srAgent = Session.lrAgent lr
        , Store.srExpiresAt = Session.lrExpiresAt lr
        , Store.srKey = keyName
        , Store.srScope = IdCmd.loScope lo
        , Store.srTtl = IdCmd.loTtl lo
        , Store.srBackend = IdCmd.loBackend lo
        }
  src <- Store.writeSession st ep rec
  Store.writeDefaultAgent st ep (Store.srAgent rec)
  let until' = case Store.srExpiresAt rec of
        Just t -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" (posixSecondsToUTCTime (fromIntegral t)))
        Nothing -> "(the server named no end)"
      scope = if null (Session.lrScope lr) then "the key's own grants" else T.intercalate ", " (Session.lrScope lr)
      kept = case src of
        Store.SessionFromKeychain -> "keychain"
        Store.SessionFromFile p -> T.pack p
        Store.SessionFromEnv -> "PLEXUS_SESSION"
  TIO.putStrLn $ "signed in to " <> IdCmd.loBackend lo <> " at " <> Store.addressKey ep <> " as " <> Store.srAgent rec
  mapM_ (\(k, v) -> TIO.putStrLn ("  " <> T.justifyLeft 10 ' ' k <> v))
    [ ("principal", Store.srPrincipal rec)
    , ("scope", scope)
    , ("until", until')
    , ("kept in", kept)
    ]
  exitSuccess

-- | AUTH-8: the session slot a person's sign-in is kept under.
personAgent :: Text
personAgent = "me"

-- | AUTH-8: @synapse -P PORT _login BACKEND@ — a person signs in through
-- the browser (the loopback flow), and the session is kept as an agent's
-- is (keychain, else file), under the name @me@, with no key to make it
-- again from: when it ends, synapse says to sign in again.
runLoginPerson :: SynapseOpts -> IdCmd.LoginOpts -> IO ()
runLoginPerson opts lo = do
  let backend = IdCmd.loBackend lo
  ep <- loginAddress opts backend
  logger <- makeLoggerFromOpts opts
  r <- Session.loginPerson logger ep backend 300 (TIO.hPutStrLn stderr)
  g <- either (\e -> dieWith ("sign-in to " <> backend <> " at " <> Store.addressKey ep <> " failed: " <> e)) pure r
  who <- case sgWhoami g of
    Just w -> pure (Just w)
    Nothing -> Session.whoamiWith logger ep backend (sgToken g)
  let rec = Store.SessionRec
        { Store.srToken = sgToken g
        , Store.srPrincipal = sgPrincipal g
        , Store.srAgent = personAgent
        , Store.srExpiresAt = sgExpiresAt g
        , Store.srKey = ""          -- a person's: nothing to sign in again from
        , Store.srScope = sgCeiling g
        , Store.srTtl = Nothing
        , Store.srBackend = backend
        }
  st <- Store.systemStoreEnv
  src <- Store.writeSession st ep rec
  Store.writeDefaultAgent st ep personAgent
  let named = case who of
        Just w -> maybe "" id (T.stripPrefix "you are " (renderCallerHeader w))
        Nothing -> case sgName g of
          Just n -> n <> " (" <> sgPrincipal g <> ")"
          Nothing -> sgPrincipal g
      until' = case sgExpiresAt g of
        Just t -> T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" (posixSecondsToUTCTime (fromIntegral t)))
        Nothing -> "(the server named no end)"
      kept = case src of
        Store.SessionFromKeychain -> "keychain"
        Store.SessionFromFile p -> T.pack p
        Store.SessionFromEnv -> "PLEXUS_SESSION"
  TIO.putStrLn $ "signed in to " <> backend <> " as " <> named
  mapM_ (\(k, v) -> TIO.putStrLn ("  " <> T.justifyLeft 10 ' ' k <> v))
    [ ("until", until')
    , ("kept in", kept <> " (as " <> personAgent <> ")")
    ]
  exitSuccess

-- | IDENTITY-43: @synapse -P PORT _logout BACKEND [--as AGENT]@.
runLogout :: SynapseOpts -> [Text] -> IO ()
runLogout opts rest = do
  lo <- handleParseResult (execParserPure defaultPrefs IdCmd.logoutInfo (map T.unpack rest))
  st <- Store.systemStoreEnv
  ep <- loginAddress opts (IdCmd.lgBackend lo)
  asked <- askedAgent opts
  dflt <- Store.readDefaultAgent st ep
  agent <- maybe (dieWith ("no agent is signed in to " <> Store.addressKey ep <> " (name one: --as AGENT)")) pure
             (IdCmd.lgAs lo <|> asked <|> dflt)
  found <- Store.lookupSession st ep agent
  rec <- case found of
    Just (Right r, src) | src /= Store.SessionFromEnv -> pure r
    _ -> dieWith ("no stored session for " <> agent <> " at " <> Store.addressKey ep)
  logger <- makeLoggerFromOpts opts
  r <- Session.logoutWithToken logger ep (IdCmd.lgBackend lo) (Store.srToken rec)
  Store.removeSession st ep agent
  Store.clearDefaultAgent st ep agent
  case r of
    Right () -> TIO.putStrLn ("signed out " <> agent <> " at " <> Store.addressKey ep)
    Left e -> TIO.putStrLn ("forgot " <> agent <> "'s session at " <> Store.addressKey ep
                             <> " (the server said: " <> e <> ")")
  exitSuccess

-- | Print a backend in the list
-- (SCAFFOLD-21: the connection point by the URL it was named with, when
-- it is not plain ws://HOST:PORT/)
printBackend :: Endpoint -> Backend -> IO ()
printBackend ep b = do
  let nameField = T.justifyLeft 15 ' ' (backendName b)
  let hostPort = Store.addressKey (endpointFor ep b)
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
      ep = soEndpoint opts
  -- Try to discover backend info
  maybeBackend <- getBackendInfo discovery backendName

  case maybeBackend of
    Just backend -> do
      -- Verify the resolved backend actually identifies as the requested name.
      -- This catches cases where -P points to a different backend than expected,
      -- or the registry has stale entries.
      -- SYNAPSE-20: a named backend's probe waits out an outage (or the
      -- server's 429 under many parallel calls) as the first one did
      -- SCAFFOLD-21: at the connection point, reached as it was named
      let at = endpointFor ep backend
      actualName <- probeWithSession' True opts at
      case actualName of
        Just name | name /= backendName -> do
          hPutStrLn stderr $ "Backend mismatch: requested '" <> T.unpack backendName
            <> "' but " <> T.unpack (Store.addressKey at)
            <> " identifies as '" <> T.unpack name <> "'"
          exitFailure
        Nothing -> do
          -- Protocol handshake failed when verifying backend
          when (epTls at) $ tlsUnreachable at
          logger <- makeLoggerFromOpts opts
          env <- buildEnv ep backendName logger opts
          result <- runSynapseM env (throwBackend (ProtocolHandshakeFailed (backendHost backend) (backendPort backend)) [])
          case result of
            Left err -> do
              hPutStrLn stderr $ renderError err
              exitFailure
            Right () -> exitSuccess
        Just _ ->
          -- Match confirmed
          run Fresh backendName at args
    Nothing -> do
      -- Backend not found in registry - check if this was a direct connection attempt
      -- where the protocol handshake failed
      maybeDirectBackend <- probeWithSession' True opts ep
      logger <- makeLoggerFromOpts opts
      env <- buildEnv ep backendName logger opts
      case maybeDirectBackend of
        Nothing -> do
          -- Protocol handshake failed at the specified host:port
          when (epTls ep) $ tlsUnreachable ep
          result <- runSynapseM env (throwBackend (ProtocolHandshakeFailed (soHost opts) (soPort opts)) [])
          case result of
            Left err -> do
              hPutStrLn stderr $ renderError err
              exitFailure
            Right () -> exitSuccess
        Just discoveredName | discoveredName /= backendName -> do
          -- Backend exists but has a different name
          backends <- discoverBackends discovery
          backendsWithStatus <- pingBackendsVia ep backends
          result <- runSynapseM env (throwBackend (BackendNotFound backendName) backendsWithStatus)
          case result of
            Left err -> do
              hPutStrLn stderr $ renderError err
              exitFailure
            Right () -> exitSuccess
        Just _ -> do
          -- Backend discovered successfully - should not reach here, but proceed
          run Fresh backendName ep args

-- | SCAFFOLD-21: a wss address whose probe failed — why, from the probe
-- (a certificate the trust store refused, a name it does not cover, a
-- proxy that does not upgrade), and what to do about a local CA. Exits 1.
tlsUnreachable :: Endpoint -> IO ()
tlsUnreachable at = do
  why <- maybe "no answer" id <$> probeFailureAt at
  let shown = T.replace "\\\"" "\"" (fst (T.breakOn "\\nCallStack" why))
      detail = case T.breakOn "Error_Protocol \"" shown of
        (_, r) | not (T.null r) -> T.takeWhile (/= '"') (T.drop (T.length "Error_Protocol \"") r)
        _ -> shown
      has = any (`T.isInfixOf` shown)
  if has ["HandshakeFailed"]
    then do
      TIO.hPutStrLn stderr $ "synapse: the TLS handshake with " <> E.shortUrl at <> " failed: " <> detail
      TIO.hPutStrLn stderr $
        if has ["UnknownCa", "unknown CA", "ertificate", "NameMismatch", "Expired", "SelfSigned"]
          then "  its certificate is checked against the system's trust store and each --cacert, and its name\n"
            <> "  against " <> epHost at <> "; for a local CA, add --cacert ROOT.pem"
          else "  the server refused it — does it serve a certificate for " <> epHost at <> "?"
    else case T.breakOn "responseCode = " shown of
      (_, r) | not (T.null r) -> do
        let code = T.takeWhile (`elem` ['0' .. '9']) (T.drop (T.length "responseCode = ") r)
        TIO.hPutStrLn stderr $ "synapse: " <> E.shortUrl at <> " answered the upgrade with HTTP " <> code
          <> ", not a WebSocket"
        TIO.hPutStrLn stderr $ if code `elem` ["401", "403"]
          then "  it refused the credential: sign in with synapse " <> cliOptions at <> " _login BACKEND"
          else "  is " <> epPath at <> " the socket's path? (a hosted app's is /ws)"
      _ -> TIO.hPutStrLn stderr $ "synapse: " <> E.shortUrl at <> " did not answer the _info probe: " <> detail
  exitFailure

-- | SYNAPSE-28: how a run gets its schemas.
data RunMode
  = Fresh
    -- ^ from the server (the probe has run), kept in the cache for the next run
  | Cached SS.Store (Maybe SS.Plan) (IO ())
    -- ^ from the cache alone, the call sent first; the action is the run
    -- again from the server, for when the cache could not answer or the
    -- server refused what it said (it exits)

-- | Run a Hub command with specified backend and connection details
run :: RunMode -> Text -> Endpoint -> Args -> IO ()
run mode backend ep args = do
  let opts = argOpts args
  logger <- makeLoggerFromOpts opts
  Log.logInfo logger Log.SubsystemCLI $ "Synapse starting: backend=" <> backend <> ", at " <> Store.addressKey ep
  -- SYNAPSE-28: where this run's schemas come from and go
  source <- case mode of
    Cached st plan _ -> pure (FromCache st, plan)
    Fresh -> do
      st <- storeAt opts ep backend
      case st of
        Just store -> do
          SS.writeRoute (soEndpoint opts) backend ep
          probedInfoAt ep >>= mapM_ (SS.writeInfo store)
        Nothing -> pure ()
      pure (FromServer st, Nothing)
  let mkEnv = uncurry withSchemaSource source <$> (withView ep =<< buildEnv ep backend logger opts)
  env <- mkEnv
  rendererCfg <- defaultRendererConfig
  Log.logDebug logger Log.SubsystemCLI "Running dispatch..."
  result <- runSynapseM env (dispatch args rendererCfg)
  -- SYNAPSE-28: the cache could not answer for the call, or the server
  -- refused it before it ran (a method or flag the cache did not know):
  -- ask the server and run it again
  stale <- takeStale
  case mode of
    Cached st _ again | stale || isStaleError result || isMiss result -> do
      Log.logInfo logger Log.SubsystemCLI $ "The cache could not answer (" <> staleWhy result <> "); asking " <> Store.addressKey ep
      -- what misled the call is forgotten; a miss forgets nothing
      when (not (isMiss result)) $ SS.forget st
      again
    _ -> pure ()
  -- IDENTITY-43: the server refused the stored session (the upgrade's 401,
  -- or the in-band "you have signed out" held back before anything was
  -- shown): log in again from the key, and run the call once more.
  ended <- Session.takeSessionEnded
  let refused = case result of
        Left (TransportErrorContext ctx) -> Session.isUpgradeRefused (tcMessage ctx)
        Left (NavError (FetchError m _)) -> Session.isUpgradeRefused m
        _ -> False
  result' <- if not (isJust ended || refused) then pure result else do
    st <- Store.systemStoreEnv
    new <- Session.reattest logger st ep
    case new of
      Just _ -> do
        env' <- mkEnv
        runSynapseM env' (dispatch args rendererCfg)
      Nothing -> do
        mapM_ (\m -> hPutStrLn stderr ("Error: " <> T.unpack m)) ended
        pure result
  case result' of
    Left err -> do
      hPutStrLn stderr $ renderError err
      exitFailure
    Right () -> do
      -- AUTH-8: a refusal was shown — the call did not happen
      refused' <- Session.wasRefused
      if refused' then exitWith (ExitFailure refusedExit) else exitSuccess
  where
    isStaleError = \case
      Left (StaleSchema _) -> True
      _ -> False
    isMiss = \case
      Left (CacheMiss _) -> True
      _ -> False
    staleWhy = \case
      Left (StaleSchema why) -> why
      Left (CacheMiss why) -> why
      _ -> "the server refused what it said"

-- | SYNAPSE-28: the cache this run reads and fills — the agent's view of
-- the backend at the address; none when the call carries a credential of
-- its own (-t, --token-file, a cookie or header, PLEXUS_SESSION), whose
-- view is no agent's.
storeAt :: SynapseOpts -> Endpoint -> Text -> IO (Maybe SS.Store)
storeAt opts ep backend = do
  explicit <- explicitCredential opts
  if explicit then pure Nothing else do
    st <- Store.systemStoreEnv
    asked <- askedAgent opts
    agent <- maybe (Store.readDefaultAgent st ep) (pure . Just) asked
    Just <$> SS.storeFor ep (fromMaybe "anonymous" agent) backend

-- | SYNAPSE-28: does the call bring its own credential?
explicitCredential :: SynapseOpts -> IO Bool
explicitCredential opts = do
  (cookies, headers) <- collectRequestContext opts
  envSession <- lookupEnv "PLEXUS_SESSION"
  pure $ isJust (soToken opts) || isJust (soTokenFile opts)
    || not (null cookies) || not (null headers)
    || maybe False (not . null) envSession

-- | SYNAPSE-28: may this call be sent first? A call to a named backend
-- (not a meta-command), and not a mode that is about the schema itself.
sendsFirst :: Args -> Text -> Bool
sendsFirst args b =
  not ("_" `T.isPrefixOf` b || "-" `T.isPrefixOf` b)
    && not (soSchema o || soGenerate o || soEmitIR o || soNoCache o)
    && isNothing (soRpc o) && isNothing (argFind args)
  where o = argOpts args

-- | SYNAPSE-28: send the call from the cache — where the backend is, its
-- _info and the method's plan as the last run left them — with no probe
-- and no schema fetch. Returns (to the run as it always went) when the
-- cache has not got what the call needs; otherwise it exits.
sendFirst :: Args -> Text -> IO ()
sendFirst args backend = do
  let opts = argOpts args
      (pathSegs, _, _, helpRequested) = parsePathAndParams (argPath args)
  route <- SS.readRoute (soEndpoint opts) backend
  case route of
    Nothing -> pure ()
    Just ep -> do
      mst <- storeAt opts ep backend
      case mst of
        Nothing -> pure ()
        Just st -> do
          info <- SS.readInfo st
          plan <- SS.readPlan st pathSegs
          case info of
            -- an anonymous view is the door, never sent first; help and
            -- listings are read from the server (they show what is new)
            Just i | infoView i /= Just Anonymous, infoBackend i == backend
                   , not (null pathSegs), not helpRequested -> do
              rememberInfo ep i
              run (Cached st plan (slowPath args)) backend ep args
            _ -> pure ()

-- | SYNAPSE-28: the cache cannot answer for the call: ask the server.
stale :: Text -> SynapseM a
stale why = throwError (StaleSchema why)

-- | SYNAPSE-28: the server refused a call sent from the cache before it
-- ran, in a way the schema explains — a method it does not have (under
-- the AUTH gate, an unlisted method is one only admin holds), or params it
-- cannot read (a field missing, unknown, or of another type). The item is
-- held back, and the call is run again from the server's schema.
{-# NOINLINE staleRef #-}
staleRef :: IORef Bool
staleRef = unsafePerformIO (newIORef False)

markStale :: IO ()
markStale = writeIORef staleRef True

takeStale :: IO Bool
takeStale = do
  v <- readIORef staleRef
  writeIORef staleRef False
  pure v

isStaleItem :: HubStreamItem -> Bool
isStaleItem item = case itemRefusal item of
  Just r -> refusalRoles r == ["admin"]
  Nothing -> case item of
    HubError _ _ msg _ -> any (`T.isInfixOf` msg)
      [ "Method not found", "Activation not found", "Unknown method", "method not found"
      , "errCode = -32601", "missing field", "unknown field", "invalid type", "unknown variant" ]
    _ -> False

-- | SYNAPSE-28: route every call through the connection holder for its
-- address and agent (started by the first run), or — with --no-hold, a
-- credential of the call's own, or no holder to be had — over one
-- connection of this process's own.
installCallRouter :: SynapseOpts -> IO ()
installCallRouter opts = do
  noHoldEnv <- lookupEnv "SYNAPSE_NO_HOLD"
  holdForEnv <- lookupEnv "SYNAPSE_HOLD_FOR"
  holdFor <- case soHoldFor opts <|> fmap T.pack holdForEnv of
    Nothing -> pure Hold.defaultHoldFor
    Just w -> case Reconnect.parseDuration w of
      Just secs -> pure secs
      Nothing -> do
        hPutStrLn stderr $ "synapse: --hold-for takes a duration (10m, 90s, 1h, 0), not '" <> T.unpack w <> "'"
        exitWith (ExitFailure 2)
  explicit <- explicitCredential opts
  let off = soNoHold opts || maybe False (`elem` ["1", "true", "yes", "on"]) noHoldEnv
              || explicit || holdFor <= 0
  st <- Store.systemStoreEnv
  asked <- askedAgent opts
  let version = T.pack (showVersion Meta.version)
      cp = soEndpoint opts
      viaRegistry = Store.addressKey cp == Store.addressKey (plainAt defaultHost defaultPort)
      socketFor cfg = do
        let ep = endpointOfConfig cfg
        -- the run's own address is held — or, reached through the
        -- registry, whatever it names; a registry the run only glances at
        -- (the auto-registration's probe) is not
        if not viaRegistry && Store.addressKey ep /= Store.addressKey cp then pure Nothing else do
          agent <- maybe (Store.readDefaultAgent st ep) (pure . Just) asked
          Just <$> Hold.socketPathFor version (E.dirName ep) (fromMaybe "anonymous" agent)
  _ <- Hold.installRouter Hold.RouterOpts
         { Hold.roHold = if off then Nothing else Just socketFor
         , Hold.roHoldFor = holdFor }
  pure ()

-- | The address a plexus-protocol config dials.
endpointOfConfig :: SubstrateConfig -> Endpoint
endpointOfConfig c = Endpoint
  { epHost = T.pack (substrateHost c)
  , epPort = substratePort c
  , epTls = isJust (substrateTls c)
  , epPath = T.pack (substratePath c)
  , epCaFiles = maybe [] tlsCaFiles (substrateTls c)
  }

-- | SYNAPSE-28: @synapse _hold --socket PATH --hold-for SECS@ — the
-- connection holder (a run starts it; it exits when idle).
runHold :: [Text] -> IO ()
runHold rest = case rest of
  ["--socket", path, "--hold-for", secs]
    | Just d <- (readMaybe (T.unpack secs) :: Maybe Double) ->
        Hold.runHolder Hold.HoldOpts { Hold.hoSocket = T.unpack path, Hold.hoHoldFor = d }
  _ -> do
    hPutStrLn stderr "synapse _hold --socket PATH --hold-for SECS (started by synapse itself)"
    exitWith (ExitFailure 2)

-- | AUTH-8: the exit status of a call the hub refused (sysexits' EX_NOPERM).
refusedExit :: Int
refusedExit = 77

-- | AUTH-8: the env's cache keys qualified by the caller's view, as the
-- _info probe at this address answered it (the view's hash, else its kind).
withView :: Endpoint -> SynapseEnv -> IO SynapseEnv
withView ep env = do
  info <- probedInfoAt ep
  let key = info >>= \i -> infoViewHash i <|> (viewText <$> infoView i)
  pure (withViewKey key env)

-- | Dispatch based on navigation result
dispatch :: Args -> RendererConfig -> SynapseM ()
dispatch Args{argOpts = soOpts@SynapseOpts{..}, argBackend, argPath, argFind} rendererCfg = do
  -- Check for _self commands first (before anything else)
  case argBackend of
    _ | Just fo <- argFind -> runFind fo
    Just "_self" -> do
      -- _self meta-command: parse subcommand and rest from argPath
      let (pathSegs, rawParams, _, _) = parsePathAndParams argPath
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
          let (pathSegs, rawParams, flagArgs, helpRequested) = parsePathAndParams argPath

          -- Normal Plexus RPC routing
          -- Apply parameter transformations (path expansion, env vars)
          transformEnv <- liftIO mkTransformEnv
          -- SYNAPSE-10: a value that names its source (@file, -) is read
          -- first, so no other step ever sees the literal "@file"; stdin is
          -- read once and kept for the second pass below
          sourceEnv <- liftIO $ mkSourceEnv (teCwd transformEnv) (teHome transformEnv)
          let fromSources pairs = liftIO (expandSources sourceEnv pairs) >>= either (throwParse . renderSourceError) pure
          sourcedParams <- if helpRequested then pure rawParams else fromSources rawParams
          inlineParams <- liftIO $ transformParams transformEnv defaultTransformers sourcedParams

          -- Mode 1.5: Respond subcommand (agent sends a response to a pending bidir request)
          if not (null pathSegs) && head pathSegs == "respond"
            then handleRespondCommand sourcedParams

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
                  -- SYNAPSE-CC-8: a site the IR cannot type is named on
                  -- stderr, and the run exits 65 (EX_DATAERR): no generator
                  -- may run on it. The IR is still on stdout.
                  let refusals = irRefusals ir
                  unless (null refusals) $ liftIO $ do
                    TIO.hPutStrLn stderr $ "synapse: refused \x2014 " <> T.pack (show (length refusals))
                      <> " site" <> (if length refusals == 1 then "" else "s")
                      <> " the IR cannot type (irRefusals); nothing was dropped:"
                    let w = maximum (map (T.length . irSite) refusals)
                        kw = maximum (map (T.length . irKeyword) refusals)
                    mapM_ (\r -> TIO.hPutStrLn stderr $ "  " <> T.justifyLeft w ' ' (irSite r) <> "  "
                                  <> T.justifyLeft kw ' ' (irKeyword r) <> "  " <> irDetail r) refusals
                    exitWith (ExitFailure 65)

                else do
                  -- Mode 5: Normal navigation
                  info <- currentInfo
                  let anonymous = (info >>= infoView) == Just Anonymous
                  -- SYNAPSE-28: sent first, from the cache — what the cache
                  -- cannot answer is asked of the server (StaleSchema)
                  cached <- asks isCached
                  plan <- asks sePlan
                  if null pathSegs
                    then if cached then stale "a listing" else do
                      -- SYNAPSE-12: a backend named, its help is the
                      -- backend's alone — no banner, options or meta-commands.
                      -- AUTH-8: to an anonymous caller, only the door.
                      rootSchema <- if anonymous
                        then (Just <$> navigate []) `catchError` \_ -> pure Nothing
                        else Just <$> navigate []
                      case rootSchema of
                        _ | anonymous -> showAnonymous info (rootSchema >>= viewSchema)
                        Just (ViewPlugin schema _) -> showPluginHelp info [] schema
                        _ -> pure ()
                    else do
                      -- Navigate to target
                      -- AUTH-8: an anonymous caller asking for anything the
                      -- door does not show is shown the door
                      view <- case plan of
                       Just p -> pure (ViewMethod (SS.plMethod p) (SS.plPath p))
                       Nothing -> navigate pathSegs `catchError` \e -> case e of
                        NavError (NotFound {}) | cached -> stale "a path the cache does not know"
                        NavError (MethodNotTerminal {}) | cached -> stale "a path the cache does not know"
                        NavError (NotFound {}) | anonymous -> do
                          root <- (Just <$> navigate []) `catchError` \_ -> pure Nothing
                          showAnonymous info (root >>= viewSchema)
                          liftIO $ do
                            TIO.hPutStrLn stderr $ "synapse: sign in to reach " <> T.intercalate " " pathSegs
                            exitWith (ExitFailure refusedExit)
                        _ -> throwError e
                      case view of
                        ViewPlugin {} | cached -> stale "a listing"
                        -- Landed on a plugin: show help
                        ViewPlugin schema _ -> showPluginHelp info pathSegs schema

                        -- Landed on a method: invoke or show help
                        ViewMethod method path -> do
                          let fullPath = T.intercalate "." path

                          -- IR-15: emit stderr deprecation warnings right before any
                          -- real invocation. Dedupe is per-process-lifetime, so this
                          -- action is idempotent on repeat calls. We bind it as a
                          -- local SynapseM action and invoke it at both invocation
                          -- sites below (fast path and slow path). --help, --dry-run,
                          -- --schema, and --emit-ir do not invoke, and intentionally
                          -- do not fire these warnings.
                          -- SYNAPSE-28: the activation's deprecation, from the
                          -- plan when there is one
                          let namespacePath = init path
                              parentDeprecation :: SynapseM (Maybe DeprecationInfo)
                              parentDeprecation = case plan of
                                Just p -> pure (SS.plDeprecation p)
                                Nothing
                                  | null namespacePath -> pure Nothing
                                  | otherwise -> psDeprecation <$> fetchSchemaAt namespacePath
                              fireDeprecationWarnings :: SynapseM ()
                              fireDeprecationWarnings = do
                                liftIO $ emitMethodWarning
                                  soNoDeprecationWarnings
                                  fullPath
                                  (methodDeprecation method)
                                when (not (null namespacePath)) $ do
                                  let activationNs = T.intercalate "." namespacePath
                                  dep <- parentDeprecation
                                  liftIO $ emitActivationWarning
                                    soNoDeprecationWarnings
                                    activationNs
                                    dep
                              -- SYNAPSE-28: what the next call of this path
                              -- reads in place of the activation's schema
                              keepPlan :: Maybe IR -> SynapseM ()
                              keepPlan mir = do
                                src <- asks seSchemaSource
                                let store = case src of
                                      FromCache st -> Just st
                                      FromServer st -> st
                                    current = case plan of
                                      Just p -> isJust (SS.plIR p) || isNothing mir
                                      Nothing -> False
                                case store of
                                  Just st | not current -> do
                                    dep <- parentDeprecation
                                    liftIO $ SS.writePlan st pathSegs SS.Plan
                                      { SS.plMethod = method, SS.plPath = path, SS.plDeprecation = dep
                                      , SS.plIR = SS.planIR fullPath <$> mir }
                                  _ -> pure ()

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
                            then fireDeprecationWarnings >> keepPlan Nothing >> invokeMethod sourceEnv method path (object [])
                            else do
                              -- Build IR for this method's namespace
                              -- (SYNAPSE-28: the plan's, when it has one)
                              ir <- case plan >>= SS.plIR of
                                Just planned -> pure planned
                                Nothing -> buildIR soGeneratorInfo (init path)

                              -- If --help was explicitly requested, show help and exit
                              if helpRequested
                                then if cached then stale "help" else do
                                  case Map.lookup fullPath (irMethods ir) of
                                    Just methodDef -> do
                                      page <- pageFor info pathSegs method
                                      liftIO $ do
                                        TIO.putStr $ renderMethodPage page ir methodDef
                                        backendHelpFooter
                                    Nothing ->
                                      liftIO $ TIO.putStrLn $ T.intercalate "." path <> " - " <> methodDescription method
                                else do
                                  -- Build params: use IR-driven parsing for inline params
                                  let schemaDefaults = extractSchemaDefaults method
                                  userParams <- case soParams of
                                    -- -p JSON: parse as raw JSON (bypass IR parsing);
                                    -- SYNAPSE-10: -p @params.json and -p - name a source
                                    Just given -> do
                                      jsonStr <- liftIO (expandValue sourceEnv "" given) >>= either (throwParse . renderSourceError) pure
                                      case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 jsonStr) of
                                        Left err -> throwParse $ T.pack err
                                        Right p -> pure p
                                    Nothing
                                      | not (null inlineParams) ->
                                          -- Use IR-driven parsing for inline params
                                          case Map.lookup fullPath (irMethods ir) of
                                            Just methodDef -> do
                                              -- Pair the flags again by parameter type, now that the
                                              -- method is known (a value may begin with dashes)
                                              methodParams <- case pairFlags ir methodDef flagArgs of
                                                Right pairs -> fromSources pairs >>= liftIO . transformParams transformEnv defaultTransformers
                                                Left _ | cached -> stale "a flag the cache does not know"
                                                Left errs -> do
                                                  liftIO $ mapM_ (hPutStrLn stderr . renderParseError) errs
                                                  throwParse "Parameter parsing failed"
                                              -- Inject boolean defaults for flags without values
                                              let paramsWithBools = injectBooleanDefaults ir methodDef methodParams
                                              -- Inject smart defaults for missing required params (e.g., --path defaults to cwd)
                                              paramsWithDefaults <- liftIO $ injectSmartDefaults transformEnv ir methodDef paramsWithBools
                                              case parseParams ir methodDef paramsWithDefaults of
                                                Right p -> pure p
                                                Left _ | cached -> stale "a flag the cache does not know"
                                                Left errs -> do
                                                  liftIO $ mapM_ (hPutStrLn stderr . renderParseError) errs
                                                  throwParse "Parameter parsing failed"
                                            Nothing | cached -> stale "a method the cache's IR lacks"
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
                                      then if cached then stale "help" else do
                                        -- Render help from IR
                                        case Map.lookup fullPath (irMethods ir) of
                                          Just methodDef -> do
                                            page <- pageFor info pathSegs method
                                            liftIO $ do
                                              TIO.putStr $ renderMethodPage page ir methodDef
                                              backendHelpFooter
                                          Nothing ->
                                            -- Fallback: method not in IR, use basic info
                                            liftIO $ TIO.putStrLn $ T.intercalate "." path <> " - " <> methodDescription method
                                      -- IR-15: fire stderr deprecation warnings before real invocation.
                                      else fireDeprecationWarnings >> keepPlan (Just ir) >> invokeMethod sourceEnv method path params
  where
    -- AUTH-8: what the _info probe at this address answered (view, auth)
    currentInfo :: SynapseM (Maybe BackendInfo)
    currentInfo = do
      ep <- asks seEndpoint
      liftIO (probedInfoAt ep)

    viewSchema = \case
      ViewPlugin schema _ -> Just schema
      _ -> Nothing

    -- A plugin's help: as SYNAPSE-12 laid it out; in a caller's view (AUTH-4)
    -- with who the caller is under the description, and each method's
    -- grant and where it is held when the schema says them.
    --
    -- SYNAPSE-13: in gh's shape — a usage line, one line per method in its
    -- group, and how to see a method's options.
    showPluginHelp :: Maybe BackendInfo -> [Text] -> PluginSchema -> SynapseM ()
    showPluginHelp info segs schema = do
      backend <- asks seBackend
      let invoke = T.unwords ([invokeProgram, backend] ++ segs)
      who <- case info >>= infoView of
        Just Caller -> Session.whoamiM
        _ -> pure Nothing
      let caller = case info >>= infoView of
            Just Caller -> Just (maybe "you are signed in" renderCallerHeader who)
            _ -> Nothing
      liftIO $ do
        TIO.putStr $ renderListing defaultStyle (Listing invoke caller) schema
        synapseHelpLine

    -- SYNAPSE-13: synapse and the global options that reach this backend,
    -- as a command line shown in help (@synapse -P 4455@).
    invokeProgram :: Text
    invokeProgram = T.unwords (filter (not . T.null) ["synapse", cliOptions soEndpoint])

    -- SYNAPSE-13: what a method's page needs beside the IR — how it is
    -- reached, its schema (examples, values, grant) and the caller's view.
    pageFor :: Maybe BackendInfo -> [Text] -> MethodSchema -> SynapseM MethodPage
    pageFor info segs method = do
      backend <- asks seBackend
      pure MethodPage
        { mpProgram = invokeProgram
        , mpTarget = T.unwords (backend : segs)
        , mpSchema = Just method
        , mpCaller = (info >>= infoView) == Just Caller
        }

    -- SYNAPSE-17: search the backend's methods and parameters (the
    -- caller's view of its schema, from the IR cache when its hash is
    -- current) and answer in help's shape; -j: one JSON object per hit.
    runFind :: Find.FindOpts -> SynapseM ()
    runFind fo = do
      backend <- asks seBackend
      cache <- liftIO $ if soNoCache then pure Nothing else
        (Just <$> (newIRCache . defaultCacheConfig =<< getHomeDirectory))
          `E.catch` \(_ :: E.SomeException) -> pure Nothing
      ir <- Find.loadCatalog cache
      tty <- liftIO $ hIsTerminalDevice stdout
      noColor <- liftIO $ lookupEnv "NO_COLOR"
      let ans = Find.answer invokeProgram backend (tty && maybe True null noColor) fo ir
      liftIO $ if not (Find.anMatched ans)
        then do
          if soJson
            then TIO.hPutStrLn stderr ("No match for '" <> T.unwords (Find.foTerms fo) <> "' in " <> Find.anScope ans <> ".")
            else TIO.putStr (Find.anText ans)
          exitWith (ExitFailure 1)
        else if soJson
          then mapM_ (LBS.putStrLn . encode) (Find.anJson ans)
          else TIO.putStr (Find.anText ans)

    -- AUTH-8: the page an anonymous caller sees: the door, and how to sign in.
    showAnonymous :: Maybe BackendInfo -> Maybe PluginSchema -> SynapseM ()
    showAnonymous info root = do
      backend <- asks seBackend
      authSchema <- (Just <$> fetchSchemaAt ["auth"]) `catchError` \_ -> pure Nothing
      let invokePrefix = invokeProgram
          page = AnonPage
            { apBackend = backend
            , apVersion = psVersion <$> root
            , apMethods = anonymousMethods (info >>= infoAuth) authSchema
            , apInvoke = invokePrefix
            }
      liftIO $ TIO.putStr (renderAnonymousPage page)

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

    invokeMethod srcEnv method path params = do
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
      -- SYNAPSE-10: a value read from stdin (-) has taken it; the JSON
      -- bidirectional mode reads stdin too, and the two cannot share
      stdinTaken <- liftIO $ stdinWasRead srcEnv
      case bidirMode of
        BidirJson | stdinTaken -> throwParse "a value was read from stdin (-), so --bidir-mode json cannot read it too; answer the backend's requests with --bidir-cmd, or use a file (@FILE) for the value"
        _ -> pure ()
      -- IDENTITY-43: while the stored session could still be made again,
      -- a "you have signed out" answered before anything else is held back
      -- for 'run' to log in again and retry.
      ep <- asks seEndpoint
      armed <- liftIO $ Session.canReattest ep
      cachedRun <- asks isCached
      let printer item = do
            shown <- Session.printedAny
            held <- readIORef staleRef
            -- SYNAPSE-28: a call sent from the cache that the server refused
            -- as the schema explains is held back, and run again
            if held then pure ()
            else if cachedRun && not shown && isStaleItem item then markStale
            else if armed && not shown && Session.isSessionEndedItem item
              then Session.markSessionEnded (endedMessage item)
              else do
                case item of
                  HubData{} -> Session.notePrinted
                  _ -> pure ()
                printResult soJson soRaw rendererCfg' item
      -- SYNAPSE-20: a stream that resumed after a gap says so — on -j as a
      -- line of its own (stdout, in the stream), else on stderr
      let resumed r
            | soJson = do
                LBS.putStrLn $ encode $ object $
                  [ "type" .= ("reconnected" :: Text)
                  , "outage_secs" .= (fromIntegral (round (Reconnect.rsOutageSecs r) :: Int) :: Int)
                  , "attempts" .= Reconnect.rsAttempts r
                  ] ++ maybe [] (\s -> ["since" .= s]) (Reconnect.rsSince r)
                hFlush stdout
            | otherwise = TIO.hPutStrLn stderr $
                "synapse: the stream resumed"
                  <> maybe "; anything sent during the gap may be missing" (\s -> " from " <> T.pack (show s) <> " (since)") (Reconnect.rsSince r)
      -- SYNAPSE-20: a stream outliving a restart finds its session gone
      -- (menger keeps them in memory): sign in again from the key, and
      -- reconnect with the new one
      backendName' <- asks seBackend
      logger' <- asks seLogger
      let refreshAuth = do
            st <- Store.systemStoreEnv
            new <- Session.reattestAgain logger' st ep
            case new of
              Nothing -> pure Nothing
              Just _ -> do
                env' <- buildEnv ep backendName' logger' soOpts
                pure (Just (mergeUpgradeHeaders (seToken env') (seCookies env') (seHeaders env')))
      -- Use bidirectional streaming handler
      invokeStreamingWithBidir
        (Reconnect.callShape method)
        refreshAuth
        resumed
        namespacePath
        methodName'
        params
        printer
        (handleBidirRequest bidirMode Nothing)

    endedMessage item = case item of
      HubData _ _ _ (Object o) | Just (String m) <- KM.lookup "message" o -> m
      HubError _ _ e _ -> e
      _ -> "your session has ended"

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

-- | SYNAPSE-12: the one line under a backend's help that points at
-- synapse's own (dim on a terminal).
backendHelpFooter :: IO ()
backendHelpFooter = TIO.putStrLn "" >> synapseHelpLine

-- | SYNAPSE-13: the pointer to synapse's own help, dimmed on a terminal —
-- under a listing's own "Run '…'" lines, with no blank between.
synapseHelpLine :: IO ()
synapseHelpLine = do
  tty <- hIsTerminalDevice stdout
  let line = "Run 'synapse --help' for synapse's own options."
  TIO.putStrLn (if tty then "\ESC[2m" <> line <> "\ESC[0m" else line)

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
cliHeader = splash <> T.pack (fst $ renderFailure failure "synapse") <> "\n"
  where
    failure = parserFailure defaultPrefs argsInfo (ShowHelpText Nothing) mempty

-- | The meta-commands, under synapse's own options: in @synapse --help@ and
-- on the page bare @synapse@ shows.
metaCommands :: [Text]
metaCommands =
      [ ""
      , "Values (after the backend and the method):"
      , "  --flag @FILE        the value is that file's contents; --flag - reads stdin"
      , "                      (once, however many values ask); @@text sends a literal"
      , "                      @text. -p @params.json and -p - take the whole object the"
      , "                      same way. No argument then carries the value, so a body"
      , "                      can be as large as the backend accepts"
      , ""
      , "Where the hub is (before the backend):"
      , "  -P PORT / -H HOST   plain ws://HOST:PORT/ (default 127.0.0.1:4444, the registry)"
      , "  --url wss://HOST/ws a hub behind TLS (every hosted app), or ws://HOST:PORT/PATH;"
      , "                      the same as -H HOST --tls --path /ws (443 unless -P). The"
      , "                      certificate is checked against the system's trust store;"
      , "                      --cacert FILE trusts a local CA too. Sessions are kept per"
      , "                      URL, so _login and every call take the same options"
      , ""
      , "A connection held between runs (SYNAPSE-28):"
      , "  the first call to a backend starts a holder that keeps the signed-in socket"
      , "  open ($XDG_RUNTIME_DIR/synapse/, one per address and agent); later calls cost"
      , "  one round trip. It exits after --hold-for (10m) with nothing to do; --no-hold"
      , "  dials a connection of the run's own. A call is sent from the schema cache"
      , "  (~/.cache/synapse/schemas) and checked against the server only when it fails"
      , ""
      , "Meta-commands:"
      , ""
      , "  synapse _key new|show|list [NAME]"
      , "      Ed25519 keys an agent proves who it is with (keychain, else"
      , "      ~/.plexus/keys/NAME.key; PLEXUS_KEY and $CREDENTIALS_DIRECTORY read first)"
      , ""
      , "  synapse -P PORT _login BACKEND"
      , "  synapse -P PORT _login BACKEND --key NAME --as AGENT [--scope ROLE@SCOPE] [--ttl SECS]"
      , "  synapse -P PORT _logout BACKEND [--as AGENT]"
      , "      A person signs in through the browser (a loopback on 127.0.0.1; kept as"
      , "      'me'); an agent logs in from a key. Later calls to that backend carry the"
      , "      session by themselves (--as AGENT / SYNAPSE_AS picks one; else the last"
      , "      logged in), and an agent's logs in again from the key when it ends"
      , ""
      , "  synapse -P PORT _find BACKEND [ACTIVATION...] WORDS... [--limit N] [--methods|--params]"
      , "      Search the backend's methods and parameters: names and descriptions,"
      , "      typos and abbreviations too (setpar finds set_parent), best first, in"
      , "      help's shape (METHODS, PARAMETERS, MENTIONED IN); -j one JSON object per hit"
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
  StaleSchema msg ->
    "The schema cache could not answer: " <> T.unpack msg
  CacheMiss msg ->
    "The schema cache could not answer: " <> T.unpack msg
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
  Parse.MalformedListValue name value reason ->
    "Malformed list value for --" <> T.unpack name <> ": " <> T.unpack value <>
    " (" <> T.unpack reason <> ")"
  Parse.UnexpectedArgument arg ->
    "Unexpected argument: " <> T.unpack arg <>
    "\n\nA value follows its flag (--key VALUE or --key=VALUE); a boolean flag takes none unless it is true or false."


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
printResult json raw cfg item = case itemRefusal item of
  -- AUTH-8: a refusal says what it needed (and the call exits non-zero)
  Just r -> do
    Session.noteRefused
    if json then LBS.putStrLn (encode item) else do
      ceiling' <- Session.currentCeiling
      TIO.hPutStr stderr (renderRefusal ceiling' r)
  Nothing -> printResult' json raw cfg item

printResult' :: Bool -> Bool -> RendererConfig -> HubStreamItem -> IO ()
printResult' True _ _ item = LBS.putStrLn $ encode item
printResult' _ True _ item = case item of
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
printResult' _ _ cfg item = do
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
 <> footerDoc (Just (Pretty.vsep (map (Pretty.pretty . T.unpack) (drop 1 metaCommands))))
  )
  where
    versionOption = infoOption (showVersion Meta.version)
      ( long "version" <> short 'V' <> help "Show version" )

argsParser :: Parser Args
argsParser = Args <$> optsParser <*> backendParser <*> restParser <*> pure Nothing
  where
    backendParser = optional $ T.pack <$> argument str
      ( metavar "BACKEND"
     <> help "Backend name (e.g., plexus, registry-hub)" )

    restParser = many $ T.pack <$> argument str
      ( metavar "PATH... [--param value ...]"
     <> help "Path and method parameters (passed through)" )

optsParser :: Parser SynapseOpts
optsParser = do
  soHostFlag <- optional $ T.pack <$> strOption
    ( long "host" <> short 'H' <> metavar "HOST"
   <> help "Registry/discovery host (default: 127.0.0.1)" )
  soPortFlag <- optional $ option auto
    ( long "port" <> short 'P' <> metavar "PORT"
   <> help "Registry/discovery port (default: 4444; 443 with --tls)" )
  soUrl <- optional $ T.pack <$> strOption
    ( long "url" <> metavar "URL"
   <> help "The hub's socket as a URL: ws://HOST[:PORT][/PATH] or wss://HOST[:PORT][/PATH] (e.g. wss://app.menger.sh/ws); in place of -H/-P/--tls/--path" )
  soTls <- switch
    ( long "tls"
   <> help "Dial wss:// (TLS; the certificate checked against the system's trust store) at -H/-P" )
  soPath <- optional $ T.pack <$> strOption
    ( long "path" <> metavar "PATH"
   <> help "The socket's path at -H/-P (default: /), e.g. /ws" )
  soCaCerts <- many $ T.pack <$> strOption
    ( long "cacert" <> metavar "FILE"
   <> help "With wss: also trust the CA certificate(s) in this PEM file (repeatable; a local CA for a test)" )
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
   <> help "Method parameters as a JSON object, sent as written; @FILE reads it from a file, - from stdin" )
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
  soAs <- optional $ T.pack <$> strOption
    ( long "as" <> metavar "AGENT"
   <> help "Carry this agent's stored session (see _login; else SYNAPSE_AS, else the backend's default agent)" )
  soReconnectFor <- optional $ T.pack <$> strOption
    ( long "reconnect-for" <> metavar "DURATION"
   <> help "Ride out an outage this long: a lost connection is retried, a read sent again, a stream resumed (10m, 90s, 1h; 0 = never; else SYNAPSE_RECONNECT_FOR, else 10m)" )
  soNoHold <- switch
    ( long "no-hold"
   <> help "Hold no connection between runs: this run dials its own (one, shared by all its requests) and closes it (else SYNAPSE_NO_HOLD=1)" )
  soHoldFor <- optional $ T.pack <$> strOption
    ( long "hold-for" <> metavar "DURATION"
   <> help "How long the connection holder keeps the signed-in socket open with nothing to do (10m, 90s, 1h; else SYNAPSE_HOLD_FOR, else 10m)" )
  pure SynapseOpts
    { soHost = maybe defaultHost id soHostFlag
    , soPort = maybe defaultPort id soPortFlag
    , soEndpoint = plainAt (maybe defaultHost id soHostFlag) (maybe defaultPort id soPortFlag)
    , ..
    }

-- | SCAFFOLD-21: the connection point from the options — @--url@, or @-H@
-- / @-P@ with @--tls@ (port 443 unless @-P@) and @--path@; plain
-- @ws://127.0.0.1:4444/@ when none is given. @soHost@ / @soPort@ follow it.
-- A @--cacert@ that cannot be read is refused here, not at the handshake.
resolveConnection :: SynapseOpts -> IO (Either Text SynapseOpts)
resolveConnection o = case pick of
  Left e -> pure (Left e)
  Right ep0 -> do
    let ep = ep0 { epCaFiles = map T.unpack (soCaCerts o) }
    missing <- filterM (fmap not . doesFileExist . T.unpack) (soCaCerts o)
    pure $ case () of
      _ | not (null (soCaCerts o)) && not (epTls ep) ->
            Left "--cacert is for a wss:// address (--url wss://… or --tls)"
        | (f : _) <- missing -> Left ("--cacert " <> f <> ": no such file")
        | otherwise -> Right o { soHost = epHost ep, soPort = epPort ep, soEndpoint = ep }
  where
    pick = case soUrl o of
      Just u
        | isJust (soHostFlag o) || isJust (soPortFlag o) || soTls o || isJust (soPath o) ->
            Left "--url names the whole address: give it alone, or -H / -P with --tls / --path"
        | otherwise -> parseUrl u
      Nothing -> Right Endpoint
        { epHost = maybe defaultHost id (soHostFlag o)
        , epPort = maybe (if soTls o then 443 else defaultPort) id (soPortFlag o)
        , epTls = soTls o
        , epPath = maybe "/" normalisePath (soPath o)
        , epCaFiles = []
        }

-- | Parse a KEY=VALUE pair (used by --cookie/--header). Treats the first '='
-- as the separator; everything before is the key, everything after is the value.
parseKv :: String -> (Text, Text)
parseKv s =
  let (k, rest) = break (== '=') s
      v = case rest of
            '=':vv -> vv
            _      -> ""
  in (T.pack k, T.pack v)

-- | SYNAPSE-CC-8: a refusal's parts, as @-i@ prints them.
irSite, irKeyword, irDetail :: IRT.Refusal -> T.Text
irSite = IRT.rfSite
irKeyword = IRT.rfKeyword
irDetail = IRT.rfDetail
