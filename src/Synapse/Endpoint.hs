-- | SCAFFOLD-21: where a service is reached — a host and port, and, for a service
-- behind TLS (every hosted app: @wss://<app>.menger.sh/ws@), the scheme
-- and the path its socket answers at.
--
-- One value, from the command line (@--url@, or @-H@ / @-P@ with @--tls@
-- and @--path@), carried to every dial: the probe, the call, the stream and
-- its reconnects, the schema, a bidirectional response, @_login@ and the
-- session a call carries. The store keys a session by 'addressKey': a
-- plain @ws://HOST:PORT/@ keeps the @HOST:PORT@ it always had (no stored
-- session moves), anything else is keyed by its whole URL, so a @ws@ and a
-- @wss@ backend on one host, or two paths, never share a session.
module Synapse.Endpoint
  ( Endpoint(..)
  , plainAt
  , isPlain
  , parseUrl
  , normalisePath
  , renderUrl
  , shortUrl
  , addressKey
  , dirName
  , audience
  , cliFlags
  , cliOptions
  , defaultHost
  , defaultPort
  , substrateConfig
  ) where

import qualified Data.ByteString as BS
import Data.Char (isAlphaNum, isDigit, ord, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.WebSockets as WS
import Numeric (showHex)

import Plexus.Client (SubstrateConfig(..), TlsOptions(..))

-- | A service's address.
data Endpoint = Endpoint
  { epHost    :: !Text
  , epPort    :: !Int
  , epTls     :: !Bool          -- ^ @wss://@: TLS, the certificate verified
  , epPath    :: !Text          -- ^ the socket's path, @/@ when none is given
  , epCaFiles :: ![FilePath]    -- ^ PEM files trusted beside the system's (@--cacert@)
  }
  deriving stock (Eq, Ord, Show)

-- | @ws://HOST:PORT/@ — what @-H@ / @-P@ have always named.
plainAt :: Text -> Int -> Endpoint
plainAt h p = Endpoint { epHost = h, epPort = p, epTls = False, epPath = "/", epCaFiles = [] }

-- | Is this the plain @ws://HOST:PORT/@ every address was before
-- SCAFFOLD-21? (Then everything keyed by it is keyed as it was.)
isPlain :: Endpoint -> Bool
isPlain ep = not (epTls ep) && epPath ep == "/"

-- | Parse @ws://HOST[:PORT][/PATH]@ or @wss://HOST[:PORT][/PATH]@. The
-- port defaults to the scheme's (80, 443); the path to @/@. An IPv6
-- literal is bracketed (@wss://[::1]:8443/ws@). A query or fragment is
-- refused: a Plexus socket is named by its path alone.
parseUrl :: Text -> Either Text Endpoint
parseUrl raw0 = do
  let raw = T.strip raw0
  (tls, rest) <- case T.breakOn "://" raw of
    (scheme, r) | not (T.null r) -> case T.toLower scheme of
      "ws"    -> Right (False, T.drop 3 r)
      "wss"   -> Right (True, T.drop 3 r)
      "http"  -> Left ("--url takes ws:// or wss://, not http:// (the socket of an http page is ws://" <> T.drop 3 r <> ")")
      "https" -> Left ("--url takes ws:// or wss://, not https:// (the socket of an https page is wss://" <> T.drop 3 r <> ")")
      other   -> Left ("--url takes ws:// or wss://, not " <> other <> "://")
    _ -> Left ("--url needs a scheme: ws://" <> raw <> " or wss://" <> raw)
  let (authority, pathPart) = T.breakOn "/" rest
  if T.any (`elem` ['?', '#']) pathPart || T.any (`elem` ['?', '#']) authority
    then Left "--url takes no query or fragment: a socket is named by its path"
    else Right ()
  if T.any (== '@') authority
    then Left "--url takes no user:password@ — a credential goes in --token-file or the stored session"
    else Right ()
  (host, port) <- splitAuthority (if tls then 443 else 80) authority
  pure Endpoint
    { epHost = host
    , epPort = port
    , epTls = tls
    , epPath = normalisePath pathPart
    , epCaFiles = []
    }

-- | @HOST[:PORT]@ or @[V6][:PORT]@.
splitAuthority :: Int -> Text -> Either Text (Text, Int)
splitAuthority dflt a
  | T.null a = Left "--url names no host"
  | Just inner <- T.stripPrefix "[" a =
      case T.breakOn "]" inner of
        (h, r) | not (T.null r), not (T.null h) -> case T.drop 1 r of
          "" -> Right (h, dflt)
          p | Just digits <- T.stripPrefix ":" p -> (,) h <$> port digits
          _ -> Left ("--url: cannot read the host " <> a)
        _ -> Left ("--url: cannot read the host " <> a)
  | otherwise = case T.breakOnEnd ":" a of
      ("", _) -> Right (T.toLower a, dflt)
      (h, p) -> (,) (T.toLower (T.dropEnd 1 h)) <$> port p
  where
    port digits
      | not (T.null digits), T.all isDigit digits, T.length digits <= 5
      , n <- read (T.unpack digits), n >= 1, n <= 65535 = Right n
      | otherwise = Left ("--url: the port '" <> digits <> "' is not a port")

-- | A path as a socket is named by it: a leading @/@, no trailing one
-- (@ws/@ → @/ws@; empty → @/@).
normalisePath :: Text -> Text
normalisePath p =
  let trimmed = T.dropWhileEnd (== '/') (T.dropWhile (== '/') (T.strip p))
  in "/" <> trimmed

-- | The URL, with the port always written (@wss://host:443/ws@) — the form
-- the store keys by.
renderUrl :: Endpoint -> Text
renderUrl ep = scheme ep <> "://" <> hostPart (epHost ep) <> ":" <> T.pack (show (epPort ep)) <> epPath ep

scheme :: Endpoint -> Text
scheme ep = if epTls ep then "wss" else "ws"

hostPart :: Text -> Text
hostPart h = if T.any (== ':') h then "[" <> h <> "]" else h

-- | What a session is kept under: @HOST:PORT@ for a plain address (as
-- every session before SCAFFOLD-21 was), the whole URL otherwise.
addressKey :: Endpoint -> Text
addressKey ep
  | isPlain ep = epHost ep <> ":" <> T.pack (show (epPort ep))
  | otherwise = renderUrl ep

-- | The directory name under @~/.plexus/sessions/@: @HOST_PORT@ for a
-- plain address (unchanged), else @wss_HOST_PORT@ (or @ws_…@) and the path
-- with each @/@ as @_@ — any other byte outside @[A-Za-z0-9.-]@ written
-- @%XX@, so two addresses never share one.
dirName :: Endpoint -> FilePath
dirName ep
  | isPlain ep = T.unpack (epHost ep) <> "_" <> show (epPort ep)
  | otherwise =
      T.unpack (scheme ep) <> "_" <> escape (T.unpack (epHost ep)) <> "_" <> show (epPort ep)
        <> concatMap pathChar (T.unpack (T.dropWhileEnd (== '/') (epPath ep)))
  where
    escape = concatMap safe
    safe c | isAlphaNum c && ord c < 128 || c `elem` ['.', '-'] = [c]
           | otherwise = pct c
    pathChar '/' = "_"
    pathChar c = safe c
    pct c = concatMap (\b -> '%' : hex2 b) (BS.unpack (TE.encodeUtf8 (T.singleton c)))
    hex2 b = let h = map toLower (showHex b "") in if length h < 2 then '0' : h else h

-- | A key proof's audience (IDENTITY-40's @aud@): the server's socket
-- origin, as the server lists the URLs it is dialed at — @ws://HOST:PORT@
-- for a plain address (unchanged), @wss://HOST@ (@:PORT@ when not 443)
-- behind TLS, which is what a service run with @--public-url https://HOST@
-- accepts (plexus-login's @socket_urls@). The path is not part of it.
audience :: Endpoint -> Text
audience ep
  | epTls ep = "wss://" <> hostPart (epHost ep) <> (if epPort ep == 443 then "" else ":" <> T.pack (show (epPort ep)))
  | otherwise = "ws://" <> epHost ep <> ":" <> T.pack (show (epPort ep))

-- | The global options that reach this address, as synapse's own help and
-- messages write them: @-P 4455@, @-H host -P 4455@, @--url wss://host/ws@
-- (and @--cacert FILE@ for each local CA). Empty for the default address.
cliFlags :: Text -> Int -> Endpoint -> Text
cliFlags defaultHost defaultPort ep
  | isPlain ep = T.unwords $
      (if epHost ep /= defaultHost then ["-H", epHost ep] else [])
        <> (if epPort ep /= defaultPort then ["-P", T.pack (show (epPort ep))] else [])
  | otherwise = T.unwords $ ["--url", shortUrl ep] <> concatMap (\f -> ["--cacert", T.pack f]) (epCaFiles ep)

-- | The URL as a person writes it: the scheme's default port and a bare
-- @/@ left out (@wss://hostdemo.menger.sh/ws@).
shortUrl :: Endpoint -> Text
shortUrl ep = scheme ep <> "://" <> hostPart (epHost ep)
  <> (if epPort ep == dflt then "" else ":" <> T.pack (show (epPort ep)))
  <> (if epPath ep == "/" then "" else epPath ep)
  where dflt = if epTls ep then 443 else 80

-- | The connection point when none is named: the registry's, @ws://127.0.0.1:4444/@.
defaultHost :: Text
defaultHost = "127.0.0.1"

defaultPort :: Int
defaultPort = 4444

-- | 'cliFlags' against the default address — what a message tells the
-- reader to type (@synapse -P 4455 _login …@, @synapse --url wss://h/ws _login …@).
cliOptions :: Endpoint -> Text
cliOptions = cliFlags defaultHost defaultPort

-- | The plexus-protocol config that dials this address for @backend@,
-- carrying @headers@ on the upgrade.
substrateConfig :: Endpoint -> Text -> WS.Headers -> SubstrateConfig
substrateConfig ep backend headers = SubstrateConfig
  { substrateHost    = T.unpack (epHost ep)
  , substratePort    = epPort ep
  , substratePath    = T.unpack (epPath ep)
  , substrateBackend = backend
  , substrateHeaders = headers
  , substrateTls     = if epTls ep then Just (TlsOptions (epCaFiles ep)) else Nothing
  }
