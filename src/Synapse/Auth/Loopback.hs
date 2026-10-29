{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | AUTH-8: a person signing in from synapse — the loopback flow (AUTH-1,
-- "Settled to start", Q3).
--
-- synapse listens on @127.0.0.1:<a port the OS picks>@, asks the service for a
-- sign-in URL that returns there (@auth.login {flow: {browser: {redirect}}}@),
-- opens it, and waits — at most five minutes — for the browser to come
-- back with @GET /cb?code=…&state=…@. The state must be the one the service
-- handed out with the URL; anything else is refused and ends the wait.
-- The code then goes back to the service (@auth.login {flow: {code: …}}@).
--
-- The parsing and the state check are pure ('parseRequestLine',
-- 'checkCallback') and tested; the socket is only 127.0.0.1, accepts one
-- answer and closes.
module Synapse.Auth.Loopback
  ( -- * The listener
    Loopback
  , loopbackPort
  , loopbackRedirect
  , withLoopback
  , awaitCallback

    -- * Pure parts
  , parseRequestLine
  , checkCallback
  , CallbackResult(..)
  , percentDecode
  , callbackPage
  , durationText

    -- * The browser
  , openBrowser
  ) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad (void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (chr, digitToInt, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import System.Environment (lookupEnv)
import System.Info (os)
import System.Process (createProcess, proc, std_err, std_out, std_in, StdStream(..))
import System.Timeout (timeout)

-- | A listening socket on 127.0.0.1 and the port the OS gave it.
data Loopback = Loopback
  { lbSocket :: N.Socket
  , lbPort   :: Int
  }

loopbackPort :: Loopback -> Int
loopbackPort = lbPort

-- | Where the service sends the browser back: @http://127.0.0.1:<port>/cb@.
loopbackRedirect :: Loopback -> Text
loopbackRedirect lb = "http://127.0.0.1:" <> T.pack (show (lbPort lb)) <> "/cb"

-- | Listen on 127.0.0.1 (never another interface), port 0, for the
-- duration of the action.
withLoopback :: (Loopback -> IO a) -> IO a
withLoopback act = bracket open (N.close . lbSocket) act
  where
    open = do
      sock <- N.socket N.AF_INET N.Stream N.defaultProtocol
      N.setSocketOption sock N.ReuseAddr 1
      N.bind sock (N.SockAddrInet 0 (N.tupleToHostAddress (127, 0, 0, 1)))
      N.listen sock 4
      port <- N.socketPort sock
      pure (Loopback sock (fromIntegral port))

-- | What one request to the loopback came to.
data CallbackResult
  = CallbackCode Text        -- ^ @/cb@ with the state expected: the code
  | CallbackRefused Text     -- ^ @/cb@ that cannot be accepted: why (ends the wait)
  | CallbackIgnored          -- ^ anything else (@/favicon.ico@): answered 404, keep waiting
  deriving (Eq, Show)

-- | The method, path and query of an HTTP request line:
-- @GET /cb?code=c_1&state=s_2 HTTP/1.1@ → @("GET", "/cb", [("code","c_1"),("state","s_2")])@.
parseRequestLine :: BS.ByteString -> Maybe (Text, Text, [(Text, Text)])
parseRequestLine raw = case BS8.words (BS8.takeWhile (\c -> c /= '\r' && c /= '\n') raw) of
  (method : target : _) ->
    let (path, query) = BS8.break (== '?') target
        params = [ (dec k, dec (BS.drop 1 v))
                 | kv <- BS8.split '&' (BS.drop 1 query), not (BS.null kv)
                 , let (k, v) = BS8.break (== '=') kv ]
    in Just (utf8 method, utf8 path, params)
  _ -> Nothing
  where
    dec = percentDecode . utf8
    utf8 = TE.decodeUtf8With TEE.lenientDecode

-- | Decide a request: @GET /cb@ carrying the expected state and a code is
-- the code; @/cb@ with a provider's @error@, a missing code, or another
-- state is refused (a mismatched state is never a code); any other path
-- is ignored.
checkCallback :: Text -> (Text, Text, [(Text, Text)]) -> CallbackResult
checkCallback expected (_method, path, params)
  | path /= "/cb" = CallbackIgnored
  | Just e <- lookup "error" params =
      CallbackRefused ("the sign-in was not completed: " <> e
        <> maybe "" (" \x2014 " <>) (lookup "error_description" params))
  | lookup "state" params /= Just expected =
      CallbackRefused "the browser came back with a state this sign-in did not ask for; refused"
  | otherwise = case lookup "code" params of
      Just c | not (T.null c) -> CallbackCode c
      _ -> CallbackRefused "the browser came back without a code"

-- | @%2F@ → @/@, @+@ → space.
percentDecode :: Text -> Text
percentDecode = T.pack . go . T.unpack
  where
    go ('%' : a : b : rest) | isHexDigit a && isHexDigit b = chr (digitToInt a * 16 + digitToInt b) : go rest
    go ('+' : rest) = ' ' : go rest
    go (c : rest) = c : go rest
    go [] = []

-- | @300@ → @5 minutes@; @90@ → @90 seconds@.
durationText :: Int -> Text
durationText secs
  | secs >= 60 && secs `mod` 60 == 0 = T.pack (show (secs `div` 60)) <> " minutes"
  | secs == 1 = "1 second"
  | otherwise = T.pack (show secs) <> " seconds"

-- | The small page the browser shows once synapse has the answer.
callbackPage :: Bool -> Text -> BS.ByteString
callbackPage ok msg = response
  where
    status = if ok then "200 OK" else "400 Bad Request"
    title = if ok then "Signed in" else "Not signed in"
    body = TE.encodeUtf8 $ T.concat
      [ "<!doctype html><meta charset=utf-8><title>", title, "</title>"
      , "<body style=\"font:16px system-ui,sans-serif;margin:3em\"><h1>", title, "</h1><p>"
      , escape msg, "</p></body>" ]
    response = BS.concat
      [ "HTTP/1.1 ", status, "\r\nContent-Type: text/html; charset=utf-8\r\n"
      , "Content-Length: ", BS8.pack (show (BS.length body)), "\r\nConnection: close\r\n"
      , "Cache-Control: no-store\r\n\r\n", body ]
    escape = T.concatMap (\case '<' -> "&lt;"; '>' -> "&gt;"; '&' -> "&amp;"; '"' -> "&quot;"; c -> T.singleton c)

-- | Wait (up to @secs@) for the browser to come back with the state
-- expected. The code, or why there is none.
awaitCallback :: Loopback -> Text -> Int -> IO (Either Text Text)
awaitCallback lb expected secs = do
  r <- timeout (secs * 1000000) loop
  pure $ case r of
    Nothing -> Left ("no answer from the browser within " <> durationText secs)
    Just x -> x
  where
    loop = do
      (conn, _) <- N.accept (lbSocket lb)
      req <- readHead conn
      let decided = maybe CallbackIgnored (checkCallback expected) (parseRequestLine req)
      case decided of
        CallbackIgnored -> do
          send conn "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
          N.close conn
          loop
        CallbackRefused why -> do
          send conn (callbackPage False why)
          N.close conn
          pure (Left why)
        CallbackCode code -> do
          send conn (callbackPage True "You are signed in to synapse. You can close this tab.")
          N.close conn
          pure (Right code)
    send conn bytes = void (try (NB.sendAll conn bytes) :: IO (Either SomeException ()))
    -- the request line is all that is read; enough of the head to hold it
    readHead conn = go BS.empty
      where
        go acc
          | "\r\n" `BS.isInfixOf` acc || BS.length acc > 16384 = pure acc
          | otherwise = do
              chunk <- try (NB.recv conn 4096) :: IO (Either SomeException BS.ByteString)
              case chunk of
                Right c | not (BS.null c) -> go (acc <> c)
                _ -> pure acc

-- | Open the URL in the person's browser (@xdg-open@; @open@ on macOS),
-- detached and silent. @SYNAPSE_NO_BROWSER=1@ opens nothing (the URL is
-- printed either way). True when a browser was asked.
openBrowser :: Text -> IO Bool
openBrowser url = do
  off <- lookupEnv "SYNAPSE_NO_BROWSER"
  if maybe False (`elem` ["1", "true", "yes"]) off then pure False else do
    let opener = if os == "darwin" then "open" else "xdg-open"
    r <- try (void (createProcess (proc opener [T.unpack url])
                { std_in = NoStream, std_out = NoStream, std_err = NoStream }))
           :: IO (Either SomeException ())
    pure (either (const False) (const True) r)
