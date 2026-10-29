{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | AUTH-8: what synapse shows of a service's auth — pure, so every render is
-- tested from fixtures.
--
-- * The anonymous page: a service whose @_info@ says @view: anonymous@ shows
--   only its door (login, signup, providers) and how to sign in from here.
-- * The caller's header: @you are NAME (PRINCIPAL) · ROLE on SCOPES, within
--   CEILING@, from @auth.whoami@ (or, on a service without it, @session.whoami@).
-- * A refusal: @refused: needs GRANT on SCOPE@ and what would allow it.
--
-- Every wire field read here is optional (AUTH-1, "Login off stays open"):
-- a service without a gate answers none of it, and nothing here is shown.
module Synapse.Auth
  ( -- * Who the caller is
    Whoami(..)
  , parseWhoami
  , renderCallerHeader
  , abbreviatePrincipal
  , scopeOf

    -- * The anonymous page
  , AnonPage(..)
  , anonymousMethods
  , renderAnonymousPage

    -- * Refusals
  , renderRefusal

    -- * The session auth.login answers
  , SessionGrant(..)
  , parseSessionGrant
  , isMissingAuth
  ) where

import Control.Applicative ((<|>))
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Plexus.Auth (AuthInfo(..), Refusal(..))
import Plexus.Schema.Recursive (MethodSchema(..), PluginSchema(..))

-- ============================================================================
-- Who the caller is
-- ============================================================================

-- | What @auth.whoami@ (or @session.whoami@'s @identity@) says.
data Whoami = Whoami
  { whoPrincipal   :: Text
  , whoName        :: Maybe Text
  , whoAgent       :: Maybe Text
  , whoMemberships :: [(Text, Text)]  -- ^ (role, scope); scope "" when the service names none
  , whoCeiling     :: [Text]          -- ^ as the service says it: @editor\@MENGER-12@ or a scope
  } deriving (Eq, Show)

-- | A whoami from a data item's content. Shapes read (all fields but the
-- principal optional):
--
-- @
-- {"type":"whoami","principal":"key:ac79…","name":"worker-1","agent":"worker-1",
--  "memberships":[{"role":"editor","scope":"MENGER"}],"ceiling":["editor\@MENGER-12"]}
-- {"type":"identity","principal":"idp:…","name":"ada","roles":["admin"]}     -- session.whoami
-- @
--
-- @memberships@ (or @roles@) may also be strings, @role\@SCOPE@ or a bare
-- role; @ceiling@ strings or @{role|grant, scope}@ objects. An anonymous
-- answer (@type: anonymous@, or no principal) is Nothing.
parseWhoami :: Value -> Maybe Whoami
parseWhoami = \case
  Object o
    | Just (String "anonymous") <- KM.lookup "type" o -> Nothing
    | Just (String p) <- KM.lookup "principal" o, not (T.null p) -> Just Whoami
        { whoPrincipal = p
        , whoName = nonEmpty (str "name" o)
        , whoAgent = nonEmpty (str "agent" o)
        , whoMemberships = memberships o
        , whoCeiling = ceilingOf o
        }
    -- a whoami nested one level (e.g. {"type":"whoami","identity":{…}})
    | Just inner@(Object _) <- KM.lookup "identity" o -> parseWhoami inner
  _ -> Nothing
  where
    memberships o = case KM.lookup "memberships" o of
      Just v -> roleScopes v
      Nothing -> maybe [] roleScopes (KM.lookup "roles" o)
    roleScopes = \case
      Array xs -> mapMaybe one (V.toList xs)
      _ -> []
    one = \case
      String s -> Just (splitRoleScope s)
      Object e -> case (str "role" e, str "scope" e) of
        ("", _) -> Nothing
        (r, s) -> Just (r, s)
      _ -> Nothing

ceilingOf :: KM.KeyMap Value -> [Text]
ceilingOf o = case KM.lookup "ceiling" o <|> KM.lookup "scope" o of
  Just (Array xs) -> mapMaybe one (V.toList xs)
  Just (String s) | not (T.null s) -> [s]
  _ -> []
  where
    one = \case
      String s -> Just s
      Object e ->
        let what = case (str "role" e, str "grant" e) of
              (r, _) | not (T.null r) -> r
              (_, g) -> g
            sc = str "scope" e
        in if T.null what && T.null sc then Nothing
           else Just (if T.null what then sc else if T.null sc then what else what <> "@" <> sc)
      _ -> Nothing

-- | @editor\@MENGER@ → (editor, MENGER); @admin@ → (admin, "").
splitRoleScope :: Text -> (Text, Text)
splitRoleScope s = case T.breakOn "@" s of
  (r, rest) | not (T.null rest) -> (r, T.drop 1 rest)
  _ -> (s, "")

-- | The scope of a ceiling entry: @editor\@MENGER-12@ → @MENGER-12@; an
-- entry with no @\@@ is its own scope.
scopeOf :: Text -> Text
scopeOf s = case T.breakOn "@" s of
  (_, rest) | not (T.null rest) -> T.drop 1 rest
  _ -> s

-- | @you are worker-1 (key:ac79…) · editor on MENGER, within MENGER-12@.
--
-- Several roles: each @ROLE on SCOPES@, joined with @; @. A membership the
-- service named no scope for is the role alone. No memberships: the name and
-- principal alone.
renderCallerHeader :: Whoami -> Text
renderCallerHeader w = "you are " <> who <> held <> within
  where
    shown = abbreviatePrincipal (whoPrincipal w)
    who = case whoName w <|> whoAgent w of
      Just n | n /= whoPrincipal w -> n <> " (" <> shown <> ")"
      _ -> shown
    roles = nub (map fst (whoMemberships w))
    onScopes r = case filter (not . T.null) [s | (r', s) <- whoMemberships w, r' == r] of
      [] -> r
      ss -> r <> " on " <> T.intercalate ", " (nub ss)
    held = if null roles then "" else " \x00B7 " <> T.intercalate "; " (map onScopes roles)
    within = case nub (map scopeOf (whoCeiling w)) of
      [] -> ""
      cs -> ", within " <> T.intercalate ", " cs

-- | A principal shortened for a one-line header: the part after its last
-- @:@ or @/@ kept to its leading word-and-underscore and four more
-- characters — @key:ac79e0…@ → @key:ac79…@, @oidc:workos/user_01M4QX…@ →
-- @oidc:workos/user_01M4…@. A short one is shown whole.
abbreviatePrincipal :: Text -> Text
abbreviatePrincipal p
  | T.length ident <= 12 = p
  | otherwise = prefix <> T.take keep ident <> "\x2026"
  where
    (prefix, ident) = splitIdent p
    lead = case T.breakOn "_" ident of
      (w, rest) | not (T.null rest), T.length w <= 8 -> T.length w + 1
      _ -> 0
    keep = lead + 4
    splitIdent t =
      let i = maximum (0 : [n + 1 | (n, c) <- zip [0 ..] (T.unpack t), c == ':' || c == '/'])
      in T.splitAt i t

-- ============================================================================
-- The anonymous page
-- ============================================================================

data AnonPage = AnonPage
  { apBackend :: Text              -- ^ the service's name
  , apVersion :: Maybe Text        -- ^ the root schema's version, when the view shows it
  , apMethods :: [(Text, Text)]    -- ^ (method, description) under @auth@
  , apInvoke  :: Text              -- ^ how this synapse was pointed: @synapse -P 4455@
  } deriving (Eq, Show)

-- | The door's methods, each with its description: the ones @_info@'s @auth@
-- block names (@auth.login@ → @login@), else the auth schema's public
-- ones, else login, signup and providers; described by the auth schema
-- when it says, else in synapse's words. login, signup, providers first,
-- in that order; any others after, by name.
anonymousMethods :: Maybe AuthInfo -> Maybe PluginSchema -> [(Text, Text)]
anonymousMethods info authSchema = sortOn (rank . fst) [ (m, describe m) | m <- names ]
  where
    fromInfo = maybe [] (map (\m -> fromMaybe m (T.stripPrefix "auth." m)) . authMethods) info
    fromSchema = maybe [] (map methodName . filter methodPublic . psMethods) authSchema
    names = nub $ case (fromInfo, fromSchema) of
      ([], []) -> ["login", "signup", "providers"]
      ([], s) -> s
      (i, _) -> i
    described = maybe [] (\s -> [(methodName m, methodDescription m) | m <- psMethods s]) authSchema
    describe m = case lookup m described of
      Just d | not (T.null d) -> d
      _ -> fromMaybe "" (lookup m ownWords)
    ownWords =
      [ ("login", "Sign in \x2014 a person through the browser, an agent with its key")
      , ("signup", "Create an account, or ask for access")
      , ("providers", "The ways in this server offers")
      ]
    rank m = (fromMaybe 3 (lookup m (zip ["login", "signup", "providers"] [0 :: Int ..])), m)

-- | The page an anonymous caller sees for a service (AUTH-8):
--
-- @
-- menger v1.0.0
--   Sign in to see what you can do here.
--
-- auth
--   login      Sign in — a person through the browser, an agent with its key
--   signup     Create an account, or ask for access
--   providers  The ways in this server offers
--
--   synapse -P 4455 _login menger                          # a person
--   synapse -P 4455 _login menger --key agent-7 --as w-1   # an agent
-- @
renderAnonymousPage :: AnonPage -> Text
renderAnonymousPage AnonPage{..} = T.unlines $
  [ apBackend <> maybe "" (\v -> if T.null v then "" else " v" <> v) apVersion
  , "  Sign in to see what you can do here."
  , ""
  , "auth"
  ]
  <> [ "  " <> T.justifyLeft nameW ' ' m <> (if T.null d then "" else "  " <> d) | (m, d) <- apMethods ]
  <> [ "" ]
  <> [ "  " <> T.justifyLeft exW ' ' ex <> "   # " <> who | (ex, who) <- examples ]
  where
    nameW = maximum (0 : map (T.length . fst) apMethods)
    examples =
      [ (apInvoke <> " _login " <> apBackend, "a person")
      , (apInvoke <> " _login " <> apBackend <> " --key agent-7 --as w-1", "an agent")
      ]
    exW = maximum (map (T.length . fst) examples)

-- ============================================================================
-- Refusals
-- ============================================================================

-- | A refusal, as a block for stderr:
--
-- @
-- refused: needs tickets.set_status on IDENTITY
--   roles with it   editor, admin
--   you hold it on  MENGER (within MENGER-12)
-- @
--
-- The ceiling is the refusal's own when the service names one, else the one
-- passed in (the stored session's, as asked at login). A line with
-- nothing to say is left out; a refusal naming no grant is its message.
renderRefusal :: [Text] -> Refusal -> Text
renderRefusal fallbackCeiling r = T.unlines (headline : details)
  where
    headline = case (refusalGrant r, refusalScope r) of
      (Just g, Just s) -> "refused: needs " <> g <> " on " <> s
      (Just g, Nothing) -> "refused: needs " <> g
      _ -> "refused: " <> refusalMessage r
    ceilingScopes = nub $ map scopeOf $
      if null (refusalCeiling r) then fallbackCeiling else refusalCeiling r
    within = if null ceilingScopes then "" else " (within " <> T.intercalate ", " ceilingScopes <> ")"
    details =
      [ "  roles with it   " <> T.intercalate ", " (refusalRoles r) | not (null (refusalRoles r)) ]
      <> [ "  you hold it on  " <> heldText ]
    heldText = case refusalHeldOn r of
      [] -> "nowhere" <> within
      hs -> T.intercalate ", " hs <> within

-- ============================================================================
-- The session auth.login answers
-- ============================================================================

-- | @{type: session, token, principal, name?, agent?, expires_at?, ceiling?}@
-- (AUTH-5), or IDENTITY-42's @{type: login, …, scope}@ from session.login.
data SessionGrant = SessionGrant
  { sgToken     :: Text
  , sgPrincipal :: Text
  , sgName      :: Maybe Text
  , sgAgent     :: Maybe Text
  , sgExpiresAt :: Maybe Integer
  , sgCeiling   :: [Text]
  , sgWhoami    :: Maybe Whoami   -- ^ when the answer carries memberships
  } deriving (Eq, Show)

-- | The session in a data item's content, when it is one.
parseSessionGrant :: Value -> Maybe SessionGrant
parseSessionGrant = \case
  Object o
    | Just (String t) <- KM.lookup "type" o, t `elem` ["session", "login"]
    , Just (String tok) <- KM.lookup "token" o, not (T.null tok) -> Just SessionGrant
        { sgToken = tok
        , sgPrincipal = str "principal" o
        , sgName = nonEmpty (str "name" o)
        , sgAgent = nonEmpty (str "agent" o)
        , sgExpiresAt = KM.lookup "expires_at" o >>= int
        , sgCeiling = ceilingOf o
        , sgWhoami = case parseWhoami (Object o) of
            Just w | not (null (whoMemberships w)) -> Just w
            _ -> Nothing
        }
  _ -> Nothing

-- | Is this error the service saying it has no @auth@ activation (or no such
-- method on it)? Then synapse falls back to IDENTITY-42's @session.*@.
isMissingAuth :: Text -> Bool
isMissingAuth m = any (`T.isInfixOf` T.toLower m)
  [ "activation not found", "method not found", "unknown method", "no such method"
  , "unknown activation", "not found: auth", "-32601", "activationnotfound", "methodnotfound" ]

-- ============================================================================
-- Helpers
-- ============================================================================

str :: Text -> KM.KeyMap Value -> Text
str k o = case KM.lookup (K.fromText k) o of
  Just (String s) -> s
  _ -> ""

nonEmpty :: Text -> Maybe Text
nonEmpty t = if T.null t then Nothing else Just t

int :: Value -> Maybe Integer
int = \case
  Number n -> fromIntegral <$> (toBoundedInteger n :: Maybe Int)
  String s | [(i, "")] <- reads (T.unpack s) -> Just i
  _ -> Nothing
