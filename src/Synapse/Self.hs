{-# LANGUAGE OverloadedStrings #-}

-- | Public surface for the per-backend defaults store.
--
-- Defaults live at @~\/.plexus\/\<backend\>\/defaults.json@ (see 'defaultsPath').
-- The file is a manifest of credential *references* — URIs like
-- @keychain:\/\/svc\/account@ or @literal:raw-value@ — that a resolver
-- registry ('Synapse.Self.Resolve') dereferences at dispatch time.
--
-- This ticket (SELF-1) establishes the types, file format, and resolver
-- API. Actual IO wiring lives in SELF-2 (read), SELF-4 (CLI), SELF-5
-- (write), SELF-7 (core resolvers), SELF-8 (keychain).
module Synapse.Self
  ( -- * File path
    defaultsPath

    -- * On-disk types and encoding
  , StoredDefaults(..)
  , CredentialRef(..)
  , ScopedDefaults(..)
  , OidcDefaults(..)
  , emptyOidcDefaults
  , emptyStoredDefaults
  , currentVersion
  , encodeDefaults
  , decodeDefaults

    -- * Resolver API
  , ParsedUri(..)
  , ParsedUriBody(..)
  , parseUri
  , Resolver(..)
  , ResolveFn
  , NamedResolver(..)
  , ResolverRegistry(..)
  , emptyRegistry
  , registerResolver
  , registerNamedResolver
  , lookupResolver
  , resolveRef
  , ResolveError(..)

    -- * Core resolvers
  , literalResolver
  , envResolver
  , fileResolver
  , keychainResolver
  , storeInKeychain
  , deleteFromKeychain
  , isKeychainPlatform
  , defaultRegistry

    -- * Read path (SELF-2): load, resolve, merge
  , ResolvedDefaults(..)
  , emptyResolvedDefaults
  , MethodPath
  , Cookies
  , Headers
  , loadDefaults
  , resolveAll
  , merge

    -- * Write path (SELF-4 stub; SELF-5 tightens)
  , writeDefaults

    -- * Token sugar (SELF-6)
  , resolveToken

    -- * OIDC login state (UT-3)
  , RefreshOutcome(..)
  , ensureFreshLogin
  ) where

import qualified Control.Exception as E
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.FilePath ((</>))

import Synapse.Self.IO
  ( Cookies
  , Headers
  , MethodPath
  , ResolvedDefaults(..)
  , emptyResolvedDefaults
  , loadDefaults
  , merge
  , resolveAll
  , writeDefaults
  )
import Synapse.Self.Resolve
import Synapse.Self.Resolve.Default (defaultRegistry)
import Synapse.Self.Resolve.Env (envResolver)
import Synapse.Self.Resolve.File (fileResolver)
import Synapse.Self.Resolve.Keychain
  ( deleteFromKeychain
  , isKeychainPlatform
  , keychainResolver
  , storeInKeychain
  )
import Synapse.Self.Oidc (RefreshOutcome(..), ensureFreshLogin)
import Synapse.Self.Resolve.Literal (literalResolver)
import Synapse.Self.Types

-- | Path to a backend\'s defaults file: @~\/.plexus\/\<backend\>\/defaults.json@.
--
-- The returned path is relative to @~@ only in the sense that it begins
-- with the literal string @~\/@ — callers wanting the absolute path should
-- expand via 'System.Directory.getHomeDirectory'. This split lets pure
-- code compute the path without IO; the disk IO wiring (SELF-2 / SELF-5)
-- performs the expansion.
defaultsPath :: Text -> FilePath
defaultsPath backend =
  "~" </> ".plexus" </> T.unpack backend </> "defaults.json"

-- | Resolve the @cookies.access_token@ entry from a backend's defaults
-- store. Returns 'Nothing' when the file is missing, the cookie is
-- absent, or the ref fails to resolve.
--
-- This is the \"sugar\" seam SELF-6 uses to collapse
-- @SynapseCC.Auth.resolveToken@'s final fallback onto 'Synapse.Self'.
-- It is deliberately conservative:
--
--   * IO errors on 'loadDefaults' (permissions, corrupt JSON, etc.)
--     swallow to 'Nothing'. Callers that care can still use
--     'loadDefaults' + 'resolveRef' directly for loud behavior.
--   * Resolve errors swallow to 'Nothing' for the same reason.
--   * An empty resolved value is treated as \"not set\" ('Nothing').
--
-- The loud variant (exit on IO / resolve failure) lives in
-- @buildEnv@ in the synapse executable because that path *must* fail
-- an unauthenticated request rather than silently drop the token.
-- Here — the fallback path, invoked only after explicit CLI / env
-- sources have been exhausted — a soft 'Nothing' is the right default.
-- Transparent refresh (UT-3): before reading the token out, give the
-- OIDC refresh path one chance to heal an expired\/missing access token.
-- Failures stay soft here — the un-refreshed store is used as-is.
resolveToken :: Text -> IO (Maybe Text)
resolveToken backend = do
  eStored <- E.try (loadDefaults backend) :: IO (Either E.IOException StoredDefaults)
  case eStored of
    Left _ -> pure Nothing
    Right stored0 -> do
      eFresh <- E.try (ensureFreshLogin backend stored0)
        :: IO (Either E.IOException (StoredDefaults, RefreshOutcome))
      let stored = either (const stored0) fst eFresh
      case Map.lookup "access_token" (sdCookies stored) of
        Nothing  -> pure Nothing
        Just ref -> do
          r <- resolveRef defaultRegistry ref
          case r of
            Right tok | not (T.null tok) -> pure (Just tok)
            _                            -> pure Nothing
