{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Recursion schemes for schema trees
--
-- This module provides proper categorical recursion schemes:
--
-- = Anamorphism (unfold)
--
-- An anamorphism builds a recursive structure from a seed using a coalgebra:
--
-- @
-- coalgebra :: a -> f a           -- one step of unfolding
-- ana :: Functor f => (a -> f a) -> a -> Fix f
-- ana coalg = Fix . fmap (ana coalg) . coalg
-- @
--
-- For effectful unfolding, we use 'anaM':
--
-- @
-- anaM :: (Monad m, Traversable f) => (a -> m (f a)) -> a -> m (Fix f)
-- @
--
-- = Catamorphism (fold)
--
-- A catamorphism collapses a recursive structure using an algebra:
--
-- @
-- algebra :: f a -> a             -- one step of folding
-- cata :: Functor f => (f a -> a) -> Fix f -> a
-- cata alg = alg . fmap (cata alg) . unFix
-- @
--
-- = Hylomorphism (unfold then fold)
--
-- A hylomorphism composes an anamorphism with a catamorphism:
--
-- @
-- hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
-- hylo alg coalg = cata alg . ana coalg
--                = alg . fmap (hylo alg coalg) . coalg  -- fused
-- @
--
-- The fused version never builds the intermediate structure!
module Synapse.Algebra.Recursion
  ( -- * Pure recursion schemes
    cata
  , ana
  , hylo
  , para
  , apo

    -- * Monadic recursion schemes
  , cataM
  , anaM
  , hyloM
  , hyloMPar

    -- * Schema-specific operations
  , unfoldSchema
  , foldSchema
  , walkSchema
  , walkSchemaPar

    -- * Re-exports
  , Fix(..)
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import System.IO (hPutStrLn, stderr)

import Synapse.Schema.Types (Path, PluginSchema(..), MethodSchema(..), ChildSummary(..))
import Synapse.Schema.Functor (SchemaF(..), Fix(..), SchemaTree)
import Synapse.Monad (SynapseM, SynapseError(..), runSynapseM)
import Control.Monad.Except (throwError)
import Synapse.Transport (fetchSchemaAt)

-- ============================================================================
-- Pure Recursion Schemes
-- ============================================================================

-- | Catamorphism: fold a recursive structure
--
-- @
-- cata alg = alg . fmap (cata alg) . unFix
-- @
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = go
  where
    go (Fix fa) = alg (fmap go fa)

-- | Anamorphism: unfold to build a recursive structure
--
-- @
-- ana coalg = Fix . fmap (ana coalg) . coalg
-- @
ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = go
  where
    go a = Fix (fmap go (coalg a))

-- | Hylomorphism: unfold then fold (fused - no intermediate structure)
--
-- @
-- hylo alg coalg = cata alg . ana coalg  -- unfused
--                = alg . fmap (hylo alg coalg) . coalg  -- fused
-- @
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = go
  where
    go a = alg (fmap go (coalg a))

-- | Paramorphism: fold with access to original substructures
--
-- Like cata but the algebra also receives the original subtree
para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para alg = go
  where
    go (Fix fa) = alg (fmap (\x -> (x, go x)) fa)

-- | Apomorphism: unfold with early termination
--
-- Like ana but can short-circuit by returning Right with final value
apo :: Functor f => (a -> f (Either (Fix f) a)) -> a -> Fix f
apo coalg = go
  where
    go a = Fix (fmap (either id go) (coalg a))

-- ============================================================================
-- Monadic Recursion Schemes
-- ============================================================================

-- | Monadic catamorphism
--
-- Fold with effects at each step
cataM :: (Monad m, Traversable f) => (f a -> m a) -> Fix f -> m a
cataM alg = go
  where
    go (Fix fa) = do
      fa' <- traverse go fa  -- recursively process children
      alg fa'                 -- apply algebra to results

-- | Monadic anamorphism
--
-- Unfold with effects at each step. This is what we need for
-- schema tree building since fetching is effectful.
anaM :: (Monad m, Traversable f) => (a -> m (f a)) -> a -> m (Fix f)
anaM coalg = go
  where
    go a = do
      fa <- coalg a           -- one step of unfolding (effectful)
      fa' <- traverse go fa   -- recursively unfold children
      pure (Fix fa')

-- | Monadic hylomorphism
--
-- Unfold then fold, both with effects. Fused version.
hyloM :: (Monad m, Traversable f) => (f b -> m b) -> (a -> m (f a)) -> a -> m b
hyloM alg coalg = go
  where
    go a = do
      fa <- coalg a           -- unfold one step
      fb <- traverse go fa    -- recursively process (sequential)
      alg fb                  -- fold one step

-- | SYNAPSE-28: the cache has not got it, or misled the call.
cacheCannotAnswer :: SynapseError -> Bool
cacheCannotAnswer = \case
  CacheMiss _ -> True
  StaleSchema _ -> True
  _ -> False

-- | Parallel monadic hylomorphism specialized for SchemaF
--
-- Like hyloM but processes child schemas in parallel using mapConcurrently.
-- This provides a significant speedup when fetching multiple plugin schemas.
hyloMPar :: (SchemaF b -> SynapseM b) -> (a -> SynapseM (SchemaF a)) -> a -> SynapseM b
hyloMPar alg coalg = go
  where
    go a = do
      fa <- coalg a  -- unfold one step
      case fa of
        PluginF schema path children -> do
          -- Process children in parallel
          -- We need to capture the environment to run each child action
          env <- asks id  -- Get the SynapseEnv
          childResults <- liftIO $ mapConcurrently
            (\child -> do
              result <- runSynapseM env (go child)
              case result of
                -- SYNAPSE-28: a run sending from the cache asks the server
                -- for what the cache lacks; it never skips it
                Left err | cacheCannotAnswer err -> pure (Left err)
                Left err -> do
                  hPutStrLn stderr $ "[synapse] warning: skipping plugin (fetch failed): " ++ show err
                  pure (Right Nothing)
                Right val -> pure (Right (Just val))
            )
            children
          case [e | Left e <- childResults] of
            (e : _) -> throwError e
            [] -> alg (PluginF schema path (catMaybes [v | Right v <- childResults]))
        MethodF method ns path ->
          -- Methods are leaves, just apply algebra
          alg (MethodF method ns path)

-- ============================================================================
-- Schema-Specific Operations
-- ============================================================================

-- | Coalgebra for unfolding schema trees
--
-- Given a path, fetch the schema and produce one layer of SchemaF
-- with child paths in the recursive positions.
schemaCoalgebra :: Path -> SynapseM (SchemaF Path)
schemaCoalgebra path = do
  schema <- fetchSchemaAt path
  let childPaths = case psChildren schema of
        Nothing -> []
        Just children -> [path ++ [csNamespace c] | c <- children]
  pure $ PluginF schema path childPaths

-- | Unfold a complete schema tree from a path
--
-- This is anaM applied to our schema coalgebra.
-- Builds the full tree structure by fetching all schemas.
unfoldSchema :: Path -> SynapseM SchemaTree
unfoldSchema = anaM schemaCoalgebra

-- | Fold a schema tree with an algebra
--
-- Pure fold - use this when you've already built the tree.
foldSchema :: (SchemaF a -> a) -> SchemaTree -> a
foldSchema = cata

-- | Walk the schema tree with a monadic hylomorphism
--
-- This is the main operation: unfold from a path, fold with an algebra.
-- Fused, so doesn't build intermediate tree in memory.
walkSchema :: (SchemaF a -> SynapseM a) -> Path -> SynapseM a
walkSchema alg = hyloM alg schemaCoalgebra

-- | Walk the schema tree with parallel child fetching
--
-- Like walkSchema but fetches child plugin schemas in parallel.
-- Provides significant speedup for backends with many plugins.
walkSchemaPar :: (SchemaF a -> SynapseM a) -> Path -> SynapseM a
walkSchemaPar alg = hyloMPar alg schemaCoalgebra
