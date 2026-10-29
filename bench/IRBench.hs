{-# LANGUAGE OverloadedStrings #-}

-- | SYNAPSE-CC-8: the IR builder alone, over menger's schemas of 2026-10-29.
--
-- The gzipped fixture is decoded once; then the IR is built @N@ times
-- (default 20) from the in-memory schemas — 'buildIR''s fold, no network —
-- and each build is forced by encoding it. Prints the median and p90 per
-- build and the bytes allocated per build, for the whole tree (7.1 MB of
-- schema) and for @tickets@ alone (its 2.68 MB schema and its children).
--
-- > cabal bench ir-bench                       # 20 runs
-- > cabal run ir-bench -- 50                   # 50 runs
-- > cabal run ir-bench -- --dump ir.json       # write the whole IR once, and stop
module Main where

import Control.Exception (evaluate)
import Control.Monad (forM, when)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import GHC.Clock (getMonotonicTime)
import GHC.Stats (getRTSStats, getRTSStatsEnabled, RTSStats(..))
import System.Environment (getArgs)
import System.Mem (performMajorGC)
import Text.Printf (printf)

import Data.Text (Text)

import IRFixture
import Synapse.Monad (SynapseEnv)

main :: IO ()
main = do
  args <- getArgs
  schemas <- loadSchemas menger20261029
  env <- envWithSchemas "menger" schemas
  case args of
    ["--dump", out] -> buildFixtureIR env [] >>= BL.writeFile out . encode
    _ -> do
      let n = case args of { [k] -> read k; _ -> 20 :: Int }
      printf "ir-bench: %d schemas, %d builds each\n" (length schemas) n
      bench env n "whole tree " []
      bench env n "tickets    " ["tickets"]

bench :: SynapseEnv -> Int -> String -> [Text] -> IO ()
bench env n label path = do
  _ <- buildOnce                                  -- warm: the memo, the code
  stats <- getRTSStatsEnabled
  performMajorGC
  a0 <- if stats then allocated_bytes <$> getRTSStats else pure 0
  times <- forM [1 .. n] $ \_ -> do
    t0 <- getMonotonicTime
    _ <- buildOnce
    t1 <- getMonotonicTime
    pure (t1 - t0)
  a1 <- if stats then allocated_bytes <$> getRTSStats else pure 0
  let sorted = sort times
      at q = sorted !! min (n - 1) (floor (q * fromIntegral n :: Double))
      perBuild = fromIntegral (a1 - a0) / fromIntegral n / (1024 * 1024) :: Double
  printf "%s median %7.1f ms  p90 %7.1f ms  min %7.1f ms  alloc %8.1f MB/build\n"
    label (1000 * at 0.5) (1000 * at 0.9) (1000 * head sorted) perBuild
  when (not stats) $ putStrLn "  (allocation: run with +RTS -T)"
  where
    buildOnce = buildFixtureIR env path >>= evaluate . BL.length . encode
