{-# LANGUAGE OverloadedStrings #-}

-- | String similarity utilities for typo suggestions
--
-- Provides Levenshtein distance computation and correction suggestions
-- for CLI parameter names and variant values.
module Synapse.CLI.Similarity
  ( levenshteinDistance
  , suggestCorrections
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sortOn)
import Data.Array

-- | Compute Levenshtein distance between two strings
--
-- Uses the Wagner-Fischer dynamic programming algorithm.
-- Time complexity: O(m*n) where m, n are string lengths.
--
-- Examples:
-- >>> levenshteinDistance "kitten" "sitting"
-- 3
-- >>> levenshteinDistance "message" "mesage"
-- 1
levenshteinDistance :: Text -> Text -> Int
levenshteinDistance s1 s2 = dp ! (m, n)
  where
    m = T.length s1
    n = T.length s2

    -- Dynamic programming array bounds: (0,0) to (m,n)
    bounds = ((0, 0), (m, n))

    -- DP table: dp!(i,j) = distance between first i chars of s1 and first j chars of s2
    dp = array bounds [(ij, dist ij) | ij <- range bounds]

    dist (0, j) = j  -- s1 is empty, need j insertions
    dist (i, 0) = i  -- s2 is empty, need i deletions
    dist (i, j) = minimum
      [ dp ! (i-1, j) + 1        -- deletion
      , dp ! (i, j-1) + 1        -- insertion
      , dp ! (i-1, j-1) + cost   -- substitution
      ]
      where
        cost = if T.index s1 (i-1) == T.index s2 (j-1) then 0 else 1

-- | Find similar strings from a list of valid options
--
-- Returns up to 3 suggestions sorted by edit distance.
-- Only suggests strings within a reasonable edit distance threshold:
-- - Threshold = max(2, length/3)
-- - This filters out dissimilar strings
--
-- Examples:
-- >>> suggestCorrections "mesage" ["message", "massage", "passage"]
-- ["message"]
-- >>> suggestCorrections "cnt" ["count", "content", "constant"]
-- ["count", "content"]
suggestCorrections :: Text -> [Text] -> [Text]
suggestCorrections typo validOptions =
  let threshold = max 2 (T.length typo `div` 3)
      withDist = [(opt, levenshteinDistance typo opt) | opt <- validOptions]
      -- Filter: distance > 0 (not exact match) and <= threshold
      filtered = filter (\(_, d) -> d <= threshold && d > 0) withDist
      sorted = sortOn snd filtered
  in take 3 (map fst sorted)
