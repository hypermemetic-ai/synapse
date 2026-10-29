-- | NAMING-9: a Plexus backend is a service, never the retired word (owner,
-- 2026-10-29; ~/AGENTS.md "Naming").
-- The suite runs the repository's gate, scripts/naming-gate.sh, so
-- @cabal test@ — and CI — fails the day the retired word is written again.
--
-- A failure says how many lines the gate found and shows the first of them.
-- The gate's output is counted, never compared whole with an expected value:
-- on 2026-10-29 hspec's diff of an 875-line output grew to 7.3 GB and the
-- kernel's OOM killer stopped it (and the session around it). The suite's
-- heap is capped too (-with-rtsopts=-M6G in the cabal file).
module Main (main) where

import Control.Monad (unless)
import GHC.IO.Encoding (setLocaleEncoding)
import System.Exit (ExitCode (..))
import System.IO (hSetEncoding, stderr, stdout, utf8)
import System.Process (readProcessWithExitCode)
import Test.Hspec

-- | How many of the gate's lines a failure shows; @bash scripts/naming-gate.sh@
-- prints every one.
shown :: Int
shown = 20

main :: IO ()
main = do
  -- the gate prints lines of UTF-8 files; read and show them as such
  -- whatever the locale (a C locale would refuse the first em dash)
  setLocaleEncoding utf8
  mapM_ (`hSetEncoding` utf8) [stdout, stderr]
  hspec $
    describe "scripts/naming-gate.sh" $
      it "finds no line that says the retired word (exit 0)" $ do
        (code, out, err) <- readProcessWithExitCode "bash" ["scripts/naming-gate.sh"] ""
        let found = lines out
            n = length found
        unless (code == ExitSuccess && n == 0) . expectationFailure . unlines $
          [ "the gate found " <> show n <> " line(s) (" <> show code <> "); the first "
              <> show (min n shown) <> ":" ]
            <> take shown found
            <> [ "... and " <> show (n - shown) <> " more: bash scripts/naming-gate.sh" | n > shown ]
            <> take 3 (lines err)
