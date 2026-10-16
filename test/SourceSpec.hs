{-# LANGUAGE OverloadedStrings #-}
-- | SYNAPSE-10: a parameter's value from a file or stdin.
module Main (main) where

import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive, createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec

import Synapse.CLI.Source

-- | An environment over a scratch directory, with stdin handed in and
-- counted, so a test can say what stdin holds and how often it was read.
withEnv :: BS.ByteString -> (SourceEnv -> FilePath -> IO Int -> IO a) -> IO a
withEnv stdinBytes body = do
  tmp <- getTemporaryDirectory
  let dir = tmp </> "synapse-source-spec"
  createDirectoryIfMissing True dir
  reads' <- newIORef (0 :: Int)
  cache <- newIORef Nothing
  let env = SourceEnv
        { seCwd = dir
        , seHome = Just (dir </> "home")
        , seStdin = do n <- readIORef reads'; writeIORef reads' (n + 1); pure stdinBytes
        , seCache = cache
        }
  createDirectoryIfMissing True (dir </> "home")
  r <- body env dir (readIORef reads')
  removeDirectoryRecursive dir
  pure r

main :: IO ()
main = hspec $ describe "a value from a file or stdin (SYNAPSE-10)" $ do
  it "reads @file as the file's contents, as they are" $ withEnv "" $ \env dir _ -> do
    BS.writeFile (dir </> "body.md") "---\ntitle: x\n---\n\ncosts $HOME, keeps the final newline\n"
    r <- expandSources env [("body", "@body.md"), ("title", "plain")]
    r `shouldBe` Right [("body", "---\ntitle: x\n---\n\ncosts $HOME, keeps the final newline\n"), ("title", "plain")]

  it "round-trips a 1 MB body" $ withEnv "" $ \env dir _ -> do
    let big = T.replicate (1024 * 1024 `div` 16) "0123456789abcdef"
    BS.writeFile (dir </> "big.md") (TE.encodeUtf8 big)
    r <- expandValue env "body" "@big.md"
    fmap T.length r `shouldBe` Right (1024 * 1024)
    r `shouldBe` Right big

  it "resolves ~/ against home and an absolute path as itself" $ withEnv "" $ \env dir _ -> do
    BS.writeFile (dir </> "home" </> "h.txt") "from home"
    BS.writeFile (dir </> "abs.txt") "absolute"
    expandValue env "a" "@~/h.txt" >>= (`shouldBe` Right "from home")
    expandValue env "b" (T.pack ("@" <> dir </> "abs.txt")) >>= (`shouldBe` Right "absolute")

  it "reads - from stdin, once, however many values ask" $ withEnv "piped in" $ \env _ reads' -> do
    r <- expandSources env [("body", "-"), ("note", "-")]
    r `shouldBe` Right [("body", "piped in"), ("note", "piped in")]
    reads' >>= (`shouldBe` 1)
    stdinWasRead env >>= (`shouldBe` True)

  it "has not read stdin when no value asked" $ withEnv "piped in" $ \env _ reads' -> do
    _ <- expandSources env [("body", "text")]
    reads' >>= (`shouldBe` 0)
    stdinWasRead env >>= (`shouldBe` False)

  it "sends @@x as the literal @x" $ withEnv "" $ \env _ _ -> do
    expandValue env "handle" "@@alice" >>= (`shouldBe` Right "@alice")
    expandValue env "handle" "@@@" >>= (`shouldBe` Right "@@")

  it "leaves a lone @ and every other value alone" $ withEnv "" $ \env _ _ -> do
    expandValue env "x" "@" >>= (`shouldBe` Right "@")
    expandValue env "x" "a@b" >>= (`shouldBe` Right "a@b")
    expandValue env "x" "--dashes" >>= (`shouldBe` Right "--dashes")
    expandValue env "x" "" >>= (`shouldBe` Right "")

  it "refuses a file it cannot read, naming the flag, the path and the escape" $ withEnv "" $ \env _ _ -> do
    r <- expandValue env "blocked_by" "@nope.md"
    case r of
      Left e -> do
        seKey e `shouldBe` "blocked_by"
        seSource e `shouldBe` "@nope.md"
        let msg = renderSourceError e
        msg `shouldSatisfy` T.isPrefixOf "--blocked-by @nope.md: cannot read nope.md"
        msg `shouldSatisfy` T.isInfixOf "write @@nope.md"
      Right v -> expectationFailure ("read " <> show v)

  it "names -p when the params object is the source" $ withEnv "" $ \env _ _ -> do
    r <- expandValue env "" "@params.json"
    fmap renderSourceError (either Just (const Nothing) r) `shouldSatisfy` maybe False (T.isPrefixOf "-p @params.json: cannot read params.json")

  it "stops at the first unreadable source" $ withEnv "" $ \env dir _ -> do
    BS.writeFile (dir </> "ok.md") "ok"
    r <- expandSources env [("a", "@ok.md"), ("b", "@missing"), ("c", "@missing2")]
    either seSource (const "read everything") r `shouldBe` "@missing"
