{-# LANGUAGE OverloadedStrings #-}

-- | Quick test to see TypeRef JSON serialization
module Main where

import Control.Monad (forM_, unless)
import Data.Aeson (decode, encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Text (Text)
import System.Exit (exitFailure)

import Synapse.IR.Types

main :: IO ()
main = do
  putStrLn "=== TypeRef JSON Serialization Test ==="
  putStrLn ""

  -- Test 1: Simple QualifiedName with namespace
  putStrLn "1. RefNamed with namespace:"
  let qn1 = QualifiedName { qnNamespace = "cone", qnLocalName = "UUID" }
  let ref1 = RefNamed qn1
  putStrLn "   Haskell: "
  print ref1
  putStrLn "   JSON (compact):"
  BSL.putStrLn (encode ref1)
  putStrLn "   JSON (pretty):"
  BSL.putStrLn (encodePretty ref1)
  putStrLn ""

  -- Test 2: Empty namespace
  putStrLn "2. RefNamed with empty namespace:"
  let qn2 = QualifiedName { qnNamespace = "", qnLocalName = "GlobalType" }
  let ref2 = RefNamed qn2
  putStrLn "   Haskell: "
  print ref2
  putStrLn "   JSON (pretty):"
  BSL.putStrLn (encodePretty ref2)
  putStrLn ""

  -- Test 3: Nested structure
  putStrLn "3. RefOptional (RefArray (RefNamed)):"
  let qn3 = QualifiedName { qnNamespace = "arbor", qnLocalName = "Node" }
  let ref3 = RefOptional (RefArray (RefNamed qn3))
  putStrLn "   Haskell: "
  print ref3
  putStrLn "   JSON (pretty):"
  BSL.putStrLn (encodePretty ref3)
  putStrLn ""

  -- Test 4: qualifiedNameFull function
  putStrLn "4. qualifiedNameFull examples:"
  putStrLn $ "   cone.UUID -> " <> show (qualifiedNameFull qn1)
  putStrLn $ "   GlobalType -> " <> show (qualifiedNameFull qn2)
  putStrLn ""

  -- Test 5: FieldDef with QualifiedName
  putStrLn "5. FieldDef with RefNamed:"
  let fd = FieldDef
        { fdName = "tree_id"
        , fdType = RefNamed (QualifiedName "arbor" "UUID")
        , fdDescription = Just "Tree identifier"
        , fdRequired = True
        , fdDefault = Nothing
        }
  putStrLn "   JSON (pretty):"
  BSL.putStrLn (encodePretty fd)
  putStrLn ""

  -- SYNAPSE-CC-8 (IR 2.1): each new constructor encodes as the contract says
  -- and decodes back; IR 2.0's RefAny (a serde_json::Value) reads as RefJson
  putStrLn "6. IR 2.1 constructors (SYNAPSE-CC-8):"
  let str = RefPrimitive "string" Nothing
      cases :: [(TypeRef, BSL.ByteString)]
      cases =
        [ (RefJson, "{\"tag\":\"RefJson\"}")
        , (RefMap str, "{\"contents\":{\"contents\":[\"string\",null],\"tag\":\"RefPrimitive\"},\"tag\":\"RefMap\"}")
        , (RefTuple [str, str], "{\"contents\":[{\"contents\":[\"string\",null],\"tag\":\"RefPrimitive\"},{\"contents\":[\"string\",null],\"tag\":\"RefPrimitive\"}],\"tag\":\"RefTuple\"}")
        , (RefRefined (RefPrimitive "integer" (Just "uint64")) noRefinements { rnMinimum = Just 1 }, "{\"contents\":[{\"contents\":[\"integer\",\"uint64\"],\"tag\":\"RefPrimitive\"},{\"minimum\":1}],\"tag\":\"RefRefined\"}")
        , (RefChild "tickets.inputs", "{\"contents\":\"tickets.inputs\",\"tag\":\"RefChild\"}")
        , (RefUnknown, "{\"tag\":\"RefUnknown\"}")
        ]
  failures <- fmap concat $ mapM (\(t, j) -> do
      let ok = encode t == j && decode j == Just t
      putStrLn ("   " <> (if ok then "ok   " else "FAIL ") <> BSL.unpack j)
      pure [ () | not ok ]) cases
  let legacy = decode "{\"tag\":\"RefAny\"}" :: Maybe TypeRef
  putStrLn ("   2.0's RefAny reads as " <> show legacy)
  let refusal = Refusal "tickets.Reply" "oneOf" (Just 2) "why"
      refusalOk = decode (encode refusal) == Just refusal
      reprOk = encode External == "\"external\"" && decode "\"untagged\"" == Just Untagged
  forM_ [("Refusal", refusalOk), ("EnumRepr", reprOk), ("legacy RefAny", legacy == Just RefJson)] $ \(n, ok) ->
    putStrLn ("   " <> (if ok then "ok   " else "FAIL ") <> n)
  unless (null failures && refusalOk && reprOk && legacy == Just RefJson) exitFailure
