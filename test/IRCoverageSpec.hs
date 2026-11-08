{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | SYNAPSE-CC-8 (IR 2.1): every schema read whole, or refused by name.
--
-- One fixture per evidence row (test/fixtures/ir-coverage; make-fixtures.py
-- writes them, menger's types copied verbatim from its 2026-10-29 schemas),
-- then menger's 2026-10-29 schemas whole: exactly the 21 refusals of the
-- walk, the IR equal to its golden, every construct IR 2.0 already typed
-- serialized as before; and the built synapse's @-i@ against a service
-- with a bare object: the IR on stdout, the site on stderr, exit 65.
module Main where

import Control.Monad (forM_, unless, when)
import qualified Codec.Compression.GZip as GZ
import Data.Aeson (Value(..), eitherDecode, encode, toJSON, (.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty')
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef (readIORef, writeIORef)
import Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Hspec

import Synapse.IR.Types
import Synapse.SchemaStore (Plan(..), Store(..), readPlan, schemasRoot, storeFor, writePlan)
import Synapse.Endpoint (plainAt)
import Plexus.Schema.Recursive (MethodSchema(..))

import IRFixture
import Plexus07 (methodAs07, withContract)
import Synapse.IR.Builder (IRBuildError(..), TypeSource(..), buildIRFromSchema)
import MockService

main :: IO ()
main = hspec $ do
  describe "unions read whole" $ do
    it "a oneOf of an enum and consts is one string enum, in schema order" $ do
      ir <- irOf "transition-field.json" ["tickets"]
      kindOf ir "tickets.TransitionField" `shouldBe` Just (KindStringEnum ["status", "kind", "archive"])
      irRefusals ir `shouldBe` []
      ir' <- irOf "answer-source.json" ["tickets"]
      kindOf ir' "tickets.AnswerSource" `shouldBe` Just (KindStringEnum ["app", "api", "system"])
      irRefusals ir' `shouldBe` []

    it "a union tagged kind: six variants, the tag out of their fields, rationale optional" $ do
      ir <- irOf "reply.json" ["tickets"]
      irRefusals ir `shouldBe` []
      case kindOf ir "tickets.Reply" of
        Just (KindEnum disc vs Internal) -> do
          disc `shouldBe` "kind"
          map vdName vs `shouldBe` ["answered", "changes_requested", "commented", "unqueued", "ended", "archived"]
          let answered = head vs
          sort (map fdName (vdFields answered)) `shouldBe` sort ["input", "value", "source", "at_unix", "rationale"]
          [ fdRequired f | f <- vdFields answered, fdName f == "rationale" ] `shouldBe` [False]
          [ fdRequired f | f <- vdFields answered, fdName f == "input" ] `shouldBe` [True]
          [ fdType f | f <- vdFields answered, fdName f == "at_unix" ] `shouldBe` [RefPrimitive "integer" (Just "int64")]
        other -> expectationFailure ("Reply: " <> show other)

    it "a union tagged event" $ do
      ir <- irOf "hash-event.json" ["tickets"]
      irRefusals ir `shouldBe` []
      case kindOf ir "tickets.HashEvent" of
        Just (KindEnum "event" [v] Internal) -> do
          vdName v `shouldBe` "hash"
          map fdName (vdFields v) `shouldBe` ["value"]
        other -> expectationFailure ("HashEvent: " <> show other)

    it "an externally tagged union: browser, code, password, session" $ do
      ir <- irOf "login-flow.json" ["auth"]
      irRefusals ir `shouldBe` []
      case kindOf ir "auth.LoginFlow" of
        Just (KindEnum "" vs External) -> do
          map vdName vs `shouldBe` ["browser", "code", "password", "session"]
          let session = last vs
          [ fdType f | f <- vdFields session, fdName f == "ttl_secs" ]
            `shouldBe` [RefOptional (RefPrimitive "integer" (Just "uint64"))]
          [ (fdName f, fdRequired f) | f <- vdFields (head vs) ] `shouldBe` [("redirect", True)]
        other -> expectationFailure ("LoginFlow: " <> show other)
      -- its JSON says external, and IR 2.0's internal enums say nothing
      encode (toJSON (KindEnum "" [] External)) `shouldSatisfy` (contains "\"keRepr\":\"external\"")
      encode (toJSON (KindEnum "type" [] Internal)) `shouldNotSatisfy` (contains "keRepr")

    it "an untagged union of refs: each variant names its type in vdRef" $ do
      ir <- irOf "schema-result.json" ["tickets"]
      irRefusals ir `shouldBe` []
      kindOf ir "tickets.SchemaResult" `shouldBe` Just (KindEnum ""
        [ VariantDef "PluginSchema" Nothing [] (Just (named "tickets" "PluginSchema"))
        , VariantDef "MethodSchema" Nothing [] (Just (named "tickets" "MethodSchema")) ] Untagged)
      mdReturns <$> method ir "tickets.schema" `shouldBe` Just (named "tickets" "SchemaResult")

    it "an unreadable union is refused: the type left out, its use RefUnknown" $ do
      ir <- irOf "unreadable-oneof.json" ["tickets"]
      map (\r -> (rfSite r, rfKeyword r, rfVariant r)) (irRefusals ir) `shouldBe` [("tickets.Weird", "oneOf", Just 1)]
      kindOf ir "tickets.Weird" `shouldBe` Nothing
      fieldType ir "tickets.TicketsEvent" "got" "w" `shouldBe` Just RefUnknown

  describe "methods" $ do
    it "a child's return is RefChild, with no refusal" $ do
      ir <- irOf "static-child.json" ["tickets"]
      mdReturns <$> method ir "tickets.inputs" `shouldBe` Just (RefChild "tickets.inputs")
      irRefusals ir `shouldBe` []

    -- RETURNS-36: plexus 0.7 requires returns, so such a method is no 0.7
    -- schema at all: refused by plexus-protocol before the builder sees it
    it "an rpc method with no returns is no 0.7 schema: it does not parse" $ do
      Right rows <- eitherDecode <$> BL.readFile (fixtures </> "no-returns.json") :: IO (Either String [Value])
      let methodsOf = \case
            Object r | Just (Object sch) <- KM.lookup "schema" r, Just (Array ms) <- KM.lookup "methods" sch -> foldr (:) [] ms
            _ -> []
          ms = map methodAs07 (concatMap methodsOf rows)
      ms `shouldNotBe` []
      forM_ ms $ \m -> case Aeson.fromJSON m :: Aeson.Result MethodSchema of
        Aeson.Error e -> e `shouldContain` "returns"
        Aeson.Success _ -> expectationFailure "a method with no returns parsed"

    it "a struct root is a type, and no RefNamed dangles" $ do
      ir <- irOf "hashes.json" ["tickets"]
      irRefusals ir `shouldBe` []
      kindOf ir "tickets.PluginHashes" `shouldSatisfy` \case { Just (KindStruct _) -> True; _ -> False }
      mdReturns <$> method ir "tickets.hashes" `shouldBe` Just (named "tickets" "PluginHashes")

    it "injected params leave mdParams for mdInjected, their source verbatim" $ do
      ir <- irOf "injected.json" ["tickets"]
      let Just md = method ir "tickets.get"
      map pdName (mdParams md) `shouldBe` ["ticket"]
      mdInjected md `shouldBe`
        [ InjectedParam "auth" (obj [("from", "derived")]) Nothing
        , InjectedParam "user" (obj [("from", "auth"), ("resolver", "self.access.verify")]) Nothing ]
      irRefusals ir `shouldBe` []

    it "mdInput: an Input<Bytes> item is a string of format binary, refused nowhere (BIDIR-18/19)" $ do
      ir <- irOf "input.json" ["tickets"]
      irRefusals ir `shouldBe` []
      mdInput <$> method ir "tickets.audio" `shouldBe` Just (Just (RefPrimitive "string" (Just "binary")))
      let Just audio = method ir "tickets.audio"
      encode (mdInput audio) `shouldBe` "{\"contents\":[\"string\",\"binary\"],\"tag\":\"RefPrimitive\"}"
      -- the bench server's ingest.items: its titled object a named type
      mdInput <$> method ir "tickets.items" `shouldBe` Just (Just (named "tickets" "Item"))
      kindOf ir "tickets.Item" `shouldSatisfy` isJust

    it "mdInput: the schema's input as a type; absent, no key" $ do
      ir <- irOf "input.json" ["tickets"]
      mdInput <$> method ir "tickets.chat" `shouldBe` Just (Just (named "tickets" "Token"))
      mdInput <$> method ir "tickets.plain" `shouldBe` Just Nothing
      let Just plain = method ir "tickets.plain"
      encode plain `shouldNotSatisfy` (contains "mdInput")
      encode plain `shouldNotSatisfy` (contains "mdInjected")
      kindOf ir "tickets.Token" `shouldSatisfy` isJust
      -- plexus-protocol reads it, and writes it back only when present
      let Right ms = Aeson.eitherDecode "{\"name\":\"chat\",\"returns\":{},\"cardinality\":\"one\",\"input\":{\"type\":\"string\"}}" :: Either String MethodSchema
      methodInput ms `shouldBe` Just (obj [("type", "string")])
      encode ms `shouldSatisfy` (contains "\"input\"")
      let Right bare = Aeson.eitherDecode "{\"name\":\"plain\",\"returns\":{},\"cardinality\":\"one\"}" :: Either String MethodSchema
      encode bare `shouldNotSatisfy` (contains "\"input\"")

  describe "the JSON type (a serde_json::Value)" $ do
    it "is RefJson, however the schema spells it" $ do
      ir <- irOf "json.json" ["tickets"]
      irRefusals ir `shouldBe` []
      forM_ ["six", "described", "empty", "yes", "byref"] $ \f ->
        structField ir "tickets.Envelope" f `shouldBe` Just RefJson
      structField ir "tickets.Envelope" "list" `shouldBe` Just (RefArray RefJson)
      structField ir "tickets.Envelope" "map" `shouldBe` Just (RefMap RefJson)
      fieldType ir "tickets.TicketsEvent" "got" "value" `shouldBe` Just RefJson
      -- the definition named Json is the JSON type, not a type of the namespace
      kindOf ir "tickets.Json" `shouldBe` Nothing
      encode RefJson `shouldBe` "{\"tag\":\"RefJson\"}"

    it "what is not: a bare object, a bare array, a type list that is neither one type nor JSON" $ do
      ir <- irOf "untyped.json" ["tickets"]
      map (\r -> (rfSite r, rfKeyword r)) (irRefusals ir) `shouldBe`
        [("tickets.Thing.bare", "type"), ("tickets.Thing.list[]", "items"), ("tickets.Thing.mixed", "type")]
      structField ir "tickets.Thing" "bare" `shouldBe` Just RefUnknown
      structField ir "tickets.Thing" "list" `shouldBe` Just (RefArray RefUnknown)

  describe "maps, tuples, bounds" $ do
    it "a map, and an optional map" $ do
      ir <- irOf "theme-map.json" ["themes"]
      irRefusals ir `shouldBe` []
      structField ir "themes.Theme" "light" `shouldBe` Just (RefMap (RefPrimitive "string" Nothing))
      structField ir "themes.Theme" "dark" `shouldBe` Just (RefMap (RefPrimitive "string" Nothing))
      param ir "themes.define" "light" `shouldBe` Just (RefOptional (RefMap (named "themes" "TokenValue")))
      param ir "themes.define" "dark" `shouldBe` Just (RefOptional (RefMap (named "themes" "TokenValue")))

    it "a tuple" $ do
      ir <- irOf "component-preview.json" ["tickets"]
      irRefusals ir `shouldBe` []
      structField ir "tickets.Component" "preview" `shouldBe`
        Just (RefArray (RefTuple [RefPrimitive "string" Nothing, RefPrimitive "string" Nothing]))

    it "implied bounds carry nothing; others are refined" $ do
      ir <- irOf "bounds.json" ["tickets"]
      irRefusals ir `shouldBe` []
      structField ir "tickets.Bounds" "a" `shouldBe` Just (RefPrimitive "integer" (Just "uint64"))
      structField ir "tickets.Bounds" "b" `shouldBe` Just (RefPrimitive "integer" (Just "uint16"))
      structField ir "tickets.Bounds" "c" `shouldBe` Just (RefRefined (RefPrimitive "integer" (Just "uint64")) noRefinements { rnMinimum = Just 1 })
      structField ir "tickets.Bounds" "d" `shouldBe` Just (RefRefined (RefPrimitive "string" Nothing) noRefinements { rnMaxLength = Just 64 })
      structField ir "tickets.Bounds" "e" `shouldBe` Just (RefOptional (RefPrimitive "integer" (Just "uint32")))

    it "an unknown keyword is refused by name" $ do
      ir <- irOf "allof.json" ["tickets"]
      map (\r -> (rfSite r, rfKeyword r)) (irRefusals ir) `shouldBe` [("tickets.Both", "allOf")]
      kindOf ir "tickets.Both" `shouldBe` Nothing
      ir' <- irOf "multipleof.json" ["tickets"]
      map (\r -> (rfSite r, rfKeyword r)) (irRefusals ir') `shouldBe` [("tickets.Even.n", "multipleOf")]

    it "a variant's field keeps its default" $ do
      ir <- irOf "variant-default.json" ["tickets"]
      case kindOf ir "tickets.TicketsEvent" of
        Just (KindEnum "type" (v : _) Internal) ->
          [ (fdDefault f, fdRequired f) | f <- vdFields v, fdName f == "limit" ] `shouldBe` [(Just (Number 1000), False)]
        other -> expectationFailure (show other)

  describe "placement" $ do
    it "a shared type goes where it is referred to most; a tie to the ancestor, then the shortest; never the root" $ do
      ir <- irOf "shared.json" []
      irRefusals ir `shouldBe` []
      let has = (`Map.member` irTypes ir)
      filter has ["notes.TicketRef", "tickets.TicketRef"] `shouldBe` ["tickets.TicketRef"]
      filter has ["chats.Tied", "tickets.Tied"] `shouldBe` ["chats.Tied"]
      filter has ["tickets.Anc", "tickets.inputs.Anc"] `shouldBe` ["tickets.Anc"]
      filter has [".Shared", "tickets.Shared"] `shouldBe` ["tickets.Shared"]
      -- every use follows its type
      fieldType ir "notes.NotesEvent" "e" "r0" `shouldBe` Just (named "tickets" "TicketRef")
      fieldType ir ".PingEvent" "e" "r0" `shouldBe` Just (named "tickets" "Shared")

  describe "every fixture" $
    it "names only types it defines, and is RefUnknown only where a refusal is" $
      forM_ smallFixtures $ \(file, path) -> do
        ir <- irOf file path
        danglingRefs ir `shouldBe` []
        -- a type the reader refused is left out, and its uses say RefUnknown
        let refused = Set.fromList (map rfSite (irRefusals ir))
            typesLeftOut = [ r | r <- Set.toList refused, not (Map.member r (irTypes ir)), not (T.any (`elem` (" (" :: String)) r) ]
        [ s | s <- unknownSites ir, not (Set.member s refused), null typesLeftOut ] `shouldBe` []

  -- RETURNS-36: synapse 4.0 reads the published 0.7.0 line too. The capture
  -- as menger served it — no cardinality, a streaming flag on each method —
  -- builds IR 3.0 whose every cardinality is that method's own flag.
  describe "menger, 2026-10-29, as served (0.7.0)" $
    it "every method's mdCardinality is its own streaming flag" $ do
      rows <- map (either error id . eitherDecode) . filter (not . BL.null) . BL.lines . GZ.decompress <$> BL.readFile menger20261029 :: IO [Value]
      let served = [ (p, sch)
                   | Object r <- rows, Just pv <- [KM.lookup "path" r], Aeson.Success p <- [Aeson.fromJSON pv]
                   , Just sch <- [KM.lookup "schema" r] ]
          flags = [ (T.intercalate "." p <> "." <> n, st)
                  | (p, Object sch) <- served, Just (Array ms) <- [KM.lookup "methods" sch]
                  , Object m <- foldr (:) [] ms, Just (String n) <- [KM.lookup "name" m]
                  , let st = KM.lookup "streaming" m == Just (Bool True) ]
      schemas <- mapM (\(p, v) -> case Aeson.fromJSON v of
                          Aeson.Success sch -> pure (p, sch)
                          Aeson.Error e -> fail (T.unpack (T.intercalate "." p) <> ": " <> e)) served
      env <- envWithSchemas "menger" schemas
      ir <- buildFixtureIR env []
      length flags `shouldSatisfy` (> 100)
      -- chats.inputs is refused whole (two types titled Message), so not here
      [ (path, st) | (path, st) <- flags, not ("chats.inputs." `T.isPrefixOf` path)
                   , fmap mdCardinality (Map.lookup path (irMethods ir)) /= Just (if st then CardinalityMany else CardinalityOne) ]
        `shouldBe` []

  describe "menger, 2026-10-29, whole" $ do
    -- RETURNS-13: one title in one namespace is one type. The capture's
    -- chats.inputs names two (Message: the vocabulary's string in params,
    -- the chat's message in returns); 2.1 kept the first, 3.0 refuses: the
    -- one plugin's build fails, and the walk refuses that activation, naming
    -- both, and goes on (the review, #9).
    it "as captured, chats.inputs puts two types under Message: the build fails naming both" $ do
      rows <- map (either error id . eitherDecode) . filter (not . BL.null) . BL.lines . GZ.decompress <$> BL.readFile menger20261029 :: IO [Value]
      let inputs = [ sch | Object r <- rows, KM.lookup "path" r == Just (toJSON (["chats", "inputs"] :: [Text])), Just sch <- [KM.lookup "schema" r] ]
      [schema] <- pure [ ps | v <- inputs, Aeson.Success ps <- [Aeson.fromJSON (withContract v)] ]
      case buildIRFromSchema "menger" ["chats", "inputs"] schema of
        Left err -> do
          (tcNamespace err, tcTitle err) `shouldBe` ("chats.inputs", "Message")
          (tcFirst err, tcSecond err) `shouldBe` (TypeSource "list" "params", TypeSource "list" "returns")
        Right _ -> expectationFailure "two types under one title built"
    it "the walk refuses chats.inputs, naming both sites, leaves it out, and goes on" $ do
      ir <- mengerIR
      [ (rfSite r, rfKeyword r) | r <- irRefusals ir ] `shouldBe` [("chats.inputs", "title")]
      [ rfDetail r | r <- irRefusals ir ] `shouldSatisfy` all (\d -> all (`T.isInfixOf` d) ["\"Message\"", "method list (params)", "method list (returns)"])
      [ k | k <- Map.keys (irMethods ir), "chats.inputs." `T.isPrefixOf` k ] `shouldBe` []
      [ k | k <- Map.keys (irTypes ir), "chats.inputs." `T.isPrefixOf` k ] `shouldBe` []
      Map.member "chats.post" (irMethods ir) `shouldBe` True
      Map.member "tickets.get" (irMethods ir) `shouldBe` True

    -- RETURNS-36: 2.1 refused the walk at the 21 schema methods' returns,
    -- which 0.6 left unstated; a 0.7 schema states every method's returns
    -- (the captures are given `{}` for them, 'withContract'), so those
    -- return the JSON type (chats.inputs' aside: it is refused whole).
    it "the 20 schema methods 2.1 refused return the JSON type; the root's carriers are the JSON type" $ do
      ir <- mengerIR
      unknownSites ir `shouldBe` []
      let schemaMethods = [ T.dropEnd (T.length " returns") site | (site, _) <- expectedMengerRefusals, site /= "chats.inputs.schema returns" ]
      [ mdReturns <$> Map.lookup m (irMethods ir) | m <- schemaMethods ] `shouldBe` replicate 20 (Just RefJson)
      danglingRefs ir `shouldBe` []
      param ir ".call" "params" `shouldBe` Just RefJson
      fieldType ir ".PlexusStreamItem" "data" "content" `shouldBe` Just RefJson
      fieldType ir ".PlexusStreamItem" "request" "requestData" `shouldBe` Just RefJson

    it "types the 7 enums, the 4 maps, the tuple and .PluginHashes" $ do
      ir <- mengerIR
      forM_ ["HashEvent", "LoginFlow", "AnswerSource", "InputAction", "ThemeLevel", "Reply", "TransitionField"] $ \n ->
        case [ tdKind td | td <- Map.elems (irTypes ir), tdName td == n ] of
          [KindEnum _ vs _] -> vs `shouldNotBe` []
          [KindStringEnum vs] -> vs `shouldNotBe` []
          other -> expectationFailure (T.unpack n <> ": " <> show other)
      structField ir "themes.Theme" "light" `shouldBe` Just (RefMap (RefPrimitive "string" Nothing))
      structField ir "themes.Theme" "dark" `shouldBe` Just (RefMap (RefPrimitive "string" Nothing))
      [ () | Just (RefOptional (RefMap _)) <- [param ir "themes.define" "light", param ir "themes.define" "dark"] ] `shouldBe` [(), ()]
      [ fdType f | td <- Map.elems (irTypes ir), tdName td == "Component", KindStruct fs <- [tdKind td], f <- fs, fdName f == "preview" ]
        `shouldBe` [RefArray (RefTuple [RefPrimitive "string" Nothing, RefPrimitive "string" Nothing])]
      kindOf ir ".PluginHashes" `shouldSatisfy` isJust
      -- no injected param is a param any more: 88 auth, 88 user
      -- (chats.inputs' 10 aside: it is refused whole)
      length [ () | md <- Map.elems (irMethods ir), ip <- mdInjected md ] `shouldBe` 166
      [ pdName p | md <- Map.elems (irMethods ir), p <- mdParams md, pdName p `elem` ["auth", "user"] ] `shouldBe` []

    it "equals its golden (menger-2026-10-29.ir-3.0.json)" $ do
      ir <- mengerIR
      let golden = fixtures </> "menger-2026-10-29.ir-3.0.json"
          bytes = encodePretty' defConfig { confCompare = compare } ir { irMetadata = Nothing }
      write <- lookupEnv "IR_GOLDEN_WRITE"
      exists <- doesFileExist golden
      when (write == Just "1" || not exists) $ BL.writeFile golden (bytes <> "\n")
      expected <- BL.readFile golden
      (bytes <> "\n") `shouldBe` expected

    it "serializes every construct IR 2.0 already typed as before" $ do
      ir <- mengerIR
      Right old <- eitherDecode . GZ.decompress <$> BL.readFile (fixtures </> "menger-2026-10-29.ir-2.0.json.gz") :: IO (Either String Value)
      let Object o = old
          Just (Object oldTypes) = KM.lookup "irTypes" o
          Just (Object oldMethods) = KM.lookup "irMethods" o
          newTypes = Map.map toJSON (irTypes ir)
          byLocal = Map.fromListWith (++) [ (tdName td, [toJSON td]) | td <- Map.elems (irTypes ir) ]
      -- types: the same JSON, refs' namespaces aside (placement moves them),
      -- variant fields' fdRequired / fdDefault aside (2.1 reads them); a
      -- type placement moved keeps the moved-to copy's namespace and doc
      let typeMismatches =
            [ "type " <> T.unpack key <> " changed:\n  old " <> show (norm v) <> "\n  new " <> show (map norm candidates)
            | (k, v) <- KM.toList oldTypes, not (changedIn20 v)
            , not ("chats.inputs." `T.isPrefixOf` K.toText k)   -- refused whole in 3.0
            , let key = K.toText k
                  local = T.takeWhileEnd (/= '.') key
                  candidates = case Map.lookup key newTypes of
                    Just n -> [n]
                    Nothing -> Map.findWithDefault [] local byLocal
                  same n = case Map.lookup key newTypes of
                    Just _ -> norm n == norm v
                    Nothing -> moved n == moved v
            , not (any same candidates) ]
          moved = norm . dropKeys ["tdNamespace", "tdDescription"]
      -- methods: the same JSON but for the keys 2.1 adds and the injected
      -- params it moves, and 3.0's contract in place of 2.0's streaming
      -- guess (RETURNS-13: mdError, mdProgress, mdCardinality; the 2026-10-29
      -- schemas state no error or progress)
          methodMismatches =
            [ "method " <> T.unpack key <> ":\n  old " <> show old' <> "\n  new " <> show new'
            | (k, v) <- KM.toList oldMethods
            , let key = K.toText k
                  -- ASK-1: 3.1 drops 2.0's mdBidir*
                  old' = norm (dropKeys ["mdStreaming", "mdBidirType", "mdBidirResponseType", "mdBidirResponseSchema"] (dropInjected v))
                  new' = maybe Null (norm . dropKeys ["mdInjected", "mdInput", "mdError", "mdProgress", "mdCardinality"] . toJSON) (Map.lookup key (irMethods ir))
            , not (changedIn20 (dropInjected v)), old' /= new'
            -- chats.inputs is refused whole in 3.0 (two types titled Message)
            , not ("chats.inputs." `T.isPrefixOf` key) ]
          -- the name IRFixture gave chats.inputs' params' Message, back
      mapM_ putStrLn (take 5 (typeMismatches ++ methodMismatches))
      (length typeMismatches, length methodMismatches) `shouldBe` (0, 0)
      -- and the guard is not vacuous: most of 2.0 was typed
      length [ () | (_, v) <- KM.toList oldTypes, not (changedIn20 v) ] `shouldSatisfy` (> 150)
      length [ () | (_, v) <- KM.toList oldMethods, not (changedIn20 (dropInjected v)) ] `shouldSatisfy` (> 100)

  describe "caches another synapse wrote" $ do
    -- RETURNS-36 review (#10): 4.0's schemas and plans (plexus 0.8's
    -- shape: cardinality, no streaming) are kept apart from 3.x's
    -- (~/.cache/synapse/schemas), so a rollback to synapse.prev never reads
    -- an entry 4.0 wrote, nor 4.0 one of 3.x's
    it "4.0 keeps its schema cache in a directory of its own layout, never 3.x's" $
      withSystemTempDirectory "r36-cache" $ \dir -> do
        setEnv "XDG_CACHE_HOME" dir
        root <- schemasRoot
        st <- storeFor (plainAt "127.0.0.1" 4455) "builder" "menger"
        unsetEnv "XDG_CACHE_HOME"
        root `shouldBe` dir </> "synapse" </> "schemas-4"
        storeDir st `shouldStartWith` (dir </> "synapse" </> "schemas-4" </> "")
    it "a plan whose IR is 2.0 (3.16.0's) is read as no plan" $
      withSystemTempDirectory "cc8-store" $ \dir -> do
        let st = Store dir
            Right ms = Aeson.eitherDecode "{\"name\":\"get\",\"returns\":{},\"cardinality\":\"one\"}" :: Either String MethodSchema
            plan v = Plan ms ["tickets", "get"] Nothing (Just emptyIR { irVersion = v })
        createDirectoryIfMissing True (dir </> "plans")
        writePlan st ["tickets", "get"] (plan "2.0")
        readPlan st ["tickets", "get"] `shouldReturn` Nothing
        writePlan st ["tickets", "get"] (plan currentIrVersion)
        fmap plPath <$> readPlan st ["tickets", "get"] `shouldReturn` Just ["tickets", "get"]

  describe "synapse -i" $ do
    exe <- runIO builtSynapse
    case exe of
      Nothing -> it "against a service with a bare object" $ pendingWith "the built synapse is not on PATH (cabal test puts it there)"
      Just bin -> do
        let root = Object (KM.fromList
              [ ("namespace", String "menger"), ("version", String "1.0.0"), ("description", String "root")
              , ("hash", String "r-cc8"), ("methods", Array mempty)
              , ("children", toJSON [ Object (KM.fromList [("namespace", String "tickets"), ("description", String "t"), ("hash", String "t-cc8")]) ]) ])
            emitIR rows = do
              writeIORef schemaOverride (Just (("", root) : rows))
              withSystemTempDirectory "cc8-cli" $ \dir -> withMockService todaysService $ \port -> do
                forM_ ["home", "cache", "run"] $ \d -> createDirectoryIfMissing True (dir </> d)
                env0 <- getEnvironment
                let env = [ (k, v) | (k, v) <- env0, k `notElem` ["HOME", "XDG_CACHE_HOME", "XDG_RUNTIME_DIR", "SYNAPSE_AS", "PLEXUS_SESSION"] ]
                       ++ [ ("HOME", dir </> "home"), ("XDG_CACHE_HOME", dir </> "cache"), ("XDG_RUNTIME_DIR", dir </> "run")
                          , ("PLEXUS_KEYCHAIN", "off"), ("SYNAPSE_HOLD_FOR", "3s") ]
                r <- readCreateProcessWithExitCode
                  (proc bin ["--url", "ws://127.0.0.1:" <> show port <> "/ws", "-i", "menger"]) { env = Just env } ""
                writeIORef schemaOverride Nothing
                pure r
        it "against a service with a bare object: the IR on stdout, the site on stderr, exit 65" $ do
          rows <- loadSchemasValues (fixtures </> "untyped.json")
          (code, out, err) <- emitIR rows
          code `shouldBe` ExitFailure 65
          err `shouldSatisfy` ("tickets.Thing.bare" `isInfixOf`)
          err `shouldSatisfy` ("neither properties nor additionalProperties" `isInfixOf`)
          -- nothing was left out: main's header
          err `shouldSatisfy` ("nothing was dropped" `isInfixOf`)
          case eitherDecode (BL.pack out) :: Either String IR of
            Left e -> expectationFailure ("stdout is not an IR: " <> e <> "\n" <> take 400 out)
            Right ir -> map rfSite (irRefusals ir) `shouldBe` ["tickets.Thing.bare", "tickets.Thing.list[]", "tickets.Thing.mixed"]
        -- RETURNS-36 review: a title collision leaves its activation out, so
        -- the header says so, by namespace, and never "nothing was dropped"
        it "against a service with two types under one title: the header says which activation was left out, exit 65" $ do
          let pong props = object07 ["title" .= ("Pong" :: Text), "type" .= ("object" :: Text), "properties" .= props]
              method n props = object07 [ "name" .= (n :: Text), "description" .= n, "hash" .= (n <> "0"), "streaming" .= False
                                        , "returns" .= pong props ]
              tickets = object07
                [ "namespace" .= ("tickets" :: Text), "version" .= ("1" :: Text), "description" .= ("t" :: Text), "hash" .= ("t-cc8" :: Text)
                , "methods" .= [ method "a" (object07 ["message" .= object07 ["type" .= ("string" :: Text)]])
                               , method "b" (object07 ["reply" .= object07 ["type" .= ("integer" :: Text)]]) ] ]
          (code, out, err) <- emitIR [("tickets", tickets)]
          code `shouldBe` ExitFailure 65
          err `shouldNotSatisfy` ("nothing was dropped" `isInfixOf`)
          err `shouldSatisfy` ("left out: tickets" `isInfixOf`)
          case eitherDecode (BL.pack out) :: Either String IR of
            Left e -> expectationFailure ("stdout is not an IR: " <> e <> "\n" <> take 400 out)
            Right ir -> map (\r -> (rfSite r, rfKeyword r)) (irRefusals ir) `shouldBe` [("tickets", "title")]

  -- RETURNS-40: synapse asks the service for its IR first — one call — and
  -- walks the raw schemas only when the service has no `ir`.
  describe "synapse -i, a service that writes its IR" $ do
    exe <- runIO builtSynapse
    case exe of
      Nothing -> it "asks ir" $ pendingWith "the built synapse is not on PATH (cabal test puts it there)"
      Just bin -> do
        let runI args = withSystemTempDirectory "r40-cli" $ \dir -> withMockService todaysService $ \port -> do
              forM_ ["home", "cache", "run"] $ \d -> createDirectoryIfMissing True (dir </> d)
              env0 <- getEnvironment
              let env = [ (k, v) | (k, v) <- env0, k `notElem` ["HOME", "XDG_CACHE_HOME", "XDG_RUNTIME_DIR", "SYNAPSE_AS", "PLEXUS_SESSION"] ]
                     ++ [ ("HOME", dir </> "home"), ("XDG_CACHE_HOME", dir </> "cache"), ("XDG_RUNTIME_DIR", dir </> "run")
                        , ("PLEXUS_KEYCHAIN", "off"), ("SYNAPSE_HOLD_FOR", "3s") ]
              writeIORef callsMade []
              writeIORef irAsked []
              r <- readCreateProcessWithExitCode
                (proc bin (["--url", "ws://127.0.0.1:" <> show port <> "/ws", "-i", "menger"] <> args)) { env = Just env } ""
              calls <- readIORef callsMade
              asked <- readIORef irAsked
              pure (r, calls, asked)
            served = emptyIR
              { irBackend = "theirs", irHash = Just "h-r40"
              , irPlugins = Map.fromList [("tickets", ["get"])]
              , irRefusals = [] }
            sameIR a b = (irTypes a, irMethods a, irPlugins a, irHash a, irRefusals a) `shouldBe` (irTypes b, irMethods b, irPlugins b, irHash b, irRefusals b)
        it "the root lists ir: its schema, then one call to ir, and its IR — named for the backend, with synapse's metadata" $ do
          writeIORef irAnswer (Just (Aeson.object ["ok" Aeson..= served]))
          ((code, out, err), calls, asked) <- runI []
          writeIORef irAnswer Nothing
          (code, err) `shouldBe` (ExitSuccess, "")
          calls `shouldBe` ["menger.schema", "menger.ir"]
          asked `shouldBe` [Aeson.object []]
          case eitherDecode (BL.pack out) :: Either String IR of
            Left e -> expectationFailure ("stdout is not an IR: " <> e)
            Right ir -> do
              ir `sameIR` served
              irBackend ir `shouldBe` "menger"
              irMetadata ir `shouldSatisfy` isJust
        it "a path is the namespace asked for" $ do
          writeIORef irAnswer (Just (Aeson.object ["ok" Aeson..= served]))
          (_, calls, asked) <- runI ["tickets"]
          writeIORef irAnswer Nothing
          calls `shouldBe` ["menger.schema", "menger.ir"]
          asked `shouldBe` [Aeson.object ["namespaces" Aeson..= (["tickets"] :: [Text])]]
        it "a refusal is the service's, said in its words, and nothing walked" $ do
          writeIORef irAnswer (Just (Aeson.object ["err" Aeson..= Aeson.object
            [ "type" Aeson..= ("not_found" :: Text), "namespace" Aeson..= ("delta" :: Text)
            , "message" Aeson..= ("no namespace delta in this service" :: Text) ]]))
          ((code, _, err), calls, _) <- runI []
          writeIORef irAnswer Nothing
          code `shouldNotBe` ExitSuccess
          err `shouldSatisfy` ("no namespace delta in this service" `isInfixOf`)
          calls `shouldBe` ["menger.schema", "menger.ir"]
        it "an IR of another version is not read: the walk builds it" $ do
          writeIORef irAnswer (Just (Aeson.object ["ok" Aeson..= served { irVersion = "2.1" }]))
          ((code, out, _), calls, _) <- runI []
          writeIORef irAnswer Nothing
          code `shouldBe` ExitSuccess
          take 2 calls `shouldBe` ["menger.schema", "menger.ir"]
          fmap irVersion (eitherDecode (BL.pack out) :: Either String IR) `shouldBe` Right currentIrVersion
        it "a root that lists no ir (0.7.0): no call to ir, the walk as before" $ do
          ((code, out, _), calls, _) <- runI []
          code `shouldBe` ExitSuccess
          calls `shouldSatisfy` notElem "menger.ir"
          take 1 calls `shouldBe` ["menger.schema"]
          length (filter (".schema" `T.isSuffixOf`) calls) `shouldSatisfy` (> 1)
          fmap irVersion (eitherDecode (BL.pack out) :: Either String IR) `shouldBe` Right currentIrVersion

-- ---------------------------------------------------------------------------

fixtures :: FilePath
fixtures = "test/fixtures/ir-coverage"

irOf :: FilePath -> [Text] -> IO IR
irOf file = fixtureIR (fixtures </> file)

mengerIR :: IO IR
mengerIR = fixtureIR menger20261029 []

smallFixtures :: [(FilePath, [Text])]
smallFixtures =
  [ ("transition-field.json", ["tickets"]), ("answer-source.json", ["tickets"]), ("reply.json", ["tickets"])
  , ("hash-event.json", ["tickets"]), ("login-flow.json", ["auth"]), ("schema-result.json", ["tickets"])
  , ("unreadable-oneof.json", ["tickets"]), ("static-child.json", ["tickets"])
  , ("hashes.json", ["tickets"]), ("injected.json", ["tickets"])
  , ("untyped.json", ["tickets"]), ("json.json", ["tickets"]), ("theme-map.json", ["themes"])
  , ("component-preview.json", ["tickets"]), ("bounds.json", ["tickets"]), ("allof.json", ["tickets"])
  , ("multipleof.json", ["tickets"]), ("variant-default.json", ["tickets"]), ("input.json", ["tickets"])
  , ("shared.json", []) ]

-- | The walk's 21 (SYNAPSE-CC-8, the owner's JSON type of 2026-10-29): the
--   21 schema methods' returns. The protocol root's three untyped values
--   are the JSON type now, not refusals.
expectedMengerRefusals :: [(Text, Text)]
expectedMengerRefusals = sort $
  [ (ns <> ".schema returns", "returns")
  | ns <- [ "", "auth", "chats", "chats.inputs", "comments", "deployments", "epics", "media", "members", "notes"
          , "orgs", "session", "themes", "tickets", "tickets.commits", "tickets.components", "tickets.filters"
          , "tickets.inputs", "tickets.nav", "tickets.revisions", "tickets.work" ] ]

named :: Text -> Text -> TypeRef
named ns n = RefNamed (QualifiedName ns n)

object07 :: [(K.Key, Value)] -> Value
object07 = Object . KM.fromList

obj :: [(Text, Text)] -> Value
obj kvs = Object (KM.fromList [ (K.fromText k, String v) | (k, v) <- kvs ])

kindOf :: IR -> Text -> Maybe TypeKind
kindOf ir n = tdKind <$> Map.lookup n (irTypes ir)

method :: IR -> Text -> Maybe MethodDef
method ir p = Map.lookup p (irMethods ir)

param :: IR -> Text -> Text -> Maybe TypeRef
param ir m p = case method ir m of
  Just md -> case [ pdType x | x <- mdParams md, pdName x == p ] of
    (t : _) -> Just t
    [] -> Nothing
  Nothing -> Nothing

structField :: IR -> Text -> Text -> Maybe TypeRef
structField ir t f = case kindOf ir t of
  Just (KindStruct fs) -> case [ fdType x | x <- fs, fdName x == f ] of
    (ty : _) -> Just ty
    [] -> Nothing
  _ -> Nothing

fieldType :: IR -> Text -> Text -> Text -> Maybe TypeRef
fieldType ir t v f = case kindOf ir t of
  Just (KindEnum _ vs _) -> case [ fdType x | var <- vs, vdName var == v, x <- vdFields var, fdName x == f ] of
    (ty : _) -> Just ty
    [] -> Nothing
  _ -> Nothing

-- | Every ref of the IR, at its site.
refsWithSites :: IR -> [(Text, TypeRef)]
refsWithSites ir = concatMap methodRefs (Map.elems (irMethods ir)) ++ concatMap typeRefs (Map.toList (irTypes ir))
  where
    methodRefs md =
      [ (mdFullPath md <> "(" <> pdName p <> ")", pdType p) | p <- mdParams md ]
      ++ [ (mdFullPath md <> " returns", mdReturns md) ]
      ++ [ (mdFullPath md <> " input", t) | Just t <- [mdInput md] ]
    typeRefs (n, td) = case tdKind td of
      KindStruct fs -> [ (n <> "." <> fdName f, fdType f) | f <- fs ]
      KindEnum _ vs _ -> [ (n <> "." <> vdName v <> "." <> fdName f, fdType f) | v <- vs, f <- vdFields v ]
                      ++ [ (n <> "." <> vdName v, t) | v <- vs, Just t <- [vdRef v] ]
      KindAlias t -> [(n, t)]
      _ -> []

-- | The sites of every RefUnknown, at any depth.
unknownSites :: IR -> [Text]
unknownSites ir = concatMap (uncurry go) (refsWithSites ir)
  where
    go site = \case
      RefUnknown -> [site]
      RefArray t -> go (site <> "[]") t
      RefOptional t -> go (site <> "?") t
      RefMap t -> go (site <> "{}") t
      RefRefined t _ -> go site t
      RefTuple ts -> concat (zipWith (\i t -> go (site <> "." <> T.pack (show i)) t) [0 :: Int ..] ts)
      _ -> []

-- | Every RefNamed that names no type.
danglingRefs :: IR -> [Text]
danglingRefs ir = [ full | (_, t) <- refsWithSites ir, full <- names t, not (Map.member full (irTypes ir)) ]
  where
    names = \case
      RefNamed qn -> [qnNamespace qn <> "." <> qnLocalName qn]
      t -> concatMap names (typeRefChildren t)

-- | A fixture's schemas as JSON, by dotted path (for the mock service).
loadSchemasValues :: FilePath -> IO [(Text, Value)]
loadSchemasValues file = do
  Right rows <- eitherDecode <$> BL.readFile file :: IO (Either String [Value])
  pure [ (T.intercalate "." p, s) | Object r <- rows, Just pv <- [KM.lookup "path" r], Aeson.Success p <- [Aeson.fromJSON pv], Just s <- [KM.lookup "schema" r] ]

-- | The synapse this suite was built with: cabal test puts it on PATH; one
--   from elsewhere (an installed one) is not it.
builtSynapse :: IO (Maybe FilePath)
builtSynapse = do
  found <- findExecutable "synapse"
  pure $ case found of
    Just p | "dist-newstyle" `isInfixOf` p -> Just p
    _ -> Nothing

-- | A construct IR 2.0 did not type (and 2.1 now does, or refuses): an
--   empty enum, a RefAny, a RefUnknown, an untyped object or array.
changedIn20 :: Value -> Bool
changedIn20 = \case
  Object o
    | KM.lookup "tag" o `elem` [Just "RefAny", Just "RefUnknown"] -> True
    | KM.lookup "tag" o == Just "RefPrimitive"
    , Just (Array a) <- KM.lookup "contents" o, (String t : _) <- foldr (:) [] a, t `elem` ["object", "array"] -> True
    | KM.lookup "tag" o == Just "KindEnum", Just (Array vs) <- KM.lookup "keVariants" o, null vs -> True
    | KM.lookup "tag" o == Just "KindStringEnum", Just (Array vs) <- KM.lookup "kseValues" o, null vs -> True
    | KM.lookup "tag" o == Just "KindPrimitive", KM.lookup "kpType" o == Just "any" -> True
    | otherwise -> any changedIn20 (KM.elems o)
  Array a -> any changedIn20 a
  _ -> False

-- | JSON with every ref's namespace blanked, and the fields of variants
--   without fdRequired / fdDefault.
norm :: Value -> Value
norm = \case
  Object o
    | Just (String "RefNamed") <- KM.lookup "tag" o -> Object (adjustKM blankNs "contents" o)
    | Just (Array vs) <- KM.lookup "keVariants" o -> Object (KM.map norm (KM.insert "keVariants" (Array (fmap variant vs)) o))
    | otherwise -> Object (KM.map norm o)
  Array a -> Array (fmap norm a)
  v -> v
  where
    blankNs (Object q) = Object (KM.insert "qnNamespace" (String "") q)
    blankNs v = v
    variant (Object v) = Object (adjustKM (\case { Array fs -> Array (fmap (dropKeys ["fdRequired", "fdDefault"]) fs); x -> x }) "vdFields" v)
    variant v = v

dropKeys :: [Text] -> Value -> Value
dropKeys ks (Object o) = Object (foldr (KM.delete . K.fromText) o ks)
dropKeys _ v = v

-- | A 2.0 method without the params 2.1 moves to mdInjected.
dropInjected :: Value -> Value
dropInjected (Object o) = Object (adjustKM keep "mdParams" o)
  where
    keep (Array ps) = Array (fmap id (foldr (\p acc -> if injected p then acc else p `consV` acc) mempty ps))
    keep v = v
    consV x xs = pure x <> xs
    injected (Object p) = case KM.lookup "pdSource" p of
      Just (Object s) -> KM.lookup "from" s /= Just "rpc"
      _ -> False
    injected _ = False
dropInjected v = v

-- | A JSON encoding holds this text.
contains :: String -> BL.ByteString -> Bool
contains needle hay = needle `isInfixOf` BL.unpack hay

adjustKM :: (Value -> Value) -> K.Key -> KM.KeyMap Value -> KM.KeyMap Value
adjustKM f k o = maybe o (\v -> KM.insert k (f v) o) (KM.lookup k o)
