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
import Data.Aeson (Value(..), eitherDecode, encode, toJSON)
import qualified Data.Aeson as Aeson
import Data.Aeson.Encode.Pretty (Config(..), defConfig, encodePretty')
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.IORef (writeIORef)
import Data.List (isInfixOf, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode)
import Test.Hspec

import Synapse.IR.Types
import Synapse.SchemaStore (Plan(..), Store(..), readPlan, writePlan)
import Plexus.Schema.Recursive (MethodSchema(..))

import IRFixture
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

    it "an rpc method with no returns is refused" $ do
      ir <- irOf "no-returns.json" ["x"]
      irRefusals ir `shouldBe` [Refusal "x.schema returns" "returns" Nothing "an rpc method with no returns schema"]
      mdReturns <$> method ir "x.schema" `shouldBe` Just RefUnknown

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

    it "mdInput: the schema's input as a type; absent, no key" $ do
      ir <- irOf "input.json" ["tickets"]
      mdInput <$> method ir "tickets.chat" `shouldBe` Just (Just (named "tickets" "Token"))
      mdInput <$> method ir "tickets.plain" `shouldBe` Just Nothing
      let Just plain = method ir "tickets.plain"
      encode plain `shouldNotSatisfy` (contains "mdInput")
      encode plain `shouldNotSatisfy` (contains "mdInjected")
      kindOf ir "tickets.Token" `shouldSatisfy` isJust
      -- plexus-protocol reads it, and writes it back only when present
      let Right ms = Aeson.eitherDecode "{\"name\":\"chat\",\"input\":{\"type\":\"string\"}}" :: Either String MethodSchema
      methodInput ms `shouldBe` Just (obj [("type", "string")])
      encode ms `shouldSatisfy` (contains "\"input\"")
      let Right bare = Aeson.eitherDecode "{\"name\":\"plain\"}" :: Either String MethodSchema
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

  describe "menger, 2026-10-29, whole" $ do
    it "is refused at exactly the walk's 21 sites; the root's carriers are the JSON type" $ do
      ir <- mengerIR
      map (\r -> (rfSite r, rfKeyword r)) (irRefusals ir) `shouldBe` expectedMengerRefusals
      sort (unknownSites ir) `shouldBe` sort (map fst expectedMengerRefusals)
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
      length [ () | md <- Map.elems (irMethods ir), ip <- mdInjected md ] `shouldBe` 176
      [ pdName p | md <- Map.elems (irMethods ir), p <- mdParams md, pdName p `elem` ["auth", "user"] ] `shouldBe` []

    it "equals its golden (menger-2026-10-29.ir-2.1.json)" $ do
      ir <- mengerIR
      let golden = fixtures </> "menger-2026-10-29.ir-2.1.json"
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
      -- methods: the same JSON but for the keys 2.1 adds and the injected params it moves
          methodMismatches =
            [ "method " <> T.unpack key <> ":\n  old " <> show old' <> "\n  new " <> show new'
            | (k, v) <- KM.toList oldMethods
            , let key = K.toText k
                  old' = norm (dropInjected v)
                  new' = maybe Null (norm . dropKeys ["mdInjected", "mdInput"] . toJSON) (Map.lookup key (irMethods ir))
            , not (changedIn20 (dropInjected v)), old' /= new' ]
      mapM_ putStrLn (take 5 (typeMismatches ++ methodMismatches))
      (length typeMismatches, length methodMismatches) `shouldBe` (0, 0)
      -- and the guard is not vacuous: most of 2.0 was typed
      length [ () | (_, v) <- KM.toList oldTypes, not (changedIn20 v) ] `shouldSatisfy` (> 150)
      length [ () | (_, v) <- KM.toList oldMethods, not (changedIn20 (dropInjected v)) ] `shouldSatisfy` (> 100)

  describe "caches another synapse wrote" $
    it "a plan whose IR is 2.0 (3.16.0's) is read as no plan" $
      withSystemTempDirectory "cc8-store" $ \dir -> do
        let st = Store dir
            Right ms = Aeson.eitherDecode "{\"name\":\"get\"}" :: Either String MethodSchema
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
      Just bin -> it "against a service with a bare object: the IR on stdout, the site on stderr, exit 65" $ do
        rows <- loadSchemasValues (fixtures </> "untyped.json")
        let root = Object (KM.fromList
              [ ("namespace", String "menger"), ("version", String "1.0.0"), ("description", String "root")
              , ("hash", String "r-cc8"), ("methods", Array mempty)
              , ("children", toJSON [ Object (KM.fromList [("namespace", String "tickets"), ("description", String "t"), ("hash", String "t-cc8")]) ]) ])
        writeIORef schemaOverride (Just (("", root) : rows))
        withSystemTempDirectory "cc8-cli" $ \dir -> withMockService todaysService $ \port -> do
          forM_ ["home", "cache", "run"] $ \d -> createDirectoryIfMissing True (dir </> d)
          env0 <- getEnvironment
          let env = [ (k, v) | (k, v) <- env0, k `notElem` ["HOME", "XDG_CACHE_HOME", "XDG_RUNTIME_DIR", "SYNAPSE_AS", "PLEXUS_SESSION"] ]
                 ++ [ ("HOME", dir </> "home"), ("XDG_CACHE_HOME", dir </> "cache"), ("XDG_RUNTIME_DIR", dir </> "run")
                    , ("PLEXUS_KEYCHAIN", "off"), ("SYNAPSE_HOLD_FOR", "3s") ]
          (code, out, err) <- readCreateProcessWithExitCode
            (proc bin ["--url", "ws://127.0.0.1:" <> show port <> "/ws", "-i", "menger"]) { env = Just env } ""
          writeIORef schemaOverride Nothing
          code `shouldBe` ExitFailure 65
          err `shouldSatisfy` ("tickets.Thing.bare" `isInfixOf`)
          err `shouldSatisfy` ("neither properties nor additionalProperties" `isInfixOf`)
          case eitherDecode (BL.pack out) :: Either String IR of
            Left e -> expectationFailure ("stdout is not an IR: " <> e <> "\n" <> take 400 out)
            Right ir -> map rfSite (irRefusals ir) `shouldBe` ["tickets.Thing.bare", "tickets.Thing.list[]", "tickets.Thing.mixed"]

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
  , ("unreadable-oneof.json", ["tickets"]), ("static-child.json", ["tickets"]), ("no-returns.json", ["x"])
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
