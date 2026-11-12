-- | The source route and its body (Note [Source view] in
-- "HaskellReviewer.Server.Wire.Source").
module HaskellReviewer.Server.SourceTest (tests) where

import ApiSupport
  ( ApiResult (..),
    TestApi (..),
    arr,
    bool,
    cborValue,
    field,
    gatedOn,
    getApi,
    indexDeps,
    indexed,
    int,
    requestApi,
    sourceApi,
    sourceTexts,
    str,
  )
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (poll, wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.STM (atomically, check)
import Control.Monad (forM_, void, when)
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileContent (FileContent (..), LinkInfo (..))
import HaskellReviewer.Core.Highlight (DisplayClass, linkedRunWidth)
import HaskellReviewer.Core.ModuleModel
  ( ParseError (..),
    TextOnlyReason (..),
    textOnlyModule,
  )
import HaskellReviewer.Core.Modules (moduleAtPath)
import HaskellReviewer.Core.ResolveWarmUp (resolveWarmUpIndex)
import HaskellReviewer.Core.SourceLinks (GuessHow, LinkReason, OutlineKind)
import HaskellReviewer.Effects.FileSystem (ListError (..), ReadError (..))
import HaskellReviewer.Server.Api (computed)
import HaskellReviewer.Server.Wire.Source
  ( SourceBody (..),
    coveringView,
    encodeSource,
    howName,
    outlineKindName,
    reasonName,
    sourceWarmUp,
    textOnlySource,
  )
import HaskellReviewer.Session.Index
  ( SlotState (..),
    isRequested,
    requestCount,
    runWorkers,
    slotState,
  )
import HaskellReviewer.Session.Snapshot (Generation (..), Snapshot (..))
import IndexSupport (isParsing, rel, waitState, within)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Source API"
    [ testCase
        "warm-up: every kind, reason, how, class, region, outline, sig, def"
        coverage,
      testCase "text only: lexed if it has tokens or regions, else plain" textOnlyCase,
      testGroup "route" routeTests
    ]

-- * Warm-up

coverage :: Assertion
coverage = do
  views <- mapM (cborValue . encodeSource (Generation 1) resolveWarmUpIndex) sourceWarmUp
  cover <- cborValue (encodeSource (Generation 1) resolveWarmUpIndex coveringView)
  sort (Set.toList (Set.fromList [str (field "highlight" v) | v <- views]))
    @?= ["lexed", "plain"]
  bool (field "partial" cover) @?= True
  let targets = arr (field "targets" cover)
      kinds = Set.fromList [str (head' t) | t <- map arr targets]
      runs = map int (arr (field "runs" cover))
      runAt k j = runs !! (linkedRunWidth * k + j)
      nRuns = length runs `div` linkedRunWidth
      regionKinds =
        Set.fromList
          [ int r
          | (i, r) <- zip [0 :: Int ..] (arr (field "regions" cover)),
            i `mod` 3 == 0
          ]
      reasons =
        Set.fromList
          [ str r
          | t <- map arr targets,
            str (head' t) `elem` ["decl", "module"],
            r <- arr (t !! (if str (head' t) == "decl" then 5 else 3))
          ]
      externals = [t | t <- map arr targets, str (head' t) == "external"]
      outline = [str (head' (arr e)) | e <- arr (field "outline" cover)]
  Set.toList kinds @?= sort ["local", "decl", "module", "external"]
  reasons @?= Set.fromList (map reasonName [minBound .. maxBound :: LinkReason])
  Set.fromList [str (t !! 3) | t <- externals]
    @?= Set.fromList (map howName [minBound .. maxBound :: GuessHow])
  Set.fromList [isNull (t !! 2) | t <- externals] @?= Set.fromList [False, True]
  Set.fromList [runAt k 4 | k <- [0 .. nRuns - 1]]
    @?= Set.fromList [0 .. fromEnum (maxBound :: DisplayClass)]
  regionKinds @?= Set.fromList [0, 1, 2]
  Set.fromList outline
    @?= Set.fromList (map outlineKindName [minBound .. maxBound :: OutlineKind])
  -- Instance labels.
  let labels = Set.fromList [str (arr e !! 2) | e <- arr (field "outline" cover)]
  mapM_
    (\l -> assertBool (Text.unpack l) (Set.member l labels))
    [ "instance C Int Int",
      "instance C [a] Int",
      "type instance O Int",
      "data instance V Int",
      "newtype instance V Bool"
    ]
  assertBool "a link" (any (\k -> runAt k 5 /= 0) [0 .. nRuns - 1])
  -- Every shape of sig, a cut one and none.
  let sigs = [t !! 7 | t <- map arr targets, str (head' t) == "decl"]
      texts = Set.fromList [str g | g <- sigs, not (isNull g)]
  forM_ sigShapes $ \g -> assertBool (show g) (Set.member g texts)
  assertBool "a cut sig" (any (Text.isSuffixOf "\x2026") texts)
  assertBool "a null sig" (any isNull sigs)
  -- The def bit, both ways.
  Set.fromList [bool (t !! 8) | t <- map arr targets, str (head' t) == "decl"]
    @?= Set.fromList [False, True]
  -- Every link is in the table.
  assertBool "links in range" (all (\k -> runAt k 5 <= length targets) [0 .. nRuns - 1])

-- | One sig of each shape, from the warm-up's sigs module.
sigShapes :: [Text]
sigShapes =
  [ "v :: Int -> Int",
    "pattern P :: Int",
    "m :: a -> Int",
    "K a",
    "L {fld :: Int}",
    "fld :: Int",
    "DI Int",
    "data T a",
    "newtype N",
    "type S = Int",
    "class C a",
    "type E a",
    "type family F a",
    "type family G a",
    "data GA a",
    "GK :: a -> GA a",
    "data family D a"
  ]

textOnlyCase :: Assertion
textOnlyCase = do
  case textOnlySource (textOnlyModule TooSlow []) of
    PlainBody -> pure ()
    LexedBody _ -> assertFailure "too slow: not plain"
  -- A parse error without tokens: nothing to show.
  case textOnlySource (textOnlyModule (ParseFailed err Nothing) []) of
    PlainBody -> pure ()
    LexedBody _ -> assertFailure "no tokens: not plain"
  where
    err = ParseError {message = "x", code = Nothing, span = Nothing}

-- * The route

routeTests :: [TestTree]
routeTests =
  [ testCase "a module: lexed, linked into another module, not partial" $ do
      -- Indexed first: A is ready.
      v <- okSource (indexed sourceApi) "s/src/B.hs"
      (str (field "path" v), str (field "ref" v), bool (field "partial" v))
        @?= ("s/src/B.hs", "s:lib:B", False)
      str (field "highlight" v) @?= "lexed"
      let targets = map arr (arr (field "targets" v))
          decls = [(str (t !! 1), str (t !! 4)) | t <- targets, str (head' t) == "decl"]
      assertBool (show decls) (("A", "val:area") `elem` decls)
      assertBool (show decls) (("A", "type:Shape") `elem` decls)
      -- Sigs: from A's text, through the states resolution read.
      [str (t !! 7) | t <- targets, str (head' t) == "decl", str (t !! 4) == "val:area"]
        @?= ["area :: Shape -> Double"]
      -- The outline.
      [str (arr e !! 1) | e <- arr (field "outline" v)] @?= ["val:sorted"],
    testCase "the file of two components: the first in tree order" $ do
      v <- okSource sourceApi "s/src/B.hs"
      -- Not s:test:t:B.
      str (field "ref" v) @?= "s:lib:B",
    testCase "plain: not a module, boot, symlink, submodule, generated, ..." $
      mapM_
        ( \(p, ref) -> do
            v <- okSource sourceApi p
            (p, str (field "highlight" v)) @?= (p, "plain")
            (p, maybeStr (field "ref" v)) @?= (p, ref)
            (p, fieldCount v) @?= (p, 6)
        )
        [ ("s/notes.txt", Nothing),
          ("s/src/A.hs-boot", Nothing),
          ("s/src/Link.hs", Nothing),
          ("s/vendor", Nothing),
          ("s/src/Lexer.x", Just "Lexer"),
          ("s/src/Tool.hs", Just "Tool"),
          ("s/src/Bin.hs", Just "Bin"),
          ("s/src/Big.hs", Just "Big"),
          ("s/src/Slow.hs", Just "Slow")
        ],
    testCase "a parse error: lexed, no targets" $ do
      v <- okSource sourceApi "s/src/Bad.hs"
      str (field "highlight" v) @?= "lexed"
      arr (field "targets" v) @?= []
      arr (field "outline" v) @?= []
      assertBool "runs" (not (null (arr (field "runs" v)))),
    testCase "CPP: regions" $ do
      v <- okSource sourceApi "s/src/Cpp.hs"
      map int (arr (field "regions" v)) @?= [0, 3, 3, 1, 4, 4, 0, 5, 5],
    testCase "not found: unknown path, %2F, bad paths; JSON body" $
      mapM_
        ( \segs -> do
            r <- getApi sourceApi (["api", "source", "tree"] <> segs)
            (segs, r.status) @?= (segs, 404)
            lookup "Content-Type" r.headers @?= Just "application/json"
            r.body @?= fileNotFound
        )
        [ [],
          ["s", "nope.hs"],
          ["s/src/B.hs"],
          ["s", "..", "s", "notes.txt"],
          ["s", "src"]
        ],
    testCase "a file gone: 404 JSON; unreadable: 500; crash: 500" $ do
      let with e = sourceApi {readEntry = \_ _ _ -> pure (Left e)}
          path = ["api", "source", "tree", "s", "src", "B.hs"]
      gone <- getApi (with ReadGone) path
      (gone.status, gone.body) @?= (404, fileNotFound)
      failed <- getApi (with (ReadFailed "Permission denied")) path
      (failed.status, failed.body) @?= (500, "cannot read file")
      crashed <- getApi sourceApi {parse = \_ _ -> ioError (userError "boom")} path
      (crashed.status, crashed.body) @?= (500, "cannot index module")
      let link = Link LinkInfo {target = "x.hs", resolved = Nothing}
      became <- getApi sourceApi {readEntry = \_ _ _ -> pure (Right link)} path
      became.status @?= 404,
    testCase "errors: other side, not GET, listing failed" $ do
      r1 <- getApi sourceApi ["api", "source", "head", "s", "src", "B.hs"]
      (r1.status, r1.body) @?= (404, "not found")
      mapM_
        ( \m -> do
            r <- requestApi m sourceApi ["api", "source", "tree", "s", "src", "B.hs"]
            (m, r.status, lookup "Allow" r.headers) @?= (m, 405, Just "GET")
        )
        ["HEAD", "POST"]
      let unlisted = sourceApi {snapshot = pure (Left (ListCrashed "x"))}
      r2 <- getApi unlisted ["api", "source", "tree", "s/x"]
      (r2.status, r2.body) @?= (500, "cannot list files"),
    testCase "waits for its own slot" $ do
      gate <- newEmptyMVar
      withAsync (okSource (gatedOn "s/src/B.hs" gate) "s/src/B.hs") $ \pending -> do
        -- B can't be parsed yet: no answer. A lower bound only.
        threadDelay 200_000
        early <- poll pending
        assertBool "answered before B was parsed" (null early)
        putMVar gate ()
        v <- wait pending
        str (field "highlight" v) @?= "lexed"
        assertBool "linked" (not (null (arr (field "targets" v)))),
    testCase "an import not ready: partial, then complete" $ do
      gate <- newEmptyMVar
      -- Two workers: one is stuck on A.
      let api = (gatedOn "s/src/A.hs" gate) {workers = 2}
      first <- okSource api "s/src/B.hs"
      bool (field "partial" first) @?= True
      let modules =
            [ t
            | t <- map arr (arr (field "targets" first)),
              str (head' t) == "module"
            ]
      assertBool (show modules) (any (\t -> str (t !! 1) == "A") modules)
      -- A parsed: a new snapshot, indexed before the request.
      putMVar gate ()
      second <- okSource (indexed api) "s/src/B.hs"
      bool (field "partial" second) @?= False,
    testCase "gate on: answered while indexing, from Pending" gateCase,
    testCase "the body is built inside the gate" $ do
      snap <- sourceApi.snapshot >>= either (assertFailure . show) pure
      entered <- newEmptyMVar
      go <- newEmptyMVar
      let mk = putMVar entered () >> takeMVar go >> pure "body"
      withAsync (computed snap mk) $ \c -> do
        within (takeMVar entered)
        atomically (requestCount snap.index) >>= (@?= 1)
        putMVar go ()
        void (within (wait c))
      atomically (requestCount snap.index) >>= (@?= 0)
  ]

-- | Two workers, held on A and B. Cpp is Pending when asked for. B goes,
-- a worker parses Cpp, then parks while the request computes. A is
-- still held, so indexing goes on. With the gate around the wait for
-- Cpp, this hangs.
gateCase :: Assertion
gateCase = do
  snap <- sourceApi.snapshot >>= either (assertFailure . show) pure
  holdA <- newEmptyMVar
  holdB <- newEmptyMVar
  let held p gate t = when (Just t == lookup p sourceTexts) (readMVar gate)
      -- Our own workers: the request must come once they are busy.
      api =
        sourceApi
          { snapshot = pure (Right snap),
            parse = \settings t -> do
              held "s/src/A.hs" holdA t
              held "s/src/B.hs" holdB t
              sourceApi.parse settings t,
            workers = 0
          }
      idOf p = fromMaybe (error (show p)) (moduleAtPath snap.modules (rel p))
      a = idOf "s/src/A.hs"
      cpp = idOf "s/src/Cpp.hs"
  withAsync (runWorkers (indexDeps api snap) 2 snap.index) $ \_ -> do
    waitState snap.index a isParsing
    waitState snap.index (idOf "s/src/B.hs") isParsing
    withAsync (okSource api "s/src/Cpp.hs") $ \pending -> do
      within (atomically (isRequested snap.index cpp >>= check))
      atomically (slotState snap.index cpp) >>= (@?= Just Pending)
      putMVar holdB ()
      v <- within (wait pending)
      str (field "highlight" v) @?= "lexed"
      atomically (slotState snap.index a) >>= (@?= Just Parsing)
      putMVar holdA ()

-- | The 404 body of the source route.
fileNotFound :: LBS.ByteString
fileNotFound = "{\"generation\":1,\"kind\":\"not-found\",\"what\":\"file\"}"

-- | GET api/source/tree/<path>: a 200 with CBOR, as JSON.
okSource :: TestApi -> Text -> IO Aeson.Value
okSource api p = do
  r <- getApi api (["api", "source", "tree"] <> pathSegments p)
  r.status @?= 200
  lookup "Content-Type" r.headers @?= Just "application/cbor"
  cborValue r.body
  where
    pathSegments = filter (/= "") . Text.splitOn "/"

-- * JSON helpers

-- | How many fields an object has.
fieldCount :: Aeson.Value -> Int
fieldCount = \case
  Aeson.Object o -> KeyMap.size o
  _ -> 0

maybeStr :: Aeson.Value -> Maybe Text
maybeStr = \case
  Aeson.String t -> Just t
  _ -> Nothing

isNull :: Aeson.Value -> Bool
isNull = (== Aeson.Null)

head' :: [Aeson.Value] -> Aeson.Value
head' = \case
  x : _ -> x
  [] -> Aeson.Null
