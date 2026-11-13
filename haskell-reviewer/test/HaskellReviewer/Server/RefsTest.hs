-- | The find references route, in process (Note [Refs route] in
-- "HaskellReviewer.Server.Refs"). The fixtures cover the bodies.
--
-- In the "API" group (Q-RTS-BLACKHOLE).
module HaskellReviewer.Server.RefsTest (tests) where

import ApiSupport
  ( ApiResult (..),
    TestApi (..),
    getApiQuery,
    indexDeps,
    indexed,
    refsApi,
    refsGatedOn,
    requestApi,
    sourceApi,
  )
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar)
import Control.Monad (forM_, (>=>))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseMaybe, withObject, (.:))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import HaskellReviewer.Effects.FileSystem (ListError (..))
import HaskellReviewer.Session.Index (runWorkers)
import HaskellReviewer.Session.Snapshot (Snapshot (index))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "refs"
    [ testCase "JSON with a length; home first, then by module name" $ do
        r <- getApiQuery (indexed refsApi) refsPath "?module=R.Shape&decl=val:area"
        r.status @?= 200
        lookup "Content-Type" r.headers @?= Just "application/json"
        lookup "Content-Length" r.headers
          @?= Just (Char8.pack (show (LBS.length r.body)))
        a <- decodeOr r.body
        a.partial @?= False
        a.total @?= length a.results
        dedupe (map (.ref) a.results) @?= ["R.Shape", "R.Guess", "R.Use"],
      testCase "the long ref works; module is the short one" $ do
        a <- refsOk (indexed refsApi) "?module=r%3Alib%3AR.Shape&decl=val:area"
        a.module_ @?= "R.Shape"
        assertBool "no results" (not (null a.results)),
      testCase "a + stays a +, encoded or not" $
        forM_ ["?module=R.Shape&decl=val:<+>", "?module=R.Shape&decl=val%3A%3C%2B%3E"] $
          \q -> do
            a <- refsOk (indexed refsApi) q
            (q, a.decl) @?= (q, "val:<+>")
            (q, [(x.ref, x.kind) | x <- a.results])
              @?= ( q,
                    [ ("R.Shape", "export"),
                      ("R.Shape", "binding"),
                      ("R.Shape", "binding"),
                      ("R.Use", "use")
                    ]
                  ),
      testCase "a field pun is a use of the field" $ do
        a <- refsOk (indexed refsApi) "?module=R.Shape&decl=field:Pt.px"
        [(x.ref, x.span, x.kind) | x <- a.results, x.ref == "R.Use"]
          @?= [("R.Use", [16, 7, 16, 9], "use")],
      testCase "a quasi-quote is a use of its quoter, over the whole quote" $ do
        a <- refsOk (indexed refsApi) "?module=R.QQ&decl=val:qq"
        [(x.ref, x.span, x.kind) | x <- a.results]
          @?= [ ("R.QQ", [1, 14, 1, 16], "export"),
                ("R.QQ", [3, 1, 3, 3], "binding"),
                ("R.Quote", [6, 10, 6, 25], "use")
              ],
      testCase "1001 references: the first 1000, total 1001" $ do
        a <- refsOk (indexed refsApi) "?module=R.Shape&decl=val:big"
        a.total @?= 1001
        length a.results @?= 1000
        map (.kind) (take 3 a.results) @?= ["export", "binding", "binding"]
        (last a.results).span @?= [1002, 5, 1002, 8],
      testCase "partial while a module isn't parsed, complete after" $ do
        gate <- newEmptyMVar
        let gated = refsGatedOn "r/src/R/Use.hs" gate
        snap <- either (assertFailure . show) pure =<< gated.snapshot
        let api = gated {snapshot = pure (Right snap), workers = 0}
            q = "?module=R.Shape&decl=con:Square"
        withAsync (runWorkers (indexDeps gated snap) 2 snap.index) $ \workers -> do
          before <- refsOk api q
          before.partial @?= True
          map (.ref) before.results @?= ["R.Shape", "R.Shape"]
          putMVar gate ()
          wait workers
        after <- refsOk api q
        after.partial @?= False
        map (.ref) after.results @?= ["R.Shape", "R.Shape", "R.Use"],
      testCase "bad params: 400, before anything is read"
        $ forM_
          [ "",
            "?module=R.Shape",
            "?decl=val:area",
            "?module&decl=val:area",
            "?module=%FF&decl=val:area",
            "?module=R.Shape&decl=%FF"
          ]
        $ \q -> do
          -- No workers and a failing listing: the 400 comes first.
          r <- getApiQuery refsApi {workers = 0, snapshot = listFails} refsPath q
          (q, r.status, r.body) @?= (q, 400, "bad query"),
      testCase "not found: 404 with JSON, module or decl" $ do
        let notFound api q what = do
              r <- getApiQuery api refsPath q
              (q, r.status) @?= (q, 404)
              lookup "Content-Type" r.headers @?= Just "application/json"
              r.body
                @?= "{\"generation\":1,\"kind\":\"not-found\",\"what\":\""
                  <> what
                  <> "\"}"
        -- Unknown: at once, nothing parsed.
        notFound refsApi {workers = 0} "?module=Nope&decl=val:x" "module"
        -- Generated, text only.
        notFound (indexed sourceApi) "?module=Lexer&decl=val:x" "module"
        notFound (indexed sourceApi) "?module=Bad&decl=val:x" "module"
        notFound (indexed refsApi) "?module=R.Shape&decl=val:nope" "decl"
        -- A name of another module.
        notFound (indexed refsApi) "?module=R.Shape&decl=val:guess" "decl"
        -- Instances have no name.
        notFound (indexed refsApi) "?module=R.Shape&decl=inst:HasArea:Shape" "decl",
      testCase "the first of each param counts" $ do
        a <- refsOk (indexed refsApi) "?decl=val:big&module=R.Shape&module=R.Use&decl=x"
        a.decl @?= "val:big",
      testCase "not GET: 405, Allow: GET; another side: 404; listing failed: 500" $ do
        forM_ ["HEAD", "POST", "PUT"] $ \m -> do
          r <- requestApi m refsApi {workers = 0} refsPath
          (m, r.status) @?= (m, 405)
          lookup "Allow" r.headers @?= Just "GET"
        base <- getApiQuery refsApi {workers = 0} ["api", "refs", "base"] okQuery
        base.status @?= 404
        failed <- getApiQuery refsApi {workers = 0, snapshot = listFails} refsPath okQuery
        failed.status @?= 500
        failed.body @?= "cannot list files"
    ]
  where
    okQuery = "?module=R.Shape&decl=val:area"
    listFails = pure (Left (ListCrashed "boom"))
    dedupe = \case
      x : y : rest | x == y -> dedupe (y : rest)
      x : rest -> x : dedupe rest
      [] -> []

refsPath :: [Text]
refsPath = ["api", "refs", "tree"]

refsOk :: TestApi -> ByteString -> IO Answer
refsOk api q = do
  r <- getApiQuery api refsPath q
  (q, r.status) @?= (q, 200)
  decodeOr r.body

decodeOr :: LBS.ByteString -> IO Answer
decodeOr body = maybe (assertFailure (show body)) pure (decodeAnswer body)

-- | The parts of an answer the tests look at.
data Answer = Answer
  { module_ :: Text,
    decl :: Text,
    partial :: Bool,
    total :: Int,
    results :: [Found]
  }

data Found = Found
  { ref :: Text,
    span :: [Int],
    kind :: Text
  }

decodeAnswer :: LBS.ByteString -> Maybe Answer
decodeAnswer = Aeson.decode >=> parseMaybe answer
  where
    answer = withObject "refs" $ \o ->
      Answer
        <$> o .: "module"
        <*> o .: "decl"
        <*> o .: "partial"
        <*> o .: "total"
        <*> (mapM found =<< (o .: "results" :: Parser [Aeson.Value]))
    found = withObject "result" $ \o ->
      Found <$> o .: "ref" <*> o .: "span" <*> o .: "kind"
