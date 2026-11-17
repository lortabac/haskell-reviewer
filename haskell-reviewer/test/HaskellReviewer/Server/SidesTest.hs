-- | Every snapshot route on each side of a session: browse mode answers
-- tree only, diff mode base and head only, each from its own snapshot
-- (D-313).
module HaskellReviewer.Server.SidesTest (tests) where

import ApiSupport
  ( ApiResult (..),
    TestApi (..),
    diffApi,
    getApiQuery,
    indexed,
    refsApi,
    requestApi,
    searchApi,
    treeSnapshot,
  )
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import HaskellReviewer.Core.Sides (Pair (..), Sides (..))
import HaskellReviewer.Effects.FileSystem (ListError (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Sides"
    [ testCase "browse: base and head are a plain 404 on every route, tree works" $
        forM_ headOnly $ \route -> do
          tree <- get browse (route.path "tree") route.query
          (route.name, tree.status) @?= (route.name, 200)
          forM_ ["base", "head", "nope"] $ \side -> do
            r <- get browse (route.path side) route.query
            (route.name, side, r.status, r.body)
              @?= (route.name, side, 404, notFoundBody),
      testCase "405 comes before a bad side" $
        forM_ headOnly $ \route -> do
          r <- requestApi "POST" browse (route.path "base")
          (route.name, r.status) @?= (route.name, 405),
      testCase "diff: 405 comes before tree's 404" $
        forM_ headOnly $ \route -> do
          r <- requestApi "POST" diff (route.path "tree")
          (route.name, r.status) @?= (route.name, 405),
      testCase "diff: tree is a plain 404 on every route" $
        forM_ headOnly $ \route -> do
          r <- get diff (route.path "tree") route.query
          (route.name, r.status, r.body) @?= (route.name, 404, notFoundBody),
      testCase "diff: each side answers from its own snapshot" $ do
        -- Head is refsApi's snapshot, base searchApi's.
        forM_ ["files", "modules"] $ \what -> do
          h <- get diff ["api", what, "head"] ""
          b <- get diff ["api", what, "base"] ""
          fromHead <- get headApi ["api", what, "tree"] ""
          fromBase <- get baseApi ["api", what, "tree"] ""
          (what, h.status, b.status) @?= (what, 200, 200)
          (what, h.body, b.body) @?= (what, fromHead.body, fromBase.body)
        forM_ headOnly $ \route -> do
          h <- get diff (route.path "head") route.query
          b <- get diff (route.path "base") route.query
          (route.name, h.status) @?= (route.name, 200)
          route.onBase b
        forM_ baseOnly $ \route -> do
          b <- get diff (route.path "base") route.query
          (route.name, b.status) @?= (route.name, 200)
          h <- get diff (route.path "head") route.query
          route.onBase h,
      testCase "diff: indexing has each side" $ do
        h <- get diff ["api", "indexing", "head"] ""
        has "\"side\":\"head\"" h
        b <- get diff ["api", "indexing", "base"] ""
        has "\"side\":\"base\"" b,
      testCase "diff: base failed to list: 500 there, head is fine" $ do
        let failing =
              diff
                { sides =
                    DiffSides
                      Pair
                        { head = treeSnapshot (indexed refsApi),
                          base = pure (Left (ListCrashed "boom"))
                        }
                }
        forM_ headOnly $ \route -> do
          b <- get failing (route.path "base") route.query
          (route.name, b.status, b.body) @?= (route.name, 500, "cannot list files")
          h <- get failing (route.path "head") route.query
          (route.name, h.status) @?= (route.name, 200)
    ]
  where
    headApi = indexed refsApi
    baseApi = indexed searchApi
    browse = headApi
    diff = diffApi headApi baseApi

-- | One route, with a request that works on one side.
data Route = Route
  { name :: Text,
    path :: Text -> [Text],
    query :: ByteString,
    -- | What the other side answers.
    onBase :: ApiResult -> Assertion
  }

-- | A request on every route that works on refsApi (head in the diff),
-- and what searchApi (base) answers.
headOnly :: [Route]
headOnly =
  [ Route "files" (\s -> ["api", "files", s]) "" ok,
    Route "file" (\s -> ["api", "file", s, "r", "src", "R", "Shape.hs"]) "" missing,
    Route "modules" (\s -> ["api", "modules", s]) "" ok,
    Route "module" (\s -> ["api", "module", s, "R.Shape"]) "" missing,
    Route "source" (\s -> ["api", "source", s, "r", "src", "R", "Shape.hs"]) "" missing,
    Route "indexing" (\s -> ["api", "indexing", s]) "" ok,
    Route "search" (\s -> ["api", "search", s]) "?q=area" (lacks "HasArea"),
    Route "refs" (\s -> ["api", "refs", s]) "?module=R.Shape&decl=val:area" missing,
    Route
      "text-search"
      (\s -> ["api", "text-search", s])
      "?q=HasArea"
      (lacks "Shape.hs")
  ]

-- | Requests that work on searchApi (base) only, and what refsApi
-- (head) answers.
baseOnly :: [Route]
baseOnly =
  [ Route "file" (\s -> ["api", "file", s, "Data", "Map.hs"]) "" missing,
    Route "module" (\s -> ["api", "module", s, "Data.Map"]) "" missing,
    Route "source" (\s -> ["api", "source", s, "Data", "Map.hs"]) "" missing,
    Route "search" (\s -> ["api", "search", s]) "?q=insert" (lacks "Data.Map")
  ]

ok :: ApiResult -> Assertion
ok r = r.status @?= 200

missing :: ApiResult -> Assertion
missing r = r.status @?= 404

-- | A 200 whose body doesn't have this text.
lacks :: ByteString -> ApiResult -> Assertion
lacks needle r = do
  r.status @?= 200
  assertBool (show r.body) (not (needle `BS.isInfixOf` LBS.toStrict r.body))

has :: ByteString -> ApiResult -> Assertion
has needle r = do
  r.status @?= 200
  assertBool (show r.body) (needle `BS.isInfixOf` LBS.toStrict r.body)

get :: TestApi -> [Text] -> ByteString -> IO ApiResult
get = getApiQuery

-- | The body of the router's plain 404.
notFoundBody :: LBS.ByteString
notFoundBody = "not found"
