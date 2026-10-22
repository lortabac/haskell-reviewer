-- | The snapshot routes, in process: status codes and containment.
module HaskellReviewer.Server.ApiTest (tests) where

import ApiSupport (ApiResult (..), fixtureApi, getApi, requestApi)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (modifyIORef', newIORef, readIORef)
import HaskellReviewer.Effects.FileSystem (ListError (..), ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Server.Api (FilesApi (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Api"
    [ testCase "bad paths: 404, readEntry not called" $ do
        calls <- newIORef (0 :: Int)
        let api =
              fixtureApi
                { readEntry = \s p k -> do
                    modifyIORef' calls (+ 1)
                    fixtureApi.readEntry s p k
                }
        mapM_
          ( \path -> do
              r <- getApi api (["api", "file", "tree"] <> path)
              (path, r.status) @?= (path, 404)
              r.body @?= "not found"
              assertNoStore r
          )
          [ [],
            [""],
            ["../../etc/passwd"],
            ["..", "..", "etc", "passwd"],
            ["app", "..", "app", "Main.hs"],
            [".", "app", "Main.hs"],
            ["app/Main.hs"],
            ["", "etc", "passwd"],
            ["app", "", "Main.hs"],
            ["app", "Main.hs", ""],
            ["app\0", "Main.hs"],
            ["app"],
            ["not", "listed.hs"],
            [".env"]
          ]
        readIORef calls >>= (@?= 0),
      testCase "unknown side: 404" $ do
        r1 <- getApi fixtureApi ["api", "files", "head"]
        r1.status @?= 404
        r2 <- getApi fixtureApi ["api", "file", "base", "app", "Main.hs"]
        r2.status @?= 404
        r3 <- getApi fixtureApi ["api", "modules", "head"]
        r3.status @?= 404,
      testCase "not GET: 405, Allow: GET" $
        mapM_
          ( \(method, path) -> do
              r <- requestApi method fixtureApi path
              (method, path, r.status) @?= (method, path, 405)
              lookup "Allow" r.headers @?= Just "GET"
          )
          [ (m, p)
          | m <- ["HEAD", "POST", "PUT"],
            p <-
              [ ["api", "files", "tree"],
                ["api", "file", "tree", "app", "Main.hs"],
                ["api", "modules", "tree"]
              ]
          ],
      testCase "JSON with a length, no-store" $ do
        r <- getApi fixtureApi ["api", "file", "tree", "app", "Main.hs"]
        r.status @?= 200
        lookup "Content-Type" r.headers @?= Just "application/json"
        lookup "Content-Length" r.headers
          @?= Just (Char8.pack (show (LBS.length r.body)))
        lookup "X-Content-Type-Options" r.headers @?= Just "nosniff"
        assertNoStore r,
      testCase "symlink to a file not in the list: resolved is null" $ do
        r <- getApi fixtureApi ["api", "file", "tree", "docs", "old.md"]
        r.body
          @?= "{\"generation\":1,\"kind\":\"symlink\",\
              \\"target\":\"gone.md\",\"resolved\":null}",
      testCase "nested repo: a submodule with commit null" $ do
        r <- getApi fixtureApi ["api", "file", "tree", "vendor", "nested"]
        r.body @?= "{\"generation\":1,\"kind\":\"submodule\",\"commit\":null}",
      testCase "gone from disk: 404; read error: 500, reason logged" $ do
        logged <- newIORef []
        let with e =
              fixtureApi
                { readEntry = \_ _ _ -> pure (Left e),
                  logger = newLogger LevelInfo (\l -> modifyIORef' logged (<> [l]))
                }
            path = ["api", "file", "tree", "app", "Main.hs"]
        gone <- getApi (with ReadGone) path
        gone.status @?= 404
        failed <- getApi (with (ReadFailed "Permission denied")) path
        failed.status @?= 500
        failed.body @?= "cannot read file"
        readIORef logged
          >>= (@?= ["haskell-reviewer: cannot read app/Main.hs: Permission denied"]),
      testCase "module tree: JSON with a length, no-store" $ do
        r <- getApi fixtureApi ["api", "modules", "tree"]
        r.status @?= 200
        lookup "Content-Type" r.headers @?= Just "application/json"
        lookup "Content-Length" r.headers
          @?= Just (Char8.pack (show (LBS.length r.body)))
        assertNoStore r,
      testCase "listing failed: 500 on every route" $ do
        let api = fixtureApi {snapshot = pure (Left (ListCrashed "boom"))}
        mapM_
          ( \path -> do
              r <- getApi api path
              r.status @?= 500
              r.body @?= "cannot list files"
              assertNoStore r
          )
          [ ["api", "files", "tree"],
            ["api", "file", "tree", "app", "Main.hs"],
            ["api", "modules", "tree"]
          ]
    ]

assertNoStore :: ApiResult -> Assertion
assertNoStore r = [v | (n, v) <- r.headers, n == "Cache-Control"] @?= ["no-store"]
