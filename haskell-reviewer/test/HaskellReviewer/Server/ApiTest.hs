-- | The snapshot routes, in process: status codes and containment.
module HaskellReviewer.Server.ApiTest (tests) where

import ApiSupport (ApiResult (..), fixtureApi, getApi, projectApi, requestApi)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import HaskellReviewer.Core.FileContent (FileContent (..), LinkInfo (..))
import HaskellReviewer.Core.FileList (CommitId (..))
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
  )
import HaskellReviewer.Core.Modules (Literate (..))
import HaskellReviewer.Core.Parser
  ( FileKind (..),
    ParseSettings (..),
    defaultParseSettings,
  )
import HaskellReviewer.Core.Project (Extension (..), Language (..))
import HaskellReviewer.Effects.FileSystem (ListError (..), ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Effects.Parser (oneAtATime)
import HaskellReviewer.Server.Api (FilesApi (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Api" $
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
        r3.status @?= 404
        r4 <- getApi projectApi ["api", "module", "head", "Foo"]
        r4.status @?= 404,
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
                ["api", "modules", "tree"],
                ["api", "module", "tree", "Foo"]
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
            ["api", "modules", "tree"],
            ["api", "module", "tree", "Foo"]
          ]
    ]
      <> moduleTests

-- | GET api/module/tree/<ref>. The fixtures cover the bodies.
moduleTests :: [TestTree]
moduleTests =
  [ testCase "module: unknown or ambiguous ref: 404, nothing read" $ do
      api <- countingReads projectApi
      mapM_
        ( \path -> do
            r <- getApi api.files (["api", "module", "tree"] <> path)
            (path, r.status) @?= (path, 404)
            assertNoStore r
        )
        [ [],
          [""],
          ["Nope"],
          -- Four components have a Main.
          ["Main"],
          ["pkg:lib:Nope"],
          ["pkg", "src", "Foo.hs"],
          ["scripts", "nope.hs"],
          ["..", "scripts", "Tool.hs"],
          -- An escaped / in a segment: not a path.
          ["scripts/Tool.hs"],
          ["scripts/build-docs.hs"]
        ]
      readIORef api.readCount >>= (@?= 0),
    testCase "module: generated, preprocessed, no source: nothing read" $ do
      api <- countingReads projectApi
      mapM_
        ( \ref -> do
            r <- getApi api.files ["api", "module", "tree", ref]
            (ref, r.status) @?= (ref, 200)
        )
        ["Lexer", "pkg:test:spec:Main", "Gone", "Gen", "Linked", "pkg:exe:outside:Main"]
      readIORef api.readCount >>= (@?= 0),
    testCase "module: component settings reach the parser" $ do
      seen <- newIORef []
      let api =
            projectApi
              { parse = \settings t -> do
                  modifyIORef' seen (<> [settings])
                  projectApi.parse settings t
              }
      mapM_
        (\ref -> getApi api ["api", "module", "tree", ref])
        ["Foo", "Lit", "Sig", "Tool"]
      got <- readIORef seen
      map (\x -> (x.language, x.literate, x.kind)) got
        @?= [ (Haskell2010, NotLiterate, ModuleFile),
              (Haskell2010, BirdOrLatex, ModuleFile),
              (Haskell2010, NotLiterate, SignatureFile),
              -- A loose file: GHC2021 and nothing else.
              (GHC2021, NotLiterate, ModuleFile)
            ]
      map (.extensions) (take 1 got)
        @?= [ [ Extension "ExplicitLevelImports" True,
                Extension "PackageImports" True,
                Extension "NoSuchThing" True
              ]
            ]
      map (.extensions) (drop 3 got) @?= [[]],
    testCase "module: a loose module by path, and preprocessed by a pragma" $ do
      r1 <- getApi projectApi ["api", "module", "tree", "scripts", "build-docs.hs"]
      r1.status @?= 200
      assertHas "\"kind\":\"parsed\"" r1
      r2 <- getApi projectApi ["api", "module", "tree", "Tool"]
      assertHas "\"reason\":{\"kind\":\"preprocessed\",\"tool\":\"hspec-discover\"}" r2,
    testCase "module: gone from disk: 404; read error: 500, reason logged" $ do
      logged <- newIORef []
      let with e =
            projectApi
              { readEntry = \_ _ _ -> pure (Left e),
                logger = newLogger LevelInfo (\l -> modifyIORef' logged (<> [l]))
              }
          path = ["api", "module", "tree", "Foo"]
      gone <- getApi (with ReadGone) path
      gone.status @?= 404
      failed <- getApi (with (ReadFailed "Permission denied")) path
      failed.status @?= 500
      failed.body @?= "cannot read file"
      readIORef logged
        >>= (@?= ["haskell-reviewer: cannot read pkg/src/Foo.hs: Permission denied"]),
    testCase "module: a listed file that became a link or a submodule: 404" $ do
      let with c = projectApi {readEntry = \_ _ _ -> pure (Right c)}
          link = Link LinkInfo {target = "elsewhere.hs", resolved = Nothing}
          commit = CommitId "0123456789abcdef0123456789abcdef01234567"
      mapM_
        ( \c -> do
            r <- getApi (with c) ["api", "module", "tree", "Foo"]
            (show c, r.status) @?= (show c, 404)
        )
        [link, Submodule commit, NestedRepo],
    testCase "module: one parse at a time" $ do
      gate <- newEmptyMVar
      active <- newIORef (0 :: Int)
      most <- newIORef 0
      entered <- newIORef (0 :: Int)
      let slow :: ParseSettings -> Text -> IO ModuleParse
          slow _ _ = do
            n <- atomicModifyIORef' active (\a -> (a + 1, a + 1))
            atomicModifyIORef' most (\m -> (max m n, ()))
            atomicModifyIORef' entered (\e -> (e + 1, ()))
            readMVar gate
            atomicModifyIORef' active (\a -> (a - 1, ()))
            pure (TextOnly TextOnlyModule {reason = CppNotSupported, warnings = []})
      limited <- oneAtATime slow
      withAsync (limited defaultParseSettings "") $ \a1 ->
        withAsync (limited defaultParseSettings "") $ \a2 -> do
          -- Never fails by being slow: at worst it checks too early.
          threadDelay 100_000
          readIORef entered >>= (@?= 1)
          putMVar gate ()
          mapM_ wait [a1, a2]
      readIORef entered >>= (@?= 2)
      readIORef most >>= (@?= 1),
    testCase "module: JSON with a length, no-store" $ do
      r <- getApi projectApi ["api", "module", "tree", "Foo"]
      r.status @?= 200
      lookup "Content-Type" r.headers @?= Just "application/json"
      lookup "Content-Length" r.headers
        @?= Just (Char8.pack (show (LBS.length r.body)))
      assertNoStore r
  ]

-- | An API that counts its reads.
data Counting = Counting
  { files :: FilesApi,
    readCount :: IORef Int
  }

countingReads :: FilesApi -> IO Counting
countingReads api = do
  count <- newIORef 0
  pure
    Counting
      { files =
          api
            { readEntry = \s p k -> do
                modifyIORef' count (+ 1)
                api.readEntry s p k
            },
        readCount = count
      }

assertHas :: BS.ByteString -> ApiResult -> Assertion
assertHas part r =
  assertBool (show r.body) (part `BS.isInfixOf` LBS.toStrict r.body)

assertNoStore :: ApiResult -> Assertion
assertNoStore r = [v | (n, v) <- r.headers, n == "Cache-Control"] @?= ["no-store"]
