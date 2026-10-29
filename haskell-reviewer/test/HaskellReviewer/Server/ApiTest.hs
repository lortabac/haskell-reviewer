-- | The snapshot routes, in process: status codes and containment.
module HaskellReviewer.Server.ApiTest (tests) where

import ApiSupport
  ( ApiResult (..),
    TestApi (..),
    fixtureApi,
    getApi,
    getApiWith,
    indexed,
    projectApi,
    requestApi,
  )
import Codec.Compression.GZip qualified as GZip
import Control.Exception (evaluate)
import Control.Monad (forM, forM_)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (nub, sort)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileContent (FileContent (..), LinkInfo (..))
import HaskellReviewer.Core.FileList (CommitId (..), EntryKind (..))
import HaskellReviewer.Core.Modules (Literate (..))
import HaskellReviewer.Core.Parser (FileKind (..), ParseSettings (..))
import HaskellReviewer.Core.Project (Extension (..), Language (..))
import HaskellReviewer.Core.RelPath (relPathFromBytes)
import HaskellReviewer.Effects.FileSystem (ListError (..), ReadError (..))
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Server.Api (cborWarmUp)
import HaskellReviewer.Server.CborJson (cborToJson)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup "Api" $
    [ testCase "bad paths: 404, readEntry not called" $ do
        calls <- newIORef (0 :: Int)
        let api =
              fixtureApi
                { readEntry = \s p k -> do
                    modifyIORef' calls (+ 1)
                    fixtureApi.readEntry s p k,
                  workers = 0
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
        r4.status @?= 404
        r5 <- getApi projectApi ["api", "indexing", "head"]
        r5.status @?= 404,
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
                ["api", "module", "tree", "Foo"],
                ["api", "indexing", "tree"]
              ]
          ],
      testCase "CBOR with a length, no-store" $ do
        r <- getApi fixtureApi ["api", "file", "tree", "app", "Main.hs"]
        r.status @?= 200
        lookup "Content-Type" r.headers @?= Just "application/cbor"
        lookup "Content-Length" r.headers
          @?= Just (Char8.pack (show (LBS.length r.body)))
        lookup "X-Content-Type-Options" r.headers @?= Just "nosniff"
        assertNoStore r,
      testCase "symlink to a file not in the list: resolved is null" $ do
        r <- getApi fixtureApi ["api", "file", "tree", "docs", "old.md"]
        cborToJson r.body
          @?= Right
            "{\"generation\":1,\"kind\":\"symlink\",\
            \\"target\":\"gone.md\",\"resolved\":null}",
      testCase "nested repo: a submodule with commit null" $ do
        r <- getApi fixtureApi ["api", "file", "tree", "vendor", "nested"]
        cborToJson r.body
          @?= Right "{\"generation\":1,\"kind\":\"submodule\",\"commit\":null}",
      testCase "gone from disk: 404; read error: 500, reason logged" $ do
        logged <- newIORef []
        let with e =
              fixtureApi
                { readEntry = \_ _ _ -> pure (Left e),
                  logger = newLogger LevelInfo (\l -> modifyIORef' logged (<> [l])),
                  -- No worker: it would read and log too.
                  workers = 0
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
            ["api", "module", "tree", "Foo"],
            ["api", "indexing", "tree"]
          ],
      testCase "indexing: JSON with a length, no-store; done once indexed" $ do
        r <- getApi (indexed projectApi) ["api", "indexing", "tree"]
        r.status @?= 200
        lookup "Content-Type" r.headers @?= Just "application/json"
        lookup "Content-Length" r.headers
          @?= Just (Char8.pack (show (LBS.length r.body)))
        assertNoStore r
        -- pkg/src/Lexer.hs is a loose module readEntry doesn't have.
        r.body @?= "{\"generation\":1,\"total\":9,\"ready\":8,\"failed\":1}"
    ]
      <> moduleTests
      <> [gzipTests, warmUpTest]

-- | GET api/module/tree/<ref>. The fixtures cover the bodies.
moduleTests :: [TestTree]
moduleTests =
  [ testCase "module: unknown or ambiguous ref: 404 with JSON, nothing read" $ do
      api <- countingReads projectApi {workers = 0}
      mapM_
        ( \path -> do
            r <- getApi api.files (["api", "module", "tree"] <> path)
            (path, r.status) @?= (path, 404)
            lookup "Content-Type" r.headers @?= Just "application/json"
            r.body @?= moduleNotFound
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
      api <- countingReads projectApi {workers = 0}
      mapM_
        ( \ref -> do
            r <- getApi api.files ["api", "module", "tree", ref]
            (ref, r.status) @?= (ref, 200)
        )
        ["Lexer", "pkg:test:spec:Main", "Gone", "Gen", "Linked", "pkg:exe:outside:Main"]
      readIORef api.readCount >>= (@?= 0),
    testCase "module: component settings reach the parser" $ do
      -- Workers parse other modules too: find each one by its text.
      seen <- newIORef []
      let api =
            projectApi
              { parse = \settings t -> do
                  atomicModifyIORef' seen (\xs -> ((t, settings) : xs, ()))
                  projectApi.parse settings t
              }
      snap <- either (const (assertFailure "not listed")) pure =<< api.snapshot
      got <-
        forM
          [ ("Foo", "pkg/src/Foo.hs"),
            ("Lit", "pkg/src/Lit.lhs"),
            ("Sig", "pkg/src/Sig.hsig"),
            ("Tool", "scripts/Tool.hs")
          ]
          $ \(ref, path) -> do
            r <- getApi api ["api", "module", "tree", ref]
            r.status @?= 200
            p <- maybe (assertFailure "bad path") pure (relPathFromBytes path)
            api.readEntry snap p EntryFile >>= \case
              Right (TextContent t) ->
                maybe (assertFailure ("not parsed: " <> show path)) pure
                  . lookup t
                  =<< readIORef seen
              other -> assertFailure (show other)
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
      gone.body @?= moduleNotFound
      failed <- getApi (with (ReadFailed "Permission denied")) path
      failed.status @?= 500
      failed.body @?= "cannot read file"
      -- The worker logs it, for each module it reads.
      lines' <- readIORef logged
      assertBool (show lines') $
        "haskell-reviewer: cannot read pkg/src/Foo.hs: Permission denied" `elem` lines',
    testCase "module: indexing crashed: 500" $ do
      let api = projectApi {parse = \_ _ -> ioError (userError "boom")}
      r <- getApi api ["api", "module", "tree", "Foo"]
      r.status @?= 500
      r.body @?= "cannot index module",
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
    testCase "module: CBOR with a length, no-store" $ do
      r <- getApi projectApi ["api", "module", "tree", "Foo"]
      r.status @?= 200
      lookup "Content-Type" r.headers @?= Just "application/cbor"
      lookup "Content-Length" r.headers
        @?= Just (Char8.pack (show (LBS.length r.body)))
      assertNoStore r
  ]

-- | gzip (Note [Compression] in "HaskellReviewer.Server.Router"). The
-- big text is built once, before the tests share it: Note [Shared values
-- in tests] in "HaskellReviewer.Core.PackageGlobTest".
gzipTests :: TestTree
gzipTests =
  withResource (evaluate (Text.replicate 2000 "main = pure ()\n")) (const (pure ())) $
    \getBig -> testGroup "gzip" (bigTests getBig)

bigTests :: IO Text -> [TestTree]
bigTests getBig =
  [ testCase "gzip: a big file view with Accept-Encoding: gzip" $ do
      bigFile <- bigFileApi
      plain <- getApi bigFile path
      lookup "Content-Encoding" plain.headers @?= Nothing
      lookup "Vary" plain.headers @?= Just "Accept-Encoding"
      r <- getApiWith [("Accept-Encoding", "gzip, deflate")] bigFile path
      r.status @?= 200
      lookup "Content-Type" r.headers @?= Just "application/cbor"
      lookup "Content-Encoding" r.headers @?= Just "gzip"
      lookup "Content-Length" r.headers @?= Nothing
      lookup "Vary" r.headers @?= Just "Accept-Encoding"
      assertNoStore r
      assertBool "gzip didn't make it smaller" (LBS.length r.body < LBS.length plain.body)
      GZip.decompress r.body @?= plain.body,
    testCase "gzip: JSON and the module page too" $
      forM_ [["api", "modules", "tree"], ["api", "module", "tree", "Foo"]] $ \p -> do
        plain <- getApi projectApi p
        r <- getApiWith [("Accept-Encoding", "gzip")] projectApi p
        (p, lookup "Content-Encoding" r.headers) @?= (p, Just "gzip")
        GZip.decompress r.body @?= plain.body,
    testCase "gzip: small bodies, plain-text errors and q=0 are left alone" $ do
      let check api p headers = do
            plain <- getApi api p
            r <- getApiWith headers api p
            (p, lookup "Content-Encoding" r.headers) @?= (p, Nothing)
            r.body @?= plain.body
            lookup "Content-Length" r.headers @?= lookup "Content-Length" plain.headers
          gzipOk = [("Accept-Encoding", "gzip")]
      bigFile <- bigFileApi
      check fixtureApi ["api", "file", "tree", "app", "Main.hs"] gzipOk
      check projectApi ["api", "module", "tree", "Nope"] gzipOk
      check bigFile ["api", "file", "tree", "nope"] gzipOk
      check bigFile path [("Accept-Encoding", "gzip;q=0")]
  ]
  where
    path = ["api", "file", "tree", "app", "Main.hs"]
    bigFileApi = do
      big <- getBig
      pure
        fixtureApi
          { readEntry = \_ _ _ -> pure (Right (TextContent big)),
            workers = 0
          }

-- | The CBOR warm-up must take every path of encodeFileView (Note
-- [Forced snapshot] in "HaskellReviewer.Server.Api"). A new kind of
-- FileContent makes the case below incomplete: add it to cborWarmUp.
warmUpTest :: TestTree
warmUpTest =
  testCase "the CBOR warm-up has every kind of file view" $
    sort (nub (map kind cborWarmUp))
      @?= ["binary", "link", "nested-repo", "submodule", "text", "too-large"]
  where
    kind :: FileContent -> String
    kind = \case
      TextContent _ -> "text"
      Binary _ -> "binary"
      TooLarge _ -> "too-large"
      Link _ -> "link"
      Submodule _ -> "submodule"
      NestedRepo -> "nested-repo"

-- | The 404 body of the module route.
moduleNotFound :: LBS.ByteString
moduleNotFound = "{\"generation\":1,\"kind\":\"not-found\",\"what\":\"module\"}"

-- | An API that counts its reads.
data Counting = Counting
  { files :: TestApi,
    readCount :: IORef Int
  }

countingReads :: TestApi -> IO Counting
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

-- | The body has this, as JSON (a CBOR body through 'cborToJson').
assertHas :: BS.ByteString -> ApiResult -> Assertion
assertHas part r = do
  body <- case lookup "Content-Type" r.headers of
    Just "application/cbor" -> either (assertFailure . show) pure (cborToJson r.body)
    _ -> pure r.body
  assertBool (show body) (part `BS.isInfixOf` LBS.toStrict body)

assertNoStore :: ApiResult -> Assertion
assertNoStore r = [v | (n, v) <- r.headers, n == "Cache-Control"] @?= ["no-store"]
