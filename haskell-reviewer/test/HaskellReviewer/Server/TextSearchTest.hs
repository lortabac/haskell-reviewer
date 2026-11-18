-- | The text search route, in process (Note [Text search route] in
-- "HaskellReviewer.Server.TextSearch"). The fixtures cover the bodies.
--
-- In the "API" group (Q-RTS-BLACKHOLE).
module HaskellReviewer.Server.TextSearchTest (tests) where

import ApiSupport
  ( ApiResult (..),
    TestApi (..),
    arr,
    bool,
    field,
    getApiQuery,
    int,
    requestApi,
    str,
    textSearchApi,
    treeSnapshot,
  )
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (throwIO)
import Control.Monad (forM_)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Vector qualified as Vector
import HaskellReviewer.Core.RelPath (relPathFromBytes)
import HaskellReviewer.Core.Sides (Sides (..))
import HaskellReviewer.Core.TextSearch (TextAnswer (..), needle)
import HaskellReviewer.Effects.Clock (MonotonicTime (..), Nanoseconds (..))
import HaskellReviewer.Effects.FileSystem (ListError (..))
import HaskellReviewer.Server.TextSearch (searchFiles, textSearchDeadline)
import HaskellReviewer.Session.TextStore (Skipped (..), TextFile (..), TextFiles (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "text search"
    [ testCase "JSON with a length; one result per line, path order" $ do
        r <- getApiQuery textSearchApi textPath "?q=main"
        r.status @?= 200
        lookup "Content-Type" r.headers @?= Just "application/json"
        lookup "Content-Length" r.headers
          @?= Just (Char8.pack (show (LBS.length r.body)))
        v <- decodeOr r.body
        [(str (field "path" x), int (field "line" x)) | x <- arr (field "results" v)]
          @?= [ ("app/Main.hs", 1),
                ("app/Main.hs", 4),
                ("app/Main.hs", 5),
                ("t/bom.md", 1),
                ("t/long.txt", 1),
                ("t/many.txt", 1),
                ("t/ünï.txt", 1)
              ]
        (int (field "total" v), int (field "files" v)) @?= (7, 5)
        bool (field "partial" v) @?= False
        bool (field "stopped" v) @?= False
        let sk = field "skipped" v
        [int (field k sk) | k <- ["binary", "tooLarge", "unreadable", "overBudget"]]
          @?= [1, 1, 1, 0],
      testCase "case-sensitive" $ do
        v <- textOk textSearchApi "?q=Main"
        [int (field "line" x) | x <- arr (field "results" v)] @?= [1]
        hitCount textSearchApi "?q=MAIN" >>= (@?= 0),
      testCase "limit: the first ones; the counts stay" $ do
        v <- textOk textSearchApi "?q=main&limit=2"
        length (arr (field "results" v)) @?= 2
        int (field "total" v) @?= 7
        big <- textOk textSearchApi "?q=main&limit=99999999999999999999999"
        length (arr (field "results" big)) @?= 7,
      testCase "a + stays a +; %20 is a space" $ do
        a <- textOk textSearchApi "?q=a+b"
        str (field "query" a) @?= "a+b"
        [str (field "text" x) | x <- arr (field "results" a)] @?= ["x = a+b"]
        b <- textOk textSearchApi "?q=a%20b"
        [str (field "text" x) | x <- arr (field "results" b)] @?= ["y = a b"],
      testCase "spaces are kept" $
        hitCount textSearchApi "?q=%20main%20" >>= (@?= 2),
      testCase "the first q and limit count" $ do
        v <- textOk textSearchApi "?x=1&q=main&q=zzz&limit=1&limit=x"
        str (field "query" v) @?= "main"
        length (arr (field "results" v)) @?= 1,
      testCase "no q, empty q, a line break: no results, nothing read" $ do
        readCount <- newIORef (0 :: Int)
        let api =
              textSearchApi
                { readEntry = \snap p k -> do
                    atomicModifyIORef' readCount (\n -> (n + 1, ()))
                    textSearchApi.readEntry snap p k,
                  workers = 0
                }
        forM_ ["", "?q=", "?q=a%0Ab", "?q=a%0D"] $ \q -> do
          v <- textOk api q
          (q, int (field "total" v), arr (field "results" v)) @?= (q, 0, [])
        readIORef readCount >>= (@?= 0),
      testCase "400s" $ do
        let longest = "?q=" <> Char8.replicate 128 'a'
        ok <- getApiQuery textSearchApi textPath longest
        ok.status @?= 200
        forM_
          [ ("?q=" <> Char8.replicate 129 'a', "query too long"),
            ("?q=%FF", "bad query"),
            ("?q=a&limit=0", "bad query"),
            ("?q=a&limit=-1", "bad query"),
            ("?q=a&limit=abc", "bad query"),
            ("?q=a&limit=", "bad query"),
            ("?q=a&limit", "bad query")
          ]
          $ \(q, why) -> do
            r <- getApiQuery textSearchApi textPath q
            (q, r.status, r.body) @?= (q, 400, why),
      testCase "GET only, HEAD too is 405; tree only" $ do
        forM_ ["POST", "HEAD"] $ \m -> do
          r <- requestApi m textSearchApi textPath
          (m, r.status, lookup "Allow" r.headers) @?= (m, 405, Just "GET")
        r <- getApiQuery textSearchApi ["api", "text-search", "base"] "?q=main"
        r.status @?= 404,
      testCase "a failed listing is a 500" $ do
        r <-
          getApiQuery
            textSearchApi {sides = BrowseSides (pure (Left (ListCrashed "boom")))}
            textPath
            "?q=main"
        (r.status, r.body) @?= (500, "cannot list files"),
      testCase "a crashed load is a 500" $ do
        r <-
          getApiQuery
            textSearchApi {readEntry = \_ _ _ -> throwIO (userError "boom")}
            textPath
            "?q=main"
        (r.status, r.body) @?= (500, "cannot read files"),
      testCase "one load per snapshot, for requests at once and later" $ do
        snap <- either (assertFailure . show) pure =<< treeSnapshot textSearchApi
        readCount <- newIORef (0 :: Int)
        let api =
              textSearchApi
                { sides = BrowseSides (pure (Right snap)),
                  readEntry = \s p k -> do
                    atomicModifyIORef' readCount (\n -> (n + 1, ()))
                    textSearchApi.readEntry s p k,
                  workers = 0
                }
        totals <- mapConcurrently (\q -> int . field "total" <$> textOk api q) qs
        totals @?= [7, 1, 0, 7, 1, 0, 7, 1, 0, 7, 1, 0]
        _ <- textOk api "?q=main"
        -- The 9 files of the list (not the link, not the submodule).
        readIORef readCount >>= (@?= 9),
      testCase "the deadline: stopped between files" $ do
        ticks <- newIORef (0 :: Int)
        let clock = do
              tick <- atomicModifyIORef' ticks (\t -> (t + 1, t))
              -- Each look at the clock is 0.6 s later.
              pure (MonotonicTime (fromIntegral tick * 600_000_000))
            files =
              TextFiles
                { files =
                    Vector.fromList
                      [ TextFile {path = p, bytes = "x\n"}
                      | p <- mapMaybe relPathFromBytes ["a", "b", "c", "d"]
                      ],
                  skipped = Skipped 0 0 0 0
                }
            n = fromMaybe (error "needle") (needle "x")
        a <- searchFiles clock textSearchDeadline n 1000 files
        (a.stopped, a.total, a.files) @?= (True, 1, 1)
        b <- searchFiles clock (Nanoseconds maxBound) n 1000 files
        (b.stopped, b.total) @?= (False, 4)
    ]
  where
    qs = concat (replicate 4 ["?q=main", "?q=Main", "?q=zzz"])

textPath :: [Text]
textPath = ["api", "text-search", "tree"]

textOk :: TestApi -> ByteString -> IO Aeson.Value
textOk api q = do
  r <- getApiQuery api textPath q
  if r.status /= 200
    then assertFailure (show q <> ": " <> show r.status <> " " <> show r.body)
    else decodeOr r.body

hitCount :: TestApi -> ByteString -> IO Int
hitCount api q = int . field "total" <$> textOk api q

decodeOr :: LBS.ByteString -> IO Aeson.Value
decodeOr body = maybe (assertFailure ("not JSON: " <> show body)) pure (Aeson.decode body)
