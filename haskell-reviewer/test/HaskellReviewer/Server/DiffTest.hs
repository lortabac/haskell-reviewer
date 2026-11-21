-- | GET api\/diff\/\<path> with fake sides: methods, modes, paths, read
-- errors, the path rule (D-331). And the line diff's time limit.
module HaskellReviewer.Server.DiffTest (tests, timeLimitTests) where

import ApiSupport
  ( ApiResult (..),
    DiffFile (..),
    TestApi (..),
    cborValue,
    diffRouteApi,
    field,
    fixtureApi,
    getApi,
    getApiQuery,
    int,
    requestApi,
    str,
  )
import Control.Exception (ErrorCall (..), evaluate, throwIO)
import Control.Monad (forM_)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import GHC.Clock (getMonotonicTime)
import HaskellReviewer.Core.Eol (CleanEol (..), Filtering (..))
import HaskellReviewer.Core.FileContent
  ( FileContent (..),
    FileNotes (..),
    FileSize (..),
    LinkInfo (..),
    TooLargeInfo (..),
    unfiltered,
  )
import HaskellReviewer.Core.FileList (CommitId (..), EntryKind (..))
import HaskellReviewer.Core.Git (ObjectFormat (..), ObjectId (..))
import HaskellReviewer.Core.LineDiff (Whitespace (..))
import HaskellReviewer.Core.RelPath (RelPath, parseRelPath)
import HaskellReviewer.Core.Renames
  ( Change (..),
    ExecChange (..),
    ModifiedFile (..),
    RenameStep (..),
    RenamedFile (..),
  )
import HaskellReviewer.Core.Root (CanonicalRoot (..))
import HaskellReviewer.Core.Sides (Pair (..), Side (..), Sides (..))
import HaskellReviewer.Core.Similarity (maxScore)
import HaskellReviewer.Core.WorkTree (DiskFile (..), WorkFile (..))
import HaskellReviewer.Effects.Clock (Microseconds (..))
import HaskellReviewer.Effects.FileSystem (ReadError (..))
import HaskellReviewer.Effects.Git (Git (..))
import HaskellReviewer.Effects.GitObjects (defaultReaderSettings, withObjectReader)
import HaskellReviewer.Effects.Log (LogLevel (..), newLogger)
import HaskellReviewer.Effects.Source (EntrySource (..), WorkTreeSource (..))
import HaskellReviewer.Server.Diff (GuardedDiff (..), guardedLineDiff, warmUpDiff)
import HaskellReviewer.Server.Wire (encodeFileNotFound)
import HaskellReviewer.Session.Diff (DiffProblem (..))
import HaskellReviewer.Session.Snapshot (Generation (..), Snapshot (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "diff route"
    [ testCase "405 first, in both modes, HEAD too" $
        forM_ [fixtureApi, api] $ \a ->
          forM_ ["POST", "HEAD", "PUT"] $ \m -> do
            r <- requestApi m a (diffPath ["a.hs"])
            (m, r.status) @?= (m, 405),
      testCase "browse mode: a plain 404" $ do
        r <- getApi fixtureApi (diffPath ["a.hs"])
        (r.status, r.body) @?= (404, "not found"),
      testCase "bad path or on neither side: 404 with the JSON body" $
        forM_ [[], [""], ["nope.hs"], ["r/to.hs"], ["r", "..", "a.hs"], ["r", ""]] $
          \p -> do
            r <- getApi api (diffPath p)
            (p, r.status, r.body) @?= (p, 404, encodeFileNotFound (Generation 1))
            lookup "Content-Type" r.headers @?= Just "application/json",
      testCase "modified: CBOR, runs, sides" $ do
        r <- getApi api (diffPath ["a.hs"])
        (r.status, lookup "Content-Type" r.headers) @?= (200, Just "application/cbor")
        v <- cborValue r.body
        str (field "status" v) @?= "modified"
        str (field "path" v) @?= "a.hs"
        field "oldPath" v @?= Aeson.Null
        runsOf v @?= [("equal", 1), ("insert", 1)]
        str (field "kind" (field "base" v)) @?= "text"
        field "ignoreWhitespace" v @?= Aeson.Bool False,
      testCase "added and removed: all insert, all delete" $ do
        a <- answer ["new.txt"]
        (str (field "status" a), runsOf a) @?= ("added", [("insert", 2)])
        str (field "kind" (field "base" a)) @?= "absent"
        d <- answer ["old.txt"]
        (str (field "status" d), runsOf d) @?= ("removed", [("delete", 1)])
        str (field "kind" (field "head" d)) @?= "absent",
      testCase "renamed: the head path and the base path give the same body" $ do
        h <- getApi api (diffPath ["r", "to.hs"])
        b <- getApi api (diffPath ["r", "from.hs"])
        h.status @?= 200
        b.body @?= h.body
        v <- cborValue h.body
        (str (field "status" v), str (field "oldPath" v)) @?= ("renamed", "r/from.hs")
        int (field "similarity" v) @?= 100
        str (field "note" v) @?= "mode-changed",
      testCase "past the pairing: unchanged, modified, added, removed" $ do
        same <- answer ["same.txt"]
        (str (field "status" same), runsOf same) @?= ("unchanged", [("equal", 2)])
        past <- answer ["past.txt"]
        str (field "status" past) @?= "modified"
        field "note" past @?= Aeson.Null
        kind <- answer ["kind"]
        (str (field "status" kind), str (field "note" kind))
          @?= ("modified", "type-changed")
        hd <- answer ["pasthead.txt"]
        str (field "status" hd) @?= "added"
        bs <- answer ["pastbase.txt"]
        str (field "status" bs) @?= "removed"
        -- Without blob ids: same sizes still count as modified, a
        -- symlink by its target only.
        forM_ [["samebin"], ["samebig"]] $ \p -> do
          v <- answer p
          (p, str (field "status" v)) @?= (p, "modified")
        link <- answer ["samelink"]
        str (field "status" link) @?= "unchanged"
        empty <- answer ["empty.txt"]
        (str (field "status" empty), runsOf empty) @?= ("unchanged", []),
      testCase "ws: ignore, and a bad value or ws twice is a 400" $ do
        r <- getApiQuery api (diffPath ["ws.hs"]) "?ws=ignore"
        r.status @?= 200
        v <- cborValue r.body
        (str (field "status" v), runsOf v) @?= ("modified", [("equal", 1)])
        field "ignoreWhitespace" v @?= Aeson.Bool True
        keep <- getApiQuery api (diffPath ["ws.hs"]) ""
        k <- cborValue keep.body
        runsOf k @?= [("delete", 1), ("insert", 1)]
        forM_ ["?ws=bad", "?ws", "?ws=", "?ws=ignore&ws=ignore", "?ws=IGNORE"] $ \q -> do
          b <- getApiQuery api (diffPath ["ws.hs"]) q
          (q, b.status, b.body) @?= (q, 400, "bad ws"),
      testCase "no pairing: the 500s" $ do
        let with p = api {diff = Just (Left p)}
        n <- getApi (with DiffNotListed) (diffPath ["a.hs"])
        (n.status, n.body) @?= (500, "cannot list files")
        p <- getApi (with (DiffNotPaired "x")) (diffPath ["a.hs"])
        (p.status, p.body) @?= (500, "cannot pair files"),
      testCase "read gone: 404; read failed: 500, logged; not local: a note" $ do
        logged <- newIORef []
        let a = api {logger = newLogger LevelInfo (\l -> modifyIORef' logged (<> [l]))}
        g <- getApi a (diffPath ["gone.txt"])
        (g.status, g.body) @?= (404, encodeFileNotFound (Generation 1))
        f <- getApi a (diffPath ["fail.txt"])
        (f.status, f.body) @?= (500, "cannot read file")
        readIORef logged
          >>= (@?= ["haskell-reviewer: base: cannot read fail.txt: Permission denied"])
        m <- answer ["remote.bin"]
        str (field "kind" (field "head" m)) @?= "not-local"
        str (field "kind" (field "base" m)) @?= "text"
        str (field "kind" (field "diff" m)) @?= "not-text"
        -- api/file keeps its 500 for now (D-331).
        let notLocal = Left (ReadMissing (ObjectId "ab"))
            missing = fixtureApi {readEntry = \_ _ _ -> pure notLocal, workers = 0}
        viaFile <- getApi missing ["api", "file", "tree", "app", "Main.hs"]
        (viaFile.status, viaFile.body) @?= (500, "cannot read file"),
      testCase "the line diff throws: 500, logged; too slow: a note" $ do
        logged <- newIORef []
        let crashing =
              api
                { lineDiff = \_ _ -> throwIO (ErrorCall "boom"),
                  logger = newLogger LevelInfo (\l -> modifyIORef' logged (<> [l]))
                }
        r <- getApi crashing (diffPath ["a.hs"])
        (r.status, r.body) @?= (500, "cannot diff file")
        readIORef logged >>= (@?= ["haskell-reviewer: cannot diff a.hs: boom"])
        slow <- getApi api {lineDiff = \_ _ -> pure DiffTimedOut} (diffPath ["a.hs"])
        v <- cborValue slow.body
        str (field "kind" (field "diff" v)) @?= "too-slow",
      testCase "removed, with the path on head's list too: head is absent" $ do
        -- Base tracks it, head has an untracked nested repo there: the
        -- pairing skips nested repos, so it says removed.
        let nested = file "nested" EntryNestedRepo (Right NestedRepo)
            gone = diffRouteApi [] [text "nested" "n\n"] [Removed (rel "nested")]
            both = diffRouteApi [nested] [text "nested" "n\n"] [Removed (rel "nested")]
        a <- getApi gone (diffPath ["nested"])
        b <- getApi both (diffPath ["nested"])
        (b.status, b.body) @?= (a.status, a.body)
        v <- cborValue b.body
        str (field "status" v) @?= "removed"
        str (field "kind" (field "head" v)) @?= "absent",
      testCase "same path: blob ids first, then text, then submodule commits" $ do
        withIds $ \api' -> do
          let status p = do
                r <- getApi api' (diffPath [p])
                (p, r.status) @?= (p, 200)
                v <- cborValue r.body
                pure (str (field "status" v), field "note" v)
          -- Equal ids win over different texts, and the other way round.
          status "ids-same" >>= (@?= ("unchanged", Aeson.Null))
          status "ids-differ" >>= (@?= ("modified", Aeson.Null))
          -- Head has no id: the texts.
          status "mixed-same" >>= (@?= ("unchanged", Aeson.Null))
          status "mixed-filtered" >>= (@?= ("modified", Aeson.String "filtered"))
          status "sub-same" >>= (@?= ("unchanged", Aeson.Null))
          status "sub-differ" >>= (@?= ("modified", Aeson.Null)),
      testCase "the warm-up runs" warmUpDiff,
      testCase "not text: binary on one side" $ do
        v <- answer ["bin"]
        str (field "kind" (field "diff" v)) @?= "not-text"
        int (field "size" (field "head" v)) @?= 3
    ]
  where
    answer p = do
      r <- getApi api (diffPath p)
      (p, r.status) @?= (p, 200)
      cborValue r.body
    api = diffRouteApi headFiles baseFiles changes

-- | Same-path files on working-tree sources with blob ids. Reads come
-- from the fake; the reader is never started.
withIds :: (TestApi -> IO a) -> IO a
withIds k =
  withObjectReader quiet defaultReaderSettings noGit $ \reader -> do
    let source files =
          WorkTreeEntries
            WorkTreeSource
              { root = CanonicalRoot {path = "/fixture", bytes = "/fixture"},
                reader = reader,
                format = Sha1,
                entries = Map.empty,
                hashable = Set.empty,
                files = Map.fromList [(rel p, w) | (p, w) <- files],
                outside = Nothing
              }
        baseSource =
          source
            [ ("ids-same", FromIndex (oid 'a')),
              ("ids-differ", FromIndex (oid 'a')),
              ("mixed-same", FromIndex (oid 'a')),
              ("mixed-filtered", FromIndex (oid 'a'))
            ]
        headSource =
          source
            [ ("ids-same", FromIndex (oid 'a')),
              ("ids-differ", FromIndex (oid 'b')),
              ("mixed-same", disk),
              ("mixed-filtered", disk)
            ]
        disk = FromDisk DiskFile {blob = Nothing, eol = KeepRaw, filtered = Unfiltered}
        sub p c = file p (EntrySubmodule (CommitId c)) (Right (Submodule (CommitId c)))
        api0 =
          diffRouteApi
            [ text "ids-same" "one\n",
              text "ids-differ" "same\n",
              text "mixed-same" "same\n",
              text "mixed-filtered" "head\n",
              sub "sub-same" "c1",
              sub "sub-differ" "c2"
            ]
            [ text "ids-same" "two\n",
              text "ids-differ" "same\n",
              text "mixed-same" "same\n",
              text "mixed-filtered" "base\n",
              sub "sub-same" "c1",
              sub "sub-differ" "c1"
            ]
            []
        withSource s get = fmap (\snap -> snap {source = s}) <$> get
        api' =
          api0
            { sides = case api0.sides of
                DiffSides p ->
                  DiffSides
                    Pair {head = withSource headSource p.head, base = withSource baseSource p.base}
                other -> other,
              fileNotes = \snap p -> case snap.side of
                HeadSide | p == rel "mixed-filtered" -> FileNotes {filtered = Filtered}
                _ -> unfiltered
            }
    k api'
  where
    oid c = ObjectId (Text.replicate 40 (Text.singleton c))
    noGit =
      Git
        { exe = "/nonexistent/git",
          cwd = "/nonexistent",
          env = [],
          probeTimeLimit = Microseconds 1,
          listTimeLimit = Microseconds 1
        }
    quiet = newLogger LevelInfo (\_ -> pure ())

diffPath :: [Text] -> [Text]
diffPath p = ["api", "diff"] <> p

-- | The runs as (op, count).
runsOf :: Aeson.Value -> [(Text, Int)]
runsOf v = case field "runs" (field "diff" v) of
  Aeson.Array rs -> [(str o, int n) | Aeson.Array r <- toList rs, [o, n] <- [toList r]]
  _ -> []

headFiles :: [DiffFile]
headFiles =
  [ text "a.hs" "x\ny\n",
    text "new.txt" "1\n2\n",
    text "r/to.hs" "r\n",
    text "same.txt" "s\nt\n",
    text "past.txt" "p2\n",
    file "kind" EntrySymlink (Right (Link LinkInfo {target = "x", resolved = Nothing})),
    text "pasthead.txt" "h\n",
    text "empty.txt" "",
    text "ws.hs" "a  b\n",
    file "gone.txt" EntryFile (Left ReadGone),
    text "fail.txt" "f\n",
    file "remote.bin" EntryFile (Left (ReadMissing (ObjectId "ab"))),
    file "bin" EntryFile (Right (Binary (FileSize 3))),
    file "samebin" EntryFile (Right (Binary (FileSize 3))),
    file "samebig" EntryFile (Right big),
    file "samelink" EntrySymlink (Right (Link LinkInfo {target = "t", resolved = Just (rel "t")}))
  ]

baseFiles :: [DiffFile]
baseFiles =
  [ text "a.hs" "x\n",
    text "old.txt" "o\n",
    text "r/from.hs" "r\n",
    text "same.txt" "s\nt\n",
    text "past.txt" "p1\n",
    text "kind" "x",
    text "pastbase.txt" "b\n",
    text "empty.txt" "",
    text "ws.hs" "a b\n",
    text "gone.txt" "g\n",
    file "fail.txt" EntryFile (Left (ReadFailed "Permission denied")),
    text "remote.bin" "r\n",
    text "bin" "b\n",
    file "samebin" EntryFile (Right (Binary (FileSize 3))),
    file "samebig" EntryFile (Right big),
    file "samelink" EntrySymlink (Right (Link LinkInfo {target = "t", resolved = Nothing}))
  ]

big :: FileContent
big = TooLarge TooLargeInfo {size = FileSize 2_000_000, limit = FileSize 1_048_576}

changes :: [Change]
changes =
  [ Modified ModifiedFile {path = rel "a.hs", note = Nothing},
    Added (rel "new.txt"),
    Removed (rel "old.txt"),
    Renamed
      RenamedFile
        { from = rel "r/from.hs",
          to = rel "r/to.hs",
          score = maxScore,
          step = ExactStep,
          exec = ExecChanged
        },
    Modified ModifiedFile {path = rel "ws.hs", note = Nothing},
    Modified ModifiedFile {path = rel "gone.txt", note = Nothing},
    Modified ModifiedFile {path = rel "fail.txt", note = Nothing},
    Modified ModifiedFile {path = rel "remote.bin", note = Nothing},
    Modified ModifiedFile {path = rel "bin", note = Nothing}
  ]

text :: ByteString -> Text -> DiffFile
text p t = file p EntryFile (Right (TextContent t))

file :: ByteString -> EntryKind -> Either ReadError FileContent -> DiffFile
file p k r = DiffFile {path = p, kind = k, answer = r}

rel :: ByteString -> RelPath
rel b = either (error . (\e -> show e <> ": " <> show (decodeUtf8 b))) id (parseRelPath b)

-- * Time limit

-- | The line diff stops at its time limit on hostile input. Heavy: in
-- the line diff group (D-321 scheduling).
timeLimitTests :: TestTree
timeLimitTests =
  testGroup
    "LineDiff diff time limit"
    [ testCase "hostile input stops soon after the limit" $ do
        let p = Pair {head = hostile 1, base = hostile 2}
        _ <- evaluate (Text.length p.head + Text.length p.base)
        t0 <- getMonotonicTime
        r <- guardedLineDiff (Microseconds 200_000) KeepWhitespace p
        t1 <- getMonotonicTime
        case r of
          DiffTimedOut -> pure ()
          DiffDone _ -> assertFailure "finished within the limit"
        -- The whole diff takes about 14 s (GHC 9.14, 2026-11): 5 s
        -- shows the limit stopped it, with room for GC pauses.
        assertBool ("took " <> show (t1 - t0) <> " s") (t1 - t0 < 5),
      testCase "a normal pair is diffed" $ do
        let p = Pair {head = "a\nb\n", base = "a\n"}
        guardedLineDiff (Microseconds 2_000_000) KeepWhitespace p >>= \case
          DiffDone _ -> pure ()
          DiffTimedOut -> assertFailure "too slow"
    ]
  where
    -- 1 MiB: 500k lines of one of 8 letters (D-321), from a small LCG.
    hostile :: Int -> Text
    hostile seed =
      Text.pack . concatMap (\x -> [letter x, '\n']) . take 500_000 . drop 1 $
        iterate (\x -> (x * 1103515245 + 12345) `mod` 2147483648) seed
    letter x = toEnum (fromEnum 'a' + (x `div` 65536) `mod` 8)
