module Bench.DiffFilesTest (tests) where

import Bench.DiffFiles
  ( BlobSizes (..),
    Candidate (..),
    ChangedFile (..),
    DiffFilesAnswer (..),
    FileStatus (..),
    StatusCounts (..),
    askLimit,
    largestChanged,
    parseDiffFiles,
    parseLsTree,
    pickCount,
    pickFiles,
    statusCounts,
  )
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Functor.Identity (Identity (..))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- Tests run from the package dir.
fixture :: FilePath -> IO DiffFilesAnswer
fixture path = do
  body <- BS.readFile ("../fixtures/" <> path)
  either (assertFailure . show) pure (parseDiffFiles body)

tests :: TestTree
tests =
  testGroup
    "Bench.DiffFiles"
    [ testCase "diff-files: statuses, outside" $ do
        a <- fixture "api/diff-files.json"
        statusCounts a.files
          @?= StatusCounts {added = 1, removed = 1, modified = 3, renamed = 2}
        a.outsideTotal @?= Just 3
        (a.renameLimitHit, a.truncated) @?= (False, False)
        [f.oldPath | f <- a.files, f.status == Renamed]
          @?= [Just "exact/a.txt", Just "run/a.sh"],
      testCase "diff-files: limit hit, truncated" $ do
        a <- fixture "api/diff-files-limit.json"
        (a.renameLimitHit, a.truncated, a.outsideTotal) @?= (True, True, Just 100001),
      testCase "diff-files: worktree, outside null" $ do
        a <- fixture "api/diff-files-worktree.json"
        a.outsideTotal @?= Nothing
        length a.files @?= 2,
      testCase "diff-files: the renames fixture repo" $ do
        a <- fixture "repos/renames.diff-files.json"
        assertBool "has renames" ((statusCounts a.files).renamed > 0)
        a.renameLimitHit @?= False,
      testCase "diff-files: a bad status is an error" $
        assertBool "error" . isLeft . parseDiffFiles $
          "{\"files\":[{\"path\":\"a\",\"oldPath\":null,\"status\":\"copied\"}],\
          \\"renameLimitHit\":false,\"truncated\":false,\"outside\":null}",
      testCase "ls-tree: blobs, a symlink; a submodule left out" $
        parseLsTree lsTree
          @?= Right (Map.fromList [("a b/Ü.hs", 1234), ("link", 5), ("src/A.hs", 70)]),
      testCase "ls-tree: a bad line is an error" $
        assertBool "error" (isLeft (parseLsTree "100644 blob x\0")),
      testCase "largestChanged: modified and renamed, by both sizes, ties by path" $ do
        let both = [("t1", 5), ("t2", 5), ("a", 9)]
            sizes =
              BlobSizes
                { base = Map.fromList ([("m", 10), ("old", 50)] <> both),
                  head = Map.fromList ([("m", 20), ("new", 1)] <> both)
                }
            fs =
              [ file "a" Nothing Added,
                file "m" Nothing Modified,
                file "new" (Just "old") Renamed,
                file "t2" Nothing Modified,
                file "t1" Nothing Modified,
                file "r" Nothing Removed,
                -- Its old path is not in the base sizes: 0.
                file "gone" (Just "nowhere") Renamed
              ]
            cs = largestChanged sizes fs
        [(c.file.path, c.baseBytes, c.headBytes) | c <- cs]
          @?= [("new", 50, 1), ("m", 10, 20), ("t1", 5, 5), ("t2", 5, 5), ("gone", 0, 0)],
      testCase "pickFiles: the first 10 that pass, at most 40 asks" $ do
        let cs = [Candidate (file (num i) Nothing Modified) 0 0 | i <- [1 .. 100 :: Int]]
            num = tshow
            -- Every third is not a line diff.
            isLines c = Identity (c.file.path `notElem` map num [3, 6 .. 100])
            (picked, asked) = runIdentity (pickFiles isLines cs)
        (pickCount, askLimit) @?= (10, 40)
        length picked @?= pickCount
        asked @?= 14
        let (none, asked') = runIdentity (pickFiles (const (Identity False)) cs)
        (none, asked') @?= ([], askLimit)
        let (few, asked'') = runIdentity (pickFiles (const (Identity True)) (take 3 cs))
        (length few, asked'') @?= (3, 3)
    ]
  where
    file p old st = ChangedFile {path = p, oldPath = old, status = st}
    tshow :: Int -> Text
    tshow = Text.pack . show

lsTree :: BS.ByteString
lsTree =
  BS.intercalate
    "\0"
    [ "100644 blob 1111111111111111111111111111111111111111      70\tsrc/A.hs",
      "120000 blob 2222222222222222222222222222222222222222       5\tlink",
      "160000 commit 3333333333333333333333333333333333333333       -\tsub",
      "100644 blob 4444444444444444444444444444444444444444    1234\ta b/\195\156.hs"
    ]
    <> "\0"
