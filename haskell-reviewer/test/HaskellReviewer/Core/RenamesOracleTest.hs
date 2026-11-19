-- | File pairing against git (D-324): the same rows as
-- @git diff --raw -M@ (status, paths, score), on generated cases, on
-- a few made to hit git's corners, and on @renames.sh@.
module HaskellReviewer.Core.RenamesOracleTest (tests) where

import Data.ByteString.Char8 qualified as Char8
import Data.Text qualified as Text
import HaskellReviewer.Core.Renames
  ( RenameLimit (..),
    Settings (..),
    defaultSettings,
    pairFiles,
  )
import HaskellReviewer.Core.WorkTree (Exec (..))
import Hedgehog (Property, annotate, evalIO, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import LineDiffSupport (chunks)
import RenamesSupport
  ( Case (..),
    Item (..),
    RepoSides (..),
    Row (..),
    Tree,
    genCase,
    gitCaseRows,
    gitDiffRows,
    ourRows,
    pairingRows,
    repoSides,
  )
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport (fixtureRepo, withCanonicalTemp)

tests :: TestTree
tests =
  testGroup
    "Renames oracle"
    [ testGroup
        "generated"
        -- 4 parts of 5 tests of 25 cases: 500 cases per limit, one git
        -- run (fast-import, log) per 25.
        ( chunks 4 "default limit" (withTests 5 (batch [] defaultSettings))
            <> chunks 2 "limit 2" (withTests 5 (batch ["-l2"] (limit 2)))
        ),
      testCase "corner cases" corners,
      testGroup
        "renames.sh"
        [ testCase "ROOT is the top" (fixture "" [] defaultSettings),
          testCase "ROOT is pkg/" (fixture "pkg/" [] defaultSettings),
          testCase "limit 2" (fixture "" ["-l2"] (limit 2))
        ]
    ]

limit :: Int -> Settings
limit l = defaultSettings {renameLimit = RenameLimit l}

batch :: [String] -> Settings -> Property
batch extra settings = property $ do
  cases <- forAll (Gen.list (Range.singleton 25) genCase)
  gits <- evalIO (gitCaseRows extra cases)
  let ours = map (ourRows settings) cases
      renamed = length [() | rs <- ours, r <- rs, r.status == 'R', r.score /= Just 100]
  annotate (show renamed <> " inexact renames in the batch")
  ours === gits

-- | Cases made for git's corners.
corners :: IO ()
corners = do
  gits <- gitCaseRows [] (map snd cases)
  [(n, ourRows defaultSettings c) | (n, c) <- cases] @?= zip (map fst cases) gits
  where
    file = IFile NotExecutable
    dup = file "dup\n"
    many :: Int -> Tree
    many n = [(Text.pack ("s/f" <> pad i), dup) | i <- [0 .. n - 1]]
    pad i = let s = show i in replicate (3 - length s) '0' <> s
    cases :: [(String, Case)]
    cases =
      [ -- git's hash table grows on the 205th insert: candidates in
        -- reverse, so the last source wins.
        ("205 identical sources", Case {base = many 205, head = [("t/new", dup)]}),
        ("204 identical sources", Case {base = many 204, head = [("t/new", dup)]}),
        -- Only the first 100 candidates are looked at: no basename win.
        ("100 candidates", Case {base = many 150, head = [("t/f120", dup)]}),
        ( "a basename win in the first 100",
          Case {base = many 150, head = [("t/f099", dup)]}
        ),
        ( "exec bit differs: still exact",
          Case {base = [("a", file "x\n")], head = [("b", IFile Executable "x\n")]}
        ),
        ( "empty files",
          Case
            { base = [("e1", file ""), ("e2", file "")],
              head = [("x/e2", file ""), ("e3", file "")]
            }
        ),
        ( "a symlink and a file with the same blob",
          Case {base = [("l", ISymlink "dup\n")], head = [("f", dup)]}
        ),
        ( "matrix: 5 sources for one destination, 4 slots",
          Case
            { base = [(Text.pack ("m" <> show i), file (body i)) | i <- [1 .. 5 :: Int]],
              head = [("dst", file (body 9))]
            }
        )
      ]
    body i =
      Char8.unlines [Char8.pack (show k) | k <- [1 .. 10 :: Int], k <= 5 + i || k == 10]

-- | renames.sh, ROOT at a prefix, against @git diff@ run in ROOT.
fixture :: String -> [String] -> Settings -> IO ()
fixture prefix extra settings = withCanonicalTemp $ \tmp -> do
  let repo = tmp </> "r"
  fixtureRepo "renames" [repo]
  sides <- repoSides repo prefix "HEAD~1" "HEAD"
  git <- gitDiffRows (repo </> prefix) extra "HEAD~1" "HEAD"
  pairingRows sides.entries (pairFiles settings sides.entries sides.contents) @?= git
