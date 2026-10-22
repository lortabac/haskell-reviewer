module HaskellReviewer.Core.PackageGlobTest (tests) where

import Control.Exception (evaluate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileList
  ( EntryKind (..),
    FileList,
    ListSource (..),
    mkFileList,
  )
import HaskellReviewer.Core.PackageGlob
  ( DirIndex,
    expandEntry,
    matchSegment,
    mkDirIndex,
    rootCabalFiles,
  )
import HaskellReviewer.Core.Project
  ( EntryIgnoredReason (..),
    Loc (..),
    PackageEntry (..),
    ProjectWarning (..),
    Required (..),
  )
import HaskellReviewer.Core.RelPath
  ( PathProblem (..),
    RelDir,
    RelPath,
    relDirFromSegments,
    relPathFromSegments,
    relPathText,
  )
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "PackageGlob"
    [ testCase "segment matching" $ do
        let yes p n = (p, n, matchSegment p n) @?= (p, n, True)
            no p n = (p, n, matchSegment p n) @?= (p, n, False)
        yes "*" "abc"
        yes "*.cabal" "x.cabal"
        no "*.cabal" "x.cabal.orig"
        yes "a*c" "ac"
        yes "{a,b}" "b"
        no "{a,b}" "c"
        yes "x{a,b{c,d}}" "xbd"
        yes "{a,}x" "x"
        yes "\\*" "*"
        no "\\*" "a"
        yes "{" "{"
        yes "a}" "a}"
        yes "a,b" "a,b"
        yes "{a,b" "{a,b"
        yes "{{a,b}" "{a"
        no "*" ".hidden"
        no "*.cabal" ".cabal"
        yes "{.a,b}" ".a"
        yes ".*" ".hidden"
        yes ".hidden" ".hidden",
      testCase "no exponential blow-up" $ do
        -- 20 stars against 60 chars: backtracking would never end.
        let stars = Text.concat (replicate 20 "*a") <> "b"
        bounded (matchSegment stars (Text.replicate 60 "a")) False
        -- 30 groups: expanding them would give 2^30 patterns.
        let groups = Text.concat (replicate 30 "{a,b}")
        bounded (matchSegment groups (Text.replicate 15 "ab")) True
        bounded (matchSegment groups (Text.replicate 15 "ab" <> "c")) False
        -- Unclosed braces.
        let opens = Text.replicate 40 "{"
        bounded (matchSegment opens opens) True,
      -- The index is built once, before the tests that share it: see
      -- Note [Shared values in tests].
      withResource (evaluate (mkDirIndex files)) (const (pure ())) $
        testGroup "entries" . entryTests
    ]

-- | Tests that share the index of 'files'.
entryTests :: IO DirIndex -> [TestTree]
entryTests getIndex =
  [ withIndex "dirs, files, ./ and trailing /" $ \idx -> do
      expand idx Required "libs/*/" @?= (["libs/a/a.cabal", "libs/b/b.cabal"], [])
      expand idx Required "./libs/a" @?= (["libs/a/a.cabal"], [])
      expand idx Required "apps/*/*.cabal" @?= (["apps/x/x.cabal"], [])
      expand idx Required "." @?= (["root.cabal"], [])
      expand idx Required "libs/a/../b" @?= (["libs/b/b.cabal"], []),
    withIndex "a trailing / matches no file" $ \idx ->
      expand idx Required "root.cabal/" @?= ([], [EntryNoMatch at "root.cabal/"]),
    withIndex "other files are skipped" $ \idx ->
      expand idx Required "apps/x/*" @?= (["apps/x/x.cabal"], []),
    withIndex "dirs without one .cabal file" $ \idx -> do
      expand idx Required "hpack" @?= ([], [HpackOnly (dir ["hpack"])])
      expand idx Required "empty" @?= ([], [NoCabalFile (dir ["empty"])])
      expand idx Required "two"
        @?= ( ["two/one.cabal", "two/two.cabal"],
              [ SeveralCabalFiles
                  (dir ["two"])
                  [path ["two", "one.cabal"], path ["two", "two.cabal"]]
              ]
            ),
    withIndex "no match: packages warns, optional-packages doesn't" $ \idx -> do
      expand idx Required "nope/*" @?= ([], [EntryNoMatch at "nope/*"])
      expand idx Optional "nope/*" @?= ([], []),
    withIndex "skipped entries" $ \idx -> do
      let skipped g r = expand idx Required g @?= ([], [EntryIgnored at g r])
      skipped "https://example.com/p.tar.gz" RemoteEntry
      skipped "file:///tmp/p" RemoteEntry
      skipped "vendor/p-1.0.tar.gz" TarballEntry
      skipped "~/p" HomeDirEntry
      skipped "/abs/p" (EntryBadPath AbsolutePath)
      skipped "../p" (EntryBadPath AboveRoot)
      skipped "a/.git" (EntryBadPath BadName),
    withIndex "**: files at any depth, hidden dirs too" $ \idx -> do
      expand idx Required "**/*.cabal"
        @?= ( [ "apps/x/x.cabal",
                "libs/.hidden/h.cabal",
                "libs/a/a.cabal",
                "libs/b/b.cabal",
                "root.cabal",
                "two/one.cabal",
                "two/two.cabal"
              ],
              []
            )
      expand idx Required "libs/**/h.cabal" @?= (["libs/.hidden/h.cabal"], [])
      -- Only right before the last segment; elsewhere it's a plain glob.
      expand idx Required "**/x/*.cabal" @?= (["apps/x/x.cabal"], [])
      expand idx Required "**/" @?= expand idx Required "*/",
    withIndex "*.cabal in ROOT" $ \idx ->
      map relPathText (rootCabalFiles idx) @?= ["root.cabal"]
  ]
  where
    withIndex name t = testCase name (getIndex >>= t)

expand :: DirIndex -> Required -> Text -> ([Text], [ProjectWarning])
expand idx req g =
  let entry = PackageEntry {at = at, glob = g, required = req}
      (ps, ws) = expandEntry idx entry
   in (map relPathText ps, ws)

at :: Loc
at = Loc {file = path ["cabal.project"], line = 1}

dir :: [Text] -> RelDir
dir segs = fromMaybe (error "bad dir") (relDirFromSegments segs)

{- Note [Shared values in tests]

On GHC 9.12 and 9.14 the test binary sometimes hung when many tests
forced the same top-level value at once (threads spinning on a
CAF_BLACKHOLE, so the GC never got all capabilities). We didn't find
the cause in our code. Building the shared value once with
'withResource' stopped it: 0 hangs in 300 runs on each of 9.12 and
9.14, against about 1 in 12 before.
-}

-- | The value, within a second.
bounded :: Bool -> Bool -> IO ()
bounded x expected = do
  r <- timeout 1_000_000 (evaluate x)
  r @?= Just expected

files :: FileList
files =
  mkFileList
    FromDisk
    0
    [ (path (Text.splitOn "/" p), EntryFile)
    | p <-
        [ "cabal.project",
          "root.cabal",
          ".hidden.cabal",
          "libs/a/a.cabal",
          "libs/b/b.cabal",
          "libs/.hidden/h.cabal",
          "apps/x/x.cabal",
          "apps/x/README",
          "hpack/package.yaml",
          "empty/README",
          "two/one.cabal",
          "two/two.cabal"
        ]
    ]

path :: [Text] -> RelPath
path segs = fromMaybe (error ("bad path: " <> show segs)) (relPathFromSegments segs)
