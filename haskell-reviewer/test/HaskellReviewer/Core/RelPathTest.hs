module HaskellReviewer.Core.RelPathTest (tests) where

import HaskellReviewer.Core.RelPath
  ( BadPath (..),
    PathProblem (..),
    parentDir,
    parseRelPath,
    relDirSegments,
    relDirText,
    relPathBytes,
    relPathFromBytes,
    relPathFromSegments,
    relPathParents,
    relPathSegments,
    relPathText,
    resolveLinkTarget,
    resolveUnder,
    rootDir,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "RelPath"
    [ testCase "bad segments are rejected" $
        mapM_
          (\segs -> (segs, relPathFromSegments segs) @?= (segs, Nothing))
          [ [],
            [""],
            ["a", ""],
            ["", "a"],
            ["."],
            [".."],
            ["a", "..", "b"],
            ["a/b"],
            ["../../etc/passwd"],
            ["a\0b"],
            [".git"],
            [".GIT", "config"],
            ["a", ".Git", "HEAD"],
            [".g\x200Cit", "config"],
            ["\xFEFF.GIT"]
          ],
      testCase "parseRelPath: the reason" $ do
        parseRelPath "caf\xe9" @?= Left NotUtf8
        parseRelPath "a/../b" @?= Left BadSegment
        parseRelPath ".git/config" @?= Left BadSegment
        fmap relPathText (parseRelPath "a/.gitignore") @?= Right "a/.gitignore",
      testCase "bad bytes are rejected" $
        mapM_
          (\b -> (b, relPathFromBytes b) @?= (b, Nothing))
          ["", "/etc/passwd", "a//b", "a/", "./a", "a/../b", "a\0b", "caf\xe9"],
      testCase "segments round trip" $ do
        let segs = ["src", "Data", "Café.hs"]
        case relPathFromSegments segs of
          Nothing -> assertFailure "rejected"
          Just p -> do
            relPathSegments p @?= segs
            relPathText p @?= "src/Data/Café.hs"
            relPathFromBytes (relPathBytes p) @?= Just p
            map relPathText (relPathParents p) @?= ["src", "src/Data"],
      testCase "order is code point order" $ do
        let ps = map (relPathFromBytes . (<> ".hs")) ["B", "a", "Z"]
        compare (ps !! 0) (ps !! 1) @?= LT
        compare (ps !! 2) (ps !! 1) @?= LT,
      testCase "resolveLinkTarget" $ do
        let link = relPathFromBytes "docs/x"
            resolve t = fmap relPathText (link >>= (`resolveLinkTarget` t))
        resolve "y" @?= Just "docs/y"
        resolve "./y" @?= Just "docs/y"
        resolve "../shared/x" @?= Just "shared/x"
        resolve "sub/../../a" @?= Just "a"
        resolve "../../etc/passwd" @?= Nothing
        resolve ".." @?= Nothing
        resolve "/etc/passwd" @?= Nothing
        resolve "" @?= Nothing
        resolve "\xff" @?= Nothing
        -- A link at the top: its dir is ROOT.
        fmap relPathText (relPathFromBytes "lib" >>= (`resolveLinkTarget` "src/lib"))
          @?= Just "src/lib",
      testCase "resolveUnder" $ do
        let pkg = maybe rootDir parentDir (relPathFromBytes "pkg/p.cabal")
            resolve t = fmap relDirText (resolveUnder pkg t)
        relDirSegments pkg @?= ["pkg"]
        resolve "src" @?= Right "pkg/src"
        resolve "." @?= Right "pkg"
        resolve "" @?= Right "pkg"
        resolve "./src/" @?= Right "pkg/src"
        resolve ".." @?= Right "."
        resolve "../shared" @?= Right "shared"
        resolve "a/../../b" @?= Right "b"
        resolve "../.." @?= Left AboveRoot
        resolve "/usr/include" @?= Left AbsolutePath
        resolve "a/.git/b" @?= Left BadName
        resolve "a/.GIT" @?= Left BadName
        fmap relDirText (resolveUnder rootDir "..") @?= Left AboveRoot
    ]
