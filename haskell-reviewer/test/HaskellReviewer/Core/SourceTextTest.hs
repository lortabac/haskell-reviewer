module HaskellReviewer.Core.SourceTextTest (tests) where

import Control.Exception (evaluate)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.SourceText
  ( GhcRange (..),
    columnMap,
    ghcToSpan,
    sliceSpan,
    sourceLines,
    stripBom,
    unlitBirdLatex,
    unlitMarkdown,
  )
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import Hedgehog (forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "SourceText"
    [ testCase "stripBom" $ do
        stripBom "\xFEFFmodule M" @?= "module M"
        stripBom "module M" @?= "module M"
        stripBom "x\xFEFF" @?= "x\xFEFF",
      testCase "bird: every line kept, > becomes a space" $ do
        let t = "text\n> x = 1\n\n>y\r\nmore"
        unlitBirdLatex t @?= "\n  x = 1\n\n y\r\n"
        lineCount (unlitBirdLatex t) @?= lineCount t,
      testCase "bird: # lines in column 1 are kept, like GHC's unlit" $
        unlitBirdLatex "#!/x\ntext\n#if X\n  #if Y\n\n> x = 1\n#endif"
          @?= "\n\n#if X\n\n\n  x = 1\n#endif",
      testCase "LaTeX: code blocks kept, the rest blank" $ do
        let t = "a\n\\begin{code}\nx = 1\n\\end{code}\nb\n"
        unlitBirdLatex t @?= "\n\nx = 1\n\n\n"
        lineCount (unlitBirdLatex t) @?= lineCount t,
      testCase "LaTeX: blanks before \\begin{code}, not before \\end{code}" $
        -- Like GHC's unlit: the indented \end{code} is still code.
        unlitBirdLatex "a\n  \\begin{code}\nx = 1\n  \\end{code}\n\\end{code}\nb"
          @?= "\n\nx = 1\n  \\end{code}\n\n",
      testCase "markdown: haskell blocks only" $ do
        let t = "# t\n```haskell\nx = 1\n```\n```haskell ignore\nno\n```\n```sh\nno\n```"
        unlitMarkdown t @?= Just "\n\nx = 1\n\n\n\n\n\n\n"
        (lineCount <$> unlitMarkdown t) @?= Just (lineCount t)
        unlitMarkdown "# t\n```sh\nx\n```\n" @?= Nothing,
      testCase "columns: no tab, no change" $
        ghcToSpan (columnMap "abc\ndef") (range 2 2 2 4) @?= spanOf 2 2 2 4,
      testCase "columns: a tab is one column" $ do
        -- GHC: the tab is at 1 and "x" at 9.
        let cols = columnMap "a\n\tx = 1\n  \t y"
        ghcToSpan cols (range 2 9 2 10) @?= spanOf 2 2 2 3
        -- "y": spaces at 1 and 2, tab at 3, space at 9, y at 10.
        ghcToSpan cols (range 3 10 3 11) @?= spanOf 3 5 3 6
        -- Past the end of the line.
        ghcToSpan cols (range 2 15 2 17) @?= spanOf 2 8 2 10,
      testCase "columns: a long line of tabs is fast" $ do
        -- 50,000 tabs, then a lookup at each one's column. Walking the
        -- line for each lookup took seconds.
        let n = 50000
            cols = columnMap (Text.replicate n "\ta")
            -- GHC puts the k-th "a" (from 0) at 8 * k + 9; we at 2 * k + 2.
            ourCol k = case ghcToSpan cols (range 1 (8 * k + 9) 1 (8 * k + 10)) of
              Span _ (Col c) _ _ -> c
        r <- timeout 5_000_000 (evaluate (sum (map ourCol [0 .. n - 1])))
        r @?= Just (sum [fromIntegral (2 * k + 2) | k <- [0 .. n - 1]]),
      testCase "slicing: a 1 MiB line, every token, is fast (Note [Slicing])" $ do
        -- 210,000 tokens on one line. Slicing with take and drop from the
        -- line's start took minutes.
        let n = 210000
            src = sourceLines (Text.replicate n "é𝑥 ")
            col k = fromIntegral (3 * k + 1)
            token k = sliceSpan src (spanOf 1 (col k) 1 (col k + 2))
            tokens = map token [0 .. n - 1]
        r <- timeout 5_000_000 (evaluate (length (filter (== "é𝑥") tokens)))
        r @?= Just n,
      testProperty "slicing: like take and drop on the lines" . property $ do
        let piece = Gen.element ["a", "é", "𝑥", "\t", " ", "\r\n", "\n", "\xFEFF"]
        long <- forAll Gen.bool
        let pieces = Range.linear 0 (if long then 900 else 60)
        t <- forAll (Text.concat <$> Gen.list pieces piece)
        let width = if long then 700 else 40
            pos = Gen.int (Range.linear 0 width)
        l1 <- forAll (Gen.int (Range.linear 1 4))
        l2 <- forAll (Gen.int (Range.linear l1 5))
        c1 <- forAll pos
        c2 <- forAll pos
        let w = fromIntegral
            s = spanOf (w l1) (w c1) (w l2) (w c2)
        sliceSpan (sourceLines t) s === reference t (l1, c1, l2, c2),
      testCase "columns: astral code points count as one" $
        -- GHC counts code points too: nothing to do.
        ghcToSpan (columnMap "\x1D465 = \t1") (range 1 1 1 2) @?= spanOf 1 1 1 2
    ]
  where
    -- The plain way, linear in the column.
    reference t (l1, c1, l2, c2) =
      let ls = map noCr (Text.splitOn "\n" (stripBom t))
          noCr l = fromMaybe l (Text.stripSuffix "\r" l)
          line n = if n >= 1 && n <= length ls then ls !! (n - 1) else ""
       in if l1 == l2
            then Text.take (c2 - c1) (Text.drop (c1 - 1) (line l1))
            else
              Text.intercalate "\n" $
                [Text.drop (c1 - 1) (line l1)]
                  <> [line l | l <- [l1 + 1 .. l2 - 1]]
                  <> [Text.take (c2 - 1) (line l2)]
    lineCount :: Text -> Int
    lineCount = length . Text.splitOn "\n"
    range a b c d = GhcRange {startLine = a, startCol = b, endLine = c, endCol = d}
    spanOf a b c d = Span (Line a) (Col b) (Line c) (Col d)
