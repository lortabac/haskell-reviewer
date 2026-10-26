{-# LANGUAGE StrictData #-}

-- | The text GHC sees, from a file's text: no BOM, unlit, and how to
-- map GHC's columns back to ours (Note [Span convention] in
-- "HaskellReviewer.Core.Span").
module HaskellReviewer.Core.SourceText
  ( -- * Preparing
    stripBom,
    unlitBirdLatex,
    unlitMarkdown,

    -- * Columns
    ColumnMap,
    columnMap,
    GhcRange (..),
    ghcToSpan,

    -- * Slicing
    blankSpans,
    SourceLines,
    sourceLines,
    sliceSpan,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))

{- Note [Unlit]

Both unlits keep every line, so GHC's line numbers are the file's. A
line that isn't code becomes empty. A bird track @>@ becomes a space, so
columns don't move either.

- .lhs (GHC's unlit): lines starting with @>@, and lines between
  @\\begin{code}@ (blanks before it are fine) and @\\end{code}@ (in
  column 1 only, like GHC's unlit). Outside code, a line with @#@ in
  column 1 is kept as it is, unless it starts with @#!@: GHC's unlit
  does the same, so cpp sees those lines.
- markdown-unlit (D-84): lines inside a fenced block whose info string
  has "haskell" and not "ignore". Approximate: that's all we read.

GHC's unlit also rejects a bird line right next to a text line. We
don't: the code still parses.
-}

-- * Preparing

-- | Drop a leading BOM. GHC skips it too, and our columns on line 1
-- don't count it.
stripBom :: Text -> Text
stripBom t = case Text.uncons t of
  Just ('\xFEFF', rest) -> rest
  _ -> t

-- | GHC's unlit for .lhs files. See Note [Unlit].
unlitBirdLatex :: Text -> Text
unlitBirdLatex = overLines (go False)
  where
    go _ [] = []
    go inCode (l : ls)
      | "\\begin{code}" `Text.isPrefixOf` Text.stripStart l = "" : go True ls
      | "\\end{code}" `Text.isPrefixOf` l = "" : go False ls
      | inCode = l : go inCode ls
      | Just rest <- Text.stripPrefix ">" l = (" " <> rest) : go inCode ls
      | "#" `Text.isPrefixOf` l, not ("#!" `Text.isPrefixOf` l) = l : go inCode ls
      | otherwise = "" : go inCode ls

-- | markdown-unlit. 'Nothing' when there is no Haskell block. See
-- Note [Unlit].
unlitMarkdown :: Text -> Maybe Text
unlitMarkdown t
  | any isOpen ls = Just (overLines (go False) t)
  | otherwise = Nothing
  where
    ls = Text.splitOn "\n" t
    fence l = Text.stripPrefix "```" (Text.stripStart l)
    isOpen l = case fence l of
      Just info -> "haskell" `Text.isInfixOf` info && not ("ignore" `Text.isInfixOf` info)
      Nothing -> False
    go _ [] = []
    go inCode (l : rest)
      | Just _ <- fence l = "" : go (not inCode && isOpen l) rest
      | inCode = l : go inCode rest
      | otherwise = "" : go inCode rest

-- * Columns

-- | For each line with tabs: the tabs, by GHC column, with our column.
-- Other lines need no work.
newtype ColumnMap = ColumnMap (IntMap (IntMap Int))

-- | From the text GHC saw. Fully built: no thunk holds the text.
columnMap :: Text -> ColumnMap
columnMap t = cols `seq` ColumnMap cols
  where
    cols =
      IntMap.fromList
        [ (n, tabStops l)
        | (n, l) <- zip [1 ..] (Text.splitOn "\n" t),
          Text.elem '\t' l
        ]

-- | The tabs of a line: GHC column to our column.
tabStops :: Text -> IntMap Int
tabStops = IntMap.fromList . go 1 1 . Text.unpack
  where
    go _ _ [] = []
    go ghc ours (c : cs) = case c of
      '\t' -> (ghc, ours) : go (nextTab ghc) (ours + 1) cs
      _ -> go (ghc + 1) (ours + 1) cs

-- | GHC's column after a tab at this column (like advanceSrcLoc).
nextTab :: Int -> Int
nextTab ghc = ((ghc - 1) `div` 8 + 1) * 8 + 1

-- | A span as GHC gives it: GHC's columns.
data GhcRange = GhcRange
  { startLine :: Int,
    startCol :: Int,
    endLine :: Int,
    endCol :: Int
  }

-- | GHC's columns to ours. See Note [Span convention].
ghcToSpan :: ColumnMap -> GhcRange -> Span
ghcToSpan (ColumnMap tabs) r =
  Span
    { startLine = Line (fromIntegral r.startLine),
      startCol = Col (fromIntegral (col r.startLine r.startCol)),
      endLine = Line (fromIntegral r.endLine),
      endCol = Col (fromIntegral (col r.endLine r.endCol))
    }
  where
    col line c = maybe c (`fromGhcCol` c) (IntMap.lookup line tabs)

-- | Our column for GHC's column, from the last tab before it. A column
-- inside a tab's run is the one after the tab.
fromGhcCol :: IntMap Int -> Int -> Int
fromGhcCol stops target = case IntMap.lookupLT target stops of
  Nothing -> target
  Just (ghc, ours) -> ours + 1 + max 0 (target - nextTab ghc)

-- * Slicing

-- | A file's text by line, to cut out spans.
newtype SourceLines = SourceLines (Vector.Vector Text)

-- | From the file's text, as stored. A BOM is dropped (columns on line 1
-- don't count it), and so is the @\r@ of a CRLF.
sourceLines :: Text -> SourceLines
sourceLines = SourceLines . Vector.fromList . map noCr . Text.splitOn "\n" . stripBom
  where
    noCr l = fromMaybe l (Text.stripSuffix "\r" l)

-- | The text of a span (Note [Span convention] in
-- "HaskellReviewer.Core.Span"). Lines are joined with @\n@; a span past
-- the end gives what there is.
sliceSpan :: SourceLines -> Span -> Text
sliceSpan (SourceLines ls) s
  | l1 == l2 = cut (c1 - 1) (c2 - c1) (line l1)
  | otherwise =
      Text.intercalate "\n" $
        [Text.drop (c1 - 1) (line l1)]
          <> [line l | l <- [l1 + 1 .. l2 - 1]]
          <> [Text.take (c2 - 1) (line l2)]
  where
    Span (Line a) (Col b) (Line c) (Col d) = s
    (l1, c1, l2, c2) = (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)
    line n = fromMaybe "" (ls Vector.!? (n - 1))
    cut from n = Text.take n . Text.drop from

-- | Spaces over these spans, newlines and tabs kept, so nothing else
-- moves. Columns as in Note [Span convention] in
-- "HaskellReviewer.Core.Span". Linear: the cuts are grouped by line.
blankSpans :: [Span] -> Text -> Text
blankSpans [] t = t
blankSpans ss t = overLines (zipWith blank [1 ..]) t
  where
    -- For each line, the column ranges to blank (0-based, end exclusive;
    -- Nothing for the end of the line).
    cuts :: IntMap [(Int, Maybe Int)]
    cuts =
      IntMap.fromListWith
        (<>)
        [ (n, [(from, to)])
        | Span (Line a1) (Col b1) (Line a2) (Col b2) <- ss,
          let (l1, c1, l2, c2) = (int a1, int b1, int a2, int b2),
          n <- [l1 .. l2],
          let from = if n == l1 then c1 - 1 else 0
              to = if n == l2 then Just (c2 - 1) else Nothing
        ]
    int :: (Integral a) => a -> Int
    int = fromIntegral
    blank :: Int -> Text -> Text
    blank n l = maybe l (foldr cut l) (IntMap.lookup n cuts)
    cut (from, to) l =
      let (a, rest) = Text.splitAt from l
          (b, c) = Text.splitAt (maybe (Text.length rest) (subtract from) to) rest
       in a <> Text.map space b <> c
    space c = if c == '\t' then c else ' '

-- * Internal

overLines :: ([Text] -> [Text]) -> Text -> Text
overLines f = Text.intercalate "\n" . f . Text.splitOn "\n"
