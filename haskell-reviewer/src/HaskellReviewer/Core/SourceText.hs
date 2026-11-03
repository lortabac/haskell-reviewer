{-# LANGUAGE StrictData #-}

-- | The text GHC sees, from a file's text: no BOM, unlit, and how to
-- map GHC's columns back to ours (Note [Span convention] in
-- "HaskellReviewer.Core.Span").
module HaskellReviewer.Core.SourceText
  ( -- * Preparing
    stripBom,
    unlitBirdLatex,
    unlitMarkdown,
    MarkdownCode (..),
    MarkdownLayout (..),
    hasMarkdownBlock,
    unlitLoose,
    LooseUnlit (..),
    looseReadsMarkdown,

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

    -- * For tests
    markdownPieces,
    MarkdownPiece (..),
    FencedBlock (..),
    fenceClasses,
    reorderKey,
  )
where

import Data.Char (isSpace)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Unsafe qualified as Text.Unsafe
import Data.Vector qualified as Vector
import Data.Vector.Unboxed qualified as Unboxed
import Data.Word (Word32)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import Text.Read (readMaybe)

{- Note [Unlit]

Both unlits keep every line, so GHC's line numbers are the file's. A
line that isn't code becomes empty. A bird track @>@ becomes a space, so
columns don't move either.

- .lhs (GHC's unlit): lines starting with @>@, and lines between
  @\\begin{code}@ (blanks before it are fine) and @\\end{code}@ (in
  column 1 only, like GHC's unlit). Outside code, a line with @#@ in
  column 1 is kept as it is, unless it starts with @#!@: GHC's unlit
  does the same, so cpp sees those lines.
- markdown-unlit (D-84, D-224): markdown-unlit 0.6.0's rules, our own
  code. A fence is a line that starts, after spaces, with @```@ or
  @~~~@. A fence opens a block and the next fence closes it, whatever
  its kind or info string. No close: the block runs to the end. The
  classes are the info string's words, with @.@ as a space and a
  @{...}@ unwrapped up to the first @}@. A block is code when its
  classes have @haskell@ and not @ignore@. That's markdown-unlit's
  default selector; we ignore selectors given with @-optL@ (D-226).
- a loose @.lhs@\/@.lhsig@, outside any component (D-218): no @-pgmL@
  is known, so we guess. markdown-unlit if the file has no Bird or
  LaTeX code outside fenced blocks and at least one Haskell block;
  GHC's unlit otherwise. The fenced blocks are the ones above, fences
  included, so a @>@ line inside an unrelated block (a shell session,
  say) doesn't count.

markdown-unlit can also move code: blocks with a @top@ or @top:N@
class go first (sorted by N), and an indented fence's lines lose that
many characters. We don't map positions for that. Such a file is
shown as text, "preprocessed by markdown-unlit", with a warning saying
why (D-225). Blocks already in order don't count, and neither do
indented fences of blocks that aren't code.

We drop a BOM before reading fences; markdown-unlit doesn't, so a
fence on line 1 after a BOM is a fence for us and not for it.

GHC's unlit also rejects a bird line right next to a text line. We
don't: the code still parses.

The markdown-unlit rules are ported from markdown-unlit 0.6.0's
Text.Markdown.Unlit (MIT, © 2012-2021 Simon Hengel). Its license:

  Copyright (c) 2012-2021 Simon Hengel <sol@typeful.net>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
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

-- | What markdown-unlit gives for a file with a Haskell block.
data MarkdownCode
  = -- | The code at the same lines: every other line is empty.
    InPlace Text
  | -- | markdown-unlit would move code: we show the file as text.
    NeedsLayout MarkdownLayout
  deriving (Eq, Show)

-- | How markdown-unlit would move code. See Note [Unlit].
data MarkdownLayout
  = -- | @top@ or @top:N@ blocks out of file order.
    Reorders
  | -- | A Haskell block with an indented fence.
    Dedents
  deriving (Eq, Show)

-- | markdown-unlit. 'Nothing' when there is no Haskell block. If both
-- a reorder and a dedent happen, 'Reorders'. See Note [Unlit].
unlitMarkdown :: Text -> Maybe MarkdownCode
unlitMarkdown t
  | null code = Nothing
  | any (isTop . fenceClasses . (.fence)) code,
    not (ascending (map (reorderKey . fenceClasses . (.fence)) code)) =
      Just (NeedsLayout Reorders)
  | any ((> 0) . fenceIndent) code = Just (NeedsLayout Dedents)
  | otherwise = Just (InPlace (Text.intercalate "\n" (concatMap keep pieces)))
  where
    pieces = markdownPieces t
    code = [b | Fenced b <- pieces, isCode b]
    -- Only a block with such a class can have a key other than 0.
    isTop = any (\c -> c == "top" || "top:" `Text.isPrefixOf` c)
    ascending ks = and (zipWith (<=) ks (drop 1 ks))
    keep = \case
      Prose _ -> [""]
      Fenced b ->
        ""
          : (if isCode b then b.body else map (const "") b.body)
            <> ["" | b.closed]

-- | Is there a Haskell block? Stops at the first one: no sort, no
-- @top:@ key read, so it's cheap even on hostile input.
hasMarkdownBlock :: Text -> Bool
hasMarkdownBlock = any isCodePiece . markdownPieces

-- | Which unlit 'unlitLoose' used.
data LooseUnlit
  = -- | Bird or LaTeX code outside fences won, or there's no block.
    UsedGhcUnlit Text
  | -- | No Bird or LaTeX code outside fences, and a Haskell block.
    UsedMarkdownUnlit MarkdownCode
  deriving (Eq, Show)

-- | A loose @.lhs@\/@.lhsig@ file, outside any component: no
-- @-pgmL@ is known, so we guess (D-218). See Note [Unlit].
unlitLoose :: Text -> LooseUnlit
unlitLoose t
  | looseReadsMarkdown t, Just c <- unlitMarkdown t = UsedMarkdownUnlit c
  | otherwise = UsedGhcUnlit (unlitBirdLatex t)

-- | Does 'unlitLoose' pick markdown-unlit? Cheap, like
-- 'hasMarkdownBlock'.
looseReadsMarkdown :: Text -> Bool
looseReadsMarkdown t = not (any birdOrLatex prose) && any isCodePiece pieces
  where
    pieces = markdownPieces t
    prose = [l | Prose l <- pieces]
    birdOrLatex l =
      "\\begin{code}" `Text.isPrefixOf` Text.stripStart l || ">" `Text.isPrefixOf` l

-- | A file's lines, as markdown-unlit splits them.
data MarkdownPiece
  = -- | A line outside any fenced block.
    Prose Text
  | -- | A fenced block, open fence to close fence (or the end).
    Fenced FencedBlock

-- | A fenced block, fences included.
data FencedBlock = FencedBlock
  { -- | The open fence's line.
    line :: Line,
    -- | The open fence.
    fence :: Text,
    -- | The lines between the fences, as they are (not dedented).
    body :: [Text],
    -- | A close fence follows. If not, the block runs to the end.
    closed :: Bool
  }

-- | The file's lines, in order. Lazy and linear: the block search
-- stops early.
markdownPieces :: Text -> [MarkdownPiece]
markdownPieces = go 1 . Text.splitOn "\n"
  where
    -- Strict in n: no chain of (+ 1) thunks on a long file.
    go :: Word32 -> [Text] -> [MarkdownPiece]
    go !_ [] = []
    go !n (l : ls)
      | isFence l = case break isFence ls of
          (body, _ : rest) ->
            Fenced (FencedBlock (Line n) l body True)
              : go (n + fromIntegral (length body) + 2) rest
          (body, []) -> [Fenced (FencedBlock (Line n) l body False)]
      | otherwise = Prose l : go (n + 1) ls

-- | A fenced block that markdown-unlit's default selector takes.
isCodePiece :: MarkdownPiece -> Bool
isCodePiece = \case
  Fenced b -> isCode b
  Prose _ -> False

isCode :: FencedBlock -> Bool
isCode b = "haskell" `elem` cs && "ignore" `notElem` cs
  where
    cs = fenceClasses b.fence

-- | @```@ or @~~~@ (or longer) after spaces.
isFence :: Text -> Bool
isFence l = "```" `Text.isPrefixOf` s || "~~~" `Text.isPrefixOf` s
  where
    s = Text.stripStart l

-- | The spaces before a fence: markdown-unlit drops that many
-- characters from each line of the block.
fenceIndent :: FencedBlock -> Int
fenceIndent b = Text.length (Text.takeWhile isSpace b.fence)

-- | A fence's classes, like markdown-unlit's parseClasses.
fenceClasses :: Text -> [Text]
fenceClasses l = Text.words (Text.map dot inner)
  where
    info = Text.stripStart (Text.dropWhile (`elem` ['`', '~']) (Text.stripStart l))
    inner = case Text.uncons info of
      Just ('{', rest) -> Text.takeWhile (/= '}') rest
      _ -> info
    dot c = if c == '.' then ' ' else c

-- | A block's sort key, like markdown-unlit's parseReorderingKey: the
-- first @top@ class gives @minBound@, the first @top:N@ (N an Int, as
-- @read@ takes it) gives @minBound + N@, overflow wraps, same as
-- markdown-unlit. Else 0.
reorderKey :: [Text] -> Int
reorderKey = \case
  [] -> 0
  "top" : _ -> minBound
  c : cs
    | Just k <- Text.stripPrefix "top:" c,
      Just n <- readMaybe (Text.unpack k) ->
        minBound + n
    | otherwise -> reorderKey cs

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

{- Note [Slicing]

Text has no index by code point: taking column c of a line walks c
code points. A span per token then costs O(column), so one very long
line (a 1 MiB module on one line) cost O(n^2) per page. So each line
of 'SourceLines' carries, built lazily the first time a column past
'shortColumns' is cut, the byte offset of each of its code points:
slicing is then two lookups and an O(1) slice of the line, linear over
a page. Short lines and small columns use plain take and drop.
-}

-- | A file's text by line, to cut out spans. Build it once per request
-- and keep it for that request only: it can hold one offset array per
-- long line. See Note [Slicing].
newtype SourceLines = SourceLines (Vector.Vector SourceLine)

-- | One line, and where each code point starts in it.
data SourceLine = SourceLine
  { text :: Text,
    -- | Byte offsets, one per code point, then the length. Built on
    -- first use.
    offsets :: ~(Unboxed.Vector Int)
  }

-- | From the file's text, as stored. A BOM is dropped (columns on line 1
-- don't count it), and so is the @\r@ of a CRLF.
sourceLines :: Text -> SourceLines
sourceLines =
  SourceLines . Vector.fromList . map line . Text.splitOn "\n" . stripBom
  where
    noCr l = fromMaybe l (Text.stripSuffix "\r" l)
    line l = let t = noCr l in SourceLine {text = t, offsets = offsetsOf t}

-- | Where each code point of a line starts, in UTF-8 bytes, then the
-- length.
offsetsOf :: Text -> Unboxed.Vector Int
offsetsOf t =
  Unboxed.fromListN (Text.length t + 1) (scanl (+) 0 (map utf8Size (Text.unpack t)))
  where
    utf8Size c
      | c < '\x80' = 1
      | c < '\x800' = 2
      | c < '\x10000' = 3
      | otherwise = 4

-- | Columns up to this are cut with take and drop. See Note [Slicing].
shortColumns :: Int
shortColumns = 256

-- | The text of a span (Note [Span convention] in
-- "HaskellReviewer.Core.Span"). Lines are joined with @\n@; a span past
-- the end gives what there is. Constant time per span, plus the copy of
-- a multi-line span (Note [Slicing]).
sliceSpan :: SourceLines -> Span -> Text
sliceSpan (SourceLines ls) s
  | l1 == l2 = cutLine (c1 - 1) (c2 - c1) (line l1)
  | otherwise =
      Text.intercalate "\n" $
        [cutLine (c1 - 1) maxBound (line l1)]
          <> [maybe "" (.text) (line l) | l <- [l1 + 1 .. l2 - 1]]
          <> [cutLine 0 (c2 - 1) (line l2)]
  where
    Span (Line a) (Col b) (Line c) (Col d) = s
    (l1, c1, l2, c2) = (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d)
    -- No line past the end: no shared empty line (Q-RTS-BLACKHOLE).
    line n = ls Vector.!? (n - 1)

-- | @n@ code points of a line from code point @from@ (both from 0), as
-- @take n . drop from@.
cutLine :: Int -> Int -> Maybe SourceLine -> Text
cutLine _ _ Nothing = ""
cutLine from n (Just l)
  | n <= 0 = ""
  | from + min n shortColumns <= shortColumns = Text.take n (Text.drop from l.text)
  | otherwise =
      let os = l.offsets
          end = Unboxed.length os - 1
          at k = os Unboxed.! max 0 (min end k)
          b1 = at from
          b2 = at (if n >= maxBound - from then end else from + max 0 n)
       in Text.Unsafe.takeWord8 (b2 - b1) (Text.Unsafe.dropWord8 b1 l.text)

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
