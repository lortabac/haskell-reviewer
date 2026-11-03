{-# LANGUAGE ViewPatterns #-}

-- | markdown-unlit 0.6.0 behind the shapes of our unlitMarkdown and
-- unlitLoose, plus a layout with a position map (the T02b prototype).
module LibUnlit
  ( Block (..),
    blocks,
    selectedBlocks,
    selects,
    reorderKey,
    libUnlitMarkdownRaw,
    libUnlitLooseRaw,
    libLayout,
    layoutMatchesLib,
    fenceLines,
  )
where

import Data.Char (isSpace)
import Data.IntSet qualified as IntSet
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import HaskellReviewer.Core.Cpp.Positions
  ( Piece (..),
    Pos (..),
    PositionMap,
    Run (..),
    insertLine,
    sameLines,
  )
import HaskellReviewer.Core.SourceText (LooseUnlit (..), unlitBirdLatex)
import Text.Markdown.Unlit (CodeBlock (..), Selector (..))
import Text.Markdown.Unlit qualified as Lib
import Text.Read (readMaybe)

-- | The default selector, as the library's run uses.
selector :: Selector
selector = "haskell" :&: Not "ignore"

-- | A block from the library's parse, with what we add: its index in
-- the file, its fence's indent and whether that fence has a close.
data Block = Block
  { index :: Int,
    classes :: [String],
    content :: [String],
    -- | File line of the first content line.
    start :: Int,
    -- | Chars dropped from each line (isSpace chars before the fence).
    indent :: Int,
    fence :: Text
  }

-- | All blocks, selected or not.
blocks :: Text -> [Block]
blocks t = zipWith mk [0 ..] (Lib.parse (Text.unpack t))
  where
    ls = Vector.fromList (Text.splitOn "\n" t)
    mk i cb =
      let f = fromMaybe "" (ls Vector.!? (cb.codeBlockStartLine - 2))
       in Block
            { index = i,
              classes = cb.codeBlockClasses,
              content = cb.codeBlockContent,
              start = cb.codeBlockStartLine,
              indent = Text.length (Text.takeWhile isSpace f),
              fence = f
            }

-- | The blocks the default selector takes, in file order.
selectedBlocks :: Text -> [Block]
selectedBlocks = filter (selects . (.classes)) . blocks

-- | Our copy of the library's toPredicate, for the default selector.
selects :: [String] -> Bool
selects = go selector
  where
    go = \case
      Class c -> elem c
      Not p -> not . go p
      a :&: b -> (&&) <$> go a <*> go b
      a :|: b -> (||) <$> go a <*> go b

-- | Our copy of the library's parseReorderingKey.
reorderKey :: [String] -> Int
reorderKey = \case
  [] -> 0
  "top" : _ -> minBound
  ('t' : 'o' : 'p' : ':' : (readMaybe -> Just n)) : _ -> minBound + n
  _ : cs -> reorderKey cs

-- | Like unlitMarkdown: the library's text as is, with its #line
-- lines. Nothing when no block is selected. That's decided from
-- parse's blocks, so no sort and no readMaybe of top: keys.
libUnlitMarkdownRaw :: FilePath -> Text -> Maybe Text
libUnlitMarkdownRaw src t
  | null (selectedBlocks t) = Nothing
  | otherwise = Just (Text.pack (Lib.unlit src selector (Text.unpack t)))

-- | File lines of each fence, open and close, as the library pairs
-- them. An unclosed fence hides everything to the end.
fenceLines :: Text -> [(Int, Int)]
fenceLines t = map span1 (blocks t)
  where
    total = length (Text.splitOn "\n" t)
    span1 b =
      let close = b.start + length b.content
       in (b.start - 1, min close total)

-- | Like unlitLoose, but the Bird/LaTeX check skips the library's
-- fenced regions.
libUnlitLooseRaw :: Text -> (Text, LooseUnlit)
libUnlitLooseRaw t
  | not birdOrLatex,
    Just m <- libUnlitMarkdownRaw "input" t =
      (m, UsedMarkdownUnlit)
  | otherwise = (unlitBirdLatex t, UsedGhcUnlit)
  where
    hidden = IntSet.fromList (concat [[a .. b] | (a, b) <- fenceLines t])
    birdOrLatex =
      or
        [ "\\begin{code}" `Text.isPrefixOf` Text.stripStart l || ">" `Text.isPrefixOf` l
        | (n, l) <- zip [1 ..] (Text.splitOn "\n" t),
          not (IntSet.member n hidden)
        ]

-- | The T02b prototype. In file order with no indent: same lines,
-- prose blanked, no map. Else the blocks one after the other (no
-- #line), and a map from each output line to its file line and column.
libLayout :: Text -> Maybe (Text, PositionMap)
libLayout t = case selectedBlocks t of
  [] -> Nothing
  bs
    | inPlace bs ordered -> Just (blanked bs, sameLines)
    | otherwise -> Just (moved ordered)
    where
      ordered = sortOn (\b -> (reorderKey b.classes, b.index)) bs
  where
    inPlace bs ordered =
      map (.index) ordered == map (.index) bs && all ((== 0) . (.indent)) bs
    blanked bs =
      let code =
            IntSet.fromList
              (concat [[b.start .. b.start + length b.content - 1] | b <- bs])
       in Text.intercalate
            "\n"
            [ if IntSet.member n code then l else ""
            | (n, l) <- zip [1 ..] (Text.splitOn "\n" t)
            ]
    moved ordered =
      let ls =
            [ (Text.pack l, Pos (b.start + j) (b.indent + 1))
            | b <- ordered,
              (j, l) <- zip [0 ..] b.content
            ]
          pm = foldr add sameLines (zip [1 ..] ls)
          add (n, (l, p)) = insertLine n [Run l (Copied p)]
       in (Text.intercalate "\n" (map fst ls), pm)

-- | Our layout and the library's text, both without blank and #line
-- lines, are the same.
layoutMatchesLib :: Text -> Bool
layoutMatchesLib t = case (libLayout t, libUnlitMarkdownRaw "input" t) of
  (Just (ours, _), Just lib) -> clean ours == clean lib
  (Nothing, Nothing) -> True
  _ -> False
  where
    clean =
      filter (\l -> not (Text.null (Text.strip l)) && not ("#line " `Text.isPrefixOf` l))
        . Text.splitOn "\n"
