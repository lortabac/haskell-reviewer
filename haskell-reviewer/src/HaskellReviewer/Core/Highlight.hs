{-# LANGUAGE StrictData #-}

-- | Lexical highlighting (SPEC §5.6): a module's tokens and line regions
-- merged by position into coloured runs. Pure, built on demand. See
-- Note [Highlight runs].
module HaskellReviewer.Core.Highlight
  ( -- * Types
    Runs,
    Run (..),
    DisplayClass (..),
    LinkedRuns,

    -- * Highlighting
    highlight,
    highlightModule,
    highlightLinked,
    displayClass,
    noRuns,

    -- * Reading
    runCount,
    runList,
    runsWords,
    linkedRunCount,
    linkedRunList,
    linkedRunsWords,
    linkedRunWidth,

    -- * Building
    linkedRunsFromList,
  )
where

import Data.Vector.Unboxed qualified as Vector
import Data.Word (Word32)
import HaskellReviewer.Core.LineRegions
  ( LineKind (..),
    LineRegion (..),
    LineRegions,
    lineRegionList,
  )
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    ParsedModule (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
  )
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (Token (..), TokenClass, Tokens, tokenList)
import HaskellReviewer.Core.Tokens qualified as Tokens (TokenClass (..))

{- Note [Highlight runs]

A run is a span and a display class. Runs are sorted, never overlap,
and use file positions (Note [Span convention] in
HaskellReviewer.Core.Span). Plain text gets no run. Packed like tokens:
5 words a run (start line and column, end line and column, class), so
the server can send the array as it is.

Inputs: the second lex's tokens (Note [Second lex] in
HaskellReviewer.Core.Parser.Lex) and the line regions GHC's lexer never
sees: CPP directives, excluded lines and literate prose.

- A region is whole lines: its run goes from (first, 1) to (last + 1, 1).
  The client clips it at the end of the file.
- Region lines win. A token that crosses a region (a {- -} around #if
  lines, a block comment across bird chunks with prose between) is cut
  in two around it; empty pieces are dropped.
- A run never starts before the end of the one before (odd Macro spans).
- No lex (lex failure, most text-only modules): regions only.

Class mapping: Special (brackets, commas, =, ->) is plain. Macro (a CPP
call's tokens) is pragma. Haddock is comment. Directive lines get their
own class (the client gives them the pragma colour); excluded lines are
greyed; prose is comment.

'highlightLinked' gives the same runs with a sixth word: the link of the
token the run comes from (0 for regions and tokens with none). Both
pieces of a token cut by a region keep its link. For the source view
(Note [Source view] in "HaskellReviewer.Server.Wire.Source").

Built per request, not stored: it's linear in tokens + regions (a few
ms for 100k tokens) and storing runs would cost about 20 bytes a token
on every module.
-}

-- | Sorted, not overlapping. See Note [Highlight runs].
newtype Runs = Runs (Vector.Vector Word32)
  deriving (Eq, Show)

-- | A span of the file and its class. See Note [Highlight runs].
data Run = Run
  { span :: Span,
    display :: DisplayClass
  }
  deriving (Eq, Show)

-- | The SPEC §5.6 classes, plus CPP directive lines. The 'Enum' numbers
-- go on the wire: only add at the end.
data DisplayClass
  = KeywordClass
  | -- | Capitalised names and operators starting with @:@.
    TypeConClass
  | VariableClass
  | OperatorClass
  | LiteralClass
  | -- | Haddock and literate prose too.
    CommentClass
  | -- | CPP calls too.
    PragmaClass
  | -- | Lines CPP skipped.
    ExcludedClass
  | -- | CPP directive lines.
    DirectiveClass
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Like 'Runs', 6 words a run: the 5 of 'Runs', then the link. See
-- Note [Highlight runs].
newtype LinkedRuns = LinkedRuns (Vector.Vector Word32)
  deriving (Eq, Show)

-- | Words per run.
width :: Int
width = 5

-- | Words per linked run: the 5 of a run, then the link.
linkedRunWidth :: Int
linkedRunWidth = 6

-- | None. Shared.
noRuns :: Runs
noRuns = Runs Vector.empty

-- | A token's class. 'Nothing': plain.
displayClass :: TokenClass -> Maybe DisplayClass
displayClass = \case
  Tokens.Keyword -> Just KeywordClass
  Tokens.ConId -> Just TypeConClass
  Tokens.ConSym -> Just TypeConClass
  Tokens.VarId -> Just VariableClass
  Tokens.VarSym -> Just OperatorClass
  Tokens.ReservedOp -> Just OperatorClass
  Tokens.Special -> Nothing
  Tokens.Literal -> Just LiteralClass
  Tokens.StringLit -> Just LiteralClass
  Tokens.CharLit -> Just LiteralClass
  Tokens.Comment -> Just CommentClass
  Tokens.Haddock -> Just CommentClass
  Tokens.Pragma -> Just PragmaClass
  Tokens.Macro -> Just PragmaClass

-- | A module's runs.
highlightModule :: ModuleParse -> Runs
highlightModule = \case
  Parsed m -> highlight (Just m.tokens) m.regions
  TextOnly t -> highlight tokens t.regions
    where
      tokens = case t.reason of
        ParseFailed _ ts -> ts
        _ -> Nothing

-- | Merge tokens (sorted by start) and regions. See Note [Highlight
-- runs].
highlight :: Maybe Tokens -> LineRegions -> Runs
highlight tokens regions = case merge (const 0) tokens regions of
  [] -> noRuns
  ps -> Runs (Vector.fromList (concatMap pack ps))
  where
    pack p =
      let (Pos l1 c1, Pos l2 c2) = (p.from, p.to)
       in [l1, c1, l2, c2, fromIntegral (fromEnum p.display)]

-- | Like 'highlight', with each run's link: the function gives a
-- token's link word by its index. See Note [Highlight runs].
highlightLinked :: (Int -> Word32) -> Maybe Tokens -> LineRegions -> LinkedRuns
highlightLinked linkOf tokens regions =
  LinkedRuns (Vector.fromList (concatMap pack (merge linkOf tokens regions)))
  where
    pack p =
      let (Pos l1 c1, Pos l2 c2) = (p.from, p.to)
       in [l1, c1, l2, c2, fromIntegral (fromEnum p.display), p.link]

-- | The pieces of the runs, in order.
merge :: (Int -> Word32) -> Maybe Tokens -> LineRegions -> [Piece]
merge linkOf tokens regions = go (Pos 0 0) coloured (lineRegionList regions)
  where
    coloured =
      [ Piece {from = start t.span, to = end t.span, display = d, link = linkOf i}
      | (i, t) <- zip [0 ..] (maybe [] tokenList tokens),
        Just d <- [displayClass t.kind]
      ]
    -- prev: where the last run ended.
    go prev ps rs = case rs of
      r : rs'
        | maybe True (\p -> p.from >= regionStart r) (headOf ps) ->
            emit prev (piece (regionStart r) (regionEnd r) (regionClass r.kind)) $
              go (max prev (regionEnd r)) ps rs'
      _ -> case ps of
        [] -> []
        p : ps' -> case rs of
          -- Cut at the region: before it now, the rest after it.
          r : _
            | p.to > regionStart r ->
                let rest = [p {from = regionEnd r} | p.to > regionEnd r]
                 in emit prev p {to = regionStart r} $
                      go (max prev (regionStart r)) (rest <> ps') rs
          _ -> emit prev p (go (max prev p.to) ps' rs)
    headOf = \case
      p : _ -> Just p
      [] -> Nothing
    piece from to d = Piece {from = from, to = to, display = d, link = 0}
    -- Clamped to prev; dropped if empty.
    emit prev p rest
      | from' < p.to = p {from = from'} : rest
      | otherwise = rest
      where
        from' = max prev p.from
    regionStart r = let Line l = r.firstLine in Pos l 1
    regionEnd r = let Line l = r.lastLine in Pos (l + 1) 1
    regionClass = \case
      CppDirectives -> DirectiveClass
      CppExcluded -> ExcludedClass
      Prose -> CommentClass
    start s = let (Line l, Col c) = (s.startLine, s.startCol) in Pos l c
    end s = let (Line l, Col c) = (s.endLine, s.endCol) in Pos l c

-- | A place: line and column.
data Pos = Pos Word32 Word32
  deriving (Eq, Ord)

-- | A token, or what's left of it, with its class.
data Piece = Piece
  { from :: Pos,
    to :: Pos,
    display :: DisplayClass,
    -- | 0 when not linked.
    link :: Word32
  }

-- | How many.
runCount :: Runs -> Int
runCount (Runs v) = Vector.length v `div` width

-- | All of them, in order.
runList :: Runs -> [Run]
runList r@(Runs v) = map at [0 .. runCount r - 1]
  where
    at i =
      Run
        { span = Span (Line (w i 0)) (Col (w i 1)) (Line (w i 2)) (Col (w i 3)),
          display = toClass (w i 4)
        }
    w i k = v Vector.! (i * width + k)

-- | A class word back. Only highlight writes them.
toClass :: Word32 -> DisplayClass
toClass n
  | n <= fromIntegral (fromEnum (maxBound :: DisplayClass)) = toEnum (fromIntegral n)
  | otherwise = error ("Highlight: bad class word " <> show n)

-- | The flat array: 5 words a run (start line, start column, end line,
-- end column, class).
runsWords :: Runs -> Vector.Vector Word32
runsWords (Runs v) = v

-- | How many.
linkedRunCount :: LinkedRuns -> Int
linkedRunCount (LinkedRuns v) = Vector.length v `div` linkedRunWidth

-- | All of them, in order, each with its link.
linkedRunList :: LinkedRuns -> [(Run, Word32)]
linkedRunList r@(LinkedRuns v) = map at [0 .. linkedRunCount r - 1]
  where
    at i =
      ( Run
          { span = Span (Line (w i 0)) (Col (w i 1)) (Line (w i 2)) (Col (w i 3)),
            display = toClass (w i 4)
          },
        w i 5
      )
    w i k = v Vector.! (i * linkedRunWidth + k)

-- | The flat array: 6 words a run (start line, start column, end line,
-- end column, class, link).
linkedRunsWords :: LinkedRuns -> Vector.Vector Word32
linkedRunsWords (LinkedRuns v) = v

-- | Back from 'linkedRunList'. The runs should be sorted and not
-- overlap. Classes and links as given.
linkedRunsFromList :: [(Run, Word32)] -> LinkedRuns
linkedRunsFromList rs = LinkedRuns (Vector.fromList (concatMap words6 rs))
  where
    words6 (r, link) =
      let Span (Line l1) (Col c1) (Line l2) (Col c2) = r.span
       in [l1, c1, l2, c2, fromIntegral (fromEnum r.display), link]
