{-# LANGUAGE StrictData #-}

-- | Lexical highlighting (SPEC §5.6): a module's tokens and line regions
-- merged by position into coloured runs. Pure, built on demand. See
-- Note [Highlight runs].
module HaskellReviewer.Core.Highlight
  ( -- * Types
    Runs,
    Run (..),
    DisplayClass (..),

    -- * Highlighting
    highlight,
    highlightModule,
    displayClass,
    noRuns,

    -- * Reading
    runCount,
    runList,
    runsWords,
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

-- | Words per run.
width :: Int
width = 5

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
highlight tokens regions = case go (Pos 0 0) coloured (lineRegionList regions) of
  [] -> noRuns
  rs -> Runs (Vector.fromList (concatMap pack rs))
  where
    coloured =
      [ Piece {from = start t.span, to = end t.span, display = d}
      | t <- maybe [] tokenList tokens,
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
    piece from to d = Piece {from = from, to = to, display = d}
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
    pack p =
      let (Pos l1 c1, Pos l2 c2) = (p.from, p.to)
       in [l1, c1, l2, c2, fromIntegral (fromEnum p.display)]

-- | A place: line and column.
data Pos = Pos Word32 Word32
  deriving (Eq, Ord)

-- | A token, or what's left of it, with its class.
data Piece = Piece
  { from :: Pos,
    to :: Pos,
    display :: DisplayClass
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
    -- Only highlight writes the class word.
    toClass n
      | n <= fromIntegral (fromEnum (maxBound :: DisplayClass)) = toEnum (fromIntegral n)
      | otherwise = error ("Highlight: bad class word " <> show n)

-- | The flat array: 5 words a run (start line, start column, end line,
-- end column, class).
runsWords :: Runs -> Vector.Vector Word32
runsWords (Runs v) = v
