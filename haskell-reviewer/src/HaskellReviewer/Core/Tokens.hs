{-# LANGUAGE StrictData #-}

-- | A module's tokens, packed (SPEC §11.4.4). For declaration heads and
-- bodies, highlighting and links.
module HaskellReviewer.Core.Tokens
  ( -- * Types
    Tokens,
    Token (..),
    TokenClass (..),

    -- * Building
    tokensFromList,
    noTokens,

    -- * Reading
    tokenCount,
    tokenAt,
    tokenList,
    tokensIn,
    tokenIndexFrom,
    tokenClassAt,
    tokenSpanAt,
    nameLikeClass,

    -- * Spans as words
    spanWords,
    wordsSpan,
  )
where

import Data.Vector.Unboxed qualified as Vector
import Data.Word (Word32)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))

{- Note [Packed tokens]

A module has a few thousand tokens, and we keep them for every module
of both sides. A list of records costs about 100 bytes a token; packed
in one unboxed vector, 5 words each (start line and column, end line
and column, class), it's 20 bytes.

Tokens are sorted by start. 'tokensIn' finds the first one with a
binary search.
-}

-- | In source order. See Note [Packed tokens].
newtype Tokens = Tokens (Vector.Vector Word32)
  deriving (Eq, Show)

-- | One token.
data Token = Token
  { span :: Span,
    kind :: TokenClass
  }
  deriving (Eq, Show)

-- | What a token is, for highlighting and comparing.
data TokenClass
  = Keyword
  | VarId
  | ConId
  | VarSym
  | ConSym
  | -- | @!@, @~@, @\@@, @.@, @*@, @-@, @%@ and the like: operators GHC's
    -- lexer names on its own, as they are syntax in some places.
    ReservedOp
  | -- | Brackets, commas, @::@, @=@, @->@ and other reserved symbols.
    Special
  | -- | Numbers.
    Literal
  | StringLit
  | CharLit
  | Pragma
  | Comment
  | Haddock
  | -- | All the tokens one CPP call made, as one token with the call's
    -- span.
    Macro
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Words per token.
width :: Int
width = 5

-- | Pack. The list should be sorted by start, for 'tokensIn'.
tokensFromList :: [Token] -> Tokens
tokensFromList ts = Tokens (Vector.fromListN (width * length ts) (concatMap words5 ts))
  where
    words5 t = spanWords t.span <> [fromIntegral (fromEnum t.kind)]

-- | None, as when the lexer failed.
noTokens :: Tokens
noTokens = Tokens Vector.empty

-- | How many.
tokenCount :: Tokens -> Int
tokenCount (Tokens v) = Vector.length v `div` width

-- | The token at this index, from 0. Out of range: 'Nothing'.
tokenAt :: Tokens -> Int -> Maybe Token
tokenAt ts i
  | i < 0 || i >= tokenCount ts = Nothing
  | otherwise =
      Just Token {span = tokenSpanAt ts i, kind = tokenClassAt ts i}

-- | All of them.
tokenList :: Tokens -> [Token]
tokenList ts = [t | i <- [0 .. tokenCount ts - 1], Just t <- [tokenAt ts i]]

-- | The index of the first token that starts at or after this place:
-- 'tokenCount' if none does. A binary search on the raw words.
tokenIndexFrom :: Tokens -> Line -> Col -> Int
tokenIndexFrom ts@(Tokens v) (Line l) (Col c) = go 0 (tokenCount ts)
  where
    go lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = (lo + hi) `div` 2
              at k = v Vector.! (mid * width + k)
           in if (at 0, at 1) < (l, c) then go (mid + 1) hi else go lo mid

-- | The class of the token at this index, which must be in range.
-- Cheaper than 'tokenAt' when only the class is needed.
tokenClassAt :: Tokens -> Int -> TokenClass
tokenClassAt (Tokens v) i = toClass (v Vector.! (i * width + 4))

-- | The span of the token at this index, which must be in range.
tokenSpanAt :: Tokens -> Int -> Span
tokenSpanAt (Tokens v) i = wordsSpan v (i * width)

-- | The classes that are always names, or words that look like names:
-- each such token gets an occurrence entry.
nameLikeClass :: TokenClass -> Bool
nameLikeClass = \case
  VarId -> True
  ConId -> True
  VarSym -> True
  ConSym -> True
  _ -> False

-- | A span as 4 words: start line and column, end line and column.
spanWords :: Span -> [Word32]
spanWords (Span (Line l1) (Col c1) (Line l2) (Col c2)) = [l1, c1, l2, c2]

-- | The span in the 4 words at this offset.
wordsSpan :: Vector.Vector Word32 -> Int -> Span
wordsSpan v o = Span (Line (w 0)) (Col (w 1)) (Line (w 2)) (Col (w 3))
  where
    w k = v Vector.! (o + k)

-- | Only tokensFromList writes the class word, so it is always in
-- range. Special is just a total fallback, no error.
toClass :: Word32 -> TokenClass
toClass n
  | n <= fromIntegral (fromEnum (maxBound :: TokenClass)) = toEnum (fromIntegral n)
  | otherwise = Special

-- | The tokens that start inside the span, in order.
tokensIn :: Span -> Tokens -> [Token]
tokensIn s ts = takeWhile inside (from (tokenIndexFrom ts s.startLine s.startCol))
  where
    inside t = (t.span.startLine, t.span.startCol) < (s.endLine, s.endCol)
    from i = case tokenAt ts i of
      Just t -> t : from (i + 1)
      Nothing -> []
