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
    words5 t =
      let Span (Line l1) (Col c1) (Line l2) (Col c2) = t.span
       in [l1, c1, l2, c2, fromIntegral (fromEnum t.kind)]

-- | None, as when the lexer failed.
noTokens :: Tokens
noTokens = Tokens Vector.empty

-- | How many.
tokenCount :: Tokens -> Int
tokenCount (Tokens v) = Vector.length v `div` width

-- | The token at this index, from 0. Out of range: 'Nothing'.
tokenAt :: Tokens -> Int -> Maybe Token
tokenAt ts@(Tokens v) i
  | i < 0 || i >= tokenCount ts = Nothing
  | otherwise =
      Just
        Token
          { span = Span (Line (w 0)) (Col (w 1)) (Line (w 2)) (Col (w 3)),
            kind = toClass (w 4)
          }
  where
    w k = v Vector.! (i * width + k)
    -- Only tokensFromList writes the class word, so it is always in
    -- range. Special is just a total fallback, no error.
    toClass n
      | n <= fromIntegral (fromEnum (maxBound :: TokenClass)) = toEnum (fromIntegral n)
      | otherwise = Special

-- | All of them.
tokenList :: Tokens -> [Token]
tokenList ts = [t | i <- [0 .. tokenCount ts - 1], Just t <- [tokenAt ts i]]

-- | The tokens that start inside the span, in order.
tokensIn :: Span -> Tokens -> [Token]
tokensIn s ts = takeWhile inside (from (firstAtOrAfter 0 (tokenCount ts)))
  where
    start t = (t.span.startLine, t.span.startCol)
    inside t = start t < (s.endLine, s.endCol)
    from i = case tokenAt ts i of
      Just t -> t : from (i + 1)
      Nothing -> []
    -- The first index whose token starts at or after the span.
    firstAtOrAfter lo hi
      | lo >= hi = lo
      | otherwise =
          let mid = (lo + hi) `div` 2
           in case tokenAt ts mid of
                Just t
                  | start t < (s.startLine, s.startCol) -> firstAtOrAfter (mid + 1) hi
                _ -> firstAtOrAfter lo mid
