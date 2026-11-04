{-# LANGUAGE LambdaCase #-}

-- | Haddock for the module.
module Basic (T (..), f, g) where

import Data.List (sort)

-- A plain comment.
data T = Int :+ Int | C !Int

f :: Int -> [Int]
f x = sort [x + 2, negate x, 0x1F]

g = \case
  C n -> "😀" <> show n <> show 'c' <> show 1.5
  _ ->	"tab"
