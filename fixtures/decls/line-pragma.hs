{-# LINE 1 "Gen.y" #-}
module LinePragma (a, b) where
import Data.List (sort)

a :: Int
a = 1
{-# LINE 100 "Other.y" #-}
b :: Int
b = {-# COLUMN 40 #-} 2
s = "{-# LINE 7 \"x\" #-}"
