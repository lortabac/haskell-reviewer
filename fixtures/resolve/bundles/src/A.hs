{-# LANGUAGE PatternSynonyms #-}
module A (T (P)) where

data T = MkT Int

pattern P :: T
pattern P = MkT 0
