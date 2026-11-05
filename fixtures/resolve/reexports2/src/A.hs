{-# LANGUAGE PatternSynonyms, TypeData, TypeFamilies #-}
module A (C (..), TD (..), T (..), pattern P, R (..)) where

class C a where
  type F a
  m :: a -> F a

type data TD = TA | TB

data T = MkT Int | T

pattern P :: T
pattern P = T

data R = R {fld :: Int}
