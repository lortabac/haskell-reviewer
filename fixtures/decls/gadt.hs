{-# LANGUAGE GADTs, KindSignatures #-}
module Gadt where

import Data.Kind (Type)

-- | Expressions.
data Expr :: Type -> Type where
  -- | Literals.
  IntE, BoolE :: Int -> Expr Int
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  -- | A record one.
  Pair :: { first :: Expr a, second :: !(Expr b) } -> Expr (a, b)
  deriving ()

data T a where
  MkT :: forall a. Show a => a -> T a
