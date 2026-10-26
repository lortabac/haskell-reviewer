{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, TypeData, DataKinds #-}
module SynonymsFamilies where

import Data.Kind (Type)

-- | A synonym.
type Name = String

type Pair :: Type -> Type
-- | Pairs.
type Pair a = (a, a)

-- | Open.
type family Elem c

-- | Injective.
type family Inj a = r | r -> a

-- | Closed.
type family IsInt a where
  IsInt Int = 'True
  IsInt a = 'False

type family Abstract a where ..

data family Vec a

type data Nat = Zero | Succ Nat

type a + b = Either a b
infixl 6 +
