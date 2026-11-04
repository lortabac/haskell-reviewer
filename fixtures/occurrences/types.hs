{-# LANGUAGE GADTs, TypeFamilies, DataKinds, PolyKinds, StandaloneKindSignatures #-}
{-# LANGUAGE ExistentialQuantification, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies, UndecidableInstances #-}
module Types where

import Data.Kind (Type)

-- Data heads: over the constructors.
data Pair a b = Pair a b | Swap {first :: b, second :: a}

newtype Wrap (f :: k -> Type) (x :: k) = Wrap (f x)

-- Existentials.
data Some = forall s. (Show s) => Some s | None

-- GADTs: constructors don't see the head.
data Expr a where
  Lit :: Int -> Expr Int
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
  Pack :: forall b c. b -> c -> Expr (b, c)

data Kinded :: Type -> Type where
  K :: x -> Kinded x

-- Synonyms.
type Twice a = (a, a)

type Const a b = a

-- Standalone kind signatures: implicit, the signature only.
type Phantom :: k -> Type
data Phantom a = Phantom

-- Families.
type family Elem c where
  Elem [e] = e
  Elem (Maybe e) = e

type family Open a :: Type

type instance Open [x] = x

data family Vec a

data instance Vec (Maybe m) = VNothing | VJust m

type family Inj a = r | r -> a

-- Classes: the head over the whole body.
class Container f where
  type Item f
  type Item f = Int
  empty :: f a
  insert :: a -> f a -> f a
  insert _ c = c

class Convert a b | a -> b where
  convert :: a -> b

-- Instances: free head variables over the instance.
instance Container [] where
  type Item [] = Bool
  empty = []
  insert = (:)

instance (Show a) => Convert (Maybe a) String where
  convert = maybe "" show

deriving instance (Show a, Show b) => Show (Pair a b)
