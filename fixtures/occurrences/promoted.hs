{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}
module Promoted where

import Data.Kind (Type)

data Nat = Z | S Nat

data Color = Red | Green

-- A type with the same name as a constructor here.
data Red = RedT

data Vec (n :: Nat) (a :: Type) where
  Nil :: Vec 'Z a
  Cons :: a -> Vec n a -> Vec ('S n) a

-- Unticked: no type S here, so the constructor, a guess.
type One = S Z

-- Red is a type here: the type wins.
type R = Red

-- Green is only a constructor.
type G = Green

type family IsZero (n :: Nat) :: Bool where
  IsZero 'Z = 'True
  IsZero _ = 'False

-- A promoted list and tuple.
type L = '[ 'Z, S Z]
type P = '( 'Z, Green)

-- Not from here: free.
type B = True
