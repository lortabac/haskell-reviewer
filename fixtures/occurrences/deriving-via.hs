{-# LANGUAGE DerivingVia, DerivingStrategies, StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module DerivingVia where

import Data.Kind (Type)
import Data.Monoid (Sum (..))

newtype Score a = Score a
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (Sum a)
  deriving newtype (Eq)

newtype Age = Age Int

deriving via (Sum Int) instance Semigroup Age

newtype Wrap (f :: Type -> Type) a = Wrap (f a)

deriving via ((f :: Type -> Type) a) instance (Eq (f a)) => Eq (Wrap f a)
