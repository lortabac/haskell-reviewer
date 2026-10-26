{-# LANGUAGE DerivingStrategies, DerivingVia, DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
module Deriving where

import qualified Prelude as P

data T a b = T a b
  deriving (Eq, Functor)
  deriving stock (Foldable)
  deriving anyclass (NFData)

newtype App a = App (StateT S IO a)
  deriving newtype (Functor, Applicative, Monad, MonadState S)

-- Arity 2; arity clamped to the parameters.
data P a b c = P deriving (Bifunctor)
data Z = Z deriving (Functor)

-- A qualified class.
data Q = Q deriving (P.Show)

newtype N = N Int

-- | Standalone, each strategy.
deriving instance Show a => Show (T a b)
deriving stock instance Ord (T a b)
deriving newtype instance Num N
deriving anyclass instance ToJSON (T x y)
deriving via (Sum Int) instance Semigroup N
