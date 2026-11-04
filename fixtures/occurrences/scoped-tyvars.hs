{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeAbstractions, DefaultSignatures #-}
{-# LANGUAGE RequiredTypeArguments, RankNTypes #-}
module ScopedTyVars where

import Data.Proxy (Proxy (..))

-- No forall: a binds in the signature only.
implicit :: a -> [a] -> [a]
implicit x xs = const x (Nothing :: Maybe a) : xs

-- A forall: a is in scope in the equations too.
explicit :: forall a b. (Show a) => a -> b -> String
explicit x _ = show (x :: a) <> go x
  where
    go :: a -> String
    go = show @a

-- A forall in a let, shadowing the outer a.
shadow :: forall a. a -> a
shadow x =
  let inner :: forall a. a -> a
      inner y = (y :: a)
   in inner x

-- Pattern signatures: c binds, a is in scope.
patSig :: forall a. a -> Int
patSig (x :: a) = case Just x of
  Just (y :: c) -> const 1 (y :: c)
  Nothing -> 0

-- Lambda and do with pattern signatures.
lam :: Maybe Int
lam = (\(n :: k) -> Just (n :: k)) 3 >>= \m -> do
  (v :: w) <- Just m
  pure (v :: w)

-- An expression signature with a forall: in scope in the expression.
expr :: Int
expr = (id :: forall t. t -> t) 1 + length ((\_ -> [] :: [t]) ())

-- Nested and visible foralls.
nested :: Int -> (forall b. b -> forall c. c -> b) -> Int
nested n _ = n

visible :: forall a -> a -> a
visible t x = (x :: t)

-- Type abstractions: @a binds anew, even when a is in scope.
abstr :: forall a. Proxy a -> Int
abstr @b Proxy = const 0 (Proxy @b)

-- In a constructor pattern: like a pattern signature.
conApp :: forall a. Maybe a -> Int
conApp (Just @a _) = 1
conApp (Nothing @z) = const 0 (Proxy @z)

-- Rules bind type variables over both sides.
{-# RULES "implicit/id" forall (x :: q) xs. implicit x xs = x : (xs :: [q]) #-}
{-# RULES "tyvars" forall r. forall (y :: r). implicit y [] = [y] #-}

-- A class's variables and a method's forall in the default method; an
-- instance signature's forall in the instance method.
class Def a where
  def :: forall b. a -> b -> a
  def x (_ :: b) = (x :: a)

instance Def [e] where
  def :: forall c. [e] -> c -> [e]
  def xs (_ :: c) = (xs :: [e])

-- In a class, the default signature's forall wins, whatever the order.
class Dflt a where
  default dflt :: forall c. (Show c) => a -> c -> String
  dflt :: forall b. a -> b -> String
  dflt _ y = show (y :: c)
