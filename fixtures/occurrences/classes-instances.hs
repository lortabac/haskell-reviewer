{-# LANGUAGE InstanceSigs #-}
module Classes where

class Container f where
  empty :: f a
  insert :: a -> f a -> f a
  insert _ c = c
  {-# MINIMAL empty #-}

newtype Box a = Box [a]

instance Container Box where
  empty = Box []
  insert :: a -> Box a -> Box a
  insert x (Box xs) = Box (x : xs)

instance Show (Box a) where
  show _ = "box"
  showsPrec _ _ = id
