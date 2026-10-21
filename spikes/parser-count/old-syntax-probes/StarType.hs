{-# LANGUAGE TypeOperators, KindSignatures #-}
module StarType where
data P (f :: * -> *) = P
type a * b = (a, b)
