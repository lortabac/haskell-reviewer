{-# LANGUAGE ExplicitNamespaces, PatternSynonyms #-}
module ExportsSubs
  ( T (..),
    U (A, field),
    V,
    C (m),
    D (..),
    x,
    type (:+:),
    pattern Pat,
    ExportsSubs.y,
    Other.z,
  )
where

data T = T1 | T2 {t :: Int}
data U = A | B {field :: Int, other :: Int}
data V = V
class C a where
  m :: a
  n :: a
class D a where
  type F a
  d :: a
data a :+: b = a :+: b
x, y, z, w :: Int
x = 1
y = 2
z = 3
w = 4
pattern Pat :: Int
pattern Pat = 0
