{-# LANGUAGE CPP #-}
module Cpp (a, b) where

#define TWICE(x) (x + x)
#if 0
a = excluded
#elif 0
a = also excluded
#else
a = TWICE(1)
#endif

b = 1 {- comment
#if 1
inside
#endif
-} + TWICE(
  2)
