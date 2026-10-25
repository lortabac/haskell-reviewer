{-# LANGUAGE CPP #-}
#define TWICE(x) x x
module CppParseError where

#if 1
y = \
  2
#endif
z = TWICE(1) = 2
