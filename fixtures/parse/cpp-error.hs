{-# LANGUAGE CPP #-}
module CppError (x) where

#if 0
#error not seen
#endif
#error boom
#warning careful
x = 1
