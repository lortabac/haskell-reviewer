{-# LANGUAGE CPP #-}
module CppDirectives where

#if MIN_VERSION_base(4,18,0)
x :: Int
x = 1
#endif
