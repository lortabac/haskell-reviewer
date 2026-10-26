{-# LANGUAGE CPP #-}
#define TY(a) Maybe a
#define NAME value
module CppDecls where

-- | A macro in the head.
NAME :: TY(Int)
NAME = Nothing

#if 0
hidden :: Int
#else
shown :: Int
#endif
shown = __LINE__
