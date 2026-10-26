{-# LANGUAGE CPP #-}
#define PAIR(a, b) a, b
#define ONE one
module CppMacroExport
  ( before,
	PAIR(foo, bar),
    PAIR ( multi,
           line ),
    ONE, after
  ) where
