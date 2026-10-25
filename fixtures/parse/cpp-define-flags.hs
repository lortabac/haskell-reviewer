{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -DFROM_PRAGMA -UUNDONE -Wall #-}
module CppDefineFlags
  ( FROM_CPP
#ifdef SPACED
  , spaced
#endif
#ifdef FROM_GHC_OPTIONS
  , fromGhcOptions
#endif
#if OPT_P == 2
  , optP
#endif
#ifdef FROM_PRAGMA
  , fromPragma
#endif
#ifdef UNDONE
  , undone
#endif
  ) where
