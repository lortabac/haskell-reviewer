{-# LANGUAGE CPP #-}
module CppMinVersion
  ( always
#if MIN_VERSION_bytestring(0,10,0) && MIN_VERSION_text_short(99,0,0)
  , anyPackage
#endif
#ifdef MIN_VERSION_text_short
  , inBuildDepends
#else
  , notInBuildDepends
#endif
  ) where
