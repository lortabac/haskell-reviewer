module HaskellReviewer.Core.CppTest (tests) where

import HaskellReviewer.Core.Cpp
  ( CppOutput (..),
    CppProblem (..),
    PositionMap (..),
    isDirective,
    runCpp,
  )
import HaskellReviewer.Core.Parser (ParseSettings (..), defaultParseSettings)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | See Note [Directive lines] in "HaskellReviewer.Core.Cpp".
tests :: TestTree
tests =
  testGroup
    "Cpp"
    [ testCase "directives" $ do
        isDirective "#if FOO" @?= True
        isDirective "  #  ifdef FOO" @?= True
        isDirective "#endif" @?= True
        isDirective "#endif\r" @?= True
        isDirective "#include \"a.h\"" @?= True
        isDirective "#include_next <a.h>" @?= True
        isDirective "#define X 1" @?= True
        isDirective "#pragma once" @?= True
        isDirective "#ident \"x\"" @?= True
        isDirective "#import \"a.h\"" @?= True
        isDirective "#elifdef X" @?= True
        isDirective "#elifndef X" @?= True
        isDirective "#assert machine(x86)" @?= True
        isDirective "#unassert machine" @?= True
        isDirective "# 12 \"f.hs\"" @?= True
        isDirective "#12" @?= True
        isDirective "#" @?= True
        isDirective "  #  " @?= True
        isDirective "#_X" @?= True,
      testCase "not directives" $ do
        isDirective "#!/usr/bin/env runghc" @?= False
        isDirective "x# = 1" @?= False
        isDirective "(# a, b #)" @?= False
        isDirective "  #)" @?= False
        isDirective "  #. f" @?= False
        isDirective "## x" @?= False
        isDirective "{-# LANGUAGE CPP #-}" @?= False
        isDirective "-- #if" @?= False
        isDirective "x = 1" @?= False
        isDirective "" @?= False,
      testCase "no directive: the text as-is" $
        runCpp env "#!/usr/bin/env runghc\nmodule M where\nx# = 1\n"
          @?= Right
            CppOutput
              { text = "#!/usr/bin/env runghc\nmodule M where\nx# = 1\n",
                positions = SameLines
              },
      testCase "a directive: not supported" $
        runCpp env "module M where\n#if 1\n#endif\n" @?= Left CppNotSupported
    ]
  where
    env = defaultParseSettings.cpp
