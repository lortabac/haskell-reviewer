-- | Tests for the CPP evaluator. The expected outputs come from gcc
-- -E -undef -traditional -x assembler-with-cpp (gcc 15), as GHC runs
-- it, except where we warn instead of failing. See Note [Traditional
-- mode] in "HaskellReviewer.Core.Cpp".
module HaskellReviewer.Core.CppTest (tests) where

import Control.Exception (evaluate)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Cpp
  ( CppEnv (..),
    CppOutput (..),
    CppProblem (..),
    CppWarning (..),
    FlagSource (..),
    Limit (..),
    MacroFlag (..),
    MacroFlags (..),
    Piece (..),
    Pos (..),
    PositionMap (..),
    Region (..),
    RegionKind (..),
    isDirective,
    macroFlags,
    mapSpan,
    renderCppWarning,
    runCpp,
    sameLines,
  )
import HaskellReviewer.Core.CppPropertyTest qualified as Properties
import HaskellReviewer.Core.Project (GhcVersion (..), PackageName (..), Platform (..))
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Effects.Parser (cppLimitsWarmUp, cppWarmUp)
import System.Timeout (timeout)
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
-- Runs inside warmUpParser, which runs CPP too (see test/Main.hs).
tests =
  testGroup
    "Cpp"
    [ directiveLines,
      basics,
      traditional,
      macros,
      directives,
      conditions,
      ghcMacros,
      positions,
      limits,
      Properties.tests
    ]

-- * Helpers

-- | GHC 9.14.1 on x86_64 Linux, depending on base and text-short.
env :: CppEnv
env =
  CppEnv
    { ghc = GhcVersion [9, 14, 1],
      platform = Platform {os = "linux", arch = "x86_64"},
      flags = [],
      buildDepends = [PackageName "base", PackageName "text-short"]
    }

-- | Run on these lines.
run :: [Text] -> CppOutput
run = runCpp env [] . Text.intercalate "\n"

-- | The output lines.
cpp :: [Text] -> [Text]
cpp = outLines . run

outLines :: CppOutput -> [Text]
outLines o = Text.splitOn "\n" o.text

-- | The warnings, as lines and problems.
warns :: [Text] -> [(Int, CppProblem)]
warns ls = [(w.line, w.problem) | w <- (run ls).warnings]

-- | Is this code kept, after these lines?
keeps :: [Text] -> Bool
keeps ls = "yes" `elem` cpp (ls <> ["yes", "#endif"])

forceOutput :: CppOutput -> Int
forceOutput o =
  Text.length o.text
    + length (map renderCppWarning o.warnings)
    + length o.regions
    + (case o.positions of PositionMap m -> sum (map IntMap.size (IntMap.elems m)))

spanOf :: Int -> Int -> Int -> Int -> Span
spanOf a b c d =
  Span (Line (w a)) (Col (w b)) (Line (w c)) (Col (w d))
  where
    w = fromIntegral

-- * Tests

-- | See Note [Directive lines] in "HaskellReviewer.Core.Cpp".
directiveLines :: TestTree
directiveLines =
  testGroup
    "directive lines"
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
        isDirective "" @?= False
    ]

basics :: TestTree
basics =
  testGroup
    "basics"
    [ testCase "no directive and no flag: the text as is" $ do
        let t = "#!/usr/bin/env runghc\nmodule M where\nx# = FOO\r\n"
        runCpp env [] t
          @?= CppOutput {text = t, positions = sameLines, regions = [], warnings = []},
      testCase "a built-in name alone is enough to run" $ do
        (runCpp env [] "v = __GLASGOW_HASKELL__\n").text @?= "v = 914\n"
        (runCpp env [] "v = MIN_VERSION_base(1,2,3) linux_HOST_OS\n").text @?= "v = 1 1\n"
        let t = "x__y = a_HOST_b\n"
        runCpp env [] t
          @?= CppOutput {text = t, positions = sameLines, regions = [], warnings = []},
      testCase "a name split by a backslash, a gap or a comment: CPP runs" $ do
        (runCpp env [] "v = __GLASGOW_\\\nHASKELL__\n").text @?= "v = 914\n\n"
        (runCpp env [] "v = linux_HO\\\nST_OS\n").text @?= "v = 1\n\n"
        (runCpp env [] "s = \"a\\  \n\\b\"\n").text @?= "s = \"a\\b\"\n\n"
        (runCpp env [] "x = 1 /* c */\n").text @?= "x = 1 \n",
      testCase "a -D flag alone is enough to run" $ do
        (runCpp env [Define "FOO" "1"] "x = FOO\n").text @?= "x = 1\n"
        (runCpp env {flags = [Define "FOO" "1"]} [] "x = FOO\n").text @?= "x = 1\n",
      testCase "same lines, directives and excluded lines blank" $ do
        let o = run ["a", "#if 0", "b", "c", "#else", "d", "#endif", "e"]
        outLines o @?= ["a", "", "", "", "", "d", "", "e"]
        o.regions
          @?= [ Region DirectiveLines 2 2,
                Region Excluded 3 4,
                Region DirectiveLines 5 5,
                Region DirectiveLines 7 7
              ]
        o.positions @?= sameLines,
      testCase "the warm-up does everything (Q-RTS-BLACKHOLE)" $ do
        -- Every line must do something, so the shared values get built.
        assertBool "warnings" (length cppWarmUp.warnings >= 20)
        let PositionMap m = cppWarmUp.positions
        let expanded = [() | c <- IntMap.elems m, Expanded _ _ <- IntMap.elems c]
        assertBool "expansions" (length expanded >= 8),
      testCase "the limits warm-up hits every limit" $ do
        let hit = [l | CppWarning {problem = LimitHit l} <- cppLimitsWarmUp.warnings]
        sort hit
          @?= [ ExpansionDepth,
                ExpansionWork,
                IfDepth,
                ExpressionDepth,
                ArgCount,
                DefineCount
              ]
        let ws = cppLimitsWarmUp.warnings
        [() | CppWarning {problem = WarningsDropped} <- ws] @?= [()],
      testCase "the trailing newline stays" $
        (runCpp env [] "#if 1\nx\n#endif\n").text @?= "\nx\n\n",
      testCase "CRLF: the \\r goes, as in gcc" $
        cpp ["a\r", "#if 1\r", "d\r", "#endif\r"] @?= ["a", "", "d", ""],
      testCase "regions: a skipped part's inner directives are excluded" $
        (run ["#if 0", "#if 1", "#endif", "#else", "#endif"]).regions
          @?= [ Region DirectiveLines 1 1,
                Region Excluded 2 3,
                Region DirectiveLines 4 5
              ]
    ]

traditional :: TestTree
traditional =
  testGroup
    "traditional mode"
    [ testCase "backslash-newline joins code lines" $
        cpp ["#define FOO 42", "foo\\", "bar FOO", "d"] @?= ["", "foobar 42", "", "d"],
      testCase "string gaps are joined too" $
        cpp ["#if 1", "\"s\\", "t\"", "#endif"] @?= ["", "\"st\"", "", ""],
      testCase "blanks after the backslash still join" $
        cpp ["#if 1", "a \\  ", "b", "#endif"] @?= ["", "a b", "", ""],
      testCase "CRLF after a backslash joins" $
        cpp ["#if 1", "b \\\r", "c\r", "#endif"] @?= ["", "b c", "", ""],
      testCase "a comment goes, with nothing in its place" $
        cpp ["#if 1", "a /* x", "y */ b", "c/**/d", "#endif"]
          @?= ["", "a  b", "", "cd", ""],
      testCase "comments go in Haskell comments, and macros expand there" $
        cpp ["#define FOO 42", "-- a /* b */ c FOO", "{- FOO -}"]
          @?= ["", "-- a  c 42", "{- 42 -}"],
      testCase "a comment hides a directive" $ do
        cpp ["#if 1", "a /*", "#if 0", "*/ b", "#endif"] @?= ["", "a  b", "", "", ""]
        warns ["#if 1", "a /*", "#if 0", "*/ b", "#endif"] @?= [],
      testCase "a comment on a directive line goes on past the line" $
        cpp ["#if 1 /* c", "#endif */", "y", "#endif"] @?= ["", "", "y", ""],
      testCase "an unterminated comment" $
        warns ["#if 1", "a /* b"] @?= [(1, MissingEndif), (2, UnterminatedComment)],
      testCase "excluded lines are not read: a /* there opens nothing" $
        cpp ["#if 0", "it's /*", "#else", "k", "#endif"] @?= ["", "", "", "k", ""],
      testCase "// is not a comment" $
        warns ["#if 1 // c", "#endif"] @?= [(1, BadExpression "missing operand")],
      testCase "quotes: nothing expands inside, up to the end of the line" $
        cpp
          [ "#define FOO 42",
            "x' = FOO",
            "-- it's FOO",
            "z = 'a' FOO",
            "\"FOO\" FOO",
            "x = \"\\\" FOO",
            "y = '\\'' FOO"
          ]
          @?= [ "",
                "x' = FOO",
                "-- it's FOO",
                "z = 'a' 42",
                "\"FOO\" 42",
                "x = \"\\\" FOO",
                "y = '\\'' 42"
              ],
      testCase "a comment in quotes is not a comment" $
        cpp ["#if 1", "\"a /* b\" c */ d", "#endif"] @?= ["", "\"a /* b\" c */ d", ""],
      testCase "longer names don't expand" $
        cpp ["#define FOO 1", "FOO_ FOO1 _FOO"] @?= ["", "FOO_ FOO1 _FOO"],
      testCase "on purpose, numbers stay whole (gcc would expand 1FOO)" $ do
        cpp ["#define FOO ff", "1FOO 0x1FOO 1e10 .5FOO"]
          @?= ["", "1FOO 0x1FOO 1e10 .5FOO"]
        keeps ["#define x10 7", "#if 0x10 == 16"] @?= True,
      testCase "quotes on directive lines hide comments" $
        cpp ["#define X it's /* c */ 1", "X"] @?= ["", "it's /* c */ 1"]
    ]

macros :: TestTree
macros =
  testGroup
    "macros"
    [ testCase "object-like and function-like" $
        cpp ["#define FOO 42", "#define F(x) [x]", "FOO F(1) F (2) F"]
          @?= ["", "", "42 [1] [2] F"],
      testCase "the paren may be on a later line" $
        cpp ["#define F(x) [x]", "a F", "(1) b", "c F", "d"]
          @?= ["", "a [1] b", "", "c F", "d"],
      testCase "the paren is not looked for past a directive" $
        cpp ["#define F(x) [x]", "c F", "#if 1", "(2)", "#endif"]
          @?= ["", "c F", "", "(2)", ""],
      testCase "arguments over lines: newlines are spaces, lines stay" $
        cpp ["#define F(x,y) [x|y]", "F(a", ",", "b) z", "G"]
          @?= ["", "[a | b] z", "", "", "G"],
      testCase "arguments keep their blanks; the body loses its own" $
        cpp ["#define F(x)  [x]  ", "F(  a  )|"] @?= ["", "[  a  ]|"],
      testCase "parens, quotes and comments in arguments" $
        cpp ["#define F(x) [x]", "F((a,b)) F(\")\") F(/* ) */ 3) F('(')"]
          @?= ["", "[(a,b)] [\")\"] [ 3] ['(']"],
      testCase "parameters are replaced in quotes" $
        cpp ["#define S(x) \"x\" 'x' x", "S(hi)"] @?= ["", "\"hi\" 'hi' hi"],
      testCase "in a \"…\" body, a \" of the argument gets a backslash, as gcc" $
        cpp ["#define K(x) \"x\" 'x'", "K(\"q\") K(a\\b)"]
          @?= ["", "\"\\\"q\\\"\" '\"q\"' \"a\\b\" 'a\\b'"],
      testCase "/**/ pastes" $
        cpp
          [ "#define P(a,b) a/**/b",
            "P(foo,bar)",
            "#define ab 1",
            "#define X a/**/b",
            "X"
          ]
          @?= ["", "foobar", "", "", "1"],
      testCase "arguments go in as written, then the result is read again" $
        cpp
          [ "#define FOO 42",
            "#define S(x) \"x\" x",
            "S(FOO)",
            "#define F(x) [x]",
            "#define G(x) F(x) x",
            "G(F(1))",
            "#define O F",
            "F(O)"
          ]
          @?= ["", "", "\"FOO\" 42", "", "", "[[1]] [1]", "", "[F]"],
      testCase "a function-like macro may call itself through an argument" $
        cpp
          [ "#define F(x) [x]",
            "#define G(x) x",
            "#define K(x) G x",
            "F(G F(1))",
            "K(K(3))"
          ]
          @?= ["", "", "", "[G [1]]", "G G 3"],
      testCase "the expansion is read with the text after it" $
        cpp
          [ "#define F(x) [x]",
            "#define H F",
            "H(2) H (3)",
            "#define Y(x) x(5)",
            "Y(F)",
            "#define T(x) x",
            "T(T)(6)"
          ]
          @?= ["", "", "[2] [3]", "", "[5]", "", "6"],
      testCase "unexpanded arguments: a comma from a macro is not a separator" $ do
        let ls =
              ["#define AB a,b", "#define F2(x,y) [x|y]", "#define W(x) F2(x)", "W(AB)"]
        cpp ls @?= ["", "", "", "F2(a,b)"]
        warns ls @?= [(4, WrongArgCount "F2")],
      testCase "recursion: left as is, with a warning" $ do
        let ls =
              [ "#define X X+1",
                "X",
                "#define A B",
                "#define B A",
                "A B",
                "#define R(x) R(x)",
                "R(1)"
              ]
        cpp ls @?= ["", "X+1", "", "", "A B", "", "R(1)"]
        warns ls
          @?= [ (2, RecursiveMacro "X"),
                -- One of a kind per line: B's is dropped.
                (5, RecursiveMacro "A"),
                (7, RecursiveMacro "R")
              ],
      testCase "wrong argument count: left as is" $ do
        let ls = ["#define F(x,y) [x|y]", "F(1) F(1,2,3) F(1,2)"]
        cpp ls @?= ["", "F(1) F(1,2,3) [1|2]"]
        warns ls @?= [(2, WrongArgCount "F")],
      testCase "no parameters: only E() fits" $ do
        cpp ["#define E() empty", "E() E ( ) E"] @?= ["", "empty E ( ) E"]
        warns ["#define E() empty", "E( )"] @?= [(2, WrongArgCount "E")],
      testCase "a comment ends a #define's name" $
        cpp ["#define A/**/B 5", "A"] @?= ["", "B 5"],
      testCase "an unterminated call is left as is" $ do
        let ls = ["#define F(x) [x]", "#define FOO 1", "F(FOO", "#if 1", ")", "#endif"]
        cpp ls @?= ["", "", "F(1", "", ")", ""]
        warns ls @?= [(3, UnterminatedCall "F")],
      testCase "a failed call over lines warns on the call's line" $ do
        warns ["#define F(x) [x]", "F(a,", "b", "#if 1", "#endif"]
          @?= [(2, UnterminatedCall "F")]
        warns ["#define F(x) [x]", "x = F(a,", "b)"] @?= [(2, WrongArgCount "F")],
      testCase "bad #define" $ do
        warns
          [ "#define",
            "#define 1x",
            "#define F(x",
            "#define F(x,x) x",
            "#define V(a,...) a"
          ]
          @?= [ (1, BadDefine "no macro name"),
                (2, BadDefine "no macro name"),
                (3, BadDefine "bad parameter list"),
                (4, BadDefine "repeated parameter in F"),
                (5, BadDefine "bad parameter list")
              ]
        warns ["#define defined 1"]
          @?= [(1, BadDefine "\"defined\" can't be a macro name")],
      testCase "#define F (x) is object-like" $
        cpp ["#define F (x) x", "F(1)"] @?= ["", "(x) x(1)"],
      testCase "redefine and #undef" $
        cpp ["#define FOO 1", "#define FOO 2", "FOO", "#undef FOO", "FOO"]
          @?= ["", "", "2", "", "FOO"],
      testCase "__LINE__ is the line being read; __FILE__ stays" $
        cpp ["#define L(x) __LINE__ x", "L(", "1", ") __LINE__", "__LINE__ __FILE__"]
          @?= ["", "4  1  4", "", "", "5 __FILE__"],
      testCase "-D and -U, in order: flags, then OPTIONS_GHC" $ do
        let fs = [Define "FOO" "1", Define "BAR" "2", Undefine "FOO", Define "FOO" "3"]
            e = env {flags = fs}
        (runCpp e [] "a = FOO BAR\n").text @?= "a = 3 2\n"
        (runCpp e [Undefine "BAR"] "a = FOO BAR\n").text @?= "a = 3 BAR\n"
        (runCpp env [Define "F(x)" "[x]"] "F(1)").text @?= "[1]",
      testCase "-D and -U don't change GHC's macros: GHC passes its own last" $ do
        let deps = env {buildDepends = [PackageName "base"]}
            run1 fs t = (runCpp deps fs t).text
        run1 [Undefine "__GLASGOW_HASKELL__"] "__GLASGOW_HASKELL__" @?= "914"
        run1 [Define "__GLASGOW_HASKELL__" "1"] "__GLASGOW_HASKELL__" @?= "914"
        run1 [Define "linux_HOST_OS" "0"] "#if linux_HOST_OS\nyes\n#endif" @?= "\nyes\n"
        run1 [Undefine "__IO_MANAGER_MIO__"] "__IO_MANAGER_MIO__" @?= "1"
        run1
          [Undefine "MIN_VERSION_GLASGOW_HASKELL"]
          "#if MIN_VERSION_GLASGOW_HASKELL(9,0,0,0)\nyes\n#endif"
          @?= "\nyes\n"
        run1 [Define "MIN_VERSION_base(a,b,c)" "0"] "MIN_VERSION_base(1,2,3)" @?= "1"
        -- In the file, #undef still works.
        run1 [] "#undef __GLASGOW_HASKELL__\n__GLASGOW_HASKELL__"
          @?= "\n__GLASGOW_HASKELL__"
        -- A package not in build-depends: the flag stands.
        run1 [Define "MIN_VERSION_other(a,b,c)" "0"] "MIN_VERSION_other(1,2,3)" @?= "0",
      testCase "macroFlags: cpp-options take -D X" $ do
        macroFlags CppOptions ["-DX", "-DY=2", "-D", "Z=", "-UW", "-U", "V", "-optP-DA"]
          @?= MacroFlags
            { found =
                [ Define "X" "1",
                  Define "Y" "2",
                  Define "Z" "",
                  Undefine "W",
                  Undefine "V"
                ],
              rest = ["-optP-DA"]
            }
        macroFlags CppOptions ["-Wall", "-D1X", "-U", "-DF(x)=x", "-I.", "-U-x"]
          @?= MacroFlags
            { found = [Define "F(x)" "x"],
              rest = ["-Wall", "-D1X", "-U", "-I.", "-U-x"]
            },
      testCase "macroFlags: ghc-options take -DX and -optP-DX only, as GHC" $
        macroFlags GhcOptions ["-DX", "-D", "Y", "-optP-DA", "-optP-UB", "-UC", "-U"]
          @?= MacroFlags
            { found = [Define "X" "1", Define "A" "1", Undefine "B", Undefine "C"],
              rest = ["-D", "Y", "-U"]
            }
    ]

directives :: TestTree
directives =
  testGroup
    "directives"
    [ testCase "unknown directives: text, with macros expanded" $ do
        let ls = ["#define FOO 42", "#bogus FOO", "#!FOO", "#if 0", "#bogus", "#endif"]
        cpp ls @?= ["", "#bogus 42", "#!42", "", "", ""]
        warns ls @?= [],
      testCase "#pragma, #ident and # give a blank line" $
        cpp ["#pragma foo", "#ident \"x\"", "#", "x"] @?= ["", "", "", "x"],
      testCase "#line is dropped, with a warning; a # and digits is text" $ do
        let ls = ["#line 40 \"f\"", "# 50 \"g\"", "#40", "b"]
        cpp ls @?= ["", "# 50 \"g\"", "#40", "b"]
        warns ls @?= [(1, LineDirectiveIgnored)],
      testCase "#undef MIN_VERSION_x: a plain name after it" $ do
        let ls = ["#undef MIN_VERSION_foo", "#undef MIN_VERSION_base", "#if 1"]
        let code = "MIN_VERSION_foo(1,2,3) MIN_VERSION_base(1,2,3) MIN_VERSION_bar(1,2,3)"
        cpp (ls <> [code])
          @?= ["", "", "", "MIN_VERSION_foo(1,2,3) MIN_VERSION_base(1,2,3) 1"]
        keeps
          [ "#define MIN_VERSION_foo(a,b,c) 0",
            "#undef MIN_VERSION_foo",
            "#if !defined(MIN_VERSION_foo)"
          ]
          @?= True,
      testCase "an indented directive is text, with a warning" $ do
        let ls = ["#define BAR 1", "  #define FOO 2", "  # if BAR", "FOO"]
        cpp ls @?= ["", "  #define FOO 2", "  # if 1", "FOO"]
        warns ls @?= [(2, IndentedDirective), (3, IndentedDirective)],
      testCase "#include is empty, with a warning" $ do
        let ls = ["#include \"a.h\"", "#include_next <b.h>", "#import \"c.h\"", "x"]
        cpp ls @?= ["", "", "", "x"]
        warns ls
          @?= [ (1, IncludeIgnored "\"a.h\""),
                (2, IncludeIgnored "<b.h>"),
                (3, IncludeIgnored "\"c.h\"")
              ],
      testCase "#error and #warning are notes; excluded ones are not seen" $ do
        let ls =
              ["#error boom  ", "#warning careful", "#if 0", "#error no", "#endif", "x"]
        cpp ls @?= ["", "", "", "", "", "x"]
        warns ls @?= [(1, ErrorDirective "boom"), (2, WarningDirective "careful")],
      testCase "#assert is not supported" $
        warns ["#assert machine(x86)"] @?= [(1, UnsupportedDirective "assert")],
      testCase "unbalanced" $ do
        warns ["#else", "#elif 1", "#endif"]
          @?= [ (1, UnbalancedElse "else"),
                (2, UnbalancedElse "elif"),
                (3, UnbalancedEndif)
              ]
        warns ["x", "#if 1", "#if 0"] @?= [(2, MissingEndif), (3, MissingEndif)]
        warns ["#if 1", "#else", "#else", "#elif 1", "#endif"]
          @?= [(3, ElseAfterElse "else"), (4, ElseAfterElse "elif")]
        cpp ["#if 1", "a", "#else", "b", "#else", "c", "#endif"]
          @?= ["", "a", "", "", "", "", ""]
    ]

conditions :: TestTree
conditions =
  testGroup
    "conditions"
    [ testCase "#if, #elif, #else" $ do
        cpp ["#if 0", "a", "#elif 1", "b", "#elif 1", "c", "#else", "d", "#endif"]
          @?= ["", "", "", "b", "", "", "", "", ""]
        cpp ["#if 0", "a", "#elif 0", "b", "#else", "d", "#endif"]
          @?= ["", "", "", "", "", "d", ""],
      testCase "#ifdef, #ifndef, #elifdef, #elifndef" $ do
        keeps ["#define X", "#ifdef X"] @?= True
        keeps ["#ifdef X"] @?= False
        keeps ["#ifndef X"] @?= True
        keeps ["#define X", "#ifdef X junk"] @?= True
        keeps ["#if 0", "#elifdef X", "#elifndef X"] @?= True
        keeps ["#define X", "#if 0", "#elifdef X"] @?= True
        warns ["#ifdef", "#endif"] @?= [(1, BadExpression "no macro name")],
      testCase "an #elif after a taken branch is not evaluated" $
        warns ["#if 1", "#elif 1/0", "#endif"] @?= [],
      testCase "undefined names are 0" $ do
        keeps ["#if nope == 0 && !nope"] @?= True
        keeps ["#if defined(nope) || nope"] @?= False,
      testCase "defined, also made by a macro" $ do
        keeps ["#define X", "#if defined X && defined(X) && defined ( X )"] @?= True
        keeps ["#define X", "#define D defined(X)", "#if D"] @?= True
        warns ["#if defined", "#endif"]
          @?= [(1, BadExpression "defined without a macro name")],
      testCase "MIN_VERSION_pkg: 1, but defined only for build-depends" $ do
        keeps ["#if MIN_VERSION_foo(1,2,3)"] @?= True
        keeps ["#if MIN_VERSION_base(99,0,0) && MIN_VERSION_foo(1,2,3)"] @?= True
        keeps ["#if defined(MIN_VERSION_base)"] @?= True
        keeps ["#ifdef MIN_VERSION_text_short"] @?= True
        keeps ["#if defined(MIN_VERSION_foo)"] @?= False
        keeps ["#ifndef MIN_VERSION_foo"] @?= True
        cpp ["#if 1", "x = MIN_VERSION_foo(1,2,3) MIN_VERSION_base", "#endif"]
          @?= ["", "x = 1 MIN_VERSION_base", ""],
      testCase "a user's MIN_VERSION_x wins" $
        keeps ["#define MIN_VERSION_foo(a,b,c) 0", "#if !MIN_VERSION_foo(1,2,3)"]
          @?= True,
      testCase "C arithmetic" $ do
        keeps ["#if 1 ? 2 : 3"] @?= True
        keeps ["#if (2 || 1/0) && !(0 && 1/0)"] @?= True
        keeps ["#if 'a' == 97 && '\\n' == 10 && '\\x41' == 65 && '\\101' == 65"] @?= True
        keeps ["#if 0x10 == 16 && 010 == 8 && 0b11 == 3 && 10u == 10 && 7L == 7"] @?= True
        keeps ["#if 7 / 2 == 3 && -7 / 2 == -3 && -7 % 2 == -1"] @?= True
        keeps ["#if (1, 0)"] @?= False
        keeps ["#if 1 < < 2"] @?= False
        keeps ["#if 3 - 2 - 1 == 0 && 2 * 3 + 1 == 7 && (1 | 2) == 3 && (1 ^ 3) == 2"]
          @?= True
        keeps ["#if ~0 == -1 && -(-1) == 1 && +1 == 1"] @?= True,
      testCase "64-bit edges" $ do
        keeps ["#if 9223372036854775807 + 1 < 0"] @?= True
        keeps ["#if -1 > 0u"] @?= True
        keeps ["#if 18446744073709551615 == -1"] @?= True
        keeps ["#if 9223372036854775808 > 0"] @?= True
        keeps ["#if (-9223372036854775807 - 1) / -1 < 0"] @?= True
        keeps ["#if (-9223372036854775807 - 1) % -1 == 0"] @?= True
        keeps ["#if (1 << 63) < 0 && (1u << 63) > 0"] @?= True
        keeps ["#if -1 >> 70"] @?= True
        keeps ["#if 1 << -1"] @?= False
        keeps ["#if 18446744073709551616"] @?= False
        warns ["#if 18446744073709551616", "#endif"]
          @?= [(1, BadExpression "number too large 18446744073709551616")],
      testCase "division by zero and big shifts: 0, with a warning" $ do
        warns ["#if 1/0", "#endif", "#if 1 % 0u", "#endif"]
          @?= [(1, DivByZero), (3, DivByZero)]
        warns ["#if 1 << 64", "#endif"] @?= [(1, BigShift)]
        keeps ["#if (1 << 64) == 0"] @?= True,
      testCase "bad expressions are false" $ do
        warns ["#if", "#endif"] @?= [(1, BadExpression "no expression")]
        warns ["#if (1", "#endif"] @?= [(1, BadExpression "missing )")]
        warns ["#if 1 2", "#endif"] @?= [(1, BadExpression "missing operator")]
        warns ["#if \"s\"", "#endif"] @?= [(1, BadExpression "string in #if: \"s\"")]
        warns ["#if 1.5", "#endif"] @?= [(1, BadExpression "bad number 1.5")]
        warns ["#if 1 ? 2", "#endif"] @?= [(1, BadExpression "missing : in ?:")]
    ]

ghcMacros :: TestTree
ghcMacros =
  testGroup
    "GHC's macros"
    [ testCase "for 9.14.1 on x86_64 Linux" $
        cpp
          [ "#if 1",
            "__GLASGOW_HASKELL__ __GLASGOW_HASKELL_FULL_VERSION__",
            "__GLASGOW_HASKELL_PATCHLEVEL1__ __GLASGOW_HASKELL_PATCHLEVEL2__",
            "[__GHCVERSION_H__] __GLASGOW_HASKELL_TH__ __IO_MANAGER_MIO__",
            "linux_HOST_OS linux_BUILD_OS x86_64_HOST_ARCH x86_64_BUILD_ARCH",
            "__SSE__ __SSE2__ __IO_MANAGER_WINIO__ __ASSEMBLER__"
          ]
          @?= [ "",
                "914 \"9.14.1\"",
                "1 0",
                "[] 1 1",
                "1 1 1 1",
                "1 1 __IO_MANAGER_WINIO__ __ASSEMBLER__"
              ],
      testCase "--ghc 9.12: the missing numbers are 0" $ do
        let e = env {ghc = GhcVersion [9, 12]}
        (runCpp e [] "#\n__GLASGOW_HASKELL__ __GLASGOW_HASKELL_FULL_VERSION__").text
          @?= "\n912 \"9.12.0\""
        let levels = "#\n__GLASGOW_HASKELL_PATCHLEVEL1__ __GLASGOW_HASKELL_PATCHLEVEL2__"
        (runCpp e [] levels).text
          @?= "\n0 0"
        (runCpp e [] "#if MIN_VERSION_GLASGOW_HASKELL(9,12,0,0)\ny\n#endif").text
          @?= "\ny\n"
        (runCpp e [] "#if MIN_VERSION_GLASGOW_HASKELL(9,12,1,0)\ny\n#endif").text
          @?= "\n\n",
      testCase "MIN_VERSION_GLASGOW_HASKELL, four numbers" $ do
        keeps ["#if MIN_VERSION_GLASGOW_HASKELL(9,14,1,0)"] @?= True
        keeps ["#if MIN_VERSION_GLASGOW_HASKELL(9,14,2,0)"] @?= False
        keeps ["#if MIN_VERSION_GLASGOW_HASKELL(9,10,0,0)"] @?= True
        keeps ["#if MIN_VERSION_GLASGOW_HASKELL(9,16,0,0)"] @?= False
        keeps ["#if MIN_VERSION_GLASGOW_HASKELL(8,0,0,0) && __GLASGOW_HASKELL__ >= 914"]
          @?= True
        keeps ["#if defined(MIN_VERSION_GLASGOW_HASKELL)"] @?= True,
      testCase "another platform" $ do
        let e = env {platform = Platform {os = "darwin", arch = "aarch64"}}
        (runCpp e [] "#\ndarwin_HOST_OS aarch64_HOST_ARCH linux_HOST_OS __SSE__").text
          @?= "\n1 1 linux_HOST_OS __SSE__"
        let windows = env {platform = Platform "mingw32" "x86_64"}
        (runCpp windows [] "#\n__IO_MANAGER_WINIO__").text
          @?= "\n1"
    ]

positions :: TestTree
positions =
  testGroup
    "positions"
    [ testCase "an expanded macro maps to the whole call" $ do
        let o = run ["#define F(x) [x]", "ab F(1) cd"]
        outLines o @?= ["", "ab [1] cd"]
        -- "[1]" is at 2:4-2:7; the call F(1) at 2:4-2:8.
        mapSpan o.positions (spanOf 2 4 2 7) @?= spanOf 2 4 2 8
        mapSpan o.positions (spanOf 2 5 2 6) @?= spanOf 2 4 2 8
        -- "cd" moved one column left.
        mapSpan o.positions (spanOf 2 8 2 10) @?= spanOf 2 9 2 11
        mapSpan o.positions (spanOf 2 1 2 3) @?= spanOf 2 1 2 3,
      testCase "a multi-line call maps to its last line" $ do
        let o = run ["#define F(x,y) [x|y]", "F(a", ",", "b) z"]
        mapSpan o.positions (spanOf 2 1 2 8) @?= spanOf 2 1 4 3
        mapSpan o.positions (spanOf 2 9 2 10) @?= spanOf 4 4 4 5,
      testCase "joined lines map back to theirs" $ do
        let o = run ["#if 1", "foo \\", "  bar", "#endif"]
        outLines o @?= ["", "foo   bar", "", ""]
        mapSpan o.positions (spanOf 2 7 2 10) @?= spanOf 3 3 3 6
        mapSpan o.positions (spanOf 2 1 2 10) @?= spanOf 2 1 3 6,
      testCase "a removed comment" $ do
        let o = run ["#if 1", "a /* x */ b", "#endif"]
        outLines o @?= ["", "a  b", ""]
        mapSpan o.positions (spanOf 2 4 2 5) @?= spanOf 2 11 2 12
        o.positions
          @?= PositionMap
            ( IntMap.fromList
                [(2, IntMap.fromList [(1, Copied (Pos 2 1)), (3, Copied (Pos 2 10))])]
            ),
      testCase "unchanged lines have no entry" $ do
        let PositionMap m = (run ["#define FOO 1", "x", "FOO", "y"]).positions
        IntMap.keys m @?= [3],
      testCase "mapSpan sameLines is the identity" $
        mapSpan sameLines (spanOf 3 4 5 6) @?= spanOf 3 4 5 6
    ]

-- | Hostile input: each case must end quickly with a bounded output.
limits :: TestTree
limits =
  localOption (mkTimeout 20_000_000) $
    testGroup
      "limits"
      [ testCase "macros that double 50 times" $ do
          let a k = " A" <> n k
              ls =
                "#define A0 x"
                  : ["#define" <> a k <> a (k - 1) <> a (k - 1) | k <- [1 .. 50]]
                    <> ["A50"]
          o <- bounded ls
          assertBool "warns" (hasLimit ExpansionWork o),
        testCase "many recursive macros" $ do
          let m k = "M" <> n (k `mod` 50)
              ls =
                ["#define " <> m k <> "(x) " <> m (k + 1) <> "(x x)" | k <- [0 .. 49]]
                  <> ["M0(a)"]
          _ <- bounded ls
          pure (),
        testCase "F(F(...)) 40 deep" $ do
          let ls =
                [ "#define F(x) [x]",
                  Text.replicate 40 "F(" <> "1" <> Text.replicate 40 ")"
                ]
          o <- bounded ls
          assertBool "warns" (any (\w -> w.problem == RecursiveMacro "F") o.warnings),
        testCase "64 macros in a chain" $ do
          let ls = ["#define M" <> n k <> " M" <> n (k + 1) | k <- [0 .. 99]] <> ["M0"]
          o <- bounded ls
          assertBool "warns" (hasLimit ExpansionDepth o),
        testCase "10,000 nested #if" $ do
          let ls = replicate 10000 "#if 1" <> ["x"] <> replicate 10000 "#endif"
          o <- bounded ls
          assertBool "warns" (hasLimit IfDepth o)
          assertBool "no missing endif" (all ((/= MissingEndif) . (.problem)) o.warnings),
        testCase "100,000 parens" $ do
          let deep = Text.replicate 100000 "(" <> "1" <> Text.replicate 100000 ")"
          o <- bounded ["#if " <> deep, "#endif"]
          assertBool "warns" (hasLimit ExpressionDepth o),
        testCase "100,000 unary operators" $ do
          o <- bounded ["#if " <> Text.replicate 100000 "!" <> "1", "#endif"]
          assertBool "warns" (hasLimit ExpressionDepth o),
        testCase "a long #if chain" $ do
          o <- bounded ["#if 1" <> Text.replicate 100000 " + 1", "x", "#endif"]
          outLines o @?= ["", "x", ""],
        testCase "1 << 99999 and a huge number" $ do
          let huge = "1" <> Text.replicate 1000000 "0"
          o <- bounded ["#if 1 << 99999", "#endif", "#if " <> huge, "#endif"]
          (map (.problem) o.warnings)
            @?= [BigShift, BadExpression (cut ("number too large " <> huge))],
        testCase "an unterminated F( over 1 MiB" $ do
          o <- bounded ["#define F(x) [x]", "F(" <> Text.replicate 1048576 "a "]
          (map (.problem) o.warnings) @?= [UnterminatedCall "F"],
        testCase "100,000 unterminated F(" $ do
          o <- bounded ["#define F(x) [x]", Text.replicate 100000 "F("]
          assertBool "warns" (hasLimit ExpansionWork o),
        testCase "300 arguments" $ do
          let many = Text.intercalate "," (replicate 300 "a")
          o <- bounded ["#define F(x) [x]", "F(" <> many <> ")"]
          assertBool "warns" (hasLimit ArgCount o),
        testCase "10,001 #define" $ do
          let defs = ["#define M" <> n k <> " 1" | k <- [0 .. 10000]]
          o <- bounded (defs <> ["M9999 M10000"])
          assertBool "warns" (hasLimit DefineCount o)
          last (outLines o) @?= "1 M10000",
        testCase "a body that uses its parameter 5000 times, with a 1 MB argument" $ do
          let body = Text.unwords (replicate 5000 "x")
          let arg = Text.replicate 1000000 "a"
          o <- bounded ["#define F(x) " <> body, "F(" <> arg <> ")"]
          assertBool "warns" (hasLimit ExpansionWork o),
        testCase "5000 unterminated F( with long strings" $ do
          let ls = replicate 5000 ("F( \"" <> Text.replicate 1000 "b")
          o <- bounded ("#define F(x) x" : ls)
          assertBool "few warnings" (length o.warnings <= 10),
        testCase "a failed call doesn't expand its put-back body again" $ do
          let ls = ["#define P(x) x", "#define Q P(Q", "d = Q"]
          o <- bounded ls
          outLines o @?= ["", "", "d = P(Q"]
          (map (.problem) o.warnings) @?= [UnterminatedCall "P", RecursiveMacro "Q"],
        testCase "two macros that call each other through a failed call" $ do
          o <- bounded ["#define P(x) x", "#define Q P(R", "#define R P(Q", "d = Q"]
          assertBool "small" (Text.length o.text < 100),
        testCase "warnings: at most 1000, then one note" $ do
          o <- bounded (replicate 3000 "#error x")
          length o.warnings @?= 1001
          (last o.warnings).problem @?= WarningsDropped,
        testCase "unclosed #if count toward the warning cap" $ do
          o <- bounded (replicate 5000 "#if 1")
          length o.warnings @?= 1001,
        testCase "warning texts are cut to 200 chars" $ do
          o <- bounded ["#error " <> Text.replicate 1000 "e"]
          (map (.problem) o.warnings)
            @?= [ErrorDirective (cut (Text.replicate 1000 "e"))],
        testCase "3500 nested unterminated F( then 1 MB of comments" $ do
          o <- quick ("#define F(x) x" : Text.replicate 3500 "F(" : comments)
          assertBool "warns" (not (null o.warnings)),
        testCase "a chain of 21 macros into 1000 failed F(" $ do
          let chain = ["#define M" <> n k <> " M" <> n (k + 1) | k <- [0 .. 19]]
              body = Text.replicate 1000 "F(" <> "a" <> Text.replicate 1000 ",b)"
          let defs = ["#define F(x) x"] <> chain <> ["#define M20 " <> body]
          o <- quick (defs <> comments <> ["M0"])
          assertBool "warns" (not (null o.warnings)),
        testCase "an unterminated call over 1 MiB, in many lines" $ do
          o <- quick ("#define P(x) x" : "P(a" : replicate 500000 "a")
          (map (.problem) o.warnings) @?= [UnterminatedCall "P"],
        testCase "Q expands to a failed call, 30000 times" $ do
          _ <- quick (["#define P(x) x", "#define Q P(Q"] <> replicate 30000 "d = Q")
          pure (),
        testCase "9000 macros, each a failed call into the next" $ do
          let defs = ["#define m" <> n k <> " F(m" <> n (k + 1) | k <- [0 .. 8999]]
          _ <- quick (["#define F(x) x"] <> defs <> ["m0"])
          pure (),
        testCase "a chain of 60 F-then-macro bodies" $ do
          let defs = ["#define M" <> n k <> " F M" <> n (k + 1) | k <- [0 .. 59]]
          _ <- quick (["#define F(x) x"] <> defs <> replicate 2000 "M0")
          pure (),
        testCase "F, then a million empty lines" $ do
          o <- quick ("#define F(x) x" : "F" : replicate 1000000 "")
          (outLines o !! 1) @?= "F",
        testCase "two limits on one line both warn" $ do
          let chain = ["#define M" <> n k <> " M" <> n (k + 1) | k <- [0 .. 99]]
              many = Text.intercalate "," (replicate 300 "a")
          o <- quick (["#define F(x) x"] <> chain <> ["M0 F(" <> many <> ")"])
          assertBool "depth" (hasLimit ExpansionDepth o)
          assertBool "args" (hasLimit ArgCount o),
        testCase "a 1 MiB line of names" $ do
          o <- bounded ["#define FOO 1", Text.replicate 100000 "FOO bar "]
          Text.length o.text @?= 600001
      ]
  where
    n :: Int -> Text
    n = Text.pack . show
    cut t = Text.take 200 t <> "..."
    comments = replicate 12000 ("-- " <> Text.replicate 76 "p")
    hasLimit l o = any (\w -> w.problem == LimitHit l) o.warnings

-- | 'bounded', in under 5 s.
quick :: [Text] -> IO CppOutput
quick ls = timeout 5_000_000 (bounded ls) >>= maybe (assertFailure "too slow") pure

-- | Run, and check the output is within the size limit (Note [CPP
-- limits]) and keeps the line count.
bounded :: [Text] -> IO CppOutput
bounded ls = do
  let input = Text.intercalate "\n" ls
      o = runCpp env [] input
  _ <- evaluate (forceOutput o)
  assertBool "output too big" (Text.length o.text <= 4 * Text.length input + 1048576)
  length (outLines o) @?= length ls
  pure o
