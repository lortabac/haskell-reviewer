-- | Running the parser from IO: GHC's own crashes become text only, a
-- limit of one parse at a time, and a warm-up for its shared values.
module HaskellReviewer.Effects.Parser
  ( parseGuarded,
    oneAtATime,
    warmUpParser,
    cppWarmUp,
    cppLimitsWarmUp,
  )
where

import Control.Concurrent.QSem (newQSem, signalQSem, waitQSem)
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    bracket_,
    displayException,
    evaluate,
    fromException,
    throwIO,
    try,
  )
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Cpp
  ( CppEnv (..),
    CppOutput (..),
    CppProblem (..),
    CppWarning (..),
    Limit (..),
    MacroFlag (..),
    renderCppWarning,
    runCpp,
  )
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
  )
import HaskellReviewer.Core.Parser
  ( FileKind (..),
    ParseSettings (..),
    defaultParseSettings,
    parseSource,
  )
import HaskellReviewer.Core.Project
  ( Extension (..),
    PackageName (..),
    hostPlatform,
    parserGhcVersion,
  )

-- | 'parseSource', fully forced. If GHC throws (a bug, in GHC or ours),
-- the module is text only. Async exceptions go through, and so do
-- StackOverflow and HeapOverflow: they are async exceptions too.
parseGuarded :: ParseSettings -> Text -> IO ModuleParse
parseGuarded settings text = do
  r <- try (evaluate (parseSource settings text))
  case r of
    Right p -> pure p
    Left e -> do
      rethrowAsync e
      -- Showing the exception can throw too, and a lazy text would keep
      -- it alive: build the text here, in its own try.
      shown <- try (evaluate (Text.pack (displayException e)))
      message <- case shown of
        Right t -> pure t
        Left e2 -> "parser crashed" <$ rethrowAsync e2
      pure (TextOnly TextOnlyModule {reason = ParserCrashed message, warnings = []})

{- Note [One parse at a time]

A parse can take a lot of memory: a hostile 1 MiB module peaked at
0.7-1.2 GB of RSS, and 8 such requests at once at 8.3 GB. The budget is
2 GiB for a large project (SPEC §10.2). So requests parse one at a time
('oneAtATime'). A parse of a normal module takes a few ms, so waiting
costs little.

The warm-up doesn't go through the limit: it runs before any request
can parse, so it can't wait on one.
-}

-- | Wrap a parse function so only one call runs at a time. See
-- Note [One parse at a time].
oneAtATime :: (a -> b -> IO c) -> IO (a -> b -> IO c)
oneAtATime f = do
  sem <- newQSem 1
  pure (\a b -> bracket_ (waitQSem sem) (signalQSem sem) (f a b))

-- | Throw it again if it's async.
rethrowAsync :: SomeException -> IO ()
rethrowAsync e = case fromException e of
  Just (_ :: SomeAsyncException) -> throwIO e
  Nothing -> pure ()

-- | Parse a few tiny modules once, so the shared top-level values (ours
-- and ghc-lib-parser's) are built before parallel threads use them
-- (Q-RTS-BLACKHOLE). Each one takes a different path: pragmas and
-- extension names, Haddock, tabs, a parse error, a pragma error, a
-- signature.
warmUpParser :: IO ()
warmUpParser = do
  mapM_
    (parseGuarded defaultParseSettings {extensions = [Extension "LambdaCase" True]})
    [ "{-# LANGUAGE CPP, Safe #-}\n{-# OPTIONS_GHC -Wall -XMultiWayIf #-}\n"
        <> "-- | Doc.\nmodule M (x, T (..)) where\nimport A (b)\nx =\t1\n",
      "module M where\nx = = 1\n",
      "{-# LANGUAGE NoSuchThing #-}\nmodule M where\n"
    ]
  _ <-
    parseGuarded
      defaultParseSettings {kind = SignatureFile}
      "signature S (T) where\ndata T\n"
  -- The CPP evaluator, on every kind of line (Q-RTS-BLACKHOLE).
  _ <- evaluate (forceCpp cppWarmUp)
  _ <- evaluate (forceCpp cppLimitsWarmUp)
  _ <- evaluate (sum (map (Text.length . renderCppWarning . CppWarning 1) allProblems))
  pure ()

-- | CPP on a module where every line does something: every operator
-- and kind of number in #if, macros of each kind used in code, a
-- failed call, warnings. Pure; a test checks it does all that.
cppWarmUp :: CppOutput
cppWarmUp =
  runCpp
    CppEnv
      { ghc = parserGhcVersion,
        platform = hostPlatform,
        flags = [Define "D" "1", Undefine "U"],
        buildDepends = [PackageName "text-short"]
      }
    [Define "P" "2"]
    ( Text.unlines
        [ "#define F(x,y) [x|y] \"x\" 'y'",
          "#define O 1",
          "#define R R",
          "#if ((1 + 2 * 3 - 4 / 2 % 3) << 1 >> 1) == 6 && 1 != 2 || 0 < 1 && 1 > 0",
          "#elif 1 <= 1 && 1 >= 1 && (5 & 3 | 4 ^ 1) && ~0 && !0 && -1 && +1",
          "#endif",
          "#if (1 ? 2 : 3) && 'a' == 97 && '\\n' && 0x1F && 0b1 && 010 && 10u && 7L \\",
          "  && defined(F) && defined O && MIN_VERSION_text_short(0,1,0) && D && P \\",
          "  && MIN_VERSION_GLASGOW_HASKELL(9,0,0,0) && __GLASGOW_HASKELL__ > 900",
          "a = F(1, O) O R __LINE__ MIN_VERSION_base(1,2,3)",
          "a2 = __GLASGOW_HASKELL__ /* c */ x",
          "b = F(p,",
          "  q) F(1) \\",
          "  c",
          "#else",
          "#endif",
          "#if 1/0 || 1 << 64",
          "#endif",
          "#if 1 2",
          "#endif",
          "#ifdef",
          "#endif",
          "#error e",
          "#warning w",
          "#include \"x.h\"",
          "#line 1",
          "#assert a",
          "  #if 1",
          "#bogus O",
          "#define",
          "#undef",
          "#undef MIN_VERSION_text_short",
          "#else",
          "#endif",
          "#elif 1",
          "#if 0",
          "#else",
          "#else",
          "#endif",
          "#pragma p",
          "#if 1",
          "c = F(unterminated",
          "/* open"
        ]
    )

-- | CPP hitting every limit, cheaply: the limit paths' shared values
-- get built too.
cppLimitsWarmUp :: CppOutput
cppLimitsWarmUp =
  runCpp defaultParseSettings.cpp [] $
    Text.unlines $
      ["#define M" <> num k <> " M" <> num (k + 1) | k <- [0 .. 64 :: Int]]
        <> [ "M0",
             "#define F(x) x",
             "F(" <> Text.intercalate "," (replicate 300 "a") <> ")",
             -- Its size is worked out first: over the budget, never built.
             -- Last, as it spends the budget.
             "#define W(x) " <> Text.replicate 3000 "x ",
             "W(" <> Text.replicate 1000 "a" <> ")",
             "#if " <> Text.replicate 300 "(" <> "1" <> Text.replicate 300 ")",
             "#endif"
           ]
        <> replicate 1030 "#if 1"
        <> replicate 1030 "#endif"
        <> ["#define D" <> num k | k <- [0 .. 10000 :: Int]]
        <> replicate 1000 "#error e"
  where
    num = Text.pack . show

-- | One of each, to render.
allProblems :: [CppProblem]
allProblems =
  [ ErrorDirective "e",
    WarningDirective "w",
    IncludeIgnored "i",
    UnsupportedDirective "u",
    IndentedDirective,
    LineDirectiveIgnored,
    BadExpression "b",
    DivByZero,
    BigShift,
    UnbalancedElse "else",
    ElseAfterElse "else",
    UnbalancedEndif,
    MissingEndif,
    BadDefine "d",
    RecursiveMacro "r",
    WrongArgCount "w",
    UnterminatedCall "c",
    UnterminatedComment,
    WarningsDropped
  ]
    <> map LimitHit [ExpansionDepth ..]

-- | Force all of the output.
forceCpp :: CppOutput -> Int
forceCpp o =
  Text.length o.text
    + sum (map (Text.length . renderCppWarning) o.warnings)
    + length o.regions
    + length (show o.positions)
