-- | Running the parser from IO: a time limit, crash catching, one
-- parse at a time, and a warm-up.
module HaskellReviewer.Effects.Parser
  ( -- * Parsing
    parseGuarded,
    parseTimeLimit,
    oneAtATime,

    -- * Warm-up
    warmUpParser,

    -- * For tests
    parseWithin,
    cppParseWarmUp,
    cppSettings,
    declsWarmUp,
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
    forceModuleParse,
  )
import HaskellReviewer.Core.Parser
  ( FileKind (..),
    ParseSettings (..),
    defaultParseSettings,
    earlyWarnings,
    parseSource,
  )
import HaskellReviewer.Core.Project
  ( Extension (..),
    PackageName (..),
    hostPlatform,
    parserGhcVersion,
  )
import HaskellReviewer.Core.Tokens (noTokens)
import System.Timeout (timeout)

-- | 'parseSource', fully forced, within 'parseTimeLimit'. If GHC
-- throws (a bug, in GHC or ours), the module is text only. Async
-- exceptions go through, and so do StackOverflow and HeapOverflow:
-- they are async exceptions too.
parseGuarded :: ParseSettings -> Text -> IO ModuleParse
parseGuarded = parseWithin parseTimeLimit

{- Note [Parse time limit]

A parse (CPP and GHC) that takes over 2 s gives up: the module is text
only, "too slow". A normal module takes a few ms, so this is only a
safety net for hostile input (SPEC §9.1).

System.Timeout throws an async exception to the thread that parses.
Pure code only gets it at a heap check, so code that never allocates
can't be stopped. CPP and GHC's parser allocate all the time; a test
checks that a slow parse stops soon after the limit.

The result keeps the warnings known before the parse (unlit's and the
component's), as for a crash.

A stopped parse leaves half-done thunks. That's fine: nothing else
holds them. The inputs were forced before (D-125), and the shared
values are built by 'warmUpParser', which has no limit
(Q-RTS-BLACKHOLE).
-}

-- | 2 s, in microseconds. See Note [Parse time limit].
parseTimeLimit :: Int
parseTimeLimit = 2_000_000

-- | 'parseGuarded' with this time limit, in microseconds.
parseWithin :: Int -> ParseSettings -> Text -> IO ModuleParse
parseWithin limit settings text = do
  r <- timeout limit (tryParse (parseSource settings text))
  case r of
    Just (Right p) -> pure p
    Just (Left message) -> gaveUp (ParserCrashed message)
    Nothing -> gaveUp TooSlow
  where
    textOnly reason ws = TextOnly TextOnlyModule {reason = reason, warnings = ws}
    -- Cheap, but they could crash too.
    gaveUp reason =
      either (const (textOnly reason [])) id
        <$> tryParse (textOnly reason (earlyWarnings settings text))

-- | Force a result, or GHC's crash message. Async exceptions, like the
-- timeout's, go through.
tryParse :: ModuleParse -> IO (Either Text ModuleParse)
tryParse parse = do
  r <- try (evaluate (forceModuleParse parse `seq` parse))
  case r of
    Right p -> pure (Right p)
    Left e -> do
      rethrowAsync e
      -- Showing the exception can throw too, and a lazy text would keep
      -- it alive: build the text here, in its own try.
      shown <- try (evaluate (Text.pack (displayException e)))
      case shown of
        Right t -> pure (Left t)
        Left e2 -> Left "parser crashed" <$ rethrowAsync e2

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
-- signature, every kind of declaration, and CPP modules end to end.
warmUpParser :: IO ()
warmUpParser = do
  mapM_
    (parse defaultParseSettings {extensions = [Extension "LambdaCase" True]})
    [ "{-# LANGUAGE CPP, Safe #-}\n{-# OPTIONS_GHC -Wall -XMultiWayIf #-}\n"
        <> "-- | Doc.\nmodule M (x, T (..)) where\nimport A (b)\nx =\t1\n",
      "module M where\nx = = 1\n",
      "{-# LANGUAGE NoSuchThing #-}\nmodule M where\n"
    ]
  _ <- parse defaultParseSettings {kind = SignatureFile} "signature S (T) where\ndata T\n"
  _ <- parse defaultParseSettings declsWarmUp
  _ <- evaluate noTokens
  mapM_ (parse cppSettings) cppParseWarmUp
  -- The CPP evaluator, on every kind of line (Q-RTS-BLACKHOLE).
  _ <- evaluate (forceCpp cppWarmUp)
  _ <- evaluate (forceCpp cppLimitsWarmUp)
  _ <- evaluate (sum (map (Text.length . renderCppWarning . CppWarning 1) allProblems))
  pure ()
  where
    -- No time limit (Note [Parse time limit]).
    parse s t = tryParse (parseSource s t)

-- | A module with every kind of declaration, every constructor and
-- export form, for the warm-up. A test checks it has them all.
declsWarmUp :: Text
declsWarmUp =
  Text.unlines
    [ "{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, TypeData, GADTs #-}",
      "{-# LANGUAGE PatternSynonyms, DerivingVia, DeriveAnyClass #-}",
      "{-# LANGUAGE FunctionalDependencies, DefaultSignatures, MagicHash #-}",
      "module W (T (..), U (A), C, x, pattern P, type (+), module M, W.y, z) where",
      "-- * Heading",
      "-- $chunk",
      "-- Text.",
      "",
      "-- | A type.",
      "data T a = -- | Con.",
      "    A !Int {-# UNPACK #-} !Int ~Int | a :+ a | R {f, g :: {-# NOUNPACK #-} Int}",
      "  -- ^ After.",
      "  deriving (Eq) deriving stock Show deriving anyclass (C)",
      "  deriving newtype (D) deriving (E) via (F a)",
      "data Ad = Ad",
      "  Int -- ^ First.",
      "  Bool -- ^ Second.",
      "newtype N = N Int",
      "data G a where { G1, G2 :: Int -> G Int; GR :: {gr :: a} -> G a }",
      "type data K = K1",
      "type S :: Type",
      "type S = Int",
      "type family O a",
      "type family J a = r | r -> a",
      "type family Q a where { Q Int = Bool; Q a = a }",
      "data family V a",
      "type a + b = Either a b",
      "infixl 6 +, :+",
      "class (Eq a) => C a b | a -> b where",
      "  type AT a",
      "  type AT a = a",
      "  data AD a",
      "  m, m2 :: a -> b",
      "  m _ = undefined",
      "  {-# INLINE m #-}",
      "  default d :: a",
      "  d :: a",
      "  infixr 5 `m`",
      "  {-# MINIMAL m #-}",
      "instance C Int Int",
      "x, y :: Int",
      "x = 1",
      "y = 2",
      "{-# INLINE x #-}",
      "{-# SPECIALISE x :: Int #-}",
      "{-# SPECIALISE x @Int #-}",
      "{-# OPAQUE y #-}",
      "{-# SCC y #-}",
      "infixr 0 `y`",
      "(z, Just w) = (1, Just 2)",
      "lonely :: Int",
      "pattern P :: Int",
      "pattern P = 0",
      "pattern Q2 x <- Just x where Q2 x = Just x",
      "pattern U2 <- 1",
      "pattern Rec {r1, r2} = (r1, r2)",
      "h = 'c' : \"s\" <> show 1.5 <> show 1#",
      "{-# LINE 100 \"Other.y\" #-}",
      "-- The end."
    ]

-- | CPP modules for the parser's warm-up, with 'cppSettings': CPP, then
-- the pragmas again, spans mapped back, CPP notes; then a parse error
-- and a pragma error after CPP. A test checks they do all that.
cppParseWarmUp :: [Text]
cppParseWarmUp =
  [ cppModule,
    cppModule <> "x = = 1\n",
    "{-# LANGUAGE CPP #-}\n#if 1\n{-# LANGUAGE NoSuchThing #-}\n#endif\n"
  ]

-- | Settings with a cpp-options flag and an ignored word.
cppSettings :: ParseSettings
cppSettings =
  defaultParseSettings
    { ignoredCppOptions = ["-bogus"],
      cpp = defaultParseSettings.cpp {flags = [Define "C" "c"]}
    }

cppModule :: Text
cppModule =
  Text.unlines
    [ "{-# LANGUAGE CPP #-}",
      "{-# OPTIONS_GHC -DO=o -UU -optP-DP=p -Wall #-}",
      "#if 0",
      "{-# LANGUAGE NoSuchThing #-}",
      "#else",
      "{-# LANGUAGE LambdaCase #-}",
      "#endif",
      "#define F(x) x, y",
      "-- | Doc.",
      "module M (F(f),\tO, P, C) where",
      "#include \"x.h\"",
      "#error e",
      "import A (b)",
      "f = __LINE__"
    ]

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
