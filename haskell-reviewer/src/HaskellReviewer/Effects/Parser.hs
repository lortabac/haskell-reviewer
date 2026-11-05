-- | Running the parser from IO: a time limit, crash catching, and a
-- warm-up.
module HaskellReviewer.Effects.Parser
  ( -- * Parsing
    parseGuarded,
    parseTimeLimit,

    -- * Warm-up
    warmUpParser,
    warmUpResolve,

    -- * For tests
    parseWithin,
    parserWarmUp,
    cppParseWarmUp,
    cppSettings,
    declsWarmUp,
    haddockWarmUp,
    occurrencesWarmUp,
    cppWarmUp,
    cppLimitsWarmUp,
    markdownWarmUp,
  )
where

import Control.Exception
  ( SomeAsyncException,
    SomeException,
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
import HaskellReviewer.Core.Entities (forceExportSet)
import HaskellReviewer.Core.Highlight (Run (..), highlightModule, noRuns, runList)
import HaskellReviewer.Core.LineRegions (noLineRegions)
import HaskellReviewer.Core.ModuleGraph (forceModuleGraph)
import HaskellReviewer.Core.ModuleModel
  ( ModuleParse (..),
    ParsedModule (occurrences),
    TextOnlyReason (..),
    forceModuleParse,
    textOnlyModule,
  )
import HaskellReviewer.Core.Modules (Literate (..))
import HaskellReviewer.Core.Occurrences (forceOccurrences, noOccurrences, occurrenceList)
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
import HaskellReviewer.Core.Resolve
  ( ExportSets (..),
    ResolveInput (..),
    exportSets,
    forceModuleResolution,
    resolveModule,
  )
import HaskellReviewer.Core.ResolveWarmUp (resolveWarmUpInput, resolveWarmUpModules)
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
    textOnly reason ws = TextOnly (textOnlyModule reason ws)
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

-- | Throw it again if it's async.
rethrowAsync :: SomeException -> IO ()
rethrowAsync e = case fromException e of
  Just (_ :: SomeAsyncException) -> throwIO e
  Nothing -> pure ()

-- | Parse a few tiny modules once, so the shared top-level values (ours
-- and ghc-lib-parser's) are built before parallel threads use them
-- (Q-RTS-BLACKHOLE). Each one takes a different path: pragmas and
-- extension names, Haddock, tabs, a parse error, a pragma error, a
-- signature, every kind of declaration, every kind of Haddock markup,
-- CPP modules end to end, and literate files. Each result is
-- highlighted too.
warmUpParser :: IO ()
warmUpParser = do
  _ <- evaluate noTokens
  _ <- evaluate noLineRegions
  _ <- evaluate noRuns
  _ <- evaluate (forceOccurrences noOccurrences)
  mapM_ (uncurry parse) parserWarmUp
  -- The cheap check behind earlyWarnings.
  mapM_
    ( \(l, t) ->
        evaluate (length (earlyWarnings defaultParseSettings {literate = l} t))
    )
    markdownWarmUp
  -- The CPP evaluator, on every kind of line (Q-RTS-BLACKHOLE).
  _ <- evaluate (forceCpp cppWarmUp)
  _ <- evaluate (forceCpp cppLimitsWarmUp)
  _ <- evaluate (sum (map (Text.length . renderCppWarning . CppWarning 1) allProblems))
  pure ()
  where
    -- No time limit (Note [Parse time limit]).
    parse s t =
      tryParse (parseSource s t) >>= \case
        -- runList reads the class words back, as the API will.
        Right p -> do
          _ <- evaluate (sum [fromEnum r.display | r <- runList (highlightModule p)])
          -- Decode the occurrence table too, as the API will.
          case p of
            Parsed m -> () <$ evaluate (length (show (occurrenceList m.occurrences)))
            TextOnly _ -> pure ()
        Left _ -> pure ()

-- | Resolve the warm-up project once, so resolution's shared values
-- are built before request threads use them (Q-RTS-BLACKHOLE). Every
-- result and export set is forced. Parses with no time limit, like
-- 'warmUpParser'.
warmUpResolve :: IO ()
warmUpResolve = do
  let input = resolveWarmUpInput
      sets = exportSets input resolveWarmUpModules
  _ <- evaluate (forceModuleGraph input.graph)
  _ <- evaluate (foldr (\x r -> forceExportSet x `seq` r) () sets.sets)
  _ <- evaluate (foldr (\x r -> forceExportSet x `seq` r) () sets.newFinal)
  mapM_
    (evaluate . maybe () forceModuleResolution . resolveModule input)
    resolveWarmUpModules

-- | Every module 'warmUpParser' parses, with its settings. A test checks
-- they take every path, and that their runs have every class.
parserWarmUp :: [(ParseSettings, Text)]
parserWarmUp =
  map
    (defaultParseSettings {extensions = [Extension "LambdaCase" True]},)
    [ "{-# LANGUAGE CPP, Safe #-}\n{-# OPTIONS_GHC -Wall -XMultiWayIf #-}\n"
        <> "-- | Doc.\nmodule M (x, T (..)) where\nimport A (b)\nx =\t1\n",
      "module M where\nx = = 1\n",
      "{-# LANGUAGE NoSuchThing #-}\nmodule M where\n"
    ]
    <> [ (defaultParseSettings {kind = SignatureFile}, "signature S (T) where\ndata T\n"),
         (defaultParseSettings, declsWarmUp),
         (defaultParseSettings, haddockWarmUp),
         (defaultParseSettings, occurrencesWarmUp)
       ]
    <> map (cppSettings,) cppParseWarmUp
    -- Every markdown-unlit outcome.
    <> [(defaultParseSettings {literate = l}, t) | (l, t) <- markdownWarmUp]
    <> highlightWarmUp

-- | Modules for the highlighting paths: bird and LaTeX prose, CPP
-- around prose, a comment across @#if@ lines, a module that fails to
-- lex and parse.
highlightWarmUp :: [(ParseSettings, Text)]
highlightWarmUp =
  [ (literate BirdOrLatex, "Prose.\n\n> module M where\n\n> x = 1\n"),
    (literate BirdOrLatex, "Prose.\n\\begin{code}\nmodule M where\n\\end{code}\n"),
    ( (literate BirdOrLatex) {extensions = [Extension "CPP" True]},
      "#if 0\nProse.\n#endif\n> module M where\n"
    ),
    ( defaultParseSettings,
      "{-# LANGUAGE CPP #-}\nmodule M where\nx = 1 {-\n#if 0\na\n#else\nb\n#endif\n-}\n"
    ),
    (defaultParseSettings, "{-# LANGUAGE CPP #-}\n#if 1\nmodule M where\n#endif\n{-\n")
  ]
  where
    literate l = defaultParseSettings {literate = l}

-- | A module with every kind of declaration, every constructor and
-- export form, for the warm-up. A test checks it has them all.
declsWarmUp :: Text
declsWarmUp =
  Text.unlines
    [ "{-# LANGUAGE TypeFamilies, TypeFamilyDependencies, TypeData, GADTs #-}",
      "{-# LANGUAGE PatternSynonyms, DerivingVia, DeriveAnyClass #-}",
      "{-# LANGUAGE FunctionalDependencies, DefaultSignatures, MagicHash #-}",
      "{-# LANGUAGE RoleAnnotations, TemplateHaskell, ForeignFunctionInterface #-}",
      "{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}",
      "{-# LANGUAGE TypeOperators, DataKinds, UnboxedTuples, UnboxedSums #-}",
      "{-# LANGUAGE LinearTypes, ImplicitParams, RankNTypes, PolyKinds #-}",
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
      "instance (Show a) => C [a] Int where",
      "  type AT [a] = a",
      "  data AD [a] = ADL a deriving (Eq)",
      "  m :: [a] -> Int",
      "  m _ = 0",
      "  {-# INLINE m #-}",
      "instance {-# OVERLAPPING #-} Show (IO a) where show _ = \"io\"",
      "-- Every branch of the key printer.",
      "instance C (a :+: (b :+: c), (:+:) a, 'Just a, a ': b, (# a, b #), (# a | b #))",
      "  ('[a], '(a, b), 1, \"s\", 'c', (a :: Type), forall x. x, forall k -> k,",
      "   ?p :: Int)",
      "instance C (*, a %1 -> b, a %m -> b, (->) a b, (,) a, [] a, [], _, Show a => a)",
      "  ($(t), M.T, (a -> b) -> c, [a], ())",
      "data Bi a b = Bi deriving (Bifunctor)",
      "deriving instance Eq N",
      "deriving stock instance Ord N",
      "deriving newtype instance Num N",
      "deriving anyclass instance C N N",
      "deriving via Int instance Enum N",
      "type instance O Int = Bool",
      "type instance Ext Int = Bool",
      "data instance V Int = VI {vi :: Int} deriving Show",
      "newtype instance V Bool = VB Int",
      "foreign import ccall \"sin\" c_sin :: Double -> Double",
      "foreign export ccall x :: Int",
      "type role T nominal",
      "type role C nominal nominal",
      "{-# RULES \"r\" forall a. id a = a #-}",
      "{-# COMPLETE P #-}",
      "{-# DEPRECATED x \"old\" #-}",
      "{-# ANN y \"note\" #-}",
      "default (Int)",
      "$(pure [])",
      "{-# LINE 100 \"Other.y\" #-}",
      "-- The end."
    ]

-- | A module that takes every path of the occurrence table: each kind
-- of scope, record form, pun and wildcard, instance methods, splices,
-- quotes, implicit parameters, arrows, type variable scopes, and each
-- resolution, role and namespace (Note [Syntactic scoping] in
-- "HaskellReviewer.Core.Parser.ConvertOccurrences"). A test checks it.
occurrencesWarmUp :: Text
occurrencesWarmUp =
  Text.unlines
    [ "{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, NamedFieldPuns #-}",
      "{-# LANGUAGE OverloadedRecordDot, DuplicateRecordFields, ImplicitParams #-}",
      "{-# LANGUAGE Arrows, RecursiveDo, ParallelListComp, TransformListComp #-}",
      "{-# LANGUAGE ViewPatterns, PatternSynonyms, OverloadedLabels, LambdaCase #-}",
      "{-# LANGUAGE TypeFamilies, MultiWayIf, NPlusKPatterns, RoleAnnotations #-}",
      "{-# LANGUAGE DataKinds, TypeAbstractions, RequiredTypeArguments, DerivingVia #-}",
      "{-# LANGUAGE TypeFamilyDependencies, RankNTypes, GADTs, DefaultSignatures #-}",
      "module W (R (..), f, module X, pattern P, W.k) where",
      "import X (y)",
      "import qualified Z as Q",
      "data R = R {a :: Int, b :: Int} | S {a :: Int}",
      "data D = D {a :: Int}",
      "data E a where E :: {e :: Int} -> E Int",
      "class C t where { m :: t -> Int; m _ = 0; infixl 5 `m` }",
      "instance C R where { m :: R -> Int; m R {a} = a }",
      "instance Show R where show _ = \"\"",
      "type family F x where F x = x",
      "data T a = T",
      "type role T nominal",
      "pattern P x <- Just x where P x = Just x",
      "pattern Q {qa} = Just qa",
      "f :: (?i :: Int) => R -> Int",
      "f r@R {..} | let z = a, Just w <- Nothing = z + w + ?i + r.a"
        <> " + (.b) r + (.dotOnly) r",
      "  where q = 1",
      "f (view -> n) = let ?i = 1 in g n where g (k + 1) = k",
      "h = \\case { R {a = v, b} -> v + b; _ -> 0 }",
      "k = do { rec { x <- pure y; rec { y <- pure x } }; pure [u | u <- x | t <- y] }",
      "l = mdo { s <- pure s; pure s }",
      "o x = proc p -> do { q <- x -< p; returnA -< q }",
      "u = $(pure []) + [| a |] + 'a + ''R + [qq|x|] + $$(t) + Q.v + W.u",
      "v = #lab : [] ++ [z | z <- [], then group by z using groupWith]",
      "w = (R 1 2) {b = 4} {a = 3} : [R {a, b = 1}]",
      "x' = if | True -> 1 | otherwise -> 2",
      "ext Foreign {..} = field + Foreign {fld = 1}",
      "dq = [d| dv = 1 |]",
      "$(pure [])",
      "{-# RULES \"r\" forall n. f n = n #-}",
      "{-# INLINE f #-}",
      "sv :: forall a. a -> a",
      "sv @b (y :: b) = (y :: a) where { g :: forall c. c -> a; g _ = undefined :: a }",
      "ex = (id :: forall q. q -> q) (\\(z :: r) -> z) [] (Just @s Nothing :: Maybe s)",
      "data G where { G1 :: forall x. x -> G; G2 :: (Show y) => {gy :: y} -> G }",
      "data X (k :: kk) = forall e. X e | Y (forall z. z -> k) deriving Show",
      "data N = Z | Sn N",
      "type One = Sn 'Z",
      "type family TF (a :: k) = (r :: k) | r -> a",
      "type family Open a",
      "type instance Open [o] = o",
      "type Syn (a :: k) = (a, forall b. b)",
      "type Phk :: k -> k",
      "class Cl a where { type At a; type At a = a; mc :: a -> b; mc = undefined }",
      "class Cd a where",
      "  { default md :: forall c. a -> c; md :: forall b. a -> b;",
      "    md _ = undefined :: c }",
      "instance Cl [a] where",
      "  { type At [a] = a; mc :: forall b. [a] -> b; mc = undefined :: b }",
      "newtype Wm m = Wm (Maybe m) deriving (Eq) via (Maybe m)",
      "deriving via (Maybe m) instance Ord (Wm m)",
      "vis :: forall a -> a -> a",
      "vis t x = (x :: t)",
      "pattern Pv :: forall a. a -> Maybe a",
      "pattern Pv v <- Just (v :: a)",
      "{-# RULES \"rt\" forall t. forall (n :: t) (m :: u). f n = m #-}"
    ]

-- | A module with every kind of Haddock markup (haddock-library has
-- shared values too), for the warm-up. A test checks it has them all.
haddockWarmUp :: Text
haddockWarmUp =
  Text.unlines
    [ "-- | Header.",
      "--",
      "-- = Heading",
      "--",
      "-- Text /emph/ __bold__ @code@ 'x' v'y' t'T' '(+)' '`elem`' 'M.x' 'case'",
      "-- \"M\" \"M#a\" [label](\"M\") #anchor# &#65; &#x42; \\(x\\) \\[y\\] a\\/b",
      "-- <http://a.b> <https://a.b label> [l](https://a.b) http://a.b/c. ftp://a.b",
      "-- <javascript:x> <<http://a.b/i.png>> <<i.png t>> ![alt](http://a.b/i.png)",
      "--",
      "-- @",
      "-- code /block/",
      "-- @",
      "--",
      "-- > bird",
      "--",
      "-- * item",
      "--",
      "--     * nested",
      "--",
      "-- 1. one",
      "--",
      "-- (2) two",
      "--",
      "-- [term]: definition",
      "--",
      "-- >>> 1 + 1",
      "-- 2",
      "--",
      "-- prop> x == x",
      "--",
      "-- +---+---+",
      "-- | a | b |",
      "-- +===+===+",
      "-- | c | d |",
      "-- +---+---+",
      "--",
      "-- @since 1.2.3",
      "module W (",
      "  -- * Section 'x'",
      "  x,",
      ") where",
      "",
      "-- $chunk",
      "-- [l](http://a.b) starts this paragraph.",
      "--",
      "-- @since base-4.5",
      "",
      "-- | Plain text (Note [Haddock cost]):",
      "-- +" <> Text.replicate 90 "-" <> "+",
      "x :: Int",
      "x = 1"
    ]

-- | Markdown and loose literate files for the warm-up: in place
-- (with @~~~@ and classes), @top@ blocks out of order and in order,
-- an indented fence, @top:@ keys of each kind, no block, and a loose
-- file each way. A test
-- checks they take every path.
markdownWarmUp :: [(Literate, Text)]
markdownWarmUp =
  [ ( MarkdownUnlit,
      "# M\n```haskell top\nmodule M where\n```\n~~~ {.haskell}\nx = 1\n~~~\n"
    ),
    (MarkdownUnlit, "```haskell\nx = 1\n```\n```haskell top:2\nmodule M where\n```\n"),
    (MarkdownUnlit, "  ```haskell\n  x = 1\n  ```\n"),
    -- top: keys that read fail, in parentheses, and overflow: the
    -- Read paths get built too.
    ( MarkdownUnlit,
      "~~~ {.haskell top:x top:(2)}\nmodule M where\n~~~\n"
        <> "```haskell\nx = 1\n```\n"
    ),
    ( MarkdownUnlit,
      "```haskell\nx = 1\n```\n"
        <> "```haskell top:99999999999999999999\ny = 2\n```\n"
    ),
    (MarkdownUnlit, "```{.haskell .ignore}\nx\n```\n```sh\n> x\n"),
    (LooseLiterate, "```haskell\nx = 1\n```\n```\n> GET /\n```\n"),
    (LooseLiterate, "```haskell\nx = 1\n```\n```haskell top\nmodule M where\n```\n"),
    (LooseLiterate, "> x = 1\n```haskell\ny = 2\n```\n")
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
