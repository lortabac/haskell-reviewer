{-# LANGUAGE StrictData #-}

-- | Macros: GHC's built-in ones, @#define@, and @-D@/@-U@ flags.
module HaskellReviewer.Core.Cpp.Macros
  ( -- * Types
    Macro (..),
    BodyPart (..),
    MacroTable,
    MacroFlag (..),
    MacroFlags (..),
    FlagSource (..),

    -- * Building
    macroTable,
    parseDefine,
    macroFlags,
    macroFlag,
    defineMacro,
    undefineMacro,

    -- * Lookup
    lookupMacro,
    isDefined,
    isMacroName,
  )
where

import Data.List (elemIndex, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Cpp.Lines
  ( Tok (..),
    dropChars,
    isBlank,
    isIdentChar,
    isIdentStart,
    lexText,
    tokText,
  )
import HaskellReviewer.Core.Project (GhcVersion (..), PackageName (..), Platform (..))

{- Note [GHC's macros]

What GHC gives cpp, checked with `ghc-9.x -E -cpp -optP-dM
-hide-all-packages` on 9.10.3, 9.12.4 and 9.14.1 (x86_64 Linux). All
three define the same names; only the values change:

  __GLASGOW_HASKELL__               914 (major * 100 + minor)
  __GLASGOW_HASKELL_FULL_VERSION__  "9.14.1"
  __GLASGOW_HASKELL_PATCHLEVEL1__   1 (third number)
  __GLASGOW_HASKELL_PATCHLEVEL2__   0 (fourth number)
  __GHCVERSION_H__                  (empty)
  MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2)  (ghcversion.h's body)
  __GLASGOW_HASKELL_TH__            1
  linux_HOST_OS, linux_BUILD_OS     1
  x86_64_HOST_ARCH, x86_64_BUILD_ARCH  1
  __SSE__, __SSE2__                 1 (x86 only)
  __IO_MANAGER_MIO__                1

The first six come from ghcversion.h (GHC passes -include), the rest
from -D flags. GHC puts these after the user's -optP flags (ghc -v3),
so -D or -U of a built-in name does nothing: macroTable applies the
built-ins last. On Windows GHC adds __IO_MANAGER_WINIO__. The os and
arch names are GHC's, and System.Info uses the same ones (linux,
darwin, mingw32, x86_64, aarch64, i386). With --ghc 9.12 the missing
numbers are 0.

Without -hide-all-packages GHC also adds MIN_VERSION_pkg and
VERSION_pkg for every exposed package; Cabal hides them all and adds
its own for the build-depends. We don't know the versions, so (SPEC
§3.1, D-84): MIN_VERSION_pkg(...) is 1 for any package, but
defined(MIN_VERSION_pkg) only for build-depends, with - turned into _.
VERSION_pkg is left out.

Left out, as they depend on flags: __PIC__, AVX/SSE4/FMA,
__GLASGOW_HASKELL_LLVM__, __GLASGOW_HASKELL_ASSERTS_IGNORED__. Also
left out: what gcc adds itself, __ASSEMBLER__ (from -x
assembler-with-cpp) and the stdc-predef.h macros (__STDC_ISO_10646__
and friends). __FILE__, __DATE__ and __TIME__ are left as is: we have
no path and want the same result every time. So are gcc's other own
names: __TIMESTAMP__, __BASE_FILE__, __COUNTER__, __INCLUDE_LEVEL__ and
__STDC_HOSTED__ (1 in gcc). __STDC__ isn't defined in traditional mode.
-}

-- * Types

-- | A macro definition.
data Macro
  = -- | Object-like: its body.
    ObjectMacro [Tok]
  | -- | Function-like: number of parameters, and the body.
    FunctionMacro Int [BodyPart]
  | -- | @__LINE__@.
    LineMacro
  | -- | @MIN_VERSION_pkg(…)@: 1 with any arguments.
    MinVersionMacro
  deriving (Eq, Show)

-- | A function-like macro's body: text, or a parameter (from 0). In
-- traditional mode parameters are found in string literals too; in a
-- @"…"@ one, gcc puts a backslash before each @"@ of the argument
-- ('QuotedParam'). Not in a @'…'@ one.
data BodyPart = Lit Text | Param Int | QuotedParam Int
  deriving (Eq, Show)

-- | Defined macros by name. Also the @MIN_VERSION_@ names that were
-- undefined: they don't get the default of 1 any more ('lookupMacro').
data MacroTable = MacroTable
  { defs :: Map Text Macro,
    undefs :: Set Text
  }

-- | A @-D@ or @-U@ flag (from cpp-options, ghc-options or OPTIONS_GHC).
-- Kept as written, not parsed: it's part of the parse settings (shown,
-- compared), and parsing it again in 'applyFlag' is cheap.
data MacroFlag
  = -- | Name (maybe with parameters, @F(x)@), and body.
    Define Text Text
  | Undefine Text
  deriving (Eq, Show)

-- * Building

-- | The macros before the file: the flags, in order, then what GHC
-- defines (Note [GHC's macros]), plus @MIN_VERSION_pkg@ for each
-- build-depends. GHC passes its own -D after the user's, so the
-- built-ins win: @-U__GLASGOW_HASKELL__@ does nothing.
macroTable :: GhcVersion -> Platform -> [PackageName] -> [MacroFlag] -> MacroTable
macroTable ghc p deps flags =
  foldl' (\m (n, x) -> defineMacro n x m) fromFlags (builtins ghc p deps)
  where
    fromFlags = foldl' applyFlag (MacroTable Map.empty Set.empty) flags

-- | What GHC defines, plus @MIN_VERSION_pkg@ for each build-depends.
builtins :: GhcVersion -> Platform -> [PackageName] -> [(Text, Macro)]
builtins (GhcVersion ns) p deps =
  [ ("__GLASGOW_HASKELL__", obj (num (part 0 * 100 + part 1))),
    ("__GLASGOW_HASKELL_FULL_VERSION__", obj ("\"" <> fullVersion <> "\"")),
    ("__GLASGOW_HASKELL_PATCHLEVEL1__", obj (num (part 2))),
    ("__GLASGOW_HASKELL_PATCHLEVEL2__", obj (num (part 3))),
    ("__GHCVERSION_H__", obj ""),
    ("__GLASGOW_HASKELL_TH__", obj "1"),
    (p.os <> "_HOST_OS", obj "1"),
    (p.os <> "_BUILD_OS", obj "1"),
    (p.arch <> "_HOST_ARCH", obj "1"),
    (p.arch <> "_BUILD_ARCH", obj "1"),
    ("__IO_MANAGER_MIO__", obj "1"),
    ("__LINE__", LineMacro)
  ]
    <> [("__SSE__", obj "1") | x86]
    <> [("__SSE2__", obj "1") | x86]
    <> [("__IO_MANAGER_WINIO__", obj "1") | p.os == "mingw32"]
    <> [m | Right m <- [parseDefine [minVersionGhc]]]
    <> [ ("MIN_VERSION_" <> Text.replace "-" "_" d, MinVersionMacro)
       | PackageName d <- deps
       ]
  where
    part i = case drop i ns of
      n : _ -> n
      [] -> 0
    num = Text.pack . show
    obj = ObjectMacro . lexText
    fullVersion = Text.intercalate "." (map num (ns <> replicate (3 - length ns) 0))
    x86 = p.arch == "x86_64" || p.arch == "i386"
    -- From ghcversion.h, on one line.
    minVersionGhc =
      "MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) ( \
      \((ma)*100+(mi)) <  __GLASGOW_HASKELL__ || \
      \((ma)*100+(mi)) == __GLASGOW_HASKELL__ \
      \&& (pl1) <  __GLASGOW_HASKELL_PATCHLEVEL1__ || \
      \((ma)*100+(mi)) == __GLASGOW_HASKELL__ \
      \&& (pl1) == __GLASGOW_HASKELL_PATCHLEVEL1__ \
      \&& (pl2) <= __GLASGOW_HASKELL_PATCHLEVEL2__ )"

-- | Read what follows @#define@: a name, maybe parameters right after
-- it (no space), and the body. The text is in chunks split where
-- comments were (see 'directiveText'): a comment ends the name, so
-- @#define A/**/B 5@ defines A as @B 5@, as gcc. Traditional mode has
-- no variadic macros, no @#@ and no @##@ (Note [Traditional mode] in
-- "HaskellReviewer.Core.Cpp").
parseDefine :: [Text] -> Either Text (Text, Macro)
parseDefine = \case
  [] -> Left "no macro name"
  first : more -> case Text.span isIdentChar (Text.dropWhile isBlank first) of
    (name, afterName)
      | not (startsIdent name) -> Left "no macro name"
      | name == "defined" -> Left "\"defined\" can't be a macro name"
      | Just r <- Text.stripPrefix "(" afterName -> do
          let cs = r : more
              whole = Text.concat cs
          (ps, body) <- paramList whole
          let bodyChunks = dropChars (Text.length whole - Text.length body) cs
          if length (nub ps) /= length ps
            then Left ("repeated parameter in " <> name)
            else Right (name, FunctionMacro (length ps) (bodyParts ps (trim bodyChunks)))
      | otherwise ->
          Right (name, ObjectMacro (lexText (Text.concat (trim (afterName : more)))))
  where
    -- Without the blanks around the body.
    trim =
      reverse
        . dropBlank (Text.dropWhileEnd isBlank)
        . reverse
        . dropBlank (Text.dropWhile isBlank)
    -- Drop blank chunks, then trim the first one left with f.
    dropBlank f = \case
      c : cs
        | Text.all isBlank c -> dropBlank f cs
        | otherwise -> f c : cs
      [] -> []
    startsIdent n = maybe False (isIdentStart . fst) (Text.uncons n)
    paramList r = case Text.uncons (Text.dropWhile isBlank r) of
      Just (')', body) -> Right ([], body)
      _ -> params [] r
    params acc r = case Text.span isIdentChar (Text.dropWhile isBlank r) of
      (p, r')
        | startsIdent p -> case Text.uncons (Text.dropWhile isBlank r') of
            Just (',', r'') -> params (p : acc) r''
            Just (')', body) -> Right (reverse (p : acc), body)
            _ -> Left "bad parameter list"
        | otherwise -> Left "bad parameter list"

-- | A function-like body, with parameters found in and out of quotes.
-- Each chunk is read on its own, so a comment ends a name.
bodyParts :: [Text] -> [Text] -> [BodyPart]
bodyParts params chunks = merge (concatMap part (concatMap lexText chunks))
  where
    part = \case
      Ident i -> param i
      Quoted q ->
        let p = if "\"" `Text.isPrefixOf` q then QuotedParam else Param
         in concatMap (word p) (Text.groupBy (\a b -> isIdentChar a == isIdentChar b) q)
      t -> [Lit (tokText t)]
    word p w = case Text.uncons w of
      Just (c, _) | isIdentStart c -> paramAs p w
      _ -> [Lit w]
    param = paramAs Param
    paramAs p i = maybe [Lit i] (\k -> [p k]) (elemIndex i params)
    merge = \case
      Lit a : Lit b : rest -> merge (Lit (a <> b) : rest)
      x : rest -> x : merge rest
      [] -> []

-- | Where flags come from: they are read differently.
data FlagSource
  = -- | Cabal passes each word to cpp (@-optP@), and cpp takes
    -- @-D X@ as well as @-DX@.
    CppOptions
  | -- | ghc-options and OPTIONS_GHC: GHC takes @-D@ and @-U@ with the
    -- rest of the word only (@-D X@ makes X a source file), and passes
    -- @-optP-DX@ on to cpp. GHC's @-optP@ also takes the next word
    -- (@-optP -DX@), and cpp joins @-optP-D -optPX@: we take neither
    -- (rare).
    GhcOptions
  deriving (Eq, Show)

-- | The flags of a list of words, and the other words.
data MacroFlags = MacroFlags
  { -- | The @-D@ and @-U@ flags, in order.
    found :: [MacroFlag],
    rest :: [Text]
  }
  deriving (Eq, Show)

-- | Take the @-D@ and @-U@ flags out of a list of words: @-DX@,
-- @-DX=v@, @-UX@, plus @-D X@ and @-U X@ in cpp-options, or
-- @-optP-DX@ and @-optP-UX@ in ghc-options. @-DX@ is @X@ = 1. A flag
-- that isn't a good @#define@ is one of the other words.
macroFlags :: FlagSource -> [Text] -> MacroFlags
macroFlags source = go [] []
  where
    go fs os = \case
      [] -> MacroFlags {found = reverse fs, rest = reverse os}
      w : v : ws
        | source == CppOptions, w == "-D", Just f <- defineFlag v -> go (f : fs) os ws
        | source == CppOptions, w == "-U", Just f <- undefineFlag v -> go (f : fs) os ws
      w : ws
        | Just f <- macroFlag source w -> go (f : fs) os ws
        | otherwise -> go fs (w : os) ws

-- | One word as a flag: @-DX@, @-DX=v@, @-UX@, or (ghc-options)
-- @-optP-DX@ and @-optP-UX@.
macroFlag :: FlagSource -> Text -> Maybe MacroFlag
macroFlag source w
  | Just d <- prefixed "D" = defineFlag d
  | Just u <- prefixed "U" = undefineFlag u
  | otherwise = Nothing
  where
    prefixed c = case [r | p <- prefixes c, Just r <- [Text.stripPrefix p w]] of
      r : _ | not (Text.null r) -> Just r
      _ -> Nothing
    prefixes c = case source of
      CppOptions -> ["-" <> c]
      GhcOptions -> ["-" <> c, "-optP-" <> c]

-- | What follows @-D@. As gcc: the first = becomes a space, then it's a
-- #define.
defineFlag :: Text -> Maybe MacroFlag
defineFlag d =
  let (n, v) = Text.breakOn "=" d
      body = if Text.null v then "1" else Text.drop 1 v
   in case parseDefine [n <> " " <> body] of
        Right _ -> Just (Define n body)
        Left _ -> Nothing

-- | What follows @-U@: a name.
undefineFlag :: Text -> Maybe MacroFlag
undefineFlag u
  | Text.all isIdentChar u,
    Just (c, _) <- Text.uncons u,
    isIdentStart c =
      Just (Undefine u)
  | otherwise = Nothing

-- | Run a flag on the table.
applyFlag :: MacroTable -> MacroFlag -> MacroTable
applyFlag m = \case
  Define n v -> case parseDefine [n <> " " <> v] of
    Right (name, macro) -> defineMacro name macro m
    -- macroFlags only makes good ones.
    Left _ -> m
  Undefine n -> undefineMacro n m

-- | @#define@.
defineMacro :: Text -> Macro -> MacroTable -> MacroTable
defineMacro n x m =
  MacroTable {defs = Map.insert n x m.defs, undefs = Set.delete n m.undefs}

-- | @#undef@.
undefineMacro :: Text -> MacroTable -> MacroTable
undefineMacro n m =
  MacroTable {defs = Map.delete n m.defs, undefs = Set.insert n m.undefs}

-- * Lookup

-- | A macro by name. A @MIN_VERSION_pkg@ never defined is still 1
-- when called (SPEC §3.1), but not 'isDefined'. After @#undef@ it's
-- just a name, as in gcc.
lookupMacro :: Text -> MacroTable -> Maybe Macro
lookupMacro n m = case Map.lookup n m.defs of
  Just x -> Just x
  Nothing
    | isMinVersion n && not (Set.member n m.undefs) -> Just MinVersionMacro
    | otherwise -> Nothing

-- | For @defined@ and @#ifdef@.
isDefined :: Text -> MacroTable -> Bool
isDefined n m = Map.member n m.defs

-- | Could expand: 'lookupMacro' would find it.
isMacroName :: MacroTable -> Text -> Bool
isMacroName m n = Map.member n m.defs || (isMinVersion n && not (Set.member n m.undefs))

isMinVersion :: Text -> Bool
isMinVersion n =
  "MIN_VERSION_" `Text.isPrefixOf` n && n /= "MIN_VERSION_GLASGOW_HASKELL"
