{-# LANGUAGE StrictData #-}

-- | CPP for modules with the CPP extension (SPEC §3.1, §11.4.3, D-55).
--
-- Only handles modules with no directive: the text is used as-is (D-84).
-- See Note [Directive lines].
module HaskellReviewer.Core.Cpp
  ( -- * Types
    CppEnv (..),
    CppOutput (..),
    PositionMap (..),
    CppProblem (..),

    -- * Running
    runCpp,

    -- * Helpers
    isDirective,
  )
where

import Data.Char (isAlpha, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Project (GhcVersion, PackageName, Platform)

{- Note [Directive lines]

GHC runs cpp as gcc -E -traditional -x assembler-with-cpp. There, a #
in column 1 followed by a name GCC doesn't know (#foo, #., #)) is not
an error: the line is passed on as it is. Indented directives are not
read at all. clang and other cpps differ.

Our rule: a line is a directive when its first non-blank character is
#, and after the # and any blanks comes the end of the line (the null
directive), a digit (a line marker, # 12 "f") or a letter or _ (#if,
#pragma, #include_next, #elifdef, ...). So indented ones count too.

Not directives: # followed by a symbol or a bracket: an operator at the
start of a line (#., ##), the end of an unboxed tuple (#)), and #! (a
script's first line). MagicHash names (x#) don't start with #.

Too many directives is the safe side: the module is then text only, not
parsed with the wrong text. The only loss is a CPP module with a line
like "  # x" (an operator # then a name) or "  #label" (OverloadedLabels)
at the start of a line.

Contrived forms like #/**/if, or #\ then a newline then if, aren't
caught.
-}

-- * Types

-- | What the macros depend on (SPEC §3.1).
data CppEnv = CppEnv
  { -- | The detected project GHC (§7.1), for @__GLASGOW_HASKELL__@.
    ghc :: GhcVersion,
    platform :: Platform,
    -- | The component's @cpp-options@.
    cppOptions :: [Text],
    -- | For @defined(MIN_VERSION_pkg)@ (D-84).
    buildDepends :: [PackageName]
  }
  deriving (Eq, Show)

-- | The text GHC sees, and where its lines come from.
data CppOutput = CppOutput
  { text :: Text,
    positions :: PositionMap
  }
  deriving (Eq, Show)

-- | From output lines back to the file's lines.
data PositionMap
  = -- | Every line is where it was.
    SameLines
  deriving (Eq, Show)

-- | Why we can't run CPP on a module.
data CppProblem
  = -- | The module has directives. We don't evaluate directives.
    CppNotSupported
  deriving (Eq, Show)

-- * Running

-- | Run CPP on the text GHC would give to cpp (after unlit).
runCpp :: CppEnv -> Text -> Either CppProblem CppOutput
runCpp _ t
  | any isDirective (Text.splitOn "\n" t) = Left CppNotSupported
  | otherwise = Right CppOutput {text = t, positions = SameLines}

-- * Helpers

-- | A line we read as a cpp directive. See Note [Directive lines].
isDirective :: Text -> Bool
isDirective l = case Text.stripPrefix "#" (Text.dropWhile isSpace l) of
  Nothing -> False
  -- isSpace also drops the \r of a CRLF line.
  Just rest -> case Text.uncons (Text.dropWhile isSpace rest) of
    Nothing -> True
    Just (c, _) -> isDigit c || isAlpha c || c == '_'
