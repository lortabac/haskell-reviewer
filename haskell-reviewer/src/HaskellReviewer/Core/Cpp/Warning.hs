{-# LANGUAGE StrictData #-}

-- | What CPP tells the user. CPP never fails: where gcc stops with an
-- error, we warn and go on (SPEC §11.4.3).
module HaskellReviewer.Core.Cpp.Warning
  ( -- * Types
    CppWarning (..),
    CppProblem (..),
    Limit (..),

    -- * Helpers
    renderCppWarning,
    shorten,
    problemKind,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

-- * Types

-- | A problem on a line of the file.
data CppWarning = CppWarning
  { line :: Int,
    problem :: CppProblem
  }
  deriving (Eq, Show)

-- | What went wrong, or what the user should know.
data CppProblem
  = -- | @#error@, with its text. GHC would stop here; we go on.
    ErrorDirective Text
  | -- | @#warning@, with its text.
    WarningDirective Text
  | -- | @#include@ (and @#include_next@, @#import@): read as empty.
    IncludeIgnored Text
  | -- | A gcc directive we don't run (@#assert@, @#unassert@).
    UnsupportedDirective Text
  | -- | A directive not in column 1: gcc reads it as text, so do we.
    IndentedDirective
  | -- | @#line@: dropped, so lines stay those of the file.
    LineDirectiveIgnored
  | -- | A bad @#if@ expression, read as false.
    BadExpression Text
  | -- | Division by zero in @#if@: 0.
    DivByZero
  | -- | A shift by 64 or more.
    BigShift
  | -- | @#else@ or @#elif@ with no @#if@.
    UnbalancedElse Text
  | -- | @#else@ or @#elif@ after @#else@.
    ElseAfterElse Text
  | -- | @#endif@ with no @#if@.
    UnbalancedEndif
  | -- | An @#if@ with no @#endif@; the warning is on the @#if@ line.
    MissingEndif
  | -- | A @#define@ or @#undef@ we can't read, with why. Ignored.
    BadDefine Text
  | -- | Left as is, as text.
    RecursiveMacro Text
  | -- | A call with the wrong number of arguments: left as is.
    WrongArgCount Text
  | -- | A call with no closing paren before a directive or the end.
    UnterminatedCall Text
  | -- | A @/*@ with no @*/@ before the end of the file.
    UnterminatedComment
  | LimitHit Limit
  | -- | Too many warnings: the rest are dropped.
    WarningsDropped
  deriving (Eq, Show)

-- | Limits for hostile input. See Note [CPP limits] in
-- "HaskellReviewer.Core.Cpp".
data Limit
  = -- | Macros expanding into macros, too deep.
    ExpansionDepth
  | -- | Too much text made by macros: we stop expanding.
    ExpansionWork
  | IfDepth
  | ExpressionDepth
  | ArgCount
  | DefineCount
  deriving (Eq, Ord, Show, Enum)

-- * Helpers

-- | E.g. @line 12: #error boom@.
renderCppWarning :: CppWarning -> Text
renderCppWarning w = "line " <> Text.pack (show w.line) <> ": " <> renderProblem w.problem

renderProblem :: CppProblem -> Text
renderProblem = \case
  ErrorDirective t -> "#error " <> t
  WarningDirective t -> "#warning " <> t
  IncludeIgnored t -> "#include " <> t <> " read as empty"
  UnsupportedDirective t -> "#" <> t <> " not supported, ignored"
  IndentedDirective -> "directive not in column 1, read as text"
  LineDirectiveIgnored -> "#line ignored"
  BadExpression t -> "bad #if expression, read as false: " <> t
  DivByZero -> "division by zero in #if"
  BigShift -> "shift by 64 or more in #if"
  UnbalancedElse d -> "#" <> d <> " without #if"
  ElseAfterElse d -> "#" <> d <> " after #else"
  UnbalancedEndif -> "#endif without #if"
  MissingEndif -> "#if without #endif"
  BadDefine t -> "bad #define: " <> t
  RecursiveMacro n -> "recursive macro " <> n <> ", left as is"
  WrongArgCount n -> "wrong number of arguments for " <> n <> ", left as is"
  UnterminatedCall n -> "unterminated call of " <> n <> ", left as is"
  UnterminatedComment -> "unterminated comment"
  LimitHit l -> "limit hit: " <> limitText l
  WarningsDropped -> "too many warnings, the rest are dropped"
  where
    limitText = \case
      ExpansionDepth -> "macros nested too deep"
      ExpansionWork -> "macros make too much text, no more expansion"
      IfDepth -> "#if nested too deep"
      ExpressionDepth -> "#if expression nested too deep"
      ArgCount -> "too many macro arguments"
      DefineCount -> "too many #define"

-- | Cut texts from the file to 200 chars, copied, so a warning never
-- keeps the whole input alive.
shorten :: CppProblem -> CppProblem
shorten = \case
  ErrorDirective t -> ErrorDirective (cut t)
  WarningDirective t -> WarningDirective (cut t)
  IncludeIgnored t -> IncludeIgnored (cut t)
  UnsupportedDirective t -> UnsupportedDirective (cut t)
  BadExpression t -> BadExpression (cut t)
  ElseAfterElse t -> ElseAfterElse (cut t)
  UnbalancedElse t -> UnbalancedElse (cut t)
  BadDefine t -> BadDefine (cut t)
  RecursiveMacro t -> RecursiveMacro (cut t)
  WrongArgCount t -> WrongArgCount (cut t)
  UnterminatedCall t -> UnterminatedCall (cut t)
  p -> p
  where
    cut t
      | Text.length t > 200 = Text.copy (Text.take 200 t <> "...")
      | otherwise = Text.copy t

-- | The constructor (and the limit), for one warning of a kind per line.
problemKind :: CppProblem -> Int
problemKind = \case
  ErrorDirective _ -> 0
  WarningDirective _ -> 1
  IncludeIgnored _ -> 2
  UnsupportedDirective _ -> 3
  IndentedDirective -> 4
  LineDirectiveIgnored -> 5
  BadExpression _ -> 6
  DivByZero -> 7
  BigShift -> 8
  UnbalancedElse _ -> 9
  ElseAfterElse _ -> 10
  UnbalancedEndif -> 11
  MissingEndif -> 12
  BadDefine _ -> 13
  RecursiveMacro _ -> 14
  WrongArgCount _ -> 15
  UnterminatedCall _ -> 16
  UnterminatedComment -> 17
  WarningsDropped -> 18
  -- Each limit its own kind.
  LimitHit l -> 100 + fromEnum l
