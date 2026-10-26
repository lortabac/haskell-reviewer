{-# LANGUAGE StrictData #-}

-- | CPP for modules with the CPP extension (SPEC §3.1, §11.4.3, D-55):
-- our own evaluator, pure, following gcc's traditional mode as GHC runs
-- it. See Note [Traditional mode].
module HaskellReviewer.Core.Cpp
  ( -- * Types
    CppEnv (..),
    MacroFlag (..),
    MacroFlags (..),
    FlagSource (..),
    CppOutput (..),
    Region (..),
    RegionKind (..),
    CppWarning (..),
    CppProblem (..),
    Limit (..),

    -- * Running
    runCpp,
    macroFlags,
    macroFlag,

    -- * Positions
    PositionMap (..),
    Piece (..),
    Pos (..),
    sameLines,
    mapSpan,
    inExpansion,

    -- * Helpers
    isDirective,
    renderCppWarning,
  )
where

import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Cpp.Expand
  ( CodeRun (..),
    Shared (..),
    expandCode,
    expandIf,
    limitHit,
    newShared,
    warn,
  )
import HaskellReviewer.Core.Cpp.Expr (evalCondition)
import HaskellReviewer.Core.Cpp.Lines
  ( Directive (..),
    Ending (..),
    LLine (..),
    Lines (..),
    allLines,
    directiveText,
    dropChars,
    identWords,
    isBlank,
    isIdentChar,
    isIdentStart,
    lastLineOf,
    lexText,
    nextLine,
    startsDirective,
  )
import HaskellReviewer.Core.Cpp.Macros
  ( FlagSource (..),
    MacroFlag (..),
    MacroFlags (..),
    MacroTable,
    defineMacro,
    isDefined,
    isMacroName,
    macroFlag,
    macroFlags,
    macroTable,
    parseDefine,
    undefineMacro,
  )
import HaskellReviewer.Core.Cpp.Positions
  ( Piece (..),
    Pos (..),
    PositionMap (..),
    inExpansion,
    insertLine,
    mapSpan,
    sameLines,
  )
import HaskellReviewer.Core.Cpp.Warning
  ( CppProblem (..),
    CppWarning (..),
    Limit (..),
    renderCppWarning,
  )
import HaskellReviewer.Core.Project (GhcVersion, PackageName, Platform)

{- Note [Directive lines]

GHC runs cpp as gcc -E -undef -traditional -x assembler-with-cpp. There,
a # in column 1 followed by a name GCC doesn't know (#foo, #., #)) is
not an error: the line is passed on as it is. Indented directives are
not read at all. clang and other cpps differ.

isDirective is the quick "is there any directive?" check: a line is a
directive when its first non-blank character is #, and after the # and
any blanks comes the end of the line (the null directive), a digit (a
line marker, # 12 "f") or a letter or _ (#if, #pragma, #include_next,
#elifdef, ...). So indented ones count too: runCpp then runs, and warns
about them.

Not directives: # followed by a symbol or a bracket: an operator at the
start of a line (#., ##), the end of an unboxed tuple (#)), and #! (a
script's first line). MagicHash names (x#) don't start with #.

Inside the evaluator a directive is a # in column 1, as in gcc.
Contrived forms like #/**/if, or #\ then a newline then if, aren't
caught by isDirective.
-}

{- Note [Traditional mode]

What gcc -E -undef -traditional -x assembler-with-cpp does, checked
with gcc 15, and what we copy:

- Backslash-newline joins lines everywhere, in code too (Haskell string
  gaps and a lambda's \ at the end of a line included). Blanks after the
  backslash still join. A \r before a newline is dropped. A lone \r
  doesn't end a line (gcc splits there, GHC doesn't).
- Joined lines (backslashes, a /* */ comment over lines, a macro call
  over lines) give their text on the first line, then blank lines, so
  lines stay where they are. See Note [Position map] in
  HaskellReviewer.Core.Cpp.Positions.
- /* */ is removed, with nothing in its place, also inside Haskell --
  comments. // is not a comment. A comment opened on a directive line
  goes on past the line end, and a directive inside it isn't seen.
  On purpose, excluded lines are not read, so a /* there opens nothing
  (gcc does track it, outside quotes; rare, and gcc then errors).
- " and ' open a literal up to the matching quote or the end of the
  line, with backslash escapes. Nothing expands inside. So in
  x' = FOO and -- it's FOO, FOO stays; in z = 'a' FOO it expands.
- Names are [A-Za-z_][A-Za-z0-9_]*.
- On purpose, numbers stay whole (C's pp-numbers): 1FOO, 0x10 and 1e10
  never expand. gcc's traditional mode has none, so there 1FOO expands
  FOO, and 0x10 in #if expands a macro x10. Haskell code wants ours.
- Macros expand everywhere outside quotes, Haskell comments too. A
  function-like name with no ( after it stays. The ( may come after
  blanks or on a later line; arguments may span lines (newlines become
  spaces) and keep their blanks. On purpose, a directive line stops
  both: gcc reads it as text there (and loses lines), we run it and
  leave the call as is. And when a function-like name ends a line and
  no ( follows, gcc moves the next line up; we keep lines in place.
- Arguments go into the body as written, not expanded; then the result
  is read again, with the text after it. In the body, parameters are
  replaced inside quotes too ("x" gives "hi"), and /**/ pastes (a/**/b
  gives ab). No #, ## or variadic macros.
- An object-like macro can't expand inside its own expansion. A
  function-like one can, up to 20 levels (gcc's rule). An expansion is
  closed when reading goes past its end, so T(T)(6) gives 6.
- Directives need # in column 1. An indented one is text (we warn). A
  # line with an unknown name (#bogus x, #!x) is text, with macros
  expanded. So is a # line starting with a digit (# 12 "f", or #14285
  in a comment). #pragma, #ident and a lone # give a blank line.
- gcc turns #line into a line marker, and GHC would renumber the lines:
  we drop it, with a warning.
- #undef MIN_VERSION_x makes it a plain name: no default of 1 after it.

Where gcc stops with an error, we warn and go on: a recursive macro or
a wrong argument count leaves the name as is; a bad #if is false;
#error is a note; #include is empty.

defined(X) works in #if, also when a macro makes it. Values are 64
bits, signed or unsigned, with C's conversions; division by zero and
shifts by 64 or more give 0 (or -1 for a negative >>) with a warning.

Not done: #include files, Cabal's VERSION_pkg macros. Left as is (gcc
expands them): __FILE__, __DATE__, __TIME__, __TIMESTAMP__,
__BASE_FILE__, __COUNTER__, __INCLUDE_LEVEL__ and __STDC_HOSTED__ (1).
__STDC__ stays in gcc too (traditional mode).
-}

{- Note [CPP limits]

The code under review is untrusted (SPEC §9.1). CPP can be made to
blow up, so:

- Work: every char a macro makes costs one, as does every char read
  while looking for a paren or arguments (calls that fail are read
  again). A call's size is worked out before its text is made. The
  budget is 3 * input length + 1 MiB, so the output is at most
  4 * input + 1 MiB. When it's spent, nothing more expands for the rest
  of the file (one warning) and text is copied as is, which is linear.
  Comments cost their length too: a failed call reads them again.
- Tokens put back after a failed call don't expand again as the
  object-like macros they came from (Note [Put back] in
  HaskellReviewer.Core.Cpp.Expand), and the reader goes back to a
  slice of the input, so nothing read is kept.
- Warnings: one of a kind per line, at most 1,000 per file (then one
  "dropped" note), texts cut to 200 chars.
- At most 64 macro expansions open at once.
- #if nested at most 1024 deep; deeper ones are false.
- #if expressions nested (parens, unary operators, ?:) at most 256.
- At most 256 arguments in a call, and 10,000 #define per file.
- Numbers: more than 64 digits is "too large" before reading it.

Each limit warns once per file.
-}

-- * Types

-- | What the macros depend on (SPEC §3.1).
data CppEnv = CppEnv
  { -- | The detected project GHC (§7.1), for @__GLASGOW_HASKELL__@.
    ghc :: GhcVersion,
    platform :: Platform,
    -- | From cpp-options, then ghc-options, in order.
    flags :: [MacroFlag],
    -- | For @defined(MIN_VERSION_pkg)@ (D-84).
    buildDepends :: [PackageName]
  }
  deriving (Eq, Show)

-- | The text GHC sees, and what we know about it.
data CppOutput = CppOutput
  { text :: Text,
    positions :: PositionMap,
    -- | Sorted, not overlapping. Blank in 'text'.
    regions :: [Region],
    -- | By line.
    warnings :: [CppWarning]
  }
  deriving (Eq, Show)

-- | File lines cpp took out, inclusive.
data Region = Region
  { kind :: RegionKind,
    firstLine :: Int,
    lastLine :: Int
  }
  deriving (Eq, Show)

-- | Why the lines are blank in the output.
data RegionKind
  = -- | Directives that were run (in active code, or the @#else@ and
    -- @#endif@ of a skipped part).
    DirectiveLines
  | -- | Skipped: in a false branch.
    Excluded
  deriving (Eq, Show)

-- * Running

-- | Run CPP on the text GHC would give to cpp (after unlit), with the
-- @-D@/@-U@ flags of OPTIONS_GHC. Never fails.
--
-- With no directive line, no comment, no backslash at a line end, no
-- macro flag and no built-in macro name (like @__GLASGOW_HASKELL__@ or
-- @linux_HOST_OS@), the text is used as is (D-84).
runCpp :: CppEnv -> [MacroFlag] -> Text -> CppOutput
runCpp env more input
  | null flags && not (any changes physical) && not builtinUsed =
      CppOutput {text = input, positions = sameLines, regions = [], warnings = []}
  | otherwise = finish (go total (start table input) (allLines input))
  where
    flags = env.flags <> more
    -- Every built-in name has one of these in it: a cheap first look.
    builtinUsed =
      any (`Text.isInfixOf` input) ["__", "_HOST_", "_BUILD_", "MIN_VERSION_"]
        && any (isMacroName table) (identWords input)
    physical = Text.splitOn "\n" input
    -- A line cpp changes even with no macro: a directive, a comment,
    -- a backslash at the end (which joins lines, maybe splitting a
    -- built-in name the check below would miss).
    changes l =
      isDirective l
        || "/*" `Text.isInfixOf` l
        || "\\" `Text.isSuffixOf` Text.dropWhileEnd (\c -> isBlank c || c == '\r') l
    total = Text.count "\n" input + 1
    table = macroTable env.ghc env.platform env.buildDepends flags

-- * Helpers

-- | A line we read as a cpp directive. See Note [Directive lines].
isDirective :: Text -> Bool
isDirective l = case Text.stripPrefix "#" (Text.dropWhile isSpace l) of
  Nothing -> False
  -- isSpace also drops the \r of a CRLF line.
  Just rest -> case Text.uncons (Text.dropWhile isSpace rest) of
    Nothing -> True
    Just (c, _) -> isDigit c || isAlpha c || c == '_'

-- * Internal

data St = St
  { shared :: Shared,
    -- | Open #if parts, innermost first.
    frames :: [Frame],
    -- | length frames, kept to check the limit in O(1).
    depth :: Int,
    -- | Output lines, newest first.
    out :: [Text],
    pmap :: PositionMap,
    -- | Newest first.
    regs :: [Region],
    defines :: Int
  }

-- | An open @#if@.
data Frame = Frame
  { branch :: Branch,
    seenElse :: Bool,
    -- | Its line, for 'MissingEndif'.
    at :: Int
  }

-- | Where an @#if@ part is.
data Branch
  = -- | The code around it is skipped: nothing here runs.
    Dead
  | -- | No branch taken yet; this one is skipped.
    Waiting
  | -- | This branch runs.
    Active
  | -- | A branch was taken; this one is skipped.
    Done

-- | A directive line, read.
data DLine = DLine
  { -- | After the @#@ and blanks.
    body :: Text,
    name :: Text,
    args :: Text,
    -- | 'args', in chunks split where comments were.
    argChunks :: [Text],
    first :: Int,
    end :: Int
  }

maxIfDepth :: Int
maxIfDepth = 1024

maxDefines :: Int
maxDefines = 10000

-- | The budget is in Note [CPP limits].
start :: MacroTable -> Text -> St
start table input =
  St
    { shared = newShared table (3 * Text.length input + 1048576),
      frames = [],
      depth = 0,
      out = [],
      pmap = sameLines,
      regs = [],
      defines = 0
    }

isActive :: St -> Bool
isActive st = case st.frames of
  Frame {branch = Active} : _ -> True
  _ : _ -> False
  [] -> True

-- | The last file line before these lines (all the rest: the last line).
lastBefore :: Int -> Lines -> Int
lastBefore total ls = case ls.input of
  Just _ -> ls.number - 1
  Nothing -> total

go :: Int -> St -> Lines -> St
go total st todo = case nextLine todo of
  Nothing -> st
  Just (l, ls)
    | startsDirective l -> directive total st l ls
    | isActive st -> code total st l ls
    | otherwise -> go total (blank Excluded l.firstLine (lastLineOf l) st) ls

-- | Code in an active part.
code :: Int -> St -> LLine -> Lines -> St
code total st0 l ls
  | l.lineCount == 1,
    not ("/*" `Text.isInfixOf` l.text),
    not (any (isMacroName st.shared.macros) (identWords l.text)) =
      go total st {out = l.text : st.out} ls
  | otherwise =
      let r = expandCode (atLine l.firstLine st.shared) l ls
          end = lastBefore total r.after
          st' =
            st
              { shared = r.state,
                out = replicate (end - l.firstLine) "" <> (r.text : st.out),
                pmap = insertLine l.firstLine r.runs st.pmap
              }
       in go total st' r.after
  where
    st
      | isDirective l.text && not (startsDirective l) =
          warnAt l.firstLine IndentedDirective st0
      | otherwise = st0

-- | A line starting with #.
directive :: Int -> St -> LLine -> Lines -> St
directive total st0 l ls
  | dl.name `elem` conditionals = blankLines (conditional dl st)
  | not (isActive st) = blankLines (st, Excluded)
  | otherwise = case activeDirective dl st of
      Just r -> blankLines r
      -- Unknown: text, as gcc in assembler mode. From st0: the code
      -- reader reads the line again and warns by itself.
      Nothing -> code total st0 l ls
  where
    d = directiveText l ls
    whole = Text.concat d.chunks
    body = Text.dropWhile isBlank (Text.drop 1 whole)
    (name, args) = Text.span isIdentChar body
    dl =
      DLine
        { body = body,
          name = name,
          args = args,
          argChunks = dropChars (Text.length whole - Text.length args) d.chunks,
          first = l.firstLine,
          end = lastBefore total d.after
        }
    st =
      let s = st0 {shared = atLine l.firstLine st0.shared}
       in if d.ending == InComment then warnHere UnterminatedComment s else s
    blankLines (s, k) = go total (blank k dl.first dl.end s) d.after
    conditionals =
      ["if", "ifdef", "ifndef", "elif", "elifdef", "elifndef", "else", "endif"]

-- | A directive in active code. Nothing: unknown, so it's text.
activeDirective :: DLine -> St -> Maybe (St, RegionKind)
activeDirective dl st = case dl.name of
  "" | Text.null dl.body -> ok st
  "define" -> ok (defineLine dl st)
  "undef" -> ok $ case firstName dl.args of
    Just n -> setMacros (undefineMacro n) st
    Nothing -> warnHere (BadDefine "#undef with no macro name") st
  "include" -> ok (included st)
  "include_next" -> ok (included st)
  "import" -> ok (included st)
  "error" -> ok (warnHere (ErrorDirective (Text.strip dl.args)) st)
  "warning" -> ok (warnHere (WarningDirective (Text.strip dl.args)) st)
  "line" -> ok (warnHere LineDirectiveIgnored st)
  "pragma" -> ok st
  "ident" -> ok st
  "sccs" -> ok st
  "assert" -> ok (warnHere (UnsupportedDirective dl.name) st)
  "unassert" -> ok (warnHere (UnsupportedDirective dl.name) st)
  _ -> Nothing
  where
    ok s = Just (s, DirectiveLines)
    included = warnHere (IncludeIgnored (Text.strip dl.args))

defineLine :: DLine -> St -> St
defineLine dl st
  | st.defines >= maxDefines = onShared (limitHit DefineCount) st
  | otherwise = case parseDefine dl.argChunks of
      Right (n, m) ->
        let st' = setMacros (defineMacro n m) st
         in st' {defines = st.defines + 1}
      Left e -> warnHere (BadDefine e) st

-- | @#if@ and friends. Run in skipped parts too, to keep the nesting.
conditional :: DLine -> St -> (St, RegionKind)
conditional dl st = case dl.name of
  "if" -> openIf dl (evalIf dl) st
  "ifdef" -> openIf dl (defined dl True) st
  "ifndef" -> openIf dl (defined dl False) st
  "else" -> elseLine dl st
  "endif" -> case st.frames of
    f : fs -> (st {frames = fs, depth = st.depth - 1}, kindOf f)
    [] -> (warnHere UnbalancedEndif st, DirectiveLines)
  -- elif, elifdef, elifndef
  _ -> elifLine dl st

-- | Open a new @#if@ part.
openIf :: DLine -> (St -> (Bool, St)) -> St -> (St, RegionKind)
openIf dl test st
  | st.depth >= maxIfDepth = push Dead (onShared (limitHit IfDepth) st) Excluded
  | isActive st =
      let (b, st') = test st
       in push (if b then Active else Waiting) st' DirectiveLines
  | otherwise = push Dead st Excluded
  where
    push b s k =
      let f = Frame {branch = b, seenElse = False, at = dl.first}
       in (s {frames = f : s.frames, depth = s.depth + 1}, k)

elseLine :: DLine -> St -> (St, RegionKind)
elseLine dl st = case st.frames of
  f : fs ->
    let st' = if f.seenElse then warnHere (ElseAfterElse dl.name) st else st
        b = case f.branch of
          Waiting -> Active
          Active -> Done
          other -> other
     in (st' {frames = f {branch = b, seenElse = True} : fs}, kindOf f)
  [] -> (warnHere (UnbalancedElse dl.name) st, DirectiveLines)

elifLine :: DLine -> St -> (St, RegionKind)
elifLine dl st = case st.frames of
  f : fs
    | f.seenElse ->
        let st' = warnHere (ElseAfterElse dl.name) st
         in (st' {frames = f {branch = off f.branch} : fs}, kindOf f)
    | Waiting <- f.branch ->
        let (b, st') = case dl.name of
              "elifdef" -> defined dl True st
              "elifndef" -> defined dl False st
              _ -> evalIf dl st
            f' = f {branch = if b then Active else Waiting}
         in (st' {frames = f' : fs}, DirectiveLines)
    | otherwise -> (st {frames = f {branch = off f.branch} : fs}, kindOf f)
  [] -> (warnHere (UnbalancedElse dl.name) st, DirectiveLines)
  where
    off = \case
      Active -> Done
      other -> other

kindOf :: Frame -> RegionKind
kindOf f = case f.branch of
  Dead -> Excluded
  _ -> DirectiveLines

-- | @#ifdef@ (yes) or @#ifndef@ (no).
defined :: DLine -> Bool -> St -> (Bool, St)
defined dl yes st = case firstName dl.args of
  Just n -> (isDefined n st.shared.macros == yes, st)
  Nothing -> (False, warnHere (BadExpression "no macro name") st)

evalIf :: DLine -> St -> (Bool, St)
evalIf dl st =
  let (sh, toks) = expandIf st.shared (lexText dl.args)
      (b, problems) = evalCondition toks
   in (b, st {shared = foldl' (flip warn) sh problems})

-- | The first word, if it's a name.
firstName :: Text -> Maybe Text
firstName t = case Text.span isIdentChar (Text.dropWhile isBlank t) of
  (n, _) | Just (c, _) <- Text.uncons n, isIdentStart c -> Just n
  _ -> Nothing

-- | Blank output lines a..b, as a region.
blank :: RegionKind -> Int -> Int -> St -> St
blank k a b st =
  st
    { out = replicate (b - a + 1) "" <> st.out,
      regs = case st.regs of
        r : rs | r.kind == k && r.lastLine + 1 == a -> r {lastLine = b} : rs
        rs -> Region {kind = k, firstLine = a, lastLine = b} : rs
    }

-- | Set the current line, for warnings and @__LINE__@.
atLine :: Int -> Shared -> Shared
atLine n sh = sh {current = n}

warnAt :: Int -> CppProblem -> St -> St
warnAt n p st = st {shared = warn p (atLine n st.shared)}

warnHere :: CppProblem -> St -> St
warnHere p = onShared (warn p)

onShared :: (Shared -> Shared) -> St -> St
onShared f st = st {shared = f st.shared}

setMacros :: (MacroTable -> MacroTable) -> St -> St
setMacros f st = st {shared = st.shared {macros = f st.shared.macros}}

finish :: St -> CppOutput
finish st =
  CppOutput
    { text = Text.intercalate "\n" (reverse st.out),
      positions = st.pmap,
      regions = reverse st.regs,
      warnings = sortOn (.line) (reverse sh.warnings)
    }
  where
    -- Through warn, so the cap holds.
    sh = foldl' (\s f -> warn MissingEndif (atLine f.at s)) st.shared (reverse st.frames)
