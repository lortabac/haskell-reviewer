-- | Throwaway mini CPP, to see how far a basic evaluator gets.
--
-- Handles #if/#ifdef/#ifndef/#elif/#else/#endif, object-like #define,
-- #undef, line continuations and /* */ in directives. Expressions:
-- defined, !, &&, ||, comparisons, arithmetic, ?:. MIN_VERSION_pkg(…) = 1,
-- GHC macros for 9.14.1, host os/arch. Undefined names are 0. #include
-- gives nothing. Lines are never added or removed: excluded and directive
-- lines become empty, so positions stay the same.
module Cpp (basicCpp, stripCpp, predefined) where

import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict as M
import Numeric (readHex)
import Types

data Def = Obj String | Fn [String] String

type Defs = M.Map String Def

-- | Macros defined by GHC 9.14.1 and Cabal on x86_64 Linux.
predefined :: [(String, String)]
predefined =
  [ ("__GLASGOW_HASKELL__", "914")
  , ("__GLASGOW_HASKELL_PATCHLEVEL1__", "1")
  , ("__GLASGOW_HASKELL_TH__", "1")
  , ("linux_HOST_OS", "1"), ("x86_64_HOST_ARCH", "1")
  , ("linux_BUILD_OS", "1"), ("x86_64_BUILD_ARCH", "1")
  ]

ghcVersion :: [Integer]
ghcVersion = [9, 14, 1, 0]

-- | Physical lines joined into logical ones. Only directive lines join.
-- Each item: (text, number of physical lines).
logical :: [String] -> [(String, Int)]
logical [] = []
logical (l : ls)
  | isDirective l = join l ls 1
  | otherwise = (l, 1) : logical ls
  where
    join acc rest n
      | "\\" `isSuffixOf` acc, (r : rs) <- rest = join (init acc ++ " " ++ r) rs (n + 1)
      | otherwise = (acc, n) : logical rest

directiveNames :: [String]
directiveNames =
  ["if", "ifdef", "ifndef", "elif", "else", "endif", "define", "undef",
   "include", "error", "warning", "line", "pragma", "include_next"]

-- | Split a directive: "#  if FOO" → Just ("if", "FOO").
directive :: String -> Maybe (String, String)
directive l = case dropWhile isSpace l of
  '#' : rest ->
    let r = dropWhile isSpace rest
        (w, more) = span isAlpha r
    in if w `elem` directiveNames then Just (w, more) else Nothing
  _ -> Nothing

isDirective :: String -> Bool
isDirective = maybe False (const True) . directive

-- | (b): blank every directive line, keep all branches.
stripCpp :: String -> String
stripCpp src = unlines (concatMap out (logical (lines src)))
  where
    out (l, n)
      | isDirective l = replicate n ""
      | otherwise = [l]

data Frame = Frame { fParent :: Bool, fTaken :: Bool, fActive :: Bool }

-- | (c): the mini evaluator. Takes the -D defines of the component.
basicCpp :: [(String, String)] -> String -> (String, CppFeatures)
basicCpp cmdDefs src = (unlines (reverse outLines), feats)
  where
    lls = logical (lines src)
    defs0 = M.fromList [(n, Obj v) | (n, v) <- predefined ++ cmdDefs]
    (outLines, feats) = go lls [] defs0 [] emptyFeats
    emptyFeats = CppFeatures 0 0 0 0 0 0 0 []

    active [] = True
    active (f : _) = fActive f

    go [] _ _ acc fs = (acc, fs)
    go ((l, n) : rest) stack defs acc fs = case directive l of
      Nothing
        | active stack ->
            let fs' = if usesMacro defs l then fs { cfMacroInCode = cfMacroInCode fs + 1 } else fs
            in go rest stack defs (l : acc) fs'
        | otherwise -> go rest stack defs ("" : acc) fs
      Just (w, body0) ->
        let body = stripComments body0
            blanks = replicate n "" ++ acc
            fs1 = countFeatures w body fs { cfDirectives = cfDirectives fs + 1 }
            here = active stack
        in case w of
          _ | w `elem` ["if", "ifdef", "ifndef"] ->
                let c = here && cond w body defs
                in go rest (Frame here c c : stack) defs blanks fs1
          "elif" -> case stack of
            (f : st) ->
              let c = fParent f && not (fTaken f) && evalBool defs body
              in go rest (Frame (fParent f) (fTaken f || c) c : st) defs blanks fs1
            [] -> go rest stack defs blanks (note "#elif without #if" fs1)
          "else" -> case stack of
            (f : st) ->
              let c = fParent f && not (fTaken f)
              in go rest (Frame (fParent f) True c : st) defs blanks fs1
            [] -> go rest stack defs blanks (note "#else without #if" fs1)
          "endif" -> case stack of
            (_ : st) -> go rest st defs blanks fs1
            [] -> go rest stack defs blanks (note "#endif without #if" fs1)
          "define" | here -> case parseDefine body of
            Just (name, d@(Fn _ _)) ->
              go rest stack (M.insert name d defs) blanks fs1 { cfFnDefines = cfFnDefines fs1 + 1 }
            Just (name, d) -> go rest stack (M.insert name d defs) blanks fs1
            Nothing -> go rest stack defs blanks (note "bad #define" fs1)
          "undef" | here -> go rest stack (M.delete (firstIdent body) defs) blanks fs1
          _ | w `elem` ["include", "include_next"], here ->
                go rest stack defs blanks fs1 { cfIncludes = cfIncludes fs1 + 1 }
          "error" | here -> go rest stack defs blanks (note "#error reached" fs1)
          _ -> go rest stack defs blanks fs1

    note s fs = fs { cfUnsupported = s : cfUnsupported fs }

    cond "ifdef" body defs = isDefined defs (firstIdent body)
    cond "ifndef" body defs = not (isDefined defs (firstIdent body))
    cond _ body defs = evalBool defs body

    countFeatures w body fs
      | w `elem` ["if", "elif", "ifdef", "ifndef"] = fs
          { cfMinVersion = cfMinVersion fs + count isMinVersion
          , cfGhcMacros = cfGhcMacros fs + count isGhcMacro
          , cfOsArch = cfOsArch fs + count isOsArch }
      | otherwise = fs
      where
        ids = identifiers body
        count p = length (filter p ids)
    isMinVersion i = "MIN_VERSION_" `isPrefixOf` i && i /= "MIN_VERSION_GLASGOW_HASKELL"
    isGhcMacro i = "__GLASGOW_HASKELL" `isPrefixOf` i || i == "MIN_VERSION_GLASGOW_HASKELL"
    isOsArch i = any (`isSuffixOf` i) ["_HOST_OS", "_HOST_ARCH", "_BUILD_OS", "_BUILD_ARCH"]

    -- A code line uses a macro we know (so real CPP would expand it).
    usesMacro defs l = any (`M.member` defs) (identifiers l)

-- | MIN_VERSION_pkg counts as defined, since it is "true" (§3.1).
isDefined :: Defs -> String -> Bool
isDefined defs x = M.member x defs || "MIN_VERSION_" `isPrefixOf` x

firstIdent :: String -> String
firstIdent = takeWhile isIdChar . dropWhile isSpace

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_'

identifiers :: String -> [String]
identifiers [] = []
identifiers s@(c : cs)
  | isAlpha c || c == '_' = let (i, r) = span isIdChar s in i : identifiers r
  | isDigit c = identifiers (dropWhile isIdChar cs)
  | otherwise = identifiers cs

stripComments :: String -> String
stripComments s = case s of
  [] -> []
  '/' : '*' : r -> ' ' : stripComments (dropComment r)
  '/' : '/' : _ -> []
  c : r -> c : stripComments r
  where
    dropComment ('*' : '/' : r) = r
    dropComment (_ : r) = dropComment r
    dropComment [] = []

parseDefine :: String -> Maybe (String, Def)
parseDefine body = case dropWhile isSpace body of
  s | (name@(_ : _), rest) <- span isIdChar s -> case rest of
      '(' : r ->
        let (params, r') = break (== ')') r
        in Just (name, Fn (words (map (\c -> if c == ',' then ' ' else c) params)) (drop 1 r'))
      _ -> Just (name, Obj (trim rest))
  _ -> Nothing
  where trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Expressions -------------------------------------------------------------

data Tok = TNum Integer | TId String | TOp String
  deriving (Eq, Show)

tokenize :: String -> [Tok]
tokenize [] = []
tokenize s@(c : cs)
  | isSpace c = tokenize cs
  | isAlpha c || c == '_' = let (i, r) = span isIdChar s in TId i : tokenize r
  | isDigit c = let (d, r) = span (\x -> isAlphaNum x) s in TNum (num d) : tokenize r
  | c == '\'' , (ch : '\'' : r) <- cs = TNum (fromIntegral (fromEnum ch)) : tokenize r
  | otherwise = case [op | op <- ops, op `isPrefixOf` s] of
      (op : _) -> TOp op : tokenize (drop (length op) s)
      [] -> TOp [c] : tokenize cs
  where
    ops = ["&&", "||", "==", "!=", "<=", ">=", "<<", ">>",
           "(", ")", "!", "~", "*", "/", "%", "+", "-", "<", ">", "&", "^", "|", "?", ":", ","]
    num d = let d' = filter (`notElem` "uUlL") d in
      case d' of
        '0' : x : h | x `elem` "xX", [(v, "")] <- readHex h -> v
        _ | all isDigit d', not (null d') -> read d'
        _ -> 0

evalBool :: Defs -> String -> Bool
evalBool defs s = case expr 0 (expand defs (20 :: Int) (tokenize s)) of
  Just (v, _) -> v /= 0
  Nothing -> False

-- | Replace defined(X), MIN_VERSION calls and object macros.
expand :: Defs -> Int -> [Tok] -> [Tok]
expand _ 0 ts = ts
expand defs d ts = case ts of
  [] -> []
  TId "defined" : TOp "(" : TId x : TOp ")" : r -> b (isDefined defs x) : expand defs d r
  TId "defined" : TId x : r -> b (isDefined defs x) : expand defs d r
  TId f : TOp "(" : r
    | f == "MIN_VERSION_GLASGOW_HASKELL" ->
        let (args, r') = callArgs r
            vs = map (\a -> maybe 0 fst (expr 0 (expand defs (d - 1) a))) args
        in b (vs <= take (length vs) ghcVersion) : expand defs d r'
    | "MIN_VERSION_" `isPrefixOf` f -> let (_, r') = callArgs r in TNum 1 : expand defs d r'
    | Just (Fn ps body) <- M.lookup f defs ->
        let (args, r') = callArgs r
            sub = M.fromList (zip ps args)
            body' = concatMap (\t -> case t of
                                  TId x | Just a <- M.lookup x sub -> a
                                  _ -> [t]) (tokenize body)
        in expand defs (d - 1) body' ++ expand defs d r'
  TId x : r
    | Just (Obj v) <- M.lookup x defs -> expand defs (d - 1) (tokenize v) ++ expand defs d r
    | otherwise -> TNum 0 : expand defs d r
  t : r -> t : expand defs d r
  where
    b x = TNum (if x then 1 else 0)

-- | Split call arguments at top-level commas, up to the closing paren.
callArgs :: [Tok] -> ([[Tok]], [Tok])
callArgs = go (0 :: Int) [] []
  where
    go _ cur acc [] = (reverse (reverse cur : acc), [])
    go 0 cur acc (TOp ")" : r) = (reverse (reverse cur : acc), r)
    go 0 cur acc (TOp "," : r) = go 0 [] (reverse cur : acc) r
    go n cur acc (t@(TOp "(") : r) = go (n + 1) (t : cur) acc r
    go n cur acc (t@(TOp ")") : r) = go (n - 1) (t : cur) acc r
    go n cur acc (t : r) = go n (t : cur) acc r

-- | Precedence climbing.
expr :: Int -> [Tok] -> Maybe (Integer, [Tok])
expr minPrec ts = do
  (lhs, r) <- unary ts
  loop lhs r
  where
    loop lhs (TOp "?" : r) | minPrec <= 0 = do
      (a, r1) <- expr 0 r
      case r1 of
        TOp ":" : r2 -> do
          (bv, r3) <- expr 0 r2
          loop (if lhs /= 0 then a else bv) r3
        _ -> Nothing
    loop lhs r@(TOp op : rest) = case lookup op binOps of
      Just (p, f) | p >= minPrec -> do
        (rhs, r') <- expr (p + 1) rest
        loop (f lhs rhs) r'
      _ -> Just (lhs, r)
    loop lhs r = Just (lhs, r)

unary :: [Tok] -> Maybe (Integer, [Tok])
unary ts = case ts of
  TOp "!" : r -> fmap (\(v, r') -> (if v == 0 then 1 else 0, r')) (unary r)
  TOp "-" : r -> fmap (\(v, r') -> (negate v, r')) (unary r)
  TOp "+" : r -> unary r
  TOp "~" : r -> fmap (\(v, r') -> (negate v - 1, r')) (unary r)
  TOp "(" : r -> do
    (v, r') <- expr 0 r
    case r' of
      TOp ")" : r'' -> Just (v, r'')
      _ -> Nothing
  TNum n : r -> Just (n, r)
  _ -> Nothing

binOps :: [(String, (Int, Integer -> Integer -> Integer))]
binOps =
  [ ("||", (1, \a b -> bool (a /= 0 || b /= 0)))
  , ("&&", (2, \a b -> bool (a /= 0 && b /= 0)))
  , ("|", (3, (.|.))), ("^", (4, xor)), ("&", (5, (.&.)))
  , ("==", (6, \a b -> bool (a == b))), ("!=", (6, \a b -> bool (a /= b)))
  , ("<", (7, \a b -> bool (a < b))), (">", (7, \a b -> bool (a > b)))
  , ("<=", (7, \a b -> bool (a <= b))), (">=", (7, \a b -> bool (a >= b)))
  , ("<<", (8, \a b -> shiftL a (fromIntegral b))), (">>", (8, \a b -> shiftR a (fromIntegral b)))
  , ("+", (9, (+))), ("-", (9, (-)))
  , ("*", (10, (*))), ("/", (10, \a b -> if b == 0 then 0 else a `quot` b))
  , ("%", (10, \a b -> if b == 0 then 0 else a `rem` b))
  ]
  where bool x = if x then 1 else 0
