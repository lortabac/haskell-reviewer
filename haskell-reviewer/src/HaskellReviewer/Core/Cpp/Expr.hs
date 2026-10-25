{-# LANGUAGE StrictData #-}

-- | @#if@ expressions, after macro expansion, as C's preprocessor: values
-- are 64-bit, signed or unsigned, with C's usual conversions.
module HaskellReviewer.Core.Cpp.Expr
  ( -- * Evaluating
    evalCondition,
  )
where

import Control.Monad (when)
import Data.Bits (Bits, complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (digitToInt, isDigit, isHexDigit, isOctDigit, ord)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HaskellReviewer.Core.Cpp.Lines (Tok (..))
import HaskellReviewer.Core.Cpp.Warning (CppProblem (..), Limit (..))

-- * Evaluating

-- | Is the condition true? An expression we can't read is false, with
-- a 'BadExpression'. Undefined names are 0. Only the parts that are
-- evaluated warn (@1 || 1/0@ is fine), as in gcc.
evalCondition :: [Tok] -> (Bool, [CppProblem])
evalCondition toks = case filter (not . isSep) (etoks toks) of
  [] -> (False, [BadExpression "no expression"])
  ts -> case comma True 0 S {toks = ts, problems = []} of
    Left (Bad t) -> (False, [BadExpression t])
    Left TooDeep -> (False, [LimitHit ExpressionDepth])
    Right (v, s)
      | null s.toks -> (truthy v, reverse s.problems)
      | otherwise -> (False, [BadExpression "missing operator"])
  where
    isSep = \case
      ESep -> True
      _ -> False

-- | Deepest nesting of parens, unary operators and @?:@.
maxDepth :: Int
maxDepth = 256

-- * Internal

-- | A value of C's @intmax_t@ or @uintmax_t@.
data Value = Signed Int64 | Unsigned Word64

truthy :: Value -> Bool
truthy = \case
  Signed n -> n /= 0
  Unsigned n -> n /= 0

bool :: Bool -> Value
bool b = Signed (if b then 1 else 0)

-- | A C integer literal: decimal, @0x@, @0b@ or octal, with @u@ and @l@
-- suffixes. Too big for signed: unsigned, as gcc.
parseNumber :: Text -> Either Text Value
parseNumber t = do
  let isSuffix c = c `elem` ("uUlL" :: String)
      body = Text.dropWhileEnd isSuffix t
      suffix = Text.takeWhileEnd isSuffix t
  when (Text.length suffix > 3) (Left ("bad number " <> t))
  (base, ds) <- case Text.unpack body of
    '0' : x : r | x `elem` ("xX" :: String) -> digits 16 isHexDigit r
    '0' : b : r | b `elem` ("bB" :: String) -> digits 2 (`elem` ("01" :: String)) r
    '0' : r -> if null r then Right (8, "0") else digits 8 isOctDigit r
    r -> digits 10 isDigit r
  let ds' = dropWhile (== '0') ds
  -- Checked before reading, so a huge literal costs nothing.
  when (length ds' > 64) (Left ("number too large " <> t))
  let n = foldl (\a d -> a * base + toInteger (digitToInt d)) 0 ds' :: Integer
  number n (Text.any (`elem` ("uU" :: String)) suffix)
  where
    digits :: Integer -> (Char -> Bool) -> String -> Either Text (Integer, String)
    digits base ok r
      | not (null r), all ok r = Right (base, r)
      | otherwise = Left ("bad number " <> t)
    number n unsigned
      | n > toInteger (maxBound :: Word64) = Left ("number too large " <> t)
      | unsigned || n > toInteger (maxBound :: Int64) = Right (Unsigned (fromInteger n))
      | otherwise = Right (Signed (fromInteger n))

-- | A binary operator, grouped by how it's done.
data BinOp = LogicOp Logic | ShiftOp Shift | ArithOp Arith

data Logic = And | Or

data Shift = Shl | Shr

data Arith
  = BitOr
  | BitXor
  | BitAnd
  | Eq
  | Ne
  | Lt
  | Gt
  | Le
  | Ge
  | Add
  | Sub
  | Mul
  | Div
  | Mod

-- | An expression token. 'EPunct' is any other char (parens, @?@,
-- @!@, ...). 'ESep' is a space: it keeps @< <@ from being @<<@.
data ETok = ENum Value | EBin BinOp | EPunct Text | EBad Text | ESep

data Err = Bad Text | TooDeep

-- | Tokens left and problems so far (newest first).
data S = S
  { toks :: [ETok],
    problems :: [CppProblem]
  }

-- | Two chars with no space between can make one operator.
etoks :: [Tok] -> [ETok]
etoks = \case
  Punct a : Punct b : rest | Just op <- binOp (a <> b) -> EBin op : etoks rest
  t : rest -> etok t : etoks rest
  [] -> []

etok :: Tok -> ETok
etok = \case
  Number n -> either EBad ENum (parseNumber n)
  Ident "defined" -> EBad "defined without a macro name"
  -- Any name left after expansion is 0, as in C.
  Ident _ -> ENum (Signed 0)
  Quoted q -> either EBad ENum (charConstant q)
  Punct p -> maybe (EPunct p) EBin (binOp p)
  Space _ -> ESep

binOp :: Text -> Maybe BinOp
binOp = \case
  "||" -> Just (LogicOp Or)
  "&&" -> Just (LogicOp And)
  "<<" -> Just (ShiftOp Shl)
  ">>" -> Just (ShiftOp Shr)
  "|" -> ar BitOr
  "^" -> ar BitXor
  "&" -> ar BitAnd
  "==" -> ar Eq
  "!=" -> ar Ne
  "<" -> ar Lt
  ">" -> ar Gt
  "<=" -> ar Le
  ">=" -> ar Ge
  "+" -> ar Add
  "-" -> ar Sub
  "*" -> ar Mul
  "/" -> ar Div
  "%" -> ar Mod
  _ -> Nothing
  where
    ar = Just . ArithOp

-- | @'a'@, @'\\n'@, @'\\x41'@, @'\\101'@.
charConstant :: Text -> Either Text Value
charConstant q = case Text.unpack q of
  '\'' : rest | Just inner <- unquote rest -> case inner of
    [c] -> Right (Signed (fromIntegral (ord c)))
    '\\' : e -> escape e
    _ -> Left ("bad character constant " <> q)
  _ -> Left ("string in #if: " <> q)
  where
    unquote r = case reverse r of
      '\'' : inner | not (null inner) -> Just (reverse inner)
      _ -> Nothing
    escape = \case
      [c] | Just v <- lookup c simple -> Right (Signed v)
      'x' : hs@(_ : _)
        | all isHexDigit hs, length hs <= 2 -> Right (Signed (readBase 16 hs))
      os@(_ : _) | all isOctDigit os, length os <= 3 -> Right (Signed (readBase 8 os))
      _ -> Left ("bad character constant " <> q)
    simple =
      [ ('n', 10),
        ('t', 9),
        ('r', 13),
        ('0', 0),
        ('a', 7),
        ('b', 8),
        ('f', 12),
        ('v', 11),
        ('e', 27),
        ('\\', 92),
        ('\'', 39),
        ('"', 34),
        ('?', 63)
      ]
    readBase b = foldl (\a d -> a * b + fromIntegral (digitToInt d)) 0

type P = S -> Either Err (Value, S)

-- | @a, b@: the lowest level.
comma :: Bool -> Int -> P
comma live d s0 = do
  (v, s1) <- cond live d s0
  loop v s1
  where
    loop v s = case s.toks of
      EPunct "," : r -> do
        (v', s') <- cond live d s {toks = r}
        loop v' s'
      _ -> Right (v, s)

-- | @c ? a : b@.
cond :: Bool -> Int -> P
cond live d s0 = do
  (c, s1) <- binary live d 1 s0
  case s1.toks of
    EPunct "?" : r -> do
      deep d
      (a, s2) <- comma (live && truthy c) (d + 1) s1 {toks = r}
      case s2.toks of
        EPunct ":" : r' -> do
          (b, s3) <- cond (live && not (truthy c)) (d + 1) s2 {toks = r'}
          let (a', b') = convert a b
          Right (if truthy c then a' else b', s3)
        _ -> Left (Bad "missing : in ?:")
    _ -> Right (c, s1)

-- | Binary operators, by precedence climbing.
binary :: Bool -> Int -> Int -> P
binary live d minPrec s0 = do
  (lhs, s1) <- unary live d s0
  loop lhs s1
  where
    loop lhs s = case s.toks of
      EBin op : r
        | precedence op >= minPrec -> do
            let live' = case op of
                  LogicOp And -> live && truthy lhs
                  LogicOp Or -> live && not (truthy lhs)
                  _ -> live
            (rhs, s') <- binary live' d (precedence op + 1) s {toks = r}
            let (v, ps) = apply op lhs rhs
            loop v (if live then s' {problems = ps <> s'.problems} else s')
      _ -> Right (lhs, s)

precedence :: BinOp -> Int
precedence = \case
  LogicOp Or -> 1
  LogicOp And -> 2
  ArithOp BitOr -> 3
  ArithOp BitXor -> 4
  ArithOp BitAnd -> 5
  ArithOp Eq -> 6
  ArithOp Ne -> 6
  ArithOp Lt -> 7
  ArithOp Gt -> 7
  ArithOp Le -> 7
  ArithOp Ge -> 7
  ShiftOp _ -> 8
  ArithOp Add -> 9
  ArithOp Sub -> 9
  ArithOp Mul -> 10
  ArithOp Div -> 10
  ArithOp Mod -> 10

unary :: Bool -> Int -> P
unary live d s = do
  deep d
  case s.toks of
    EPunct "!" : r -> inner (bool . not . truthy) r
    EPunct "~" : r -> inner (onValue complement complement) r
    EBin (ArithOp Sub) : r -> inner (onValue negate negate) r
    EBin (ArithOp Add) : r -> unary live (d + 1) s {toks = r}
    EPunct "(" : r -> do
      (v, s') <- comma live (d + 1) s {toks = r}
      case s'.toks of
        EPunct ")" : r' -> Right (v, s' {toks = r'})
        _ -> Left (Bad "missing )")
    ENum v : r -> Right (v, s {toks = r})
    -- Spaces are gone before parsing.
    ESep : r -> unary live d s {toks = r}
    EBad t : _ -> Left (Bad t)
    EPunct p : _ -> Left (Bad ("unexpected " <> p))
    EBin _ : _ -> Left (Bad "missing operand")
    [] -> Left (Bad "missing operand")
  where
    inner f r = do
      (v, s') <- unary live (d + 1) s {toks = r}
      Right (f v, s')

deep :: Int -> Either Err ()
deep d = when (d >= maxDepth) (Left TooDeep)

onValue :: (Int64 -> Int64) -> (Word64 -> Word64) -> Value -> Value
onValue f g = \case
  Signed n -> Signed (f n)
  Unsigned n -> Unsigned (g n)

-- | C's usual conversions: unsigned if either one is.
convert :: Value -> Value -> (Value, Value)
convert a b = case (a, b) of
  (Signed x, Unsigned y) -> (Unsigned (fromIntegral x), Unsigned y)
  (Unsigned x, Signed y) -> (Unsigned x, Unsigned (fromIntegral y))
  _ -> (a, b)

-- | A binary operator on two values, and its warnings.
apply :: BinOp -> Value -> Value -> (Value, [CppProblem])
apply op a b = case op of
  LogicOp And -> (bool (truthy a && truthy b), [])
  LogicOp Or -> (bool (truthy a || truthy b), [])
  ShiftOp Shl -> shift True a b
  ShiftOp Shr -> shift False a b
  ArithOp o -> case convert a b of
    (Signed x, Signed y) -> arith Signed o x y
    (Unsigned x, Unsigned y) -> arith Unsigned o x y
    -- convert never mixes them; the same as it would do.
    (Signed x, Unsigned y) -> arith Unsigned o (fromIntegral x) y
    (Unsigned x, Signed y) -> arith Unsigned o x (fromIntegral y)

-- | Arithmetic and comparisons, on two values of the same type.
arith :: (Integral n, Bits n) => (n -> Value) -> Arith -> n -> n -> (Value, [CppProblem])
arith k o x y = case o of
  Mul -> ok (k (x * y))
  Add -> ok (k (x + y))
  Sub -> ok (k (x - y))
  BitAnd -> ok (k (x .&. y))
  BitOr -> ok (k (x .|. y))
  BitXor -> ok (k (x `xor` y))
  Eq -> ok (bool (x == y))
  Ne -> ok (bool (x /= y))
  Lt -> ok (bool (x < y))
  Gt -> ok (bool (x > y))
  Le -> ok (bool (x <= y))
  Ge -> ok (bool (x >= y))
  Div
    | y == 0 -> (k 0, [DivByZero])
    -- quot minBound (-1) overflows: C wraps.
    | minusOne -> ok (k (negate x))
    | otherwise -> ok (k (x `quot` y))
  Mod
    | y == 0 -> (k 0, [DivByZero])
    | minusOne -> ok (k 0)
    | otherwise -> ok (k (x `rem` y))
  where
    ok v = (v, [])
    -- Only a signed -1.
    minusOne = toInteger y == -1

-- | The result has the left side's type. A negative count shifts the
-- other way, as gcc.
shift :: Bool -> Value -> Value -> (Value, [CppProblem])
shift left a b
  | count >= 64 = (big, [BigShift])
  | otherwise = (shiftBy (fromInteger count), [])
  where
    n = case b of
      Signed x -> toInteger x
      Unsigned x -> toInteger x
    (left', count) = if n < 0 then (not left, negate n) else (left, n)
    big = case a of
      Signed x | not left', x < 0 -> Signed (-1)
      Signed _ -> Signed 0
      Unsigned _ -> Unsigned 0
    shiftBy k = case a of
      Signed x -> Signed (if left' then shiftL x k else shiftR x k)
      Unsigned x -> Unsigned (if left' then shiftL x k else shiftR x k)
