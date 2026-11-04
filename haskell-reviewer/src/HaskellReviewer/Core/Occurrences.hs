{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- | What each identifier token of a module names, found from the syntax
-- alone (SPEC §11.4.4): a local binder, a top-level declaration of this
-- module, something free, a built-in, or not a name. Packed, one word a
-- token, next to "HaskellReviewer.Core.Tokens". Built by
-- "HaskellReviewer.Core.Parser.ConvertOccurrences".
module HaskellReviewer.Core.Occurrences
  ( -- * Types
    Occurrences,
    Occurrence (..),
    Namespace (..),
    Role (..),
    Resolution (..),
    Certainty (..),
    FreeContext (..),
    NotNameKind (..),
    BinderId (..),
    TokenIx (..),
    Binder (..),
    BinderName (..),

    -- * Reading
    occurrenceAt,
    occurrenceList,
    freeOccurrences,
    binderAt,
    binderCount,
    hasTopSplices,
    conflicts,

    -- * Building
    Builder,
    Ref (..),
    TargetIx,
    newBuilder,
    addTarget,
    newBinder,
    record,
    recordPun,
    isRecorded,
    freezeBuilder,
    maxIndex,
    mergeScopes,
    noOccurrences,
    forceOccurrences,
  )
where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sortOn)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Vector qualified as Boxed
import Data.Vector.Unboxed qualified as Vector
import Data.Vector.Unboxed.Mutable qualified as MVector
import Data.Word (Word32)
import HaskellReviewer.Core.Names (DeclId, forceDeclId)
import HaskellReviewer.Core.Span (Span (..))
import HaskellReviewer.Core.Tokens (spanWords, wordsSpan)

{- Note [Occurrence table]

For each token, one word: what kind of name it is (namespace), what it
does there (role), what it refers to (resolution), and three flags. The
words are parallel to the tokens, so token i has word i. A token that
names nothing has 0.

Bits, from the lowest:

  0-3   resolution kind: none, local, top-level, built-in, not a name,
        and five kinds of free (one per FreeContext)
  4-6   namespace
  7-9   role
  10    heuristic (a top-level guess)
  11    in a splice or a quasi-quote
  12    pun: the field is in the side table
  13-31 payload: binder id, target index, anchor token + 1, or the
        NotNameKind

19 bits of payload: a module over 2^19 - 2 tokens (or binders, or
targets) gets no table at all. Text is capped at 1 MiB ('textLimit' in
"HaskellReviewer.Core.FileContent"), so only a module of half a million
one-character tokens could hit that; real code doesn't. Binders past the
limit are not stored, so a hostile module can't build a huge table
first.

Binders have 6 words each: the binder's span, the index of its first
scope span and how many there are. Scope spans have 4 words each; a
binder's spans are sorted and merged ('mergeScopes'). A binder with no
token of its own (the fields a @..@ binds, a binder made by a CPP
macro) has its name in a small map; the others are named by the text at
their span.

Targets are the top-level declarations of the module the table points
at, each once. They share the model's DeclIds.

Puns (@C {f}@) give one token two names: the word is the variable, and
a sorted side table of (token, word) pairs has the field.

The table is the same for both sides of a diff: it's a function of the
module text (SPEC §11.4.4).
-}

-- * Types

-- | See Note [Occurrence table].
data Occurrences = Occurrences
  { entries :: Vector.Vector Word32,
    binders :: Vector.Vector Word32,
    scopes :: Vector.Vector Word32,
    binderNames :: IntMap Text,
    targets :: Boxed.Vector DeclId,
    -- | Pairs: token, word.
    puns :: Vector.Vector Word32,
    topSplices :: Bool,
    -- | Tokens written twice while building. Always 0, unless there is
    -- a bug. For tests.
    conflictCount :: Int
  }
  deriving (Eq, Show)

-- | What a token names.
data Occurrence = Occurrence
  { namespace :: Namespace,
    role :: Role,
    resolution :: Resolution,
    -- | Inside a splice, or a quasi-quote's quoter: Template Haskell
    -- may make it mean something else.
    inSplice :: Bool,
    -- | For a field pun (@C {f}@): the token is the variable, and this
    -- is the field.
    punField :: Maybe Resolution
  }
  deriving (Eq, Show)

-- | Which kind of name.
data Namespace
  = VarNs
  | ConNs
  | -- | Types and classes.
    TypeNs
  | TyVarNs
  | FieldNs
  | -- | Implicit parameters, @?x@.
    IPNs
  | ModuleNs
  | OtherNs
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | What the name does at this token. Same as the HIE index (D-87).
data Role
  = Use
  | -- | Where it is bound or defined (each equation of a function).
    Def
  | -- | In its own signature, fixity or pragma.
    Sig
  | Import
  | Export
  | -- | A method defined in an instance.
    InstanceDef
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | What it refers to.
data Resolution
  = LocalRef BinderId
  | TopLevelRef DeclId Certainty
  | -- | Not bound in this module, as far as the syntax tells.
    FreeRef FreeContext
  | -- | @:@ and the like.
    BuiltInRef
  | NotAName NotNameKind
  deriving (Eq, Show)

-- | How sure a top-level link is.
data Certainty
  = Exact
  | -- | A guess: one of several fields of that name, or a record dot.
    Heuristic
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | What else we know about a free name, for the semantic tier.
data FreeContext
  = Plain
  | -- | An instance method of an imported class: the class's token.
    MethodOfClass TokenIx
  | -- | A field of an imported constructor: the constructor's token.
    FieldOfCon TokenIx
  | -- | Maybe a field bound by an imported constructor's @C{..}@ (a
    -- guess): the constructor's token.
    MaybeWildcardOf TokenIx
  | -- | The quoter of a quasi-quote.
    QuasiQuoter
  deriving (Eq, Show)

-- | Why an identifier token names nothing.
data NotNameKind
  = ModuleWord
  | -- | @#foo@.
    LabelWord
  | -- | Anything else: role words, a variable-like word the syntax
    -- doesn't take as a name.
    OtherWord
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A local binder, from 0.
newtype BinderId = BinderId Int
  deriving (Eq, Ord, Show)

-- | A token index, from 0.
newtype TokenIx = TokenIx Int
  deriving (Eq, Ord, Show)

-- | A local binder: where it is and where it's visible.
data Binder = Binder
  { span :: Span,
    name :: BinderName,
    -- | Where it's visible, inner shadowed parts included.
    scopes :: [Span]
  }
  deriving (Eq, Show)

-- | How to get a binder's name.
data BinderName
  = -- | The text at its span.
    AtSpan
  | -- | No token of its own (a field @..@ binds, a macro's name).
    Stored Text
  deriving (Eq, Show)

-- * Encoding

-- | The resolution kind, in bits 0-3.
data Kind
  = KindNone
  | KindLocal
  | KindTop
  | KindBuiltIn
  | KindNotAName
  | KindFree
  | KindMethodOf
  | KindFieldOf
  | KindWildcard
  | KindQuoter
  deriving (Eq, Enum, Bounded)

-- | Where each part of a word is. See Note [Occurrence table].
kindMask, threeBits :: Word32
kindMask = 15
threeBits = 7

nsShift, roleShift, heuristicBit, spliceBit, punBit, payloadShift :: Int
nsShift = 4
roleShift = 7
heuristicBit = 10
spliceBit = 11
punBit = 12
payloadShift = 13

-- | The largest payload.
maxIndex :: Int
maxIndex = 2 ^ (32 - payloadShift :: Int) - 2

encode :: Namespace -> Role -> Ref -> Bool -> Bool -> Word32
encode ns role ref splice pun =
  enum kind
    .|. (enum ns `shiftL` nsShift)
    .|. (enum role `shiftL` roleShift)
    .|. flag heuristic heuristicBit
    .|. flag splice spliceBit
    .|. flag pun punBit
    .|. (fromIntegral payload `shiftL` payloadShift)
  where
    enum :: (Enum a) => a -> Word32
    enum = fromIntegral . fromEnum
    flag b k = if b then 1 `shiftL` k else 0
    (kind, payload, heuristic) = case ref of
      LocalR (BinderId b) -> (KindLocal, b, False)
      TopLevelR (TargetIx t) c -> (KindTop, t, c == Heuristic)
      FreeR c -> case c of
        Plain -> (KindFree, 0, False)
        MethodOfClass (TokenIx t) -> (KindMethodOf, t + 1, False)
        FieldOfCon (TokenIx t) -> (KindFieldOf, t + 1, False)
        MaybeWildcardOf (TokenIx t) -> (KindWildcard, t + 1, False)
        QuasiQuoter -> (KindQuoter, 0, False)
      BuiltInR -> (KindBuiltIn, 0, False)
      NotANameR k -> (KindNotAName, fromEnum k, False)

-- | 'Nothing' for 0.
decode :: Occurrences -> Int -> Word32 -> Maybe Occurrence
decode occs token w = case kindOf w of
  KindNone -> Nothing
  _ ->
    Just
      Occurrence
        { namespace = toEnum (fromIntegral ((w `shiftR` nsShift) .&. threeBits)),
          role = toEnum (fromIntegral ((w `shiftR` roleShift) .&. threeBits)),
          resolution = resolutionOf occs w,
          inSplice = testBit w spliceBit,
          punField =
            if testBit w punBit then resolutionOf occs <$> punAt occs token else Nothing
        }

-- | Only 'encode' writes words, so the kind is always in range; the
-- fallback is just to be total.
kindOf :: Word32 -> Kind
kindOf w
  | k <= fromEnum (maxBound :: Kind) = toEnum k
  | otherwise = KindNone
  where
    k = fromIntegral (w .&. kindMask)

resolutionOf :: Occurrences -> Word32 -> Resolution
resolutionOf occs w = case kindOf w of
  KindLocal -> LocalRef (BinderId p)
  KindTop ->
    TopLevelRef (occs.targets Boxed.! p) $
      if testBit w heuristicBit then Heuristic else Exact
  KindBuiltIn -> BuiltInRef
  KindNotAName -> NotAName (toEnum p)
  KindMethodOf -> FreeRef (MethodOfClass (TokenIx (p - 1)))
  KindFieldOf -> FreeRef (FieldOfCon (TokenIx (p - 1)))
  KindWildcard -> FreeRef (MaybeWildcardOf (TokenIx (p - 1)))
  KindQuoter -> FreeRef QuasiQuoter
  KindFree -> FreeRef Plain
  -- decode never asks.
  KindNone -> FreeRef Plain
  where
    p = fromIntegral (w `shiftR` payloadShift)

-- | The pun side table's word for this token.
punAt :: Occurrences -> Int -> Maybe Word32
punAt occs token = go 0 (Vector.length occs.puns `div` 2)
  where
    go lo hi
      | lo >= hi = Nothing
      | otherwise =
          let mid = (lo + hi) `div` 2
              t = fromIntegral (occs.puns Vector.! (2 * mid))
           in case compare t token of
                EQ -> Just (occs.puns Vector.! (2 * mid + 1))
                LT -> go (mid + 1) hi
                GT -> go lo mid

-- * Reading

-- | What the token at this index names. 'Nothing' if nothing, or out
-- of range.
occurrenceAt :: Occurrences -> TokenIx -> Maybe Occurrence
occurrenceAt occs (TokenIx i) = decode occs i =<< (occs.entries Vector.!? i)

-- | Every token that names something, with its index, in order.
occurrenceList :: Occurrences -> [(TokenIx, Occurrence)]
occurrenceList occs =
  [ (TokenIx i, o)
  | (i, w) <- zip [0 ..] (Vector.toList occs.entries),
    Just o <- [decode occs i w]
  ]

-- | The free names, for resolution with the imports.
freeOccurrences :: Occurrences -> [(TokenIx, Occurrence)]
freeOccurrences occs = [p | p@(_, o) <- occurrenceList occs, isFree o.resolution]
  where
    isFree = \case
      FreeRef _ -> True
      _ -> False

-- | How many local binders.
binderCount :: Occurrences -> Int
binderCount occs = Vector.length occs.binders `div` 6

-- | A local binder. 'Nothing' if out of range.
binderAt :: Occurrences -> BinderId -> Maybe Binder
binderAt occs (BinderId b)
  | b < 0 || b >= binderCount occs = Nothing
  | otherwise =
      Just
        Binder
          { span = wordsSpan occs.binders (6 * b),
            name = maybe AtSpan Stored (IntMap.lookup b occs.binderNames),
            scopes = [wordsSpan occs.scopes (4 * s) | s <- [first .. first + count - 1]]
          }
  where
    first = fromIntegral (occs.binders Vector.! (6 * b + 4))
    count = fromIntegral (occs.binders Vector.! (6 * b + 5))

-- | The module has a top-level splice: it may define names we can't
-- see.
hasTopSplices :: Occurrences -> Bool
hasTopSplices occs = occs.topSplices

-- | Tokens written twice while building: a bug if not 0. For tests.
conflicts :: Occurrences -> Int
conflicts occs = occs.conflictCount

-- * Building

-- | What a name refers to, while building: targets by index.
data Ref
  = LocalR BinderId
  | TopLevelR TargetIx Certainty
  | FreeR FreeContext
  | BuiltInR
  | NotANameR NotNameKind

-- | An index into the targets.
newtype TargetIx = TargetIx Int
  deriving (Eq)

-- | A table being built. The entries are written in place; the rest is
-- consed and reversed once at the end.
data Builder s = Builder
  { entries :: MVector.MVector s Word32,
    conflictCount :: STRef s Int,
    binderTotal :: STRef s Int,
    -- | Newest first.
    binderRows :: STRef s [(Span, [Span])],
    scopeCount :: STRef s Int,
    binderNames :: STRef s (IntMap Text),
    targetCount :: STRef s Int,
    -- | Newest first.
    targetList :: STRef s [DeclId],
    -- | Newest first.
    puns :: STRef s [(Int, Word32)]
  }

-- | A builder for this many tokens.
newBuilder :: Int -> ST s (Builder s)
newBuilder n = do
  entries <- MVector.replicate n 0
  conflictCount <- newSTRef 0
  binderTotal <- newSTRef 0
  binderRows <- newSTRef []
  scopeCount <- newSTRef 0
  binderNames <- newSTRef IntMap.empty
  targetCount <- newSTRef 0
  targetList <- newSTRef []
  puns <- newSTRef []
  pure Builder {..}

-- | A new target. The caller adds each declaration once.
addTarget :: Builder s -> DeclId -> ST s TargetIx
addTarget b d = do
  n <- readSTRef b.targetCount
  writeSTRef b.targetCount (n + 1)
  modifySTRef' b.targetList (d :)
  pure (TargetIx n)

-- | A new binder at this span, visible in these spans (merged, see
-- 'mergeScopes'). A name if it has no token of its own. Past
-- 'maxIndex' binders nothing is stored: the table will be dropped.
newBinder :: Builder s -> Span -> Maybe Text -> [Span] -> ST s BinderId
newBinder b s name scope = do
  n <- readSTRef b.binderTotal
  writeSTRef b.binderTotal (n + 1)
  when (n <= maxIndex) $ do
    let merged = mergeScopes scope
    modifySTRef' b.binderRows ((s, merged) :)
    modifySTRef' b.scopeCount (+ length merged)
    case name of
      Just t -> modifySTRef' b.binderNames (IntMap.insert n t)
      Nothing -> pure ()
  pure (BinderId n)

-- | Sorted, with overlapping, touching and contained spans merged. More
-- than 'maxScopes' left: one span over all of them (a bigger scope, so
-- hostile nesting can't make long lists).
mergeScopes :: [Span] -> [Span]
mergeScopes ss = case go (sortOn start ss) of
  merged@(first : _)
    | length merged > maxScopes -> [hull first (maximumOn end merged)]
  merged -> merged
  where
    go = \case
      a : b : rest
        | start b <= end a -> go (hull a (if end b > end a then b else a) : rest)
        | otherwise -> a : go (b : rest)
      rest -> rest
    start x = (x.startLine, x.startCol)
    end x = (x.endLine, x.endCol)
    hull a b = Span a.startLine a.startCol b.endLine b.endCol
    maximumOn f = foldr1 (\a b -> if f a >= f b then a else b)

-- | At most this many scope spans a binder. See 'mergeScopes'.
maxScopes :: Int
maxScopes = 8

-- | Write a token's entry. The Bool: the name is inside a splice. A
-- token already written keeps its first entry, and counts as a
-- conflict.
record :: Builder s -> TokenIx -> Namespace -> Role -> Ref -> Bool -> ST s ()
record b i ns role ref splice = recordWith b i (encode ns role ref splice False)

-- | Write a pun: the variable, and the field it stands for. The Bool:
-- inside a splice.
recordPun :: Builder s -> TokenIx -> Namespace -> Role -> Ref -> Ref -> Bool -> ST s ()
recordPun b i@(TokenIx t) ns role ref field splice = do
  written <- isRecorded b i
  recordWith b i (encode ns role ref splice True)
  if written
    then pure ()
    else modifySTRef' b.puns ((t, encode FieldNs role field splice False) :)

recordWith :: Builder s -> TokenIx -> Word32 -> ST s ()
recordWith b (TokenIx i) w = do
  old <- MVector.read b.entries i
  if kindOf old == KindNone
    then MVector.write b.entries i w
    else modifySTRef' b.conflictCount (+ 1)

-- | The token has an entry already.
isRecorded :: Builder s -> TokenIx -> ST s Bool
isRecorded b (TokenIx i) = (/= KindNone) . kindOf <$> MVector.read b.entries i

-- | The table. 'noOccurrences' when a payload would not fit.
freezeBuilder :: Builder s -> Bool -> ST s Occurrences
freezeBuilder b topSplices = do
  entries <- Vector.freeze b.entries
  conflictCount <- readSTRef b.conflictCount
  nb <- readSTRef b.binderTotal
  rows <- reverse <$> readSTRef b.binderRows
  ns <- readSTRef b.scopeCount
  binderNames <- readSTRef b.binderNames
  nt <- readSTRef b.targetCount
  targets <- Boxed.fromListN nt . reverse <$> readSTRef b.targetList
  punList <- readSTRef b.puns
  let starts = scanl (+) 0 [length sc | (_, sc) <- rows]
      binders =
        Vector.fromListN
          (6 * stored)
          ( concat
              [ spanWords s <> [fromIntegral first, fromIntegral (length sc)]
              | ((s, sc), first) <- zip rows starts
              ]
          )
      scopes = Vector.fromListN (4 * ns) (concatMap spanWords (concatMap snd rows))
      stored = min nb (maxIndex + 1)
      puns =
        Vector.fromList
          (concat [[fromIntegral t, w] | (t, w) <- sortOn fst punList])
  pure $
    if MVector.length b.entries > maxIndex || nb > maxIndex || nt > maxIndex
      then noOccurrences
      else Occurrences {..}

-- | No table: a text-only module, or one too big.
noOccurrences :: Occurrences
noOccurrences =
  Occurrences
    { entries = Vector.empty,
      binders = Vector.empty,
      scopes = Vector.empty,
      binderNames = IntMap.empty,
      targets = Boxed.empty,
      puns = Vector.empty,
      topSplices = False,
      conflictCount = 0
    }

-- | Force the boxed parts, so the table holds no thunk.
forceOccurrences :: Occurrences -> ()
forceOccurrences occs =
  Boxed.foldr (\d r -> forceDeclId d `seq` r) () occs.targets `seq`
    IntMap.foldr seq () occs.binderNames
