{-# LANGUAGE StrictData #-}

-- | Instance keys (D-20, SPEC §4.6): what tells two instances apart, so
-- the two sides of a diff can be paired. Also the orphan rule.
-- The printer and the orphan rule are pure, on 'KeyType'.
module HaskellReviewer.Core.Parser.InstanceKey
  ( -- * Types
    KeyType (..),
    KName (..),
    Arrow (..),
    TupleSort (..),
    ForallVis (..),
    Head (..),

    -- * From GHC
    instanceHead,
    typeArgs,
    binderParams,
    plainName,

    -- * Keys
    instanceKey,
    Derivable,
    derivable,
    derivedKey,
    derivedArity,
    renderArgs,

    -- * Orphans
    Locals (..),
    isOrphan,
  )
where

import Data.Char (isAlpha, isDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Data.FastString (unpackFS)
import GHC.Hs (GhcPs)
import GHC.Hs qualified as Ghc
import GHC.Parser.Annotation (getLocA)
import GHC.Types.Name.Reader (RdrName, isExact, isOrig_maybe, isQual_maybe, isRdrTyVar)
import GHC.Types.SrcLoc (GenLocated (..), SrcSpan, unLoc)
import HaskellReviewer.Core.Names (InstanceKey (..), keyHead, keyHeadChunks)
import HaskellReviewer.Core.Parser.Helpers (moduleName, occText)
import HaskellReviewer.Core.Project (ModuleName)

{- Note [Instance keys]

An instance is a declaration, matched across sides by its key (D-20):
the class name and the head, @inst:Show:Maybe a@. The head is printed
from the AST, so layout and comments don't count:

- the forall and the context are dropped: only the class's arguments
  count. Kind applications (@\@k@) are dropped too;
- type variables are renamed a, b, ..., z, a1, ..., in order of first
  appearance, left to right, over all the arguments. So @Maybe a@ and
  @Maybe b@ are the same key;
- qualifiers are dropped (@M.T@ is @T@). Two classes or types with the
  same name from different modules then clash; that's rare;
- one argument is printed as is; with several, each is in argument
  position (in parens if it's an application, an operator, ...);
- parens are ours, not the source's: @(Maybe a)@ written with extra
  parens is the same key. Nested operator applications always get
  parens, as we don't know fixities;
- an operator applied to two arguments is printed infix, so @(:+:) a b@
  and @a :+: b@ are the same key. Built-ins written prefix are
  printed like the usual form: @(->) a b@ is @a -> b@, @(,) a b@ is
  @(a, b)@, @[] a@ is @[a]@. Unsaturated, they are @(->)@, @(,)@, @[]@.

A family instance has the same kind of key: the family and its
arguments, @inst:F:[a]@.

A class in a deriving clause gives an instance too. Its head is the
class's own arguments, then the type applied to its parameters, minus
the last ones the class takes (its arity, 'derivedArity'):
@data T a b deriving (Functor)@ is @inst:Functor:T a@. The key must be
the same as for the standalone @deriving instance Functor (T a)@:
turning one into the other is a strategy change, not a removed and an
added instance (SPEC §4.6). The arity table only lists well-known
classes; for any other class we assume it takes none, so the two keys
differ. That's a known limit, as are parameters hidden in a kind
signature (@data T a :: Type -> Type@: we only see @a@).

The type's part of a derived head only depends on how many parameters
the class drops, so it is printed once per arity and shared. A class
with no arguments uses it as is: a type with n parameters and n
classes stays linear. A class whose arguments have no variables
(@MonadState S@) prints them and adds the shared part, as another
chunk of the key's head ('KeyHead'), so nothing long is copied.
A class whose arguments have variables (@C s@) is printed whole, as
the renaming depends on them: n such classes on n parameters is
quadratic. That's a known limit (such classes are rare).
-}

{- Note [Orphans]

An instance is an orphan when neither its class nor any type
constructor in its head is declared in this module (GHC's rule, but
syntactic). A name counts as declared here when it's unqualified, or
qualified with this module's name (@Main@ with no @module@ line), and:

- a type, class, family or synonym of this module has that name; or
- it's ticked (@'A@) and a data constructor of this module has that
  name. An unticked one is taken as a type: a constructor named like an
  imported type (@data V = Text Text@) doesn't make @Text@ local.

A family instance is an orphan when neither the family nor any type
constructor in its arguments is from this module. An instance from a
deriving clause is never an orphan.
-}

-- * Types

-- | A type, as far as keys care.
data KeyType
  = KVar Text
  | KCon KName
  | -- | Never with an empty list.
    KApp KeyType [KeyType]
  | KFun Arrow KeyType KeyType
  | KList KeyType
  | KTuple TupleSort [KeyType]
  | -- | @(# a | b #)@.
    KSum [KeyType]
  | -- | @'[a, b]@.
    KPromotedList [KeyType]
  | KLit Text
  | KWild
  | KStar
  | KKindSig KeyType KeyType
  | -- | Binders and the body.
    KForall [KeyType] ForallVis KeyType
  | KQual [KeyType] KeyType
  | KIParam Text KeyType
  | KSplice
  deriving (Eq, Show)

-- | A type constructor or data constructor name.
data KName = KName
  { qualifier :: Maybe ModuleName,
    occ :: Text,
    -- | Written with a tick.
    promoted :: Bool
  }
  deriving (Eq, Show)

-- | A function arrow.
data Arrow = PlainArrow | LinearArrow | MultArrow KeyType
  deriving (Eq, Show)

-- | Which kind of tuple.
data TupleSort = Boxed | Unboxed | PromotedTuple
  deriving (Eq, Show)

-- | @forall a ->@ or @forall a.@.
data ForallVis = VisibleForall | InvisibleForall
  deriving (Eq, Show)

-- | An instance head: the class and its arguments.
data Head = Head
  { className :: KName,
    -- | Where the class name is, if it's a name.
    classSpan :: Maybe SrcSpan,
    args :: [KeyType]
  }
  deriving (Show)

-- | The direct parts of a type, in print order (but see 'Arrow').
children :: KeyType -> [KeyType]
children = \case
  KApp f as -> f : as
  -- Renaming order puts the multiplicity first, though it prints
  -- between the sides. Fine: keys only need to agree with themselves.
  KFun (MultArrow m) x y -> [m, x, y]
  KFun _ x y -> [x, y]
  KList x -> [x]
  KTuple _ xs -> xs
  KSum xs -> xs
  KPromotedList xs -> xs
  KKindSig x k -> [x, k]
  KForall bs _ x -> bs <> [x]
  KQual cs x -> cs <> [x]
  KIParam _ x -> [x]
  KVar _ -> []
  KCon _ -> []
  KLit _ -> []
  KWild -> []
  KStar -> []
  KSplice -> []

-- * From GHC

-- | The head of @instance ctx => C t1 t2@ or @deriving instance ...@,
-- or of a class in a deriving clause.
instanceHead :: Ghc.LHsSigType GhcPs -> Head
instanceHead (L _ sig) = case sig of
  Ghc.HsSig {Ghc.sig_body = body} -> go (strip body)
  where
    strip (L l t) = case t of
      Ghc.HsForAllTy {Ghc.hst_body = b} -> strip b
      Ghc.HsQualTy {Ghc.hst_body = b} -> strip b
      Ghc.HsParTy _ b -> strip b
      Ghc.HsDocTy _ b _ -> strip b
      _ -> L l t
    go lt = case spine lt [] of
      (L _ (Ghc.HsTyVar _ prom n), as) ->
        Head
          { className = kname prom (unLoc n),
            classSpan = Just (getLocA n),
            args = map keyType as
          }
      (L _ (Ghc.HsOpTy _ prom a n b), []) ->
        Head
          { className = kname prom (unLoc n),
            classSpan = Just (getLocA n),
            args = [keyType a, keyType b]
          }
      (h, as) ->
        -- Not a class name (a splice, say): print it as the class.
        Head
          { className = KName Nothing (renderArgs [keyType h]) False,
            classSpan = Nothing,
            args = map keyType as
          }

-- | Peel applications, kind applications and parens off the left.
-- Linear: arguments are consed onto the accumulator.
spine ::
  Ghc.LHsType GhcPs -> [Ghc.LHsType GhcPs] -> (Ghc.LHsType GhcPs, [Ghc.LHsType GhcPs])
spine lt@(L _ t) acc = case t of
  Ghc.HsAppTy _ f x -> spine f (x : acc)
  Ghc.HsAppKindTy _ f _ -> spine f acc
  Ghc.HsParTy _ x -> spine x acc
  Ghc.HsDocTy _ x _ -> spine x acc
  _ -> (lt, acc)

-- | A GHC type as a 'KeyType'.
keyType :: Ghc.LHsType GhcPs -> KeyType
keyType lt = case spine lt [] of
  (L _ (Ghc.HsTyVar _ Ghc.NotPromoted (L _ n)), as)
    | Just b <- builtin n -> builtinApp b (map keyType as)
  (h, []) -> atom h
  (h, as) -> case atom h of
    -- An application of an application: @(f a) b@ after parens.
    KApp f bs -> KApp f (bs <> map keyType as)
    k -> KApp k (map keyType as)
  where
    atom (L _ t) = case t of
      Ghc.HsTyVar _ prom (L _ n)
        | isRdrTyVar n -> KVar (occText n)
        | otherwise -> KCon (kname prom n)
      Ghc.HsOpTy _ prom a (L _ n) b
        | isRdrTyVar n -> KApp (KVar (occText n)) [keyType a, keyType b]
        | otherwise -> KApp (KCon (kname prom n)) [keyType a, keyType b]
      Ghc.HsForAllTy {Ghc.hst_tele = tele, Ghc.hst_body = b} -> case tele of
        Ghc.HsForAllVis {Ghc.hsf_vis_bndrs = bs} ->
          KForall (map binder bs) VisibleForall (keyType b)
        Ghc.HsForAllInvis {Ghc.hsf_invis_bndrs = bs} ->
          KForall (map binder bs) InvisibleForall (keyType b)
      Ghc.HsQualTy {Ghc.hst_ctxt = L _ ctx, Ghc.hst_body = b} ->
        KQual (map keyType ctx) (keyType b)
      Ghc.HsFunTy _ mult a b -> KFun (arrow mult) (keyType a) (keyType b)
      Ghc.HsListTy _ a -> KList (keyType a)
      Ghc.HsTupleTy _ Ghc.HsUnboxedTuple as -> KTuple Unboxed (map keyType as)
      Ghc.HsTupleTy _ Ghc.HsBoxedOrConstraintTuple as -> KTuple Boxed (map keyType as)
      Ghc.HsSumTy _ as -> KSum (map keyType as)
      Ghc.HsIParamTy _ (L _ (Ghc.HsIPName n)) a ->
        KIParam (Text.pack (unpackFS n)) (keyType a)
      Ghc.HsStarTy _ _ -> KStar
      Ghc.HsKindSig _ a k -> KKindSig (keyType a) (keyType k)
      Ghc.HsSpliceTy _ _ -> KSplice
      Ghc.HsExplicitListTy _ _ as -> KPromotedList (map keyType as)
      Ghc.HsExplicitTupleTy _ _ as -> KTuple PromotedTuple (map keyType as)
      Ghc.HsTyLit _ l -> KLit $ case l of
        Ghc.HsNumTy _ n -> Text.pack (show n)
        Ghc.HsStrTy _ s -> Text.pack (show (unpackFS s))
        Ghc.HsCharTy _ c -> Text.pack (show c)
      Ghc.HsWildCardTy _ -> KWild
      -- Taken off by 'spine'; here for completeness.
      Ghc.HsAppTy _ f x -> KApp (keyType f) [keyType x]
      Ghc.HsAppKindTy _ f _ -> keyType f
      Ghc.HsParTy _ x -> keyType x
      Ghc.HsDocTy _ x _ -> keyType x
      Ghc.XHsType _ -> KWild
    arrow = \case
      Ghc.HsLinearAnn _ -> LinearArrow
      Ghc.HsExplicitMult _ m -> MultArrow (keyType m)
      _ -> PlainArrow
    binder (L _ b) = case b of
      Ghc.HsTvb {Ghc.tvb_var = v, Ghc.tvb_kind = k} -> case k of
        Ghc.HsBndrKind _ kind -> KKindSig (bndrVar v) (keyType kind)
        _ -> bndrVar v

-- | A built-in type constructor written prefix.
data Builtin = FunCon | ListCon | TupleCon TupleSort Int

-- | @(->)@, @[]@, @(,)@, @(# , #)@. The parser gives them Exact or Orig
-- names, which user code can't write, so a user's own @List@ isn't one.
builtin :: RdrName -> Maybe Builtin
builtin n
  | isExact n || isOrig n = case occText n of
      "->" -> Just FunCon
      "List" -> Just ListCon
      o -> tuple o
  | otherwise = Nothing
  where
    isOrig = isJust . isOrig_maybe
    tuple o = do
      rest <- Text.stripPrefix "Tuple" o
      let (digits, hash) = Text.span isDigit rest
      k <- if Text.null digits then Nothing else Just (read (Text.unpack digits))
      case hash of
        "" -> Just (TupleCon Boxed k)
        "#" -> Just (TupleCon Unboxed k)
        _ -> Nothing

-- | A built-in applied to arguments: the usual form when saturated.
builtinApp :: Builtin -> [KeyType] -> KeyType
builtinApp b as = case (b, as) of
  (FunCon, [x, y]) -> KFun PlainArrow x y
  (ListCon, [x]) -> KList x
  (TupleCon s k, _) | length as == k -> KTuple s as
  _ -> case as of
    [] -> con
    _ -> KApp con as
  where
    con = KCon KName {qualifier = Nothing, occ = prefixText, promoted = False}
    prefixText = case b of
      FunCon -> "->"
      ListCon -> "[]"
      TupleCon Unboxed k -> "(#" <> Text.replicate (k - 1) "," <> "#)"
      TupleCon _ k -> "(" <> Text.replicate (k - 1) "," <> ")"

-- | A binder's variable.
bndrVar :: Ghc.HsBndrVar GhcPs -> KeyType
bndrVar = \case
  Ghc.HsBndrVar _ (L _ n) -> KVar (occText n)
  _ -> KWild

-- | The visible arguments of a family instance: kind arguments
-- (@\@k@) dropped.
typeArgs :: [Ghc.LHsTypeArg GhcPs] -> [KeyType]
typeArgs = mapMaybe $ \case
  Ghc.HsValArg _ t -> Just (keyType t)
  _ -> Nothing

-- | A data declaration's visible parameters, as variables (kinds
-- dropped).
binderParams :: [Ghc.LHsTyVarBndr (Ghc.HsBndrVis GhcPs) GhcPs] -> [KeyType]
binderParams bs =
  [ bndrVar v
  | L _ Ghc.HsTvb {Ghc.tvb_flag = Ghc.HsBndrRequired _, Ghc.tvb_var = v} <- bs
  ]

-- | A name as written, no tick.
plainName :: RdrName -> KName
plainName = kname Ghc.NotPromoted

kname :: Ghc.PromotionFlag -> RdrName -> KName
kname prom n =
  KName
    { qualifier = moduleName . fst <$> isQual_maybe n,
      occ = occText n,
      promoted = case prom of
        Ghc.IsPromoted -> True
        Ghc.NotPromoted -> False
    }

-- * Keys

-- | The key of an instance with this head. See Note [Instance keys].
instanceKey :: Head -> InstanceKey
instanceKey h = InstanceKey {name = h.className.occ, head = keyHead (renderArgs h.args)}

-- | A type (or data instance) with its parameters, ready for the keys
-- of its deriving clauses. Build it once per type: the heads of classes
-- without arguments are shared (Note [Instance keys]).
data Derivable = Derivable
  { name :: Text,
    params :: [KeyType],
    count :: Int,
    -- | The head for a class without arguments, by how many parameters
    -- it drops (0 to 2). Lazy: each is printed when first used.
    plainHeads :: [Text],
    -- | The same, in argument position, for a class whose arguments
    -- have no variables.
    argHeads :: [Text]
  }

-- | See 'Derivable'.
derivable :: Text -> [KeyType] -> Derivable
derivable n ps =
  Derivable
    { name = n,
      params = ps,
      count = c,
      plainHeads = [renderArgs [t] | t <- kept],
      argHeads = [piece (renaming [t]) AppArg t | t <- kept]
    }
  where
    c = length ps
    kept = [applied n (take (c - d) ps) | d <- [0 .. 2]]

-- | The type applied to these parameters.
applied :: Text -> [KeyType] -> KeyType
applied n ps = if null ps then con else KApp con ps
  where
    con = KCon KName {qualifier = Nothing, occ = n, promoted = False}

-- | The key of a class in a deriving clause (its head, as written). See
-- Note [Instance keys].
derivedKey :: Derivable -> Head -> InstanceKey
derivedKey ty cls = InstanceKey {name = cls.className.occ, head = h}
  where
    d = min (derivedArity cls.className.occ) ty.count
    kept = take (ty.count - d) ty.params
    own = renderArgs (cls.args <> [applied ty.name kept])
    -- No variables in the class's arguments: the type's part is
    -- renamed alone, so it can be shared.
    closed = Text.intercalate " " (map (piece Map.empty AppArg) cls.args)
    h = case (cls.args, drop d ty.plainHeads, drop d ty.argHeads) of
      ([], shared : _, _) -> keyHeadChunks [shared]
      (_, _, shared : _) | null (varsOf cls.args) -> keyHeadChunks [closed, " ", shared]
      _ -> keyHead own

-- | How many of the type's last parameters a derivable class takes.
-- Only well-known classes; others take none. See Note [Instance keys].
derivedArity :: Text -> Int
derivedArity c
  | Set.member c ones = 1
  | Set.member c twos = 2
  | otherwise = 0
  where
    ones =
      Set.fromList
        [ "Functor",
          "Foldable",
          "Traversable",
          "Generic1",
          "Applicative",
          "Alternative",
          "Contravariant",
          "Monad",
          "MonadFail",
          "MonadIO",
          "MonadPlus",
          "MonadFix",
          "MonadZip",
          "MonadState",
          "MonadReader",
          "MonadWriter",
          "MonadError",
          "MonadRWS",
          "MonadCont",
          "MonadThrow",
          "MonadCatch",
          "MonadMask",
          "MonadUnliftIO",
          "MonadBase",
          "MonadBaseControl",
          "PrimMonad",
          "MonadLogger",
          "MonadResource",
          "Eq1",
          "Ord1",
          "Show1",
          "Read1",
          "NFData1",
          "Hashable1"
        ]
    twos =
      Set.fromList
        [ "Bifunctor",
          "Bifoldable",
          "Bitraversable",
          "Category",
          "Profunctor",
          "Arrow",
          "ArrowChoice",
          "Eq2",
          "Ord2",
          "Show2",
          "Read2"
        ]

-- | Print a head's arguments, variables renamed. See Note [Instance
-- keys].
renderArgs :: [KeyType] -> Text
renderArgs ts = Text.intercalate " " $ case ts of
  [t] -> [piece names Top t]
  _ -> map (piece names AppArg) ts
  where
    names = renaming ts

-- | One type, printed with these names.
piece :: Map Text Text -> Prec -> KeyType -> Text
piece names p t = Text.concat (render names p t [])

-- | The variables, in print order, repeats kept.
varsOf :: [KeyType] -> [Text]
varsOf = foldr vars []
  where
    vars t rest = case t of
      KVar v -> v : rest
      _ -> foldr vars rest (children t)

-- | Pieces of text, joined once at the end, so deep types print in
-- linear time.
type Pieces = [Text] -> [Text]

-- | Variables to their new names, in order of first appearance.
renaming :: [KeyType] -> Map Text Text
renaming ts = fst (foldl add (Map.empty, 0 :: Int) (varsOf ts))
  where
    add (m, n) v
      | Map.member v m = (m, n)
      | otherwise = (Map.insert v (fresh n) m, n + 1)
    fresh n =
      let (q, r) = n `divMod` 26
          letter = Text.singleton (toEnum (fromEnum 'a' + r))
       in if q == 0 then letter else letter <> Text.pack (show q)

-- | Where a type is printed, from loosest to tightest.
data Prec
  = Top
  | -- | Left of an arrow or @::@.
    FunLeft
  | -- | An operand of an operator.
    OpArg
  | -- | An argument of an application.
    AppArg
  deriving (Eq, Ord)

-- | Print at a precedence.
render :: Map Text Text -> Prec -> KeyType -> Pieces
render names = go
  where
    go :: Prec -> KeyType -> Pieces
    go p = \case
      KVar v -> txt (Map.findWithDefault v v names)
      KCon n -> prefixName n
      KApp (KCon n) [a, b]
        | isSym n.occ -> parens (p >= OpArg) (infixApp (tick n <> n.occ) a b)
      KApp f as -> parens (p >= AppArg) (go AppArg f . foldr (\a r -> spaced a . r) id as)
      KFun a x y -> parens (p >= FunLeft) (go FunLeft x . arrow a . go Top y)
      KList x -> txt "[" . go Top x . txt "]"
      KTuple Boxed xs -> txt "(" . commas xs . txt ")"
      KTuple Unboxed xs -> txt "(# " . commas xs . txt " #)"
      KTuple PromotedTuple xs -> txt "'(" . commas xs . txt ")"
      KSum xs -> txt "(# " . sepBy " | " xs . txt " #)"
      KPromotedList xs -> txt "'[" . commas xs . txt "]"
      KLit l -> txt l
      KWild -> txt "_"
      KStar -> txt "*"
      KKindSig x k -> parens (p >= FunLeft) (go FunLeft x . txt " :: " . go Top k)
      KForall bs vis x ->
        parens (p >= FunLeft) $
          txt "forall"
            . foldr (\b r -> txt " " . go AppArg b . r) id bs
            . txt (if vis == VisibleForall then " -> " else ". ")
            . go Top x
      KQual cs x -> parens (p >= FunLeft) (context cs . txt " => " . go Top x)
      KIParam n x -> parens (p >= FunLeft) (txt ("?" <> n <> " :: ") . go Top x)
      KSplice -> txt "$(..)"
    spaced a = txt " " . go AppArg a
    infixApp op a b = go OpArg a . txt (" " <> op <> " ") . go OpArg b
    arrow = \case
      PlainArrow -> txt " -> "
      LinearArrow -> txt " %1 -> "
      MultArrow m -> txt " %" . go AppArg m . txt " -> "
    context = \case
      [c] -> go FunLeft c
      cs -> txt "(" . commas cs . txt ")"
    commas = sepBy ", "
    sepBy s xs = case xs of
      [] -> id
      x : rest -> go Top x . foldr (\y r -> txt s . go Top y . r) id rest
    parens b q = if b then txt "(" . q . txt ")" else q
    txt t = (t :)
    prefixName n
      | isSym n.occ = txt (tick n <> "(" <> n.occ <> ")")
      | otherwise = txt (tick n <> n.occ)
    tick n = if n.promoted then "'" else ""

-- | An operator name, like @:+:@ or @~@ (not @(,)@ or @[]@).
isSym :: Text -> Bool
isSym t = case Text.uncons t of
  Just (c, _) -> not (isAlpha c || c `elem` ("_([\"'" :: String))
  Nothing -> False

-- * Orphans

-- | The names this module declares, for 'isOrphan'.
data Locals = Locals
  { -- | @Main@ when there's no @module@ line.
    self :: ModuleName,
    -- | Types, classes, families, synonyms.
    types :: Set Text,
    -- | Data constructors, for ticked names.
    constructors :: Set Text
  }

-- | See Note [Orphans]. The class (or family) and the head's arguments.
isOrphan :: Locals -> KName -> [KeyType] -> Bool
isOrphan locals cls args = not (any local (cls : foldr cons [] args))
  where
    local n =
      Set.member n.occ (if n.promoted then locals.constructors else locals.types)
        && maybe True (== locals.self) n.qualifier
    cons t rest = case t of
      KCon n -> n : rest
      _ -> foldr cons rest (children t)
