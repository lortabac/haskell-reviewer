{-# LANGUAGE StrictData #-}

-- | The free type variables of types, each at its first place, left to
-- right: GHC's extractHsTyRdrTyVars, which lives in the renamer. They
-- are a type's implicit binders (Note [Syntactic scoping] in
-- "HaskellReviewer.Core.Parser.ConvertOccurrences").
module HaskellReviewer.Core.Parser.FreeTyVars
  ( typeFree,
    headFree,
    famEqnFree,
    gadtFree,
    bndrKind,
  )
where

import GHC.Hs
import GHC.Types.Name.Occurrence
  ( OccSet,
    elemOccSet,
    emptyOccSet,
    extendOccSet,
    isTvOcc,
  )
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)

-- | The free type variables of a type.
typeFree :: LHsType GhcPs -> [LIdP GhcPs]
typeFree t = freeTyVars (fvType noBound noFree t)

-- | A declaration head's: those in its binders' kinds (each kind sees
-- the binders before it) and in the extra kinds (a result kind, which
-- sees all the binders).
headFree :: [LHsTyVarBndr flag GhcPs] -> [LHsType GhcPs] -> [LIdP GhcPs]
headFree bs kinds =
  let (bound, acc) = fvBndrs noBound noFree bs
   in freeTyVars (foldl' (fvType bound) acc kinds)

-- | A family equation's: those in its patterns, then the extra kinds.
famEqnFree :: HsFamEqnPats GhcPs -> [LHsType GhcPs] -> [LIdP GhcPs]
famEqnFree pats kinds =
  freeTyVars (foldl' (fvType noBound) (foldl' arg noFree pats) kinds)
  where
    arg :: FreeTvs -> LHsTypeArg GhcPs -> FreeTvs
    arg acc = \case
      HsValArg _ t -> fvType noBound acc t
      HsTypeArg _ k -> fvType noBound acc k
      HsArgPar _ -> acc

-- | A GADT constructor's, past its outer binders: those of its inner
-- foralls, context, arguments and result.
gadtFree ::
  [HsForAllTelescope GhcPs] ->
  Maybe (LHsContext GhcPs) ->
  HsConDeclGADTDetails GhcPs ->
  LHsType GhcPs ->
  [LIdP GhcPs]
gadtFree inner cxt args res =
  let (bound, acc) = foldl' (\(b, a) t -> fvTele b a t) (noBound, noFree) inner
      acc' = foldl' (fvType bound) acc (maybe [] unLoc cxt)
      acc'' = case args of
        PrefixConGADT _ fs -> foldl' (fvField bound) acc' fs
        RecConGADT _ (L _ rs) -> foldl' (fvRecField bound) acc' rs
   in freeTyVars (fvType bound acc'' res)

-- | A binder's kind, if written.
bndrKind :: HsTyVarBndr flag GhcPs -> Maybe (LHsType GhcPs)
bndrKind b = case hsBndrKind b of
  HsBndrKind _ k -> Just k
  HsBndrNoKind _ -> Nothing

-- * Internal

-- | Free type variables found so far.
data FreeTvs = FreeTvs
  { seen :: OccSet,
    -- | Each at its first place, newest first.
    found :: [LIdP GhcPs]
  }

noFree :: FreeTvs
noFree = FreeTvs {seen = emptyOccSet, found = []}

noBound :: OccSet
noBound = emptyOccSet

-- | Left to right.
freeTyVars :: FreeTvs -> [LIdP GhcPs]
freeTyVars acc = reverse acc.found

fvVar :: OccSet -> FreeTvs -> LIdP GhcPs -> FreeTvs
fvVar bound acc n@(L _ rdr)
  | isTvOcc occ,
    not (occ `elemOccSet` bound),
    not (occ `elemOccSet` acc.seen) =
      FreeTvs {seen = extendOccSet acc.seen occ, found = n : acc.found}
  | otherwise = acc
  where
    occ = rdrNameOcc rdr

-- | The free type variables of a type, not counting the bound ones.
fvType :: OccSet -> FreeTvs -> LHsType GhcPs -> FreeTvs
fvType bound acc (L _ ty) = case ty of
  HsForAllTy _ tele body ->
    let (bound', acc') = fvTele bound acc tele in fvType bound' acc' body
  HsQualTy _ (L _ cs) body -> go (foldl' go acc cs) body
  HsTyVar _ prom n -> case prom of
    NotPromoted -> fvVar bound acc n
    IsPromoted -> acc
  HsAppTy _ a b -> go (go acc a) b
  HsAppKindTy _ a k -> go (go acc a) k
  HsFunTy _ mult a b -> go (go (fvMult bound acc mult) a) b
  HsListTy _ a -> go acc a
  HsTupleTy _ _ ts -> foldl' go acc ts
  HsSumTy _ ts -> foldl' go acc ts
  HsOpTy _ prom a op b -> case prom of
    NotPromoted -> go (fvVar bound (go acc a) op) b
    IsPromoted -> go (go acc a) b
  HsParTy _ a -> go acc a
  HsIParamTy _ _ t -> go acc t
  HsStarTy _ _ -> acc
  HsKindSig _ a k -> go (go acc a) k
  -- What a splice makes is unknown.
  HsSpliceTy _ _ -> acc
  HsDocTy _ a _ -> go acc a
  HsExplicitListTy _ _ ts -> foldl' go acc ts
  HsExplicitTupleTy _ _ ts -> foldl' go acc ts
  HsTyLit _ _ -> acc
  HsWildCardTy _ -> acc
  XHsType ext -> case ext of
    HsCoreTy _ -> acc
    HsBangTy _ _ t -> go acc t
    HsRecTy _ fs -> foldl' (fvRecField bound) acc fs
  where
    go = fvType bound

fvMult :: OccSet -> FreeTvs -> HsMultAnn GhcPs -> FreeTvs
fvMult bound acc = \case
  HsUnannotated _ -> acc
  HsLinearAnn _ -> acc
  HsExplicitMult _ t -> fvType bound acc t

fvField :: OccSet -> FreeTvs -> HsConDeclField GhcPs -> FreeTvs
fvField bound acc = \case
  CDF {cdf_multiplicity = mult, cdf_type = t} -> fvType bound (fvMult bound acc mult) t

fvRecField :: OccSet -> FreeTvs -> LHsConDeclRecField GhcPs -> FreeTvs
fvRecField bound acc (L _ r) = case r of
  HsConDeclRecField _ _ spec -> fvField bound acc spec

-- | Binders in order: their kinds' free variables; returns the names
-- bound after them.
fvBndrs :: OccSet -> FreeTvs -> [LHsTyVarBndr flag GhcPs] -> (OccSet, FreeTvs)
fvBndrs bound0 acc0 = foldl' step (bound0, acc0)
  where
    step (bound, acc) lb@(L _ b) =
      let acc' = maybe acc (fvType bound acc) (bndrKind b)
          bound' =
            maybe bound (extendOccSet bound . rdrNameOcc . unLoc) (hsLTyVarLocName lb)
       in (bound', acc')

fvTele :: OccSet -> FreeTvs -> HsForAllTelescope GhcPs -> (OccSet, FreeTvs)
fvTele bound acc = fvBndrs bound acc . hsForAllTelescopeBndrs
