{-# LANGUAGE StrictData #-}

-- | From GHC's declarations to ours (SPEC §4.2, §4.3): types, classes,
-- values and pattern synonyms, with the things attached to them.
module HaskellReviewer.Core.Parser.ConvertDecls
  ( convertDecls,
  )
where

import Data.Foldable (toList)
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Hs (GhcPs)
import GHC.Hs qualified as Ghc
import GHC.Parser.Annotation (HasLoc)
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import HaskellReviewer.Core.Decls
  ( ArgType (..),
    ClassDecl (..),
    ConArg (..),
    ConArgs (..),
    ConDecl (..),
    DataBody (..),
    Decl (..),
    DeclInfo (..),
    Definition (..),
    DerivStrategy (..),
    DerivingClause (..),
    Extent (..),
    FieldDecl (..),
    Fixity (..),
    FixityDir (..),
    Located (..),
    MethodDecl (..),
    NewtypeBody (..),
    Part (..),
    PartRole (..),
    PatSynDecl (..),
    PatSynDir (..),
    RawDoc (..),
    Signature (..),
    Strictness (..),
    TypeDecl (..),
    TypeForm (..),
    Unpack (..),
    ValueDecl (..),
    declInfo,
    mkExtent,
  )
import HaskellReviewer.Core.ModuleModel (DocItem (..), Section (..))
import HaskellReviewer.Core.Names (DeclId (..), DeclNs (..))
import HaskellReviewer.Core.Parser.Helpers (SpanMap, locatedSpan, occText, rawDoc)
import HaskellReviewer.Core.Span (Span (..))
import Language.Haskell.Syntax.Basic qualified as Ghc (FixityDirection (..))
import Language.Haskell.Syntax.Basic qualified as GhcBasic (Fixity (..))

{- Note [Haddock pairing]

Doc comments are their own items in GHC's list of declarations. We pair
them like GHC (GHC.HsToCore.Docs): a @-- |@ comment goes with the next
item, a @-- ^@ comment with the one before. Headings (@-- *@) and named
chunks (@-- $x@) are items too, kept as DocItems.

At the top level GHC pairs only after dropping some kinds: fixities,
INLINE and other pragmas, kind signatures, role annotations, RULES,
WARNING, ANN and splices. A comment slides past them onto the next
declaration. Instances, deriving, @default@ and foreign declarations do
take comments (so those never slide onto a neighbour), even though they
are not in the model yet.

In a class body comments skip fixities, INLINE and friends, and
associated type defaults too. Then GHC pairs, and drops the comments of
everything but signatures and associated types. So a comment before a
default method equation, MINIMAL or a @default@ signature is lost (GHC
keys the last under @$dm...@, which Haddock never shows).
Checked with @ghc -haddock@ and @--show-iface@ (fixtures/decls/
haddock-pairing.hs).

The comments of a signature go to every name in it. Those of a pattern
binding go to its first name only (GHC's getMainDeclBinder). A
declaration keeps all its comments, in order.

Argument docs (@Int -- ^ arg@) are not the declaration's Haddock, but
they are parts of its extent.
-}

{- Note [Grouping]

A value's pieces can be far apart: a signature, equations (maybe split
up; GHC rejects that, we merge them), INLINE and other pragmas, a
fixity. We group them by name. A pragma or fixity with no local
declaration of that name is dropped. A signature with no binding is a
value with NoDefinition (as in a .hsig).

Types and classes are single items, so duplicates stay as two
declarations with the same id. Fixities and standalone kind signatures
attach by name: a fixity with @type@ only to types and classes, with
@data@ only to values, constructors, methods and pattern synonyms,
without either to all of them.

Instances, standalone deriving, family instances, foreign declarations,
role annotations, RULES, COMPLETE, ANN, @default@, DEPRECATED and
splices are not in the model: they are skipped.

Grouping is linear: lists are built by consing, and reversed once
(untrusted input can have thousands of equations of one name).
-}

-- * Items

-- | A doc comment, or something it can go with, or something comments
-- skip (Note [Haddock pairing]).
data Slot a = DocNext RawDoc | DocPrev RawDoc | Slot a | Unpaired a

-- | Something with the doc comments paired with it.
data WithDocs a = WithDocs
  { value :: a,
    docs :: [RawDoc]
  }

-- | One declaration of a list, before grouping. Spans are converted.
data Item
  = TypeItem TypeSyntax
  | ClassItem ClassSyntax
  | -- | A type signature, or a method's.
    SigItem SigSyntax
  | -- | @default m :: T@, in a class.
    DefaultSigItem SigSyntax
  | PatSynSigItem SigSyntax
  | BindItem (Located Text) Span
  | PatBindItem (NonEmpty (Located Text)) Span
  | PatSynItem PatSynDef
  | FixityDeclItem [Text] Namespace Fixity
  | -- | INLINE, SPECIALISE, OPAQUE, SCC.
    PragmaItem Text Span
  | KindSigItem Text Span
  | -- | Of an associated family, in a class.
    AssocDefaultItem Text Span
  | MinimalItem Span
  | DocOnlyItem DocItem
  | -- | Something not in the model (Note [Grouping]).
    Skipped

-- | A signature, the names in it, and the argument docs in its type.
data SigSyntax = SigSyntax
  { signature :: Signature,
    names :: NonEmpty (Located Text),
    argDocs :: [Span]
  }

-- | A pattern synonym's definition.
data PatSynDef = PatSynDef
  { name :: Located Text,
    span :: Span,
    direction :: PatSynDir,
    fields :: [Located Text]
  }

-- | The namespace a fixity declaration names, if any.
data Namespace = AnyNamespace | TypeNamespace | DataNamespace

-- | A fixity declaration, for one of its names.
data FixityItem = FixityItem
  { namespace :: Namespace,
    fixity :: Fixity
  }

-- | Where a name lives, for 'fixityFor'.
data Level = TypeLevel | DataLevel

-- | What declarations pick up by name from other items. See Note
-- [Grouping].
data Attached = Attached
  { fixities :: Map Text [FixityItem],
    kindSigs :: Map Text [Span],
    -- | Associated family defaults, in a class.
    defaults :: Map Text [Span]
  }

-- | Pair doc comments with items. See Note [Haddock pairing].
pairDocs :: [Slot a] -> [WithDocs a]
pairDocs = go [] Nothing
  where
    -- Comments in reverse.
    go docs prev = \case
      DocNext d : rest -> case prev of
        Nothing -> go (d : docs) Nothing rest
        Just p -> WithDocs p (reverse docs) : go [d] Nothing rest
      DocPrev d : rest -> go (d : docs) prev rest
      Slot x : rest -> case prev of
        Nothing -> go docs (Just x) rest
        Just p -> WithDocs p (reverse docs) : go [] (Just x) rest
      Unpaired x : rest -> WithDocs x [] : go docs prev rest
      [] -> case prev of
        Nothing -> []
        Just p -> [WithDocs p (reverse docs)]

-- * Top level

-- | The declarations, sorted by where they start, and the doc items. The
-- exported flags are all off: see "HaskellReviewer.Core.Exported".
convertDecls :: SpanMap -> [Ghc.LHsDecl GhcPs] -> ([Decl], [DocItem])
convertDecls spans ds =
  (sortOn (startOf . declInfo) decls, [d | WithDocs (DocOnlyItem d) _ <- paired])
  where
    paired = pairDocs (concatMap (topItem spans) ds)
    items = map (.value) paired
    at =
      Attached
        { fixities = fixityMap items,
          kindSigs = byName [(n, s) | KindSigItem n s <- items],
          defaults = Map.empty
        }
    decls =
      [TypeD (buildType spans at docs t) | WithDocs (TypeItem t) docs <- paired]
        <> [ClassD (buildClass spans at docs c) | WithDocs (ClassItem c) docs <- paired]
        <> mapMaybe (fmap ValueD . valueDecl at) (Map.toList (bitsMap paired))
        <> map PatSynD (patSynDecls at paired)

-- | The items of one top-level declaration.
topItem :: SpanMap -> Ghc.LHsDecl GhcPs -> [Slot Item]
topItem spans ld@(L _ d) = case d of
  Ghc.DocD _ doc -> docSlot spans s doc
  Ghc.TyClD _ t -> [Slot (tyClItem s t)]
  Ghc.ValD _ b -> [Slot (bindItem spans s b)]
  Ghc.SigD _ sig
    | userSig sig -> [Slot (sigItem spans s sig)]
    | otherwise -> [Unpaired (sigItem spans s sig)]
  Ghc.KindSigD _ (Ghc.StandaloneKindSig _ n _) -> [Unpaired (KindSigItem (occ n) s)]
  -- Not in the model, but they take comments.
  Ghc.InstD {} -> [Slot Skipped]
  Ghc.DerivD {} -> [Slot Skipped]
  Ghc.DefD {} -> [Slot Skipped]
  Ghc.ForD {} -> [Slot Skipped]
  -- Comments skip these.
  _ -> [Unpaired Skipped]
  where
    s = locatedSpan spans ld

-- | GHC's isUserSig: the signatures that take comments.
userSig :: Ghc.Sig GhcPs -> Bool
userSig = \case
  Ghc.TypeSig {} -> True
  Ghc.ClassOpSig {} -> True
  Ghc.PatSynSig {} -> True
  _ -> False

-- | A doc comment as items.
docSlot :: SpanMap -> Span -> Ghc.DocDecl GhcPs -> [Slot Item]
docSlot spans s = \case
  Ghc.DocCommentNext d -> [DocNext (rawDoc spans d)]
  Ghc.DocCommentPrev d -> [DocPrev (rawDoc spans d)]
  Ghc.DocCommentNamed n d ->
    let chunk = Located {span = s, value = Text.pack n}
     in [Slot (DocOnlyItem (DocChunk chunk (rawDoc spans d)))]
  Ghc.DocGroup level d ->
    [Slot (DocOnlyItem (DocHeading Section {level = level, heading = rawDoc spans d}))]

-- | A binding.
bindItem :: SpanMap -> Span -> Ghc.HsBind GhcPs -> Item
bindItem spans s = \case
  Ghc.FunBind {Ghc.fun_id = n} -> BindItem (name spans n) s
  Ghc.PatBind {Ghc.pat_lhs = p} -> case NonEmpty.nonEmpty (patBinders spans p) of
    Nothing -> Skipped
    Just ns -> PatBindItem ns s
  Ghc.PatSynBind _ psb -> case psb of
    Ghc.PSB {Ghc.psb_id = n, Ghc.psb_args = args, Ghc.psb_dir = dir} ->
      PatSynItem
        PatSynDef
          { name = name spans n,
            span = s,
            direction = case dir of
              Ghc.Unidirectional -> Unidirectional
              Ghc.ImplicitBidirectional -> ImplicitBidirectional
              Ghc.ExplicitBidirectional _ -> ExplicitBidirectional,
            fields = case args of
              Ghc.RecCon fs -> [name spans (fieldLabel f.recordPatSynField) | f <- fs]
              _ -> []
          }
  _ -> Skipped

-- | A signature or pragma.
sigItem :: SpanMap -> Span -> Ghc.Sig GhcPs -> Item
sigItem spans s = \case
  Ghc.TypeSig _ ns t -> maybe Skipped SigItem (sigSyntax ns (Ghc.hswc_body t))
  Ghc.ClassOpSig _ False ns t -> maybe Skipped SigItem (sigSyntax ns t)
  Ghc.ClassOpSig _ True ns t -> maybe Skipped DefaultSigItem (sigSyntax ns t)
  Ghc.PatSynSig _ ns t -> maybe Skipped PatSynSigItem (sigSyntax ns t)
  Ghc.FixSig _ (Ghc.FixitySig ns names (GhcBasic.Fixity prec dir)) ->
    FixityDeclItem
      (map occ names)
      ( case ns of
          Ghc.NoNamespaceSpecifier -> AnyNamespace
          Ghc.TypeNamespaceSpecifier _ -> TypeNamespace
          Ghc.DataNamespaceSpecifier _ -> DataNamespace
      )
      Fixity
        { span = s,
          precedence = prec,
          direction = case dir of
            Ghc.InfixL -> InfixLeft
            Ghc.InfixR -> InfixRight
            Ghc.InfixN -> InfixNone
        }
  Ghc.InlineSig _ n _ -> PragmaItem (occ n) s
  Ghc.SpecSig _ n _ _ -> PragmaItem (occ n) s
  Ghc.SpecSigE _ _ e _ -> maybe Skipped (`PragmaItem` s) (exprHead e)
  Ghc.SCCFunSig _ n _ -> PragmaItem (occ n) s
  Ghc.MinimalSig _ _ -> MinimalItem s
  _ -> Skipped
  where
    sigSyntax ns (L _ t) = case NonEmpty.nonEmpty ns of
      Nothing -> Nothing
      Just ns' ->
        Just
          SigSyntax
            { signature = Signature {span = s, names = NonEmpty.map occ ns'},
              names = NonEmpty.map (name spans) ns',
              argDocs = typeDocs spans (Ghc.sig_body t)
            }

-- | The spans of argument and result docs in a signature's type.
typeDocs :: SpanMap -> Ghc.LHsType GhcPs -> [Span]
typeDocs spans (L _ t) = case t of
  Ghc.HsForAllTy {Ghc.hst_body = b} -> typeDocs spans b
  Ghc.HsQualTy {Ghc.hst_body = b} -> typeDocs spans b
  Ghc.HsFunTy _ _ a b -> typeDocs spans a <> typeDocs spans b
  Ghc.HsDocTy _ a d -> typeDocs spans a <> [locatedSpan spans d]
  _ -> []

-- | The function in @{-# SPECIALISE f \@Int #-}@.
exprHead :: Ghc.LHsExpr GhcPs -> Maybe Text
exprHead (L _ e) = case e of
  Ghc.HsVar _ n -> Just (occ n)
  Ghc.HsApp _ f _ -> exprHead f
  Ghc.HsAppType _ f _ -> exprHead f
  Ghc.HsPar _ x -> exprHead x
  Ghc.ExprWithTySig _ x _ -> exprHead x
  _ -> Nothing

-- | The names a pattern binds, in order.
patBinders :: SpanMap -> Ghc.LPat GhcPs -> [Located Text]
patBinders spans = go
  where
    go (L _ p) = case p of
      Ghc.VarPat _ n -> [name spans n]
      Ghc.LazyPat _ q -> go q
      Ghc.AsPat _ n q -> name spans n : go q
      Ghc.ParPat _ q -> go q
      Ghc.BangPat _ q -> go q
      Ghc.ListPat _ qs -> concatMap go qs
      Ghc.TuplePat _ qs _ -> concatMap go qs
      Ghc.OrPat _ qs -> concatMap go (toList qs)
      Ghc.SumPat _ q _ _ -> go q
      Ghc.ConPat {Ghc.pat_args = args} -> case args of
        Ghc.PrefixCon qs -> concatMap go qs
        Ghc.InfixCon a b -> go a <> go b
        Ghc.RecCon (Ghc.HsRecFields _ fs _) -> concatMap (field . unLoc) fs
      Ghc.ViewPat _ _ q -> go q
      Ghc.SigPat _ q _ -> go q
      Ghc.NPlusKPat _ n _ _ _ _ -> [name spans n]
      _ -> []
    -- A pun binds the field's name.
    field f
      | f.hfbPun = [name spans (fieldLabel (unLoc f.hfbLHS))]
      | otherwise = go f.hfbRHS

-- * Types

-- | A type declaration, as GHC has it.
data TypeSyntax = TypeSyntax
  { span :: Span,
    name :: Ghc.LIdP GhcPs,
    shape :: TypeShape
  }

-- | What kind of type declaration.
data TypeShape
  = DataShape (Ghc.HsDataDefn GhcPs)
  | SynonymShape
  | FamilyShape (Ghc.FamilyInfo GhcPs)

-- | A type or class declaration.
tyClItem :: Span -> Ghc.TyClDecl GhcPs -> Item
tyClItem s = \case
  Ghc.FamDecl _ fam -> TypeItem (familySyntax s fam)
  Ghc.SynDecl {Ghc.tcdLName = n} -> TypeItem (TypeSyntax s n SynonymShape)
  Ghc.DataDecl {Ghc.tcdLName = n, Ghc.tcdDataDefn = defn} ->
    TypeItem (TypeSyntax s n (DataShape defn))
  Ghc.ClassDecl
    { Ghc.tcdCtxt = ctx,
      Ghc.tcdLName = n,
      Ghc.tcdFDs = fds,
      Ghc.tcdSigs = sigs,
      Ghc.tcdMeths = meths,
      Ghc.tcdATs = ats,
      Ghc.tcdATDefs = atDefs,
      Ghc.tcdDocs = docs
    } ->
      ClassItem
        ClassSyntax
          { span = s,
            context = ctx,
            name = n,
            fundeps = fds,
            sigs = sigs,
            meths = meths,
            ats = ats,
            atDefs = atDefs,
            docs = docs
          }

familySyntax :: Span -> Ghc.FamilyDecl GhcPs -> TypeSyntax
familySyntax s Ghc.FamilyDecl {Ghc.fdInfo = fi, Ghc.fdLName = n} =
  TypeSyntax s n (FamilyShape fi)

-- | A type, with what is attached to it: fixities (its own and its
-- constructors'), a kind signature, associated defaults.
buildType :: SpanMap -> Attached -> [RawDoc] -> TypeSyntax -> TypeDecl
buildType spans at docs t =
  TypeDecl
    { info =
        info
          (Named DType o)
          (locatedSpan spans t.name)
          ( Part t.span HeadPart
              : fixityParts fixity
                <> kindParts kind
                <> [Part d HeadPart | d <- defaults]
                -- Constructor docs can end after the type (a last @-- ^@).
                <> [p | c <- cons, p <- docParts c.info]
          )
          docs,
      form = form,
      kindSig = kind,
      fixity = fixity
    }
  where
    o = occ t.name
    fixity = fixityFor at.fixities TypeLevel o
    kind = safeHead (Map.findWithDefault [] o at.kindSigs)
    defaults = Map.findWithDefault [] o at.defaults
    conDecls = concatMap (conDecl spans at o) . toList
    derivs = map (derivingClause spans)
    form = case t.shape of
      SynonymShape -> Synonym
      FamilyShape Ghc.DataFamily -> DataFamily
      FamilyShape Ghc.OpenTypeFamily -> OpenFamily defaults
      FamilyShape (Ghc.ClosedTypeFamily eqs) ->
        ClosedFamily (maybe [] (map (locatedSpan spans)) eqs)
      DataShape Ghc.HsDataDefn {Ghc.dd_cons = cs, Ghc.dd_derivs = ds} -> case cs of
        -- GHC's parser takes one constructor here, but a GADT line can
        -- name several (the renamer rejects that): the first one.
        Ghc.NewTypeCon c -> case conDecls [c] of
          con : _ -> Newtype NewtypeBody {constructor = con, derivings = derivs ds}
          [] -> DataType DataBody {constructors = [], derivings = derivs ds}
        Ghc.DataTypeCons True xs ->
          TypeData DataBody {constructors = conDecls xs, derivings = derivs ds}
        Ghc.DataTypeCons False xs ->
          DataType DataBody {constructors = conDecls xs, derivings = derivs ds}
    cons = case form of
      DataType b -> b.constructors
      TypeData b -> b.constructors
      Newtype b -> [b.constructor]
      _ -> []

-- | The doc parts of a constructor and its fields.
docParts :: DeclInfo -> [Part]
docParts i = [p | p <- NonEmpty.toList i.extent.parts, p.role == DocPart]

-- | One constructor, or one per name for a GADT line.
conDecl :: SpanMap -> Attached -> Text -> Ghc.LConDecl GhcPs -> [ConDecl]
conDecl spans at parent lc@(L _ c) = case c of
  Ghc.ConDeclH98 {Ghc.con_name = n, Ghc.con_args = args, Ghc.con_doc = d} ->
    [ one n d [] $ case args of
        Ghc.PrefixCon as -> PrefixArgs (map conArg as)
        Ghc.InfixCon a b -> InfixArgs (conArg a) (conArg b)
        Ghc.RecCon (L _ fs) -> RecordArgs (concatMap field fs)
    ]
  Ghc.ConDeclGADT
    { Ghc.con_names = ns,
      Ghc.con_g_args = args,
      Ghc.con_res_ty = res,
      Ghc.con_doc = d
    } ->
      [ one n d (typeDocs spans res) $ case args of
          Ghc.PrefixConGADT _ as -> GadtArgs (map conArg as)
          Ghc.RecConGADT _ (L _ fs) -> GadtRecordArgs (concatMap field fs)
      | n <- toList ns
      ]
  where
    s = locatedSpan spans lc
    one n d resDocs args =
      let fixity = fixityFor at.fixities DataLevel (occ n)
          argDocs = case args of
            PrefixArgs as -> [x.span | a <- as, x <- maybeToList a.doc]
            InfixArgs a b -> [x.span | x <- maybeToList a.doc <> maybeToList b.doc]
            GadtArgs as -> [x.span | a <- as, x <- maybeToList a.doc]
            RecordArgs fs -> [p.span | f <- fs, p <- docParts f.info]
            GadtRecordArgs fs -> [p.span | f <- fs, p <- docParts f.info]
       in ConDecl
            { info =
                info
                  (Named DCon (occ n))
                  (locatedSpan spans n)
                  ( Part s HeadPart
                      : fixityParts fixity
                        <> [Part x DocPart | x <- argDocs <> resDocs]
                  )
                  (docs d),
              args = args,
              fixity = fixity
            }
    docs = maybe [] (pure . rawDoc spans)
    argType (f :: Ghc.HsConDeclField GhcPs) =
      ArgType
        { type_ = locatedSpan spans f.cdf_type,
          strictness = case f.cdf_bang of
            Ghc.SrcStrict -> Strict
            Ghc.SrcLazy -> Lazy
            Ghc.NoSrcStrict -> Unmarked,
          unpack = case f.cdf_unpack of
            Ghc.SrcUnpack -> Unpack
            Ghc.SrcNoUnpack -> NoUnpack
            Ghc.NoSrcUnpack -> NoUnpackPragma
        }
    conArg f = ConArg {type_ = argType f, doc = rawDoc spans <$> f.cdf_doc}
    field lf@(L _ (Ghc.HsConDeclRecField _ labels spec)) =
      [ FieldDecl
          { info =
              info
                (Field parent (occ label))
                (locatedSpan spans label)
                [Part (locatedSpan spans lf) HeadPart]
                (docs spec.cdf_doc),
            type_ = argType spec
          }
      | L _ fo <- labels,
        let label = fieldLabel fo
      ]

-- | A @deriving@ clause.
derivingClause :: SpanMap -> Ghc.LHsDerivingClause GhcPs -> DerivingClause
derivingClause spans lc@(L _ (Ghc.HsDerivingClause _ strategy tys)) =
  DerivingClause
    { span = locatedSpan spans lc,
      strategy = case unLoc <$> strategy of
        Nothing -> NoStrategy
        Just (Ghc.StockStrategy _) -> StockStrategy
        Just (Ghc.NewtypeStrategy _) -> NewtypeStrategy
        Just (Ghc.AnyclassStrategy _) -> AnyclassStrategy
        Just (Ghc.ViaStrategy (Ghc.XViaStrategyPs _ t)) ->
          ViaStrategy (locatedSpan spans t),
      classes = case unLoc tys of
        Ghc.DctSingle _ t -> [locatedSpan spans t]
        Ghc.DctMulti _ ts -> map (locatedSpan spans) ts
    }

-- * Classes

-- | A class, as GHC has it.
data ClassSyntax = ClassSyntax
  { span :: Span,
    context :: Maybe (Ghc.LHsContext GhcPs),
    name :: Ghc.LIdP GhcPs,
    fundeps :: [Ghc.LHsFunDep GhcPs],
    sigs :: [Ghc.LSig GhcPs],
    meths :: Ghc.LHsBinds GhcPs,
    ats :: [Ghc.LFamilyDecl GhcPs],
    atDefs :: [Ghc.LTyFamDefltDecl GhcPs],
    docs :: [Ghc.LDocDecl GhcPs]
  }

-- | A class, with its body paired and grouped like the top level (but
-- see Note [Haddock pairing]). Fixities in the body come before the top
-- level's.
buildClass :: SpanMap -> Attached -> [RawDoc] -> ClassSyntax -> ClassDecl
buildClass spans top docs c =
  ClassDecl
    { info =
        info
          (Named DClass o)
          (locatedSpan spans c.name)
          ( carve c.span (sortOn (.span) bodyParts)
              <> fixityParts fixity
              <> kindParts kind
              -- A last method's @-- ^@ can end after the class.
              <> [p | m <- methods, p <- docParts m.info]
          )
          docs,
      context = locatedSpan spans <$> c.context,
      fundeps = map (locatedSpan spans) c.fundeps,
      kindSig = kind,
      fixity = fixity,
      methods = methods,
      associated = [buildType spans at ds t | WithDocs (TypeItem t) ds <- kept],
      minimal = safeHead [m | MinimalItem m <- items]
    }
  where
    methods = sortOn (startOf . (.info)) (mapMaybe method (Map.toList (bitsMap kept)))
    o = occ c.name
    fixity = fixityFor top.fixities TypeLevel o
    kind = safeHead (Map.findWithDefault [] o top.kindSigs)
    slots =
      sortOn
        fst
        ( [withSpan l (classSig (locatedSpan spans l)) | l <- c.sigs]
            <> [withSpan l (Slot . bindItem spans (locatedSpan spans l)) | l <- c.meths]
            <> [withSpan l (Slot . TypeItem . familySyntax (here l)) | l <- c.ats]
            <> [withSpan l (Unpaired . assocDefault (here l)) | l <- c.atDefs]
            <> [ (locatedSpan spans l, slot)
               | l <- c.docs,
                 slot <- docSlot spans (locatedSpan spans l) (unLoc l)
               ]
        )
    -- Comments skip fixities and INLINE and friends here too, but not
    -- MINIMAL (checked with ghc -haddock).
    classSig sp sig = case sigItem spans sp sig of
      i@(FixityDeclItem {}) -> Unpaired i
      i@(PragmaItem {}) -> Unpaired i
      i -> Slot i
    here :: (HasLoc l) => GenLocated l a -> Span
    here = locatedSpan spans
    withSpan :: (HasLoc l) => GenLocated l a -> (a -> b) -> (Span, b)
    withSpan l f = (locatedSpan spans l, f (unLoc l))
    -- Only signatures and associated types keep their comments. GHC
    -- keys a default signature's under $dm..., so Haddock never shows
    -- them: dropped too.
    kept = map keepDocs (pairDocs (map snd slots))
    keepDocs w = case w.value of
      SigItem _ -> w
      TypeItem _ -> w
      other -> WithDocs other []
    items = map (.value) kept
    bodyParts =
      [Part e BodyPart | BindItem _ e <- items]
        <> [Part p PragmaPart | PragmaItem _ p <- items]
    at =
      Attached
        { fixities = Map.unionWith (<>) (fixityMap items) top.fixities,
          kindSigs = Map.empty,
          defaults = byName [(n, sp) | AssocDefaultItem n sp <- items]
        }
    defaultSigs = byName [(n.value, d) | DefaultSigItem d <- items, n <- toList d.names]
    assocDefault :: Span -> Ghc.TyFamInstDecl GhcPs -> Item
    assocDefault sp = \case
      Ghc.TyFamInstDecl {Ghc.tfid_eqn = Ghc.FamEqn {Ghc.feqn_tycon = n}} ->
        AssocDefaultItem (occ n) sp
    method (n, b) = case b.sigs of
      [] -> Nothing
      sig : _ ->
        let dsigs = Map.findWithDefault [] n defaultSigs
            mfix = fixityFor at.fixities DataLevel n
            parts =
              sigParts b.sigs
                <> sigParts dsigs
                <> [Part e BodyPart | e <- b.equations]
                <> [Part p PragmaPart | p <- b.pragmas]
                <> fixityParts mfix
         in Just
              MethodDecl
                { info =
                    info
                      (Named DVal n)
                      (minimum b.names)
                      parts
                      b.docs,
                  signature = sig.signature,
                  defaultSig = (.signature.span) <$> safeHead dsigs,
                  hasDefault = not (null b.equations),
                  fixity = mfix
                }

-- | The class's span with the default method equations and the method
-- pragmas cut out.
carve :: Span -> [Part] -> [Part]
carve outer = go (outer.startLine, outer.startCol)
  where
    go from = \case
      p : ps ->
        let to = (p.span.startLine, p.span.startCol)
         in [Part (spanFrom from to) HeadPart | from < to]
              <> [p]
              <> go (max from (p.span.endLine, p.span.endCol)) ps
      [] ->
        [ Part (spanFrom from (outer.endLine, outer.endCol)) HeadPart
        | from < (outer.endLine, outer.endCol)
        ]
    spanFrom (l1, c1) (l2, c2) = Span l1 c1 l2 c2

-- * Values

-- | What we know of one value-level name, from all its items. In
-- source order.
data Bits = Bits
  { names :: [Span],
    sigs :: [SigSyntax],
    equations :: [Span],
    patterns :: [Span],
    pragmas :: [Span],
    docs :: [RawDoc]
  }

instance Semigroup Bits where
  a <> b =
    Bits
      { names = a.names <> b.names,
        sigs = a.sigs <> b.sigs,
        equations = a.equations <> b.equations,
        patterns = a.patterns <> b.patterns,
        pragmas = a.pragmas <> b.pragmas,
        docs = a.docs <> b.docs
      }

noBits :: Bits
noBits =
  Bits {names = [], sigs = [], equations = [], patterns = [], pragmas = [], docs = []}

-- | Value names to their bits. See Note [Grouping]. Each name's bits
-- are joined with foldr, so the appends nest to the right.
bitsMap :: [WithDocs Item] -> Map Text Bits
bitsMap paired = Map.map (foldr (<>) noBits) (byName (concatMap bits paired))
  where
    bits (WithDocs item docs) = case item of
      SigItem sig ->
        [ (n.value, noBits {names = [n.span], sigs = [sig], docs = docs})
        | n <- toList sig.names
        ]
      BindItem n s -> [(n.value, noBits {names = [n.span], equations = [s], docs = docs})]
      PatBindItem (n :| ns) s ->
        -- The docs go to the first name only (Note [Haddock pairing]).
        (n.value, noBits {names = [n.span], patterns = [s], docs = docs})
          : [(m.value, noBits {names = [m.span], patterns = [s]}) | m <- ns]
      PragmaItem n s -> [(n, noBits {pragmas = [s], docs = docs})]
      _ -> []

-- | A value, if it has a signature or a binding.
valueDecl :: Attached -> (Text, Bits) -> Maybe ValueDecl
valueDecl at (n, b)
  | null b.names = Nothing
  | otherwise =
      Just
        ValueDecl
          { info =
              info
                (Named DVal n)
                (minimum b.names)
                (parts <> fixityParts fixity)
                b.docs,
            signature = (.signature) <$> safeHead b.sigs,
            definition = definition,
            fixity = fixity
          }
  where
    fixity = fixityFor at.fixities DataLevel n
    definition
      | not (null b.equations) = Equations
      | not (null b.patterns) = PatternBound
      | otherwise = NoDefinition
    parts =
      sigParts b.sigs
        <> [Part e BodyPart | e <- b.equations <> b.patterns]
        <> [Part p PragmaPart | p <- b.pragmas]

-- | Signatures as head parts, with their argument docs.
sigParts :: [SigSyntax] -> [Part]
sigParts sigs =
  [Part s.signature.span HeadPart | s <- sigs]
    <> [Part d DocPart | s <- sigs, d <- s.argDocs]

-- | Pattern synonyms, with their signatures. A signature with no
-- definition is dropped.
patSynDecls :: Attached -> [WithDocs Item] -> [PatSynDecl]
patSynDecls at paired =
  [ PatSynDecl
      { info =
          info
            (Named DPat n)
            (minimum (def.name.span : map (.value.span) sigs))
            ( sigParts [x.value.sig | x <- sigs]
                <> [Part d.value.span BodyPart | d <- defs]
                <> fixityParts fixity
            )
            (concatMap (.docs) sigs <> concatMap (.docs) defs),
        signature = (.value.sig.signature) <$> safeHead sigs,
        direction = def.direction,
        fields =
          [info (Field n f.value) f.span [Part f.span HeadPart] [] | f <- def.fields],
        fixity = fixity
      }
  | (n, defs@(WithDocs def _ : _)) <- Map.toList defsByName,
    let sigs = Map.findWithDefault [] n sigsByName
        fixity = fixityFor at.fixities DataLevel n
  ]
  where
    defsByName =
      byName [(d.name.value, WithDocs d docs) | WithDocs (PatSynItem d) docs <- paired]
    sigsByName =
      byName
        [ (x.value, WithDocs PatSynSig {sig = sig, span = x.span} docs)
        | WithDocs (PatSynSigItem sig) docs <- paired,
          x <- toList sig.names
        ]

-- | A pattern synonym signature, for one of its names.
data PatSynSig = PatSynSig
  { sig :: SigSyntax,
    -- | Of the name.
    span :: Span
  }

-- * Fixities

-- | Fixities by name, in order.
fixityMap :: [Item] -> Map Text [FixityItem]
fixityMap items =
  byName
    [ (n, FixityItem {namespace = ns, fixity = f})
    | FixityDeclItem names ns f <- items,
      n <- names
    ]

-- | The first fixity for a name at a level.
fixityFor :: Map Text [FixityItem] -> Level -> Text -> Maybe Fixity
fixityFor fs level n = (.fixity) <$> find at (Map.findWithDefault [] n fs)
  where
    at f = case (f.namespace, level) of
      (AnyNamespace, _) -> True
      (TypeNamespace, TypeLevel) -> True
      (DataNamespace, DataLevel) -> True
      _ -> False

fixityParts :: Maybe Fixity -> [Part]
fixityParts f = [Part x.span HeadPart | x <- toList f]

kindParts :: Maybe Span -> [Part]
kindParts k = [Part x HeadPart | x <- toList k]

-- * Helpers

-- | A declaration's info. The doc comments become parts too.
-- @exported@ is False; "HaskellReviewer.Core.Exported" sets it.
info :: DeclId -> Span -> [Part] -> [RawDoc] -> DeclInfo
info i n parts docs =
  DeclInfo
    { id = i,
      nameSpan = n,
      extent =
        mkExtent $
          fromMaybe
            (Part n HeadPart :| [])
            (NonEmpty.nonEmpty (parts <> [Part d.span DocPart | d <- docs])),
      haddock = sortOn (.span) docs,
      exported = False
    }

-- | Where a declaration starts; ties (a shared signature) by name.
startOf :: DeclInfo -> (Span, Span)
startOf i = ((NonEmpty.head i.extent.parts).span, i.nameSpan)

name :: SpanMap -> Ghc.LIdP GhcPs -> Located Text
name spans n = Located {span = locatedSpan spans n, value = occ n}

occ :: GenLocated l RdrName -> Text
occ = occText . unLoc

fieldLabel :: Ghc.FieldOcc GhcPs -> Ghc.LIdP GhcPs
fieldLabel (Ghc.FieldOcc _ l) = l

-- | Group by name, in order. Linear: consed, then reversed once.
byName :: [(Text, a)] -> Map Text [a]
byName xs = Map.map reverse (Map.fromListWith (<>) [(n, [x]) | (n, x) <- xs])

safeHead :: [a] -> Maybe a
safeHead = \case
  x : _ -> Just x
  [] -> Nothing
