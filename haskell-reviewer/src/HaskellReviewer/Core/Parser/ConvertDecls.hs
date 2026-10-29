{-# LANGUAGE StrictData #-}

-- | From GHC's declarations to ours (SPEC §4.2, §4.3): types, classes,
-- values, pattern synonyms, instances, with the things attached to
-- them, and the Pragmas block.
module HaskellReviewer.Core.Parser.ConvertDecls
  ( Converted (..),
    convertDecls,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Data.FastString (unpackFS)
import GHC.Hs (GhcPs)
import GHC.Hs qualified as Ghc
import GHC.Parser.Annotation (HasLoc)
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import HaskellReviewer.Core.Decls
  ( AnnTarget (..),
    ArgType (..),
    ClassDecl (..),
    ConArg (..),
    ConArgs (..),
    ConDecl (..),
    DataBody (..),
    Decl (..),
    DeclInfo (..),
    Definition (..),
    DerivStrategy (..),
    Derived (..),
    DerivedClass (..),
    DerivingClause (..),
    DocComment (..),
    Extent (..),
    FamInstDecl (..),
    FamInstForm (..),
    FieldDecl (..),
    Fixity (..),
    FixityDir (..),
    InstanceBody (..),
    InstanceDecl (..),
    InstanceForm (..),
    InstanceMethod (..),
    Located (..),
    MethodDecl (..),
    ModulePragma (..),
    NewtypeBody (..),
    OpenFamilyBody (..),
    Part (..),
    PartRole (..),
    PatSynDecl (..),
    PatSynDir (..),
    PragmaKind (..),
    Signature (..),
    Strictness (..),
    TypeDecl (..),
    TypeForm (..),
    Unpack (..),
    ValueDecl (..),
    WarningText (..),
    declExtent,
    declInfo,
    famInstConstructors,
    formConstructors,
    mkExtent,
  )
import HaskellReviewer.Core.ModuleModel (DocItem (..), Section (..))
import HaskellReviewer.Core.Names
  ( DeclId (..),
    DeclNs (..),
    InstanceKey (..),
    declName,
    keyHead,
  )
import HaskellReviewer.Core.Parser.Helpers
  ( SpanMap,
    docComment,
    headingComment,
    locatedSpan,
    occText,
    spanOf,
    warningText,
  )
import HaskellReviewer.Core.Parser.InstanceKey
  ( Derivable,
    Head (..),
    KName (..),
    KeyType,
    Locals (..),
    binderParams,
    derivable,
    derivedKey,
    instanceHead,
    instanceKey,
    isOrphan,
    localMentions,
    plainName,
    renderArgs,
    typeArgs,
  )
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RichText (Doc)
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
declaration. Instances, deriving, type and data instances, @default@
and foreign declarations do take comments, so those never slide onto
a neighbour. GHC keeps them for instances, standalone deriving, type
and data instances and foreign imports, and drops them for foreign
exports and @default@.

In an instance body GHC drops every doc comment: on methods, on
associated instances, even on their constructors and fields.

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
declarations with the same id. Fixities, standalone kind signatures,
role annotations and DEPRECATED/WARNING pragmas attach by name: a
fixity or warning with @type@ only to types and classes, with @data@
only to values, constructors, fields, methods and pattern synonyms,
without either to all of them. When one such pragma has several
entries, each name's warning part is only its own @name "msg"@: the
@{-#@ and @#-}@ belong to no declaration. A foreign import is a value;
a foreign export attaches to its value.

Type and data instances are declarations of their own, and their ids
also go on the family, when it's here. Each class in a deriving clause
is an instance too (Note [Instance keys] in
"HaskellReviewer.Core.Parser.InstanceKey"). RULES, COMPLETE, ANN and
@default@ go to the Pragmas block. A splice is a declaration without a
name.

Grouping is linear: lists are built by consing, and reversed once
(untrusted input can have thousands of equations of one name).
-}

-- * Items

-- | A doc comment, or something it can go with, or something comments
-- skip (Note [Haddock pairing]).
data Slot a = DocNext (DocComment Doc) | DocPrev (DocComment Doc) | Slot a | Unpaired a

-- | Something with the doc comments paired with it.
data WithDocs a = WithDocs
  { value :: a,
    docs :: [DocComment Doc]
  }

-- | One declaration of a list, before grouping. Spans are converted.
data Item
  = TypeItem TypeSyntax
  | ClassItem ClassSyntax
  | InstItem InstSyntax
  | DerivItem DerivSyntax
  | FamInstItem FamInstSyntax
  | ForeignImportItem SigSyntax
  | ForeignExportItem Text Span
  | RolesItem Text Span
  | WarningItem [Text] Namespace WarningText
  | PragmaDeclItem ModulePragma
  | SpliceItem Span
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

-- | A fixity or warning, for one of its names.
data Scoped a = Scoped
  { namespace :: Namespace,
    value :: a
  }

-- | Where a name lives, for 'scopedFor'.
data Level = TypeLevel | DataLevel

-- | What declarations pick up by name from other items. See Note
-- [Grouping].
data Attached = Attached
  { fixities :: Map Text [Scoped Fixity],
    warnings :: Map Text [Scoped WarningText],
    kindSigs :: Map Text [Span],
    roleAnnots :: Map Text [Span],
    -- | Associated family defaults, in a class.
    defaults :: Map Text [Span],
    -- | Family instance ids by family.
    famInstances :: Map Text [DeclId],
    locals :: Locals
  }

-- | Whether doc comments count here. GHC drops them in instance bodies
-- (Note [Haddock pairing]).
data DocsHere = KeepDocs | DropDocs

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

-- | What 'convertDecls' gives.
data Converted = Converted
  { -- | Sorted by where they start. The exported flags are all off:
    -- see "HaskellReviewer.Core.Exported".
    decls :: [Decl],
    docItems :: [DocItem],
    -- | In source order.
    pragmas :: [ModulePragma]
  }

-- | Our declarations, doc items and Pragmas block, for a module with
-- this name (for orphans).
convertDecls :: SpanMap -> Maybe ModuleName -> [Ghc.LHsDecl GhcPs] -> Converted
convertDecls spans self ds =
  Converted
    { decls = sortOn declStart decls,
      docItems = [d | WithDocs (DocOnlyItem d) _ <- paired],
      pragmas = [p | PragmaDeclItem p <- items]
    }
  where
    paired = pairDocs (concatMap (topItem spans) ds)
    items = map (.value) paired
    famInsts = [f | FamInstItem f <- items] <> [f | InstItem i <- items, f <- i.famInsts]
    at =
      Attached
        { fixities = fixityMap items,
          warnings =
            scopedMap [(n, Scoped ns w) | WarningItem names ns w <- items, n <- names],
          kindSigs = byName [(n, s) | KindSigItem n s <- items],
          roleAnnots = byName [(n, s) | RolesItem n s <- items],
          defaults = Map.empty,
          famInstances = byName [(f.familyName.occ, Inst f.key) | f <- famInsts],
          locals =
            Locals
              { self = fromMaybe (ModuleName "Main") self,
                types = Set.fromList (concatMap localTypes items),
                constructors = Set.fromList (concatMap localCons items)
              }
        }
    built =
      [TypeD (buildType spans at docs t) | WithDocs (TypeItem t) docs <- paired]
        <> [ClassD (buildClass spans at docs c) | WithDocs (ClassItem c) docs <- paired]
        <> mapMaybe (fmap ValueD . valueDecl at) (Map.toList (bitsMap paired))
        <> map PatSynD (patSynDecls at paired)
        <> [ InstanceD (buildInstance spans at docs i)
           | WithDocs (InstItem i) docs <- paired
           ]
        <> [ InstanceD (buildDeriving spans at docs d)
           | WithDocs (DerivItem d) docs <- paired
           ]
        <> [ FamInstD (buildFamInst spans at KeepDocs docs f)
           | WithDocs (FamInstItem f) docs <- paired
           ]
    decls =
      built
        <> map InstanceD (concatMap (derivedInstances at.locals) built)
        <> [SpliceD (mkExtent (Part s HeadPart :| [])) | SpliceItem s <- items]

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
  Ghc.InstD _ i -> [Slot (instItem spans s i)]
  Ghc.DerivD _ x -> [Slot (DerivItem DerivSyntax {span = s, decl = x})]
  Ghc.DefD _ x -> [Slot (PragmaDeclItem (defaultPragma s x))]
  Ghc.ForD _ f -> [Slot (foreignItem spans s f)]
  -- Comments skip these.
  Ghc.RoleAnnotD _ (Ghc.RoleAnnotDecl _ n _) -> [Unpaired (RolesItem (occ n) s)]
  Ghc.WarningD _ (Ghc.Warnings _ ws) ->
    -- With several in one pragma, each has its own span.
    let at w = if length ws > 1 then locatedSpan spans w else s
     in [Unpaired (warnItem (at w) (unLoc w)) | w <- ws]
  Ghc.RuleD _ (Ghc.HsRules _ rs) ->
    [Unpaired (PragmaDeclItem ModulePragma {span = s, kind = Rules (map ruleName rs)})]
  Ghc.AnnD _ (Ghc.HsAnnotation _ target _) ->
    [ Unpaired
        (PragmaDeclItem ModulePragma {span = s, kind = Annotation (annTarget target)})
    ]
  Ghc.SpliceD {} -> [Unpaired (SpliceItem s)]
  where
    s = locatedSpan spans ld
    ruleName :: Ghc.LRuleDecl GhcPs -> Text
    ruleName (L _ r) = case r of
      Ghc.HsRule {Ghc.rd_name = L _ n} -> Text.pack (unpackFS n)
    annTarget = \case
      Ghc.ValueAnnProvenance n -> AnnValue (occ n)
      Ghc.TypeAnnProvenance n -> AnnType (occ n)
      Ghc.ModuleAnnProvenance -> AnnModule

-- | @default (Int)@.
defaultPragma :: Span -> Ghc.DefaultDecl GhcPs -> ModulePragma
defaultPragma s = \case
  Ghc.DefaultDecl {Ghc.defd_class = c} ->
    ModulePragma {span = s, kind = Default (occ <$> c)}

-- | One name list of a @DEPRECATED@ or @WARNING@ pragma.
warnItem :: Span -> Ghc.WarnDecl GhcPs -> Item
warnItem s = \case
  Ghc.Warning (ns, _) names txt ->
    WarningItem (map occ names) (namespace ns) (warningText s txt)

-- | A foreign import is a value, its whole declaration the signature.
foreignItem :: SpanMap -> Span -> Ghc.ForeignDecl GhcPs -> Item
foreignItem spans s = \case
  Ghc.ForeignImport {Ghc.fd_name = n, Ghc.fd_sig_ty = L _ t} ->
    ForeignImportItem
      SigSyntax
        { signature = Signature {span = s, names = occ n :| []},
          names = name spans n :| [],
          argDocs = typeDocs spans (Ghc.sig_body t)
        }
  Ghc.ForeignExport {Ghc.fd_name = n} -> ForeignExportItem (occ n) s

-- | The types, classes, families and synonyms a declaration adds to
-- this module, for orphans (Note [Orphans] in
-- "HaskellReviewer.Core.Parser.InstanceKey").
localTypes :: Item -> [Text]
localTypes = \case
  -- @type data@ constructors are types.
  TypeItem TypeSyntax {name = n, shape = DataShape defn}
    | Ghc.DataTypeCons True _ <- Ghc.dd_cons defn -> occ n : defnCons defn
  TypeItem t -> [occ t.name]
  ClassItem c -> occ c.name : [occ (Ghc.fdLName f) | L _ f <- c.ats]
  _ -> []

-- | Like 'localTypes', for data constructors.
localCons :: Item -> [Text]
localCons = \case
  TypeItem TypeSyntax {shape = DataShape defn} -> defnCons defn
  FamInstItem f -> rhsCons f.rhs
  InstItem i -> concatMap (rhsCons . (.rhs)) i.famInsts
  _ -> []
  where
    rhsCons = \case
      DataRhs defn -> defnCons defn
      TypeRhs -> []

-- | The constructor names of a data declaration or instance.
defnCons :: Ghc.HsDataDefn GhcPs -> [Text]
defnCons defn = map occ (concatMap conNames (toList (Ghc.dd_cons defn)))

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
  Ghc.DocCommentNext d -> [DocNext (docComment spans d)]
  Ghc.DocCommentPrev d -> [DocPrev (docComment spans d)]
  Ghc.DocCommentNamed n d ->
    let chunk = Located {span = s, value = Text.pack n}
     in [Slot (DocOnlyItem (DocChunk chunk (docComment spans d)))]
  Ghc.DocGroup level d ->
    let heading = headingComment spans d
     in [Slot (DocOnlyItem (DocHeading Section {level = level, heading = heading}))]

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
      (namespace ns)
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
  Ghc.CompleteMatchSig _ ns ty ->
    PragmaDeclItem ModulePragma {span = s, kind = Complete (map occ ns) (occ <$> ty)}
  _ -> Skipped
  where
    sigSyntax :: [Ghc.LIdP GhcPs] -> Ghc.LHsSigType GhcPs -> Maybe SigSyntax
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
    -- | The visible parameters, for deriving clauses.
    params :: [KeyType],
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
  Ghc.SynDecl {Ghc.tcdLName = n} -> TypeItem (TypeSyntax s n [] SynonymShape)
  Ghc.DataDecl {Ghc.tcdLName = n, Ghc.tcdTyVars = vs, Ghc.tcdDataDefn = defn} ->
    TypeItem (TypeSyntax s n (binderParams (Ghc.hsQTvExplicit vs)) (DataShape defn))
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
  TypeSyntax s n [] (FamilyShape fi)

-- | A type, with what is attached to it: fixities (its own and its
-- constructors'), a kind signature, roles, a warning, associated
-- defaults, family instances.
buildType :: SpanMap -> Attached -> [DocComment Doc] -> TypeSyntax -> TypeDecl
buildType spans at docs t =
  TypeDecl
    { info =
        info
          NewInfo
            { id = Named DType o,
              nameSpan = locatedSpan spans t.name,
              parts =
                Part t.span HeadPart
                  : fixityParts fixity
                    <> headParts kind
                    <> headParts roles
                    <> [Part d HeadPart | d <- defaults]
                    -- Constructor docs can end after the type (a last @-- ^@).
                    <> [p | c <- formConstructors form, p <- docParts c.info],
              docs = docs,
              warning = scopedFor at.warnings TypeLevel o
            },
      form = form,
      kindSig = kind,
      roles = roles,
      fixity = fixity
    }
  where
    o = occ t.name
    fixity = scopedFor at.fixities TypeLevel o
    kind = safeHead (Map.findWithDefault [] o at.kindSigs)
    roles = safeHead (Map.findWithDefault [] o at.roleAnnots)
    defaults = Map.findWithDefault [] o at.defaults
    instances = Map.findWithDefault [] o at.famInstances
    form = case t.shape of
      SynonymShape -> Synonym
      FamilyShape Ghc.DataFamily -> DataFamily instances
      FamilyShape Ghc.OpenTypeFamily ->
        OpenFamily OpenFamilyBody {defaults = defaults, instances = instances}
      FamilyShape (Ghc.ClosedTypeFamily eqs) ->
        ClosedFamily (maybe [] (map (locatedSpan spans)) eqs)
      DataShape defn -> case dataDefn spans at KeepDocs (derivable o t.params) o defn of
        NewtypeDefn b -> Newtype b
        DataDefn b -> DataType b
        TypeDataDefn b -> TypeData b

-- | What a data declaration or instance has.
data Defn = NewtypeDefn NewtypeBody | DataDefn DataBody | TypeDataDefn DataBody

-- | The constructors and deriving clauses of a data declaration or data
-- instance. Fields get the parent given.
dataDefn ::
  SpanMap -> Attached -> DocsHere -> Derivable -> Text -> Ghc.HsDataDefn GhcPs -> Defn
dataDefn spans at here ty parent defn = case cs of
  -- GHC's parser takes one constructor here, but a GADT line can
  -- name several (the renamer rejects that): the first one.
  Ghc.NewTypeCon c -> case conDecls [c] of
    con : _ -> NewtypeDefn NewtypeBody {constructor = con, derivings = derivs}
    [] -> DataDefn DataBody {constructors = [], derivings = derivs}
  Ghc.DataTypeCons True xs ->
    TypeDataDefn DataBody {constructors = conDecls xs, derivings = derivs}
  Ghc.DataTypeCons False xs ->
    DataDefn DataBody {constructors = conDecls xs, derivings = derivs}
  where
    cs = Ghc.dd_cons defn
    conDecls = concatMap (conDecl spans at here parent) . toList
    derivs = map (derivingClause spans ty) (Ghc.dd_derivs defn)

-- | The names of a constructor declaration (a GADT line can have
-- several).
conNames :: Ghc.LConDecl GhcPs -> [Ghc.LIdP GhcPs]
conNames (L _ c) = case c of
  Ghc.ConDeclH98 {Ghc.con_name = n} -> [n]
  Ghc.ConDeclGADT {Ghc.con_names = ns} -> toList ns

-- | The doc parts of a constructor and its fields.
docParts :: DeclInfo -> [Part]
docParts i = [p | p <- NonEmpty.toList i.extent.parts, p.role == DocPart]

-- | One constructor, or one per name for a GADT line.
conDecl :: SpanMap -> Attached -> DocsHere -> Text -> Ghc.LConDecl GhcPs -> [ConDecl]
conDecl spans at here parent lc@(L _ c) = case c of
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
      let fixity = scopedFor at.fixities DataLevel (occ n)
          argDocs = case args of
            PrefixArgs as -> [x.span | a <- as, x <- maybeToList a.doc]
            InfixArgs a b -> [x.span | x <- maybeToList a.doc <> maybeToList b.doc]
            GadtArgs as -> [x.span | a <- as, x <- maybeToList a.doc]
            RecordArgs fs -> [p.span | f <- fs, p <- docParts f.info]
            GadtRecordArgs fs -> [p.span | f <- fs, p <- docParts f.info]
       in ConDecl
            { info =
                info
                  NewInfo
                    { id = Named DCon (occ n),
                      nameSpan = locatedSpan spans n,
                      parts =
                        Part s HeadPart
                          : fixityParts fixity
                            <> [Part x DocPart | x <- argDocs <> resDocs],
                      docs = docs d,
                      warning = scopedFor at.warnings DataLevel (occ n)
                    },
              args = args,
              fixity = fixity
            }
    docs d = maybeToList (doc d)
    doc :: Maybe (Ghc.LHsDoc GhcPs) -> Maybe (DocComment Doc)
    doc d = case here of
      KeepDocs -> docComment spans <$> d
      DropDocs -> Nothing
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
    conArg f = ConArg {type_ = argType f, doc = doc f.cdf_doc}
    field lf@(L _ (Ghc.HsConDeclRecField _ labels spec)) =
      [ FieldDecl
          { info =
              info
                NewInfo
                  { id = Field parent (occ label),
                    nameSpan = locatedSpan spans label,
                    parts = [Part (locatedSpan spans lf) HeadPart],
                    docs = docs spec.cdf_doc,
                    warning = scopedFor at.warnings DataLevel (occ label)
                  },
            type_ = argType spec
          }
      | L _ fo <- labels,
        let label = fieldLabel fo
      ]

-- | A @deriving@ clause on this type, each class with its instance
-- key.
derivingClause :: SpanMap -> Derivable -> Ghc.LHsDerivingClause GhcPs -> DerivingClause
derivingClause spans ty lc@(L _ (Ghc.HsDerivingClause _ strategy tys)) =
  DerivingClause
    { span = locatedSpan spans lc,
      strategy = strategyOf spans strategy,
      strategySpan = locatedSpan spans <$> strategy,
      classes =
        [ DerivedClass
            { span = locatedSpan spans t,
              id = Inst (derivedKey ty (instanceHead t))
            }
        | t <- case unLoc tys of
            Ghc.DctSingle _ t -> [t]
            Ghc.DctMulti _ ts -> ts
        ]
    }

-- | A deriving strategy, if written.
strategyOf :: SpanMap -> Maybe (Ghc.LDerivStrategy GhcPs) -> DerivStrategy
strategyOf spans strategy = case unLoc <$> strategy of
  Nothing -> NoStrategy
  Just (Ghc.StockStrategy _) -> StockStrategy
  Just (Ghc.NewtypeStrategy _) -> NewtypeStrategy
  Just (Ghc.AnyclassStrategy _) -> AnyclassStrategy
  Just (Ghc.ViaStrategy (Ghc.XViaStrategyPs _ t)) -> ViaStrategy (locatedSpan spans t)

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
buildClass :: SpanMap -> Attached -> [DocComment Doc] -> ClassSyntax -> ClassDecl
buildClass spans top docs c =
  ClassDecl
    { info =
        info
          NewInfo
            { id = Named DClass o,
              nameSpan = locatedSpan spans c.name,
              parts =
                carve c.span (sortOn (.span) (bodyParts items))
                  <> fixityParts fixity
                  <> headParts kind
                  <> headParts roles
                  -- A last method's @-- ^@ can end after the class.
                  <> [p | m <- methods, p <- docParts m.info],
              docs = docs,
              warning = scopedFor top.warnings TypeLevel o
            },
      context = locatedSpan spans <$> c.context,
      fundeps = map (locatedSpan spans) c.fundeps,
      kindSig = kind,
      roles = roles,
      fixity = fixity,
      methods = methods,
      associated = [buildType spans at ds t | WithDocs (TypeItem t) ds <- kept],
      minimal = safeHead [m | MinimalItem m <- items]
    }
  where
    methods = sortOn (infoStart . (.info)) (mapMaybe method (Map.toList (bitsMap kept)))
    o = occ c.name
    fixity = scopedFor top.fixities TypeLevel o
    kind = safeHead (Map.findWithDefault [] o top.kindSigs)
    roles = safeHead (Map.findWithDefault [] o top.roleAnnots)
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
    at =
      Attached
        { fixities =
            Map.unionWith
              (<>)
              (fixityMap items)
              top.fixities,
          warnings = top.warnings,
          kindSigs = Map.empty,
          roleAnnots = Map.empty,
          defaults = byName [(n, sp) | AssocDefaultItem n sp <- items],
          famInstances = top.famInstances,
          locals = top.locals
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
            mfix = scopedFor at.fixities DataLevel n
         in Just
              MethodDecl
                { info =
                    info
                      NewInfo
                        { id = Named DVal n,
                          nameSpan = minimum b.names,
                          parts = bitsParts b <> sigParts dsigs <> fixityParts mfix,
                          docs = b.docs,
                          warning = scopedFor at.warnings DataLevel n
                        },
                  signature = sig.signature,
                  defaultSig = (.signature.span) <$> safeHead dsigs,
                  hasDefault = not (null b.equations),
                  fixity = mfix
                }

-- | The method equations and pragmas of a class or instance body: what
-- 'carve' cuts out of its head.
bodyParts :: [Item] -> [Part]
bodyParts items =
  [Part e BodyPart | BindItem _ e <- items]
    <> [Part p PragmaPart | PragmaItem _ p <- items]

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
    ffiImports :: [Span],
    ffiExports :: [Span],
    pragmas :: [Span],
    docs :: [DocComment Doc]
  }

instance Semigroup Bits where
  a <> b =
    Bits
      { names = a.names <> b.names,
        sigs = a.sigs <> b.sigs,
        equations = a.equations <> b.equations,
        patterns = a.patterns <> b.patterns,
        ffiImports = a.ffiImports <> b.ffiImports,
        ffiExports = a.ffiExports <> b.ffiExports,
        pragmas = a.pragmas <> b.pragmas,
        docs = a.docs <> b.docs
      }

noBits :: Bits
noBits =
  Bits
    { names = [],
      sigs = [],
      equations = [],
      patterns = [],
      ffiImports = [],
      ffiExports = [],
      pragmas = [],
      docs = []
    }

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
      ForeignImportItem sig ->
        [ ( n.value,
            noBits
              { names = [n.span],
                sigs = [sig],
                ffiImports = [sig.signature.span],
                docs = docs
              }
          )
        | n <- toList sig.names
        ]
      -- GHC drops a foreign export's docs.
      ForeignExportItem n s -> [(n, noBits {ffiExports = [s]})]
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
                NewInfo
                  { id = Named DVal n,
                    nameSpan = minimum b.names,
                    parts = bitsParts b <> fixityParts fixity,
                    docs = b.docs,
                    warning = scopedFor at.warnings DataLevel n
                  },
            signature = (.signature) <$> safeHead b.sigs,
            definition = definition,
            foreignExports = b.ffiExports,
            fixity = fixity
          }
  where
    fixity = scopedFor at.fixities DataLevel n
    definition
      | not (null b.equations) = Equations
      | not (null b.patterns) = PatternBound
      | not (null b.ffiImports) = ForeignImport
      | otherwise = NoDefinition

-- | The parts a value-level name's bits give: signatures head,
-- equations body, pragmas pragma, foreign exports head.
bitsParts :: Bits -> [Part]
bitsParts b =
  sigParts b.sigs
    <> [Part e BodyPart | e <- b.equations <> b.patterns]
    <> [Part p PragmaPart | p <- b.pragmas]
    <> headParts b.ffiExports

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
            NewInfo
              { id = Named DPat n,
                nameSpan = minimum (def.name.span : map (.value.span) sigs),
                parts =
                  sigParts [x.value.sig | x <- sigs]
                    <> [Part d.value.span BodyPart | d <- defs]
                    <> fixityParts fixity,
                docs = concatMap (.docs) sigs <> concatMap (.docs) defs,
                warning = scopedFor at.warnings DataLevel n
              },
        signature = (.value.sig.signature) <$> safeHead sigs,
        direction = def.direction,
        fields =
          [ info
              NewInfo
                { id = Field n f.value,
                  nameSpan = f.span,
                  parts = [Part f.span HeadPart],
                  docs = [],
                  warning = scopedFor at.warnings DataLevel f.value
                }
          | f <- def.fields
          ],
        fixity = fixity
      }
  | (n, defs@(WithDocs def _ : _)) <- Map.toList defsByName,
    let sigs = Map.findWithDefault [] n sigsByName
        fixity = scopedFor at.fixities DataLevel n
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

-- * Instances

-- | A class instance, as GHC has it.
data InstSyntax = InstSyntax
  { span :: Span,
    poly :: Ghc.LHsSigType GhcPs,
    sigs :: [Ghc.LSig GhcPs],
    binds :: Ghc.LHsBinds GhcPs,
    -- | The associated type and data instances, in order.
    famInsts :: [FamInstSyntax]
  }

-- | A standalone deriving, as GHC has it.
data DerivSyntax = DerivSyntax
  { span :: Span,
    decl :: Ghc.DerivDecl GhcPs
  }

-- | A type or data instance.
data FamInstSyntax = FamInstSyntax
  { span :: Span,
    family :: Ghc.LIdP GhcPs,
    familyName :: KName,
    args :: [KeyType],
    -- | 'famKey', printed once: the family's list of instances and the
    -- instance itself share it.
    key :: InstanceKey,
    rhs :: FamRhs
  }

-- | What a family instance defines.
data FamRhs = TypeRhs | DataRhs (Ghc.HsDataDefn GhcPs)

-- | A family instance's key: the family and its arguments.
famKey :: KName -> [KeyType] -> InstanceKey
famKey fam args = InstanceKey {name = fam.occ, head = keyHead (renderArgs args)}

-- | A class, type or data instance.
instItem :: SpanMap -> Span -> Ghc.InstDecl GhcPs -> Item
instItem spans s = \case
  Ghc.ClsInstD {Ghc.cid_inst = i} -> case i of
    Ghc.ClsInstDecl
      { Ghc.cid_poly_ty = poly,
        Ghc.cid_binds = binds,
        Ghc.cid_sigs = sigs,
        Ghc.cid_tyfam_insts = tys,
        Ghc.cid_datafam_insts = datas
      } ->
        InstItem
          InstSyntax
            { span = s,
              poly = poly,
              sigs = sigs,
              binds = binds,
              famInsts =
                sortOn
                  (.span)
                  ( [tyFamInst (locatedSpan spans l) (unLoc l) | l <- tys]
                      <> [dataFamInst (locatedSpan spans l) (unLoc l) | l <- datas]
                  )
            }
  Ghc.TyFamInstD {Ghc.tfid_inst = t} -> FamInstItem (tyFamInst s t)
  Ghc.DataFamInstD {Ghc.dfid_inst = d} -> FamInstItem (dataFamInst s d)
  where
    tyFamInst sp = \case
      Ghc.TyFamInstDecl {Ghc.tfid_eqn = eqn} -> famInstSyntax sp eqn TypeRhs
    dataFamInst sp = \case
      Ghc.DataFamInstDecl {Ghc.dfid_eqn = eqn} ->
        famInstSyntax sp eqn (DataRhs (Ghc.feqn_rhs eqn))
    famInstSyntax :: Span -> Ghc.FamEqn GhcPs rhs -> FamRhs -> FamInstSyntax
    famInstSyntax sp eqn rhs = case eqn of
      Ghc.FamEqn {Ghc.feqn_tycon = n, Ghc.feqn_pats = pats} ->
        let fam = plainName (unLoc n)
            args = typeArgs pats
         in FamInstSyntax
              { span = sp,
                family = n,
                familyName = fam,
                args = args,
                key = famKey fam args,
                rhs = rhs
              }

-- | An @instance@, its methods grouped by name like values. Its body is
-- head but for the method equations and pragmas (Note [Extents] in
-- "HaskellReviewer.Core.Decls").
buildInstance :: SpanMap -> Attached -> [DocComment Doc] -> InstSyntax -> InstanceDecl
buildInstance spans at docs i =
  InstanceDecl
    { info =
        info
          NewInfo
            { id = Inst (instanceKey h),
              nameSpan = headNameSpan spans h i.poly,
              parts = carve i.span (sortOn (.span) (bodyParts items <> specs)),
              docs = docs,
              warning = Nothing
            },
      orphan = isOrphan at.locals h.className h.args,
      mentions = localMentions at.locals h.className h.args,
      form =
        ExplicitInstance
          InstanceBody
            { methods = sortOn (\m -> (NonEmpty.head m.extent.parts).span) methods,
              associated = map (buildFamInst spans at DropDocs []) i.famInsts
            }
    }
  where
    h = instanceHead i.poly
    items =
      [sigItem spans (locatedSpan spans l) (unLoc l) | l <- i.sigs]
        <> [bindItem spans (locatedSpan spans l) (unLoc l) | l <- i.binds]
    specs = [Part (locatedSpan spans l) PragmaPart | l@(L _ Ghc.SpecInstSig {}) <- i.sigs]
    methods =
      [ InstanceMethod
          { name = Located {span = minimum b.names, value = n},
            extent = extentOf (minimum b.names) (bitsParts b)
          }
      | (n, b) <- Map.toList (bitsMap [WithDocs x [] | x <- items]),
        not (null b.names)
      ]

-- | A standalone deriving: all head.
buildDeriving :: SpanMap -> Attached -> [DocComment Doc] -> DerivSyntax -> InstanceDecl
buildDeriving spans at docs d = case d.decl of
  Ghc.DerivDecl {Ghc.deriv_type = ty, Ghc.deriv_strategy = strategy} ->
    let poly = Ghc.hswc_body ty
        h = instanceHead poly
     in InstanceDecl
          { info =
              info
                NewInfo
                  { id = Inst (instanceKey h),
                    nameSpan = headNameSpan spans h poly,
                    parts = [Part d.span HeadPart],
                    docs = docs,
                    warning = Nothing
                  },
            orphan = isOrphan at.locals h.className h.args,
            mentions = localMentions at.locals h.className h.args,
            form = StandaloneDeriving (strategyOf spans strategy)
          }

-- | Where the class's name is in a head, or the head.
headNameSpan :: SpanMap -> Head -> Ghc.LHsSigType GhcPs -> Span
headNameSpan spans h poly = maybe (locatedSpan spans poly) (spanOf spans) h.classSpan

-- | A type or data instance. Data instance fields have the first
-- constructor as parent (D-88).
buildFamInst ::
  SpanMap -> Attached -> DocsHere -> [DocComment Doc] -> FamInstSyntax -> FamInstDecl
buildFamInst spans at here docs f =
  FamInstDecl
    { info =
        info
          NewInfo
            { id = Inst f.key,
              nameSpan = locatedSpan spans f.family,
              parts =
                Part f.span HeadPart
                  : [p | c <- famInstConstructors form, p <- docParts c.info],
              docs = docs,
              warning = Nothing
            },
      family = f.familyName.occ,
      form = form,
      orphan = isOrphan at.locals f.familyName f.args
    }
  where
    form = case f.rhs of
      TypeRhs -> TypeInstance
      DataRhs defn ->
        let firstCon = safeHead (concatMap conNames (toList (Ghc.dd_cons defn)))
            parent = maybe f.familyName.occ occ firstCon
            ty = derivable f.familyName.occ f.args
         in case dataDefn spans at here ty parent defn of
              NewtypeDefn b -> NewtypeInstance b
              DataDefn b -> DataInstance b
              TypeDataDefn b -> DataInstance b

-- | The instances of the deriving clauses of the types and data
-- instances already built (top-level, or in an instance). Each takes
-- the key its clause has. Head: the class and the strategy (with the
-- via type).
derivedInstances :: Locals -> Decl -> [InstanceDecl]
derivedInstances locals = \case
  TypeD t -> case t.form of
    DataType b -> fromClauses t.info.id b.derivings
    Newtype b -> fromClauses t.info.id b.derivings
    _ -> []
  FamInstD f -> fromFamInst f
  InstanceD i -> case i.form of
    ExplicitInstance b -> concatMap fromFamInst b.associated
    _ -> []
  _ -> []
  where
    fromFamInst f = case f.form of
      DataInstance b -> fromClauses f.info.id b.derivings
      NewtypeInstance b -> fromClauses f.info.id b.derivings
      TypeInstance -> []
    fromClauses tyId ds =
      [ InstanceDecl
          { info =
              info
                NewInfo
                  { id = c.id,
                    nameSpan = c.span,
                    parts = Part c.span HeadPart : headParts d.strategySpan,
                    docs = [],
                    warning = Nothing
                  },
            orphan = False,
            mentions = derivedMentions c.id tyId,
            form = DerivedInstance Derived {strategy = d.strategy, onType = tyId}
          }
      | d <- ds,
        c <- d.classes
      ]
    -- The class if it's from here, then the type, or the data family
    -- if it's from here.
    derivedMentions clsId tyId =
      nubOrd (filter local (keyName clsId) <> typeName tyId)
    typeName = \case
      Inst k -> filter local [k.name]
      i -> toList (declName i)
    keyName = \case
      Inst k -> [k.name]
      _ -> []
    local n = Set.member n locals.types

-- * Fixities and warnings

-- | Fixities by name, in order.
fixityMap :: [Item] -> Map Text [Scoped Fixity]
fixityMap items =
  scopedMap [(n, Scoped ns f) | FixityDeclItem names ns f <- items, n <- names]

-- | Fixities or warnings by name, in order.
scopedMap :: [(Text, Scoped a)] -> Map Text [Scoped a]
scopedMap = byName

-- | The first fixity or warning for a name at a level.
scopedFor :: Map Text [Scoped a] -> Level -> Text -> Maybe a
scopedFor xs level n = (.value) <$> find at (Map.findWithDefault [] n xs)
  where
    at x = case (x.namespace, level) of
      (AnyNamespace, _) -> True
      (TypeNamespace, TypeLevel) -> True
      (DataNamespace, DataLevel) -> True
      _ -> False

namespace :: Ghc.NamespaceSpecifier -> Namespace
namespace = \case
  Ghc.NoNamespaceSpecifier -> AnyNamespace
  Ghc.TypeNamespaceSpecifier _ -> TypeNamespace
  Ghc.DataNamespaceSpecifier _ -> DataNamespace

fixityParts :: Maybe Fixity -> [Part]
fixityParts f = [Part x.span HeadPart | x <- toList f]

headParts :: (Foldable f) => f Span -> [Part]
headParts xs = [Part x HeadPart | x <- toList xs]

-- * Helpers

-- | What 'info' makes a declaration's info from.
data NewInfo = NewInfo
  { id :: DeclId,
    nameSpan :: Span,
    parts :: [Part],
    docs :: [DocComment Doc],
    warning :: Maybe WarningText
  }

-- | A declaration's info. The doc comments and the warning become parts
-- too. @exported@ is False; "HaskellReviewer.Core.Exported" sets it.
info :: NewInfo -> DeclInfo
info n =
  DeclInfo
    { id = n.id,
      nameSpan = n.nameSpan,
      extent =
        extentOf n.nameSpan $
          n.parts
            <> [Part d.span DocPart | d <- n.docs]
            <> [Part w.span PragmaPart | w <- toList n.warning],
      haddock = sortOn (.span) n.docs,
      warning = n.warning,
      exported = False
    }

-- | The parts as an extent; just the name if there are none.
extentOf :: Span -> [Part] -> Extent
extentOf n parts = mkExtent (fromMaybe (Part n HeadPart :| []) (NonEmpty.nonEmpty parts))

-- | Where a declaration starts; ties (a shared signature) by name.
infoStart :: DeclInfo -> (Span, Span)
infoStart i = ((NonEmpty.head i.extent.parts).span, i.nameSpan)

-- | Like 'infoStart', for any top-level declaration.
declStart :: Decl -> (Span, Span)
declStart d = case declInfo d of
  Just i -> infoStart i
  Nothing -> let s = (NonEmpty.head (declExtent d).parts).span in (s, s)

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
