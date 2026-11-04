{-# LANGUAGE StrictData #-}

-- | The occurrence table of a module (Note [Occurrence table] in
-- "HaskellReviewer.Core.Occurrences"), from GHC's AST while it's alive
-- (D-61). Values only: type variables are all free here.
module HaskellReviewer.Core.Parser.ConvertOccurrences
  ( convertOccurrences,
  )
where

import Control.Monad (foldM, forM, forM_, unless, void, when)
import Control.Monad.ST (ST, runST)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Data.FastString (FastString, mkFastString, unpackFS)
import GHC.Hs hiding (ModuleName)
import GHC.Types.Name.Occurrence
  ( OccEnv,
    OccName,
    OccSet,
    dataName,
    elemOccSet,
    emptyOccEnv,
    extendOccEnv,
    isDataOcc,
    isSymOcc,
    isTcOcc,
    isTvOcc,
    isVarOcc,
    lookupOccEnv,
    mapOccEnv,
    mkOccNameFS,
    mkOccSet,
    mkVarOccFS,
    occNameFS,
    occNameString,
    plusOccEnv,
    setOccNameSpace,
    tcClsName,
    varName,
  )
import GHC.Types.Name.Reader (RdrName (Qual, Unqual), rdrNameOcc)
import GHC.Types.Name.Reader qualified as Rdr
import GHC.Types.SrcLoc (GenLocated (..), SrcSpan, unLoc)
import HaskellReviewer.Core.Decls
  ( ConArgs (..),
    ConDecl (..),
    Decl (ClassD, FamInstD, InstanceD, PatSynD, TypeD, ValueD),
    DeclInfo (..),
    FamInstDecl (..),
    FieldDecl (..),
    InstanceBody (..),
    InstanceDecl (..),
    InstanceForm (..),
    MethodDecl (..),
    NamedDecl (..),
    PatSynDecl (..),
    famInstConstructors,
    formConstructors,
    namedDecls,
  )
import HaskellReviewer.Core.Decls qualified as Model
import HaskellReviewer.Core.ModuleModel (Extensions (..))
import HaskellReviewer.Core.Names (DeclId (..), DeclNs (..), declName)
import HaskellReviewer.Core.Occurrences
  ( BinderId,
    Builder,
    Certainty (..),
    FreeContext (..),
    Namespace (..),
    NotNameKind (..),
    Occurrences,
    Ref (..),
    Role (..),
    TargetIx,
    TokenIx (..),
    addTarget,
    freezeBuilder,
    isRecorded,
    maxIndex,
    mergeScopes,
    newBinder,
    newBuilder,
    noOccurrences,
    record,
    recordPun,
  )
import HaskellReviewer.Core.Parser.Helpers (SpanMap, occText, toSpan)
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens
  ( TokenClass (..),
    Tokens,
    nameLikeClass,
    tokenClassAt,
    tokenCount,
    tokenIndexFrom,
    tokenSpanAt,
  )
import Language.Haskell.Syntax.Basic (FieldLabelString (..))
import Language.Haskell.Syntax.BooleanFormula (BooleanFormula (..))
import Language.Haskell.Syntax.Module.Name qualified as Ghc (ModuleName, mkModuleName)

{- Note [Syntactic scoping]

We walk GHC's AST by hand, with every constructor matched, so a new
ghc-lib-parser that adds one fails to build (-Wincomplete-patterns is
an error under -Werror in CI). Data.Data would be shorter, but it walks
the exact-print annotations too, and it can't tell a binder from a use;
it's a test oracle only.

GHC's own collectors (collectPatBinders and friends) give bare names
with no place, so they are test oracles only.

The environment maps a name (with its namespace) to a local binder.
Lookup: locals, then the module's top level (from the converted
declarations, so the ids are the model's), then free.

Scopes, values only:

- A match (an equation, a case alternative, a lambda): its patterns,
  left to right (a view pattern sees the patterns before it), are in
  scope in the whole match. Its where clause is recursive and in scope
  in every guard and right-hand side. A pattern guard or guard let is
  in scope in the later guards and that right-hand side.
- let and where groups are recursive: every binder is in scope in every
  right-hand side and in the body. Local signatures and fixities point
  at their binder (role Sig).
- do: @p <- e@ binds from the next statement to the end of the block;
  @let@ from itself to the end. @rec@ and @mdo@: the whole block.
- Comprehensions: in order, and the head sees them all. Parallel
  branches start from the outer scope; the head sees all of them.
  @then f@ and @then group using f@ keep the same binders; @f@ sees only
  the scope outside the comprehension's statements (as GHC's renamer),
  @by e@ sees them.
- Arrows: the @proc@ pattern is in scope in the command; commands reuse
  the statement and match rules. The arrow of @f -< x@ and the operator
  of @(| e cmd |)@ see only the scope outside the @proc@ (GHC's
  escapeArrowScope); @f -<< x@ sees everything. @(| |)@ is otherwise
  partial.
- Implicit parameters: an enclosing @let ?x@, lexically, else free.

A binder's scope is the whole region, inner parts where it is shadowed
included (like HIE). Its spans are merged; past a few, one span covers
them all (mergeScopes), so it may be a bit too big.

Records: in @C {f = p}@ the field is the field of C when C is from this
module, else free with C's token as context. In an update @r {f = e}@:
the one field of that name here, or the first of several with the
heuristic flag, else free. A record dot (@r.f@, @(.f)@): the one field
of that name here, with the heuristic flag, else free. Puns (@C {f}@)
give the token to the variable (pun flag) and the field to the side
table. @C {..}@ in a pattern, with C from here, binds the fields not
written: binders with the @..@ span and their name stored. They are made
when first used, so a module with many fields and many wildcards stays
linear. With C from elsewhere, any non-operator variable we can't
resolve in that scope is free with the context MaybeWildcardOf C, and a
top-level one gets the heuristic flag (the wildcard may bind it).
@C {..}@ in an expression: the implicit uses are not recorded.

Instance methods: the class's method when the class is from here, else
free with the class's token as context; role InstanceDef.

Template Haskell: inside a splice nothing is top-level (the stage
restriction), so top-level names become free, and every name there gets
the splice flag. Binders inside a splice scope as usual. Quotes scope
as usual, and a quote inside a splice clears the flag: its names are
resolved as in normal code. A quasi-quote's quoter is free, with the
splice flag, on the whole quasi-quote token.

Top level: equation names are definitions, signatures, fixities and
pragmas are Sig, local export items Export (else free), import items
Import and free. A class method's signature is its definition (Def),
like a default method's equations; a default signature is Sig. RULES
binders scope over both sides.

Types are walked for their names (types and classes: top-level or free)
and their splices. Type variables are all free, binders included (role
Def).

At the end every VarId, ConId, VarSym or ConSym token with no entry is
NotAName (module names, role words, labels get their kind on the way).
-}

-- | The occurrence table of a parsed module.
convertOccurrences ::
  SpanMap ->
  Extensions ->
  Tokens ->
  -- | The module's name, if it has a header.
  Maybe ModuleName ->
  -- | The converted declarations: their ids are the targets.
  [Decl] ->
  HsModule GhcPs ->
  Occurrences
convertOccurrences spans exts tokens self decls m
  | tokenCount tokens > maxIndex = noOccurrences
  | otherwise = runST $ do
      b <- newBuilder (tokenCount tokens)
      top <- buildTop b decls
      wildcardBinders <- newSTRef IntMap.empty
      wildcardCount <- newSTRef 0
      let ctx =
            Ctx
              { builder = b,
                tokens = tokens,
                count = tokenCount tokens,
                spans = spans,
                top = top,
                self = Ghc.mkModuleName (maybe "Main" moduleText self),
                fieldSelectors = "FieldSelectors" `elem` exts.enabled,
                wildcardBinders = wildcardBinders,
                wildcardCount = wildcardCount
              }
      walkModule ctx m
      notNames ctx
      freezeBuilder b (any isSplice (hsmodDecls m))
  where
    moduleText (ModuleName t) = Text.unpack t
    isSplice (L _ d) = case d of
      SpliceD {} -> True
      _ -> False

-- * Context

-- | What the whole walk shares.
data Ctx s = Ctx
  { builder :: Builder s,
    tokens :: Tokens,
    count :: Int,
    spans :: SpanMap,
    top :: Top,
    self :: Ghc.ModuleName,
    fieldSelectors :: Bool,
    -- | The binders each local @C {..}@ has made so far, by its number.
    wildcardBinders :: STRef s (IntMap (OccEnv BinderId)),
    wildcardCount :: STRef s Int
  }

-- | What's in scope, and where we are.
data Env = Env
  { locals :: OccEnv Local,
    -- | How many binders are around: a later one shadows an earlier one.
    depth :: Int,
    -- | Local @C {..}@ patterns in scope, innermost first.
    wildcards :: [Wildcard],
    -- | Implicit parameters, by name.
    ips :: OccEnv BinderId,
    splice :: Bool,
    -- | The innermost @C {..}@ pattern with C not from here.
    foreignWildcard :: Maybe TokenIx,
    -- | In a @[d| |]@ quote: what it declares is not top-level.
    quote :: Bool,
    -- | The scope just outside the innermost @proc@: arrow tails see
    -- only that.
    outsideProc :: Maybe Env
  }

-- | A local binder in scope.
data Local = Local
  { binder :: BinderId,
    depth :: Int
  }

-- | A @C {..}@ pattern with C from here. Its binders are made when a
-- field is first used: see Note [Syntactic scoping].
data Wildcard = Wildcard
  { key :: Int,
    fields :: OccEnv TargetIx,
    written :: OccSet,
    -- | The @..@.
    at :: Span,
    scope :: [Span],
    depth :: Int
  }

emptyEnv :: Env
emptyEnv =
  Env
    { locals = emptyOccEnv,
      depth = 0,
      wildcards = [],
      ips = emptyOccEnv,
      splice = False,
      foreignWildcard = Nothing,
      quote = False,
      outsideProc = Nothing
    }

-- * Top level

-- | The module's top-level names (Note [Syntactic scoping]).
data Top = Top
  { names :: OccEnv TopName,
    -- | Fields by name, in the variable namespace, in source order.
    fields :: OccEnv (NonEmpty TargetIx),
    -- | A constructor's (or record pattern synonym's) fields.
    conFields :: OccEnv (OccEnv TargetIx),
    -- | A class's methods.
    methods :: OccEnv (OccEnv TargetIx)
  }

data TopName = TopName
  { target :: TargetIx,
    -- | Heuristic when several declarations have the name.
    certainty :: Certainty,
    namespace :: Namespace
  }

buildTop :: Builder s -> [Decl] -> ST s Top
buildTop b decls = do
  let named = concatMap namedDecls decls
  ids <- foldM addId Map.empty named
  let ix d = ids Map.! d
      cons = topCons decls
  pure
    Top
      { names = topNames ix named,
        fields = topFields ix named,
        conFields = topConFields ix cons,
        methods = topMethods ix decls
      }
  where
    addId m NamedDecl {info}
      | Map.member info.id m = pure m
      | otherwise = (\t -> Map.insert info.id t m) <$> addTarget b info.id

-- | Every top-level name, with its namespace.
topNames :: (DeclId -> TargetIx) -> [NamedDecl] -> OccEnv TopName
topNames ix named =
  foldl'
    (\env (o, ns, t) -> extendOccEnv env o (keep (lookupOccEnv env o) t ns))
    emptyOccEnv
    [ (occ, ns, ix info.id)
    | NamedDecl {info} <- named,
      Just (occ, ns) <- [nameOcc info.id]
    ]
  where
    keep old t ns = case old of
      Nothing -> TopName {target = t, certainty = Exact, namespace = ns}
      Just o -> o {certainty = Heuristic}
    nameOcc = \case
      Named ns n -> Just $ case ns of
        DType -> (tcOcc n, TypeNs)
        DClass -> (tcOcc n, TypeNs)
        DCon -> (dataOcc n, ConNs)
        DPat -> (dataOcc n, ConNs)
        DVal -> (varOcc n, VarNs)
      Field _ _ -> Nothing
      Inst _ -> Nothing

-- | Fields by name, in source order.
topFields :: (DeclId -> TargetIx) -> [NamedDecl] -> OccEnv (NonEmpty TargetIx)
topFields ix named =
  -- Consed, then reversed: back to source order, so the first is first.
  mapOccEnv NonEmpty.reverse $
    foldl'
      ( \env (o, t) ->
          extendOccEnv env o $ case lookupOccEnv env o of
            -- A field of several constructors of one type: one id.
            Just ts | NonEmpty.head ts == t -> ts
            Just ts -> NonEmpty.cons t ts
            Nothing -> t :| []
      )
      emptyOccEnv
      [(varOcc n, ix info.id) | NamedDecl {info} <- named, Field _ n <- [info.id]]

-- | The constructors and record pattern synonyms, with their fields.
topCons :: [Decl] -> [(OccName, [DeclInfo])]
topCons decls =
  [ (dataOcc n, map (.info) (recordFields c.args))
  | c <- concatMap consOf decls,
    Just n <- [declName c.info.id]
  ]
    <> [(dataOcc n, p.fields) | PatSynD p <- decls, Just n <- [declName p.info.id]]
  where
    consOf = \case
      TypeD t -> formConstructors t.form
      InstanceD i -> case i.form of
        ExplicitInstance body ->
          concatMap (\(f :: FamInstDecl) -> famInstConstructors f.form) body.associated
        _ -> []
      FamInstD f -> famInstConstructors f.form
      ClassD _ -> []
      ValueD _ -> []
      PatSynD _ -> []
      Model.SpliceD _ -> []
    recordFields = \case
      RecordArgs fs -> fs
      GadtRecordArgs fs -> fs
      PrefixArgs _ -> []
      InfixArgs _ _ -> []
      GadtArgs _ -> []

-- | Each constructor's fields, by name.
topConFields ::
  (DeclId -> TargetIx) -> [(OccName, [DeclInfo])] -> OccEnv (OccEnv TargetIx)
topConFields ix cons =
  foldl'
    (\env (c, fs) -> extendOccEnv env c (foldl' field emptyOccEnv fs))
    emptyOccEnv
    cons
  where
    field e (i :: DeclInfo) =
      maybe e (\n -> extendOccEnv e (varOcc n) (ix i.id)) (declName i.id)

-- | Each class's methods, by name.
topMethods :: (DeclId -> TargetIx) -> [Decl] -> OccEnv (OccEnv TargetIx)
topMethods ix decls =
  foldl'
    (\env (c, ms) -> extendOccEnv env c ms)
    emptyOccEnv
    [ (tcOcc n, foldl' method emptyOccEnv c.methods)
    | ClassD c <- decls,
      Just n <- [declName c.info.id]
    ]
  where
    method e (MethodDecl {info}) =
      maybe e (\k -> extendOccEnv e (varOcc k) (ix info.id)) (declName info.id)

varOcc, dataOcc, tcOcc :: Text -> OccName
varOcc = mkOccNameFS varName . mkFastString . Text.unpack
dataOcc = mkOccNameFS dataName . mkFastString . Text.unpack
tcOcc = mkOccNameFS tcClsName . mkFastString . Text.unpack

-- | A top-level name. A variable that isn't one may be a field
-- selector.
topLookup :: Ctx s -> OccName -> Maybe TopName
topLookup ctx occ = case lookupOccEnv ctx.top.names occ of
  Just t -> Just t
  Nothing
    | ctx.fieldSelectors && isVarOcc occ -> fieldName <$> lookupOccEnv ctx.top.fields occ
    | otherwise -> Nothing
  where
    fieldName = \case
      t :| [] -> TopName {target = t, certainty = Exact, namespace = FieldNs}
      t :| _ -> TopName {target = t, certainty = Heuristic, namespace = FieldNs}

-- | A top-level definition's resolution. In a declaration quote, free.
defRef :: Ctx s -> Env -> OccName -> (Namespace, Ref)
defRef ctx env occ = case topLookup ctx occ of
  Just t | not env.quote -> (t.namespace, TopLevelR t.target t.certainty)
  _ -> (occNamespace occ, FreeR Plain)

-- * Tokens

-- | Our span of GHC's.
convSpan :: Ctx s -> SrcSpan -> Maybe Span
convSpan ctx = toSpan ctx.spans

-- | The first token in the span that can be a name, past parens and
-- backticks. None for a macro's span.
nameToken :: Ctx s -> Span -> Maybe TokenIx
nameToken ctx s = TokenIx <$> go (tokenIndexFrom ctx.tokens s.startLine s.startCol)
  where
    go i
      | i >= ctx.count = Nothing
      | otherwise =
          let t = tokenSpanAt ctx.tokens i
           in if (t.startLine, t.startCol) >= (s.endLine, s.endCol)
                then Nothing
                else case tokenClassAt ctx.tokens i of
                  Special -> go (i + 1)
                  Comment -> go (i + 1)
                  Haddock -> go (i + 1)
                  Pragma -> go (i + 1)
                  Keyword -> Just i
                  ReservedOp -> Just i
                  c
                    | nameLikeClass c -> Just i
                    | otherwise -> Nothing

-- | The name token at GHC's span.
tokenAtLoc :: Ctx s -> SrcSpan -> Maybe TokenIx
tokenAtLoc ctx l = convSpan ctx l >>= nameToken ctx

-- | Write an entry for the name at this span, if it has a token.
put :: Ctx s -> Env -> SrcSpan -> Namespace -> Role -> Ref -> ST s ()
put ctx env l ns role ref = case tokenAtLoc ctx l of
  Just i -> record ctx.builder i ns role ref env.splice
  Nothing -> pure ()

-- | The namespace of a name as GHC parsed it.
occNamespace :: OccName -> Namespace
occNamespace occ
  | isTvOcc occ = TyVarNs
  | isVarOcc occ = VarNs
  | isDataOcc occ = ConNs
  | isTcOcc occ = TypeNs
  | otherwise = VarNs

-- * Resolving

-- | What a used name refers to.
resolve :: Ctx s -> Env -> RdrName -> ST s (Namespace, Ref)
resolve ctx env rdr = case rdr of
  Rdr.Exact _ -> pure (occNamespace occ, BuiltInR)
  Rdr.Orig _ _ -> pure (occNamespace occ, BuiltInR)
  Qual m _
    | m == ctx.self -> pure fromTop
    | otherwise -> pure (occNamespace occ, FreeR Plain)
  Unqual _ ->
    lookupLocal ctx env occ >>= \case
      Just b -> pure (occNamespace occ, LocalR b)
      Nothing -> pure fromTop
  where
    occ = rdrNameOcc rdr
    -- Inside a foreign C {..}: the wildcard may bind it.
    maybeWild = isJust env.foreignWildcard && isVarOcc occ && not (isSymOcc occ)
    fromTop = case topLookup ctx occ of
      Just t
        | env.splice -> (t.namespace, FreeR Plain)
        | maybeWild -> (t.namespace, TopLevelR t.target Heuristic)
        | otherwise -> (t.namespace, TopLevelR t.target t.certainty)
      Nothing -> (occNamespace occ, FreeR context)
    context = case env.foreignWildcard of
      Just a | maybeWild -> MaybeWildcardOf a
      _ -> Plain

-- | The local binder of a name: a bound one, or a field of a local
-- @C {..}@, whichever is innermost.
lookupLocal :: Ctx s -> Env -> OccName -> ST s (Maybe BinderId)
lookupLocal ctx env occ = case (lookupOccEnv env.locals occ, wild) of
  (Just l, Just w) | l.depth > w.depth -> pure (Just l.binder)
  (_, Just w) -> Just <$> wildcardBinder ctx w occ
  (Just l, Nothing) -> pure (Just l.binder)
  (Nothing, Nothing) -> pure Nothing
  where
    -- Usually none or one or two are active: a scan is fine.
    wild = case env.wildcards of
      [] -> Nothing
      ws
        | isVarOcc occ -> case filter binds ws of
            w : _ -> Just w
            [] -> Nothing
        | otherwise -> Nothing
    binds w = isJust (lookupOccEnv w.fields occ) && not (occ `elemOccSet` w.written)

-- | The binder a local @C {..}@ gives this field, made the first time.
wildcardBinder :: Ctx s -> Wildcard -> OccName -> ST s BinderId
wildcardBinder ctx w occ = do
  made <- readSTRef ctx.wildcardBinders
  case IntMap.lookup w.key made >>= (`lookupOccEnv` occ) of
    Just b -> pure b
    Nothing -> do
      b <- newBinder ctx.builder w.at (Just (Text.pack (occNameString occ))) w.scope
      let add = Just . (\e -> extendOccEnv e occ b) . fromMaybe emptyOccEnv
      writeSTRef ctx.wildcardBinders (IntMap.alter add w.key made)
      pure b

-- | A used name.
useName :: Ctx s -> Env -> Role -> LIdP GhcPs -> ST s ()
useName ctx env role (L l rdr) = do
  (ns, ref) <- resolve ctx env rdr
  put ctx env (locA l) ns role ref

-- | A name that may be in either namespace, as in export lists and
-- pragmas: the other one is tried when the parsed one has nothing.
useEither :: Ctx s -> Env -> Role -> LIdP GhcPs -> ST s ()
useEither ctx env role ln@(L l rdr) = do
  first <- resolve ctx env rdr
  case (first, flipped) of
    ((_, FreeR _), Just other) ->
      resolve ctx env other >>= \case
        (ns, ref@(TopLevelR _ _)) -> put ctx env (locA l) ns role ref
        _ -> useName ctx env role ln
    _ -> useName ctx env role ln
  where
    occ = rdrNameOcc rdr
    flipped
      | isDataOcc occ = Just (withOcc (setOccNameSpace tcClsName occ))
      | isTcOcc occ = Just (withOcc (setOccNameSpace dataName occ))
      | otherwise = Nothing
    withOcc o = case rdr of
      Qual m _ -> Qual m o
      _ -> Unqual o

-- | A top-level definition (or Sig) of a name. In a declaration quote,
-- free.
defName :: Ctx s -> Env -> Role -> LIdP GhcPs -> ST s ()
defName ctx env role (L l rdr) = put ctx env (locA l) ns role ref
  where
    (ns, ref) = defRef ctx env (rdrNameOcc rdr)

-- | Whether a new binder writes the entry of its token.
data BinderToken
  = -- | Def, now.
    MarkToken
  | -- | The caller writes it: a function's equations, a pun.
    LeaveToken

-- | Where a binder is: its token's span, or the name's span when it has
-- no token (a macro made it). The token, if any.
binderPlace :: Ctx s -> SrcSpan -> (Span, Maybe TokenIx)
binderPlace ctx l = case tok of
  Just (TokenIx i) -> (tokenSpanAt ctx.tokens i, tok)
  Nothing -> (fromMaybe noSpan s, Nothing)
  where
    s = convSpan ctx l
    tok = s >>= nameToken ctx

-- | A new local binder for a name.
bindLocal :: Ctx s -> Env -> [Span] -> BinderToken -> LIdP GhcPs -> ST s (Env, BinderId)
bindLocal ctx env scope mark (L l rdr) = do
  let (bspan, tok) = binderPlace ctx (locA l)
      occ = rdrNameOcc rdr
      name = if isJust tok then Nothing else Just (occText rdr)
  b <- newBinder ctx.builder bspan name scope
  case (tok, mark) of
    (Just i, MarkToken) ->
      record ctx.builder i (occNamespace occ) Def (LocalR b) env.splice
    _ -> pure ()
  let depth = env.depth + 1
  pure (env {locals = extendOccEnv env.locals occ (Local b depth), depth = depth}, b)

-- | A local binder that is also a field pun: the field goes to the side
-- table.
bindPun :: Ctx s -> Env -> [Span] -> Ref -> LIdP GhcPs -> ST s Env
bindPun ctx env scope field ln@(L l rdr) = do
  (env', b) <- bindLocal ctx env scope LeaveToken ln
  forM_ (tokenAtLoc ctx (locA l)) $ \i ->
    let ns = occNamespace (rdrNameOcc rdr)
     in recordPun ctx.builder i ns Def (LocalR b) field env.splice
  pure env'

noSpan :: Span
noSpan = Span (Line 0) (Col 0) (Line 0) (Col 0)

-- | From the start of one span to the end of another.
hull :: Span -> Span -> Span
hull a b = Span a.startLine a.startCol b.endLine b.endCol

-- | Our span of something located, or none.
spanL :: Ctx s -> SrcSpan -> [Span]
spanL ctx l = maybe [] pure (convSpan ctx l)

-- * Fields

-- | What we know of a constructor, for its fields.
data ConInfo
  = -- | From here, with its fields.
    LocalCon (OccEnv TargetIx)
  | -- | From elsewhere: its token, if it has one.
    ForeignCon (Maybe TokenIx)

conInfo :: Ctx s -> Env -> LIdP GhcPs -> ConInfo
conInfo ctx env (L l rdr) = case rdr of
  Unqual occ | Just fs <- local occ -> LocalCon fs
  Qual m occ | m == ctx.self, Just fs <- local occ -> LocalCon fs
  _ -> ForeignCon (tokenAtLoc ctx (locA l))
  where
    local occ
      | env.splice = Nothing
      | otherwise = lookupOccEnv ctx.top.conFields occ

-- | A field of a constructor, as in @C {f = x}@.
conFieldRef :: ConInfo -> RdrName -> Ref
conFieldRef ci rdr = case ci of
  LocalCon fs -> case lookupOccEnv fs occ of
    Just t -> TopLevelR t Exact
    Nothing -> FreeR Plain
  ForeignCon (Just t) -> FreeR (FieldOfCon t)
  ForeignCon Nothing -> FreeR Plain
  where
    occ = mkOccNameFS varName (occNameFS (rdrNameOcc rdr))

-- | A field in a record update, by name: the one field here, or the
-- first of several, a guess.
updateFieldRef :: Ctx s -> Env -> FastString -> Ref
updateFieldRef ctx env f
  | env.splice = FreeR Plain
  | otherwise = case lookupOccEnv ctx.top.fields (mkVarOccFS f) of
      Just (t :| []) -> TopLevelR t Exact
      Just (t :| _) -> TopLevelR t Heuristic
      Nothing -> FreeR Plain

-- | A record dot field: the one field of that name here, a guess.
dotFieldRef :: Ctx s -> Env -> FastString -> Ref
dotFieldRef ctx env f
  | env.splice = FreeR Plain
  | otherwise = case lookupOccEnv ctx.top.fields (mkVarOccFS f) of
      Just (t :| []) -> TopLevelR t Heuristic
      _ -> FreeR Plain

putDotField :: Ctx s -> Env -> DotFieldOcc GhcPs -> ST s ()
putDotField ctx env = \case
  DotFieldOcc _ (L l (FieldLabelString f)) ->
    put ctx env (locA l) FieldNs Use (dotFieldRef ctx env f)

-- | A pun in an expression (@C {f}@): the variable @f@ is used, the
-- field is in the side table. The parser's right-hand side is a
-- placeholder with no place, so the label is the variable.
punUse :: Ctx s -> Env -> Ref -> LIdP GhcPs -> ST s ()
punUse ctx env field (L l rdr) = forM_ (tokenAtLoc ctx (locA l)) $ \i -> do
  (ns, ref) <- resolve ctx env rdr
  recordPun ctx.builder i ns Use ref field env.splice

-- * Patterns

-- | What a pattern's variables are.
data PatMode
  = -- | Local binders, in scope there.
    BindIn [Span]
  | -- | Top-level definitions.
    BindTop
  | -- | Uses: a pattern synonym's right-hand side, of its arguments.
    UseVars

-- | Walk a pattern; the scope it returns has its binders.
walkPat :: Ctx s -> PatMode -> Env -> LPat GhcPs -> ST s Env
walkPat ctx mode = go
  where
    go env (L _ pat) = case pat of
      WildPat _ -> pure env
      VarPat _ n -> bindVar env n
      LazyPat _ p -> go env p
      AsPat _ n p -> bindVar env n >>= \e -> go e p
      ParPat _ p -> go env p
      BangPat _ p -> go env p
      ListPat _ ps -> foldM go env ps
      TuplePat _ ps _ -> foldM go env ps
      -- Or-patterns bind nothing.
      OrPat _ ps -> env <$ mapM_ (go env) ps
      SumPat _ p _ _ -> go env p
      ConPat _ con details -> do
        useName ctx env Use con
        case details of
          PrefixCon ps -> foldM go env ps
          InfixCon a b -> go env a >>= \e -> go e b
          RecCon fs -> recordPat env con fs
      ViewPat _ e p -> walkExpr ctx env e >> go env p
      SplicePat _ s -> env <$ walkSplice ctx env s
      LitPat _ _ -> pure env
      NPat {} -> pure env
      NPlusKPat _ n _ _ _ _ -> bindVar env n
      SigPat _ p (HsPS _ t) -> do
        e <- go env p
        walkType ctx e t
        pure e
      EmbTyPat _ (HsTP _ t) -> env <$ walkType ctx env t
      InvisPat _ (HsTP _ t) -> env <$ walkType ctx env t

    bindVar env n = case mode of
      BindIn scope -> fst <$> bindLocal ctx env scope MarkToken n
      BindTop -> env <$ defName ctx env Def n
      UseVars -> env <$ useName ctx env Use n

    bindPunVar env field n = case mode of
      BindIn scope -> bindPun ctx env scope field n
      BindTop -> env <$ writePun env field Def n
      UseVars -> env <$ writePun env field Use n

    -- A pun that binds nothing here: the variable is top-level or used.
    writePun env field role (L l rdr) = forM_ (tokenAtLoc ctx (locA l)) $ \i -> do
      (ns, ref) <- case role of
        Def -> pure (defRef ctx env (rdrNameOcc rdr))
        _ -> resolve ctx env rdr
      recordPun ctx.builder i ns role ref field env.splice

    recordPat env con (HsRecFields _ flds dotdot) = do
      let ci = conInfo ctx env con
      env' <- foldM (fieldPat ci) env flds
      case dotdot of
        Nothing -> pure env'
        Just (L l _) -> case (ci, mode) of
          (LocalCon fs, BindIn scope) -> do
            key <- readSTRef ctx.wildcardCount
            writeSTRef ctx.wildcardCount (key + 1)
            let depth = env'.depth + 1
                w =
                  Wildcard
                    { key = key,
                      fields = fs,
                      written =
                        mkOccSet
                          [ mkOccNameFS varName (occNameFS (rdrNameOcc (unLoc lbl)))
                          | L _ (HsFieldBind _ (L _ (FieldOcc _ lbl)) _ _) <- flds
                          ],
                      at = fromMaybe noSpan (convSpan ctx (locA l)),
                      scope = scope,
                      depth = depth
                    }
            pure env' {wildcards = w : env'.wildcards, depth = depth}
          (ForeignCon (Just t), BindIn _) -> pure env' {foreignWildcard = Just t}
          _ -> pure env'

    fieldPat ci env (L _ (HsFieldBind _ (L _ (FieldOcc _ lbl@(L ll rdr))) rhs pun))
      | pun = bindPunVar env (conFieldRef ci rdr) lbl
      | otherwise = do
          put ctx env (locA ll) FieldNs Use (inSplice env (conFieldRef ci rdr))
          go env rhs

-- | Top-level refs become free in a splice.
inSplice :: Env -> Ref -> Ref
inSplice env ref
  | env.splice, TopLevelR _ _ <- ref = FreeR Plain
  | otherwise = ref

-- * Expressions

walkExpr :: Ctx s -> Env -> LHsExpr GhcPs -> ST s ()
walkExpr ctx env (L l expr) = case expr of
  HsVar _ n -> useName ctx env Use n
  HsOverLabel _ _ -> put ctx env (locA l) OtherNs Use (NotANameR LabelWord)
  HsIPVar _ (HsIPName f) -> put ctx env (locA l) IPNs Use (ipRef env f)
  HsOverLit _ _ -> pure ()
  HsLit _ _ -> pure ()
  HsLam _ variant mg ->
    walkMatches ctx env (lamScope ctx (locA l) variant) noFun (walkExpr ctx) mg
  HsApp _ f x -> go f >> go x
  HsAppType _ e (HsWC _ t) -> go e >> walkType ctx env t
  OpApp _ a op b -> go a >> go op >> go b
  NegApp _ e _ -> go e
  HsPar _ e -> go e
  SectionL _ a op -> go a >> go op
  SectionR _ op b -> go op >> go b
  ExplicitTuple _ args _ -> forM_ args $ \case
    Present _ e -> go e
    Missing _ -> pure ()
  ExplicitSum _ _ _ e -> go e
  HsCase _ e mg -> go e >> walkMatches ctx env Nothing noFun (walkExpr ctx) mg
  HsIf _ c t e -> go c >> go t >> go e
  HsMultiIf _ grhss -> mapM_ (walkGRHS ctx env (walkExpr ctx)) grhss
  HsLet _ binds body -> do
    env' <- walkLocalBinds ctx env (spanL ctx (locA l)) binds
    walkExpr ctx env' body
  HsDo _ flavour (L ls stmts) ->
    let stmtsIn = void (walkStmts ctx env (walkExpr ctx) [] stmts)
     in case flavour of
          MDoExpr _ ->
            void (walkRecStmts ctx env (walkExpr ctx) (spanL ctx (locA ls)) stmts)
          DoExpr _ -> stmtsIn
          GhciStmtCtxt -> stmtsIn
          ListComp -> stmtsIn
          MonadComp -> stmtsIn
  ExplicitList _ es -> mapM_ go es
  RecordCon {rcon_con = con, rcon_flds = HsRecFields _ flds _} -> do
    useName ctx env Use con
    let ci = conInfo ctx env con
    forM_ flds $ \(L _ (HsFieldBind _ (L _ fo) rhs pun)) -> case fo of
      FieldOcc _ (L ll rdr)
        | pun -> punUse ctx env (inSplice env (conFieldRef ci rdr)) (L ll rdr)
        | otherwise -> do
            put ctx env (locA ll) FieldNs Use (inSplice env (conFieldRef ci rdr))
            go rhs
  RecordUpd {rupd_expr = e, rupd_flds = flds} -> do
    go e
    case flds of
      RegularRecUpdFields _ fs -> forM_ fs $ \(L _ (HsFieldBind _ (L _ fo) rhs pun)) ->
        case fo of
          FieldOcc _ (L ll rdr) -> do
            let ref = updateFieldRef ctx env (occNameFS (rdrNameOcc rdr))
            if pun
              then punUse ctx env ref (L ll rdr)
              else put ctx env (locA ll) FieldNs Use ref >> go rhs
      OverloadedRecUpdFields _ ps -> forM_ ps $ \(L _ bind) -> do
        let HsFieldBind _ (L _ (FieldLabelStrings labels)) rhs pun = bind
        let dots = map unLoc (toList labels)
        if pun
          then case dots of
            [DotFieldOcc _ (L ll (FieldLabelString f))] ->
              punUse ctx env (dotFieldRef ctx env f) (L ll (Unqual (mkVarOccFS f)))
            _ -> mapM_ (putDotField ctx env) dots >> go rhs
          else mapM_ (putDotField ctx env) dots >> go rhs
  HsGetField {gf_expr = e, gf_field = L _ f} -> go e >> putDotField ctx env f
  HsProjection {proj_flds = fs} -> mapM_ (putDotField ctx env) fs
  ExprWithTySig _ e (HsWC _ t) -> go e >> walkSigType ctx env t
  ArithSeq _ _ info -> case info of
    From a -> go a
    FromThen a b -> go a >> go b
    FromTo a b -> go a >> go b
    FromThenTo a b c -> go a >> go b >> go c
  HsTypedBracket _ e -> walkExpr ctx env {splice = False} e
  HsUntypedBracket _ q -> walkQuote ctx env {splice = False} (locA l) q
  HsTypedSplice _ s -> case s of
    HsTypedSpliceExpr _ e -> walkExpr ctx env {splice = True} e
  HsUntypedSplice _ s -> walkSplice ctx env s
  HsProc _ pat (L _ top) -> case top of
    HsCmdTop _ cmd -> do
      env' <- walkPat ctx (BindIn (spanL ctx (locA l))) env pat
      walkCmd ctx env' {outsideProc = Just env} cmd
  HsStatic _ e -> go e
  HsPragE _ prag e -> case prag of
    HsPragSCC _ _ -> go e
  HsEmbTy _ (HsWC _ t) -> walkType ctx env t
  HsHole kind -> case kind of
    HoleVar n -> useName ctx env Use n
    HoleError -> pure ()
  HsForAll _ tele e -> walkTelescope ctx env tele >> go e
  HsQual _ (L _ cs) e -> mapM_ go cs >> go e
  HsFunArr _ mult a b -> do
    case mult of
      HsUnannotated _ -> pure ()
      HsLinearAnn _ -> pure ()
      HsExplicitMult _ m -> go m
    go a
    go b
  where
    go = walkExpr ctx env

-- | An implicit parameter: an enclosing @let ?x@, else free.
ipRef :: Env -> FastString -> Ref
ipRef env f = maybe (FreeR Plain) LocalR (lookupOccEnv env.ips (mkVarOccFS f))

-- | A lambda's binders are in scope in the lambda; @\\case@ and
-- @\\cases@ alternatives are matches of their own.
lamScope :: Ctx s -> SrcSpan -> HsLamVariant -> Maybe [Span]
lamScope ctx l = \case
  LamSingle -> Just (spanL ctx l)
  LamCase -> Nothing
  LamCases -> Nothing

-- | No function name to write (a case, a lambda).
noFun :: LIdP GhcPs -> ST s ()
noFun _ = pure ()

-- | A splice: what's inside sees no top-level names.
walkSplice :: Ctx s -> Env -> HsUntypedSplice GhcPs -> ST s ()
walkSplice ctx env = \case
  HsUntypedSpliceExpr _ e -> walkExpr ctx env {splice = True} e
  HsQuasiQuote _ (L l _) _ ->
    -- The quoter is inside the quasi-quote's one token.
    forM_ (convSpan ctx (locA l)) $ \s -> do
      let i = tokenIndexFrom ctx.tokens s.startLine s.startCol - 1
      when (i >= 0 && i < ctx.count) $ do
        let t = tokenSpanAt ctx.tokens i
        when ((t.endLine, t.endCol) >= (s.endLine, s.endCol)) $
          record ctx.builder (TokenIx i) VarNs Use (FreeR QuasiQuoter) True

-- | A quote: normal scoping.
walkQuote :: Ctx s -> Env -> SrcSpan -> HsQuote GhcPs -> ST s ()
walkQuote ctx env l = \case
  ExpBr _ e -> walkExpr ctx env e
  PatBr _ p -> void (walkPat ctx (BindIn (spanL ctx l)) env p)
  DecBrL _ ds -> walkQuoteDecls ctx env ds
  -- Only after the renamer.
  DecBrG _ _ -> pure ()
  TypBr _ t -> walkType ctx env t
  VarBr _ _ n -> useName ctx env Use n

-- * Commands

walkCmd :: Ctx s -> Env -> LHsCmd GhcPs -> ST s ()
walkCmd ctx env (L l cmd) = case cmd of
  HsCmdArrApp _ f arg app _ -> do
    case app of
      HsFirstOrderApp -> walkExpr ctx outside f
      HsHigherOrderApp -> walkExpr ctx env f
    walkExpr ctx env arg
  HsCmdArrForm _ op _ args -> do
    walkExpr ctx outside op
    forM_ args $ \(L _ top) -> case top of
      HsCmdTop _ c -> walkCmd ctx env c
  HsCmdApp _ c e -> walkCmd ctx env c >> walkExpr ctx env e
  HsCmdLam _ variant mg ->
    walkMatches ctx env (lamScope ctx (locA l) variant) noFun (walkCmd ctx) mg
  HsCmdPar _ c -> walkCmd ctx env c
  HsCmdCase _ e mg -> do
    walkExpr ctx env e
    walkMatches ctx env Nothing noFun (walkCmd ctx) mg
  HsCmdIf _ _ c t e -> walkExpr ctx env c >> walkCmd ctx env t >> walkCmd ctx env e
  HsCmdLet _ binds c -> do
    env' <- walkLocalBinds ctx env (spanL ctx (locA l)) binds
    walkCmd ctx env' c
  HsCmdDo _ (L _ stmts) -> void (walkStmts ctx env (walkCmd ctx) [] stmts)
  where
    -- The arrow's scope: what's outside the proc (GHC's escapeArrowScope).
    outside = fromMaybe env env.outsideProc

-- * Matches

-- | What the walks over matches and statements need of a body type
-- (expressions or arrow commands): their parts have places.
type Body body =
  ( HasLoc (Anno (Match GhcPs (LocatedA (body GhcPs)))),
    HasLoc (Anno (GRHS GhcPs (LocatedA (body GhcPs)))),
    HasLoc (Anno (StmtLR GhcPs GhcPs (LocatedA (body GhcPs))))
  )

-- | Matches: each one's patterns are in scope in it (or in the given
-- scope, for a lambda). The function name callback writes each
-- equation's name.
walkMatches ::
  (Body body) =>
  Ctx s ->
  Env ->
  Maybe [Span] ->
  (LIdP GhcPs -> ST s ()) ->
  (Env -> LocatedA (body GhcPs) -> ST s ()) ->
  MatchGroup GhcPs (LocatedA (body GhcPs)) ->
  ST s ()
walkMatches ctx env scope funName body = \case
  MG _ (L _ ms) -> forM_ ms $ \(L l m) -> case m of
    Match _ mctx (L _ pats) grhss -> do
      case mctx of
        FunRhs {mc_fun = n} -> funName n
        CaseAlt -> pure ()
        LamAlt _ -> pure ()
        IfAlt -> pure ()
        ArrowMatchCtxt _ -> pure ()
        PatBindRhs -> pure ()
        PatBindGuards -> pure ()
        RecUpd -> pure ()
        StmtCtxt _ -> pure ()
        ThPatSplice -> pure ()
        ThPatQuote -> pure ()
        PatSyn -> pure ()
        LazyPatCtx -> pure ()
      let whole = spanL ctx (locA l)
      env' <- foldM (walkPat ctx (BindIn (fromMaybe whole scope))) env pats
      walkGRHSs ctx env' whole body grhss

-- | Right-hand sides with their where clause, which is in scope from
-- the first guard to the end of the match.
walkGRHSs ::
  (Body body) =>
  Ctx s ->
  Env ->
  [Span] ->
  (Env -> LocatedA (body GhcPs) -> ST s ()) ->
  GRHSs GhcPs (LocatedA (body GhcPs)) ->
  ST s ()
walkGRHSs ctx env whole body = \case
  GRHSs _ grhss binds -> do
    let firstStart = spanL ctx (case NonEmpty.head grhss of L x _ -> locA x)
        whereScope = case (firstStart, whole) of
          ([a], [w]) -> [hull a w]
          _ -> whole
    env' <- walkLocalBinds ctx env whereScope binds
    mapM_ (walkGRHS ctx env' body) grhss

-- | Guards, in order, then the right-hand side.
walkGRHS ::
  Ctx s ->
  Env ->
  (Env -> LocatedA (body GhcPs) -> ST s ()) ->
  LGRHS GhcPs (LocatedA (body GhcPs)) ->
  ST s ()
walkGRHS ctx env body (L _ g) = case g of
  GRHS _ guards rhs@(L lr _) -> do
    env' <- walkStmts ctx env (walkExpr ctx) (spanL ctx (locA lr)) guards
    body env' rhs

-- * Statements

-- | Statements in order. A binder is in scope from the next statement
-- to the end, and in the extra spans. A comprehension's head (its last
-- statement, written first) is walked last and sees them all.
walkStmts ::
  (Body body) =>
  Ctx s ->
  Env ->
  (Env -> LocatedA (body GhcPs) -> ST s ()) ->
  [Span] ->
  [LStmt GhcPs (LocatedA (body GhcPs))] ->
  ST s Env
walkStmts ctx env body extra stmts = do
  env' <- go env ordered
  forM_ heads $ \s -> walkStmt ctx env' body [] s
  pure env'
  where
    -- The head of a comprehension starts before the first statement.
    firstStart = case stmts of
      s : _ -> start s
      [] -> Nothing
    isHead s@(L _ st) = case st of
      LastStmt {} -> start s < firstStart && isJust (start s)
      _ -> False
    heads = filter isHead stmts
    ordered = filter (not . isHead) stmts
    headSpans = concatMap stmtSpan heads
    blockEnd = case reverse ordered of
      s : _ -> stmtSpan s
      [] -> []
    go e = \case
      [] -> pure e
      s : rest -> do
        let after = case (concatMap stmtSpan (take 1 rest), blockEnd) of
              ([a], [b]) -> [hull a b]
              _ -> []
        e' <- walkStmt ctx e body (mergeScopes (after <> extra <> headSpans)) s
        go e' rest
    start s =
      (\x -> (x.startLine, x.startCol)) <$> case stmtSpan s of
        x : _ -> Just x
        [] -> Nothing
    stmtSpan (L l _) = spanL ctx (locA l)

-- | One statement. Its binders are in scope in the given spans.
walkStmt ::
  (Body body) =>
  Ctx s ->
  Env ->
  (Env -> LocatedA (body GhcPs) -> ST s ()) ->
  [Span] ->
  LStmt GhcPs (LocatedA (body GhcPs)) ->
  ST s Env
walkStmt ctx env body scope (L l stmt) = case stmt of
  LastStmt _ b _ _ -> env <$ body env b
  BindStmt _ pat b -> do
    body env b
    walkPat ctx (BindIn scope) env pat
  BodyStmt _ b _ _ -> env <$ body env b
  LetStmt _ binds ->
    walkLocalBinds ctx env (mergeScopes (spanL ctx (locA l) <> scope)) binds
  ParStmt _ blocks _ _ -> do
    envs <- forM (toList blocks) $ \case
      ParStmtBlock _ ss _ _ -> walkStmts ctx env (walkExpr ctx) scope ss
    -- The union of the branches: their binders, their wildcards (each
    -- branch's new ones, then the outer ones), the deepest depth.
    let new x = take (length x.wildcards - length env.wildcards) x.wildcards
        foreignOf x
          | x.foreignWildcard /= env.foreignWildcard = x.foreignWildcard
          | otherwise = Nothing
    pure
      env
        { locals = foldl' (\acc x -> plusOccEnv acc x.locals) env.locals envs,
          wildcards = concatMap new envs <> env.wildcards,
          foreignWildcard =
            case [w | Just w <- map foreignOf envs] of
              w : _ -> Just w
              [] -> env.foreignWildcard,
          depth = maximum (env.depth : map (.depth) envs)
        }
  -- Like GHC's renamer: @using f@ sees the scope outside the statements,
  -- @by e@ sees their binders.
  TransStmt {trS_stmts = ss, trS_using = using, trS_by = by} -> do
    let extra = concatMap (spanL ctx . getLocA) (toList by)
    env' <- walkStmts ctx env (walkExpr ctx) (mergeScopes (scope <> extra)) ss
    walkExpr ctx env using
    mapM_ (walkExpr ctx env') by
    pure env'
  RecStmt {recS_stmts = L _ ss} ->
    walkRecStmts ctx env body (mergeScopes (spanL ctx (locA l) <> scope)) ss

-- | A @rec@ block or @mdo@: every binder is in scope in the whole
-- block (and the given spans).
walkRecStmts ::
  (Body body) =>
  Ctx s ->
  Env ->
  (Env -> LocatedA (body GhcPs) -> ST s ()) ->
  [Span] ->
  [LStmt GhcPs (LocatedA (body GhcPs))] ->
  ST s Env
walkRecStmts ctx env body scope stmts = do
  (env', later) <- foldM declare (env, []) stmts
  mapM_ ($ env') (reverse later)
  pure env'
  where
    declare (e, ks) s@(L _ stmt) = case stmt of
      BindStmt _ pat b -> do
        e' <- walkPat ctx (BindIn scope) e pat
        pure (e', (\x -> body x b) : ks)
      -- A nested rec: its binders are in scope in the whole block too.
      RecStmt {recS_stmts = L _ ss} -> foldM declare (e, ks) ss
      LetStmt _ binds -> do
        (e', k) <- declareLocalBinds ctx e scope binds
        pure (e', k : ks)
      LastStmt {} -> pure (e, (\x -> void (walkStmt ctx x body scope s)) : ks)
      BodyStmt {} -> pure (e, (\x -> void (walkStmt ctx x body scope s)) : ks)
      ParStmt {} -> pure (e, (\x -> void (walkStmt ctx x body scope s)) : ks)
      TransStmt {} -> pure (e, (\x -> void (walkStmt ctx x body scope s)) : ks)

-- * Local bindings

-- | A let or where group: recursive. Returns the scope with its
-- binders.
walkLocalBinds :: Ctx s -> Env -> [Span] -> HsLocalBinds GhcPs -> ST s Env
walkLocalBinds ctx env scope binds = do
  (env', k) <- declareLocalBinds ctx env scope binds
  k env'
  pure env'

-- | A group's binders, and what walks its right-hand sides once the
-- whole scope is known.
declareLocalBinds ::
  Ctx s -> Env -> [Span] -> HsLocalBinds GhcPs -> ST s (Env, Env -> ST s ())
declareLocalBinds ctx env scope = \case
  HsValBinds _ vb -> case vb of
    ValBinds _ binds sigs -> do
      (env', ks) <- foldM (declareBind ctx scope) (env, []) binds
      pure
        ( env',
          \e -> do
            mapM_ ($ e) (reverse ks)
            mapM_ (walkSig ctx e (useName ctx e) Sig) sigs
        )
    -- Only after the renamer.
    XValBindsLR _ -> pure (env, \_ -> pure ())
  HsIPBinds _ ipbs -> case ipbs of
    IPBinds _ bs -> do
      env' <-
        foldM
          ( \e (L _ bind) -> case bind of
              IPBind _ (L ln (HsIPName f)) rhs -> do
                -- Not recursive: the right-hand side sees the outer ones.
                walkExpr ctx env rhs
                let (bspan, tok) = binderPlace ctx (locA ln)
                    name = if isJust tok then Nothing else Just ("?" <> fsText f)
                b <- newBinder ctx.builder bspan name scope
                forM_ tok $ \i -> record ctx.builder i IPNs Def (LocalR b) env.splice
                pure e {ips = extendOccEnv e.ips (mkVarOccFS f) b}
          )
          env
          bs
      pure (env', \_ -> pure ())
  EmptyLocalBinds _ -> pure (env, \_ -> pure ())
  where
    fsText = Text.pack . unpackFS

-- | One binding of a group: its binders now, its right-hand side later.
declareBind ::
  Ctx s ->
  [Span] ->
  (Env, [Env -> ST s ()]) ->
  LHsBind GhcPs ->
  ST s (Env, [Env -> ST s ()])
declareBind ctx scope (env, ks) (L l bind) = case bind of
  FunBind _ n mg -> do
    (env', b) <- bindLocal ctx env scope LeaveToken n
    let k e = walkMatches ctx e Nothing (\x -> putLocalDef e b x) (walkExpr ctx) mg
    pure (env', k : ks)
  PatBind _ pat mult grhss -> do
    env' <- walkPat ctx (BindIn scope) env pat
    let k e = do
          walkMult ctx e mult
          walkGRHSs ctx e (spanL ctx (locA l)) (walkExpr ctx) grhss
    pure (env', k : ks)
  -- Only made by the type checker.
  VarBind {} -> pure (env, ks)
  -- Only at the top level (or in a quote).
  PatSynBind _ psb -> pure (env, (\e -> walkPatSyn ctx e psb) : ks)
  where
    putLocalDef e b (L ln rdr) =
      put ctx e (locA ln) (occNamespace (rdrNameOcc rdr)) Def (LocalR b)

walkMult :: Ctx s -> Env -> HsMultAnn GhcPs -> ST s ()
walkMult ctx env = \case
  HsUnannotated _ -> pure ()
  HsLinearAnn _ -> pure ()
  HsExplicitMult _ t -> walkType ctx env t

-- | A signature, fixity or pragma. The callback writes each name with
-- a role; class method signatures get the given role (Def in a class).
walkSig ::
  Ctx s -> Env -> (Role -> LIdP GhcPs -> ST s ()) -> Role -> LSig GhcPs -> ST s ()
walkSig ctx env name classOpRole (L l sig) = case sig of
  TypeSig _ ns (HsWC _ t) -> mapM_ (name Sig) ns >> walkSigType ctx env t
  PatSynSig _ ns t -> mapM_ (name Sig) ns >> walkSigType ctx env t
  ClassOpSig _ isDefault ns t -> do
    mapM_ (name (if isDefault then Sig else classOpRole)) ns
    walkSigType ctx env t
  FixSig _ f -> case f of
    FixitySig _ ns _ -> mapM_ (name Sig) ns
  InlineSig _ n _ -> name Sig n
  SpecSig _ n ts _ -> name Sig n >> mapM_ (walkSigType ctx env) ts
  SpecSigE _ bndrs e _ -> do
    env' <- ruleBinders ctx env (spanL ctx (locA l)) bndrs
    walkExpr ctx env' e
  SpecInstSig _ t -> walkSigType ctx env t
  MinimalSig _ (L _ formula) -> boolFormula formula
  SCCFunSig _ n _ -> name Sig n
  CompleteMatchSig _ ns ty -> do
    mapM_ (useEither ctx env Sig) ns
    mapM_ (useName ctx env Use) ty
  where
    boolFormula = \case
      Var n -> name Sig n
      And fs -> mapM_ (boolFormula . unLoc) fs
      Or fs -> mapM_ (boolFormula . unLoc) fs
      Parens f -> boolFormula (unLoc f)

-- | RULES and SPECIALISE binders: in scope in the given spans.
ruleBinders :: Ctx s -> Env -> [Span] -> RuleBndrs GhcPs -> ST s Env
ruleBinders ctx env scope = \case
  RuleBndrs _ tyvs tmvs -> do
    mapM_ (mapM_ (walkTyVarBndr ctx env)) tyvs
    foldM
      ( \e (L _ r) -> case r of
          RuleBndr _ n -> fst <$> bindLocal ctx e scope MarkToken n
          RuleBndrSig _ n (HsPS _ t) -> do
            walkType ctx e t
            fst <$> bindLocal ctx e scope MarkToken n
      )
      env
      tmvs

-- * Types

-- | A type. Type variables are free here: types and classes resolve,
-- splices are walked.
walkType :: Ctx s -> Env -> LHsType GhcPs -> ST s ()
walkType ctx env (L _ ty) = case ty of
  HsForAllTy _ tele body -> walkTelescope ctx env tele >> go body
  HsQualTy _ (L _ cs) body -> mapM_ go cs >> go body
  HsTyVar _ prom n -> tyName prom n
  HsAppTy _ a b -> go a >> go b
  HsAppKindTy _ a k -> go a >> go k
  HsFunTy _ mult a b -> walkMult ctx env mult >> go a >> go b
  HsListTy _ a -> go a
  HsTupleTy _ _ ts -> mapM_ go ts
  HsSumTy _ ts -> mapM_ go ts
  HsOpTy _ prom a op b -> go a >> tyName prom op >> go b
  HsParTy _ a -> go a
  HsIParamTy _ (L ln (HsIPName f)) t -> do
    put ctx env (locA ln) IPNs Use (ipRef env f)
    go t
  HsStarTy _ _ -> pure ()
  HsKindSig _ a k -> go a >> go k
  HsSpliceTy _ s -> walkSplice ctx env s
  HsDocTy _ a _ -> go a
  HsExplicitListTy _ _ ts -> mapM_ go ts
  HsExplicitTupleTy _ _ ts -> mapM_ go ts
  HsTyLit _ _ -> pure ()
  HsWildCardTy _ -> pure ()
  XHsType ext -> case ext of
    -- Only made by GHC itself.
    HsCoreTy _ -> pure ()
    -- Only while parsing a data declaration, but just in case.
    HsBangTy _ _ t -> go t
    HsRecTy _ fs -> forM_ fs $ \(L _ r) -> case r of
      HsConDeclRecField _ names spec -> do
        forM_ names $ \(L _ (FieldOcc _ n)) -> useName ctx env Use n
        case spec of
          CDF {cdf_type = t} -> go t
  where
    go = walkType ctx env
    -- A promoted name is a constructor.
    tyName prom n@(L l rdr) = case prom of
      NotPromoted -> useName ctx env Use n
      IsPromoted -> useName ctx env Use (L l (promoted rdr))
    promoted rdr = case rdr of
      Unqual o | isTcOcc o -> Unqual (setOccNameSpace dataName o)
      Qual m o | isTcOcc o -> Qual m (setOccNameSpace dataName o)
      _ -> rdr

walkSigType :: Ctx s -> Env -> LHsSigType GhcPs -> ST s ()
walkSigType ctx env (L _ s) = case s of
  HsSig _ outer body -> walkOuter ctx env outer >> walkType ctx env body

walkOuter :: Ctx s -> Env -> HsOuterTyVarBndrs flag GhcPs -> ST s ()
walkOuter ctx env = \case
  HsOuterImplicit _ -> pure ()
  HsOuterExplicit _ bs -> mapM_ (walkTyVarBndr ctx env) bs

walkTelescope :: Ctx s -> Env -> HsForAllTelescope GhcPs -> ST s ()
walkTelescope ctx env = \case
  HsForAllVis _ bs -> mapM_ (walkTyVarBndr ctx env) bs
  HsForAllInvis _ bs -> mapM_ (walkTyVarBndr ctx env) bs

-- | A type variable binder: free, role Def.
walkTyVarBndr :: Ctx s -> Env -> LHsTyVarBndr flag GhcPs -> ST s ()
walkTyVarBndr ctx env (L _ b) = case b of
  HsTvb _ _ var kind -> do
    case var of
      HsBndrVar _ (L l rdr) ->
        put ctx env (locA l) (occNamespace (rdrNameOcc rdr)) Def (FreeR Plain)
      HsBndrWildCard _ -> pure ()
    case kind of
      HsBndrKind _ k -> walkType ctx env k
      HsBndrNoKind _ -> pure ()

walkQTyVars :: Ctx s -> Env -> LHsQTyVars GhcPs -> ST s ()
walkQTyVars ctx env = \case
  HsQTvs _ bs -> mapM_ (walkTyVarBndr ctx env) bs

walkContext :: Ctx s -> Env -> Maybe (LHsContext GhcPs) -> ST s ()
walkContext ctx env = mapM_ (\(L _ cs) -> mapM_ (walkType ctx env) cs)

-- | A type variable written as a name (fundeps, injectivity).
tyVarUse :: Ctx s -> Env -> LIdP GhcPs -> ST s ()
tyVarUse ctx env (L l rdr) =
  put ctx env (locA l) (occNamespace (rdrNameOcc rdr)) Use (FreeR Plain)

-- * Declarations

-- | The header, the export and import lists, the declarations.
walkModule :: Ctx s -> HsModule GhcPs -> ST s ()
walkModule ctx m = do
  forM_ (hsmodName m) moduleWord
  forM_ (hsmodExports m) $ \(L _ ies) -> forM_ ies $ \(L _ ie) -> case ie of
    IEVar _ n _ -> wrapped Export n
    IEThingAbs _ n _ -> wrapped Export n
    IEThingAll _ n _ -> wrapped Export n
    IEThingWith _ n _ subs _ -> wrapped Export n >> mapM_ (sub Export) subs
    IEModuleContents _ mn -> moduleWord mn
    IEGroup {} -> pure ()
    IEDoc {} -> pure ()
    IEDocNamed {} -> pure ()
  forM_ (hsmodImports m) $ \(L _ i) -> case i of
    ImportDecl {ideclName = n, ideclAs = as, ideclImportList = list} -> do
      moduleWord n
      mapM_ moduleWord as
      forM_ list $ \(_, L _ ies) -> forM_ ies $ \(L _ ie) -> case ie of
        IEVar _ n' _ -> imported n'
        IEThingAbs _ n' _ -> imported n'
        IEThingAll _ n' _ -> imported n'
        IEThingWith _ n' _ subs _ -> imported n' >> mapM_ imported subs
        IEModuleContents _ mn -> moduleWord mn
        IEGroup {} -> pure ()
        IEDoc {} -> pure ()
        IEDocNamed {} -> pure ()
  mapM_ (walkDecl ctx emptyEnv) (hsmodDecls m)
  where
    moduleWord (L l _) = put ctx emptyEnv (locA l) ModuleNs Use (NotANameR ModuleWord)
    wrapped role (L _ w) = useEither ctx emptyEnv role (ieWrappedLName w)
    -- A subordinate: a constructor, a field or a method.
    sub role (L _ w) = do
      let n@(L l rdr) = ieWrappedLName w
          occ = rdrNameOcc rdr
          asVar = mkOccNameFS varName (occNameFS occ)
      resolve ctx emptyEnv rdr >>= \case
        (_, FreeR _)
          | Just t <- topLookup ctx asVar,
            not (isVarOcc occ) ->
              put ctx emptyEnv (locA l) t.namespace role (TopLevelR t.target t.certainty)
          | Just (t :| more) <- lookupOccEnv ctx.top.fields asVar ->
              let certainty = if null more then Exact else Heuristic
               in put ctx emptyEnv (locA l) FieldNs role (TopLevelR t certainty)
        _ -> useEither ctx emptyEnv role n
    imported (L _ w) =
      let L l rdr = ieWrappedLName w
       in put ctx emptyEnv (locA l) (occNamespace (rdrNameOcc rdr)) Import (FreeR Plain)

-- | The declarations of a @[d| |]@ quote: its values are local to it,
-- the rest is free.
walkQuoteDecls :: Ctx s -> Env -> [LHsDecl GhcPs] -> ST s ()
walkQuoteDecls ctx env ds = do
  let env0 = env {quote = True}
      scope = case [s | L l _ <- ds, s <- spanL ctx (locA l)] of
        [] -> []
        ss@(a : _) -> [hull a (last ss)]
  (env', ks) <- foldM (declareBind ctx scope) (env0, []) [L l b | L l (ValD _ b) <- ds]
  mapM_ ($ env') (reverse ks)
  forM_ ds $ \d@(L l decl) -> case decl of
    ValD {} -> pure ()
    SigD _ s -> walkSig ctx env' (useName ctx env') Sig (L l s)
    _ -> walkDecl ctx env' d

walkDecl :: Ctx s -> Env -> LHsDecl GhcPs -> ST s ()
walkDecl ctx env (L l decl) = case decl of
  TyClD _ d -> walkTyClDecl ctx env d
  InstD _ d -> walkInstDecl ctx env d
  DerivD _ d -> case d of
    DerivDecl _ (HsWC _ t) strategy _ -> do
      mapM_ (walkStrategy ctx env) strategy
      walkSigType ctx env t
  ValD _ b -> walkTopBind ctx env (locA l) b
  SigD _ s -> walkSig ctx env (useName ctx env) Sig (L l s)
  KindSigD _ k -> case k of
    StandaloneKindSig _ n t -> useName ctx env Sig n >> walkSigType ctx env t
  DefD _ d -> case d of
    DefaultDecl _ cls ts -> mapM_ (useName ctx env Use) cls >> mapM_ (walkType ctx env) ts
  ForD _ d -> case d of
    ForeignImport _ n t _ -> defName ctx env Def n >> walkSigType ctx env t
    ForeignExport _ n t _ -> useName ctx env Sig n >> walkSigType ctx env t
  WarningD _ ws -> case ws of
    Warnings _ ds -> forM_ ds $ \(L _ w) -> case w of
      Warning _ ns _ -> mapM_ (useEither ctx env Sig) ns
  AnnD _ a -> case a of
    HsAnnotation _ prov e -> do
      case prov of
        ValueAnnProvenance n -> useEither ctx env Use n
        TypeAnnProvenance n -> useEither ctx env Use n
        ModuleAnnProvenance -> pure ()
      walkExpr ctx env e
  RuleD _ rs -> case rs of
    HsRules _ rules -> forM_ rules $ \(L lr r) -> case r of
      HsRule {rd_bndrs = bndrs, rd_lhs = lhs, rd_rhs = rhs} -> do
        env' <- ruleBinders ctx env (spanL ctx (locA lr)) bndrs
        walkExpr ctx env' lhs
        walkExpr ctx env' rhs
  SpliceD _ s -> case s of
    SpliceDecl _ (L _ sp) _ -> walkSplice ctx env sp
  DocD _ _ -> pure ()
  RoleAnnotD _ r -> case r of
    RoleAnnotDecl _ n _ -> useName ctx env Use n

walkStrategy :: Ctx s -> Env -> LDerivStrategy GhcPs -> ST s ()
walkStrategy ctx env (L _ s) = case s of
  StockStrategy _ -> pure ()
  AnyclassStrategy _ -> pure ()
  NewtypeStrategy _ -> pure ()
  ViaStrategy (XViaStrategyPs _ t) -> walkSigType ctx env t

-- | A top-level binding: its names are the module's.
walkTopBind :: Ctx s -> Env -> SrcSpan -> HsBind GhcPs -> ST s ()
walkTopBind ctx env l = \case
  FunBind _ _ mg -> walkMatches ctx env Nothing (defName ctx env Def) (walkExpr ctx) mg
  PatBind _ pat mult grhss -> do
    env' <- walkPat ctx BindTop env pat
    walkMult ctx env' mult
    walkGRHSs ctx env' (spanL ctx l) (walkExpr ctx) grhss
  VarBind {} -> pure ()
  PatSynBind _ psb -> walkPatSyn ctx env psb

-- | A pattern synonym: its arguments are in scope in the right-hand
-- side, where they are uses.
walkPatSyn :: Ctx s -> Env -> PatSynBind GhcPs GhcPs -> ST s ()
walkPatSyn ctx env = \case
  PSB _ n args def dir -> do
    defName ctx env Def n
    let scope = case (spanL ctx (getLocA n), spanL ctx (getLocA def)) of
          ([a], [b]) -> [hull a b]
          _ -> []
    let bind e v = fst <$> bindLocal ctx e scope MarkToken v
        -- Its own fields, as for a constructor.
        ci = if env.quote then ForeignCon Nothing else conInfo ctx env n
    env' <- case args of
      PrefixCon vs -> foldM bind env vs
      InfixCon a b -> bind env a >>= \e -> bind e b
      RecCon fs ->
        foldM
          ( \e (RecordPatSynField fo v) -> case fo of
              FieldOcc _ (L _ rdr) -> bindPun ctx e scope (conFieldRef ci rdr) v
          )
          env
          fs
    void (walkPat ctx UseVars env' def)
    case dir of
      Unidirectional -> pure ()
      ImplicitBidirectional -> pure ()
      ExplicitBidirectional mg ->
        walkMatches ctx env Nothing (defName ctx env Def) (walkExpr ctx) mg

walkTyClDecl :: Ctx s -> Env -> TyClDecl GhcPs -> ST s ()
walkTyClDecl ctx env = \case
  FamDecl _ f -> walkFamily ctx env f
  SynDecl _ n tvs _ rhs -> do
    defName ctx env Def n
    walkQTyVars ctx env tvs
    walkType ctx env rhs
  DataDecl _ n tvs _ defn -> do
    defName ctx env Def n
    walkQTyVars ctx env tvs
    walkDataDefn ctx env defn
  ClassDecl
    { tcdCtxt = cxt,
      tcdLName = n,
      tcdTyVars = tvs,
      tcdFDs = fds,
      tcdSigs = sigs,
      tcdMeths = meths,
      tcdATs = ats,
      tcdATDefs = defs
    } -> do
      walkContext ctx env cxt
      defName ctx env Def n
      walkQTyVars ctx env tvs
      forM_ fds $ \(L _ fd) -> case fd of
        FunDep _ as bs -> mapM_ (tyVarUse ctx env) (as <> bs)
      mapM_ (walkSig ctx env (defOrUse ctx env) Def) sigs
      -- Default methods: definitions of the class's methods.
      forM_ meths $ \(L lb b) -> walkTopBind ctx env (locA lb) b
      mapM_ (walkFamily ctx env . unLoc) ats
      forM_ defs $ \(L _ d) -> walkTyFamInst ctx env d

-- | In a class: Def for a method's signature, else what the name is.
defOrUse :: Ctx s -> Env -> Role -> LIdP GhcPs -> ST s ()
defOrUse ctx env role = case role of
  Def -> defName ctx env Def
  _ -> useName ctx env role

walkFamily :: Ctx s -> Env -> FamilyDecl GhcPs -> ST s ()
walkFamily ctx env = \case
  FamilyDecl _ info _ n tvs _ (L _ res) inj -> do
    defName ctx env Def n
    walkQTyVars ctx env tvs
    case res of
      NoSig _ -> pure ()
      KindSig _ k -> walkType ctx env k
      TyVarSig _ b -> walkTyVarBndr ctx env b
    forM_ inj $ \(L _ i) -> case i of
      InjectivityAnn _ a bs -> mapM_ (tyVarUse ctx env) (a : bs)
    case info of
      DataFamily -> pure ()
      OpenTypeFamily -> pure ()
      ClosedTypeFamily eqns -> forM_ (fromMaybe [] eqns) $ \(L _ e) ->
        walkFamEqn ctx env (walkType ctx env) e

walkFamEqn :: Ctx s -> Env -> (rhs -> ST s ()) -> FamEqn GhcPs rhs -> ST s ()
walkFamEqn ctx env rhs = \case
  FamEqn _ tycon bndrs pats _ r -> do
    useName ctx env Use tycon
    walkOuter ctx env bndrs
    forM_ pats $ \case
      HsValArg _ t -> walkType ctx env t
      HsTypeArg _ k -> walkType ctx env k
      HsArgPar _ -> pure ()
    rhs r

walkTyFamInst :: Ctx s -> Env -> TyFamInstDecl GhcPs -> ST s ()
walkTyFamInst ctx env = \case
  TyFamInstDecl _ e -> walkFamEqn ctx env (walkType ctx env) e

walkDataFamInst :: Ctx s -> Env -> DataFamInstDecl GhcPs -> ST s ()
walkDataFamInst ctx env (DataFamInstDecl e) = walkFamEqn ctx env (walkDataDefn ctx env) e

walkDataDefn :: Ctx s -> Env -> HsDataDefn GhcPs -> ST s ()
walkDataDefn ctx env = \case
  HsDataDefn {dd_ctxt = cxt, dd_kindSig = k, dd_cons = cons, dd_derivs = derivs} -> do
    walkContext ctx env cxt
    mapM_ (walkType ctx env) k
    mapM_ (walkConDecl ctx env) (toList cons)
    forM_ derivs $ \(L _ d) -> case d of
      HsDerivingClause _ strategy (L _ tys) -> do
        mapM_ (walkStrategy ctx env) strategy
        case tys of
          DctSingle _ t -> walkSigType ctx env t
          DctMulti _ ts -> mapM_ (walkSigType ctx env) ts

walkConDecl :: Ctx s -> Env -> LConDecl GhcPs -> ST s ()
walkConDecl ctx env (L _ c) = case c of
  ConDeclH98 {con_name = n, con_ex_tvs = tvs, con_mb_cxt = cxt, con_args = args} -> do
    defName ctx env Def n
    mapM_ (walkTyVarBndr ctx env) tvs
    walkContext ctx env cxt
    case args of
      PrefixCon fs -> mapM_ field fs
      InfixCon a b -> field a >> field b
      RecCon (L _ rs) -> mapM_ (recField n) rs
  ConDeclGADT
    { con_names = ns,
      con_outer_bndrs = L _ outer,
      con_inner_bndrs = inner,
      con_mb_cxt = cxt,
      con_g_args = args,
      con_res_ty = res
    } -> do
      mapM_ (defName ctx env Def) ns
      walkOuter ctx env outer
      mapM_ (walkTelescope ctx env) inner
      walkContext ctx env cxt
      case args of
        PrefixConGADT _ fs -> mapM_ field fs
        RecConGADT _ (L _ rs) -> mapM_ (recField (NonEmpty.head ns)) rs
      walkType ctx env res
  where
    field = \case
      CDF {cdf_multiplicity = mult, cdf_type = t} -> do
        walkMult ctx env mult
        walkType ctx env t
    recField con (L _ r) = case r of
      HsConDeclRecField _ names spec -> do
        let ci = if env.quote then ForeignCon Nothing else conInfo ctx env con
        forM_ names $ \(L _ fo) -> case fo of
          FieldOcc _ (L l rdr) -> put ctx env (locA l) FieldNs Def (conFieldRef ci rdr)
        field spec

walkInstDecl :: Ctx s -> Env -> InstDecl GhcPs -> ST s ()
walkInstDecl ctx env = \case
  ClsInstD _ i -> walkClsInst ctx env i
  DataFamInstD _ d -> walkDataFamInst ctx env d
  TyFamInstD _ d -> walkTyFamInst ctx env d

-- | An instance: its methods are the class's, if the class is from
-- here; else free, with the class's token (GHC finds the class with
-- getLHsInstDeclClass_maybe).
walkClsInst :: Ctx s -> Env -> ClsInstDecl GhcPs -> ST s ()
walkClsInst ctx env = \case
  ClsInstDecl
    { cid_poly_ty = poly,
      cid_binds = binds,
      cid_sigs = sigs,
      cid_tyfam_insts = tyfams,
      cid_datafam_insts = datafams
    } -> do
      walkSigType ctx env poly
      let cls = getLHsInstDeclClass_maybe poly
          anchor = cls >>= \(L l _) -> tokenAtLoc ctx (locA l)
          local = case cls of
            Just (L _ rdr)
              | not env.quote,
                isLocalName rdr ->
                  lookupOccEnv ctx.top.methods (rdrNameOcc rdr)
            _ -> Nothing
          methodRef occ = case local of
            Just ms
              | Just t <- lookupOccEnv ms (mkOccNameFS varName (occNameFS occ)) ->
                  TopLevelR t Exact
            _ -> FreeR (maybe Plain MethodOfClass anchor)
          method role (L l rdr) =
            put ctx env (locA l) VarNs role (methodRef (rdrNameOcc rdr))
      forM_ binds $ \(L lb b) -> case b of
        FunBind _ _ mg ->
          walkMatches ctx env Nothing (method InstanceDef) (walkExpr ctx) mg
        _ -> walkTopBind ctx env (locA lb) b
      mapM_ (walkSig ctx env method Sig) sigs
      forM_ tyfams $ \(L _ d) -> walkTyFamInst ctx env d
      forM_ datafams $ \(L _ d) -> walkDataFamInst ctx env d
  where
    isLocalName = \case
      Unqual _ -> True
      Qual m _ -> m == ctx.self
      _ -> False

-- * The rest

-- | Every identifier token with no entry names nothing.
notNames :: Ctx s -> ST s ()
notNames ctx = forM_ [0 .. ctx.count - 1] $ \i ->
  when (nameLikeClass (tokenClassAt ctx.tokens i)) $ do
    written <- isRecorded ctx.builder (TokenIx i)
    unless written $
      record ctx.builder (TokenIx i) OtherNs Use (NotANameR OtherWord) False
