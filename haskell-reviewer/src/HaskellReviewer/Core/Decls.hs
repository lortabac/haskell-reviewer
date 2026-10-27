{-# LANGUAGE StrictData #-}

-- | Declarations in the module model (SPEC §4.2, §4.3, §11.4.4): types,
-- classes, values, pattern synonyms, instances, with ids, extents,
-- Haddock and the exported flag. Also the module's Pragmas block.
module HaskellReviewer.Core.Decls
  ( -- * Shared
    Located (..),
    DocComment (..),
    WarningText (..),
    WarningKind (..),

    -- * Declarations
    Decl (..),
    DeclInfo (..),
    declInfo,
    declExtent,

    -- * Extents
    Extent (parts),
    Part (..),
    PartRole (..),
    mkExtent,
    headTokens,
    headPieces,

    -- * Types
    TypeDecl (..),
    TypeForm (..),
    DataBody (..),
    NewtypeBody (..),
    formConstructors,
    ConDecl (..),
    ConArgs (..),
    ConArg (..),
    ArgType (..),
    Strictness (..),
    Unpack (..),
    FieldDecl (..),
    OpenFamilyBody (..),
    DerivingClause (..),
    DerivedClass (..),
    DerivStrategy (..),

    -- * Classes
    ClassDecl (..),
    MethodDecl (..),

    -- * Values
    ValueDecl (..),
    Definition (..),
    Signature (..),
    Fixity (..),
    FixityDir (..),

    -- * Pattern synonyms
    PatSynDecl (..),
    PatSynDir (..),

    -- * Instances
    InstanceDecl (..),
    InstanceForm (..),
    Derived (..),
    InstanceBody (..),
    InstanceMethod (..),
    FamInstDecl (..),
    FamInstForm (..),
    famInstConstructors,

    -- * Pragmas block
    ModulePragma (..),
    PragmaKind (..),
    AnnTarget (..),

    -- * Forcing
    forceDecls,
    forcePragmas,
    forceWarningText,
    forceDocComment,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import HaskellReviewer.Core.Names (DeclId, forceDeclId)
import HaskellReviewer.Core.RichText (Doc, forceDoc)
import HaskellReviewer.Core.SourceText (SourceLines, sliceSpan)
import HaskellReviewer.Core.Span (Span (..))
import HaskellReviewer.Core.Tokens (Token (..), TokenClass (..), Tokens, tokensIn)

-- * Shared

-- | A value and where it is.
data Located a = Located
  { span :: Span,
    value :: a
  }
  deriving (Eq, Show)

-- | A Haddock comment: the text as written, without the @|@ or @^@, and
-- what it says. The text is what the wire sends for now.
data DocComment a = DocComment
  { span :: Span,
    text :: Text,
    -- | A 'Doc', or inline text for a section heading.
    parsed :: a
  }
  deriving (Eq, Show)

-- | @{-# DEPRECATED "..." #-}@ or @{-# WARNING "..." #-}@: on the module,
-- an export or a declaration.
data WarningText = WarningText
  { span :: Span,
    kind :: WarningKind,
    messages :: [Text]
  }
  deriving (Eq, Show)

-- | Which pragma.
data WarningKind
  = DeprecatedPragma
  | -- | With its category, if given (@WARNING in "x-foo"@).
    WarningPragma (Maybe Text)
  deriving (Eq, Show)

-- * Declarations

-- | A top-level declaration.
data Decl
  = TypeD TypeDecl
  | ClassD ClassDecl
  | ValueD ValueDecl
  | PatSynD PatSynDecl
  | -- | An instance: written, standalone deriving, or from a deriving
    -- clause.
    InstanceD InstanceDecl
  | -- | A @type instance@ or @data instance@.
    FamInstD FamInstDecl
  | -- | A top-level splice. It has no name, so no id.
    SpliceD Extent
  deriving (Eq, Show)

-- | What every declaration has, nested ones too.
data DeclInfo = DeclInfo
  { id :: DeclId,
    -- | Where the name is first defined or declared.
    nameSpan :: Span,
    extent :: Extent,
    -- | The doc comments GHC pairs with it, in order (Note [Haddock
    -- pairing] in "HaskellReviewer.Core.Parser.ConvertDecls").
    haddock :: [DocComment Doc],
    -- | A @DEPRECATED@ or @WARNING@ pragma on its name.
    warning :: Maybe WarningText,
    exported :: Bool
  }
  deriving (Eq, Show)

-- | The info of a top-level declaration. A splice has none.
declInfo :: Decl -> Maybe DeclInfo
declInfo = \case
  TypeD t -> Just t.info
  ClassD c -> Just c.info
  ValueD v -> Just v.info
  PatSynD p -> Just p.info
  InstanceD i -> Just i.info
  FamInstD f -> Just f.info
  SpliceD _ -> Nothing

-- | The extent of a top-level declaration.
declExtent :: Decl -> Extent
declExtent = \case
  TypeD t -> t.info.extent
  ClassD c -> c.info.extent
  ValueD v -> v.info.extent
  PatSynD p -> p.info.extent
  InstanceD i -> i.info.extent
  FamInstD f -> f.info.extent
  SpliceD e -> e

-- * Extents

{- Note [Extents]

An extent is the source of one declaration (SPEC §4.3): a signature,
its pragmas, its equations, its doc comments. They need not be next to
each other, so it's a list of parts, sorted, with no overlaps.

Each part has a role, for the semantic diff (SPEC §4.6): the head is
the type-level part (signature, data declaration, deriving clauses,
fixity), the body the implementation (equations). Pragmas and doc
comments have their own roles.

- types, synonyms, families, kind signatures: head;
- values: signature and fixity head, equations body, INLINE and
  friends pragma;
- classes: the class minus the default method equations and method
  pragmas is head; those equations are body, those pragmas pragma.
  MINIMAL stays in the head: it says what instances must define, so it
  is API;
- pattern synonyms: signature head, definition body;
- instances: the instance minus the method equations and pragmas is
  head (so the @instance ... where@ line, instance signatures,
  associated instances); those equations are body; method pragmas and
  @SPECIALISE instance@ are pragma;
- an instance from a deriving clause: the class and the strategy, head;
- standalone deriving, type and data instances, foreign imports and
  exports, role annotations, splices: head;
- a @DEPRECATED@ or @WARNING@ pragma on a name: pragma.

Doc comments inside a head (a constructor's, an argument's) stay in the
head: their tokens are Haddock tokens, so comparing heads skips them.
-}

-- | Sorted, no overlaps. See Note [Extents].
newtype Extent = Extent {parts :: NonEmpty Part}
  deriving (Eq, Show)

-- | One range of an extent.
data Part = Part
  { span :: Span,
    role :: PartRole
  }
  deriving (Eq, Show)

-- | What a part is. See Note [Extents].
data PartRole = HeadPart | BodyPart | PragmaPart | DocPart
  deriving (Eq, Show)

-- | Sort the parts and drop overlaps: a part inside an earlier one goes,
-- a part that starts inside an earlier one starts at its end.
mkExtent :: NonEmpty Part -> Extent
mkExtent ps = Extent (first :| go first.span rest)
  where
    first :| rest = NonEmpty.sortOn start ps
    start (p :: Part) = (p.span.startLine, p.span.startCol)
    end (s :: Span) = (s.endLine, s.endCol)
    go :: Span -> [Part] -> [Part]
    go _ [] = []
    go prev (p : more)
      | end p.span <= end prev = go prev more
      | start p < end prev =
          let s = p.span
              cut = s {startLine = prev.endLine, startCol = prev.endCol}
              p' = Part {span = cut, role = p.role}
           in p' : go p'.span more
      | otherwise = p : go p.span more

-- | The tokens of the head parts, comments and Haddock left out.
headTokens :: Tokens -> Extent -> [Token]
headTokens ts e =
  [ t
  | p <- NonEmpty.toList e.parts,
    p.role == HeadPart,
    t <- tokensIn p.span ts,
    t.kind `notElem` [Comment, Haddock]
  ]

-- | The text of each head token.
headPieces :: SourceLines -> Tokens -> Extent -> [Text]
headPieces src ts e = [sliceSpan src t.span | t <- headTokens ts e]

-- * Types

-- | @data@, @newtype@, @type@, a family.
data TypeDecl = TypeDecl
  { info :: DeclInfo,
    form :: TypeForm,
    -- | @type T :: K@.
    kindSig :: Maybe Span,
    -- | @type role T nominal@.
    roles :: Maybe Span,
    fixity :: Maybe Fixity
  }
  deriving (Eq, Show)

-- | Which kind of type declaration, with what only that kind has.
data TypeForm
  = DataType DataBody
  | Newtype NewtypeBody
  | -- | @type data@.
    TypeData DataBody
  | Synonym
  | OpenFamily OpenFamilyBody
  | -- | With the ids of its instances in this module.
    DataFamily [DeclId]
  | -- | With its equations.
    ClosedFamily [Span]
  deriving (Eq, Show)

-- | What an open type family has.
data OpenFamilyBody = OpenFamilyBody
  { -- | The defaults, when it's associated.
    defaults :: [Span],
    -- | The ids of its instances in this module, top-level or in an
    -- instance.
    instances :: [DeclId]
  }
  deriving (Eq, Show)

-- | The constructors and deriving clauses of a @data@ or @type data@.
data DataBody = DataBody
  { constructors :: [ConDecl],
    derivings :: [DerivingClause]
  }
  deriving (Eq, Show)

-- | A newtype has one constructor.
data NewtypeBody = NewtypeBody
  { constructor :: ConDecl,
    derivings :: [DerivingClause]
  }
  deriving (Eq, Show)

-- | The constructors of a type, if it has any.
formConstructors :: TypeForm -> [ConDecl]
formConstructors = \case
  DataType b -> b.constructors
  TypeData b -> b.constructors
  Newtype b -> [b.constructor]
  _ -> []

-- | A data constructor. A GADT line with several names gives one each.
data ConDecl = ConDecl
  { info :: DeclInfo,
    args :: ConArgs,
    fixity :: Maybe Fixity
  }
  deriving (Eq, Show)

-- | The constructor's shape and its arguments.
data ConArgs
  = PrefixArgs [ConArg]
  | InfixArgs ConArg ConArg
  | RecordArgs [FieldDecl]
  | GadtArgs [ConArg]
  | GadtRecordArgs [FieldDecl]
  deriving (Eq, Show)

-- | One positional argument.
data ConArg = ConArg
  { type_ :: ArgType,
    doc :: Maybe (DocComment Doc)
  }
  deriving (Eq, Show)

-- | An argument's or field's type, with its marks.
data ArgType = ArgType
  { type_ :: Span,
    strictness :: Strictness,
    unpack :: Unpack
  }
  deriving (Eq, Show)

-- | @!@ or @~@.
data Strictness = Unmarked | Strict | Lazy
  deriving (Eq, Show)

-- | @{-# UNPACK #-}@ or @{-# NOUNPACK #-}@.
data Unpack = NoUnpackPragma | Unpack | NoUnpack
  deriving (Eq, Show)

-- | A record field of one constructor. A field in several constructors
-- has one of these in each, with the same id. Its doc is in the info.
data FieldDecl = FieldDecl
  { info :: DeclInfo,
    type_ :: ArgType
  }
  deriving (Eq, Show)

-- | A @deriving@ clause.
data DerivingClause = DerivingClause
  { span :: Span,
    strategy :: DerivStrategy,
    -- | Where the strategy is written, with the via type.
    strategySpan :: Maybe Span,
    classes :: [DerivedClass]
  }
  deriving (Eq, Show)

-- | A class in a deriving clause. Each one is also an instance, an
-- 'InstanceD' of its own.
data DerivedClass = DerivedClass
  { -- | As written (@Show@, @MonadState S@).
    span :: Span,
    -- | The instance's id.
    id :: DeclId
  }
  deriving (Eq, Show)

-- | How the instances are derived.
data DerivStrategy
  = NoStrategy
  | StockStrategy
  | NewtypeStrategy
  | AnyclassStrategy
  | -- | With the type.
    ViaStrategy Span
  deriving (Eq, Show)

-- * Classes

-- | A class.
data ClassDecl = ClassDecl
  { info :: DeclInfo,
    -- | The superclasses.
    context :: Maybe Span,
    fundeps :: [Span],
    kindSig :: Maybe Span,
    roles :: Maybe Span,
    fixity :: Maybe Fixity,
    methods :: [MethodDecl],
    associated :: [TypeDecl],
    minimal :: Maybe Span
  }
  deriving (Eq, Show)

-- | A class method.
data MethodDecl = MethodDecl
  { info :: DeclInfo,
    signature :: Signature,
    -- | @default m :: ...@.
    defaultSig :: Maybe Span,
    -- | Has a default definition.
    hasDefault :: Bool,
    fixity :: Maybe Fixity
  }
  deriving (Eq, Show)

-- * Values

-- | A function or a value.
data ValueDecl = ValueDecl
  { info :: DeclInfo,
    -- | Always there for 'NoDefinition' and 'ForeignImport'.
    signature :: Maybe Signature,
    definition :: Definition,
    -- | @foreign export@ declarations of it.
    foreignExports :: [Span],
    fixity :: Maybe Fixity
  }
  deriving (Eq, Show)

-- | How a value is defined.
data Definition
  = Equations
  | -- | Bound by a pattern, maybe with other names.
    PatternBound
  | -- | @foreign import@. The signature is the whole declaration.
    ForeignImport
  | -- | A signature alone (a .hsig, or a missing binding). The
    -- signature is then always there.
    NoDefinition
  deriving (Eq, Show)

-- | A type signature. One shared by several names (@f, g :: T@) is the
-- same value under each.
data Signature = Signature
  { span :: Span,
    names :: NonEmpty Text
  }
  deriving (Eq, Show)

-- | The fixity declaration for it.
data Fixity = Fixity
  { span :: Span,
    direction :: FixityDir,
    precedence :: Int
  }
  deriving (Eq, Show)

-- | @infixl@, @infixr@ or @infix@.
data FixityDir = InfixLeft | InfixRight | InfixNone
  deriving (Eq, Show)

-- * Pattern synonyms

-- | A pattern synonym.
data PatSynDecl = PatSynDecl
  { info :: DeclInfo,
    signature :: Maybe Signature,
    direction :: PatSynDir,
    -- | Record pattern synonym fields.
    fields :: [DeclInfo],
    fixity :: Maybe Fixity
  }
  deriving (Eq, Show)

-- | Which way a pattern synonym works.
data PatSynDir
  = -- | @pattern P <- ...@.
    Unidirectional
  | -- | @pattern P = ...@.
    ImplicitBidirectional
  | -- | @pattern P <- ... where P = ...@.
    ExplicitBidirectional
  deriving (Eq, Show)

-- * Instances

-- | An instance. Its id is the instance key (D-20).
data InstanceDecl = InstanceDecl
  { info :: DeclInfo,
    -- | Neither the class nor any type in the head is from this module
    -- (Note [Orphans] in "HaskellReviewer.Core.Parser.InstanceKey").
    -- Never for one from a deriving clause.
    orphan :: Bool,
    form :: InstanceForm
  }
  deriving (Eq, Show)

-- | Where an instance comes from.
data InstanceForm
  = -- | @instance ... where ...@.
    ExplicitInstance InstanceBody
  | -- | @deriving instance ...@.
    StandaloneDeriving DerivStrategy
  | -- | A class in a deriving clause.
    DerivedInstance Derived
  deriving (Eq, Show)

-- | How a deriving clause instance was made.
data Derived = Derived
  { strategy :: DerivStrategy,
    -- | The type (or data instance) the clause is on.
    onType :: DeclId
  }
  deriving (Eq, Show)

-- | What an @instance@ defines.
data InstanceBody = InstanceBody
  { methods :: [InstanceMethod],
    -- | Type and data instances inside it.
    associated :: [FamInstDecl]
  }
  deriving (Eq, Show)

-- | A method defined in an instance. It has no id of its own, and no
-- Haddock (GHC drops docs in instance bodies).
data InstanceMethod = InstanceMethod
  { name :: Located Text,
    extent :: Extent
  }
  deriving (Eq, Show)

-- | A @type instance@ or @data instance@, top-level or in an instance.
-- Its id is @inst:<Family>:<Head>@. Always exported.
data FamInstDecl = FamInstDecl
  { info :: DeclInfo,
    -- | The family's name.
    family :: Text,
    form :: FamInstForm,
    -- | Neither the family nor any type in the head is from this
    -- module.
    orphan :: Bool
  }
  deriving (Eq, Show)

-- | Which kind of family instance. Data instance fields have the first
-- constructor as parent (D-88).
data FamInstForm
  = TypeInstance
  | DataInstance DataBody
  | NewtypeInstance NewtypeBody
  deriving (Eq, Show)

-- | The constructors of a family instance.
famInstConstructors :: FamInstForm -> [ConDecl]
famInstConstructors = \case
  DataInstance b -> b.constructors
  NewtypeInstance b -> [b.constructor]
  TypeInstance -> []

-- * Pragmas block

-- | A pragma or declaration shown in the Pragmas block at the end of the
-- module page (SPEC §4.2).
data ModulePragma = ModulePragma
  { span :: Span,
    kind :: PragmaKind
  }
  deriving (Eq, Show)

-- | Which one, with the names it mentions.
data PragmaKind
  = -- | @{-# RULES ... #-}@, with the rule names.
    Rules [Text]
  | -- | @{-# COMPLETE P, Q :: T #-}@.
    Complete [Text] (Maybe Text)
  | -- | @{-# ANN ... #-}@.
    Annotation AnnTarget
  | -- | @default (Int)@, with the class for NamedDefaults.
    Default (Maybe Text)
  deriving (Eq, Show)

-- | What an @ANN@ pragma is on.
data AnnTarget = AnnValue Text | AnnType Text | AnnModule
  deriving (Eq, Show)

-- * Forcing

-- | Walk every list and 'Maybe', so nothing holds a thunk. StrictData
-- does the rest.
forceDecls :: [Decl] -> ()
forceDecls = each decl
  where
    decl = \case
      TypeD t -> typeDecl t
      ClassD c -> classDecl c
      ValueD v -> valueDecl v
      PatSynD p -> patSyn p
      InstanceD i -> instanceDecl i
      FamInstD f -> famInst f
      SpliceD e -> extent e
    extent e = list (NonEmpty.toList e.parts)
    info i =
      forceDeclId i.id `seq`
        extent i.extent `seq`
          each forceDocComment i.haddock `seq`
            maybe () forceWarningText i.warning
    typeDecl t =
      info t.info `seq`
        form t.form `seq`
          whnf t.kindSig `seq`
            whnf t.roles `seq`
              whnf t.fixity
    form = \case
      DataType b -> dataBody b
      Newtype b -> newtypeBody b
      TypeData b -> dataBody b
      OpenFamily b -> list b.defaults `seq` each forceDeclId b.instances
      ClosedFamily ss -> list ss
      Synonym -> ()
      DataFamily is -> each forceDeclId is
    dataBody b = each con b.constructors `seq` each deriving_ b.derivings
    newtypeBody b = con b.constructor `seq` each deriving_ b.derivings
    con c = info c.info `seq` conArgs c.args `seq` whnf c.fixity
    conArgs = \case
      PrefixArgs as -> each arg as
      InfixArgs a b -> arg a `seq` arg b
      RecordArgs fs -> each field fs
      GadtArgs as -> each arg as
      GadtRecordArgs fs -> each field fs
    arg a = maybe () forceDocComment a.doc
    field f = info f.info
    deriving_ d = whnf d.strategySpan `seq` each (forceDeclId . (.id)) d.classes
    classDecl c =
      info c.info `seq`
        whnf c.context `seq`
          list c.fundeps `seq`
            whnf c.kindSig `seq`
              whnf c.roles `seq`
                whnf c.fixity `seq`
                  each method c.methods `seq`
                    each typeDecl c.associated `seq`
                      whnf c.minimal
    method m =
      info m.info `seq` signature m.signature `seq` whnf m.defaultSig `seq` whnf m.fixity
    signature s = list (NonEmpty.toList s.names)
    valueDecl v =
      info v.info `seq`
        maybe () signature v.signature `seq`
          list v.foreignExports `seq`
            whnf v.fixity
    patSyn p =
      info p.info `seq`
        maybe () signature p.signature `seq`
          each info p.fields `seq`
            whnf p.fixity
    instanceDecl i = info i.info `seq` instanceForm i.form
    instanceForm = \case
      ExplicitInstance b -> each method_ b.methods `seq` each famInst b.associated
      StandaloneDeriving _ -> ()
      DerivedInstance d -> forceDeclId d.onType
    method_ (m :: InstanceMethod) = extent m.extent
    famInst f = info f.info `seq` famInstForm f.form
    famInstForm = \case
      TypeInstance -> ()
      DataInstance b -> dataBody b
      NewtypeInstance b -> newtypeBody b

-- | Like 'forceDecls', for the Pragmas block.
forcePragmas :: [ModulePragma] -> ()
forcePragmas = each (kind . (.kind))
  where
    kind = \case
      Rules ns -> list ns
      Complete ns t -> list ns `seq` whnf t
      Annotation _ -> ()
      Default c -> whnf c

-- | Like 'forceDecls', for a warning.
forceWarningText :: WarningText -> ()
forceWarningText w = list w.messages `seq` kind w.kind
  where
    kind = \case
      WarningPragma c -> whnf c
      DeprecatedPragma -> ()

-- | Like 'forceDecls', for a doc comment.
forceDocComment :: DocComment Doc -> ()
forceDocComment d = forceDoc d.parsed

-- | Force each element with the function.
each :: (a -> ()) -> [a] -> ()
each f = foldr (\x r -> f x `seq` r) ()

-- | Each element to WHNF. With StrictData that is often all of it.
list :: [a] -> ()
list = each (`seq` ())

-- | The value inside to WHNF.
whnf :: Maybe a -> ()
whnf = maybe () (`seq` ())
