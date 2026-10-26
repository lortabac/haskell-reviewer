{-# LANGUAGE StrictData #-}

-- | Declarations in the module model (SPEC §4.2, §4.3, §11.4.4): types,
-- classes, values and pattern synonyms, with ids, extents, Haddock and
-- the exported flag.
module HaskellReviewer.Core.Decls
  ( -- * Shared
    Located (..),
    RawDoc (..),

    -- * Declarations
    Decl (..),
    DeclInfo (..),
    declInfo,

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
    ConDecl (..),
    ConArgs (..),
    ConArg (..),
    ArgType (..),
    Strictness (..),
    Unpack (..),
    FieldDecl (..),
    DerivingClause (..),
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

    -- * Forcing
    forceDecls,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import HaskellReviewer.Core.Names (DeclId)
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

-- | A Haddock comment as written, without the @|@ or @^@.
data RawDoc = RawDoc
  { span :: Span,
    text :: Text
  }
  deriving (Eq, Show)

-- * Declarations

-- | A top-level declaration.
data Decl
  = TypeD TypeDecl
  | ClassD ClassDecl
  | ValueD ValueDecl
  | PatSynD PatSynDecl
  deriving (Eq, Show)

-- | What every declaration has, nested ones too.
data DeclInfo = DeclInfo
  { id :: DeclId,
    -- | Where the name is first defined or declared.
    nameSpan :: Span,
    extent :: Extent,
    -- | The doc comments GHC pairs with it, in order (Note [Haddock
    -- pairing] in "HaskellReviewer.Core.Parser.ConvertDecls").
    haddock :: [RawDoc],
    exported :: Bool
  }
  deriving (Eq, Show)

-- | The info of a top-level declaration.
declInfo :: Decl -> DeclInfo
declInfo = \case
  TypeD t -> t.info
  ClassD c -> c.info
  ValueD v -> v.info
  PatSynD p -> p.info

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
- pattern synonyms: signature head, definition body.

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
  | -- | An open type family, with its defaults when it's associated.
    OpenFamily [Span]
  | DataFamily
  | -- | With its equations.
    ClosedFamily [Span]
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
    doc :: Maybe RawDoc
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
    -- | Each class, as written (@Show@, @MonadState S@).
    classes :: [Span]
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
    -- | Always there for 'NoDefinition'.
    signature :: Maybe Signature,
    definition :: Definition,
    fixity :: Maybe Fixity
  }
  deriving (Eq, Show)

-- | How a value is defined.
data Definition
  = Equations
  | -- | Bound by a pattern, maybe with other names.
    PatternBound
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
    info i = list (NonEmpty.toList i.extent.parts) `seq` list i.haddock
    typeDecl t = info t.info `seq` form t.form `seq` whnf t.kindSig `seq` whnf t.fixity
    form = \case
      DataType b -> dataBody b
      Newtype b -> con b.constructor `seq` each deriving_ b.derivings
      TypeData b -> dataBody b
      OpenFamily ss -> list ss
      ClosedFamily ss -> list ss
      Synonym -> ()
      DataFamily -> ()
    dataBody b = each con b.constructors `seq` each deriving_ b.derivings
    con c = info c.info `seq` conArgs c.args `seq` whnf c.fixity
    conArgs = \case
      PrefixArgs as -> each arg as
      InfixArgs a b -> arg a `seq` arg b
      RecordArgs fs -> each field fs
      GadtArgs as -> each arg as
      GadtRecordArgs fs -> each field fs
    arg a = whnf a.doc
    field f = info f.info
    deriving_ d = list d.classes
    classDecl c =
      info c.info `seq`
        whnf c.context `seq`
          list c.fundeps `seq`
            whnf c.kindSig `seq`
              whnf c.fixity `seq`
                each method c.methods `seq`
                  each typeDecl c.associated `seq`
                    whnf c.minimal
    method m =
      info m.info `seq` signature m.signature `seq` whnf m.defaultSig `seq` whnf m.fixity
    signature s = list (NonEmpty.toList s.names)
    valueDecl v = info v.info `seq` maybe () signature v.signature `seq` whnf v.fixity
    patSyn p =
      info p.info `seq`
        maybe () signature p.signature `seq`
          each info p.fields `seq`
            whnf p.fixity
    each f = foldr (\x r -> f x `seq` r) ()
    list = each (`seq` ())
    whnf = maybe () (`seq` ())
