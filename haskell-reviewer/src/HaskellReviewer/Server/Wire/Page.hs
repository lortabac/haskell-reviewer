-- | The module page (SPEC §4.2, §4.3): one module with its
-- declarations, docs and source, as CBOR. See Note [Module page].
module HaskellReviewer.Server.Wire.Page
  ( -- * Types
    ModuleView (..),
    ModuleAnswer (..),
    NotParsed (..),

    -- * Bodies
    encodeModulePage,

    -- * Warm-up
    warmUpPages,
    pageWarmUp,

    -- * Code
    tokenClassName,
  )
where

import Codec.CBOR.Encoding (Encoding)
import Control.Exception (evaluate)
import Data.ByteString.Lazy qualified as LBS
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (toList)
import Data.List (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Code
  ( Code,
    CodeClass (..),
    CodeEnv (..),
    CodeToken (..),
    classHead,
    codeEnv,
    codeOf,
    codeOfSpans,
    famInstHead,
    instanceHead,
    plainCode,
    typeHead,
  )
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
    Extent (..),
    FamInstDecl (..),
    FamInstForm (..),
    FieldDecl (..),
    Fixity (..),
    FixityDir (..),
    InstanceBody (..),
    InstanceDecl (..),
    InstanceForm (..),
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
    declInfo,
    famInstConstructors,
    formConstructors,
  )
import HaskellReviewer.Core.DocLinks (localNames, resolveLocal)
import HaskellReviewer.Core.FileContent (FileSize (..), TooLargeInfo (..))
import HaskellReviewer.Core.FileList (EntryKind (..), ListSource (..), mkFileList)
import HaskellReviewer.Core.ModuleModel
  ( DocComment (..),
    DocItem (..),
    Export (..),
    Extensions (..),
    Import (..),
    ImportLevel (..),
    ImportList (..),
    ImportSource (..),
    Item (..),
    ItemName (..),
    ListKind (..),
    Located (..),
    ModuleExport (..),
    ModuleHeader (..),
    ModuleParse (..),
    NameKeyword (..),
    ParseError (..),
    ParseWarning (..),
    ParsedModule (..),
    Qualified (..),
    Section (..),
    SubList (..),
    Subs (..),
    TextOnlyModule (..),
    TextOnlyReason (..),
    WarningKind (..),
    WarningText (..),
    renderParseWarning,
  )
import HaskellReviewer.Core.Modules
  ( ModuleId (..),
    ModuleIndex (..),
    NoSourceReason (..),
    PackageModules (..),
    Preprocessor (..),
    buildModuleIndex,
    moduleRef,
    renderModuleId,
    renderPreprocessor,
    resolveModuleRef,
  )
import HaskellReviewer.Core.Names
  ( DeclId (..),
    InstanceKey (..),
    declName,
    keyHeadParts,
    renderDeclId,
  )
import HaskellReviewer.Core.Parser (defaultParseSettings, parseSource)
import HaskellReviewer.Core.Project
  ( DetectedGhc (..),
    GhcSource (..),
    ModuleName (..),
    PackageName (..),
    Project (..),
    ProjectSource (..),
    parserGhcVersion,
    renderComponentName,
  )
import HaskellReviewer.Core.RelPath (RelPath, relPathFromSegments, relPathText)
import HaskellReviewer.Core.RichText
  ( Block (..),
    Cell (..),
    DefItem (..),
    Doc (..),
    Example (..),
    Header (..),
    Hyperlink (..),
    Inline (..),
    ListItem (..),
    MathStyle (..),
    ModLink (..),
    NameRef (..),
    RefNamespace (..),
    Since (..),
    TableBody (..),
    urlText,
  )
import HaskellReviewer.Core.SourceText (sliceSpan)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (TokenClass (..))
import HaskellReviewer.Effects.Parser (declsWarmUp, haddockWarmUp)
import HaskellReviewer.Server.Cbor qualified as Cbor
import HaskellReviewer.Server.Wire (noSourceReason)
import HaskellReviewer.Session.Index (ParsedSource (..))
import HaskellReviewer.Session.Snapshot (Generation (..))

-- * Types

-- | One module, for GET api\/module\/\<side>\/\<ref>.
data ModuleView = ModuleView
  { moduleId :: ModuleId,
    -- | The file, if there is one.
    path :: Maybe RelPath,
    answer :: ModuleAnswer
  }

-- | What we have for a module.
data ModuleAnswer
  = -- | It parsed: the model and the file's text.
    ParsedAnswer ParsedSource
  | -- | We ran the parser, and show it as text.
    TextOnlyAnswer TextOnlyModule
  | -- | Text only, without running the parser.
    NotParsed NotParsed

-- | Why we didn't run the parser.
data NotParsed
  = -- | A preprocessor input. Known from the index.
    GeneratedModule Preprocessor
  | -- | -F -pgmF or -pgmL in ghc-options. Known from the index.
    PreprocessedModule Text
  | -- | A listed module with no file.
    NoSourceModule NoSourceReason
  | -- | Binary, or not UTF-8.
    NotTextModule FileSize
  | -- | Over the text limit.
    TooLargeModule TooLargeInfo

{- Note [Module page]

GET api/module/<side>/<ref> is CBOR, built per request from the slot
(no cache). In JSON terms (fields that may be missing are null, never
left out; spans are [startLine, startCol, endLine, endCol], code
points, end exclusive):

  {generation, id, ref, path, package: {name, version} | null,
   component | null, tier: {kind: syntactic, reason: no-hie}, parse}

parse is {kind: text-only, reason, warnings} (reasons as for the
banner: parse-error, pragma-error, preprocessed, parser-crashed,
too-slow, generated, no-source, not-text, too-large) or

  {kind: parsed, header, exports, imports, extensions, warnings,
   declarations, docItems, pragmas}

Declarations come in source order. Every one has kind (type, class,
value, patsyn, instance, faminst, splice), n (its handle) and id (Note
[Handles]; null for splices), name (null for instances and splices),
exported, docs, warning, fixity, line
(of the name), source (each part of the extent with its role and its
exact text, SPEC §4.3), then its own fields. Nested ones
(constructors, fields, methods, associated types) have the same first
fields, without source. A piece several declarations share (a
signature, a doc, a source part...) is sent once: the others have
{sharedWith: n} in its place (Note [Sent once]). Every reference to a
declaration is its handle n: sharedWith, instances, onType, a deriving
clause's instance, links in code and docs (Note [Handles]).

Instances (Note [Instance placement]) are listed once, each with
attached; types and classes list the handles of theirs.

Code (heads, signatures, types) is a list of tokens, each
[class, text] or [class, text, n] when it links (Note [Local
links in code] in "HaskellReviewer.Core.Code"); classes are short
names ('tokenClassName'), "sp" a space.

Doc comments are {span, doc}; a doc is {blocks, since}, each block and
inline {kind, ...}. A name in a comment carries its local target
(Note [Local doc links] in "HaskellReviewer.Core.DocLinks"); a module
link its ref if the module is in the project, and its anchor as
written: Haddock's (v:x, t:T), not our declaration ids (cross-module
links come later). List item numbers and @since versions are decimal
text, as they can be over 2^53; a number is null, and since too, when
haddock-library may have got it wrong (Note [Big numbers] in
"HaskellReviewer.Core.Parser.Haddock"). URLs are http or https
('SafeUrl'); the client checks again.
-}

{- Note [Instance placement]

Each instance is on the page once, in source order. Top-level types
and classes of this module list the instances whose head names them
(the instance's mentions, from "HaskellReviewer.Core.Parser.InstanceKey"):
a class lists its instances, a type the instances that mention it,
derived ones included. A name of an associated family counts for its
class. A family lists its type and data instances.

An instance no local type or class lists (an orphan, say) has
attached false: the page shows it in a block of its own at the end.
-}

-- * Bodies

-- | GET api\/module\/\<side>\/\<ref>. See Note [Module page].
encodeModulePage :: Generation -> ModuleIndex -> ModuleView -> LBS.ByteString
encodeModulePage (Generation g) idx v =
  Cbor.cborBody . Cbor.obj $
    [ ("generation", Cbor.word64 g),
      ("id", Cbor.text (renderModuleId v.moduleId)),
      ("ref", Cbor.text (moduleRef idx v.moduleId)),
      ("path", Cbor.nullable (Cbor.text . relPathText) v.path),
      ("package", Cbor.nullable package owner),
      ("component", Cbor.nullable (Cbor.text . renderComponentName . snd) owner),
      -- Always syntactic: no HIE files.
      ("tier", Cbor.obj [kind "syntactic", ("reason", Cbor.text "no-hie")]),
      ("parse", answer v.answer)
    ]
  where
    owner = case v.moduleId of
      ComponentModule p c _ -> Just (p, c)
      LooseModule _ -> Nothing
    package (p@(PackageName name), _) =
      Cbor.obj
        [ ("name", Cbor.text name),
          ( "version",
            Cbor.nullable (Cbor.text . (.version)) (find (\x -> x.name == p) idx.packages)
          )
        ]
    answer = \case
      ParsedAnswer s -> parsedModule idx s
      TextOnlyAnswer t -> textOnly (parserReason t.reason) t.warnings
      NotParsed r -> textOnly (notParsedReason r) []
    textOnly reason ws =
      Cbor.obj
        [ kind "text-only",
          ("reason", Cbor.obj reason),
          ("warnings", Cbor.list (Cbor.text . renderParseWarning) ws)
        ]
    parserReason = \case
      ParseFailed e -> kind "parse-error" : parseError e
      PragmaFailed e -> kind "pragma-error" : parseError e
      Preprocessed tool -> [kind "preprocessed", ("tool", Cbor.text tool)]
      ParserCrashed t -> [kind "parser-crashed", ("message", Cbor.text t)]
      TooSlow -> [kind "too-slow"]
    parseError e =
      [ ("message", Cbor.text e.message),
        ("code", Cbor.nullable Cbor.text e.code),
        ("span", Cbor.nullable span_ e.span)
      ]
    notParsedReason = \case
      GeneratedModule tool ->
        [kind "generated", ("tool", Cbor.text (renderPreprocessor tool))]
      PreprocessedModule tool -> [kind "preprocessed", ("tool", Cbor.text tool)]
      NoSourceModule r -> [kind "no-source", ("reason", Cbor.text (noSourceReason r))]
      NotTextModule size -> [kind "not-text", ("size", bytes size)]
      TooLargeModule info ->
        [kind "too-large", ("size", bytes info.size), ("limit", bytes info.limit)]
    bytes (FileSize n) = Cbor.int n

-- | The short name of a token class on the wire. See Note [Module page].
tokenClassName :: CodeClass -> Text
tokenClassName = \case
  Space -> "sp"
  Tok k -> case k of
    Keyword -> "kw"
    VarId -> "var"
    ConId -> "con"
    VarSym -> "op"
    ConSym -> "conop"
    ReservedOp -> "rop"
    Special -> "pun"
    Literal -> "num"
    StringLit -> "str"
    CharLit -> "chr"
    Pragma -> "prag"
    Comment -> "com"
    Haddock -> "doc"
    Macro -> "mac"

-- * Warm-up

{- Note [Page warm-up]

Request threads encode pages in parallel, and the encoder has shared
top-level values: our keys and texts, cborg's, the code and doc link
helpers'. On GHC 9.12 and 9.14 threads that force the same thunk at
once can livelock the RTS (Q-RTS-BLACKHOLE). So before the index is
filled, the listing thread encodes 'pageWarmUp': the warm-up modules
of the parser (every kind of declaration, every kind of Haddock
markup), one with CPP, one with every export form and a line past 256
columns (Note [Slicing] in "HaskellReviewer.Core.SourceText"), and a
text-only page for each reason. A test checks they have every token class code
can have, every declaration kind, doc node and value of the other
enum-like fields.
-}

-- | Encode 'pageWarmUp'. See Note [Page warm-up].
warmUpPages :: IO ()
warmUpPages = mapM_ (evaluate . LBS.length) pageWarmUp

-- | The warm-up pages. See Note [Page warm-up].
pageWarmUp :: [LBS.ByteString]
pageWarmUp =
  map parsedPage [declsWarmUp, haddockWarmUp, cppPage, exportsPage]
    <> map
      (page . TextOnlyAnswer . textOnly)
      [ ParseFailed parseError,
        PragmaFailed parseError,
        Preprocessed "x",
        ParserCrashed "x",
        TooSlow
      ]
    <> map
      (page . NotParsed)
      [ GeneratedModule Alex,
        PreprocessedModule "x",
        NoSourceModule Autogen,
        NoSourceModule Missing,
        NotTextModule (FileSize 1),
        TooLargeModule TooLargeInfo {size = FileSize 2, limit = FileSize 1}
      ]
  where
    parsedPage t = case parseSource defaultParseSettings t of
      Parsed m -> page (ParsedAnswer ParsedSource {text = t, parsed = m})
      TextOnly o -> page (TextOnlyAnswer o)
    page a =
      encodeModulePage
        (Generation 1)
        warmUpIndex
        ModuleView {moduleId = warmUpId, path = Nothing, answer = a}
    textOnly r = TextOnlyModule {reason = r, warnings = [NoMarkdownCode]}
    parseError =
      ParseError
        { message = "x",
          code = Just "x",
          span = Just (Span (Line 1) (Col 1) (Line 1) (Col 2))
        }
    cppPage =
      "{-# LANGUAGE CPP #-}\nmodule M where\n#define T Int\nx :: T\nx = 'c'\n"
    exportsPage =
      Text.unlines
        [ "{-# LANGUAGE PatternSynonyms, ExplicitNamespaces, NamedDefaults #-}",
          "{-# LANGUAGE ImportQualifiedPost, ExplicitLevelImports #-}",
          "module E {-# WARNING \"w\" #-}",
          "  ( -- * A",
          "    -- | D.",
          "    x, -- ^ X.",
          "    type (+), pattern P, data D, default Num, T (..), U (A, b),",
          "    -- $c",
          "    module M",
          "  ) where",
          "import M",
          "import qualified M as N",
          "import M qualified as O (T, x)",
          "import M hiding (y)",
          "import splice M",
          "import M quote (z)",
          "-- * H",
          "-- $c",
          "-- See \"E\".",
          "x = 1",
          "infix 4 ===",
          "a === _ = a",
          -- Past 256 columns: the long-line slicing path (Note [Slicing]
          -- in "HaskellReviewer.Core.SourceText").
          "long :: " <> Text.intercalate " -> " (replicate 60 "Int"),
          "long = undefined"
        ]

-- | An index with one loose module, @E.hs@, for the module links.
warmUpIndex :: ModuleIndex
warmUpIndex =
  buildModuleIndex
    Project
      { source = ImplicitRootCabalFiles,
        withCompiler = Nothing,
        ghc = DetectedGhc {version = parserGhcVersion, source = GhcFromParser},
        packages = [],
        brokenPackages = [],
        warnings = []
      }
    (mkFileList FromDisk 0 [(p, EntryFile) | p <- toList warmUpPath])

warmUpPath :: Maybe RelPath
warmUpPath = relPathFromSegments ["E.hs"]

-- | @E.hs@.
warmUpId :: ModuleId
warmUpId = maybe (error "E.hs is a valid path") LooseModule warmUpPath

-- * Parsed modules

-- | What encoding a parsed module needs.
data Env = Env
  { code :: CodeEnv,
    index :: ModuleIndex,
    objects :: Objects,
    placed :: Placement,
    -- | Who sends what. See Note [Sent once].
    senders :: Map Piece Handle
  }

parsedModule :: ModuleIndex -> ParsedSource -> Encoding
parsedModule idx s =
  Cbor.obj
    [ kind "parsed",
      ("header", header env m.header),
      ("exports", Cbor.nullable (Cbor.list (export env)) m.exports),
      ("imports", Cbor.list (importDecl env) m.imports),
      ("extensions", extensions m.extensions),
      ("warnings", Cbor.list (Cbor.text . renderParseWarning) m.warnings),
      ("idParts", Cbor.list Cbor.text objs.partList),
      ("declarations", Cbor.list (topDecl env) m.declarations),
      ("docItems", Cbor.list (docItem env) m.docItems),
      ("pragmas", Cbor.list (pragma env) m.pragmas)
    ]
  where
    m = s.parsed
    names = localNames ((.value) <$> m.header.name) m.declarations
    objs = objectsOf m.declarations
    env =
      Env
        { code = codeEnv s.text m.tokens names,
          index = idx,
          objects = objs,
          placed = placement objs m.declarations,
          senders = objs.senders
        }

-- | Who lists which instances, by handle. See Note [Instance placement].
data Placement = Placement
  { -- | Instances by owner (a type, a class, a family).
    owned :: Map Handle [Handle],
    -- | Instances with an owner.
    attached :: Set.Set Handle
  }

-- | See Note [Instance placement].
placement :: Objects -> [Decl] -> Placement
placement objs decls =
  Placement
    { owned = Map.map reverse (Map.fromListWith (<>) [(o, [i]) | (i, o) <- pairs]),
      attached = Set.fromList (map fst pairs)
    }
  where
    handle = handleOf objs
    -- Top-level types and classes by name, the first one. An
    -- associated family stands for its class.
    owners =
      Map.fromListWith
        (\_ old -> old)
        ( [(n, handle t.info) | TypeD t <- decls, Just n <- [declName t.info.id]]
            <> [(n, handle c.info) | ClassD c <- decls, Just n <- [declName c.info.id]]
            <> [ (n, handle c.info)
               | ClassD c <- decls,
                 t <- c.associated,
                 Just n <- [declName t.info.id]
               ]
        )
    -- (instance, owner), in source order.
    pairs =
      [ (handle i.info, o)
      | InstanceD i <- decls,
        o <- nubOrd (mapMaybe (`Map.lookup` owners) i.mentions)
      ]
        <> [ (f, handle t.info)
           | t <- [t | TypeD t <- decls] <> [t | ClassD c <- decls, t <- c.associated],
             -- One id per instance: equal keys repeat it, but byId has
             -- them all already.
             i <- nubOrd (familyIds t.form),
             f <- Map.findWithDefault [] i objs.byId
           ]

-- | The instances a family lists.
familyIds :: TypeForm -> [DeclId]
familyIds = \case
  OpenFamily b -> b.instances
  DataFamily is -> is
  _ -> []

{- Note [Sent once]

Several declarations can share one piece of source: a signature
@x1, ..., xN :: T@, a fixity @infixl 6 a, b, c@, a doc comment or a
DEPRECATED pragma on many names, a GADT line @A, B :: T@, record
fields @f1, f2 :: T@, a deriving clause with a strategy or a via type.
Sent under each name, such a piece makes a page quadratic in its
size (5000 names in 34 KB made a 909 MB page). So each piece is sent
once, by the first declaration that has it, in page order; the others
send @{sharedWith: n}@, the sender's handle (with the span, for a doc,
a warning or a source part), and the page finds it on that declaration
('byHandle' in web/src/api/module.ts). Not by id: ids aren't unique
(Note [Handles]). Pieces, by span: a signature's code, a doc
comment, a warning, a top-level source part, a constructor's code with
its arguments and fields, a field's type, a derived instance's via
type.
-}

-- | A piece of source that only one declaration sends. See Note [Sent
-- once].
data Piece
  = SigPiece Span
  | DocPiece Span
  | WarningPiece Span
  | -- | A part of a top-level declaration's extent.
    SourcePiece Span
  | -- | A constructor's own line, with its arguments and fields.
    ConPiece Span
  | FieldTypePiece Span
  | ViaPiece Span
  deriving (Eq, Ord)

-- | A declaration on the page, top-level or nested, by its number in
-- page order. See Note [Handles].
type Handle = Int

{- Note [Handles]

Declaration ids aren't unique on a page (a field in several
constructors, two equal instances), and some are long (a derived
instance's id has the whole type in it, a field's has its parent).
So every declaration on the page, nested ones too, has a handle n, a
number unique on the page, and the page refers to declarations by
handle: in sharedWith, in instance lists, in onType, in links of code
and docs. Each id is sent once, on its declaration. Long parts of ids
go in a page-level table, idParts: a field id is {field: name, parent:
part}, an instance or family instance id {inst: name, head: [part]}
(its text is the parts joined). Other ids are text.

Handles go up in page order but can have gaps: the fields of a
constructor that shares its line with another (Note [Sent once]) have
numbers but aren't sent. And under CPP, one macro use can give two
declarations with the same id at the same span (@#define II instance
C T; instance C T@): they get the same n. The client must take that.
-}

-- | The declarations of a page, numbered. See Note [Handles].
data Objects = Objects
  { -- | By id and name span. Names don't overlap, so this is unique,
    -- but for a CPP macro that makes the same declaration twice: both
    -- get the first's handle (Note [Handles]).
    handles :: Map (DeclId, Span) Handle,
    -- | Every handle with this id.
    byId :: Map DeclId [Handle],
    senders :: Map Piece Handle,
    -- | The id parts, numbered, and in order.
    parts :: Map Text Int,
    partList :: [Text]
  }

-- | Number every declaration of the page, in page order, and find
-- which one sends each piece (the first). See Note [Sent once].
objectsOf :: [Decl] -> Objects
objectsOf decls =
  Objects
    { handles = firstWins [((o.id, o.nameSpan), n) | (n, o, _) <- objs],
      byId = Map.map reverse (Map.fromListWith (<>) [(o.id, [n]) | (n, o, _) <- objs]),
      senders = firstWins [(p, n) | (n, _, ps) <- objs, p <- ps],
      parts = Map.fromList (zip partList [0 ..]),
      partList = partList
    }
  where
    firstWins :: (Ord k) => [(k, v)] -> Map k v
    firstWins = Map.fromListWith (\_ old -> old)
    objs = zipWith (\n (o, ps) -> (n, o, ps)) [0 ..] (concatMap top decls)
    partList = nubOrd (concat [idParts o.id | (_, o, _) <- objs])
    -- Each declaration with its pieces, in page order.
    top d = case declInfo d of
      Nothing -> []
      Just i ->
        (i, [SourcePiece p.span | p <- NonEmpty.toList i.extent.parts] <> own i <> sig d)
          : rest d
    own (i :: DeclInfo) =
      [DocPiece c.span | c <- i.haddock] <> [WarningPiece w.span | w <- toList i.warning]
    sig = \case
      ValueD v -> [SigPiece s.span | s <- toList v.signature]
      PatSynD p -> [SigPiece s.span | s <- toList p.signature]
      InstanceD i -> case i.form of
        DerivedInstance x -> [ViaPiece v | ViaStrategy v <- [x.strategy]]
        _ -> []
      _ -> []
    rest = \case
      TypeD t -> typeDecl t
      ClassD c ->
        [(m.info, own m.info <> [SigPiece m.signature.span]) | m <- c.methods]
          <> concatMap (\t -> (t.info, own t.info) : typeDecl t) c.associated
      PatSynD p -> [(f, own f) | f <- p.fields]
      InstanceD i -> case i.form of
        ExplicitInstance b -> concatMap famInst b.associated
        _ -> []
      FamInstD f -> cons (famInstConstructors f.form)
      _ -> []
    typeDecl t = cons (formConstructors t.form)
    famInst f = (f.info, own f.info) : cons (famInstConstructors f.form)
    cons cs =
      concat
        [ (c.info, own c.info <> [ConPiece s | s <- conSpan c])
            : [(f.info, own f.info <> [FieldTypePiece f.type_.type_]) | f <- conFields c]
        | c <- cs
        ]

-- | The long parts of an id. See Note [Handles].
idParts :: DeclId -> [Text]
idParts = \case
  Field parent _ -> [parent]
  Inst k -> keyHeadParts k.head
  Named _ _ -> []

-- | A declaration's handle. Every declaration on the page has one.
handleOf :: Objects -> DeclInfo -> Handle
handleOf objs i =
  fromMaybe
    (error ("no handle: " <> show i.id))
    (Map.lookup (i.id, i.nameSpan) objs.handles)

-- | The instances a type, class or family lists.
ownedBy :: Env -> DeclInfo -> [Handle]
ownedBy env i = Map.findWithDefault [] (handleOf env.objects i) env.placed.owned

-- | Some type or class lists the instance.
isAttached :: Env -> DeclInfo -> Bool
isAttached env i = Set.member (handleOf env.objects i) env.placed.attached

-- | The first declaration with this id, for links.
firstWithId :: Env -> DeclId -> Maybe Handle
firstWithId env i = case Map.lookup i env.objects.byId of
  Just (n : _) -> Just n
  _ -> Nothing

-- | An id on the wire. See Note [Handles].
wireId :: Env -> DeclId -> Encoding
wireId env = \case
  Field parent n -> Cbor.obj [("field", Cbor.text n), ("parent", part parent)]
  Inst k ->
    Cbor.obj [("inst", Cbor.text k.name), ("head", Cbor.list part (keyHeadParts k.head))]
  i -> Cbor.text (renderDeclId i)
  where
    part t = Cbor.int (Map.findWithDefault 0 t env.objects.parts)

-- | The piece, or @{sharedWith: n}@ (and these fields first) if another
-- declaration sends it. See Note [Sent once].
sentOnce ::
  Env -> DeclInfo -> Piece -> [(Text, Encoding)] -> Encoding -> Encoding
sentOnce env self piece first enc = case Map.lookup piece env.senders of
  Just o
    | o /= handleOf env.objects self -> Cbor.obj (first <> [("sharedWith", Cbor.int o)])
  _ -> enc

-- | A constructor's own line: its first head part.
conSpan :: ConDecl -> [Span]
conSpan c =
  take 1 [p.span | p <- NonEmpty.toList c.info.extent.parts, p.role == HeadPart]

-- | A constructor's record fields.
conFields :: ConDecl -> [FieldDecl]
conFields c = case c.args of
  RecordArgs fs -> fs
  GadtRecordArgs fs -> fs
  _ -> []

header :: Env -> ModuleHeader -> Encoding
header env h =
  Cbor.obj
    [ ("name", Cbor.nullable (located moduleNameText) h.name),
      ("haddock", Cbor.nullable (docComment env) h.haddock),
      ("warning", Cbor.nullable warningText h.warning)
    ]

export :: Env -> Export -> Encoding
export env = \case
  ExportItem i -> item env (Just (kind "item")) i
  ExportModule e ->
    Cbor.obj
      [ kind "module",
        ("name", located moduleNameText e.name),
        ("warning", Cbor.nullable warningText e.warning)
      ]
  ExportSection s ->
    Cbor.obj
      [ kind "section",
        ("level", Cbor.int s.level),
        ("heading", headingComment env s.heading)
      ]
  ExportDoc d -> Cbor.obj (kind "doc" : docFields env d)
  ExportDocNamed n ->
    Cbor.obj [kind "named-doc", ("span", span_ n.span), ("name", Cbor.text n.value)]

-- | An export item (with a kind) or an import item (without).
item :: Env -> Maybe (Text, Encoding) -> Item -> Encoding
item env first i =
  Cbor.obj $
    maybe [] pure first
      <> [ ("span", span_ i.span),
           ("name", itemName i.name),
           ("subs", subs i.subs),
           ("doc", Cbor.nullable (docComment env) i.doc),
           ("warning", Cbor.nullable warningText i.warning)
         ]
  where
    subs = \case
      NoSubs -> Cbor.obj [kind "none"]
      AllSubs -> Cbor.obj [kind "all"]
      SomeSubs l ->
        Cbor.obj
          [ kind "some",
            ("names", Cbor.list itemName l.names),
            ("wildcardAt", Cbor.nullable Cbor.int l.wildcardAt)
          ]

itemName :: ItemName -> Encoding
itemName n =
  Cbor.obj
    [ ("span", span_ n.span),
      ("qualifier", Cbor.nullable (Cbor.text . moduleNameText) n.qualifier),
      ("occ", Cbor.text n.occ),
      ("keyword", Cbor.nullable Cbor.text (keyword n.keyword))
    ]
  where
    keyword = \case
      NoKeyword -> Nothing
      TypeKeyword -> Just "type"
      PatternKeyword -> Just "pattern"
      DataKeyword -> Just "data"
      DefaultKeyword -> Just "default"

importDecl :: Env -> Import -> Encoding
importDecl env i =
  Cbor.obj
    [ ("span", span_ i.span),
      ("name", located moduleNameText i.name),
      ("package", Cbor.nullable Cbor.text i.package),
      ("source", Cbor.bool (i.source == SourceImport)),
      ("safe", Cbor.bool i.safe),
      ("qualified", Cbor.text (qualified i.qualified)),
      ("level", Cbor.text (level i.level)),
      ("as", Cbor.nullable (located moduleNameText) i.as),
      ("list", Cbor.nullable importList i.list)
    ]
  where
    qualified = \case
      NotQualified -> "no"
      QualifiedPre -> "pre"
      QualifiedPost -> "post"
    level = \case
      NormalLevel -> "normal"
      SpliceLevel -> "splice"
      QuoteLevel -> "quote"
    importList l =
      Cbor.obj
        [ ( "kind",
            Cbor.text $ case l.kind of
              Only -> "only"
              Hiding -> "hiding"
          ),
          ("items", Cbor.list (item env Nothing) l.items)
        ]

extensions :: Extensions -> Encoding
extensions e =
  Cbor.obj
    [ ("language", Cbor.text e.language),
      ("enabled", Cbor.list Cbor.text e.enabled),
      ("fromPragmas", Cbor.list Cbor.text e.fromPragmas)
    ]

-- | @{span, kind, category, messages}@: kind deprecated (category null)
-- or warning (category null or like @x-old@).
warningText :: WarningText -> Encoding
warningText w =
  Cbor.obj
    [ ("span", span_ w.span),
      kind k,
      ("category", Cbor.nullable Cbor.text category),
      ("messages", Cbor.list Cbor.text w.messages)
    ]
  where
    (k, category) = case w.kind of
      DeprecatedPragma -> ("deprecated", Nothing)
      WarningPragma c -> ("warning", c)

docItem :: Env -> DocItem -> Encoding
docItem env = \case
  DocHeading s ->
    Cbor.obj
      [ kind "heading",
        ("level", Cbor.int s.level),
        ("inlines", inlines env s.heading.parsed),
        ("span", span_ s.heading.span)
      ]
  DocChunk n d ->
    Cbor.obj
      [ kind "chunk",
        ("name", Cbor.text n.value),
        ("doc", doc env d.parsed),
        ("span", span_ d.span)
      ]

pragma :: Env -> ModulePragma -> Encoding
pragma env p =
  Cbor.obj $
    ("span", span_ p.span) : fields <> [("code", code_ env (plainCode env.code p.span))]
  where
    fields = case p.kind of
      Rules ns -> [kind "rules", ("names", Cbor.list Cbor.text ns)]
      Complete ns t ->
        [ kind "complete",
          ("names", Cbor.list Cbor.text ns),
          ("type", Cbor.nullable Cbor.text t)
        ]
      Annotation t ->
        [ kind "ann",
          ( "target",
            case t of
              AnnValue n -> Cbor.obj [kind "value", ("name", Cbor.text n)]
              AnnType n -> Cbor.obj [kind "type", ("name", Cbor.text n)]
              AnnModule -> Cbor.obj [kind "module", ("name", Cbor.null_)]
          )
        ]
      Default c -> [kind "default", ("class", Cbor.nullable Cbor.text c)]

-- * Declarations

-- | A top-level declaration. See Note [Module page].
topDecl :: Env -> Decl -> Encoding
topDecl env d = Cbor.obj $ case d of
  TypeD t -> common "type" t.info t.fixity <> typeFields env t
  ClassD c -> common "class" c.info c.fixity <> classFields env c
  ValueD v -> common "value" v.info v.fixity <> valueFields env v
  PatSynD p -> common "patsyn" p.info p.fixity <> patSynFields env p
  InstanceD i -> common "instance" i.info Nothing <> instanceFields env i
  FamInstD f -> common "faminst" f.info Nothing <> famInstFields env f TopLevel
  SpliceD e ->
    [ kind "splice",
      ("id", Cbor.null_),
      ("name", Cbor.null_),
      ("exported", Cbor.bool False),
      ("docs", Cbor.list (docComment env) []),
      ("warning", Cbor.null_),
      ("fixity", Cbor.null_),
      ("line", line (NonEmpty.head e.parts).span),
      ("source", source Nothing e),
      ("head", code_ env (codeOfSpans env.code [] [p.span | p <- toList e.parts]))
    ]
  where
    common k i f = (kind k : infoFields env i f) <> [("source", source (Just i) i.extent)]
    source owner e = Cbor.list (part owner) (NonEmpty.toList e.parts)
    part owner p =
      let first = [("span", span_ p.span), ("role", Cbor.text (role p.role))]
          text = sliceSpan env.code.source p.span
          whole = Cbor.obj (first <> [("text", Cbor.text text)])
       in case owner of
            Just i -> sentOnce env i (SourcePiece p.span) first whole
            Nothing -> whole
    role = \case
      HeadPart -> "head"
      BodyPart -> "body"
      PragmaPart -> "pragma"
      DocPart -> "doc"

-- | What every declaration has: id, name, exported, docs, warning,
-- fixity, line.
infoFields :: Env -> DeclInfo -> Maybe Fixity -> [(Text, Encoding)]
infoFields env i f =
  [ ("n", Cbor.int (handleOf env.objects i)),
    ("id", wireId env i.id),
    ("name", Cbor.nullable Cbor.text (declName i.id)),
    ("exported", Cbor.bool i.exported),
    ("docs", Cbor.list doc_ i.haddock),
    ("warning", Cbor.nullable warning i.warning),
    ("fixity", Cbor.nullable fixity f),
    ("line", line i.nameSpan)
  ]
  where
    doc_ d = sentOnce env i (DocPiece d.span) [("span", span_ d.span)] (docComment env d)
    warning w =
      sentOnce env i (WarningPiece w.span) [("span", span_ w.span)] (warningText w)

fixity :: Fixity -> Encoding
fixity f =
  Cbor.obj
    [ ( "direction",
        Cbor.text $ case f.direction of
          InfixLeft -> "infixl"
          InfixRight -> "infixr"
          InfixNone -> "infix"
      ),
      -- 0 to 9: GHC's parser rejects others.
      ("precedence", Cbor.int f.precedence)
    ]

typeFields :: Env -> TypeDecl -> [(Text, Encoding)]
typeFields env t =
  [ ("form", Cbor.text form),
    ("head", code_ env (typeHead env.code t)),
    ("kindSig", Cbor.nullable (spanCode env [t.info]) t.kindSig),
    ("roles", Cbor.nullable (spanCode env [t.info]) t.roles),
    ("constructors", Cbor.list (constructor env) cons),
    ("derivings", Cbor.list (deriving_ env) derivs),
    ("equations", Cbor.list (spanCode env [t.info]) equations),
    ("defaults", Cbor.list (spanCode env [t.info]) defaults),
    ( "instances",
      handles (ownedBy env t.info)
    )
  ]
  where
    (form, cons, derivs, equations, defaults) = case t.form of
      DataType b -> ("data", b.constructors, b.derivings, [], [])
      Newtype b -> ("newtype", [b.constructor], b.derivings, [], [])
      TypeData b -> ("type-data", b.constructors, b.derivings, [], [])
      Synonym -> ("synonym", [], [], [], [])
      OpenFamily b -> ("open-family", [], [], [], b.defaults)
      DataFamily _ -> ("data-family", [], [], [], [])
      ClosedFamily es -> ("closed-family", [], [], es, [])

-- | A constructor. One that shares its line with an earlier one (@A, B
-- :: T@) has @sharedWith@, a null @code@ and no arguments or fields:
-- they are the other one's. See Note [Sent once].
constructor :: Env -> ConDecl -> Encoding
constructor env c =
  Cbor.obj $
    infoFields env c.info c.fixity
      <> [("shape", Cbor.text shape)]
      <> case sharer of
        Just o ->
          [ ("sharedWith", Cbor.int o),
            ("code", Cbor.null_),
            ("args", Cbor.list id []),
            ("fields", Cbor.list id [])
          ]
        Nothing ->
          [ ("sharedWith", Cbor.null_),
            ("code", code_ env (codeOfSpans env.code [c.info] (conSpan c))),
            ("args", Cbor.list (conArg env) args),
            ("fields", Cbor.list (field env) fields)
          ]
  where
    (shape, args, fields) = case c.args of
      PrefixArgs as -> ("prefix", as, [])
      InfixArgs a b -> ("infix", [a, b], [])
      RecordArgs fs -> ("record", [], fs)
      GadtArgs as -> ("gadt", as, [])
      GadtRecordArgs fs -> ("gadt-record", [], fs)
    sharer = case [o | s <- conSpan c, Just o <- [Map.lookup (ConPiece s) env.senders]] of
      o : _ | o /= handleOf env.objects c.info -> Just o
      _ -> Nothing

conArg :: Env -> ConArg -> Encoding
conArg env a =
  Cbor.obj $
    ("type", spanCode env [] a.type_.type_)
      : argMarks a.type_
        <> [("doc", Cbor.nullable (docComment env) a.doc)]

-- | A field. Its type may be another field's (@f1, f2 :: T@): then
-- @{sharedWith: id}@. See Note [Sent once].
field :: Env -> FieldDecl -> Encoding
field env f =
  Cbor.obj $
    infoFields env f.info Nothing
      <> [ ( "type",
             sentOnce
               env
               f.info
               (FieldTypePiece f.type_.type_)
               []
               (spanCode env [] f.type_.type_)
           )
         ]
      <> argMarks f.type_

-- | @strictness@ and @unpack@.
argMarks :: ArgType -> [(Text, Encoding)]
argMarks a =
  [ ( "strictness",
      Cbor.text $ case a.strictness of
        Unmarked -> "none"
        Strict -> "strict"
        Lazy -> "lazy"
    ),
    ( "unpack",
      Cbor.text $ case a.unpack of
        NoUnpackPragma -> "none"
        Unpack -> "unpack"
        NoUnpack -> "nounpack"
    )
  ]

deriving_ :: Env -> DerivingClause -> Encoding
deriving_ env d =
  Cbor.obj $
    strategy env d.strategy
      <> [ ( "classes",
             Cbor.list
               ( \c ->
                   Cbor.obj
                     [ ("code", spanCode env [] c.span),
                       ("instance", Cbor.nullable Cbor.int (derivedHandle c))
                     ]
               )
               d.classes
           )
         ]
  where
    -- The derived instance: its name span is the class's.
    derivedHandle c = Map.lookup (c.id, c.span) env.objects.handles

-- | @strategy@ and @via@.
strategy :: Env -> DerivStrategy -> [(Text, Encoding)]
strategy env s =
  [ ("strategy", Cbor.text name),
    ("via", Cbor.nullable (spanCode env []) via)
  ]
  where
    (name, via) = case s of
      NoStrategy -> ("none", Nothing)
      StockStrategy -> ("stock", Nothing)
      NewtypeStrategy -> ("newtype", Nothing)
      AnyclassStrategy -> ("anyclass", Nothing)
      ViaStrategy t -> ("via", Just t)

classFields :: Env -> ClassDecl -> [(Text, Encoding)]
classFields env c =
  [ ("head", code_ env (classHead env.code c)),
    ("kindSig", Cbor.nullable (spanCode env [c.info]) c.kindSig),
    ("roles", Cbor.nullable (spanCode env [c.info]) c.roles),
    ("methods", Cbor.list method c.methods),
    ("associated", Cbor.list associated c.associated),
    ("minimal", Cbor.nullable (spanCode env [c.info]) c.minimal),
    ("instances", handles (ownedBy env c.info))
  ]
  where
    method m =
      Cbor.obj $
        infoFields env m.info m.fixity
          <> [ ("signature", signature env m.info m.signature),
               ("defaultSig", Cbor.nullable (spanCode env [m.info]) m.defaultSig),
               ("hasDefault", Cbor.bool m.hasDefault)
             ]
    associated t =
      Cbor.obj (kind "type" : infoFields env t.info t.fixity <> typeFields env t)

-- | @{code, names}@, or @{sharedWith: id}@ for a signature that an
-- earlier name sends (Note [Sent once]).
signature :: Env -> DeclInfo -> Signature -> Encoding
signature env self s =
  sentOnce env self (SigPiece s.span) [] $
    Cbor.obj
      [ ("code", code_ env (codeOf env.code [self] s.span)),
        ("names", Cbor.list Cbor.text (NonEmpty.toList s.names))
      ]

valueFields :: Env -> ValueDecl -> [(Text, Encoding)]
valueFields env v =
  [ ("signature", Cbor.nullable (signature env v.info) v.signature),
    ( "definition",
      Cbor.text $ case v.definition of
        Equations -> "equations"
        PatternBound -> "pattern"
        ForeignImport -> "foreign-import"
        NoDefinition -> "none"
    ),
    ("foreignExports", Cbor.list (spanCode env [v.info]) v.foreignExports)
  ]

patSynFields :: Env -> PatSynDecl -> [(Text, Encoding)]
patSynFields env p =
  [ ("signature", Cbor.nullable (signature env p.info) p.signature),
    ( "direction",
      Cbor.text $ case p.direction of
        Unidirectional -> "unidirectional"
        ImplicitBidirectional -> "bidirectional"
        ExplicitBidirectional -> "explicit"
    ),
    ("fields", Cbor.list (\f -> Cbor.obj (infoFields env f Nothing)) p.fields)
  ]

instanceFields :: Env -> InstanceDecl -> [(Text, Encoding)]
instanceFields env i =
  [ ("head", code_ env (instanceHead env.code i)),
    ("orphan", Cbor.bool i.orphan),
    ("form", Cbor.obj form),
    ("associated", Cbor.list famInst associated),
    ("attached", Cbor.bool (isAttached env i.info))
  ]
  where
    (form, associated) = case i.form of
      ExplicitInstance b ->
        (kind "explicit" : strategy env NoStrategy <> noType, b.associated)
      StandaloneDeriving s ->
        (kind "standalone" : strategy env s <> noType, [])
      DerivedInstance d ->
        (kind "derived" : derivedStrategy d.strategy <> onType d.onType, [])
    -- The via type of a clause, once for all its classes.
    derivedStrategy = \case
      ViaStrategy v ->
        [ ("strategy", Cbor.text "via"),
          ("via", sentOnce env i.info (ViaPiece v) [] (spanCode env [] v))
        ]
      s -> strategy env s
    noType = [("onType", Cbor.null_)]
    onType t = [("onType", Cbor.nullable Cbor.int (firstWithId env t))]
    famInst f =
      Cbor.obj $
        kind "faminst" : infoFields env f.info Nothing <> famInstFields env f InInstance

-- | Where a type or data instance is.
data FamInstAt = TopLevel | InInstance

-- | A type or data instance. @attached@ only at the top level: one in
-- an instance is with it.
famInstFields :: Env -> FamInstDecl -> FamInstAt -> [(Text, Encoding)]
famInstFields env f at =
  [ ("family", Cbor.text f.family),
    ("head", code_ env (famInstHead env.code f)),
    ("form", Cbor.text form),
    ("constructors", Cbor.list (constructor env) cons),
    ("derivings", Cbor.list (deriving_ env) derivs),
    ("orphan", Cbor.bool f.orphan)
  ]
    <> case at of
      TopLevel -> [("attached", Cbor.bool (isAttached env f.info))]
      InInstance -> []
  where
    (form, cons, derivs) = case f.form of
      TypeInstance -> ("type", [], [])
      DataInstance b -> ("data", b.constructors, b.derivings)
      NewtypeInstance b -> ("newtype", [b.constructor], b.derivings)

-- * Rich text

-- | @{span, doc}@.
docComment :: Env -> DocComment Doc -> Encoding
docComment env d = Cbor.obj (docFields env d)

docFields :: Env -> DocComment Doc -> [(Text, Encoding)]
docFields env d = [("span", span_ d.span), ("doc", doc env d.parsed)]

-- | A section heading: @{span, inlines}@.
headingComment :: Env -> DocComment [Inline] -> Encoding
headingComment env d =
  Cbor.obj [("span", span_ d.span), ("inlines", inlines env d.parsed)]

doc :: Env -> Doc -> Encoding
doc env d =
  Cbor.obj
    [ ("blocks", blocks env d.blocks),
      ("since", Cbor.nullable since d.since)
    ]
  where
    since s =
      Cbor.obj
        [ ("package", Cbor.nullable Cbor.text s.package),
          ("version", Cbor.text (version s.version))
        ]

blocks :: Env -> [Block] -> Encoding
blocks env = Cbor.list block
  where
    block = \case
      Paragraph is -> Cbor.obj [kind "paragraph", ("inlines", inlines env is)]
      Heading h ->
        Cbor.obj
          [kind "heading", ("level", Cbor.int h.level), ("inlines", inlines env h.title)]
      CodeBlock is -> Cbor.obj [kind "code", ("inlines", inlines env is)]
      UnorderedList items ->
        Cbor.obj [kind "bullets", ("items", Cbor.list (blocks env) items)]
      OrderedList items ->
        Cbor.obj
          [ kind "numbered",
            ( "items",
              Cbor.list
                ( \i ->
                    Cbor.obj
                      [ ("number", Cbor.nullable (Cbor.text . decimal) i.number),
                        ("blocks", blocks env i.content)
                      ]
                )
                items
            )
          ]
      DefinitionList items ->
        Cbor.obj
          [ kind "definitions",
            ( "items",
              Cbor.list
                ( \i ->
                    Cbor.obj
                      [("term", inlines env i.term), ("blocks", blocks env i.definition)]
                )
                items
            )
          ]
      Examples es ->
        Cbor.obj
          [ kind "examples",
            ( "items",
              Cbor.list
                ( \e ->
                    Cbor.obj
                      [ ("expression", Cbor.text e.expression),
                        ("results", Cbor.list Cbor.text e.results)
                      ]
                )
                es
            )
          ]
      Property t -> Cbor.obj [kind "property", ("text", Cbor.text t)]
      Table t ->
        Cbor.obj
          [ kind "table",
            ("header", Cbor.list (Cbor.list cell) t.header),
            ("body", Cbor.list (Cbor.list cell) t.body)
          ]
    cell c =
      Cbor.obj
        [ ("colspan", Cbor.int c.colspan),
          ("rowspan", Cbor.int c.rowspan),
          ("inlines", inlines env c.content)
        ]

inlines :: Env -> [Inline] -> Encoding
inlines env = Cbor.list inline
  where
    inline = \case
      Str t -> Cbor.obj [kind "text", ("text", Cbor.text t)]
      Emphasis is -> Cbor.obj [kind "emphasis", ("inlines", inlines env is)]
      Bold is -> Cbor.obj [kind "bold", ("inlines", inlines env is)]
      Code is -> Cbor.obj [kind "code", ("inlines", inlines env is)]
      Math style t ->
        Cbor.obj
          [ kind "math",
            ( "style",
              Cbor.text $ case style of
                InlineMath -> "inline"
                DisplayMath -> "display"
            ),
            ("tex", Cbor.text t)
          ]
      Link l ->
        Cbor.obj
          [ kind "link",
            ("url", Cbor.text (urlText l.url)),
            ("label", Cbor.nullable (inlines env) l.label)
          ]
      Name r ->
        Cbor.obj
          [ kind "name",
            ("written", Cbor.text r.written),
            ( "ns",
              Cbor.text $ case r.namespace of
                ValueNs -> "value"
                TypeNs -> "type"
                AnyNs -> "any"
            ),
            ("target", Cbor.nullable Cbor.int (target r))
          ]
      ModuleLink l ->
        Cbor.obj
          [ kind "module",
            ("module", Cbor.text (moduleNameText l.name)),
            ("anchor", Cbor.nullable Cbor.text l.anchor),
            ("ref", Cbor.nullable Cbor.text (refOf l.name)),
            ("label", Cbor.nullable (inlines env) l.label)
          ]
    target r = firstWithId env =<< resolveLocal env.code.names r
    refOf (ModuleName n) = moduleRef env.index <$> resolveModuleRef env.index n

-- * Internal

-- | Code: @[class, text]@ or @[class, text, id]@ per token. See
-- Note [Module page].
code_ :: Env -> Code -> Encoding
code_ env = Cbor.list token
  where
    token t = case firstWithId env =<< t.link of
      Nothing -> Cbor.list id [Cbor.text (tokenClassName t.kind), Cbor.text t.text]
      Just n ->
        Cbor.list id [Cbor.text (tokenClassName t.kind), Cbor.text t.text, Cbor.int n]

-- | The code of a span, part of these declarations (they don't link
-- to themselves).
spanCode :: Env -> [DeclInfo] -> Span -> Encoding
spanCode env self s = code_ env (codeOf env.code self s)

handles :: [Handle] -> Encoding
handles = Cbor.list Cbor.int

kind :: Text -> (Text, Encoding)
kind k = ("kind", Cbor.text k)

located :: (a -> Text) -> Located a -> Encoding
located f l = Cbor.obj [("span", span_ l.span), ("value", Cbor.text (f l.value))]

moduleNameText :: ModuleName -> Text
moduleNameText (ModuleName n) = n

-- | @[startLine, startCol, endLine, endCol]@.
span_ :: Span -> Encoding
span_ s =
  Cbor.list
    Cbor.int
    [lineNo s.startLine, colNo s.startCol, lineNo s.endLine, colNo s.endCol]
  where
    colNo (Col n) = fromIntegral n

-- | The start line of a span.
line :: Span -> Encoding
line s = Cbor.int (lineNo s.startLine)

lineNo :: Line -> Int
lineNo (Line n) = fromIntegral n

decimal :: Int -> Text
decimal = Text.pack . show

-- | @1.2.3@.
version :: NonEmpty.NonEmpty Int -> Text
version = Text.intercalate "." . map decimal . NonEmpty.toList
