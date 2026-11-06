{-# LANGUAGE StrictData #-}

-- | The links of the source view: for each token of a parsed module,
-- what it links to, from the occurrence table and the module's
-- resolution; and the module's outline. Pure. See Note [Source links].
module HaskellReviewer.Core.SourceLinks
  ( -- * Input
    LinksInput (..),

    -- * Results
    SourceLinks (..),
    LinkTarget (..),
    LocalLink (..),
    DeclLink (..),
    ModuleLink (..),
    ExternalLink (..),
    LinkReason (..),
    GuessHow (..),
    OutlineEntry (..),
    OutlineKind (..),

    -- * Building
    linksOf,
    sourceLinks,
    forceSourceLinks,

    -- * Reading
    linkAt,
  )
where

import Data.Containers.ListUtils (nubOrd)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Vector.Unboxed qualified as Vector
import Data.Word (Word32)
import HaskellReviewer.Core.Code (nameRef)
import HaskellReviewer.Core.Decls
  ( Decl (..),
    DeclInfo (..),
    Extent (..),
    NamedDecl (..),
    Part (..),
    declInfo,
    namedDecls,
  )
import HaskellReviewer.Core.ModuleGraph (MappingGuess (..))
import HaskellReviewer.Core.ModuleModel (ParsedModule (..))
import HaskellReviewer.Core.Modules (ModuleId, ModuleIndex, modulePath, moduleRef)
import HaskellReviewer.Core.Names
  ( DeclId,
    ExternalModule (..),
    forceDeclId,
    forceExternalModule,
  )
import HaskellReviewer.Core.Occurrences
  ( Binder (..),
    FreeContext (..),
    Occurrence (..),
    Resolution (..),
    TokenIx (..),
    binderAt,
    occurrenceList,
  )
import HaskellReviewer.Core.Occurrences qualified as Occ (Certainty (..))
import HaskellReviewer.Core.Project (ModuleName (..))
import HaskellReviewer.Core.RelPath (RelPath)
import HaskellReviewer.Core.Resolve
  ( ExternalGuess (..),
    GuessKind (..),
    ModuleResolution (..),
    ModuleState (..),
    ParsedInput (..),
    Reason (..),
    Resolved (..),
    Target (..),
    quoter,
  )
import HaskellReviewer.Core.RichText (NameRef (..))
import HaskellReviewer.Core.RichText qualified as Rich (RefNamespace (..))
import HaskellReviewer.Core.SourceText (SourceLines, sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Col (..), Line (..), Span (..))
import HaskellReviewer.Core.Tokens (tokenCount, tokenSpanAt)

{- Note [Source links]

Each token gets one link word: 0 for none, else 1 + the index of its
target in a table where each distinct target is once. A target is:

- a local binder (its start; all uses of the binder share it);
- a declaration of ours: module, id, the line of its name, why it's a
  guess and how many candidates there were (1 unless ambiguous);
- a module of ours, when the declaration is unknown;
- an external module, with how sure we are and a Hoogle query.

Where links come from:

- A local: its binder. A top-level name of this module: its
  declaration; the occurrence table's own guesses (a field of several
  constructors, a record dot) get the reason 'ThisModule'.
- A free name: its resolution (Note [Resolution] in
  "HaskellReviewer.Core.Resolve"). Unresolved: no link.
- Built-ins and words that aren't names: no link.
- A definition or signature links to itself.
- A pun (@C {f}@) links the variable only, not the field.
- A quasi-quote links its quoter over the whole quasi-quote token.

Two uses share a target when everything about it is the same,
reasons and candidates too. Targets are in order of first use.

The Hoogle query (G3) is the name without its qualifier, plus
@ +module:M@ when we know the module (listed, or the only open
import). For @one-of@ the module is a guess: the query has the name
only.

The line of a declaration of another module comes from that module's
declarations, through the same module states resolution read: each
target module's names are read once. Its ref and file too, once per
module, not per target (a file is a map lookup, 'modulePath'). Linear
in tokens, plus the declarations of the modules linked to.

The outline is the top-level declarations with an id, in source order:
kind, id, the line of the name and the lines the declaration spans.
-}

-- * Input

-- | What 'sourceLinks' reads. The states should be the ones resolution
-- read, so the two agree.
data LinksInput = LinksInput
  { self :: ModuleId,
    index :: ModuleIndex,
    -- | The module's text.
    text :: Text,
    parsed :: ParsedModule,
    -- | 'Nothing': free names get no link.
    resolution :: Maybe ModuleResolution,
    state :: ModuleId -> ModuleState,
    -- | The file of a module ('modulePath'). Asked once per module
    -- linked to.
    pathOf :: ModuleId -> Maybe RelPath
  }

-- * Results

-- | See Note [Source links].
data SourceLinks = SourceLinks
  { -- | One word per token: 0 for none, else 1 + the target's index.
    links :: Vector.Vector Word32,
    -- | In order of first use.
    targets :: [LinkTarget],
    outline :: [OutlineEntry]
  }
  deriving (Eq, Show)

-- | Where a token links to.
data LinkTarget
  = ToLocal LocalLink
  | ToDecl DeclLink
  | ToModule ModuleLink
  | ToExternal ExternalLink
  deriving (Eq, Show)

-- | A local binder: where it starts.
data LocalLink = LocalLink
  { line :: Line,
    col :: Col
  }
  deriving (Eq, Show)

-- | A declaration of one of our modules.
data DeclLink = DeclLink
  { module_ :: ModuleId,
    ref :: Text,
    path :: RelPath,
    -- | Of its name. 'Nothing' if not found.
    line :: Maybe Line,
    id :: DeclId,
    -- | Empty when exact.
    reasons :: [LinkReason],
    -- | 1 unless ambiguous.
    candidates :: Int
  }
  deriving (Eq, Show)

-- | One of our modules, the declaration unknown.
data ModuleLink = ModuleLink
  { module_ :: ModuleId,
    ref :: Text,
    path :: Maybe RelPath,
    reasons :: [LinkReason],
    candidates :: Int
  }
  deriving (Eq, Show)

-- | A module of a dependency.
data ExternalLink = ExternalLink
  { module_ :: ExternalModule,
    how :: GuessHow,
    -- | 1, or how many open imports for 'HowOneOf'.
    count :: Int,
    -- | For Hoogle.
    query :: Text
  }
  deriving (Eq, Show)

-- | Why a link is a guess: resolution's reasons, plus this module's own
-- guesses. The wire names are in "HaskellReviewer.Server.Wire.Source".
data LinkReason
  = ReasonAmbiguous
  | ReasonNotReady
  | ReasonNotParsed
  | ReasonMadeBySplice
  | ReasonMadeBySpliceHere
  | ReasonViaSplice
  | ReasonWildcard
  | ReasonRecordDot
  | ReasonDataKinds
  | ReasonCppImport
  | ReasonImportCycle
  | ReasonAmbiguousModule
  | ReasonUnlistedModule
  | ReasonLooseImport
  | ReasonListedNotFound
  | -- | A guess of the occurrence table: a field of several
    -- constructors, a record dot.
    ReasonThisModule
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | How sure an external link is.
data GuessHow
  = -- | An import list names it.
    HowListed
  | -- | The only open external import.
    HowOnlyOpen
  | -- | The first of several open external imports.
    HowOneOf
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A top-level declaration with an id.
data OutlineEntry = OutlineEntry
  { kind :: OutlineKind,
    id :: DeclId,
    -- | Of its name.
    line :: Line,
    firstLine :: Line,
    lastLine :: Line
  }
  deriving (Eq, Show)

-- | Which kind of declaration.
data OutlineKind
  = OutlineType
  | OutlineClass
  | OutlineValue
  | OutlinePatSyn
  | OutlineInstance
  | OutlineFamInst
  deriving (Eq, Ord, Show, Enum, Bounded)

-- * Building

-- | The links and outline of a parsed module, with the module states
-- resolution read. See Note [Source links].
linksOf ::
  ModuleIndex ->
  (ModuleId -> ModuleState) ->
  ModuleId ->
  ParsedInput ->
  Maybe ModuleResolution ->
  SourceLinks
linksOf idx state self p res =
  sourceLinks
    LinksInput
      { self = self,
        index = idx,
        text = p.text,
        parsed = p.parsed,
        resolution = res,
        state = state,
        pathOf = modulePath idx
      }

-- | The links and outline of a parsed module. See Note [Source links].
sourceLinks :: LinksInput -> SourceLinks
sourceLinks input =
  SourceLinks
    { links = Vector.replicate n 0 Vector.// done.words,
      targets = map (targetOf places lines') (reverse done.new),
      outline = mapMaybe outlineEntry input.parsed.declarations
    }
  where
    n = tokenCount input.parsed.tokens
    ls = sourceLines input.text
    keyed =
      [ (i, k)
      | (TokenIx i, o) <- occurrenceList input.parsed.occurrences,
        Just k <- [keyOf input ls i o]
      ]
    done = foldl' assign (Assigned Map.empty 0 [] []) keyed
    -- Each decl target module's names, read once.
    lines' =
      Map.fromList
        [(m, declLines input m) | m <- nubOrd [m | KeyDecl m _ _ _ <- done.new]]
    -- Each target module's ref and file, found once.
    places =
      Map.fromList
        [ (m, (moduleRef input.index m, input.pathOf m))
        | m <- nubOrd (concatMap keyModule done.new)
        ]
    keyModule = \case
      KeyDecl m _ _ _ -> [m]
      KeyModule m _ _ -> [m]
      _ -> []

-- | Force it fully.
forceSourceLinks :: SourceLinks -> ()
forceSourceLinks s =
  s.links `seq`
    foldr (seq . forceTarget) () s.targets `seq`
      foldr (seq . entry) () s.outline
  where
    forceTarget = \case
      ToLocal l -> l `seq` ()
      ToDecl d -> d.ref `seq` d.line `seq` forceDeclId d.id `seq` reasons d.reasons
      ToModule m -> m.ref `seq` m.path `seq` reasons m.reasons
      ToExternal x -> forceExternalModule x.module_ `seq` x.query `seq` ()
    reasons = foldr seq ()
    entry e = forceDeclId e.id `seq` ()

-- * Reading

-- | A token's link word: 0 for none (and out of range).
linkAt :: SourceLinks -> Int -> Word32
linkAt s i = fromMaybe 0 (s.links Vector.!? i)

-- * Internal

-- | What makes a target distinct. See Note [Source links].
data Key
  = KeyLocal Line Col
  | KeyDecl ModuleId DeclId [LinkReason] Int
  | KeyModule ModuleId [LinkReason] Int
  | -- | The query: the name and maybe the module.
    KeyExternal ExternalModule GuessHow Int Text
  deriving (Eq, Ord)

-- | Targets so far: their indices, how many, the new ones (last
-- first), and the link words set (last first).
data Assigned = Assigned
  { seen :: Map Key Int,
    count :: Int,
    new :: [Key],
    words :: [(Int, Word32)]
  }

assign :: Assigned -> (Int, Key) -> Assigned
assign a (i, k) = case Map.lookup k a.seen of
  Just j -> a {words = (i, word j) : a.words}
  Nothing ->
    Assigned
      { seen = Map.insert k a.count a.seen,
        count = a.count + 1,
        new = k : a.new,
        words = (i, word a.count) : a.words
      }
  where
    word j = fromIntegral j + 1

-- | A token's target key, if it links.
keyOf :: LinksInput -> SourceLines -> Int -> Occurrence -> Maybe Key
keyOf input ls i o = case o.resolution of
  LocalRef b -> do
    binder <- binderAt input.parsed.occurrences b
    Just (KeyLocal binder.span.startLine binder.span.startCol)
  TopLevelRef d c -> Just (KeyDecl input.self d [ReasonThisModule | c == Occ.Heuristic] 1)
  FreeRef ctx -> do
    res <- input.resolution
    resolved <- IntMap.lookup i res.names
    fromResolved ls input i ctx resolved
  BuiltInRef -> Nothing
  NotAName _ -> Nothing

fromResolved :: SourceLines -> LinksInput -> Int -> FreeContext -> Resolved -> Maybe Key
fromResolved ls input i ctx = \case
  Exact t -> Just (home t [] 1)
  Heuristic rs t ->
    Just (home t (map reasonOf (NonEmpty.toList rs)) (candidatesOf (NonEmpty.toList rs)))
  External g -> Just (external g)
  Unresolved _ -> Nothing
  where
    home t rs k = case t of
      DeclTarget m d -> KeyDecl m d rs k
      ModuleTarget m -> KeyModule m rs k
    external g =
      let (how, k) = case g.how of
            Listed -> (HowListed, 1)
            OnlyOpenImport -> (HowOnlyOpen, 1)
            OneOf c -> (HowOneOf, c)
          ModuleName m = g.module_.name
          withModule = case how of
            HowOneOf -> ""
            _ -> " +module:" <> m
       in KeyExternal g.module_ how k (nameOf ls input i ctx <> withModule)
    candidatesOf rs = case [k | Ambiguous k <- rs] of
      k : _ -> k
      [] -> 1

-- | The name of a free token, as Hoogle wants it: no qualifier, no
-- parens or backticks.
nameOf :: SourceLines -> LinksInput -> Int -> FreeContext -> Text
nameOf ls input i ctx = case ctx of
  QuasiQuoter -> (quoter written).occ
  _ -> (nameRef Rich.ValueNs written).occ
  where
    written = sliceSpan ls (tokenSpanAt input.parsed.tokens i)

reasonOf :: Reason -> LinkReason
reasonOf = \case
  Ambiguous _ -> ReasonAmbiguous
  NotReady -> ReasonNotReady
  NotParsed -> ReasonNotParsed
  MadeBySplice -> ReasonMadeBySplice
  MadeBySpliceHere -> ReasonMadeBySpliceHere
  ViaSplice -> ReasonViaSplice
  Wildcard -> ReasonWildcard
  RecordDot -> ReasonRecordDot
  DataKinds -> ReasonDataKinds
  CppImport -> ReasonCppImport
  ImportCycle -> ReasonImportCycle
  ModuleGuess AmbiguousModule -> ReasonAmbiguousModule
  ModuleGuess UnlistedModule -> ReasonUnlistedModule
  ModuleGuess LooseImport -> ReasonLooseImport
  ListedNotFound -> ReasonListedNotFound

-- | The target of a key, with each module's ref and file. A
-- declaration of a module with no file (never happens: resolution only
-- finds declarations in parsed modules) is a module target.
targetOf ::
  Map ModuleId (Text, Maybe RelPath) ->
  Map ModuleId (Map DeclId Line) ->
  Key ->
  LinkTarget
targetOf places lineMaps = \case
  KeyLocal l c -> ToLocal LocalLink {line = l, col = c}
  KeyDecl m d rs k -> case place m of
    (ref, Just p) ->
      ToDecl
        DeclLink
          { module_ = m,
            ref = ref,
            path = p,
            line = Map.lookup d =<< Map.lookup m lineMaps,
            id = d,
            reasons = rs,
            candidates = k
          }
    (ref, Nothing) -> moduleLink m ref Nothing rs k
  KeyModule m rs k -> let (ref, p) = place m in moduleLink m ref p rs k
  KeyExternal x how k q ->
    ToExternal ExternalLink {module_ = x, how = how, count = k, query = q}
  where
    -- Every module of a key is in the map.
    place m = Map.findWithDefault ("", Nothing) m places
    moduleLink m ref p rs k =
      ToModule
        ModuleLink
          { module_ = m,
            ref = ref,
            path = p,
            reasons = rs,
            candidates = k
          }

-- | The line of each named declaration of a module (the first one, if
-- an id is there twice). Empty if the module isn't parsed.
declLines :: LinksInput -> ModuleId -> Map DeclId Line
declLines input m = case decls of
  Just ds ->
    Map.fromListWith
      (\_ old -> old)
      [(nd.info.id, nd.info.nameSpan.startLine) | d <- ds, nd <- namedDecls d]
  Nothing -> Map.empty
  where
    decls
      | m == input.self = Just input.parsed.declarations
      | otherwise = case input.state m of
          ModuleParsed p -> Just p.parsed.declarations
          _ -> Nothing

-- | A top-level declaration's outline entry. Splices have none.
outlineEntry :: Decl -> Maybe OutlineEntry
outlineEntry d = do
  info <- declInfo d
  kind <- case d of
    TypeD _ -> Just OutlineType
    ClassD _ -> Just OutlineClass
    ValueD _ -> Just OutlineValue
    PatSynD _ -> Just OutlinePatSyn
    InstanceD _ -> Just OutlineInstance
    FamInstD _ -> Just OutlineFamInst
    SpliceD _ -> Nothing
  let parts = info.extent.parts
      first = (NonEmpty.head parts).span
      final = (NonEmpty.last parts).span
  Just
    OutlineEntry
      { kind = kind,
        id = info.id,
        line = info.nameSpan.startLine,
        firstLine = first.startLine,
        lastLine = lastLineOf final
      }
  where
    -- End exclusive: a span that ends at column 1 ends on the line
    -- before.
    lastLineOf s
      | s.endCol == Col 1 && s.endLine > s.startLine =
          let Line l = s.endLine in Line (l - 1)
      | otherwise = s.endLine
