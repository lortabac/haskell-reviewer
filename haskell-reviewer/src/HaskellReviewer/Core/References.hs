{-# LANGUAGE StrictData #-}

-- | Find references (SPEC §11.4.5): the tokens of the whole side that
-- name one declaration. Pure. See Note [Find references].
module HaskellReviewer.Core.References
  ( -- * The index
    OccKeys,
    OccKey,
    occKeys,
    hasKey,
    keyHash,
    mayRefer,
    occKeyCount,

    -- * Finding
    RefsInput (..),
    RefKind (..),
    RefMatch (..),
    RefLine (..),
    Reference (..),
    RefsAnswer (..),
    findRefs,
    forceRefsAnswer,
    refsLimit,

    -- * Lines
    lineCap,
    refLine,
  )
where

import Control.Applicative ((<|>))
import Data.Bits (xor)
import Data.Char (ord)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector.Unboxed qualified as Vector
import Data.Word (Word64)
import HaskellReviewer.Core.Entities (ExportSet, forceExportSets)
import HaskellReviewer.Core.ModuleModel (ParsedModule (..))
import HaskellReviewer.Core.Modules (ModuleId)
import HaskellReviewer.Core.Names (DeclId, declName)
import HaskellReviewer.Core.Occurrences
  ( Occurrence (..),
    Resolution (..),
    Role (..),
    TokenIx (..),
    occurrenceAt,
    occurrenceList,
  )
import HaskellReviewer.Core.Occurrences qualified as Occ (Certainty (..))
import HaskellReviewer.Core.Resolve
  ( ModuleResolution (..),
    ModuleState (..),
    ParsedInput (..),
    ResolveInput (..),
    Resolved (..),
    Target (..),
    isPartial,
    occKey,
    resolveModuleOnly,
  )
import HaskellReviewer.Core.SourceLinks (LinkReason (..), linkReason)
import HaskellReviewer.Core.SourceText (SourceLines, sliceSpan, sourceLines)
import HaskellReviewer.Core.Span (Col (..), Span (..))
import HaskellReviewer.Core.Tokens (tokenSpanAt)

{- Note [Find references]

G5, D-294. Find references runs on demand, for one declaration, over
the whole side (every component). There is no reverse table.

Each parsed module keeps, from parse time, the keys of its free tokens
and pun fields ('occKeys'): the 'occKey' of each (the name, no
qualifier), as a 64-bit FNV-1a hash of its code points, sorted, each
once. A free token can only resolve to a declaration with that name,
so a module whose keys don't have the name can't refer to it. A hash
collision only adds a candidate.

For declaration d of module H:

- Candidates: H first, then the parsed modules whose keys have d's
  name ('mayRefer'), in the order the caller gives (module name, then
  path).
- In H: every token the occurrence table links to d, and every free
  token or pun field that resolves to d (in a splice, @type data@).
- Elsewhere: only the free tokens and pun fields named like d are
  resolved ('resolveModuleOnly'), with the memo of final export sets.
  New final sets of one candidate serve the next ones. Those that
  resolve to d, exact or heuristic, are references.
- Not references: results on a module only ('ModuleTarget'), names in
  doc comments, @T(..)@ (no token per name).
- Kinds: an equation, a signature, a fixity, a pragma, a method in an
  instance: binding. Import and export lists: import, export. Anything
  else, and a pun's field: use.
- Gap: an ambiguous name ('Ambiguous') resolves to its first candidate
  only, so it's missed when d isn't first.
- Partial: a slot isn't done yet (the caller says), or a candidate's
  resolution read a module not ready. Modules that failed, are text
  only or too large don't count: they never get parsed.

Results go in candidate order, then token order (line, column). The
first 'refsLimit' are kept, with their lines ('refLine'); 'total'
counts them all.
-}

-- * The index

-- | The keys of a module's free tokens and pun fields: hashes, sorted,
-- each once. See Note [Find references].
newtype OccKeys = OccKeys (Vector.Vector Word64)
  deriving (Eq, Show)

-- | One key: the hash of a name ('keyHash').
newtype OccKey = OccKey Word64
  deriving (Eq, Ord, Show)

-- | The keys of a parsed module, from its text. An unboxed vector:
-- in WHNF it holds no thunk.
occKeys :: Text -> ParsedModule -> OccKeys
occKeys text p = OccKeys (Vector.fromListN (Set.size keys) (Set.toAscList keys))
  where
    ls = sourceLines text
    keys =
      Set.fromList
        [ h
        | (TokenIx i, o) <- occurrenceList p.occurrences,
          c <- free o.resolution <> maybe [] free o.punField,
          let OccKey h = keyHash (occKey c (sliceSpan ls (tokenSpanAt p.tokens i)))
        ]
    free = \case
      FreeRef c -> [c]
      _ -> []

-- | Is this a key of the module?
hasKey :: OccKeys -> OccKey -> Bool
hasKey (OccKeys v) (OccKey h) = go 0 (Vector.length v)
  where
    go lo hi
      | lo >= hi = False
      | otherwise =
          let mid = (lo + hi) `div` 2
           in case compare (v Vector.! mid) h of
                EQ -> True
                LT -> go (mid + 1) hi
                GT -> go lo mid

-- | A name's key: 64-bit FNV-1a, over code points.
keyHash :: Text -> OccKey
keyHash = OccKey . Text.foldl' step 14695981039346656037
  where
    step h c = (h `xor` fromIntegral (ord c)) * 1099511628211

-- | Can a module with these keys refer to the declaration? Only if
-- they have its name. Instances have no name: always False. See Note
-- [Find references].
mayRefer :: DeclId -> OccKeys -> Bool
mayRefer d keys = maybe False (hasKey keys . keyHash) (declName d)

-- | How many keys. For tests and measures.
occKeyCount :: OccKeys -> Int
occKeyCount (OccKeys v) = Vector.length v

-- * Finding

-- | What 'findRefs' reads.
data RefsInput = RefsInput
  { -- | The module states and the memo, read at once.
    resolve :: ResolveInput,
    -- | The declaration's module: always looked in, first.
    home :: ModuleId,
    decl :: DeclId,
    -- | The other modules to look in, in results order: those whose
    -- keys have the name ('mayRefer'). Home is skipped if there.
    others :: [ModuleId],
    -- | A slot isn't done yet.
    indexing :: Bool
  }

-- | What a reference does. See Note [Find references].
data RefKind = RefUse | RefImport | RefExport | RefBinding
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | One token that names the declaration.
data RefMatch = RefMatch
  { module_ :: ModuleId,
    span :: Span,
    kind :: RefKind,
    -- | Empty when exact.
    reasons :: [LinkReason]
  }
  deriving (Eq, Show)

-- | The line of a reference, cut to 'lineCap' code points around it.
data RefLine = RefLine
  { -- | The column of the first code point here.
    col :: Col,
    text :: Text
  }
  deriving (Eq, Show)

-- | A reference and its line.
data Reference = Reference
  { match :: RefMatch,
    line :: RefLine
  }
  deriving (Eq, Show)

-- | The answer. See Note [Find references].
data RefsAnswer = RefsAnswer
  { partial :: Bool,
    -- | Every reference, before the cap.
    total :: Int,
    -- | The first 'refsLimit'.
    results :: [Reference],
    -- | How many modules were resolved, home too.
    resolved :: Int,
    -- | Final export sets computed here, not in the memo yet.
    newFinal :: Map ModuleId ExportSet
  }

-- | At most this many results (G5).
refsLimit :: Int
refsLimit = 1000

-- | Find the references. See Note [Find references].
findRefs :: RefsInput -> RefsAnswer
findRefs input =
  RefsAnswer
    { partial = input.indexing || or [isPartial r | (_, _, r) <- done],
      total = length matches,
      results = withLines input.resolve.state (take refsLimit matches),
      resolved = length done,
      newFinal = Map.difference memo input.resolve.memo
    }
  where
    keep = maybe (const False) (==) (declName input.decl)
    candidates = input.home : filter (/= input.home) input.others
    (memo, doneRev) = foldl' step (input.resolve.memo, []) candidates
    done = reverse doneRev
    step (m, acc) i = case input.resolve.state i of
      ModuleParsed p
        | Just r <- resolveModuleOnly input.resolve {memo = m} i keep ->
            (m `Map.union` r.newFinal, (i, p, r) : acc)
      _ -> (m, acc)
    matches = concat [moduleMatches input i p r | (i, p, r) <- done]

-- | Force it fully.
forceRefsAnswer :: RefsAnswer -> ()
forceRefsAnswer a =
  a.partial `seq`
    a.total `seq`
      a.resolved `seq`
        foldr (seq . ref) () a.results `seq`
          forceExportSets a.newFinal
  where
    ref r =
      r.match.span `seq`
        r.match.kind `seq`
          foldr seq () r.match.reasons `seq`
            r.line

-- | The references in one candidate, in token order.
moduleMatches :: RefsInput -> ModuleId -> ParsedInput -> ModuleResolution -> [RefMatch]
moduleMatches input self p res =
  [x | (TokenIx i, o) <- tokens, Just x <- [hitAt i o]]
  where
    isHome = self == input.home
    occs = p.parsed.occurrences
    -- Elsewhere, only the tokens resolved can be references.
    tokens
      | isHome = occurrenceList occs
      | otherwise =
          [ (TokenIx i, o)
          | let resolved = IntMap.keysSet res.names <> IntMap.keysSet res.punFields,
            i <- IntSet.toAscList resolved,
            Just o <- [occurrenceAt occs (TokenIx i)]
          ]
    hitAt i o = mainHit <|> punHit
      where
        mainHit = case o.resolution of
          TopLevelRef d c | isHome, d == input.decl -> Just (ours (kindOf o.role) c)
          FreeRef _ -> resolvedHit (kindOf o.role) =<< IntMap.lookup i res.names
          _ -> Nothing
        punHit = case o.punField of
          Just (TopLevelRef d c) | isHome, d == input.decl -> Just (ours RefUse c)
          Just (FreeRef _) -> resolvedHit RefUse =<< IntMap.lookup i res.punFields
          _ -> Nothing
        ours k c = matchOf k [ReasonThisModule | c == Occ.Heuristic]
        resolvedHit k = \case
          Exact (DeclTarget m d) | isTarget m d -> Just (matchOf k [])
          Heuristic rs (DeclTarget m d)
            | isTarget m d -> Just (matchOf k (map linkReason (NonEmpty.toList rs)))
          _ -> Nothing
        matchOf k rs =
          RefMatch
            { module_ = self,
              span = tokenSpanAt p.parsed.tokens i,
              kind = k,
              reasons = rs
            }
    isTarget m d = m == input.home && d == input.decl

-- | What a token's role makes of it.
kindOf :: Role -> RefKind
kindOf = \case
  Use -> RefUse
  Def -> RefBinding
  Sig -> RefBinding
  InstanceDef -> RefBinding
  Import -> RefImport
  Export -> RefExport

-- | Add the lines. Each module's lines are built once.
withLines :: (ModuleId -> ModuleState) -> [RefMatch] -> [Reference]
withLines state = go
  where
    go = \case
      [] -> []
      ms@(m : _) ->
        let (same, rest) = span (\x -> x.module_ == m.module_) ms
            ls = case state m.module_ of
              ModuleParsed p -> sourceLines p.text
              _ -> sourceLines ""
         in [Reference x (refLine ls x.span) | x <- same] <> go rest

-- * Lines

-- | How many code points of a line a reference sends.
lineCap :: Int
lineCap = 240

-- | The line a span starts on: all of it, if it's short; else
-- 'lineCap' code points with the span in the middle, moved to stay on
-- the line. A span over several lines counts to the end of its first.
-- Cost bounded by 'lineCap', after the line's offsets (Note [Slicing]
-- in "HaskellReviewer.Core.SourceText").
refLine :: SourceLines -> Span -> RefLine
refLine ls s
  -- Short line: all of it.
  | Text.length lineHead <= lineCap = RefLine (Col 1) lineHead
  -- Near the end: the last 'lineCap' code points.
  | Text.length window < lineCap =
      windowAt (max 1 (windowStart + Text.length window - lineCap))
  -- Else the window around the span.
  | otherwise = RefLine (toCol windowStart) window
  where
    Col startCol = s.startCol
    Col endCol = s.endCol
    spanStart = fromIntegral startCol :: Int
    -- The span's length on its first line, at most the cap.
    spanLength
      | s.endLine == s.startLine = min lineCap (max 0 (fromIntegral endCol - spanStart))
      | otherwise = lineCap
    windowStart = max 1 (spanStart - (lineCap - spanLength) `div` 2)
    window = codePoints windowStart lineCap
    windowAt a = RefLine (toCol a) (codePoints a lineCap)
    -- One more than the cap: tells a short line from a long one.
    lineHead = codePoints 1 (lineCap + 1)
    -- n code points from column a.
    codePoints a n = sliceSpan ls (Span s.startLine (toCol a) s.startLine (toCol (a + n)))
    toCol = Col . fromIntegral
