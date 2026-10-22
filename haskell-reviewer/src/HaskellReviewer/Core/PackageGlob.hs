-- | Turn @packages:@ entries into .cabal files, against the snapshot's
-- file list. Never looks at the real disk.
module HaskellReviewer.Core.PackageGlob
  ( -- * Types
    DirIndex,

    -- * Operations
    mkDirIndex,
    expandEntry,
    rootCabalFiles,

    -- * Internal (for tests)
    matchSegment,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.FileList (FileList (..))
import HaskellReviewer.Core.Project
  ( EntryIgnoredReason (..),
    PackageEntry (..),
    ProjectWarning (..),
    Required (..),
  )
import HaskellReviewer.Core.RelPath
  ( RelDir,
    RelPath,
    relDirSegments,
    relPathDirs,
    relPathName,
    relPathSegments,
    resolveUnder,
    rootDir,
  )

{- Note [Package globs]

What cabal does with a @packages:@ entry, on the file list:

- Relative to ROOT (where cabal.project is). A leading @./@ is dropped,
  @..@ is applied lexically; above ROOT is skipped with a warning.
- Each path segment can use @*@ (any run of chars), @{a,b}@
  (alternatives, can nest) and @\\@ (the next char is literal). A name
  that starts with @.@ only matches when a literal @.@ comes first in
  the pattern: @*@ skips hidden names.
- A segment that is exactly @**@, followed by exactly one last
  segment, matches that last segment against the names of all files
  at any depth below (like cabal, hidden dirs included). Anywhere
  else, @**@ is the same as @*@.
- A trailing @/@ matches dirs only.
- A match that is a .cabal file is used. A dir is used through the
  .cabal file in it: one is the normal case; several are all used,
  with a warning; none gives a warning (an Hpack one when there is a
  package.yaml). Other files are skipped.
- URLs, tarballs, absolute paths and @~@ are skipped with a warning:
  we never read outside the snapshot.
- A @packages:@ entry that matches nothing is a warning; an
  @optional-packages:@ one is not.

Matching never backtracks and never expands braces: we track the set
of positions in the name the pattern can reach, piece by piece. So it
takes about (pattern size × name length) steps, whatever the input
(the project file is untrusted, SPEC §9.1). An unclosed brace, a
stray @}@ and a comma outside braces are literal chars.

For braces in @packages:@ and the lexer, see Note [Braces in fields]
in "HaskellReviewer.Core.ProjectFile".

Known small differences from cabal:

- A @\\@ before a char other than @*{},@ is a dir separator in cabal;
  for us it escapes that char.
- With @{}@, @{a,}@ or unbalanced braces, cabal reads the whole entry
  as a literal path; we make only that segment literal.
- @a**@ matches @a@ for us, not in cabal. @**@ not followed by
  exactly one last segment is a plain glob for us; cabal reads the
  whole entry as a literal path.

Dirs only exist through the files under them: an empty dir is not in
the file list, so it never matches.
-}

-- | The dirs of a file list, for globbing. Build it once per list.
newtype DirIndex = DirIndex (Map RelDir (Map Text Child))

-- | A dir entry, or a glob match: a dir, or a file (any kind of list
-- entry).
data Child = ChildDir RelDir | ChildFile RelPath

-- | Index the dirs of a file list.
mkDirIndex :: FileList -> DirIndex
mkDirIndex fl =
  DirIndex (foldl' addPath (Map.singleton rootDir Map.empty) (Map.keys fl.entries))
  where
    addPath m p = foldl' add m (zip3 dirs (relPathSegments p) children)
      where
        dirs = relPathDirs p
        -- Each step goes into the next dir, the last one is the file.
        children = map ChildDir (drop 1 dirs) <> [ChildFile p]
    add m (dir, name, child) = Map.insertWith Map.union dir (Map.singleton name child) m

-- | The .cabal files of one entry. See Note [Package globs].
expandEntry :: DirIndex -> PackageEntry -> ([RelPath], [ProjectWarning])
expandEntry idx e
  | "://" `Text.isInfixOf` g || "file:" `Text.isPrefixOf` g = skip RemoteEntry
  | any (`Text.isSuffixOf` g) [".tar.gz", ".tgz"] = skip TarballEntry
  | "~" `Text.isPrefixOf` g = skip HomeDirEntry
  | otherwise = case resolveUnder rootDir (Text.dropWhileEnd (== '/') g) of
      Left p -> skip (EntryBadPath p)
      Right d -> case filter usable (matchGlob idx (relDirSegments d)) of
        [] -> ([], [EntryNoMatch e.at g | e.required == Required])
        found -> mconcat (map (packageOf idx) found)
  where
    g = e.glob
    dirOnly = "/" `Text.isSuffixOf` g
    skip r = ([], [EntryIgnored e.at g r])
    usable = \case
      ChildDir _ -> True
      ChildFile p -> not dirOnly && isCabalName (relPathName p)

-- | The *.cabal files in ROOT: the packages when there is no
-- cabal.project.
rootCabalFiles :: DirIndex -> [RelPath]
rootCabalFiles idx = cabalFilesIn idx rootDir

-- | Does a name match one glob segment? See Note [Package globs].
matchSegment :: Text -> Text -> Bool
matchSegment pat name = IntSet.member len (reach (IntSet.singleton 0) pieces)
  where
    pieces = parsePieces (balance (lexGlob (Text.unpack pat)))
    chars = IntMap.fromList (zip [0 ..] (Text.unpack name))
    len = Text.length name
    hidden = "." `Text.isPrefixOf` name

    -- Where in the name a list of pieces can end, from these starts.
    reach :: IntSet -> [Piece] -> IntSet
    reach = foldl' step
    step here = \case
      Lit c ->
        IntSet.fromList [i + 1 | i <- IntSet.toList here, IntMap.lookup i chars == Just c]
      Alts alts -> IntSet.unions [reach here alt | alt <- alts]
      Star ->
        -- A star can't start a hidden name, not even empty: only a
        -- literal dot can.
        let from = if hidden then IntSet.delete 0 here else here
         in case IntSet.minView from of
              Nothing -> IntSet.empty
              Just (lo, _) -> IntSet.fromList [lo .. len]

-- * Internal

-- | Paths that match the segments, from ROOT.
matchGlob :: DirIndex -> [Text] -> [Child]
matchGlob idx = go [ChildDir rootDir]
  where
    go here = \case
      [] -> here
      -- Files at any depth, by name. See Note [Package globs].
      ["**", pat] ->
        [ ChildFile p
        | ChildDir dir <- here,
          (name, p) <- filesUnder idx dir,
          matchSegment pat name
        ]
      pat : rest ->
        go
          [ child
          | ChildDir dir <- here,
            (name, child) <- Map.toList (childrenOf idx dir),
            matchSegment pat name
          ]
          rest

-- | Every file under a dir, at any depth (hidden dirs too), with its
-- name.
filesUnder :: DirIndex -> RelDir -> [(Text, RelPath)]
filesUnder idx dir = concatMap one (Map.toList (childrenOf idx dir))
  where
    one = \case
      (name, ChildFile p) -> [(name, p)]
      (_, ChildDir d) -> filesUnder idx d

-- | The .cabal file(s) of a match.
packageOf :: DirIndex -> Child -> ([RelPath], [ProjectWarning])
packageOf idx = \case
  ChildFile p -> ([p], [])
  ChildDir dir -> case cabalFilesIn idx dir of
    [one] -> ([one], [])
    [] -> ([], [if hasPackageYaml dir then HpackOnly dir else NoCabalFile dir])
    several -> (several, [SeveralCabalFiles dir several])
  where
    hasPackageYaml dir = case Map.lookup "package.yaml" (childrenOf idx dir) of
      Just (ChildFile _) -> True
      _ -> False

-- | The .cabal files directly in a dir.
cabalFilesIn :: DirIndex -> RelDir -> [RelPath]
cabalFilesIn idx dir =
  [p | (name, ChildFile p) <- Map.toList (childrenOf idx dir), isCabalName name]

childrenOf :: DirIndex -> RelDir -> Map Text Child
childrenOf (DirIndex m) dir = Map.findWithDefault Map.empty dir m

-- | @x.cabal@, not hidden.
isCabalName :: Text -> Bool
isCabalName n = ".cabal" `Text.isSuffixOf` n && not ("." `Text.isPrefixOf` n)

-- | A glob segment, lexed. Escapes are already literal chars.
data Tok = TLit Char | TStar | TOpen | TComma | TClose

-- | A glob segment, parsed.
data Piece = Lit Char | Star | Alts [[Piece]]

lexGlob :: String -> [Tok]
lexGlob = \case
  [] -> []
  '\\' : c : r -> TLit c : lexGlob r
  '*' : r -> TStar : lexGlob r
  '{' : r -> TOpen : lexGlob r
  ',' : r -> TComma : lexGlob r
  '}' : r -> TClose : lexGlob r
  c : r -> TLit c : lexGlob r

-- | Make unmatched braces, and commas outside braces, literal. After
-- this, every open brace has its close.
balance :: [Tok] -> [Tok]
balance toks = go (0 :: Int) (zip [0 ..] toks)
  where
    matched = pairs [] (zip [0 ..] toks)
    -- The positions of braces that have a partner.
    pairs :: [Int] -> [(Int, Tok)] -> IntSet
    pairs open = \case
      [] -> IntSet.empty
      (i, TOpen) : r -> pairs (i : open) r
      (i, TClose) : r -> case open of
        o : os -> IntSet.insert o (IntSet.insert i (pairs os r))
        [] -> pairs open r
      _ : r -> pairs open r
    go depth = \case
      [] -> []
      (i, TOpen) : r
        | IntSet.member i matched -> TOpen : go (depth + 1) r
        | otherwise -> TLit '{' : go depth r
      (i, TClose) : r
        | IntSet.member i matched -> TClose : go (depth - 1) r
        | otherwise -> TLit '}' : go depth r
      (_, TComma) : r
        | depth > 0 -> TComma : go depth r
        | otherwise -> TLit ',' : go depth r
      (_, t) : r -> t : go depth r

-- | Parse balanced tokens. Linear: each token is looked at once.
parsePieces :: [Tok] -> [Piece]
parsePieces = fst . pieces
  where
    -- Up to the end, or a comma or close brace of the enclosing group.
    pieces = \case
      TLit c : r -> first (Lit c) (pieces r)
      TStar : r -> first Star (pieces r)
      TOpen : r -> let (alts, r') = group r in first (Alts alts) (pieces r')
      rest -> ([], rest)
    group r = case pieces r of
      (alt, TComma : r') -> let (more, r'') = group r' in (alt : more, r'')
      (alt, TClose : r') -> ([alt], r')
      -- Can't happen after 'balance'.
      (alt, r') -> ([alt], r')
    first p (ps, r) = (p : ps, r)
