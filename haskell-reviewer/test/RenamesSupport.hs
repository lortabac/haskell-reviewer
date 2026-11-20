-- | Shared by the rename tests: cases, our pairing of them, and git as
-- an oracle (@git log --raw -M@ over many cases in one repo, or @git
-- diff --raw -M@ on a fixture repo).
module RenamesSupport
  ( -- * Cases
    Item (..),
    Tree,
    Case (..),
    caseEntries,
    caseContents,
    ourRows,
    genCase,
    genContent,

    -- * Rows
    Row (..),
    pairingRows,

    -- * Oracle
    gitCaseRows,
    RepoSides (..),
    repoSides,
    gitDiffRows,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.List (nubBy, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.BlobId (blobId)
import HaskellReviewer.Core.Git (ObjectFormat (..), ObjectId (..))
import HaskellReviewer.Core.RelPath (RelPath, parseRelPath, relPathText)
import HaskellReviewer.Core.Renames
  ( Blob (..),
    Change (..),
    ChangeNote (..),
    DiffEntry (..),
    ExecChange (..),
    ModifiedFile (..),
    Pairing (..),
    RenamedFile (..),
    Settings,
    pairFiles,
  )
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Core.Similarity (Spans, hashChars, scorePercent)
import HaskellReviewer.Core.WorkTree (Exec (..))
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import IndexSupport (rel)
import System.Exit (ExitCode (..))
import Test.Tasty.HUnit (assertFailure)
import TestSupport (GitRun (..), gitBytes, withCanonicalTemp)

-- * Cases

-- | A path's contents.
data Item
  = IFile Exec ByteString
  | ISymlink ByteString
  | -- | A gitlink, with its commit (40 hex digits).
    ISubmodule Text
  deriving (Eq, Show)

-- | One side: paths (unique, ROOT-relative) and their items.
type Tree = [(Text, Item)]

-- | Base and head of one case.
data Case = Case {base :: Tree, head :: Tree}
  deriving (Eq, Show)

-- | A case's entries, with blob ids and sizes.
caseEntries :: Case -> Pair (Map RelPath DiffEntry)
caseEntries c = Pair {head = side c.head, base = side c.base}
  where
    side t = Map.fromList [(rel p, entry i) | (p, i) <- t]
    entry = \case
      IFile x b -> File x (blob b)
      ISymlink b -> Symlink (blob b)
      ISubmodule h -> Submodule (ObjectId h)
    blob b = Blob {oid = Just (blobId Sha1 b), size = Just (BS.length b)}

-- | Every file's span table.
caseContents :: Case -> Pair (Map RelPath Spans)
caseContents c = Pair {head = side c.head, base = side c.base}
  where
    side t = Map.fromList [(rel p, hashChars b) | (p, IFile _ b) <- t]

-- | Our rows for a case.
ourRows :: Settings -> Case -> [Row]
ourRows settings c =
  pairingRows (caseEntries c) (pairFiles settings (caseEntries c) (caseContents c))

-- | A small repo change: up to 8 files on base, kept, edited, moved
-- (with or without edits), removed, chmod-ed or turned into a symlink,
-- and up to 3 added ones. Few dirs and names, so basenames clash.
-- Sizes don't depend on Hedgehog's size.
genCase :: Gen Case
genCase = do
  base <- uniq <$> Gen.list (Range.constant 0 8) ((,) <$> genPath <*> genItem)
  heads <- concat <$> mapM evolve base
  added <- Gen.list (Range.constant 0 3) $ do
    p <- genPath
    i <-
      Gen.frequency
        [ (2, genItem),
          (3, if null base then genItem else Gen.element (map snd base) >>= editItem)
        ]
    pure (p, i)
  pure Case {base = base, head = uniq (heads <> added)}
  where
    uniq = nubBy (\a b -> fst a == fst b)
    evolve (p, i) =
      Gen.frequency
        [ (2, pure [(p, i)]),
          (2, (\j -> [(p, j)]) <$> editItem i),
          (1, pure []),
          (4, (\q j -> [(q, j)]) <$> genPath <*> editItem i),
          (2, (\q -> [(q, i)]) <$> genPath),
          -- Another dir, same name, a small edit: the basename step.
          (3, (\dir j -> [(dir <> baseName p, j)]) <$> genDir <*> smallEdit i),
          (1, pure [(p, chmod i)]),
          (1, (\t -> [(p, ISymlink t)]) <$> genTarget)
        ]
    chmod = \case
      IFile NotExecutable b -> IFile Executable b
      IFile Executable b -> IFile NotExecutable b
      other -> other

genPath :: Gen Text
genPath = (<>) <$> genDir <*> Gen.element ["x.hs", "y.hs", "z.txt", "w", "X.hs"]

genDir :: Gen Text
genDir = Gen.element ["", "a/", "b/", "a/c/"]

baseName :: Text -> Text
baseName = Text.takeWhileEnd (/= '/')

-- | One line added to a file, or nothing.
smallEdit :: Item -> Gen Item
smallEdit = \case
  IFile x b -> (\l -> IFile x (b <> l <> "\n")) <$> genLine
  other -> pure other

genTarget :: Gen ByteString
genTarget = Gen.element ["t1", "t2", "../t1"]

genItem :: Gen Item
genItem =
  Gen.frequency
    [ (12, IFile NotExecutable <$> genContent),
      (2, IFile Executable <$> genContent),
      (2, ISymlink <$> genTarget),
      (1, ISubmodule <$> Gen.element [Text.replicate 40 "1", Text.replicate 40 "2"])
    ]

-- | Edit a file by 0 to 6 line edits; symlinks get a new target or not.
editItem :: Item -> Gen Item
editItem = \case
  IFile x b -> IFile x <$> editBytes b
  ISymlink t -> Gen.frequency [(1, pure (ISymlink t)), (1, ISymlink <$> genTarget)]
  ISubmodule h -> pure (ISubmodule h)

editBytes :: ByteString -> Gen ByteString
editBytes b = do
  k <- Gen.int (Range.constant 0 6)
  BS.concat <$> go k (Char8.lines b)
  where
    go :: Int -> [ByteString] -> Gen [ByteString]
    go 0 ls = pure (map (<> "\n") ls)
    go k ls = do
      i <- Gen.int (Range.constant 0 (length ls))
      let (x, y) = splitAt i ls
      ls' <-
        Gen.choice
          [ (\l -> x <> [l] <> y) <$> genLine,
            pure (x <> drop 1 y),
            (\l -> x <> [l] <> drop 1 y) <$> genLine
          ]
      go (k - 1) ls'

-- | 0 to 14 lines from a small pool: short and long (over 64 bytes)
-- lines, CRLF ones, sometimes a NUL (binary), sometimes no final
-- newline.
genContent :: Gen ByteString
genContent = do
  ls <- Gen.list (Range.constant 0 14) genLine
  nul <- Gen.frequency [(9, pure ""), (1, pure "\0")]
  final <- Gen.frequency [(5, pure "\n"), (1, pure "")]
  pure $ case ls of
    [] -> nul
    _ -> nul <> BS.intercalate "\n" ls <> final

genLine :: Gen ByteString
genLine =
  Gen.frequency
    [ (6, (\n -> "line " <> Char8.pack (show n)) <$> Gen.int (Range.constant 0 30)),
      (1, (\n -> "crlf " <> Char8.pack (show n) <> "\r") <$> small),
      (1, (\n -> Char8.replicate (60 + n) 'q') <$> Gen.int (Range.constant 0 80)),
      (1, pure "")
    ]
  where
    small = Gen.int (Range.constant 0 5)

-- * Rows

-- | One raw diff record, as far as we compare: modes, status letter,
-- score (renames), paths.
data Row = Row
  { oldMode :: Text,
    newMode :: Text,
    status :: Char,
    score :: Maybe Int,
    from :: Maybe Text,
    path :: Text
  }
  deriving (Eq, Ord, Show)

-- | Our pairing as git's rows, sorted. Modes come from the entries, as
-- git prints them. A mode note that doesn't fit them (an exec bit
-- change on M or R) turns the status into @!@, so it fails against
-- git's rows.
pairingRows :: Pair (Map RelPath DiffEntry) -> Pairing -> [Row]
pairingRows entries p = sort (map row p.changes)
  where
    row = \case
      Added a -> Row none (headMode a) 'A' Nothing Nothing (relPathText a)
      Removed a -> Row (baseMode a) none 'D' Nothing Nothing (relPathText a)
      Modified m ->
        let s = case m.note of
              Just TypeChanged -> 'T'
              Just ModeChanged -> if execDiffers m.path m.path then 'M' else '!'
              Nothing -> if execDiffers m.path m.path then '!' else 'M'
         in Row (baseMode m.path) (headMode m.path) s Nothing Nothing (relPathText m.path)
      Renamed r ->
        let s = if (r.exec == ExecChanged) == execDiffers r.from r.to then 'R' else '!'
         in Row
              (baseMode r.from)
              (headMode r.to)
              s
              (Just (scorePercent r.score))
              (Just (relPathText r.from))
              (relPathText r.to)
    none = "000000"
    baseMode a = maybe "?" modeOf (Map.lookup a entries.base)
    headMode a = maybe "?" modeOf (Map.lookup a entries.head)
    execDiffers a b = case (Map.lookup a entries.base, Map.lookup b entries.head) of
      (Just (File x _), Just (File y _)) -> x /= y
      _ -> False
    modeOf = \case
      File NotExecutable _ -> "100644"
      File Executable _ -> "100755"
      Symlink _ -> "120000"
      Submodule _ -> "160000"

-- | Parse @--raw -z@ records, split at @CASE n@ lines (from
-- @--format=CASE %s@) if any. Gives rows per case, in case order.
parseRaw :: ByteString -> Either String [(Text, [Row])]
parseRaw out = finish <$> go [] (filter (not . BS.null) (map strip (BS.split 0 out)))
  where
    strip = Char8.dropWhile (== '\n')
    go acc = \case
      [] -> Right acc
      t : rest
        | Just n <- BS.stripPrefix "CASE " t -> go ((txt n, []) : acc) rest
        | Char8.isPrefixOf ":" t -> case (Char8.words (BS.drop 1 t), rest) of
            ([m1, m2, _, _, st], a : b : rest')
              | Just ('R', n) <- Char8.uncons st ->
                  let r = Row (txt m1) (txt m2) 'R' (Just (read (Char8.unpack n)))
                   in go (add (r (Just (txt a)) (txt b)) acc) rest'
            ([m1, m2, _, _, st], a : rest')
              | Just (c, "") <- Char8.uncons st ->
                  go (add (Row (txt m1) (txt m2) c Nothing Nothing (txt a)) acc) rest'
            _ -> Left ("bad raw record: " <> show t)
        | otherwise -> Left ("unexpected: " <> show t)
    add r = \case
      (n, rs) : acc -> (n, r : rs) : acc
      [] -> [("", [r])]
    finish = reverse . map (\(n, rs) -> (n, sort rs))
    txt = Text.Encoding.decodeUtf8

-- * Oracle

-- | git's rows for each case: all cases go into one repo by
-- @fast-import@ (a root commit for base, a child for head), then one
-- @git log --raw -M@ over the heads, with the extra diff options.
gitCaseRows :: [String] -> [Case] -> IO [[Row]]
gitCaseRows extra cases = withCanonicalTemp $ \dir -> do
  run dir ["init", "-q", "--object-format=sha1"] ""
  let input = BS.concat (zipWith stream [0 :: Int ..] cases)
  run dir (flags <> ["fast-import", "--quiet"]) input
  r <-
    gitBytes
      dir
      ( flags
          <> [ "log",
               "--no-walk=unsorted",
               "--raw",
               "-z",
               "-M",
               "--no-abbrev",
               "--no-ext-diff",
               "--no-textconv",
               "--no-color",
               "--format=CASE %s"
             ]
          <> extra
          <> ["refs/heads/h" <> show i | i <- [0 .. length cases - 1]]
      )
      ""
  case (r.code, parseRaw r.out) of
    (ExitSuccess, Right perCase)
      | map fst perCase == [Text.pack (show i) | i <- [0 .. length cases - 1]] ->
          pure (map snd perCase)
      | otherwise -> assertFailure ("git log: cases missing: " <> show (map fst perCase))
    (ExitSuccess, Left e) -> assertFailure e
    _ -> assertFailure ("git log failed: " <> Char8.unpack r.err)
  where
    stream i c =
      BS.concat
        [ commit baseRef (show i) Nothing c.base,
          commit ("refs/heads/h" <> show i) (show i) (Just baseRef) c.head
        ]
      where
        baseRef = "refs/heads/b" <> show i
    commit ref msg parent tree =
      BS.concat $
        [ Char8.pack ("commit " <> ref <> "\n"),
          "committer T <t@example.com> 1767225600 +0000\n",
          dataBlock (Char8.pack msg)
        ]
          <> maybe [] (\p -> [Char8.pack ("from " <> p <> "\n"), "deleteall\n"]) parent
          <> map modify tree
          <> ["\n"]
    modify (p, item) =
      let path = Text.Encoding.encodeUtf8 p
       in case item of
            IFile x b -> BS.concat ["M ", fileMode x, " inline ", path, "\n", dataBlock b]
            ISymlink b -> BS.concat ["M 120000 inline ", path, "\n", dataBlock b]
            ISubmodule h ->
              BS.concat ["M 160000 ", Text.Encoding.encodeUtf8 h, " ", path, "\n"]
    fileMode = \case
      NotExecutable -> "100644"
      Executable -> "100755"
    dataBlock b = BS.concat ["data ", Char8.pack (show (BS.length b)), "\n", b, "\n"]

-- | Pinned config for the oracle: nothing from the user, no rename
-- config. @core.ignorecase=false@: on a case-insensitive file system
-- (macOS) @git init@ sets it to true, and then fast-import folds paths
-- that differ only by case (@x.hs@, @X.hs@) into one tree entry. The
-- repo has no work tree files, so false is safe there.
flags :: [String]
flags =
  [ "-c",
    "core.ignorecase=false",
    "-c",
    "core.autocrlf=false",
    "-c",
    "diff.renames=true",
    "-c",
    "core.quotePath=false"
  ]

run :: FilePath -> [String] -> ByteString -> IO ()
run dir args input = do
  r <- gitBytes dir args input
  case r.code of
    ExitSuccess -> pure ()
    _ -> assertFailure ("git " <> unwords args <> " failed: " <> Char8.unpack r.err)

-- | Both sides of a fixture repo: entries and contents.
data RepoSides = RepoSides
  { entries :: Pair (Map RelPath DiffEntry),
    contents :: Pair (Map RelPath Spans)
  }

-- | The sides of two revisions, under a prefix of the repo
-- (@""@ or @"pkg/"@): @ls-tree -r -z --long@, contents by
-- @cat-file --batch@.
repoSides :: FilePath -> String -> String -> String -> IO RepoSides
repoSides dir prefix baseRev headRev = do
  b <- side baseRev
  h <- side headRev
  pure
    RepoSides
      { entries = Pair {head = fmap fst h, base = fmap fst b},
        contents = Pair {head = Map.mapMaybe snd h, base = Map.mapMaybe snd b}
      }
  where
    side :: String -> IO (Map RelPath (DiffEntry, Maybe Spans))
    side rev = do
      let spec = if null prefix then rev else rev <> ":" <> init prefix
      r <- gitBytes dir ["ls-tree", "-r", "-z", "--long", spec] ""
      let records = filter (not . BS.null) (BS.split 0 r.out)
      rows <- either assertFailure pure (traverse treeRow records)
      let blobs = [oid | (_, File _ Blob {oid = Just (ObjectId oid)}) <- rows]
          ids = Char8.unlines (map Text.Encoding.encodeUtf8 blobs)
      c <- gitBytes dir ["cat-file", "--batch"] ids
      byId <- either assertFailure pure (catBatch c.out)
      pure (Map.fromList [(p, (e, spansFor byId e)) | (p, e) <- rows])
    spansFor byId = \case
      File _ Blob {oid = Just (ObjectId o)} -> hashChars <$> Map.lookup o byId
      _ -> Nothing
    treeRow t = case Char8.break (== '\t') t of
      (meta, rest) | Just ('\t', p) <- Char8.uncons rest -> case Char8.words meta of
        [m, _, o, s] -> do
          path <- either (Left . show) Right (parseRelPath p)
          let oid = ObjectId (Text.Encoding.decodeLatin1 o)
              size = fst <$> Char8.readInt s
              blob = Blob {oid = Just oid, size = size}
          entry <- case m of
            "100644" -> Right (File NotExecutable blob)
            "100755" -> Right (File Executable blob)
            "120000" -> Right (Symlink blob)
            "160000" -> Right (Submodule oid)
            _ -> Left ("mode " <> show m)
          Right (path, entry)
        _ -> Left ("bad ls-tree row " <> show t)
      _ -> Left ("bad ls-tree row " <> show t)

-- | Parse @cat-file --batch@ output: blobs by id.
catBatch :: ByteString -> Either String (Map Text ByteString)
catBatch = go Map.empty
  where
    go acc b
      | BS.null b = Right acc
      | otherwise = case Char8.words header of
          [oid, "blob", n]
            | Just (size, "") <- Char8.readInt n ->
                let (body, rest') = BS.splitAt size (BS.drop 1 rest)
                    key = Text.Encoding.decodeLatin1 oid
                 in go (Map.insert key body acc) (BS.drop 1 rest')
          _ -> Left ("bad cat-file header " <> show header)
      where
        (header, rest) = Char8.break (== '\n') b

-- | git's rows for two revisions: @git diff --raw -z -M --relative@,
-- run in a dir (ROOT), with extra options (e.g. @-l2@).
gitDiffRows :: FilePath -> [String] -> String -> String -> IO [Row]
gitDiffRows dir extra baseRev headRev = do
  r <-
    gitBytes
      dir
      ( flags
          <> ["diff", "--raw", "-z", "-M", "--relative", "--no-abbrev"]
          <> ["--no-ext-diff", "--no-textconv"]
          <> extra
          <> [baseRev, headRev]
      )
      ""
  case (r.code, parseRaw r.out) of
    (ExitSuccess, Right [(_, rows)]) -> pure rows
    (ExitSuccess, Right []) -> pure []
    (ExitSuccess, other) -> assertFailure ("git diff: " <> show other)
    _ -> assertFailure ("git diff failed: " <> Char8.unpack r.err)
