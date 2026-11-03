-- | Census: every .lhs/.lhsig of some project dirs, our unlit vs the
-- library's, as the product would read them.
module Census (census, codeLines) where

import Check (Result (..), badNames, nameCount, parseResult)
import Control.Monad (filterM, forM, forM_)
import Data.ByteString qualified as BS
import Data.List (isSuffixOf, nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.IO qualified as Text.IO
import HaskellReviewer.Core.Cpp.Positions (mapSpan)
import HaskellReviewer.Core.FileList (ListSource (..), fileListCap, mkFileList)
import HaskellReviewer.Core.Modules
  ( ComponentModules (..),
    ComponentSettings,
    HaskellFile (..),
    Literate (..),
    LooseFile (..),
    ModuleEntry (..),
    ModuleIndex (..),
    ModuleSource (..),
    OtherFile (..),
    PackageModules (..),
    PreprocessedFile (..),
    buildModuleIndex,
    looseSettings,
  )
import HaskellReviewer.Core.Parser (ParseSettings (..), moduleParseSettings)
import HaskellReviewer.Core.Parser qualified as P
import HaskellReviewer.Core.Project (GhcVersion, hostPlatform)
import HaskellReviewer.Core.RelPath (RelPath, relPathText)
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Core.SourceText
  ( LooseUnlit (..),
    stripBom,
    unlitLoose,
    unlitMarkdown,
  )
import HaskellReviewer.Effects.FileSystem (WalkResult (..), canonicalRoot, walkFiles)
import HaskellReviewer.Session.Project (readProject)
import LibUnlit
  ( Block (..),
    blocks,
    libLayout,
    libUnlitLooseRaw,
    reorderKey,
    selectedBlocks,
    selects,
  )
import System.Directory qualified as Dir
import System.FilePath ((</>))

-- | One literate file and how the product reads it.
data LitFile = LitFile
  { project :: FilePath,
    root :: FilePath,
    path :: RelPath,
    mode :: Text,
    settings :: Maybe ParseSettings
  }

-- | Write one TSV row per literate file to OUT, and print them.
census :: FilePath -> [FilePath] -> IO ()
census out dirs = do
  roots <- concat <$> mapM projectRoots dirs
  files <- concat <$> mapM litFiles roots
  rows <- forM files row
  let header =
        [ "project",
          "file",
          "mode",
          "blocks_ours",
          "blocks_lib",
          "diff_lines",
          "classes",
          "top_blocks",
          "loose_ours",
          "loose_lib",
          "parse_ours",
          "parse_lib",
          "names_ours",
          "bad_ours",
          "names_lib",
          "bad_lib",
          "same_tokens"
        ]
  Text.IO.writeFile out (Text.unlines (map (Text.intercalate "\t") (header : rows)))
  putStrLn ("files: " <> show (length rows))
  forM_ rows (Text.IO.putStrLn . Text.intercalate "\t")

-- | A dir with a cabal.project or a .cabal is a project; else its
-- subdirs (not symlinks) are.
projectRoots :: FilePath -> IO [FilePath]
projectRoots d = do
  here <- isProject d
  if here
    then pure [d]
    else do
      subs <- map (d </>) . sort <$> Dir.listDirectory d
      real <- filterM realDir subs
      filterM isProject real
  where
    realDir s = (&&) <$> Dir.doesDirectoryExist s <*> (not <$> Dir.pathIsSymbolicLink s)
    isProject p = do
      fs <- Dir.listDirectory p
      pure (any (\f -> f == "cabal.project" || ".cabal" `isSuffixOf` f) fs)

-- | The literate files of a project, with the product's mode.
litFiles :: FilePath -> IO [LitFile]
litFiles dir = do
  canon <- canonicalRoot (Root dir)
  w <- walkFiles canon fileListCap
  let fl = mkFileList FromDisk w.skippedNames w.entries
  proj <- readProject canon fl hostPlatform Nothing
  let idx = buildModuleIndex proj fl
  pure (dedupe (fromIndex dir idx))
  where
    dedupe fs =
      Map.elems (Map.fromListWith (\_ old -> old) [(relPathText f.path, f) | f <- fs])

fromIndex :: FilePath -> ModuleIndex -> [LitFile]
fromIndex dir idx = owned <> loose
  where
    ghc = idx.ghc
    owned =
      [ lit f.path (modeName f.literate) (Just (settingsFor ghc c.settings f))
      | p <- idx.packages,
        c <- p.components,
        m <- c.modules,
        HaskellSource f <- [m.source],
        f.literate /= NotLiterate
      ]
        <> [ lit f.path ("preprocessed:" <> f.tool) Nothing
           | p <- idx.packages,
             c <- p.components,
             m <- c.modules,
             PreprocessedSource f <- [m.source],
             isLit (relPathText f.path)
           ]
    loose =
      [ lit lf.path "LooseLiterate" (Just (settingsFor ghc looseSettings loose1))
      | OtherModule lf <- idx.other,
        isLit (relPathText lf.path),
        let loose1 = HaskellFile lf.path LooseLiterate
      ]
    lit p m s = LitFile {project = dir, root = dir, path = p, mode = m, settings = s}
    isLit t = any (`Text.isSuffixOf` t) [".lhs", ".lhsig"]

settingsFor :: GhcVersion -> ComponentSettings -> HaskellFile -> ParseSettings
settingsFor = moduleParseSettings

modeName :: Literate -> Text
modeName = Text.pack . show

-- | Our markdown code lines, by file line (blank ones left out).
codeLines :: Text -> Map Int Text
codeLines t = case unlitMarkdown t of
  Nothing -> Map.empty
  Just o ->
    Map.fromList
      [ (n, l)
      | (n, l) <- zip [1 ..] (Text.splitOn "\n" o),
        not (Text.null (Text.strip l))
      ]

-- | The library's code lines, by file line (blank ones left out).
libLines :: Text -> Map Int Text
libLines t =
  Map.fromList
    [ (b.start + j, l')
    | b <- selectedBlocks t,
      (j, l) <- zip [0 ..] b.content,
      let l' = Text.pack l,
      not (Text.null (Text.strip l'))
    ]

-- | Our open test for a fence line, copied from SourceText.isOpen.
ourOpens :: Text -> Bool
ourOpens l = case Text.stripPrefix "```" (Text.stripStart l) of
  Just info -> "haskell" `Text.isInfixOf` info && not ("ignore" `Text.isInfixOf` info)
  Nothing -> False

row :: LitFile -> IO [Text]
row f = do
  bytes <- BS.readFile (f.root </> Text.unpack (relPathText f.path))
  let t = stripBom (Text.Encoding.decodeUtf8Lenient bytes)
      ours = codeLines t
      lib = libLines t
      diffs = differing ours lib
      allBlocks = blocks t
      classes =
        nub (sort (concatMap (classify diffs) allBlocks <> orphan diffs allBlocks))
      tops = length [() | b <- selectedBlocks t, reorderKey b.classes /= 0]
      oursBlocks = length (filter ourOpens (Text.splitOn "\n" t))
      libBlocks = length (selectedBlocks t)
      (_, looseOurs) = unlitLoose t
      (_, looseLib) = libUnlitLooseRaw t
      (pOurs, pLib) = case f.settings of
        Nothing -> (Nothing, Nothing)
        Just s -> (Just (parseResult s id t), libParse s t)
      outcome = maybe "-" (.outcome)
      names = maybe "-" (num . nameCount)
      bad r = maybe "-" (num . length . badNames t) r
      sameTokens = case (pOurs, pLib) of
        (Just a, Just b)
          | a.outcome == "parsed" && b.outcome == "parsed" ->
              if map show a.tokens == map show b.tokens then "yes" else "no"
        _ -> "-"
  pure
    [ Text.pack f.project,
      relPathText f.path,
      f.mode,
      num oursBlocks,
      num libBlocks,
      num (length diffs),
      if null classes then "-" else Text.intercalate "," classes,
      num tops,
      Text.pack (show looseOurs),
      Text.pack (show looseLib),
      outcome pOurs,
      outcome pLib,
      names pOurs,
      bad pOurs,
      names pLib,
      bad pLib,
      sameTokens
    ]
  where
    num :: Int -> Text
    num = Text.pack . show

-- | How the library would read it: the layout for markdown, else the
-- same as ours.
libParse :: ParseSettings -> Text -> Maybe Result
libParse s t = case s.literate of
  MarkdownUnlit -> Just (layout "no-block")
  LooseLiterate -> case libUnlitLooseRaw t of
    (_, UsedMarkdownUnlit) -> Just (layout "no-block")
    (_, UsedGhcUnlit) -> Just (parseResult s {P.literate = BirdOrLatex} id t)
  _ -> Just (parseResult s id t)
  where
    layout none = case libLayout t of
      Just (out, pm) -> parseResult s {P.literate = NotLiterate} (mapSpan pm) out
      Nothing -> Result none [] [] [] []

-- | Line numbers where the two disagree.
differing :: Map Int Text -> Map Int Text -> [Int]
differing a b =
  [ n
  | n <- Map.keys (Map.union a b),
    Map.lookup n a /= Map.lookup n b
  ]

-- | The kind of a library block with a differing line in it.
classify :: [Int] -> Block -> [Text]
classify diffs b
  | not (any inside diffs) = []
  | "~~~" `Text.isPrefixOf` Text.stripStart b.fence = ["tilde"]
  | ourOpens b.fence /= selects b.classes = ["class-word"]
  | selects b.classes && b.indent > 0 = ["dedent"]
  | otherwise = ["pairing"]
  where
    inside n = n >= b.start - 1 && n <= b.start + length b.content

-- | Differing lines outside every library block.
orphan :: [Int] -> [Block] -> [Text]
orphan diffs bs =
  [ "other"
  | any (\n -> not (any (inBlock n) bs)) diffs
  ]
  where
    inBlock n b = n >= b.start - 1 && n <= b.start + length b.content
