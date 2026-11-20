-- | Module markers (G10, D-324): every rule by hand, and the
-- cabal-change fixture read from disk and paired.
module HaskellReviewer.Core.MarkersTest (tests) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import HaskellReviewer.Core.Changed (moduleKeys)
import HaskellReviewer.Core.FileList (ListSource (..), fileListCap, mkFileList)
import HaskellReviewer.Core.Markers
  ( Marker (..),
    ModuleChange (..),
    ModuleFacts (..),
    SettingsChange (..),
    moduleFacts,
    moduleMarkers,
    noMarker,
  )
import HaskellReviewer.Core.Modules
  ( ModuleId (..),
    ModuleRole (..),
    buildModuleIndex,
    renderModuleId,
  )
import HaskellReviewer.Core.Parser (ParseSettings (..), defaultParseSettings)
import HaskellReviewer.Core.Project (Platform (..))
import HaskellReviewer.Core.Renames
  ( Change (..),
    ExecChange (..),
    Limit (..),
    ModifiedFile (..),
    Pairing (..),
    RenameStep (..),
    RenamedFile (..),
    defaultSettings,
    pairFiles,
  )
import HaskellReviewer.Core.Root (Root (..))
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Core.Similarity (maxScore)
import HaskellReviewer.Effects.FileSystem (WalkResult (..), canonicalRoot, walkFiles)
import HaskellReviewer.Effects.Source (EntrySource (..))
import HaskellReviewer.Session.Project (readProject)
import IndexSupport (rel)
import RenamesSupport (RepoSides (..), repoSides)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport (fixtureRepo, git, withCanonicalTemp)

tests :: TestTree
tests =
  testGroup
    "Markers"
    [ testCase "added and removed" $
        markers [] [(m, facts "a.hs")] [(n, facts "b.hs")]
          @?= Pair
            { head = Map.fromList [(m, alone ModuleAdded)],
              base = Map.fromList [(n, alone ModuleRemoved)]
            },
      testCase "renamed file" $
        both [Renamed (renamed "a.hs" "b.hs")] (facts "a.hs") (facts "b.hs")
          @?= Marker (Just ModuleRenamed) SettingsSame,
      testCase "same path, modified file" $
        let change = Modified ModifiedFile {path = rel "a.hs", note = Nothing}
         in both [change] (facts "a.hs") (facts "a.hs")
              @?= Marker (Just ModuleModified) SettingsSame,
      testCase "same path, unchanged" $
        both [] (facts "a.hs") (facts "a.hs") @?= noMarker,
      testCase "other paths, not paired: modified" $
        both [Added (rel "b.hs"), Removed (rel "a.hs")] (facts "a.hs") (facts "b.hs")
          @?= Marker (Just ModuleModified) SettingsSame,
      testCase "a file on one side only: modified" $
        both [] (facts "a.hs") noFile
          @?= Marker (Just ModuleModified) SettingsSame,
      testCase "no file on either side: none" $
        both [] noFile noFile @?= noMarker,
      testCase "settings or role changed" $ do
        let other = Just defaultParseSettings {ghcOptions = ["-x"]}
        both [] (facts "a.hs") (factsWith (Just "a.hs") (Just RoleExposed) other)
          @?= Marker Nothing SettingsChanged
        let otherRole =
              factsWith (Just "a.hs") (Just RoleOther) (Just defaultParseSettings)
        both [] (facts "a.hs") otherRole
          @?= Marker Nothing SettingsChanged
        both [] (facts "a.hs") (factsWith (Just "a.hs") (Just RoleExposed) Nothing)
          @?= Marker Nothing SettingsChanged,
      testCase "cabal-change.sh" cabalChange
    ]
  where
    m = LooseModule (rel "a.hs")
    n = LooseModule (rel "b.hs")
    alone c = Marker (Just c) SettingsSame
    markers cs h b =
      moduleMarkers (pairing cs) Pair {head = Map.fromList h, base = Map.fromList b}
    both cs b h =
      let r = markers cs [(m, h)] [(m, b)]
       in if r.head Map.! m == r.base Map.! m
            then r.head Map.! m
            else error "sides differ"
    renamed a b =
      RenamedFile
        { from = rel a,
          to = rel b,
          score = maxScore,
          step = ExactStep,
          exec = SameExec
        }
    pairing cs = Pairing {changes = cs, limit = WithinLimits}

facts :: Text -> ModuleFacts
facts p = factsWith (Just p) (Just RoleExposed) (Just defaultParseSettings)

-- | A module without a file (generated, missing).
noFile :: ModuleFacts
noFile = factsWith Nothing (Just RoleExposed) (Just defaultParseSettings)

factsWith :: Maybe Text -> Maybe ModuleRole -> Maybe ParseSettings -> ModuleFacts
factsWith p r s = ModuleFacts {path = rel <$> p, role = r, settings = s}

-- | Both commits checked out and read from disk; files paired from
-- the commits. See the script for what changed.
cabalChange :: IO ()
cabalChange = withCanonicalTemp $ \tmp -> do
  let repo = tmp </> "r"
  fixtureRepo "cabal-change" [repo]
  git repo ["worktree", "add", "-q", "--detach", tmp </> "base", "HEAD~1"]
  git repo ["worktree", "add", "-q", "--detach", tmp </> "head", "HEAD"]
  b <- factsAt (tmp </> "base")
  h <- factsAt (tmp </> "head")
  sides <- repoSides repo "" "HEAD~1" "HEAD"
  let pairing = pairFiles defaultSettings sides.entries sides.contents
      r = moduleMarkers pairing Pair {head = h, base = b}
      changed side =
        sortOn
          fst
          [(renderModuleId i, mk) | (i, mk) <- Map.toAscList side, mk /= noMarker]
  changed r.head
    @?= [ ("added:lib:C", Marker (Just ModuleAdded) SettingsSame),
          ("ext:lib:X", Marker Nothing SettingsChanged),
          ("ext:lib:Y", Marker Nothing SettingsChanged),
          ("moved:lib:M", Marker (Just ModuleRenamed) SettingsSame),
          ("moved:lib:N", Marker (Just ModuleRenamed) SettingsSame)
        ]
  changed r.base
    @?= [ ("added/src/C.hs", Marker (Just ModuleRemoved) SettingsSame),
          ("ext:lib:X", Marker Nothing SettingsChanged),
          ("ext:lib:Y", Marker Nothing SettingsChanged),
          ("moved:lib:M", Marker (Just ModuleRenamed) SettingsSame),
          ("moved:lib:N", Marker (Just ModuleRenamed) SettingsSame)
        ]
  where
    factsAt :: FilePath -> IO (Map ModuleId ModuleFacts)
    factsAt dir = do
      canon <- canonicalRoot (Root dir)
      w <- walkFiles canon (fileListCap + 1)
      let fl = mkFileList FromDisk w.skippedNames w.entries
      let platform = Platform {os = "linux", arch = "x86_64"}
      project <- readProject (DiskEntries canon) fl platform Nothing
      let idx = buildModuleIndex project fl
      pure (moduleFacts (moduleKeys (const Nothing) idx) idx)
