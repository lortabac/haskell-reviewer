-- | Read a .cabal file with Cabal-syntax and list its components, with
-- their parse settings and module files.
module Components (loadPackage, settingsKey) where

import Control.Exception (evaluate)
import qualified Data.ByteString as BS
import Data.List (isPrefixOf, nub)
import Distribution.Compiler
import qualified Distribution.ModuleName as MN
import Distribution.PackageDescription hiding (pkgName)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.PackageDescription.Parsec
import Distribution.Parsec (showPError)
import Distribution.Pretty (prettyShow)
import Distribution.System (buildPlatform)
import Distribution.Types.ComponentRequestedSpec
import Distribution.Types.DependencySatisfaction
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Version (mkVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Language.Haskell.Extension
import System.Directory (doesFileExist)
import System.FilePath
import Types

-- | Load one package. The path is relative to root.
loadPackage :: FilePath -> FilePath -> IO (Either String Package)
loadPackage root rel = do
  bs <- BS.readFile (root </> rel)
  t0 <- getMonotonicTimeNSec
  let (_, r) = runParseResult (parseGenericPackageDescription bs)
  r' <- evaluate (either (Left . show . fmap (showPError rel) . snd) Right r)
  t1 <- getMonotonicTimeNSec
  let secs = fromIntegral (t1 - t0) / 1e9
  case r' of
    Left err -> pure (Left err)
    Right gpd -> case finalize gpd of
      Left missing -> pure (Left ("finalizePD: " ++ show (length missing) ++ " missing deps"))
      Right pd -> do
        let dir = takeDirectory rel
        comps <- mapM (resolveComp root dir) (components pd)
        pure (Right (Package (prettyShow (pkgName' pd)) rel comps secs))
  where
    pkgName' = PD.pkgName . package

finalize :: GenericPackageDescription -> Either [()] PackageDescription
finalize gpd =
  case finalizePD mempty (ComponentRequestedSpec True True) (const Satisfied)
         buildPlatform ghc914 [] gpd of
    Left ms -> Left (map (const ()) ms)
    Right (pd, _) -> Right pd
  where
    ghc914 = unknownCompilerInfo (CompilerId GHC (mkVersion [9, 14, 1])) NoAbiTag

-- | Raw component: name, build info, listed modules, main-is.
data RawComp = RawComp String BuildInfo [(MN.ModuleName, Bool)] (Maybe FilePath)

components :: PackageDescription -> [RawComp]
components pd =
  [ RawComp (libLabel l) (libBuildInfo l)
      ([(m, False) | m <- exposedModules l ++ otherModules (libBuildInfo l)]
        ++ [(m, True) | m <- signatures l]) Nothing
  | l <- maybe [] pure (library pd) ++ subLibraries pd ]
  ++ [ RawComp ("flib:" ++ prettyShow (foreignLibName f)) (foreignLibBuildInfo f)
         [(m, False) | m <- foreignLibModules f] Nothing
     | f <- foreignLibs pd ]
  ++ [ RawComp ("exe:" ++ prettyShow (exeName e)) (buildInfo e)
         [(m, False) | m <- otherModules (buildInfo e)]
         (Just (getSymbolicPath (modulePath e)))
     | e <- executables pd ]
  ++ [ RawComp ("test:" ++ prettyShow (testName t)) (testBuildInfo t)
         ([(m, False) | m <- otherModules (testBuildInfo t)] ++ libMod)
         mainIs
     | t <- testSuites pd
     , let (mainIs, libMod) = case testInterface t of
             TestSuiteExeV10 _ p -> (Just (getSymbolicPath p), [])
             TestSuiteLibV09 _ m -> (Nothing, [(m, False)])
             _ -> (Nothing, []) ]
  ++ [ RawComp ("bench:" ++ prettyShow (benchmarkName b)) (benchmarkBuildInfo b)
         [(m, False) | m <- otherModules (benchmarkBuildInfo b)]
         (case benchmarkInterface b of
            BenchmarkExeV10 _ p -> Just (getSymbolicPath p)
            _ -> Nothing)
     | b <- benchmarks pd ]
  where
    libLabel l = case libName l of
      LMainLibName -> "lib"
      LSubLibName n -> "lib:" ++ prettyShow n

resolveComp :: FilePath -> FilePath -> RawComp -> IO Component
resolveComp root pkgDir (RawComp name bi mods mainIs) = do
  let dirs = case map getSymbolicPath (hsSourceDirs bi) of
        [] -> ["."]
        ds -> ds
      autogen = map MN.toFilePath (autogenModules bi)
  ms <- mapM (\(m, isSig) -> (,) (prettyShow m) <$> findModule root pkgDir dirs autogen isSig m)
               (nubFst mods)
  mainR <- case mainIs of
    Nothing -> pure []
    Just p -> do
      found <- firstExisting [pkgDir </> d </> p | d <- dirs]
      pure [("Main(" ++ p ++ ")", maybe Missing (Source . clean) found)]
  pure (Component name (settingsOf bi) (ms ++ mainR))
  where
    nubFst xs = go [] xs
      where go _ [] = []
            go seen ((m, s) : rest)
              | m `elem` seen = go seen rest
              | otherwise = (m, s) : go (m : seen) rest
    firstExisting [] = pure Nothing
    firstExisting (f : fs) = do
      ok <- doesFileExist (root </> f)
      if ok then pure (Just f) else firstExisting fs

findModule :: FilePath -> FilePath -> [FilePath] -> [FilePath] -> Bool -> MN.ModuleName -> IO ModSource
findModule root pkgDir dirs autogen isSig m = do
  let base = MN.toFilePath m
      srcExts = if isSig then [".hsig", ".lhsig"] else [".hs", ".lhs"]
      ppExts = [".hsc", ".x", ".y", ".ly", ".chs", ".gc", ".cpphs"]
      cands es = [pkgDir </> d </> base <.> e | d <- dirs, e <- es]
  src <- firstOf (cands srcExts)
  case src of
    Just f -> pure (Source (clean f))
    Nothing -> do
      pp <- firstOf (cands ppExts)
      case pp of
        Just f -> pure (Generated (clean f))
        Nothing
          | base `elem` autogen -> pure Autogen
          | any (`isPrefixOf` takeFileName base) ["Paths_", "PackageInfo_"] -> pure Autogen
          | otherwise -> pure Missing
  where
    firstOf [] = pure Nothing
    firstOf (f : fs) = do
      ok <- doesFileExist (root </> f)
      if ok then pure (Just f) else firstOf fs

-- | Normalise and drop "x/.." pairs (main-is: ../unit.hs).
clean :: FilePath -> FilePath
clean = joinPath . go [] . splitDirectories . normalise
  where
    go acc [] = reverse acc
    go (a : acc) ("..": r) | a /= ".." = go acc r
    go acc (x : r) = go (x : acc) r

-- | Parse settings, in Cabal's order. -pgmL takes its value from the
-- next word, so join it.
settingsOf :: BuildInfo -> Settings
settingsOf bi = Settings
  { setLanguage = maybe "Haskell98" prettyShow (defaultLanguage bi)
  , setExts = map extFlag (nub (defaultExtensions bi ++ oldExtensions bi))
  , setGhcOpts = keepGhc (joinPgm (ghcOpts (options bi)))
  , setCppDefs = defs (cppOptions bi) ++ defs (optP (ghcOpts (options bi)))
  }
  where
    ghcOpts (PerCompilerFlavor g _) = g
    extFlag e = case e of
      EnableExtension k -> "-X" ++ prettyShow k
      DisableExtension k -> "-XNo" ++ prettyShow k
      UnknownExtension s -> "-X" ++ s
    keepGhc = filter keep
    joinPgm (o : v : r) | o `elem` ["-pgmL", "-pgmF", "-pgmP"] = (o ++ v) : joinPgm r
    joinPgm (o : r) = o : joinPgm r
    joinPgm [] = []
    keep o = "-X" `isPrefixOf` o || o == "-cpp" || o == "-F"
             || "-pgmP" `isPrefixOf` o || "-pgmF" `isPrefixOf` o || "-pgmL" `isPrefixOf` o
    optP os = [drop 5 o | o <- os, "-optP" `isPrefixOf` o]
              ++ [o | o <- os, "-D" `isPrefixOf` o]
    defs os = [ (n, if null v then "1" else drop 1 v)
              | o <- os, "-D" `isPrefixOf` o
              , let (n, v) = break (== '=') (drop 2 o), not (null n) ]

-- | A short, stable text for a settings value.
settingsKey :: Settings -> String
settingsKey s = unwords (("-X" ++ setLanguage s) : setExts s ++ setGhcOpts s
                         ++ ["-D" ++ n ++ "=" ++ v | (n, v) <- setCppDefs s])
