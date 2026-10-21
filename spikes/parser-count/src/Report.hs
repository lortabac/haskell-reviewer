{-# LANGUAGE OverloadedStrings #-}
-- | Counts, the text table and the JSON file.
module Report
  ( ProjectRun (..)
  , printReport
  , writeJson
  , readTriage
  ) where

import Data.Aeson (Value, object, toJSON, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as BL
import Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Ord (Down (..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Text.Printf (printf)
import Components (settingsKey)
import Types

data ProjectRun = ProjectRun
  { prName :: String
  , prInfo :: ProjectInfo
  , prPkgs :: [Package]
  , prPkgErrors :: [(FilePath, String)]
  , prCompMods :: [(String, String, String, ModSource)]  -- ^ pkg, comp, module, source
  , prResults :: [(ModResult, Settings, Bool)]           -- ^ result, settings, other file?
  , prBoots :: Int
  , prSettingsErrors :: [(String, String)]
  , prWallNs :: Integer
  , prCaps :: Int
  , prMaxMem :: Integer
  , prAllocs :: Integer
  }

-- | Triage rules, one per line: @tag | project | rule@. Project @*@
-- matches any. Rule: "file:prefix", "code:GHC-…", "msg:prefix",
-- "phase:…". First match wins.
readTriage :: FilePath -> IO [(String, String, String)]
readTriage f = do
  ls <- lines <$> readFile f
  pure [ (trim tag, trim proj, trim rule)
       | l <- ls, not (null (trim l)), not ("#" `isPrefixOf` trim l)
       , [tag, proj, rule] <- [splitBar l] ]
  where
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    splitBar s = case break (== '|') s of
      (a, '|' : b) -> a : splitBar b
      (a, _) -> [a]

triageOf :: [(String, String, String)] -> String -> FilePath -> Failure -> String
triageOf rules proj file f = case [t | (t, p, r) <- rules, p == "*" || p == proj, matches r] of
  (t : _) -> t
  [] -> "untriaged"
  where
    matches r
      | Just p <- strip "file:" r = p `isPrefixOf` file
      | Just p <- strip "code:" r = fCode f == p
      | Just p <- strip "msg:" r = p `isPrefixOf` fMsg f
      | Just p <- strip "phase:" r = fPhase f == p
      | otherwise = False
    strip p s = if p `isPrefixOf` s then Just (drop (length p) s) else Nothing

-- | Failure with the phase fixed: lexer when the text doesn't lex.
phased :: ModResult -> Maybe Failure
phased mr = case mrFailure mr of
  Just f | fPhase f == "parser", not (mrLexes mr) -> Just f { fPhase = "lexer" }
  other -> other

-- | One result per file: the first failing pair, else the first one.
perFile :: [(ModResult, Settings, Bool)] -> [(ModResult, Settings)]
perFile rs = M.elems (M.fromListWith pick [(mrFile r, (r, s)) | (r, s, _) <- rs])
  where
    pick new old
      | isJust (mrFailure (fst old)) = old
      | isJust (mrFailure (fst new)) = new
      | otherwise = old

data Counts = Counts
  { cListed, cSource, cGenerated, cAutogen, cMissing :: Int
  , cFiles, cPairs, cOther, cCppFiles :: Int
  , cRejected, cRejectedLexes, cOtherRejected :: Int
  , cCppA, cCppB, cCppC :: Int
  , cLines :: Int
  }

counts :: ProjectRun -> Counts
counts r = Counts
  { cListed = length (prCompMods r)
  , cSource = length [() | (_, _, _, Source _) <- prCompMods r]
  , cGenerated = length [() | (_, _, _, Generated _) <- prCompMods r]
  , cAutogen = length [() | (_, _, _, Autogen) <- prCompMods r]
  , cMissing = length [() | (_, _, _, Missing) <- prCompMods r]
  , cFiles = length comp
  , cPairs = length [() | (_, _, False) <- prResults r]
  , cOther = length other
  , cCppFiles = length cpp
  , cRejected = length [() | (m, _) <- comp, isJust (mrFailure m)]
  , cRejectedLexes = length [() | (m, _) <- comp, isJust (mrFailure m), mrLexes m]
  , cOtherRejected = length [() | (m, _) <- other, isJust (mrFailure m)]
  , cCppA = length [() | (_, o) <- cpp, isNothing (coRaw o)]
  , cCppB = length [() | (_, o) <- cpp, isNothing (coStrip o)]
  , cCppC = length [() | (_, o) <- cpp, isNothing (coBasic o)]
  , cLines = sum [mrLines m | (m, _, _) <- prResults r]
  }
  where
    comp = perFile [x | x@(_, _, False) <- prResults r]
    other = perFile [x | x@(_, _, True) <- prResults r]
    cpp = cppFiles comp

cppFiles :: [(ModResult, Settings)] -> [(ModResult, CppOutcome)]
cppFiles xs = [(m, o) | (m, _) <- xs, Just (_, o) <- [mrCpp m]]

-- | Buckets: (phase, code, message) → failing files.
buckets :: [(ModResult, Settings)] -> [((String, String, String), [(ModResult, Failure)])]
buckets xs = sortOn (Down . length . snd) . M.toList $
  M.fromListWith (flip (++))
    [ ((fPhase f, fCode f, fMsg f), [(m, f)]) | (m, _) <- xs, Just f <- [phased m] ]

-- | How many files carry each tag (pgmF, pgmP, pgmL).
tagCounts :: ProjectRun -> M.Map String Int
tagCounts r = M.fromListWith (+) [ (t, 1) | (m, _) <- perFile (prResults r), t <- mrTags m ]

pct :: Int -> Int -> Double
pct _ 0 = 0
pct a b = 100 * fromIntegral a / fromIntegral b

-- | Per-module time (read + header + parse), in ms.
modTimes :: ProjectRun -> [(Double, ModResult)]
modTimes r = sortOn fst
  [ (fromIntegral (mrReadNs m + mrHeaderNs m + mrParseNs m) / 1e6, m) | (m, _, _) <- prResults r ]

quantile :: Double -> [Double] -> Double
quantile _ [] = 0
quantile q xs = xs !! min (length xs - 1) (floor (q * fromIntegral (length xs)))

printReport :: ProjectRun -> IO ()
printReport r = do
  let c = counts r
      comp = perFile [x | x@(_, _, False) <- prResults r]
      cpp = cppFiles comp
      ts = map fst (modTimes r)
      wall = fromIntegral (prWallNs r) / 1e9 :: Double
  printf "== %s\n" (prName r)
  printf "packages: %d (%d failed), missing package entries: %d %s\n"
    (length (prPkgs r)) (length (prPkgErrors r)) (length (piMissing (prInfo r))) (show (piMissing (prInfo r)))
  printf "components: %d\n" (sum (map (length . pkgComps) (prPkgs r)))
  printf "listed modules: %d (source %d, generated %d, autogen %d, missing %d)\n"
    (cListed c) (cSource c) (cGenerated c) (cAutogen c) (cMissing c)
  printf "unique files: %d, unique (file, settings): %d, other files: %d, hs-boot: %d\n"
    (cFiles c) (cPairs c) (cOther c) (prBoots r)
  printf "CPP files: %d (%.1f%%)\n" (cCppFiles c) (pct (cCppFiles c) (cFiles c))
  printf "  outcomes ok: raw %d, strip %d, basic %d (%.1f%%)\n"
    (cCppA c) (cCppB c) (cCppC c) (pct (cCppC c) (cCppFiles c))
  let feat p = length [() | (_, o) <- cpp, p (coFeatures o)]
  printf "  features: include %d, fn-define %d, macro-in-code %d, MIN_VERSION %d, ghc %d, os/arch %d, no directives %d\n"
    (feat ((> 0) . cfIncludes)) (feat ((> 0) . cfFnDefines)) (feat ((> 0) . cfMacroInCode))
    (feat ((> 0) . cfMinVersion)) (feat ((> 0) . cfGhcMacros)) (feat ((> 0) . cfOsArch))
    (feat ((== 0) . cfDirectives))
  printf "rejected: %d (%.2f%%), of which lex OK: %d; other files rejected: %d/%d\n"
    (cRejected c) (pct (cRejected c) (cFiles c)) (cRejectedLexes c) (cOtherRejected c) (cOther c)
  mapM_ (\((ph, code, msg), fs) -> printf "  %4d  %-7s %-10s %s\n" (length fs) ph code (take 90 msg))
        (take 15 (buckets comp))
  printf "time: wall %.2fs on %d caps, %d lines, %.0f lines/s, %.0f lines/s/core\n"
    wall (prCaps r) (cLines c) (fromIntegral (cLines c) / wall)
    (fromIntegral (cLines c) / wall / fromIntegral (prCaps r))
  printf "  per module ms: p50 %.2f p95 %.2f max %.2f\n" (quantile 0.5 ts) (quantile 0.95 ts) (maximum (0 : ts))
  printf "  .cabal parse: total %.3fs, max %.3fs\n"
    (sum (map pkgParseSecs (prPkgs r))) (maximum (0 : map pkgParseSecs (prPkgs r)))
  printf "  memory: max in use %.0f MB, allocated %.1f GB\n"
    (fromIntegral (prMaxMem r) / 1e6 :: Double) (fromIntegral (prAllocs r) / 1e9 :: Double)
  printf "tags: %s\n" (show (M.toList (tagCounts r)))
  mapM_ (\(e, m) -> printf "  settings error: %s: %s\n" e m) (prSettingsErrors r)
  mapM_ (\(f, e) -> printf "  package error: %s: %s\n" f (take 200 e)) (prPkgErrors r)

writeJson :: FilePath -> [(String, String, String)] -> ProjectRun -> IO ()
writeJson path rules r = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (A.encode (toJson rules r))

toJson :: [(String, String, String)] -> ProjectRun -> Value
toJson rules r = object
  [ "project" .= prName r
  , "counts" .= object
      [ "packages" .= length (prPkgs r)
      , "packageErrors" .= [object ["cabal" .= f, "error" .= take 300 e] | (f, e) <- prPkgErrors r]
      , "missingPackageEntries" .= piMissing (prInfo r)
      , "components" .= sum (map (length . pkgComps) (prPkgs r))
      , "listedModules" .= cListed c, "source" .= cSource c, "generated" .= cGenerated c
      , "autogen" .= cAutogen c, "missing" .= cMissing c
      , "uniqueFiles" .= cFiles c, "uniquePairs" .= cPairs c, "otherFiles" .= cOther c
      , "hsBoot" .= prBoots r, "lines" .= cLines c
      , "cppFiles" .= cCppFiles c, "cppPercent" .= pct (cCppFiles c) (cFiles c)
      , "rejected" .= cRejected c, "rejectedButLexes" .= cRejectedLexes c
      , "otherRejected" .= cOtherRejected c
      ]
  , "tags" .= tagCounts r
  , "projectNotes" .= piNotes (prInfo r)
  , "settingsErrors" .= [object ["settings" .= s, "error" .= e] | (s, e) <- prSettingsErrors r]
  , "missingModules" .= [ pn ++ ":" ++ cn ++ ":" ++ m | (pn, cn, m, Missing) <- prCompMods r ]
  , "generatedModules" .= [ f | (_, _, _, Generated f) <- prCompMods r ]
  , "buckets" .= [ object [ "phase" .= ph, "code" .= code, "message" .= msg, "count" .= length fs
                          , "examples" .= [ mrFile m ++ ":" ++ show (fLine f) | (m, f) <- take 3 fs ] ]
                 | ((ph, code, msg), fs) <- buckets comp ]
  , "rejectedFiles" .= [ failJson s m f | (m, s) <- comp, Just f <- [phased m] ]
  , "otherFiles" .= object
      [ "count" .= cOther c
      -- the full list only when short
      , "files" .= (if cOther c <= 100 then map (mrFile . fst) other else [])
      , "rejected" .= [ failJson s m f | (m, s) <- other, Just f <- [phased m] ] ]
  , "cpp" .= object
      [ "files" .= cCppFiles c
      , "rawOk" .= cCppA c, "stripOk" .= cCppB c, "basicOk" .= cCppC c
      , "bySource" .= M.fromListWith (+) [ (show src, 1 :: Int) | (m, _) <- comp, Just (src, _) <- [mrCpp m] ]
      , "modules" .= [ cppJson m src o | (m, _) <- comp, Just (src, o) <- [mrCpp m] ] ]
  , "timing" .= object
      [ "wallSeconds" .= (fromIntegral (prWallNs r) / 1e9 :: Double)
      , "capabilities" .= prCaps r
      , "linesPerSecond" .= (fromIntegral (cLines c) / (fromIntegral (prWallNs r) / 1e9) :: Double)
      , "moduleMsP50" .= quantile 0.5 ts, "moduleMsP95" .= quantile 0.95 ts
      , "moduleMsMax" .= maximum (0 : ts)
      , "slowest" .= [ object ["file" .= mrFile m, "ms" .= t, "lines" .= mrLines m]
                     | (t, m) <- take 10 (reverse (modTimes r)) ]
      , "readSecondsSum" .= secs mrReadNs, "headerSecondsSum" .= secs mrHeaderNs
      , "parseSecondsSum" .= secs mrParseNs
      , "cabalParseSecondsTotal" .= sum (map pkgParseSecs (prPkgs r))
      , "cabalParseSecondsMax" .= maximum (0 : map pkgParseSecs (prPkgs r))
      , "maxMemInUseBytes" .= prMaxMem r
      , "allocatedBytes" .= prAllocs r ]
  ]
  where
    c = counts r
    comp = perFile [x | x@(_, _, False) <- prResults r]
    other = perFile [x | x@(_, _, True) <- prResults r]
    ts = map fst (modTimes r)
    secs g = fromIntegral (sum [g m | (m, _, _) <- prResults r]) / 1e9 :: Double
    failJson s m f = object
      [ "file" .= mrFile m, "phase" .= fPhase f, "code" .= fCode f, "message" .= fMsg f
      , "line" .= fLine f, "lexes" .= mrLexes m, "cpp" .= isJust (mrCpp m)
      , "tags" .= mrTags m, "settings" .= settingsKey s, "triage" .= triageOf rules (prName r) (mrFile m) f ]
    cppJson m src o = object $
      [ "file" .= mrFile m, "source" .= show src
      , "raw" .= ok (coRaw o), "strip" .= ok (coStrip o), "basic" .= ok (coBasic o) ]
      ++ [ "basicError" .= (show (fLine f) ++ ": " ++ fMsg f) | Just f <- [coBasic o] ]
      ++ mapMaybe feat
        [ ("include", cfIncludes), ("fnDefine", cfFnDefines), ("macroInCode", cfMacroInCode)
        , ("minVersion", cfMinVersion), ("ghcMacros", cfGhcMacros), ("osArch", cfOsArch)
        , ("directives", cfDirectives) ]
      ++ [ "notes" .= cfUnsupported (coFeatures o) | not (null (cfUnsupported (coFeatures o))) ]
      where
        feat (k, g) = let v = g (coFeatures o) in if v > 0 then Just (K.fromString k .= v) else Nothing
    ok = toJSON . isNothing
