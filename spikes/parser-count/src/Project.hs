-- | Rough cabal.project reader. Reads only top-level @packages:@,
-- @optional-packages:@ and @import:@ (local files, recursively).
--
-- Known limits: conditionals (@if@ blocks) are skipped, flags are not
-- read, URL packages and remote imports are skipped. Each skip is noted.
module Project (readProject) where

import Control.Monad (filterM, forM)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, nub, sort)
import System.Directory
import System.FilePath
import Types

readProject :: FilePath -> IO ProjectInfo
readProject root = do
  let proj = root </> "cabal.project"
  hasProj <- doesFileExist proj
  if not hasProj
    then do
      fs <- cabalFilesIn root
      pure (ProjectInfo fs [] ["no cabal.project: *.cabal in ROOT"])
    else do
      (entries, notes) <- readProjFile root [] proj
      results <- forM entries $ \(optional, e) -> do
        r <- resolveEntry root e
        pure $ case r of
          [] | optional -> ([], [], ["optional package not found: " ++ e])
             | otherwise -> ([], [e], [])
          fs -> (fs, [], [])
      let (fss, miss, ns) = unzip3 results
      pure ProjectInfo
        { piCabalFiles = nub (concat fss)
        , piMissing = concat miss
        , piNotes = notes ++ concat ns
        }

-- | @*.cabal@ files directly in a dir, relative to root.
cabalFilesIn :: FilePath -> IO [FilePath]
cabalFilesIn dir = do
  ok <- doesDirectoryExist dir
  if not ok then pure [] else do
    xs <- listDirectory dir
    pure (sort [x | x <- xs, ".cabal" `isSuffixOf` x, x /= ".cabal"])

-- | Read one project file. Returns (optional?, entry) pairs and notes.
readProjFile :: FilePath -> [FilePath] -> FilePath -> IO ([(Bool, String)], [String])
readProjFile root seen file
  | file `elem` seen = pure ([], ["import loop: " ++ file])
  | otherwise = do
      txt <- readFile file
      let fields = topFields (lines txt)
          notes0 = [ "skipped conditional block in " ++ makeRelative root file
                   | l <- lines txt, "if " `isPrefixOf` l || "if(" `isPrefixOf` l ]
      rs <- forM fields $ \(name, val) -> case name of
        "packages" -> pure (map ((,) False) (splitEntries val), [])
        "optional-packages" -> pure (map ((,) True) (splitEntries val), [])
        "import"
          | "http" `isPrefixOf` trim val -> pure ([], ["remote import skipped: " ++ trim val])
          | otherwise -> do
              let imp = takeDirectory file </> trim val
              ok <- doesFileExist imp
              if ok then readProjFile root (file : seen) imp
                    else pure ([], ["import not found: " ++ trim val])
        _ -> pure ([], [])
      let (es, ns) = unzip rs
      pure (concat es, notes0 ++ concat ns)

-- | Top-level fields (name at column 0, with continuation lines).
-- Lines inside stanzas (@package foo@, @if …@, …) are skipped.
topFields :: [String] -> [(String, String)]
topFields = go . map stripComment
  where
    go [] = []
    go (l : ls)
      | null (trim l) = go ls
      | startsSpace l = go ls          -- inside a stanza we skipped
      | (name, ':' : rest) <- break (== ':') l
      , all (\c -> c == '-' || c `elem` ['a' .. 'z'] || c `elem` ['A'..'Z']) name =
          let (cont, more) = span (\x -> null (trim x) || startsSpace x) ls
          in (name, unwords (rest : cont)) : go more
      | otherwise = go ls                 -- stanza header
    stripComment l = case breakOn "--" l of
      (a, Just _) | null (trim a) || isSpace (last a) -> a
      _ -> l

breakOn :: String -> String -> (String, Maybe String)
breakOn pat s = case s of
  [] -> ([], Nothing)
  _ | pat `isPrefixOf` s -> ([], Just s)
  (c : cs) -> let (a, b) = breakOn pat cs in (c : a, b)

startsSpace :: String -> Bool
startsSpace (c : _) = isSpace c
startsSpace [] = False

splitEntries :: String -> [String]
splitEntries = filter (not . null) . words . map (\c -> if c == ',' then ' ' else c)

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | One packages: entry → .cabal files (relative to root).
resolveEntry :: FilePath -> String -> IO [FilePath]
resolveEntry root e
  | "http://" `isPrefixOf` e || "https://" `isPrefixOf` e = pure []
  | any (`elem` "*?{[") e = do
      ms <- glob root (normalise e)
      fmap concat (mapM (resolveEntry root) ms)
  | ".cabal" `isSuffixOf` e = do
      ok <- doesFileExist (root </> e)
      pure [normalise e | ok]
  | otherwise = do
      fs <- cabalFilesIn (root </> e)
      pure [normalise (e </> f) | f <- fs]

-- | Small glob: @*@, @?@ and @{a,b}@ in path segments.
glob :: FilePath -> String -> IO [FilePath]
glob root pat = fmap concat (mapM go (expandBraces pat))
  where
    go p = walk "" (splitDirectories p)
    walk acc [] = pure [acc]
    walk acc (s : ss)
      | any (`elem` "*?") s = do
          let dir = root </> acc
          ok <- doesDirectoryExist dir
          if not ok then pure [] else do
            xs <- sort <$> listDirectory dir
            let ms = [x | x <- xs, matchSeg s x, not ("." `isPrefixOf` x)]
            ms' <- if null ss then pure ms
                   else filterM (\x -> doesDirectoryExist (dir </> x)) ms
            fmap concat (mapM (\x -> walk (acc </> x) ss) ms')
      | otherwise = walk (if null acc then s else acc </> s) ss

matchSeg :: String -> String -> Bool
matchSeg [] [] = True
matchSeg ('*' : ps) xs = any (matchSeg ps) [drop n xs | n <- [0 .. length xs]]
matchSeg ('?' : ps) (_ : xs) = matchSeg ps xs
matchSeg (p : ps) (x : xs) = p == x && matchSeg ps xs
matchSeg _ _ = False

expandBraces :: String -> [String]
expandBraces s = case break (== '{') s of
  (pre, '{' : rest) -> case break (== '}') rest of
    (alts, '}' : post) -> [pre ++ a ++ p | a <- splitComma alts, p <- expandBraces post]
    _ -> [s]
  _ -> [s]
  where
    splitComma xs = case break (== ',') xs of
      (a, ',' : b) -> a : splitComma b
      (a, _) -> [a]
