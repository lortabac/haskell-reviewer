-- | parser-count: run ghc-lib-parser over every module of some projects.
-- Throwaway M0 spike (T06, D-83). Not part of the main build.
-- Results: results/summary.md. More on running it: README.md.
--
-- Usage: parser-count [--jobs N] [--json DIR] [--triage FILE] PROJECT_DIR...
--        parser-count --cpp-dump FILE [NAME=VALUE...]
--
-- How to run (from the repo root):
--
-- > bench/fetch-projects.sh                  # small, typical, large
-- > bench/fetch-projects.sh --role extra     # the extras
-- > cd spikes/parser-count                   # own cabal.project
-- > cabal build
-- > for p in optparse-applicative pandoc wire-server clash-compiler servant cabal; do
-- >   $(cabal list-bin parser-count) --triage triage.txt --json results ../../bench/projects/$p
-- > done
-- > ./cpp-check.sh ../../bench/projects/pandoc results/pandoc.json
--
-- Timings: add @+RTS -N1 -RTS@ or @+RTS -N8 -A64m -RTS@.
module Main (main) where

import Control.Concurrent
import Control.Exception (SomeException, evaluate, try)
import Control.Monad
import Data.IORef
import Data.List (isSuffixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.IO
import System.Process (readProcess)
import Components
import Cpp (basicCpp)
import Parse
import Project
import Report
import Types

data Opts = Opts
  { oJobs :: Maybe Int
  , oJson :: Maybe FilePath
  , oTriage :: Maybe FilePath
  , oDirs :: [FilePath]
  }

parseArgs :: [String] -> Either String Opts
parseArgs = go (Opts Nothing Nothing Nothing [])
  where
    go o [] = if null (oDirs o) then Left "no PROJECT_DIR" else Right o { oDirs = reverse (oDirs o) }
    go o ("--jobs" : n : r) = go o { oJobs = Just (read n) } r
    go o ("--json" : d : r) = go o { oJson = Just d } r
    go o ("--triage" : f : r) = go o { oTriage = Just f } r
    go _ (('-' : x) : _) = Left ("unknown option -" ++ x)
    go o (d : r) = go o { oDirs = d : oDirs o } r

main :: IO ()
main = do
  args <- getArgs
  case args of
    -- Print the mini CPP output of one file (to compare with cpp).
    ("--cpp-dump" : file : defs) -> do
      txt <- readFile file
      putStr (fst (basicCpp [ (n, drop 1 v) | d <- defs, let (n, v) = break (== '=') d ] txt))
      exitSuccess
    _ -> pure ()
  opts <- either (\e -> hPutStrLn stderr ("parser-count: " ++ e) >> exitWith (ExitFailure 2)) pure
            (parseArgs args)
  forM_ (oJobs opts) setNumCapabilities
  triage <- maybe (pure []) readTriage (oTriage opts)
  env <- newEnv
  forM_ (oDirs opts) $ \dir -> do
    r <- runProject env dir
    printReport r
    forM_ (oJson opts) $ \d -> writeJson (d </> (prName r ++ ".json")) triage r

runProject :: Env -> FilePath -> IO ProjectRun
runProject env root = do
  let name = takeFileName (dropTrailingPathSeparator root)
  pinfo <- readProject root
  loaded <- forM (piCabalFiles pinfo) $ \f -> do
    r <- try (loadPackage root f)
    pure $ case r of
      Left e -> Left (f, show (e :: SomeException))
      Right (Left e) -> Left (f, e)
      Right (Right p) -> Right p
  let pkgs = [p | Right p <- loaded]
      pkgErrs = [e | Left e <- loaded]
      compMods = [ (pkgName p, compName c, compSettings c, m, src)
                 | p <- pkgs, c <- pkgComps p, (m, src) <- compModules c ]
      pairs = S.toList (S.fromList [ (f, s) | (_, _, s, _, Source f) <- compMods ])
      compFiles = S.fromList (map fst pairs)
  tracked <- trackedFiles root
  let isHs f = any (`isSuffixOf` f) [".hs", ".lhs", ".hsig", ".lhsig"]
      others = [f | f <- tracked, isHs f, not (S.member f compFiles)]
      boots = length [f | f <- tracked, any (`isSuffixOf` f) [".hs-boot", ".lhs-boot"]]
      jobs = [(f, s, False) | (f, s) <- pairs] ++ [(f, otherSettings, True) | f <- others]
  -- component flags, once per settings value
  flagMap <- fmap M.fromList $ forM (S.toList (S.fromList [s | (_, s, _) <- jobs])) $ \s -> do
    df <- compFlags env s
    pure (s, df)
  caps <- getNumCapabilities
  allocs0 <- allocated_bytes <$> getRTSStats
  t0 <- getMonotonicTimeNSec
  results <- runPool caps jobs $ \(f, s, isOther) -> do
    r <- try (parseFile env s (flagMap M.! s) (root </> f) f)
    case r of
      Right mr -> mr `seq` pure (mr, s, isOther)
      Left e -> pure (crashResult f (show (e :: SomeException)), s, isOther)
  t1 <- getMonotonicTimeNSec
  stats <- getRTSStats
  pure ProjectRun
    { prName = name
    , prInfo = pinfo
    , prPkgs = pkgs
    , prPkgErrors = pkgErrs
    , prCompMods = [(pn, cn, m, src) | (pn, cn, _, m, src) <- compMods]
    , prResults = results
    , prBoots = boots
    , prSettingsErrors = [(settingsKey s, e) | (s, Left e) <- M.toList flagMap]
    , prWallNs = toInteger (t1 - t0)
    , prCaps = caps
    , prMaxMem = toInteger (max_mem_in_use_bytes stats)
    , prAllocs = toInteger (allocated_bytes stats - allocs0)
    }
  where
    crashResult f e = ModResult f 0 0 (Just (Failure "crash" "-" (take 200 e) 0)) True Nothing [] 0 0 0

-- | Tracked files, from git. Empty if git fails.
trackedFiles :: FilePath -> IO [FilePath]
trackedFiles root = do
  r <- try (readProcess "git" ["-C", root, "ls-files", "-z"] "")
  pure $ case r of
    Left (_ :: SomeException) -> []
    Right out -> filter (not . null) (splitNul out)
  where
    splitNul s = case break (== '\0') s of
      (a, _ : b) -> a : splitNul b
      (a, []) -> [a]

-- | Run jobs on n workers; results in job order.
runPool :: Int -> [a] -> (a -> IO b) -> IO [b]
runPool n xs f = do
  let indexed = zip [0 :: Int ..] xs
  queue <- newIORef indexed
  out <- newIORef M.empty
  dones <- forM [1 .. max 1 n] $ \_ -> do
    done <- newEmptyMVar
    _ <- forkIO $ do
      let loop = do
            next <- atomicModifyIORef' queue $ \q -> case q of
              [] -> ([], Nothing)
              (y : ys) -> (ys, Just y)
            case next of
              Nothing -> pure ()
              Just (i, x) -> do
                r <- f x >>= evaluate
                atomicModifyIORef' out (\m -> (M.insert i r m, ()))
                loop
      loop
      putMVar done ()
    pure done
  mapM_ takeMVar dones
  M.elems <$> readIORef out
