-- | Throwaway M3/T01 spike. See README.md.
module Main (main) where

import BlobId
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (forConcurrently_, wait, withAsync)
import Control.Concurrent.MVar (readMVar)
import Control.Exception (IOException, evaluate, try)
import Control.Monad (forM, forM_, when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Char (toLower)
import Data.IORef
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Vector qualified as V
import GHC.Clock (getMonotonicTimeNSec)
import HaskellReviewer.Effects.Git (hardenedArgs, hardenedEnv)
import Pool
import Proc
import Stat
import System.Directory (listDirectory, makeAbsolute)
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (splitSearchPath, takeDirectory, (</>))
import System.IO
import System.Posix.Directory (changeWorkingDirectory)
import System.Posix.Files (getFileStatus)
import System.Posix.Files.ByteString (getFdStatus)
import System.Posix.Files.ByteString qualified as PB
import System.Posix.IO.ByteString qualified as PIO
import Text.Printf (printf)

main :: IO ()
main =
  getArgs >>= \case
    ["flags"] -> flags
    "read-blobs" : rest -> readBlobs rest
    "worktree" : rest -> worktree rest
    ["list-side", repo, rev] -> listSide repo rev
    _ -> usage

usage :: IO a
usage = do
  hPutStrLn stderr "usage: git-sides flags"
  hPutStrLn stderr "       git-sides read-blobs --pool N --workers W [--requester MS] [--runs R] [--warmup K] REPO OIDS"
  hPutStrLn stderr "       git-sides worktree [--list] REPO"
  hPutStrLn stderr "       git-sides list-side REPO REV"
  exitWith (ExitFailure 2)

-- * flags

-- | Same lines as @hgit.sh --print-flags@.
flags :: IO ()
flags = do
  env <- getEnvironment
  let dirs = splitSearchPath (fromMaybe "/usr/bin:/bin" (lookup "HGIT_PATH" env))
  forM_ (hardenedArgs []) $ \a -> putStrLn ("arg " <> a)
  forM_ (hardenedEnv env dirs) $ \(k, v) -> putStrLn ("env " <> k <> "=" <> v)

-- * read-blobs

data BlobOpts = BlobOpts
  { pool :: Int,
    workers :: Int,
    requester :: Maybe Int,
    runs :: Int,
    warmup :: Int,
    repo :: FilePath,
    oidsFile :: FilePath
  }

parseBlobOpts :: [String] -> BlobOpts -> IO BlobOpts
parseBlobOpts args o = case args of
  "--pool" : n : r -> parseBlobOpts r o {pool = read n}
  "--workers" : n : r -> parseBlobOpts r o {workers = read n}
  "--requester" : n : r -> parseBlobOpts r o {requester = Just (read n)}
  "--runs" : n : r -> parseBlobOpts r o {runs = read n}
  "--warmup" : n : r -> parseBlobOpts r o {warmup = read n}
  [repo, oids] -> pure o {repo = repo, oidsFile = oids}
  _ -> usage

data RunResult = RunResult
  { wallMs :: Double,
    startMs :: Double,
    waits :: [Double],
    reads :: [Double],
    reqs :: [Double],
    bytes :: Int,
    hwms :: [Int]
  }

readBlobs :: [String] -> IO ()
readBlobs args = do
  o <- parseBlobOpts args BlobOpts {pool = 1, workers = 1, requester = Nothing, runs = 10, warmup = 1, repo = "", oidsFile = ""}
  g <- gitCfg o.repo
  oids <- V.fromList . filter (not . BS.null) . BC.lines <$> BS.readFile o.oidsFile
  forM_ [1 .. o.warmup] $ \_ -> runOnce g o oids
  results <- forM [1 .. o.runs] $ \i -> do
    r <- runOnce g o oids
    hPutStrLn stderr (printf "run %d: %.1f ms" (i :: Int) r.wallMs)
    pure r
  let med f = median (map f results)
      walls = map (.wallMs) results
      req = maybe "-" show o.requester
  -- pool workers requester oids bytes wall_med wall_min wall_max start_med
  -- wait_p50 wait_p95 wait_max read_p50 read_p95 req_n req_p50 req_p95 req_max hwm_kb_max
  printf
    "%d\t%d\t%s\t%d\t%d\t%.1f\t%.1f\t%.1f\t%.2f\t%.3f\t%.3f\t%.3f\t%.3f\t%.3f\t%d\t%.3f\t%.3f\t%.3f\t%d\n"
    o.pool
    o.workers
    req
    (V.length oids)
    (maybe 0 (.bytes) (safeHead results))
    (median walls)
    (minimum walls)
    (maximum walls)
    (med (.startMs))
    (med (pct 50 . (.waits)))
    (med (pct 95 . (.waits)))
    (med (maxOr0 . (.waits)))
    (med (pct 50 . (.reads)))
    (med (pct 95 . (.reads)))
    (sum (map (length . (.reqs)) results) `div` max 1 (length results))
    (med (pct 50 . (.reqs)))
    (med (pct 95 . (.reqs)))
    (med (maxOr0 . (.reqs)))
    (maximum (0 : concatMap (.hwms) results))
  where
    safeHead = \case
      (x : _) -> Just x
      [] -> Nothing
    maxOr0 xs = if null xs then 0 else maximum xs

runOnce :: GitCfg -> BlobOpts -> V.Vector ByteString -> IO RunResult
runOnce g o oids = do
  t0 <- now
  p <- startPool g o.pool
  -- One round trip per handle, so start-up is really done.
  firstOid <- evaluate (V.head oids)
  startAll p (readObject False) firstOid o.pool
  t1 <- now
  next <- newIORef (0 :: Int)
  done <- newIORef False
  waitsRef <- newIORef []
  readsRef <- newIORef []
  bytesRef <- newIORef (0 :: Int)
  let one oid = do
        a <- now
        withCatFile p $ \c -> do
          b <- now
          r <- readObject True c oid
          e <- now
          atomicModifyIORef' waitsRef (\xs -> (ms a b : xs, ()))
          atomicModifyIORef' readsRef (\xs -> (ms b e : xs, ()))
          pure r
      worker = do
        i <- atomicModifyIORef' next (\n -> (n + 1, n))
        when (i < V.length oids) $ do
          r <- one (oids V.! i)
          case r of
            Just blob -> atomicModifyIORef' bytesRef (\n -> (n + BS.length blob.bytes, ()))
            Nothing -> ioError (userError ("missing " <> show (oids V.! i)))
          worker
      requesterLoop period seed acc = do
        stop <- readIORef done
        if stop
          then pure acc
          else do
            threadDelay (period * 1000)
            let seed' = (seed * 6364136223846793005 + 1442695040888963407) :: Word
                k = fromIntegral (seed' `div` 65536) `mod` V.length oids
            a <- now
            _ <- withCatFile p $ \c -> readObject True c (oids V.! k)
            e <- now
            requesterLoop period seed' (ms a e : acc)
  reqs <- case o.requester of
    Nothing -> do
      forConcurrently_ [1 .. o.workers] (const worker)
      pure []
    Just period ->
      withAsync (requesterLoop period 42 []) $ \req -> do
        forConcurrently_ [1 .. o.workers] (const worker)
        writeIORef done True
        wait req
  t2 <- now
  hw <- poolHwm p
  stopPool p
  ws <- readIORef waitsRef
  rs <- readIORef readsRef
  bs <- readIORef bytesRef
  pure RunResult {wallMs = ms t1 t2, startMs = ms t0 t1, waits = ws, reads = rs, reqs = reqs, bytes = bs, hwms = hw}
  where
    startAll p f oid n = forConcurrently_ [1 .. n] $ \_ -> withCatFile p (\c -> f c oid)

poolHwm :: Pool -> IO [Int]
poolHwm p = fmap catMaybes . forM p.members $ \mv -> do
  c <- readMVar mv
  maybe (pure Nothing) vmHwmKb c.pid

now :: IO Double
now = fromIntegral <$> getMonotonicTimeNSec

ms :: Double -> Double -> Double
ms a b = (b - a) / 1e6

median :: [Double] -> Double
median = pct 50

pct :: Double -> [Double] -> Double
pct _ [] = 0
pct p xs =
  let s = sort xs
      n = length s
      i = min (n - 1) (max 0 (ceiling (p / 100 * fromIntegral n) - 1))
   in s !! i

-- * worktree

worktree :: [String] -> IO ()
worktree args = do
  (list, repo0) <- case args of
    ["--list", r] -> pure (True, r)
    [r] -> pure (False, r)
    _ -> usage
  repo <- makeAbsolute repo0
  g <- gitCfg repo
  changeWorkingDirectory repo
  fmtOut <- runGitOk g ["rev-parse", "--show-object-format"] Nothing
  fmt <- maybe (ioError (userError "object format")) pure (parseObjectFormat fmtOut)
  indexPath <- (repo </>) . BC.unpack . BC.strip <$> runGitOk g ["rev-parse", "--git-path", "index"] Nothing
  (_, cfgOut, _) <-
    runGit g ["config", "-z", "--get-regexp", "^core\\.(autocrlf|filemode|symlinks|ignorecase|trustctime|checkstat)$"] Nothing
  let cfg = parseConfigZ cfgOut
      boolCfg k d = case Map.lookup k cfg of
        Nothing -> pure d
        Just v -> maybe (ioError (userError ("bad bool " <> show (k, v)))) pure (gitBool v)
      -- autocrlf: a bool or "input"; both true and input convert on add.
      autocrlf = case Map.lookup "core.autocrlf" cfg of
        Just (Just v) | BC.map toLower v == "input" -> pure True
        _ -> boolCfg "core.autocrlf" False
  fileModeOn <- boolCfg "core.filemode" True
  symlinksOn <- boolCfg "core.symlinks" True
  autocrlfOn <- autocrlf
  -- Stat the index before ls-files reads it: a rewrite in between can
  -- then only make more entries racy, never fewer.
  idxMtime <- indexTime indexPath
  t0 <- now
  dbg <- runGitOk g ["ls-files", "-z", "--stage", "--debug"] Nothing
  t1 <- now
  entries <- either (\e -> ioError (userError ("ls-files --debug: " <> e))) pure (parseDebug dbg)
  _ <- evaluate (length entries)
  t2 <- now
  let opts =
        Opts
          { indexMtime = idxMtime,
            trustFileMode = fileModeOn,
            symlinks = symlinksOn,
            emptyBlob = blobId fmt ""
          }
  dirCache <- newIORef Map.empty
  checked <- forM entries $ \e -> do
    -- As git's has_symlink_leading_path: a symlink (or a non-directory)
    -- in a leading directory means the entry is gone.
    lead <- leadingDirsOk dirCache e.path
    c <-
      if not lead && e.mode /= 0o160000
        then pure (Gone "symlink or file in leading path")
        else do
          st <- try @IOException (PB.getSymbolicLinkStatus e.path)
          pure (checkEntry opts e (either (const Nothing) Just st))
    _ <- evaluate c
    pure (e, c)
  t3 <- now
  others <- filter (not . BS.null) . BS.split 0 <$> runGitOk g ["ls-files", "-z", "--others", "--exclude-standard"] Nothing
  let unmerged = Map.fromList [(e.path, ()) | e <- entries, e.stage > 0]
      classify (e, c)
        | e.stage > 0 = Left ("unmerged", e.path)
        | e.flags .&. flagSkipWorktree /= 0 = Right Nothing
        | e.flags .&. flagIntentToAdd /= 0 = Left ("intent-to-add", e.path)
        | otherwise = case c of
            StatClean -> Right Nothing
            -- Not entered: the side shows the index's commit id.
            Gitlink -> Right Nothing
            Gone why -> Left (why, e.path)
            Candidate why -> Right (Just (e, why))
      cls = map classify checked
      early = [d | Left d <- cls]
      cands = [x | Right (Just x) <- cls]
  -- check-attr, for candidates and untracked only.
  attrs <-
    if null cands && null others
      then pure Map.empty
      else
        parseCheckAttr
          <$> runGitOk'
            g
            ["check-attr", "-z", "--stdin", "text", "eol", "crlf", "filter", "ident", "working-tree-encoding"]
            (BS.concat [p <> "\0" | p <- map ((.path) . fst) cands <> others])
  t4 <- now
  contents <- forM cands $ \(e, why) -> do
    r <- readWorktree e
    case r of
      Just b -> do
        _ <- evaluate (BS.length b)
        pure (e, why, Just b)
      Nothing -> pure (e, why, Nothing)
  t5 <- now
  let verdict (e, _, Nothing) = Just ("changed while reading", e.path)
      verdict (e, why, Just b) =
        let a = Map.findWithDefault Map.empty e.path attrs
            eol = eolFor a autocrlfOn
            ids = map (blobId fmt) (cleanEol eol b)
            filtered = any (\k -> Map.findWithDefault "unspecified" k a `notElem` ["unspecified", "unset"]) ["filter", "ident", "working-tree-encoding"]
         in if e.oid `elem` ids
              then Nothing
              else Just (if filtered then "filtered (" <> why <> ")" else "content (" <> why <> ")", e.path)
      dirty = early <> mapMaybe verdict contents
  _ <- evaluate (length dirty)
  t6 <- now
  -- Cost of both formats on the same bytes.
  s1 <- now
  _ <- evaluate (sum [BS.length (blobId Sha1 b) | (_, _, Just b) <- contents])
  s2 <- now
  _ <- evaluate (sum [BS.length (blobId Sha256 b) | (_, _, Just b) <- contents])
  s3 <- now
  let kv :: String -> String -> IO ()
      kv k v = putStrLn (k <> "\t" <> v)
      msS a b = printf "%.1f" (ms a b) :: String
  kv "entries" (show (length entries))
  kv "unmerged_paths" (show (Map.size unmerged))
  kv "untracked" (show (length others))
  kv "ls_files_debug_ms" (msS t0 t1) -- spawn and read, in our exe
  kv "parse_ms" (msS t1 t2)
  kv "lstat_all_ms" (msS t2 t3)
  kv "candidates" (show (length cands))
  kv "candidate_bytes" (show (sum [BS.length b | (_, _, Just b) <- contents]))
  kv "others_and_check_attr_ms" (msS t3 t4)
  kv "read_candidates_ms" (msS t4 t5)
  kv "hash_and_compare_ms" (msS t5 t6)
  kv "hash_sha1_ms" (msS s1 s2)
  kv "hash_sha256_ms" (msS s2 s3)
  kv "dirty" (show (length dirty))
  kv "filtered" (show (length [() | (w, _) <- dirty, take 8 w == "filtered"]))
  kv "candidate_reasons" (show (Map.toList (Map.fromListWith (+) [(why, 1 :: Int) | (_, why) <- cands])))
  when list $ forM_ dirty $ \(why, p) -> BC.putStrLn ("dirty\t" <> BC.pack why <> "\t" <> p)
  where
    runGitOk' g a input = runGitOk g a (Just input)

-- | Are all leading directories of a path real directories (not
-- symlinks)? Cached per directory.
leadingDirsOk :: IORef (Map ByteString Bool) -> ByteString -> IO Bool
leadingDirsOk cache path = go (init' (BC.split '/' path)) ""
  where
    init' xs = if null xs then [] else init xs
    go [] _ = pure True
    go (d : ds) pre = do
      let dir = if BS.null pre then d else pre <> "/" <> d
      known <- Map.lookup dir <$> readIORef cache
      ok <- case known of
        Just k -> pure k
        Nothing -> do
          st <- try @IOException (PB.getSymbolicLinkStatus dir)
          let k = either (const False) PB.isDirectory st
          modifyIORef' cache (Map.insert dir k)
          pure k
      if ok then go ds dir else pure False

-- | Read an entry's bytes: a symlink's target, or a regular file opened
-- with O_NOFOLLOW and O_NONBLOCK, then checked with fstat (so a file
-- swapped for a FIFO or a symlink after the lstat is not read).
-- Nothing if it changed type under us.
readWorktree :: Entry -> IO (Maybe ByteString)
readWorktree e = do
  st <- PB.getSymbolicLinkStatus e.path
  if PB.isSymbolicLink st
    then Just <$> PB.readSymbolicLink e.path
    else do
      r <- try @IOException (PIO.openFd e.path PIO.ReadOnly PIO.defaultFileFlags {PIO.nofollow = True, PIO.nonBlock = True})
      case r of
        Left _ -> pure Nothing
        Right fd -> do
          fst' <- getFdStatus fd
          if not (PB.isRegularFile fst')
            then PIO.closeFd fd >> pure Nothing
            else do
              h <- PIO.fdToHandle fd
              Just <$> BS.hGetContents h

-- | git's boolean: no value is true; true/yes/on, false/no/off and an
-- empty value, any case; an integer is true unless 0.
gitBool :: Maybe ByteString -> Maybe Bool
gitBool Nothing = Just True
gitBool (Just v0) = case BC.map toLower v0 of
  v
    | v `elem` ["true", "yes", "on"] -> Just True
    | v `elem` ["false", "no", "off", ""] -> Just False
    | Just (n, "") <- BC.readInteger v -> Just (n /= 0)
    | otherwise -> Nothing

-- * list-side

-- | List a commit side with sizes. Proposed fallback for git where
-- @ls-tree --long@ dies on a missing blob (2.44.1, partial clone): plain
-- @ls-tree -r -z@, then @cat-file info@ per blob; a dead cat-file is
-- restarted and that blob is "missing". Never fetches.
listSide :: FilePath -> String -> IO ()
listSide repo0 rev = do
  repo <- makeAbsolute repo0
  g <- gitCfg repo
  (code, _, err) <- runGit g ["ls-tree", "-r", "-z", "--long", "--full-tree", "--end-of-options", rev] Nothing
  case code of
    ExitSuccess -> do
      out <- runGitOk g ["ls-tree", "-r", "-z", "--long", "--full-tree", "--end-of-options", rev] Nothing
      let rows = [r | r <- BS.split 0 out, not (BS.null r)]
          bad = [() | r <- rows, [_, "blob", _, "BAD"] <- [BC.words (BC.takeWhile (/= '\t') r)]]
      putStrLn "method\tls-tree --long"
      putStrLn ("entries\t" <> show (length rows))
      putStrLn ("missing\t" <> show (length bad))
    _ -> do
      putStrLn "method\tfallback (ls-tree --long failed)"
      BC.putStrLn ("ls_tree_long_stderr\t" <> BC.takeWhile (/= '\n') err)
      out <- runGitOk g ["ls-tree", "-r", "-z", "--full-tree", "--end-of-options", rev] Nothing
      let rows = [r | r <- BS.split 0 out, not (BS.null r)]
          blobs = [w !! 2 | r <- rows, let w = BC.words (BC.takeWhile (/= '\t') r), length w == 3, w !! 1 == "blob"]
      restarts <- newIORef (0 :: Int)
      poolRef <- startPool g 1 >>= newIORef
      sizes <- forM blobs $ \oid -> do
        p <- readIORef poolRef
        r <- try @IOException (withCatFile p (\c -> readObject False c oid))
        case r of
          Right (Just _) -> pure (Just ())
          Right Nothing -> pure Nothing
          Left _ -> do
            stopPool p
            startPool g 1 >>= writeIORef poolRef
            modifyIORef' restarts (+ 1)
            pure Nothing
      readIORef poolRef >>= stopPool
      n <- readIORef restarts
      putStrLn ("entries\t" <> show (length rows))
      putStrLn ("blobs\t" <> show (length blobs))
      putStrLn ("missing\t" <> show (length [() | Nothing <- sizes]))
      putStrLn ("restarts\t" <> show n)

-- | The index file's mtime; with a split index, the oldest of it and
-- the shared index files (entries from the shared one are as old).
indexTime :: FilePath -> IO Time
indexTime indexPath = do
  st <- getFileStatus indexPath
  let dir = takeDirectory indexPath
  names <- listDirectory dir
  shared <- forM [dir </> n | n <- names, take 12 n == "sharedindex."] $ \p ->
    statTime . PB.modificationTimeHiRes <$> getFileStatus p
  pure (minimum (statTime (PB.modificationTimeHiRes st) : shared))

-- | @config -z@: @key\nvalue\0@, or @key\0@ for a key with no value
-- (Nothing). The last one wins.
parseConfigZ :: ByteString -> Map ByteString (Maybe ByteString)
parseConfigZ out =
  Map.fromList
    [ (k, if BS.null v then Nothing else Just (BS.drop 1 v))
    | r <- BS.split 0 out,
      not (BS.null r),
      let (k, v) = BC.break (== '\n') r
    ]

-- | @check-attr -z@: path NUL attr NUL value NUL ...
parseCheckAttr :: ByteString -> Map ByteString (Map ByteString ByteString)
parseCheckAttr out = go Map.empty (BS.split 0 out)
  where
    go acc (p : a : v : rest) = go (Map.insertWith Map.union p (Map.singleton a v) acc) rest
    go acc _ = acc
