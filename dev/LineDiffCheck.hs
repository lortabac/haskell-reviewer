{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Dev only: the line diff against git on a repo's history. Clones the
-- repo into a temp dir, and for the last N first-parent commits (200 by
-- default) diffs both blobs of each changed text file, in both modes,
-- with 'lineDiff' and with git. Prints the share that is identical, the
-- pairs that are not, and our time on the biggest files. The repo is
-- only read (@git clone --no-local@).
--
-- > runghc dev/LineDiffCheck.hs [-n N] [REPO]
--
-- REPO is any local repo (default: the current dir). Run with
-- @make linediff-check@.
module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (foldM, forM_, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import GHC.Clock (getMonotonicTimeNSec)
import HaskellReviewer.Core.LineDiff
  ( GitRule,
    LineDiff (..),
    Stats (..),
    Whitespace (..),
    lineDiffStatsAs,
  )
import HaskellReviewer.Core.Sides (Pair (..))
import LineDiffSupport (gitRule, gitRunsMany)
import System.Directory (canonicalizePath)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.IO (IOMode (..), withBinaryFile)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
  ( CreateProcess (..),
    StdStream (..),
    proc,
    readCreateProcessWithExitCode,
    waitForProcess,
    withCreateProcess,
  )
import Text.Printf (printf)
import Text.Read (readMaybe)

-- | A commit id.
newtype Rev = Rev String

-- | A blob id.
newtype BlobId = BlobId String
  deriving (Eq, Ord)

-- | git's id for a missing file (added or deleted).
isMissing :: BlobId -> Bool
isMissing (BlobId b) = all (== '0') b

-- | A changed file: where it changed and the two blobs.
data Change = Change
  { path :: String,
    oldRev :: Rev,
    newRev :: Rev,
    oldBlob :: BlobId,
    newBlob :: BlobId
  }

-- | The last commits: how many, how many have no parent (they are
-- skipped), and the changes.
data History = History
  { commits :: Int,
    roots :: Int,
    changes :: [Change]
  }

-- | Blobs larger than this are skipped.
maxBytes :: Int
maxBytes = 1024 * 1024

main :: IO ()
main = do
  (n, repo) <-
    getArgs >>= \case
      ["-n", k, r] | Just i <- readMaybe k -> pure (i, r)
      ["-n", k] | Just i <- readMaybe k -> pure (i, ".")
      [r] -> pure (200 :: Int, r)
      [] -> pure (200, ".")
      _ -> putStrLn "usage: LineDiffCheck.hs [-n N] [REPO]" >> exitFailure
  absRepo <- canonicalizePath repo
  withSystemTempDirectory "haskell-reviewer-linediff" $ \tmp -> do
    let clone = tmp </> "clone"
    _ <- git tmp ["clone", "--quiet", "--no-local", absRepo, clone]
    hist <- historyChanges clone n
    rule <- gitRule
    -- 20 changes at a time, so only a few blobs are in memory.
    tally <- foldM (step rule tmp clone) emptyTally (chunksOf 20 hist.changes)
    printf
      "%d commits (%d without a parent, skipped), %d changes, %d text pairs\n"
      hist.commits
      hist.roots
      (length hist.changes)
      tally.pairs
    forM_ [KeepWhitespace, IgnoreWhitespace] $ \ws -> do
      let bad = [c | (w, c) <- tally.bad, w == ws]
          same = tally.pairs - length bad
      printf
        "%s: %d of %d identical (%.2f%%)\n"
        (modeName ws)
        same
        tally.pairs
        (100 * fromIntegral same / fromIntegral (max 1 tally.pairs) :: Double)
      forM_ bad $ \c ->
        printf
          "  MISMATCH (%s) %s %s..%s\n"
          (modeName ws)
          c.path
          (short c.oldRev)
          (short c.newRev)
    timings rule tally.biggest
    unless (null tally.bad) exitFailure
  where
    short (Rev r) = take 10 r

-- | What we know so far.
data Tally = Tally
  { pairs :: Int,
    bad :: [(Whitespace, Change)],
    -- | The 10 biggest pairs, for the timings.
    biggest :: [(Change, Pair Text)]
  }

emptyTally :: Tally
emptyTally = Tally {pairs = 0, bad = [], biggest = []}

-- | Diff a few changes with ours and with git, in both modes.
step :: GitRule -> FilePath -> FilePath -> Tally -> [Change] -> IO Tally
step rule tmp clone tally cs = do
  blobs <- readBlobs tmp clone (nub (concat [[c.oldBlob, c.newBlob] | c <- cs]))
  let -- Missing files (added or deleted) are empty.
      side b = if isMissing b then Just "" else Map.lookup b blobs >>= textOf
      ps =
        [ (c, Pair {head = h, base = b})
        | c <- cs,
          Just b <- [side c.oldBlob],
          Just h <- [side c.newBlob],
          b /= h,
          usable b,
          usable h
        ]
  bad <- concat <$> mapM (compareMode rule ps) [KeepWhitespace, IgnoreWhitespace]
  pure
    Tally
      { pairs = tally.pairs + length ps,
        bad = tally.bad <> bad,
        biggest = take 10 (sortOn (Down . size . snd) (tally.biggest <> ps))
      }

size :: Pair Text -> Int
size p = Text.length p.base + Text.length p.head

-- | The changed files of the last @n@ first-parent commits, each
-- against its first parent. A commit without a parent is skipped.
historyChanges :: FilePath -> Int -> IO History
historyChanges clone n = do
  revs <- lines <$> git clone ["rev-list", "--first-parent", "-n", show n, "HEAD"]
  perRev <- mapM changesOf revs
  pure
    History
      { commits = length revs,
        roots = length (filter isNothing perRev),
        changes = concat (catMaybes perRev)
      }
  where
    changesOf rev = do
      parents <- words <$> git clone ["rev-list", "--parents", "-n", "1", rev]
      case parents of
        _ : parent : _ -> do
          raw <-
            lines
              <$> git clone ["diff-tree", "-r", "--no-renames", "--raw", parent, rev]
          pure (Just (mapMaybe (parseRaw (Rev parent) (Rev rev)) raw))
        _ -> pure Nothing
    -- :100644 100644 <old> <new> M<TAB>path. Files only (no links, no
    -- submodules).
    parseRaw parent rev l = case break (== '\t') l of
      (meta, '\t' : p)
        | [m1, m2, old, new, _] <- words meta,
          all regular [m1, m2] ->
            Just
              Change
                { path = p,
                  oldRev = parent,
                  newRev = rev,
                  oldBlob = BlobId old,
                  newBlob = BlobId new
                }
      _ -> Nothing
    regular m = dropWhile (== ':') m `elem` ["100644", "100755", "000000"]

-- | The text of a blob: UTF-8, no NUL, within the size limit.
textOf :: ByteString -> Maybe Text
textOf b
  | BS.length b > maxBytes || BS.elem 0 b = Nothing
  | otherwise = either (const Nothing) Just (Text.Encoding.decodeUtf8' b)

-- | git can't compare a BOM or a lone final CR the way D-276 does (see
-- Note [Line diff]).
usable :: Text -> Bool
usable t = not (Text.isPrefixOf "\xFEFF" t) && not (Text.isSuffixOf "\r" t)

-- | All blobs with one @git cat-file --batch@, through files so the
-- pipes can't fill up. Fails if git doesn't know a blob.
readBlobs :: FilePath -> FilePath -> [BlobId] -> IO (Map.Map BlobId ByteString)
readBlobs tmp clone ids = do
  let input = tmp </> "ids"
      output = tmp </> "blobs"
  writeFile input (unlines [b | BlobId b <- ids, not (isMissing (BlobId b))])
  withBinaryFile input ReadMode $ \hin -> withBinaryFile output WriteMode $ \hout ->
    withCreateProcess
      (proc "git" ["cat-file", "--batch"])
        { cwd = Just clone,
          std_in = UseHandle hin,
          std_out = UseHandle hout
        }
      $ \_ _ _ p -> do
        code <- waitForProcess p
        when (code /= ExitSuccess) (fail "git cat-file failed")
  either fail (pure . Map.fromList) . parseBatch =<< BS.readFile output
  where
    parseBatch s
      | BS.null s = Right []
      | otherwise = case Char8.break (== '\n') s of
          (hdr, rest)
            | [oid, "missing"] <- Char8.words hdr ->
                Left ("git cat-file: " <> Char8.unpack oid <> " missing")
            | [oid, _, len] <- Char8.words hdr,
              Just (k, _) <- Char8.readInt len ->
                let (body, more) = BS.splitAt k (BS.drop 1 rest)
                 in ((BlobId (Char8.unpack oid), body) :) <$> parseBatch (BS.drop 1 more)
          (hdr, _) -> Left ("git cat-file: unreadable header " <> show hdr)

-- | Compare a mode with git, 20 pairs per git run. Gives the pairs
-- that differ.
compareMode ::
  GitRule -> [(Change, Pair Text)] -> Whitespace -> IO [(Whitespace, Change)]
compareMode rule pairs ws = concat <$> mapM batch (chunksOf 20 pairs)
  where
    batch ps =
      gitRunsMany ws (map snd ps) >>= \case
        Left err -> fail err
        Right gits ->
          pure
            [ (ws, c)
            | ((c, p), g) <- zip ps gits,
              (lineDiffStatsAs rule ws p).diff.runs /= g
            ]

modeName :: Whitespace -> String
modeName = \case
  KeepWhitespace -> "exact"
  IgnoreWhitespace -> "-w"

-- | Our time on the 10 biggest pairs (base and head chars), per mode.
-- The interpreted script runs with GHC's default RTS options.
timings :: GitRule -> [(Change, Pair Text)] -> IO ()
timings rule biggest = do
  putStrLn "our time on the 10 biggest pairs (ms, exact / -w):"
  forM_ biggest $ \(c, p) -> do
    a <- time KeepWhitespace p
    b <- time IgnoreWhitespace p
    printf "  %7.1f %7.1f  %7d chars  %s\n" a b (size p) c.path
  where
    time ws p = do
      _ <- evaluate (Text.length p.base + Text.length p.head)
      t0 <- getMonotonicTimeNSec
      _ <- evaluate (length (lineDiffStatsAs rule ws p).diff.runs)
      t1 <- getMonotonicTimeNSec
      pure (fromIntegral (t1 - t0) / 1e6 :: Double)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf k xs = let (a, b) = splitAt k xs in a : chunksOf k b

-- | Run git in a dir. Fails if git does.
git :: FilePath -> [String] -> IO String
git dir args = do
  (code, out, err) <- readCreateProcessWithExitCode (proc "git" args) {cwd = Just dir} ""
  case code of
    ExitSuccess -> pure out
    ExitFailure _ -> fail ("git " <> unwords args <> ": " <> err)
