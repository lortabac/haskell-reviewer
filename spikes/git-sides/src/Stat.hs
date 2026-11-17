-- | Our own "is it clean?" stat check, over @ls-files -z --stage --debug@.
module Stat
  ( Entry (..),
    Time (..),
    parseDebug,
    Check (..),
    Opts (..),
    checkEntry,
    statTime,
    rawToPath,
    flagIntentToAdd,
    flagSkipWorktree,
    flagValid,
  )
where

import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word32)
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import Numeric (readHex, readOct)
import System.Posix.Files.ByteString
  ( FileStatus,
    fileGroup,
    fileID,
    fileMode,
    fileOwner,
    fileSize,
    isRegularFile,
    isSymbolicLink,
    modificationTimeHiRes,
    statusChangeTimeHiRes,
  )

-- | Seconds and nanoseconds, as the index keeps them (32-bit seconds).
data Time = Time {sec :: Word32, nsec :: Word32}
  deriving (Eq, Ord, Show)

data Entry = Entry
  { mode :: Int,
    oid :: ByteString,
    stage :: Int,
    path :: ByteString,
    ctime :: Time,
    mtime :: Time,
    ino :: Word32,
    uid :: Word32,
    gid :: Word32,
    size :: Word32,
    flags :: Int
  }
  deriving (Show)

-- | In-memory ce_flags bits that --debug prints.
flagValid, flagIntentToAdd, flagSkipWorktree :: Int
flagValid = 0x8000
flagIntentToAdd = 0x20000000
flagSkipWorktree = 0x40000000

-- | Each entry: @<mode> <oid> <stage>\t<path>\0@ then 5 lines. A line
-- we don't know is an error: never "everything clean".
parseDebug :: ByteString -> Either String [Entry]
parseDebug = go []
  where
    go acc s
      | BS.null s = Right (reverse acc)
      | otherwise = do
          let (hdr, rest0) = BS.break (== 0) s
          rest1 <- maybe (Left "no NUL after path") (Right . snd) (BS.uncons rest0)
          let (meta, path) = BS.break (== 9) hdr
          (m, o, st) <- case BC.words meta of
            [m, o, st] -> Right (m, o, st)
            _ -> Left ("bad header " <> show hdr)
          (ls, rest2) <- takeLines 5 rest1
          case map BC.words ls of
            [ ["ctime:", ct],
              ["mtime:", mt],
              ["dev:", _, "ino:", i],
              ["uid:", u, "gid:", g],
              ["size:", sz, "flags:", fl]
              ] -> do
                e <-
                  Entry
                    <$> octal m
                    <*> pure o
                    <*> dec st
                    <*> pure (BS.drop 1 path)
                    <*> time ct
                    <*> time mt
                    <*> dec i
                    <*> dec u
                    <*> dec g
                    <*> dec sz
                    <*> hex fl
                go (e : acc) rest2
            _ -> Left ("bad debug lines " <> show ls)
    takeLines :: Int -> ByteString -> Either String ([ByteString], ByteString)
    takeLines 0 s = Right ([], s)
    takeLines n s = case BS.elemIndex 10 s of
      Nothing -> Left "short debug block"
      Just i -> do
        (more, rest) <- takeLines (n - 1) (BS.drop (i + 1) s)
        pure (BS.take i s : more, rest)
    dec :: (Integral a) => ByteString -> Either String a
    dec s = case BC.readInteger s of
      Just (n, "") -> Right (fromInteger n)
      _ -> Left ("bad number " <> show s)
    octal s = case readOct (BC.unpack s) of
      [(n, "")] -> Right n
      _ -> Left ("bad mode " <> show s)
    hex s = case readHex (BC.unpack s) of
      [(n, "")] -> Right n
      _ -> Left ("bad flags " <> show s)
    time s = case BC.split ':' s of
      [a, b] -> Time <$> dec a <*> dec b
      _ -> Left ("bad time " <> show s)

statTime :: POSIXTime -> Time
statTime t =
  let ns = floor (toRational t * 1_000_000_000) :: Integer
   in Time {sec = fromInteger (ns `div` 1_000_000_000), nsec = fromInteger (ns `mod` 1_000_000_000)}

data Opts = Opts
  { indexMtime :: Time,
    -- | core.filemode: the exec bit counts.
    trustFileMode :: Bool,
    -- | core.symlinks.
    symlinks :: Bool,
    -- | The empty blob's id: a recorded size of 0 is fine for it.
    emptyBlob :: ByteString
  }

-- | What the stat check says about one stage-0 entry.
data Check
  = -- | Stat matches and not racy: clean, read the index blob.
    StatClean
  | -- | Hash it to know.
    Candidate String
  | -- | Deleted, type or mode changed: dirty without hashing.
    Gone String
  | -- | A gitlink: not entered.
    Gitlink
  deriving (Eq, Show)

checkEntry :: Opts -> Entry -> Maybe FileStatus -> Check
checkEntry opts e mst
  | e.mode == 0o160000 = Gitlink
  | otherwise = case mst of
      Nothing -> Gone "deleted"
      Just st
        | not (typeOk st) -> Gone "type changed"
        -- A mode change is a change, whatever the content.
        | opts.trustFileMode && e.mode /= 0o120000 && execBit st /= (e.mode == 0o100755) ->
            Gone "mode changed"
        | e.size == 0 && e.oid /= opts.emptyBlob -> Candidate "size 0 (smudged)"
        | fromIntegral (fileSize st) /= e.size -> Candidate "size"
        | statTime (modificationTimeHiRes st) /= e.mtime -> Candidate "mtime"
        | statTime (statusChangeTimeHiRes st) /= e.ctime -> Candidate "ctime"
        | fromIntegral (fileID st) /= e.ino -> Candidate "ino"
        | fromIntegral (fileOwner st) /= e.uid || fromIntegral (fileGroup st) /= e.gid ->
            Candidate "owner"
        -- Seconds only: never laxer than git with or without USE_NSEC.
        | e.mtime.sec >= opts.indexMtime.sec -> Candidate "racy"
        | otherwise -> StatClean
  where
    typeOk st = case e.mode of
      0o120000 -> isSymbolicLink st || (not opts.symlinks && isRegularFile st)
      _ -> isRegularFile st
    execBit st = fileMode st .&. 0o100 /= 0

-- | A raw path as a FilePath, with the file system encoding (round trips
-- for non-UTF-8 names).
rawToPath :: ByteString -> IO FilePath
rawToPath b = do
  enc <- getFileSystemEncoding
  BS.useAsCStringLen b (GHC.peekCStringLen enc)
