-- | What we can learn about the machine a benchmark ran on, for the
-- JSON report.
module Bench.Machine
  ( MachineInfo (..),
    getMachineInfo,
  )
where

import Control.Exception (IOException, try)
import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (listToMaybe)
import GHC.Conc (getNumProcessors)
import System.Info qualified as Info
import System.Posix.Resource
  ( Resource (ResourceOpenFiles),
    ResourceLimit (..),
    ResourceLimits (..),
    getResourceLimit,
  )
import System.Process (readProcess)
import Text.Read (readMaybe)

-- | Any field we can't read is 'Nothing', not a failure: a benchmark
-- run still goes ahead without it.
data MachineInfo = MachineInfo
  { os :: String,
    arch :: String,
    -- | @uname -sr@.
    kernel :: Maybe String,
    cpu :: Maybe String,
    cores :: Maybe Int,
    memoryBytes :: Maybe Integer,
    -- | @RLIMIT_NOFILE@ (soft, hard): how many file descriptors a
    -- spawned process may open. Matters here because the product
    -- closes every fd up to this limit at start-up (D-85): a huge
    -- limit (some sandboxes set one) makes every process spawn, and
    -- so every run, slower.
    nofileSoft :: Maybe Integer,
    nofileHard :: Maybe Integer
  }
  deriving (Eq, Show)

-- | Gather it. Each field is read on its own, so one failing doesn't
-- lose the rest.
getMachineInfo :: IO MachineInfo
getMachineInfo = do
  kernelV <- tryRun "uname" ["-sr"]
  cpuV <- cpuModel
  coresV <- tryIO getNumProcessors
  memV <- memBytes
  (softV, hardV) <- nofileLimits
  pure
    MachineInfo
      { os = Info.os,
        arch = Info.arch,
        kernel = kernelV,
        cpu = cpuV,
        cores = coresV,
        memoryBytes = memV,
        nofileSoft = softV,
        nofileHard = hardV
      }

-- * Internal

cpuModel :: IO (Maybe String)
cpuModel
  | Info.os == "darwin" = tryRun "sysctl" ["-n", "machdep.cpu.brand_string"]
  | otherwise = tryIO $ do
      ls <- lines <$> readFile "/proc/cpuinfo"
      case listToMaybe (filter ("model name" `isPrefixOf`) ls) of
        Nothing -> ioError (userError "no model name in /proc/cpuinfo")
        Just l -> pure (dropWhile (== ' ') (drop 1 (dropWhile (/= ':') l)))

memBytes :: IO (Maybe Integer)
memBytes
  | Info.os == "darwin" = do
      out <- tryRun "sysctl" ["-n", "hw.memsize"]
      pure (out >>= readMaybe)
  | otherwise = tryIO $ do
      ls <- lines <$> readFile "/proc/meminfo"
      case listToMaybe (filter ("MemTotal:" `isPrefixOf`) ls) of
        Just l | Just kb <- readMaybe (filter isDigit l) -> pure (kb * 1024)
        _ -> ioError (userError "no MemTotal in /proc/meminfo")

-- | @RLIMIT_NOFILE@, soft and hard. 'Nothing' for "unlimited" or
-- "unknown", same as every other field here.
nofileLimits :: IO (Maybe Integer, Maybe Integer)
nofileLimits = do
  result <- tryIO (getResourceLimit ResourceOpenFiles)
  pure $ case result of
    Nothing -> (Nothing, Nothing)
    Just limits -> (toLimit limits.softLimit, toLimit limits.hardLimit)
  where
    toLimit (ResourceLimit n) = Just n
    toLimit ResourceLimitInfinity = Nothing
    toLimit ResourceLimitUnknown = Nothing

tryRun :: String -> [String] -> IO (Maybe String)
tryRun cmd args = tryIO (trimEnd <$> readProcess cmd args "")
  where
    trimEnd = reverse . dropWhile (== '\n') . reverse

tryIO :: IO a -> IO (Maybe a)
tryIO action = either onError Just <$> try action
  where
    onError :: IOException -> Maybe a
    onError _ = Nothing
