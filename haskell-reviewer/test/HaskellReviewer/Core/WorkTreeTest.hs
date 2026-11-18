{-# LANGUAGE RecordWildCards #-}

-- | The @--debug@ parser and the stat check, one row per D-311 case.
module HaskellReviewer.Core.WorkTreeTest (tests) where

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Int (Int64)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Word (Word32, Word64)
import HaskellReviewer.Core.Eol (ExecBit (..), Symlinks (..))
import HaskellReviewer.Core.Git (ObjectId (..))
import HaskellReviewer.Core.WorkTree
  ( CheckSettings (..),
    Exec (..),
    FullStat (..),
    IndexEntry (..),
    IndexKind (..),
    IndexStat (..),
    IndexTime (..),
    Look (..),
    Seconds (..),
    StatTime (..),
    Verdict (..),
    checkEntry,
    parseDebugEntries,
  )
import Hedgehog (Gen, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric (showHex, showOct)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "WorkTree check"
    [ testProperty "--debug: rendered entries parse back" $ property $ do
        es <- forAll (Gen.list (Range.linear 0 20) genEntry)
        parseDebugEntries (BS.concat (map render es)) === Right es,
      testCase "--debug: junk is an error" $ do
        let good = render sample
            isLeft = either (const True) (const False)
            bad b = assertBool (show b) (isLeft (parseDebugEntries b))
        parseDebugEntries good @?= Right [sample]
        bad (BS.take (BS.length good - 1) good)
        bad (good <> "x")
        bad (replace "ino: 4" "ino: -4" good)
        bad (replace "ino: 4" "ino: 99999999999" good)
        bad (replace "ctime: 1:2" "ctime: 1" good)
        bad (replace "flags: 0" "flags: z" good)
        bad (replace "\0" "" good)
        bad (replace " 0\t" " 7\t" good)
        bad (replace "100644 " "" good)
        bad (replace "  uid:" " uid:" good),
      testCase "stat check: one row per case" $ do
        let check e l = checkEntry settings e l
            found = LookFound clean
            with f = LookFound (f clean)
            withMode m = with (setMode m)
        check sample found @?= StatClean
        check sample LookGone @?= Deleted
        check sample (LookFailed "EACCES") @?= Unhashed
        check sample (withMode 0o040755) @?= Deleted
        check sample (withMode 0o120777) @?= Modified
        check sample (withMode 0o010644) @?= Modified
        check sample (withMode 0o100755) @?= Modified
        checkEntry settings {execBit = IgnoreExecBit} sample (withMode 0o100755)
          @?= StatClean
        check sample (with (setSize 6)) @?= Modified
        -- Sizes are kept in 32 bits.
        check sample (with (setSize (5 + 2 ^ (32 :: Int)))) @?= StatClean
        check sample (with (setMtime (StatTime 3 9))) @?= Candidate
        check sample (with (setCtime (StatTime 2 2))) @?= Candidate
        check sample (with (setIno 5)) @?= Candidate
        check sample (with (setUid 1)) @?= Candidate
        check sample (with (setGid 1)) @?= Candidate
        -- dev is not compared.
        check sample (with (setDev 1)) @?= StatClean
        -- Racy: mtime seconds not older than the index's.
        checkEntry settings {indexMtime = IndexTime (Seconds 3)} sample found
          @?= Candidate
        checkEntry settings {indexMtime = IndexTimeUnknown} sample found @?= Candidate
        -- Smudged: size 0 but not the empty blob.
        let smudged = sample {stat = IndexStat (StatTime 1 2) (StatTime 3 4) 4 5 6 0}
            empty = with (setSize 0)
        check smudged empty @?= Candidate
        check smudged {oid = settings.emptyBlob} empty @?= StatClean
        -- The size counts even when the recorded one is 0.
        check smudged {oid = settings.emptyBlob} (with (setSize 9)) @?= Candidate
        check smudged (with (setSize 9)) @?= Candidate
        -- Flags.
        check sample {flags = 0x40000000} LookGone @?= StatClean
        check sample {flags = 0x20000000} found @?= Unhashed
        check sample {flags = 0x20000000} LookGone @?= Deleted
        check sample {flags = 0x8000} (with (setMtime (StatTime 3 9))) @?= Candidate
        check sample {flags = 0x20000} found @?= Modified
        check sample {stage = 2} found @?= Unhashed
        check sample {stage = 2} LookGone @?= Deleted
        -- Symlinks.
        let link = sample {kind = IndexSymlink}
        check link (withMode 0o120777) @?= StatClean
        check link found @?= Modified
        checkEntry settings {symlinks = SymlinksOff} link found @?= StatClean
        -- Gitlinks.
        let gl = sample {kind = IndexGitlink}
        check gl (with (setMtime (StatTime 9 9) . setMode 0o040755)) @?= StatClean
        check gl found @?= Deleted
        check gl LookGone @?= Deleted
        check gl (LookFailed "x") @?= StatClean,
      testCase "stat check: an executable entry" $ do
        let x = sample {kind = IndexRegular Executable}
        forM_ [(0o100755, StatClean), (0o100644, Modified), (0o100744, StatClean)] $
          \(m, v) -> checkEntry settings x (LookFound (setMode m clean)) @?= v
    ]

-- | What lstat says for 'sample' when it is clean.
clean :: FullStat
clean =
  FullStat
    { mode = 0o100644,
      size = 5,
      mtime = StatTime 3 4,
      ctime = StatTime 1 2,
      ino = 4,
      dev = 77,
      uid = 5,
      gid = 6
    }

-- Setters: record update would be ambiguous here (IndexStat has the
-- same field names).
setMode :: Word32 -> FullStat -> FullStat
setMode x FullStat {..} = FullStat {mode = x, ..}

setSize :: Int64 -> FullStat -> FullStat
setSize x FullStat {..} = FullStat {size = x, ..}

setMtime, setCtime :: StatTime -> FullStat -> FullStat
setMtime x FullStat {..} = FullStat {mtime = x, ..}
setCtime x FullStat {..} = FullStat {ctime = x, ..}

setIno, setDev :: Word64 -> FullStat -> FullStat
setIno x FullStat {..} = FullStat {ino = x, ..}
setDev x FullStat {..} = FullStat {dev = x, ..}

setUid, setGid :: Word32 -> FullStat -> FullStat
setUid x FullStat {..} = FullStat {uid = x, ..}
setGid x FullStat {..} = FullStat {gid = x, ..}

settings :: CheckSettings
settings =
  CheckSettings
    { indexMtime = IndexTime (Seconds 100),
      execBit = TrustExecBit,
      symlinks = SymlinksOn,
      emptyBlob = ObjectId (Text.replicate 40 "e")
    }

sample :: IndexEntry
sample =
  IndexEntry
    { kind = IndexRegular NotExecutable,
      oid = ObjectId (Text.replicate 40 "a"),
      stage = 0,
      path = "a.hs",
      stat = IndexStat (StatTime 1 2) (StatTime 3 4) 4 5 6 5,
      flags = 0
    }

-- | As @ls-files -z --stage --debug@ prints it.
render :: IndexEntry -> ByteString
render e =
  Char8.pack (showOct (modeOf e.kind) "")
    <> " "
    <> Text.Encoding.encodeUtf8 (let ObjectId o = e.oid in o)
    <> " "
    <> num e.stage
    <> "\t"
    <> e.path
    <> "\0"
    <> "  ctime: "
    <> time e.stat.ctime
    <> "\n  mtime: "
    <> time e.stat.mtime
    <> "\n  dev: 2049\tino: "
    <> num e.stat.ino
    <> "\n  uid: "
    <> num e.stat.uid
    <> "\tgid: "
    <> num e.stat.gid
    <> "\n  size: "
    <> num e.stat.size
    <> "\tflags: "
    <> Char8.pack (showHex e.flags "")
    <> "\n"
  where
    num :: (Show a) => a -> ByteString
    num = Char8.pack . show
    time t = num t.sec <> ":" <> num t.nsec
    modeOf :: IndexKind -> Word32
    modeOf = \case
      IndexRegular NotExecutable -> 0o100644
      IndexRegular Executable -> 0o100755
      IndexSymlink -> 0o120000
      IndexGitlink -> 0o160000
      IndexOther _ -> 0o040000

genEntry :: Gen IndexEntry
genEntry = do
  kind <-
    Gen.element
      [ IndexRegular NotExecutable,
        IndexRegular Executable,
        IndexSymlink,
        IndexGitlink,
        IndexOther "40000"
      ]
  oid <- Gen.text (Range.singleton 40) (Gen.element ("0123456789abcdef" :: String))
  stage <- Gen.int (Range.linear 0 3)
  path <- Gen.bytes (Range.linear 1 30)
  let w32 = Gen.word32 Range.linearBounded
      t = StatTime <$> w32 <*> w32
  st <- IndexStat <$> t <*> t <*> w32 <*> w32 <*> w32 <*> w32
  flags <- w32
  pure
    IndexEntry
      { kind = kind,
        oid = ObjectId oid,
        stage = stage,
        path = BS.map (\c -> if c == 0 then 1 else c) path,
        stat = st,
        flags = flags
      }

replace :: ByteString -> ByteString -> ByteString -> ByteString
replace old new s = case BS.breakSubstring old s of
  (a, b)
    | BS.null b -> s
    | otherwise -> a <> new <> BS.drop (BS.length old) b
