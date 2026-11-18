-- | git's line-end rules and config booleans, with git as the oracle
-- where it can be one.
module HaskellReviewer.Core.EolTest (tests) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import HaskellReviewer.Core.BlobId (blobId)
import HaskellReviewer.Core.Eol
  ( AttrState (..),
    Attrs (..),
    AutoCrlf (..),
    CleanEol (..),
    CleanForms (..),
    CoreConfig (..),
    EolRule (..),
    ExecBit (..),
    Filtering (..),
    Symlinks (..),
    applyEol,
    cleanForms,
    convertIsBinary,
    defaultCoreConfig,
    dropCrBeforeLf,
    eolRule,
    eolSure,
    filtering,
    gitBool,
    hasCrlfInIndex,
    parseCheckAttr,
    parseCoreConfig,
    textStats,
    unspecifiedAttrs,
  )
import HaskellReviewer.Core.Git (ObjectFormat (..), ObjectId (..))
import Hedgehog (Gen, annotateShow, forAll, property, withTests, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory qualified as Dir
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)
import TestSupport (GitRun (..), git, gitBytes)

tests :: TestTree
tests =
  -- A scratch repo with every attribute, made once (Note [Shared
  -- values in tests] in "HaskellReviewer.Core.PackageGlobTest").
  withResource makeRepo Dir.removeDirectoryRecursive $ \get ->
    testGroup
      "Eol"
      [ testCase "gitBool: as git config --type=bool reads it" $ do
          dir <- get
          forM_ boolSpellings $ \v -> do
            let arg = maybe "x.y" (\b -> "x.y=" <> Char8.unpack b) v
            run <- gitBytes dir ["-c", arg, "config", "--type=bool", "x.y"] ""
            let fromGit = case (run.code, run.out) of
                  (ExitSuccess, "true\n") -> Just True
                  (ExitSuccess, "false\n") -> Just False
                  _ -> Nothing
            assertBool (show (v, run.code, run.out)) (gitBool v == fromGit),
        testCase "gitBool: a few by hand" $ do
          gitBool Nothing @?= Just True
          gitBool (Just "") @?= Just False
          gitBool (Just "On") @?= Just True
          gitBool (Just "0x10") @?= Just True
          gitBool (Just "08") @?= Nothing
          gitBool (Just "2g") @?= Nothing
          gitBool (Just "bad") @?= Nothing
          -- Not in the git table: 2.44 refuses it, 2.56 takes it. We
          -- take it; on 2.44 git itself then fails the listing.
          gitBool (Just "-2147483648") @?= Just True,
        testCase "config: no value is true, last wins, bad bool fails" $ do
          parseCoreConfig "" @?= Right defaultCoreConfig
          parseCoreConfig "core.filemode\0core.symlinks\nfalse\0core.autocrlf\nInput\0"
            @?= Right
              CoreConfig
                { autocrlf = AutoCrlfInput,
                  fileMode = TrustExecBit,
                  symlinks = SymlinksOff,
                  stateDrivers = []
                }
          parseCoreConfig "filter.unset.clean\ntr a-z A-Z\0filter.unset.smudge\ncat\0"
            @?= Right defaultCoreConfig {stateDrivers = [AttrUnset]}
          parseCoreConfig
            "core.filemode\nfalse\0core.filemode\nyes\0core.autocrlf\ntrue\0"
            @?= Right defaultCoreConfig {autocrlf = AutoCrlfTrue}
          parseCoreConfig "core.filemode\nfalse\0core.checkstat\nminimal\0"
            @?= Right defaultCoreConfig {fileMode = IgnoreExecBit}
          case parseCoreConfig "core.filemode\nmaybe\0" of
            Left _ -> pure ()
            Right c -> assertFailure ("parsed " <> show c),
        testCase "check-attr: states, values, cut and unknown records" $ do
          let out =
                "a\0text\0set\0a\0eol\0crlf\0a\0crlf\0unset\0a\0filter\0lfs\0"
                  <> "a\0ident\0unspecified\0a\0working-tree-encoding\0UTF-16\0"
              want =
                Attrs
                  { text = AttrSet,
                    eol = AttrValue "crlf",
                    crlf = AttrUnset,
                    filter = AttrValue "lfs",
                    ident = AttrUnspecified,
                    encoding = AttrValue "UTF-16"
                  }
          parseCheckAttr out @?= Right (Map.fromList [("a", want)])
          isLeft (parseCheckAttr "a\0text\0") @?= True
          isLeft (parseCheckAttr "a\0color\0set\0") @?= True
          let plain = filtering defaultCoreConfig
              unsetDriver = defaultCoreConfig {stateDrivers = [AttrUnset]}
          plain want @?= Filtered
          plain unspecifiedAttrs @?= Unfiltered
          plain unspecifiedAttrs {ident = AttrSet} @?= Filtered
          plain unspecifiedAttrs {filter = AttrUnset} @?= Unfiltered
          -- filter=unset, and a driver named unset: maybe filtered.
          filtering unsetDriver unspecifiedAttrs {filter = AttrUnset} @?= Filtered
          filtering unsetDriver unspecifiedAttrs @?= Unfiltered,
        testCase "eolRule: by hand" $ do
          let a = unspecifiedAttrs
          eolRule AutoCrlfFalse a @?= EolKeep
          eolRule AutoCrlfTrue a @?= EolAuto
          eolRule AutoCrlfInput a @?= EolAuto
          eolRule AutoCrlfTrue a {text = AttrUnset} @?= EolKeep
          eolRule AutoCrlfFalse a {crlf = AttrSet} @?= EolText
          eolRule AutoCrlfFalse a {crlf = AttrValue "input"} @?= EolText
          eolRule AutoCrlfFalse a {text = AttrValue "auto", eol = AttrValue "lf"}
            @?= EolAuto
          eolRule AutoCrlfFalse a {eol = AttrValue "crlf"} @?= EolText
          eolRule AutoCrlfTrue a {text = AttrUnset, eol = AttrValue "lf"} @?= EolKeep
          -- text wins over crlf.
          eolRule AutoCrlfFalse a {text = AttrUnset, crlf = AttrSet} @?= EolKeep
          eolRule AutoCrlfFalse a {text = AttrValue "x", crlf = AttrSet} @?= EolText,
        testCase "convertIsBinary: git's rule" $ do
          let bin = convertIsBinary . textStats
          bin "abc\n" @?= False
          bin "a\0b" @?= True
          bin "a\rb" @?= True
          bin "a\r" @?= True
          bin "a\r\nb" @?= False
          -- 128 printable bytes allow one non-printable.
          bin (BS.replicate 128 0x61 <> "\1") @?= False
          bin (BS.replicate 127 0x61 <> "\1") @?= True
          bin (BS.replicate 127 0x61 <> "\DEL") @?= True
          -- A ^Z at the end doesn't count; elsewhere it does.
          bin "abc\26" @?= False
          bin "\26abc" @?= True
          bin "\b\t\ESC\f" @?= False,
        testCase "clean step: drop CR before LF only" $ do
          dropCrBeforeLf "a\r\nb\rc\r\n" @?= "a\nb\rc\n"
          dropCrBeforeLf "\r\r\n" @?= "\r\n"
          applyEol KeepRaw "a\r\n" @?= "a\r\n"
          applyEol DropCrBeforeLf "a\r\n\r" @?= "a\n\r"
          applyEol AutoOnRead "a\r\n" @?= "a\n"
          applyEol AutoOnRead "a\r\n\0" @?= "a\r\n\0"
          hasCrlfInIndex "a\r\n" @?= True
          hasCrlfInIndex "a\r\nb\r" @?= False
          hasCrlfInIndex "a\n" @?= False
          cleanForms EolAuto "a\r\n" @?= AutoForms "a\r\n" "a\n"
          cleanForms EolAuto "a\r\n\0" @?= OneForm "a\r\n\0"
          cleanForms EolText "a\r\n" @?= OneForm "a\n",
        testCase "eolSure: every reading of set or unset gives the same bytes" $ do
          let a = unspecifiedAttrs
              png = "\137PNG\r\n\26\n\0\0\0\rIHDR"
              sure ac at = eolSure ac at "a\r\nb\r\n"
          sure AutoCrlfFalse a @?= True
          -- text: converts; text=set: no action, so raw.
          sure AutoCrlfFalse a {text = AttrSet} @?= False
          -- text=unset crlf: crlf converts; -text: raw.
          sure AutoCrlfFalse a {text = AttrUnset, crlf = AttrSet} @?= False
          -- -text, binary, -crlf: raw either way when autocrlf is off.
          sure AutoCrlfFalse a {text = AttrUnset} @?= True
          sure AutoCrlfFalse a {crlf = AttrUnset} @?= True
          -- With autocrlf on, a literal unset means auto: converts text...
          sure AutoCrlfTrue a {text = AttrUnset} @?= False
          -- ...but not a binary file.
          eolSure AutoCrlfTrue a {text = AttrUnset} png @?= True
          eolSure AutoCrlfFalse a {text = AttrUnset} png @?= True
          -- No CRLF: nothing to tell apart.
          eolSure AutoCrlfFalse a {text = AttrSet} "a\nb\n" @?= True
          sure AutoCrlfFalse a {text = AttrValue "auto", crlf = AttrSet} @?= True,
        -- One git call per run, for many files: see BlobIdTest.
        testProperty "eolRule and the clean step: same as git hash-object" $
          withTests 30 . property $ do
            ac <- forAll (Gen.element [AutoCrlfFalse, AutoCrlfTrue, AutoCrlfInput])
            items <-
              forAll . Gen.list (Range.linear 1 20) $
                (,) <$> Gen.element attrCases <*> genBytes
            dir <- liftIO get
            files <- liftIO . forM (zip [0 :: Int ..] items) $ \(i, ((ext, _), b)) -> do
              let f = "f" <> show i <> ext
              BS.writeFile (dir </> f) b
              pure f
            let acArg = case ac of
                  AutoCrlfFalse -> "false"
                  AutoCrlfTrue -> "true"
                  AutoCrlfInput -> "input"
                args =
                  [ "-c",
                    "core.autocrlf=" <> acArg,
                    "-c",
                    "core.safecrlf=false",
                    "hash-object",
                    "--stdin-paths"
                  ]
            run <- liftIO (gitBytes dir args (Char8.pack (unlines files)))
            run.code === ExitSuccess
            -- No index: the blob never has CRLF, so auto converts
            -- unless binary.
            let ours =
                  [ blobId Sha1 (noIndexForm (cleanForms (eolRule ac a) b))
                  | ((_, a), b) <- items
                  ]
            annotateShow [eolRule ac a | ((_, a), _) <- items]
            ours === map (ObjectId . Text.pack . Char8.unpack) (Char8.lines run.out)
      ]
  where
    isLeft = either (const True) (const False)

-- | What git hashes when the index has no blob for the path: auto
-- converts.
noIndexForm :: CleanForms -> ByteString
noIndexForm = \case
  OneForm b -> b
  AutoForms _ converted -> converted

-- | Biased to CR, LF, NUL and ^Z.
genBytes :: Gen ByteString
genBytes =
  BS.pack
    <$> Gen.list
      (Range.linear 0 200)
      ( Gen.frequency
          [ (4, pure 13),
            (4, pure 10),
            (1, pure 0),
            (1, pure 26),
            (8, Gen.word8 (Range.linear 32 126)),
            (2, Gen.word8 (Range.linear 1 31)),
            (1, Gen.word8 (Range.linear 127 255))
          ]
      )

-- | A file extension in the scratch repo, and its attributes.
attrCases :: [(String, Attrs)]
attrCases =
  [ (".t", u {text = AttrSet}),
    (".a", u {text = AttrValue "auto"}),
    (".e", u {eol = AttrValue "crlf"}),
    (".b", u {text = AttrUnset}),
    (".i", u {text = AttrValue "input"}),
    (".ai", u {text = AttrValue "auto", eol = AttrValue "lf"}),
    (".c", u {crlf = AttrSet}),
    (".ci", u {crlf = AttrValue "input"}),
    (".bl", u {text = AttrUnset, eol = AttrValue "lf"}),
    (".n", u)
  ]
  where
    u = unspecifiedAttrs

gitattributes :: ByteString
gitattributes =
  "*.t text\n*.a text=auto\n*.e eol=crlf\n*.b -text\n*.i text=input\n\
  \*.ai text=auto eol=lf\n*.c crlf\n*.ci crlf=input\n*.bl -text eol=lf\n"

boolSpellings :: [Maybe ByteString]
boolSpellings =
  Nothing
    : map
      Just
      [ "",
        "true",
        "TRUE",
        "Yes",
        "on",
        "false",
        "No",
        "OFF",
        "1",
        "0",
        "-1",
        "+3",
        " 2",
        "0x0",
        "0x10",
        "010",
        "08",
        "0x",
        "1k",
        "0K",
        "1m",
        "1g",
        "2g",
        "2147483647",
        "2147483648",
        "-2147483647",
        "bad",
        "1x",
        "tru"
      ]

makeRepo :: IO FilePath
makeRepo = do
  tmp <- getCanonicalTemporaryDirectory
  dir <- createTempDirectory tmp "eol" >>= Dir.canonicalizePath
  git dir ["init", "-q"]
  BS.writeFile (dir </> ".gitattributes") gitattributes
  pure dir
