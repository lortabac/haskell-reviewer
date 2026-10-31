-- | Preferences: precedence, ui.json and POST bodies. The ui.json
-- goldens are in fixtures/prefs (one level up: tests run from the
-- package dir).
module HaskellReviewer.Core.PrefsTest (tests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List (sort)
import Data.Text (Text)
import HaskellReviewer.Core.Config
  ( SidebarView (..),
    Theme (..),
    UiDefaults (..),
    builtinDefaults,
  )
import HaskellReviewer.Core.Prefs
import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory qualified as Dir
import System.FilePath (takeExtension)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Prefs"
    [ testCase "precedence: ui.json over config over built-in, per key" $ do
        let config =
              UiDefaults {theme = ThemeDark, exportedOnly = True, sidebar = SidebarFiles}
        effective builtinDefaults noPrefs @?= eff ThemeSystem False SidebarAuto
        effective config noPrefs @?= eff ThemeDark True SidebarFiles
        effective config (prefsOf (Just ThemeLight) Nothing Nothing)
          @?= eff ThemeLight True SidebarFiles
        effective config (prefsOf Nothing (Just False) Nothing)
          @?= eff ThemeDark False SidebarFiles
        effective config (prefsOf Nothing Nothing (Just SidebarModules))
          @?= eff ThemeDark True SidebarModules,
      testCase "applyPatch: the patch's keys win, the others stay" $ do
        patch <- decoded "{\"theme\":\"light\",\"sidebar\":\"files\"}"
        applyPatch patch (prefsOf (Just ThemeDark) (Just True) Nothing)
          @?= prefsOf (Just ThemeLight) (Just True) (Just SidebarFiles),
      testCase "decodePatch: good ones" $ do
        p1 <- decoded "{\"exportedOnly\":false}"
        patchPrefs p1 @?= prefsOf Nothing (Just False) Nothing
        p2 <-
          decoded " {\"theme\":\"system\",\"exportedOnly\":true,\"sidebar\":\"auto\"} \n"
        patchPrefs p2 @?= prefsOf (Just ThemeSystem) (Just True) (Just SidebarAuto),
      testCase "decodePatch: bad ones, with a reason" $
        mapM_
          ( \(body, why) ->
              (body, renderPatchError <$> firstOf (decodePatch body)) @?= (body, Just why)
          )
          [ ("", "the body is not JSON"),
            ("{", "the body is not JSON"),
            ("{\"theme\":\"dark\"} x", "the body is not JSON"),
            ("[]", "the body is not a JSON object"),
            ("null", "the body is not a JSON object"),
            ("{}", "no preference to set"),
            ("{\"version\":1}", "unknown preference \"version\""),
            ("{\"theme\":\"dark\",\"font\":1}", "unknown preference \"font\""),
            ("{\"theme\":\"blue\"}", badTheme),
            ("{\"theme\":\"Dark\"}", badTheme),
            ("{\"theme\":null}", badTheme),
            ("{\"exportedOnly\":\"true\"}", "exportedOnly: expected true or false"),
            ("{\"exportedOnly\":1}", "exportedOnly: expected true or false"),
            ("{\"sidebar\":[]}", badSidebar)
          ],
      testCase "ui.json: unknown keys and values we don't understand are kept" $
        decodePrefsFile
          "{\"version\":1,\"theme\":\"dark\",\"zoom\":1.5,\"exportedOnly\":\"yes\",\
          \\"layout\":{\"a\":[1,null]}}"
          @?= Right
            ( PrefsCurrent
                DecodedPrefs
                  { file =
                      PrefsFile
                        { prefs = prefsOf (Just ThemeDark) Nothing Nothing,
                          others =
                            KeyMap.fromList
                              [ ("zoom", Aeson.Number 1.5),
                                ("exportedOnly", Aeson.String "yes"),
                                ("layout", Aeson.object [("a", Aeson.toJSON nums)])
                              ]
                        },
                    oddValues =
                      [BadValue {key = "exportedOnly", expected = "true or false"}]
                  }
            ),
      testCase "ui.json: a value kept as is goes when a patch sets its key" $ do
        let bytes = "{\"version\":1,\"theme\":\"high-contrast\",\"sidebar\":1}"
        d <- either assertFailure pure (current bytes)
        encodePrefsFile d.file
          @?= "{\"version\":1,\"sidebar\":1,\"theme\":\"high-contrast\"}\n"
        patch <- decoded "{\"theme\":\"dark\"}"
        encodePrefsFile (applyPatchFile patch d.file)
          @?= "{\"version\":1,\"theme\":\"dark\",\"sidebar\":1}\n",
      testCase "ui.json: bad files" $
        mapM_
          ( \(bytes, bad) ->
              (bytes, firstOf (decodePrefsFile bytes)) @?= (bytes, Just bad)
          )
          [ ("[]", PrefsNotObject),
            ("1", PrefsNotObject),
            ("{}", PrefsBadVersion),
            ("{\"theme\":\"dark\"}", PrefsBadVersion),
            ("{\"version\":0}", PrefsBadVersion),
            ("{\"version\":-3}", PrefsBadVersion),
            ("{\"version\":\"1\"}", PrefsBadVersion),
            ("{\"version\":null}", PrefsBadVersion)
          ],
      testCase "ui.json: a newer version, with the keys we understand" $ do
        decodePrefsFile "{\"version\":2}" @?= Right (PrefsNewer noPrefs)
        decodePrefsFile "{\"version\":1.5,\"theme\":\"dark\",\"sidebar\":\"wide\"}"
          @?= Right (PrefsNewer (prefsOf (Just ThemeDark) Nothing Nothing)),
      testCase "ui.json: not JSON" $
        case decodePrefsFile "{\"version\":1," of
          Left (PrefsNotJson _) -> pure ()
          other -> assertFailure (show other),
      testProperty "ui.json: encode then decode gives the same" roundTrip,
      goldens
    ]

-- | encodePrefsFile's order: version, our keys, the others sorted.
goldens :: TestTree
goldens =
  testGroup "ui.json fixtures" $
    testCase "every fixture has a test, and the other way round" noStale
      : [ goldenVsString name (fixtureDir <> name <> ".json") (pure (encodePrefsFile f))
        | (name, f) <- cases
        ]
  where
    cases =
      [ ("empty", emptyPrefsFile),
        ( "full",
          PrefsFile
            { prefs = prefsOf (Just ThemeLight) (Just False) (Just SidebarModules),
              others =
                KeyMap.fromList
                  [("zz", Aeson.Bool True), ("aa", Aeson.String "x"), ("mm", Aeson.Null)]
            }
        ),
        ( "others-only",
          PrefsFile
            { prefs = noPrefs,
              others = KeyMap.fromList [("future", Aeson.object [("b", "1"), ("a", "2")])]
            }
        )
      ]
    noStale = do
      files <- Dir.listDirectory fixtureDir
      sort [f | f <- files, takeExtension f == ".json"]
        @?= sort [n <> ".json" | (n, _) <- cases]

fixtureDir :: FilePath
fixtureDir = "../fixtures/prefs/"

roundTrip :: Property
roundTrip = property $ do
  file <- forAll genFile
  decodePrefsFile (LBS.toStrict (encodePrefsFile file))
    === Right (PrefsCurrent DecodedPrefs {file = file, oddValues = []})

genFile :: Gen PrefsFile
genFile = do
  theme <- Gen.maybe (Gen.element [ThemeSystem, ThemeLight, ThemeDark])
  exportedOnly <- Gen.maybe Gen.bool
  sidebar <- Gen.maybe (Gen.element [SidebarAuto, SidebarModules, SidebarFiles])
  others <- Gen.list (Range.linear 0 5) ((,) <$> genKey <*> genValue)
  pure
    PrefsFile
      { prefs = Prefs {theme = theme, exportedOnly = exportedOnly, sidebar = sidebar},
        others = KeyMap.fromList [(Key.fromText k, v) | (k, v) <- others]
      }
  where
    genKey :: Gen Text
    genKey =
      Gen.filter
        (`notElem` ["version", "theme", "exportedOnly", "sidebar"])
        (Gen.text (Range.linear 0 8) Gen.unicode)
    genValue =
      Gen.choice
        [ pure Aeson.Null,
          Aeson.Bool <$> Gen.bool,
          Aeson.String <$> Gen.text (Range.linear 0 10) Gen.unicode,
          Aeson.toJSON <$> Gen.int (Range.linear (-1000) 1000),
          Aeson.toJSON <$> Gen.list (Range.linear 0 3) Gen.bool
        ]

decoded :: ByteString -> IO PrefsPatch
decoded body = either (assertFailure . show) pure (decodePatch body)

-- | A version 1 ui.json.
current :: ByteString -> Either String DecodedPrefs
current bytes = case decodePrefsFile bytes of
  Right (PrefsCurrent d) -> Right d
  other -> Left (show other)

firstOf :: Either a b -> Maybe a
firstOf = either Just (const Nothing)

badTheme :: Text
badTheme = "theme: expected one of \"system\", \"light\", \"dark\""

badSidebar :: Text
badSidebar = "sidebar: expected one of \"auto\", \"modules\", \"files\""

nums :: [Maybe Int]
nums = [Just 1, Nothing]

eff :: Theme -> Bool -> SidebarView -> Effective
eff t e s = Effective {theme = t, exportedOnly = e, sidebar = s}

-- | Prefs with these keys. Record updates would be ambiguous here.
prefsOf :: Maybe Theme -> Maybe Bool -> Maybe SidebarView -> Prefs
prefsOf t e s = Prefs {theme = t, exportedOnly = e, sidebar = s}
