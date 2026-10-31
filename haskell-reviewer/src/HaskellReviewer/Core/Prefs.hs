-- | UI preferences: what the user picked in the page (ui.json, D-68) on
-- top of the config's defaults. Pure.
module HaskellReviewer.Core.Prefs
  ( -- * Types
    Prefs (..),
    PrefsPatch,
    Effective (..),
    PrefsFile (..),
    DecodedPrefs (..),
    PrefsRead (..),
    BadPrefsFile (..),
    BadValue (..),
    PatchError (..),

    -- * Operations
    noPrefs,
    emptyPrefsFile,
    effective,
    applyPatch,
    applyPatchFile,
    overridePrefs,
    patchPrefs,
    patchKeys,

    -- * ui.json
    decodePrefsFile,
    encodePrefsFile,
    renderBadPrefsFile,
    renderBadValue,

    -- * POST bodies
    decodePatch,
    renderPatchError,
  )
where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as Encoding
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Config
  ( SidebarView,
    Theme,
    UiDefaults (..),
    sidebarFromName,
    sidebarName,
    themeFromName,
    themeName,
  )

{- Note [ui.json]

D-200. $XDG_STATE_HOME/haskell-reviewer/ui.json holds what the user
picked in the page, and only that:

  {"version":1,"theme":"dark","exportedOnly":true}

A key that is there wins over config.toml; a missing key takes the
config's default.

A newer version of the tool may add keys, or values to our keys (a
"high-contrast" theme). So we keep what we don't understand: unknown
keys, and known keys with a value we can't use, are written back as
they are (sorted, after ours), unless a change sets that key. Such a
value counts as not set.

The file is bad if it isn't JSON, isn't an object, or its version is
missing, not a number or below 1. A version above 1 is from a newer
tool: we never touch that file. See Note [Saving preferences] in
"HaskellReviewer.Session.Prefs" for what we do in both cases.

The keys are the ones of the wire (camelCase), not config.toml's
(kebab-case): the file is ours, the config is the user's.
-}

-- | The keys the user set. 'Nothing': not set, the default applies.
data Prefs = Prefs
  { theme :: !(Maybe Theme),
    exportedOnly :: !(Maybe Bool),
    sidebar :: !(Maybe SidebarView)
  }
  deriving (Eq, Show)

-- | A change from the page: the keys it sets.
--
-- Invariant: at least one key is set. Build one with 'decodePatch'.
newtype PrefsPatch = PrefsPatch Prefs
  deriving (Eq, Show)

-- | Every preference, with defaults filled in. What @GET api/prefs@
-- answers.
data Effective = Effective
  { theme :: !Theme,
    exportedOnly :: !Bool,
    sidebar :: !SidebarView
  }
  deriving (Eq, Show)

-- | What ui.json holds.
data PrefsFile = PrefsFile
  { prefs :: !Prefs,
    -- | What we don't use, kept as is: unknown keys, and known keys
    -- with a value we don't understand. See Note [ui.json].
    others :: !(KeyMap Value)
  }
  deriving (Eq, Show)

-- | A good ui.json.
data DecodedPrefs = DecodedPrefs
  { file :: !PrefsFile,
    -- | Known keys with a value we don't understand (kept in 'others').
    oddValues :: ![BadValue]
  }
  deriving (Eq, Show)

-- | What a ui.json that isn't bad holds. See Note [ui.json].
data PrefsRead
  = -- | Version 1: ours.
    PrefsCurrent DecodedPrefs
  | -- | Above 1: a newer tool wrote it. Not ours to touch; these are the
    -- keys we understand in it.
    PrefsNewer Prefs
  deriving (Eq, Show)

-- | Why ui.json can't be used. See Note [ui.json].
data BadPrefsFile
  = PrefsNotJson Text
  | PrefsNotObject
  | -- | Missing, not a number, or below 1.
    PrefsBadVersion
  | -- | Over the size limit of the reader.
    PrefsTooLarge
  deriving (Eq, Show)

-- | A known key with a value we can't use.
data BadValue = BadValue
  { key :: !Text,
    -- | What we take, for the user.
    expected :: !Text
  }
  deriving (Eq, Show)

-- | Why a POST body is not a good patch (a 400).
data PatchError
  = PatchNotJson
  | PatchNotObject
  | PatchEmpty
  | PatchUnknownKey Text
  | PatchBadValue BadValue
  deriving (Eq, Show)

-- | Nothing set.
noPrefs :: Prefs
noPrefs = Prefs {theme = Nothing, exportedOnly = Nothing, sidebar = Nothing}

-- | No ui.json yet.
emptyPrefsFile :: PrefsFile
emptyPrefsFile = PrefsFile {prefs = noPrefs, others = KeyMap.empty}

-- | The user's picks over the defaults.
effective :: UiDefaults -> Prefs -> Effective
effective d p =
  Effective
    { theme = fromMaybe d.theme p.theme,
      exportedOnly = fromMaybe d.exportedOnly p.exportedOnly,
      sidebar = fromMaybe d.sidebar p.sidebar
    }

-- | The keys of the patch win.
applyPatch :: PrefsPatch -> Prefs -> Prefs
applyPatch (PrefsPatch new) = overridePrefs new

-- | The keys set in the first win over the second's.
overridePrefs :: Prefs -> Prefs -> Prefs
overridePrefs new old =
  Prefs
    { theme = orElse new.theme old.theme,
      exportedOnly = orElse new.exportedOnly old.exportedOnly,
      sidebar = orElse new.sidebar old.sidebar
    }
  where
    orElse a b = maybe b Just a

-- | 'applyPatch' on a file. A key the patch sets is no longer kept as
-- is in 'others'.
applyPatchFile :: PrefsPatch -> PrefsFile -> PrefsFile
applyPatchFile patch old =
  PrefsFile
    { prefs = applyPatch patch old.prefs,
      others = foldr (KeyMap.delete . Key.fromText) old.others (patchKeys patch)
    }

-- | The keys a patch sets, as in JSON.
patchKeys :: PrefsPatch -> [Text]
patchKeys (PrefsPatch new) =
  ["theme" | Just _ <- [new.theme]]
    <> ["exportedOnly" | Just _ <- [new.exportedOnly]]
    <> ["sidebar" | Just _ <- [new.sidebar]]

-- | The keys a patch sets.
patchPrefs :: PrefsPatch -> Prefs
patchPrefs (PrefsPatch p) = p

-- | Read ui.json. See Note [ui.json].
decodePrefsFile :: ByteString -> Either BadPrefsFile PrefsRead
decodePrefsFile bytes = case Aeson.eitherDecodeStrict bytes of
  Left e -> Left (PrefsNotJson (Text.pack e))
  Right (Aeson.Object o) -> case KeyMap.lookup "version" o of
    Just (Aeson.Number v)
      | v == 1 -> Right (PrefsCurrent decoded)
      | v > 1 -> Right (PrefsNewer decoded.file.prefs)
      where
        rest = KeyMap.toAscList (KeyMap.delete "version" o)
        decoded = foldl' step (DecodedPrefs emptyPrefsFile []) rest
    _ -> Left PrefsBadVersion
  Right _ -> Left PrefsNotObject
  where
    step d (k, v) = case readKey (Key.toText k) v of
      Just (Right set) -> d {file = d.file {prefs = set d.file.prefs}}
      Just (Left bad) -> (keep k v d) {oddValues = d.oddValues <> [bad]}
      Nothing -> keep k v d
    keep k v d = d {file = d.file {others = KeyMap.insert k v d.file.others}}

-- | Write ui.json, in a fixed order: version, our keys, then the others
-- sorted by name. Ends with a newline.
encodePrefsFile :: PrefsFile -> LBS.ByteString
encodePrefsFile file =
  (<> "\n") . Encoding.encodingToLazyByteString . Aeson.pairs $
    Encoding.pair "version" (Encoding.int 1)
      <> maybe mempty (Encoding.pair "theme" . Encoding.text . themeName) p.theme
      <> maybe mempty (Encoding.pair "exportedOnly" . Encoding.bool) p.exportedOnly
      <> maybe mempty (Encoding.pair "sidebar" . Encoding.text . sidebarName) p.sidebar
      <> foldMap
        (\(k, v) -> Encoding.pair k (Aeson.toEncoding v))
        (KeyMap.toAscList file.others)
  where
    p = file.prefs

-- | For the user.
renderBadPrefsFile :: BadPrefsFile -> Text
renderBadPrefsFile = \case
  PrefsNotJson e -> "not JSON: " <> e
  PrefsNotObject -> "not a JSON object"
  PrefsBadVersion -> "no \"version\": 1"
  PrefsTooLarge -> "too large"

-- | For the user.
renderBadValue :: BadValue -> Text
renderBadValue b = b.key <> ": expected " <> b.expected

-- | A POST body: a JSON object with one or more known keys, each with a
-- good value.
decodePatch :: ByteString -> Either PatchError PrefsPatch
decodePatch bytes = case Aeson.eitherDecodeStrict bytes of
  Left _ -> Left PatchNotJson
  Right (Aeson.Object o)
    | KeyMap.null o -> Left PatchEmpty
    | otherwise -> PrefsPatch <$> foldl' step (Right noPrefs) (KeyMap.toAscList o)
  Right _ -> Left PatchNotObject
  where
    step acc (k, v) = do
      p <- acc
      case readKey (Key.toText k) v of
        Nothing -> Left (PatchUnknownKey (Key.toText k))
        Just (Left bad) -> Left (PatchBadValue bad)
        Just (Right set) -> Right (set p)

-- | The body of the 400.
renderPatchError :: PatchError -> Text
renderPatchError = \case
  PatchNotJson -> "the body is not JSON"
  PatchNotObject -> "the body is not a JSON object"
  PatchEmpty -> "no preference to set"
  PatchUnknownKey k -> "unknown preference " <> quoted k
  PatchBadValue bad -> renderBadValue bad

-- * Internal

-- | One of our keys: how it changes the prefs, or why its value is bad.
-- 'Nothing': not ours.
readKey :: Text -> Value -> Maybe (Either BadValue (Prefs -> Prefs))
readKey key v = case key of
  "theme" -> Just (enum themeFromName ["system", "light", "dark"] setTheme)
  "exportedOnly" -> Just $ case v of
    Aeson.Bool b -> Right (setExportedOnly b)
    _ -> Left BadValue {key = key, expected = "true or false"}
  "sidebar" -> Just (enum sidebarFromName ["auto", "modules", "files"] setSidebar)
  _ -> Nothing
  where
    enum ::
      (Text -> Maybe a) ->
      [Text] ->
      (a -> Prefs -> Prefs) ->
      Either BadValue (Prefs -> Prefs)
    enum fromName names set = case v of
      Aeson.String t | Just x <- fromName t -> Right (set x)
      _ ->
        Left
          BadValue
            { key = key,
              expected = "one of " <> Text.intercalate ", " (map quoted names)
            }

-- Plain updates would be ambiguous: Effective has the same fields.
setTheme :: Theme -> Prefs -> Prefs
setTheme x p =
  Prefs {theme = Just x, exportedOnly = p.exportedOnly, sidebar = p.sidebar}

setExportedOnly :: Bool -> Prefs -> Prefs
setExportedOnly x p =
  Prefs {theme = p.theme, exportedOnly = Just x, sidebar = p.sidebar}

setSidebar :: SidebarView -> Prefs -> Prefs
setSidebar x p =
  Prefs {theme = p.theme, exportedOnly = p.exportedOnly, sidebar = Just x}

quoted :: Text -> Text
quoted t = "\"" <> t <> "\""
