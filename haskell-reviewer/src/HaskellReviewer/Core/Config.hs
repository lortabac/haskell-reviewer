-- | The user config file, config.toml (SPEC §7). Pure: the caller reads
-- the file. Only the @[ui]@ table for now.
module HaskellReviewer.Core.Config
  ( -- * Types
    Theme (..),
    SidebarView (..),
    UiDefaults (..),
    ParsedConfig (..),
    ConfigWarning (..),

    -- * Defaults
    builtinDefaults,

    -- * Parsing
    parseConfig,
    renderConfigWarning,

    -- * Names
    themeName,
    themeFromName,
    sidebarName,
    sidebarFromName,
  )
where

import Data.Char (isControl, ord)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Span (Col (..), Line (..))
import Numeric (showHex)
import Toml.Semantics
  ( SemanticError (..),
    SemanticErrorKind (..),
    Table' (..),
    Value' (..),
    semantics,
    valueAnn,
  )
import Toml.Syntax (Located (..), Position (..), parseRawToml)

{- Note [Config file]

D-199. config.toml is read once at start and never written. Today it
has one table:

  [ui]
  theme = "system"        # system, light or dark
  exported-only = false
  sidebar = "auto"        # auto, modules or files

These are defaults: what the user picks in the page (ui.json) wins.

A bad file is never fatal. Each problem is a warning with line and
column, and the program goes on:

- a TOML syntax error, or a key set twice: the whole file is ignored;
- a known key with a bad type or value: that key keeps its default;
- an unknown key or table: ignored.

We walk the table ourselves instead of using Toml.decode, so each
warning gets its own position and one bad key doesn't spoil the rest.
Columns follow Note [Span convention] in "HaskellReviewer.Core.Span": a
tab is one column, and a BOM at the start doesn't count (we drop it).
Control characters in messages are escaped, so a file can't send
escape sequences to the terminal.
-}

-- | The colour theme.
data Theme
  = -- | Follow the OS.
    ThemeSystem
  | ThemeLight
  | ThemeDark
  deriving (Eq, Show)

-- | What the sidebar shows (D-111).
data SidebarView
  = -- | Modules if the project has any, else Files.
    SidebarAuto
  | SidebarModules
  | SidebarFiles
  deriving (Eq, Show)

-- | The @[ui]@ table: defaults for the page's preferences.
data UiDefaults = UiDefaults
  { theme :: !Theme,
    exportedOnly :: !Bool,
    sidebar :: !SidebarView
  }
  deriving (Eq, Show)

-- | What a config file gives.
data ParsedConfig = ParsedConfig
  { defaults :: !UiDefaults,
    -- | In file order.
    warnings :: ![ConfigWarning]
  }
  deriving (Eq, Show)

-- | One problem in the file.
data ConfigWarning = ConfigWarning
  { line :: !Line,
    column :: !Col,
    message :: !Text
  }
  deriving (Eq, Show)

-- | What we use with no config file.
builtinDefaults :: UiDefaults
builtinDefaults =
  UiDefaults {theme = ThemeSystem, exportedOnly = False, sidebar = SidebarAuto}

-- | The defaults from a config file, and its problems. See Note
-- [Config file].
parseConfig :: Text -> ParsedConfig
parseConfig input = case parseRawToml text of
  Left (Located pos msg) ->
    ParsedConfig builtinDefaults [warning (pos, Text.pack msg <> "; file ignored")]
  Right exprs -> case semantics exprs of
    Left err ->
      ParsedConfig
        builtinDefaults
        [warning (err.errorAnn, semanticMessage err <> "; file ignored")]
    Right (MkTable top) ->
      let (d, ws) = Map.foldlWithKey' topKey (builtinDefaults, []) top
       in ParsedConfig d (map warning (sortOn (posIndex . fst) ws))
  where
    -- Editors on Windows may add one.
    text = fromMaybe input (Text.stripPrefix "\xFEFF" input)
    -- Where each line starts, in code points: toml-parser counts a tab
    -- as up to 8 columns, we count it as one.
    starts =
      Seq.fromList (scanl (+) 0 (map ((+ 1) . Text.length) (Text.splitOn "\n" text)))
    warning (Position i l _, msg) =
      ConfigWarning
        { line = Line (fromIntegral l),
          column = Col (fromIntegral (i - fromMaybe 0 (Seq.lookup (l - 1) starts) + 1)),
          message = escapeControls msg
        }
    posIndex (Position i _ _) = i

-- | @\<path>:\<line>:\<col>: \<message>@.
renderConfigWarning :: FilePath -> ConfigWarning -> Text
renderConfigWarning path w@ConfigWarning {line = Line l, column = Col c} =
  Text.pack path
    <> ":"
    <> Text.pack (show l)
    <> ":"
    <> Text.pack (show c)
    <> ": "
    <> w.message

-- | As in config.toml and on the wire.
themeName :: Theme -> Text
themeName = \case
  ThemeSystem -> "system"
  ThemeLight -> "light"
  ThemeDark -> "dark"

-- | The other way round.
themeFromName :: Text -> Maybe Theme
themeFromName = \case
  "system" -> Just ThemeSystem
  "light" -> Just ThemeLight
  "dark" -> Just ThemeDark
  _ -> Nothing

-- | As in config.toml and on the wire.
sidebarName :: SidebarView -> Text
sidebarName = \case
  SidebarAuto -> "auto"
  SidebarModules -> "modules"
  SidebarFiles -> "files"

-- | The other way round.
sidebarFromName :: Text -> Maybe SidebarView
sidebarFromName = \case
  "auto" -> Just SidebarAuto
  "modules" -> Just SidebarModules
  "files" -> Just SidebarFiles
  _ -> Nothing

-- * Internal

-- | The defaults so far, and the warnings, in no order.
type Acc = (UiDefaults, [(Position, Text)])

topKey :: Acc -> Text -> (Position, Value' Position) -> Acc
topKey acc key (keyPos, value) = case (key, value) of
  ("ui", Table' _ (MkTable ui)) -> Map.foldlWithKey' uiKey acc ui
  ("ui", _) -> warn acc keyPos ("ui must be a table, got " <> typeName value)
  (_, Table' _ _) -> warn acc keyPos ("unknown table [" <> key <> "]")
  _ -> warn acc keyPos ("unknown key " <> quoted key)

uiKey :: Acc -> Text -> (Position, Value' Position) -> Acc
uiKey acc@(d, ws) key (keyPos, value) = case key of
  "theme" -> enum themeFromName "system, light or dark" (\t -> d {theme = t})
  "sidebar" -> enum sidebarFromName "auto, modules or files" (\s -> d {sidebar = s})
  "exported-only" -> case value of
    Bool' _ b -> (d {exportedOnly = b}, ws)
    _ -> wrongType "a boolean"
  _ -> case value of
    Table' _ _ -> warn acc keyPos ("unknown table [ui." <> key <> "]")
    _ -> warn acc keyPos ("unknown key " <> quoted ("ui." <> key))
  where
    enum :: (Text -> Maybe a) -> Text -> (a -> UiDefaults) -> Acc
    enum fromName expected set = case value of
      Text' pos t -> case fromName t of
        Just x -> (set x, ws)
        Nothing ->
          warn acc pos $
            "ui." <> key <> ": unknown value " <> quoted t <> ", expected " <> expected
      _ -> wrongType "a string"
    wrongType expected =
      warn acc (valueAnn value) $
        "ui." <> key <> ": expected " <> expected <> ", got " <> typeName value

warn :: Acc -> Position -> Text -> Acc
warn (d, ws) pos msg = (d, (pos, msg) : ws)

-- | Control characters (C0, DEL, C1) as @\\xNN@.
escapeControls :: Text -> Text
escapeControls = Text.concatMap $ \ch ->
  if isControl ch
    then "\\x" <> Text.justifyRight 2 '0' (Text.pack (showHex (ord ch) ""))
    else Text.singleton ch

quoted :: Text -> Text
quoted t = "\"" <> t <> "\""

semanticMessage :: SemanticError Position -> Text
semanticMessage err =
  "key " <> quoted err.errorKey <> case err.errorKind of
    AlreadyAssigned -> " is set twice"
    ClosedTable -> " is a table that is already closed"
    ImplicitlyTable -> " is already a table"

-- | TOML's names for value types.
typeName :: Value' a -> Text
typeName = \case
  Integer' _ _ -> "an integer"
  Double' _ _ -> "a float"
  List' _ _ -> "an array"
  Table' _ _ -> "a table"
  Bool' _ _ -> "a boolean"
  Text' _ _ -> "a string"
  TimeOfDay' _ _ -> "a time"
  ZonedTime' _ _ -> "a date-time"
  LocalTime' _ _ -> "a date-time"
  Day' _ _ -> "a date"
