-- | Where the user config and the state live (XDG base dirs, SPEC §7).
-- Pure: the caller passes the environment.
module HaskellReviewer.Core.Xdg
  ( -- * Types
    XdgDir (..),
    XdgProblem (..),

    -- * Paths
    xdgDir,
    configFile,
    prefsFile,
    renderXdgProblem,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import System.FilePath (isAbsolute, (</>))

{- Note [XDG dirs]

D-201. $XDG_CONFIG_HOME and $XDG_STATE_HOME count when set, not empty
and absolute (the XDG spec says to ignore a relative one). Else we
use $HOME/.config and $HOME/.local/state. Same on macOS: no
~/Library (SPEC §7).

If the variable doesn't count and HOME is unset, empty or relative,
there is no dir: no config, and preferences live in memory only.
-}

-- | Which base dir.
data XdgDir
  = -- | @$XDG_CONFIG_HOME@, for config.toml.
    XdgConfig
  | -- | @$XDG_STATE_HOME@, for ui.json.
    XdgState
  deriving (Eq, Show)

-- | Why there is no dir.
data XdgProblem
  = -- | HOME is unset or empty.
    HomeUnset
  | -- | HOME is a relative path.
    HomeRelative FilePath
  deriving (Eq, Show)

-- | The base dir. See Note [XDG dirs].
xdgDir :: XdgDir -> (String -> Maybe String) -> Either XdgProblem FilePath
xdgDir which env = case env var of
  Just d | usable d -> Right d
  _ -> case env "HOME" of
    Nothing -> Left HomeUnset
    Just "" -> Left HomeUnset
    Just home
      | isAbsolute home -> Right (home </> fallback)
      | otherwise -> Left (HomeRelative home)
  where
    usable d = not (null d) && isAbsolute d
    (var, fallback) = case which of
      XdgConfig -> ("XDG_CONFIG_HOME", ".config")
      XdgState -> ("XDG_STATE_HOME", ".local/state")

-- | @haskell-reviewer/config.toml@ in the config dir.
configFile :: (String -> Maybe String) -> Either XdgProblem FilePath
configFile env = (</> "haskell-reviewer" </> "config.toml") <$> xdgDir XdgConfig env

-- | @haskell-reviewer/ui.json@ in the state dir.
prefsFile :: (String -> Maybe String) -> Either XdgProblem FilePath
prefsFile env = (</> "haskell-reviewer" </> "ui.json") <$> xdgDir XdgState env

-- | For the user.
renderXdgProblem :: XdgProblem -> Text
renderXdgProblem = \case
  HomeUnset -> "HOME is not set"
  HomeRelative home -> "HOME is not an absolute path: " <> Text.pack home
