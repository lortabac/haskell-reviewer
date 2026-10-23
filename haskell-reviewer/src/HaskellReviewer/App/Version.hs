-- | What @--version@ prints.
module HaskellReviewer.App.Version
  ( -- * Version
    toolVersion,
    versionText,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Version (showVersion)
import HaskellReviewer.Core.Project (parserGhcVersion, renderGhcVersion)
import PackageInfo_haskell_reviewer qualified as PackageInfo

-- | The package version, e.g. @0.1.0.0@.
toolVersion :: Text
toolVersion = Text.pack (showVersion PackageInfo.version)

-- | The full @--version@ output (SPEC §2.1), without a final newline.
-- There is no companion lookup yet, so that line is fixed.
versionText :: Text
versionText =
  Text.intercalate
    "\n"
    [ "haskell-reviewer " <> toolVersion,
      "Haskell parser: GHC " <> renderGhcVersion parserGhcVersion,
      "HIE companions: none found"
    ]
