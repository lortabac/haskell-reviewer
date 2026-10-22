-- | A plain text dump of a 'Project', for golden tests. Stable: no
-- Show output, empty fields left out.
module ProjectDump (dumpProject) where

import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Project
  ( BrokenPackage (..),
    Component (..),
    Extension (..),
    FlagKind (..),
    FlagName (..),
    FlagSource (..),
    FlagValue (..),
    ListedModule (..),
    ModuleKind (..),
    ModuleName (..),
    Package (..),
    PackageName (..),
    Project (..),
    ProjectSource (..),
    WithCompiler (..),
    renderCabalProblem,
    renderComponentName,
    renderGhcVersion,
    renderProjectWarning,
  )
import HaskellReviewer.Core.RelPath (relDirText, relPathText)

-- | One line per fact, two spaces per level.
dumpProject :: Project -> Text
dumpProject p =
  Text.unlines $
    ["source: " <> source]
      <> [ "with-compiler: "
             <> w.written
             <> maybe "" (\v -> " (" <> renderGhcVersion v <> ")") w.version
             <> " from "
             <> relPathText w.file
         | Just w <- [p.withCompiler]
         ]
      <> concatMap package p.packages
      <> [ "broken: " <> relPathText b.cabalFile <> ": " <> renderCabalProblem b.problem
         | b <- p.brokenPackages
         ]
      <> ["warning: " <> renderProjectWarning w | w <- p.warnings]
  where
    source = case p.source of
      ImplicitRootCabalFiles -> "*.cabal in ROOT"
      ProjectFile [] -> "cabal.project"
      ProjectFile is -> "cabal.project, imports " <> commas (map relPathText is)

package :: Package -> [Text]
package p =
  ("package " <> name <> " " <> p.version <> " (" <> relPathText p.cabalFile <> ")")
    : map (indent . flag) p.flags
      <> concatMap (map indent . component) p.components
  where
    PackageName name = p.name
    flag f =
      let FlagName n = f.name
       in "flag "
            <> n
            <> " = "
            <> Text.pack (show f.value)
            <> ( case f.kind of
                   Manual -> " (manual, "
                   Automatic -> " (automatic, "
               )
            <> ( case f.source of
                   FlagDefault -> "default)"
                   FlagFromProject file -> "from " <> relPathText file <> ")"
               )

component :: Component -> [Text]
component c =
  (renderComponentName c.name <> if c.buildable then "" else " (not buildable)")
    : map
      indent
      ( field "source dirs" (map relDirText c.sourceDirs)
          <> ["language: " <> Text.pack (show c.language)]
          <> field "extensions" (map ext c.extensions)
          <> field "build-depends" [n | PackageName n <- c.buildDepends]
          <> ["main-is: " <> m | Just m <- [c.mainIs]]
          <> field "modules" (map listed c.modules)
          <> field "ghc-options" c.ghcOptions
          <> field "cpp-options" c.cppOptions
          <> field "include-dirs" (map relDirText c.includeDirs)
      )
  where
    ext e = (if e.enabled then "" else "-") <> e.name
    listed m =
      let ModuleName n = m.name
       in n <> " (" <> kind m.kind <> (if m.autogen then ", autogen" else "") <> ")"
    kind = \case
      Exposed -> "exposed"
      Other -> "other"
      Signature -> "signature"
      TestModule -> "test-module"

field :: Text -> [Text] -> [Text]
field _ [] = []
field label xs = [label <> ": " <> commas xs]

commas :: [Text] -> Text
commas = Text.intercalate ", "

indent :: Text -> Text
indent = ("  " <>)
