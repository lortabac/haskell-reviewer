-- | Reference projects, parsed from @bench\/projects.txt@ (D-82).
--
-- Pure: parsing and picking projects, nothing else. @bench\/fetch-projects.sh@
-- reads the same file with the same rules, so a line that parses here also
-- fetches there.
module Bench.Projects
  ( -- * Types
    Project (..),
    Role (..),
    ProjectsError (..),

    -- * Parsing and selection
    parseProjectsFile,
    selectProjects,
    renderProjectsError,
  )
where

import Data.Char (isDigit)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text

-- * Types

-- | One pinned reference project: a line of @bench\/projects.txt@.
data Project = Project
  { name :: Text,
    role :: Role,
    url :: Text,
    sha :: Text
  }
  deriving (Eq, Show)

-- | A project's size bucket. 'Extra' projects are only for spikes: they
-- are never picked by default, only by name.
data Role = Small | Typical | Large | Extra
  deriving (Eq, Show)

-- | What went wrong reading or picking projects.
data ProjectsError
  = -- | 1-based line number, and why.
    BadLine Int Text
  | UnknownName Text
  deriving (Eq, Show)

-- | A message for the user.
renderProjectsError :: ProjectsError -> Text
renderProjectsError (BadLine n why) =
  "bench/projects.txt line " <> Text.pack (show n) <> ": " <> why
renderProjectsError (UnknownName n) = n <> ": not in bench/projects.txt"

-- * Parsing and selection

-- | Parse the whole file: one @name role url sha@ per line, blank lines
-- and @#@ comments skipped.
parseProjectsFile :: Text -> Either ProjectsError [Project]
parseProjectsFile text =
  traverse parseLine (filter (not . skip . snd) (zip [1 ..] (Text.lines text)))
  where
    skip l = Text.null stripped || "#" `Text.isPrefixOf` stripped
      where
        stripped = Text.strip l

parseLine :: (Int, Text) -> Either ProjectsError Project
parseLine (n, l) = case Text.words l of
  (nm : r : u : s : _) -> do
    checkName n nm
    role <- parseRole n r
    checkSha n s
    Right Project {name = nm, role = role, url = u, sha = s}
  _ -> Left (BadLine n "expected 4 fields: name role url sha")

parseRole :: Int -> Text -> Either ProjectsError Role
parseRole n r = case r of
  "small" -> Right Small
  "typical" -> Right Typical
  "large" -> Right Large
  "extra" -> Right Extra
  _ -> Left (BadLine n ("bad role: " <> r))

-- | Same shape as @fetch-projects.sh@'s @^[a-z0-9-]+$@ check.
checkName :: Int -> Text -> Either ProjectsError ()
checkName n nm
  | not (Text.null nm) && Text.all okChar nm = Right ()
  | otherwise = Left (BadLine n ("bad name: " <> nm))
  where
    okChar c = (c >= 'a' && c <= 'z') || isDigit c || c == '-'

-- | Same shape as @fetch-projects.sh@'s @^[0-9a-f]{40}$@ check.
checkSha :: Int -> Text -> Either ProjectsError ()
checkSha n s
  | Text.length s == 40 && Text.all isLowerHex s = Right ()
  | otherwise = Left (BadLine n ("bad sha: " <> s))
  where
    isLowerHex c = isDigit c || (c >= 'a' && c <= 'f')

-- | Pick the projects asked for. No names: the three default roles
-- (small, typical, large). Some names: exactly those, whatever their
-- role; an unknown one is an error.
selectProjects :: [Text] -> [Project] -> Either ProjectsError [Project]
selectProjects [] projects = Right (filter defaultRole projects)
  where
    defaultRole p = p.role `elem` [Small, Typical, Large]
selectProjects names projects = traverse lookupOne names
  where
    lookupOne nm = case find ((== nm) . (.name)) projects of
      Just p -> Right p
      Nothing -> Left (UnknownName nm)
