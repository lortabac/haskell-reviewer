-- | Our own cabal.project parser (D-67). Reads only what we use:
-- @packages:@, @optional-packages:@, @with-compiler:@, @flags:@ (top
-- level and @package name@), @import:@ and @if@ \/ @elif@ \/
-- @else@. Everything else is skipped, silently.
--
-- Lexing is Cabal-syntax's 'readFields', so the layout rules are
-- cabal's.
module HaskellReviewer.Core.ProjectFile
  ( -- * Types
    RawItem (..),

    -- * Parsing
    parseProjectFile,
    parseFlags,
    parseWithCompilerVersion,
  )
where

import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Distribution.Fields
  ( Field (..),
    FieldLine (..),
    Name (..),
    SectionArg (..),
    readFields,
  )
import Distribution.Fields.ConfVar (parseConditionConfVar)
import Distribution.Fields.ParseResult (runParseResult)
import Distribution.Parsec.Position (Position (..))
import Distribution.Types.Condition (Condition)
import Distribution.Types.ConfVar (ConfVar (..))
import HaskellReviewer.Core.Project
  ( FlagName (..),
    FlagScope (..),
    FlagSetting (..),
    FlagsField (..),
    GhcVersion (..),
    Loc (..),
    PackageEntry (..),
    PackageName (..),
    ProjectWarning (..),
    Required (..),
    parseGhcVersionNumbers,
  )
import HaskellReviewer.Core.RelPath (RelPath)

-- | One field of a project file, before imports are followed.
data RawItem
  = RawPackage PackageEntry
  | RawFlags FlagsField
  | -- | The value as written.
    RawWithCompiler Loc Text
  | -- | The value as written.
    RawImport Loc Text
  | RawIf (Condition ConfVar) [RawItem] [RawItem]
  deriving (Eq, Show)

-- | Parse one project file. 'Left' with the lexer's error when it
-- doesn't lex at all. Otherwise the items, plus warnings for the parts
-- we skipped (a bad condition, @flag()@ in a condition).
parseProjectFile :: RelPath -> Text -> Either Text ([RawItem], [ProjectWarning])
parseProjectFile file txt = case readFields (Text.Encoding.encodeUtf8 txt) of
  Left e -> Left (Text.replace "\"the input\" " "" (oneLine (show e)))
  Right fields -> Right (topLevel file fields)

{- Note [Braces in fields]

Cabal's lexer reads a { at the start of a field's value, or at the
start of its first continuation line, as the start of a block. So
packages: {a,b}/* doesn't lex, in cabal-install too (it uses the same
lexer). Write a brace glob after another entry, or on a later line.
A file that doesn't lex gives a warning; see
"HaskellReviewer.Core.ProjectLoad" for what happens next.
-}

-- | A @flags:@ value: @+a -b c@ (bare is on), spaces or commas
-- between. Names are made lower case, like Cabal does.
parseFlags :: Text -> [FlagSetting]
parseFlags t = concatMap one (splitWords t)
  where
    one w = case Text.uncons w of
      Just ('+', n) -> flag n True
      Just ('-', n) -> flag n False
      _ -> flag w True
    flag n v
      | Text.null n = []
      | otherwise = [FlagSetting {name = FlagName (Text.toLower n), value = v}]

-- | The version in a @with-compiler:@ value, from a file name like
-- @ghc-9.12.2@ (any dir before it). 'Nothing' for plain @ghc@ or
-- anything else.
parseWithCompilerVersion :: Text -> Maybe GhcVersion
parseWithCompilerVersion t = do
  let base = Text.takeWhileEnd (/= '/') (Text.strip t)
  v <- Text.stripPrefix "ghc-" base
  GhcVersion <$> parseGhcVersionNumbers v

-- * Internal

-- | Items and warnings of a list of fields.
type Parsed = ([RawItem], [ProjectWarning])

topLevel :: RelPath -> [Field Position] -> Parsed
topLevel file = go
  where
    go = \case
      [] -> ([], [])
      Field (Name _ "packages") ls : rest -> add (entries Required ls) (go rest)
      Field (Name _ "optional-packages") ls : rest -> add (entries Optional ls) (go rest)
      Field (Name pos name) ls : rest -> add (field pos name (value ls)) (go rest)
      Section (Name _ "package") args body : rest ->
        add (packageStanza args body) (go rest)
      Section (Name pos "if") args body : rest ->
        let (elses, rest') = span isElse rest
         in add (conditional pos args body elses) (go rest')
      -- A stray elif or else, or a section we don't read.
      Section {} : rest -> go rest

    field pos name v = case name of
      "with-compiler" -> ([RawWithCompiler (loc pos) (Text.strip v)], [])
      "flags" -> ([flags pos GlobalFlags v], [])
      "import" -> ([RawImport (loc pos) (Text.strip v)], [])
      _ -> ([], [])

    -- One entry per glob, with its own line.
    entries req ls =
      ( [ RawPackage PackageEntry {at = loc pos, glob = g, required = req}
        | FieldLine pos b <- ls,
          g <- splitGlobs (decode b)
        ],
        []
      )

    flags pos scope v =
      RawFlags FlagsField {at = loc pos, scope = scope, flags = parseFlags v}

    -- flags: in package * are skipped, like cabal does for planning.
    -- See Note [Flag values] in "HaskellReviewer.Core.CabalFile".
    packageStanza args body = case Text.strip (argsText args) of
      "*" -> ([], [])
      n ->
        ( [ flags pos (OnePackage (PackageName n)) (value ls)
          | Field (Name pos "flags") ls <- body
          ],
          []
        )

    -- if / elif / else. An elif is an if inside the else branch.
    conditional pos args body elses =
      case runParseResult (parseConditionConfVar args) of
        (_, Left _) -> ([], [BadProjectCondition (loc pos)])
        (_, Right cond) ->
          let (thenItems, thenWarns) = go body
              (elseItems, elseWarns) = case elses of
                [] -> ([], [])
                Section (Name pos' "elif") args' body' : more ->
                  conditional pos' args' body' more
                Section _ _ body' : _ -> go body'
                Field {} : _ -> ([], [])
              flagWarn = [FlagInProjectCondition (loc pos) | any isFlag (toList cond)]
           in ( [RawIf cond thenItems elseItems],
                flagWarn <> thenWarns <> elseWarns
              )

    loc (Position l _) = Loc {file = file, line = l}

add :: Parsed -> Parsed -> Parsed
add (a, b) (c, d) = (a <> c, b <> d)

-- | @elif@ and @else@ go with the @if@ before them.
isElse :: Field Position -> Bool
isElse = \case
  Section (Name _ n) _ _ -> n == "elif" || n == "else"
  Field {} -> False

isFlag :: ConfVar -> Bool
isFlag = \case
  PackageFlag _ -> True
  _ -> False

-- | A field's lines, joined with spaces.
value :: [FieldLine Position] -> Text
value ls = Text.unwords [decode b | FieldLine _ b <- ls]

-- | A section's arguments, as written.
argsText :: [SectionArg Position] -> Text
argsText = Text.unwords . map arg
  where
    arg = \case
      SecArgName _ b -> decode b
      SecArgStr _ b -> decode b
      SecArgOther _ b -> decode b

-- | Globs split by spaces or commas, but not at a comma inside braces
-- (@{a,b}\/*@) or inside double quotes (@"my dir"@, quotes dropped).
splitGlobs :: Text -> [Text]
splitGlobs = filter (not . Text.null) . map Text.pack . go (0 :: Int) "" . Text.unpack
  where
    -- The current word is kept reversed.
    go depth w = \case
      [] -> [reverse w]
      '"' : rest ->
        let (quoted, rest') = break (== '"') rest
         in go depth (reverse quoted <> w) (drop 1 rest')
      c : rest
        | c == '{' -> go (depth + 1) (c : w) rest
        | c == '}' -> go (max 0 (depth - 1)) (c : w) rest
        | isSpace c || (c == ',' && depth == 0) -> reverse w : go depth "" rest
        | otherwise -> go depth (c : w) rest

-- | Words split by spaces or commas.
splitWords :: Text -> [Text]
splitWords = filter (not . Text.null) . Text.split (\c -> isSpace c || c == ',')

decode :: BS.ByteString -> Text
decode = Text.Encoding.decodeUtf8Lenient

oneLine :: String -> Text
oneLine = Text.unwords . Text.words . Text.pack
