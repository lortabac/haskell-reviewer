{-# LANGUAGE StrictData #-}

-- | Rich text: one small AST for Haddock comments, and later for LLM
-- answers (SPEC §11.4.9, D-59). It has no raw HTML and no images, so
-- whatever a comment says, the page can only show text, code and plain
-- links (SPEC §6.4, §9.2).
module HaskellReviewer.Core.RichText
  ( -- * Documents
    Doc (..),
    Since (..),
    Block (..),
    Header (..),
    ListItem (..),
    DefItem (..),
    Example (..),
    TableBody (..),
    Cell (..),

    -- * Inline text
    Inline (..),
    Hyperlink (..),
    ModLink (..),
    MathStyle (..),
    NameRef (..),
    RefNamespace (..),

    -- * Links
    SafeUrl,
    safeUrl,
    urlText,

    -- * Forcing
    forceDoc,
    forceInlines,
  )
where

import Data.Char (GeneralCategory (..), generalCategory)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Project (ModuleName)

-- * Documents

-- | A whole comment.
data Doc = Doc
  { blocks :: [Block],
    since :: Maybe Since
  }
  deriving (Eq, Show)

-- | @\@since 1.2.3@. The package (@\@since base-4.5@) only with GHC 9.12
-- and later. See Note [Two haddock-libraries] in
-- "HaskellReviewer.Core.Parser.Haddock".
data Since = Since
  { package :: Maybe Text,
    version :: NonEmpty Int
  }
  deriving (Eq, Show)

-- | A paragraph-level piece.
data Block
  = Paragraph [Inline]
  | -- | @= Title@ in a comment.
    Heading Header
  | -- | A code block. Haddock allows markup in @\@...\@@ blocks.
    CodeBlock [Inline]
  | UnorderedList [[Block]]
  | OrderedList [ListItem]
  | DefinitionList [DefItem]
  | -- | @>>> expr@ lines, with their results.
    Examples [Example]
  | -- | @prop> ...@.
    Property Text
  | Table TableBody
  deriving (Eq, Show)

-- | A heading inside a comment.
data Header = Header
  { -- | 1 to 6.
    level :: Int,
    title :: [Inline]
  }
  deriving (Eq, Show)

-- | An item of an ordered list.
data ListItem = ListItem
  { number :: Int,
    content :: [Block]
  }
  deriving (Eq, Show)

-- | @[term]: definition@.
data DefItem = DefItem
  { term :: [Inline],
    definition :: [Block]
  }
  deriving (Eq, Show)

-- | One @>>>@ line.
data Example = Example
  { expression :: Text,
    results :: [Text]
  }
  deriving (Eq, Show)

-- | A grid table, by rows.
data TableBody = TableBody
  { header :: [[Cell]],
    body :: [[Cell]]
  }
  deriving (Eq, Show)

-- | A table cell.
data Cell = Cell
  { colspan :: Int,
    rowspan :: Int,
    content :: [Inline]
  }
  deriving (Eq, Show)

-- * Inline text

-- | A piece of a paragraph.
data Inline
  = Str Text
  | Emphasis [Inline]
  | Bold [Inline]
  | -- | Monospaced. Can have markup inside.
    Code [Inline]
  | -- | TeX source, shown as is.
    Math MathStyle Text
  | Link Hyperlink
  | -- | @'name'@: a link once it's resolved, plain code until then.
    Name NameRef
  | ModuleLink ModLink
  deriving (Eq, Show)

-- | An http or https link.
data Hyperlink = Hyperlink
  { url :: SafeUrl,
    -- | 'Nothing': show the URL. Never @Just []@.
    label :: Maybe [Inline]
  }
  deriving (Eq, Show)

-- | @\"Module\"@ or @\"Module#anchor\"@, maybe with a label.
data ModLink = ModLink
  { name :: ModuleName,
    anchor :: Maybe Text,
    -- | 'Nothing': show the module name. Never @Just []@.
    label :: Maybe [Inline]
  }
  deriving (Eq, Show)

-- | @\\(...\\)@ or @\\[...\\]@.
data MathStyle = InlineMath | DisplayMath
  deriving (Eq, Show)

-- | An identifier in a comment, as written. "HaskellReviewer.Core.DocLinks"
-- resolves it.
data NameRef = NameRef
  { namespace :: RefNamespace,
    qualifier :: Maybe ModuleName,
    -- | No qualifier, no parens or backticks.
    occ :: Text,
    -- | Between the quotes, like @M.foo@, @(+)@ or @\`elem\`@.
    written :: Text
  }
  deriving (Eq, Show)

-- | @v'x'@, @t'T'@ or plain @'x'@.
data RefNamespace = ValueNs | TypeNs | AnyNs
  deriving (Eq, Show)

-- * Links

-- | A URL that is safe to link: http or https only (no @javascript:@,
-- no @data:@, no relative links), with no spaces of any kind, no line
-- or paragraph separators, no control characters and no invisible
-- format characters (like a right-to-left override,
-- which could make the URL read as another one). The page still shows
-- it in full and opens it only on a click (SPEC §6.4).
newtype SafeUrl = SafeUrl Text
  deriving (Eq, Show)

-- | 'Nothing' if the URL isn't safe.
safeUrl :: Text -> Maybe SafeUrl
safeUrl t
  | not (Text.any bad t) && any hasScheme ["http://", "https://"] = Just (SafeUrl t)
  | otherwise = Nothing
  where
    hasScheme s = maybe False (not . Text.null) (Text.stripPrefix s (Text.toLower t))
    bad c =
      c <= ' '
        || ('\DEL' <= c && c <= '\x9F')
        || generalCategory c `elem` [Format, Space, LineSeparator, ParagraphSeparator]

-- | The URL, in full.
urlText :: SafeUrl -> Text
urlText (SafeUrl t) = t

-- * Forcing

-- | Walk every list and 'Maybe', so nothing holds a thunk. StrictData
-- does the rest.
forceDoc :: Doc -> ()
forceDoc d = forceBlocks d.blocks `seq` maybe () since d.since
  where
    since s = maybe () (`seq` ()) s.package `seq` foldr seq () s.version

forceBlocks :: [Block] -> ()
forceBlocks = each block
  where
    block = \case
      Paragraph is -> forceInlines is
      Heading h -> forceInlines h.title
      CodeBlock is -> forceInlines is
      UnorderedList items -> each forceBlocks items
      OrderedList items -> each (\i -> forceBlocks i.content) items
      DefinitionList items ->
        each (\i -> forceInlines i.term `seq` forceBlocks i.definition) items
      Examples es -> each (\e -> each (`seq` ()) e.results) es
      Property _ -> ()
      Table t -> each row t.header `seq` each row t.body
    row = each (\c -> forceInlines c.content)

-- | Like 'forceDoc', for inline text.
forceInlines :: [Inline] -> ()
forceInlines = each inline
  where
    inline = \case
      Str _ -> ()
      Emphasis is -> forceInlines is
      Bold is -> forceInlines is
      Code is -> forceInlines is
      Math _ _ -> ()
      Link l -> maybe () forceInlines l.label
      Name r -> maybe () (`seq` ()) r.qualifier
      ModuleLink l -> maybe () (`seq` ()) l.anchor `seq` maybe () forceInlines l.label

each :: (a -> ()) -> [a] -> ()
each f = foldr (\x r -> f x `seq` r) ()
