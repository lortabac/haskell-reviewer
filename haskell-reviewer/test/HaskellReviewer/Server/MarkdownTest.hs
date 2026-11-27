-- | The Markdown warm-up doc has every doc node. See 'warmUpMarkdown'
-- in "HaskellReviewer.Server.Wire.Markdown".
module HaskellReviewer.Server.MarkdownTest (tests) where

import Data.List (nub, sort)
import HaskellReviewer.Core.RichText
import HaskellReviewer.Server.Wire.Markdown (warmUpDoc)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Markdown wire"
    [ testCase "the warm-up doc uses every block and inline constructor" $ do
        let (bs, is) = walk warmUpDoc
        sort (nub (map conName bs)) @?= sort allBlocks
        sort (nub (map conName is)) @?= sort allInlines
    ]

-- | Every constructor name. Add a new one here, and to 'warmUpDoc'.
allBlocks, allInlines :: [String]
allBlocks =
  [ "Paragraph",
    "Heading",
    "CodeBlock",
    "UnorderedList",
    "OrderedList",
    "DefinitionList",
    "Examples",
    "Property",
    "Table",
    "BlockQuote",
    "ThematicBreak",
    "Listing",
    "TaskList"
  ]
allInlines =
  [ "Str",
    "Emphasis",
    "Bold",
    "Code",
    "Math",
    "Link",
    "Name",
    "ModuleLink",
    "Strike",
    "FileLink",
    "AnchorLink"
  ]

-- | The constructor name, from 'Show'.
conName :: (Show a) => a -> String
conName = takeWhile (/= ' ') . show

-- | Every block and inline in the doc, at any depth. The two child
-- functions have no catch-all: a new constructor makes them
-- incomplete (a warning), so we must say where its children are.
walk :: Doc -> ([Block], [Inline])
walk d = (bs, is)
  where
    bs = closure (fst . blockKids) d.blocks
    is = closure inlineKids (concatMap (snd . blockKids) bs)
    closure kids = go
      where
        go [] = []
        go xs = xs <> go (concatMap kids xs)

-- | Child blocks and top-level inlines of a block. No catch-all, on
-- purpose: with -Werror in CI a new constructor breaks the build here.
-- Then add it to 'warmUpDoc' and to the name lists above.
blockKids :: Block -> ([Block], [Inline])
blockKids = \case
  Paragraph is -> ([], is)
  Heading h -> ([], h.title)
  CodeBlock is -> ([], is)
  UnorderedList bss -> (concat bss, [])
  OrderedList items -> (concatMap (.content) items, [])
  DefinitionList items -> (concatMap (.definition) items, concatMap (.term) items)
  Examples _ -> ([], [])
  Property _ -> ([], [])
  Table t -> ([], [i | row <- t.header <> t.body, c <- row, i <- c.content])
  BlockQuote bs -> (bs, [])
  ThematicBreak -> ([], [])
  Listing _ -> ([], [])
  TaskList items -> (concatMap (.content) items, [])

-- | Same: keep it exhaustive.
inlineKids :: Inline -> [Inline]
inlineKids = \case
  Str _ -> []
  Emphasis is -> is
  Bold is -> is
  Code is -> is
  Math _ _ -> []
  Link l -> concat l.label
  Name _ -> []
  ModuleLink l -> concat l.label
  Strike is -> is
  FileLink l -> concat l.label
  AnchorLink l -> concat l.label
