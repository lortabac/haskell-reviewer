-- | The project's modules, from @api\/modules\/\<side\>@ (D-124): which
-- ones get a parse, in which order, and which ones the bench asks for.
module Bench.ModuleList
  ( -- * Types
    ModuleItem (..),
    SideModules (..),
    Source (..),
    SampleKind (..),
    Sample (..),
    OnDemandStatus (..),
    OnDemandTimes (..),
    Queued (..),
    Sized (..),
    IdleStep (..),
    ModuleListError (..),

    -- * Reading
    parseModuleList,
    isSlot,
    isChanged,
    queueOrder,
    diffQueueOrder,
    modeQueueOrder,

    -- * Picking
    pickSamples,
    pickDiffSamples,
    sampleKindText,
    onDemandStatus,
    idlePlan,
  )
where

import Bench.Mode (Side (..))
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as Text

-- * Types

-- | One listed module.
data ModuleItem = ModuleItem
  { ref :: Text,
    source :: Source,
    -- | The file's change (@added@, @modified@, ...), diff mode only
    -- (D-325). 'Nothing' in browse mode or for an unchanged file.
    change :: Maybe Text,
    -- | Its parse settings differ from the other side's.
    settingsChanged :: Bool
  }
  deriving (Eq, Show)

-- | One side's module tree.
data SideModules = SideModules
  { side :: Side,
    items :: [ModuleItem]
  }
  deriving (Eq, Show)

-- | Where its text comes from (the @source.kind@ of the wire format).
data Source
  = -- | A Haskell file we parse: a component's module or a loose one.
    -- It has an index slot (D-157). The path is relative to the root.
    Parsed FilePath
  | Generated
  | -- | Preprocessed by the component's flags (@-pgmF@, @-pgmL@).
    PreprocessedByFlags
  | -- | The reason: autogen, missing, outside, symlink.
    NoSource Text
  deriving (Eq, Show)

-- | Which module an on-demand sample is (plan §1).
data SampleKind
  = -- | Last in the background queue: the one most likely not done yet.
    LastQueued
  | -- | The largest file: may hit the big-file lock (D-159).
    Largest
  | -- | Half way down the queue.
    Middle
  deriving (Eq, Show, Enum, Bounded)

-- | A module page the start-up runs ask for while indexing.
data Sample = Sample
  { kind :: SampleKind,
    side :: Side,
    ref :: Text
  }
  deriving (Eq, Show)

-- | Does an on-demand sample count? Only 'Counted' ones go in the
-- statistic.
data OnDemandStatus
  = Counted
  | -- | Asked once indexing was done: not on demand.
    IndexedBefore
  deriving (Eq, Show)

-- | When the sample's request started, and when the done line came,
-- both since start.
data OnDemandTimes = OnDemandTimes
  { startUs :: Integer,
    doneUs :: Integer
  }
  deriving (Eq, Show)

-- | A sample counts if @api\/indexing@ said "not done" just before it
-- and its request started before the done line. A slow answer that
-- ends after the done line still counts: that's the case the target is
-- about (a queue jump that doesn't work, the big-file lock).
onDemandStatus :: Bool -> OnDemandTimes -> OnDemandStatus
onDemandStatus indexedBefore t
  | indexedBefore || t.startUs > t.doneUs = IndexedBefore
  | otherwise = Counted

-- | A module we parse, in queue order.
data Queued = Queued
  { side :: Side,
    ref :: Text,
    -- | Relative to the project root.
    path :: FilePath
  }
  deriving (Eq, Show)

-- | A queued module and its file size.
data Sized = Sized
  { side :: Side,
    ref :: Text,
    bytes :: Integer
  }
  deriving (Eq, Show)

-- | One page of the navigation with idle gaps.
data IdleStep = IdleStep
  { -- | Index into the modules we parse, in queue order.
    index :: Int,
    -- | How long to wait before asking, 200 to 1000.
    gapMs :: Int
  }
  deriving (Eq, Show)

-- | The body is not a module tree (D-124): aeson's message.
newtype ModuleListError = ModuleListError String
  deriving (Eq, Show)

-- * Reading

-- | Every module of the tree, in tree order: packages, components,
-- modules as listed, then the loose modules of @other@. Boot files
-- are skipped (no module page).
parseModuleList :: ByteString -> Either ModuleListError [ModuleItem]
parseModuleList body = either (Left . ModuleListError) Right $ do
  v <- Aeson.eitherDecodeStrict body
  Aeson.Types.parseEither parser v
  where
    parser = Aeson.withObject "modules" $ \o -> do
      pkgs <- o .: "packages" :: Aeson.Types.Parser [Aeson.Value]
      inComponents <- concat <$> traverse package pkgs
      others <- o .: "other" :: Aeson.Types.Parser [Aeson.Value]
      loose <- concat <$> traverse other others
      pure (inComponents <> loose)
    package = Aeson.withObject "package" $ \p -> do
      comps <- p .: "components" :: Aeson.Types.Parser [Aeson.Value]
      concat <$> traverse component comps
    component = Aeson.withObject "component" $ \c -> do
      ms <- c .: "modules" :: Aeson.Types.Parser [Aeson.Value]
      traverse owned ms
    owned = Aeson.withObject "module" $ \m -> do
      r <- m .: "ref"
      src <- m .: "source"
      s <- source src
      (c, sc) <- markers m
      pure ModuleItem {ref = r, source = s, change = c, settingsChanged = sc}
    markers m = (,) <$> m .: "change" <*> m .: "settingsChanged"
    source = Aeson.withObject "source" $ \s -> do
      kind <- s .: "kind" :: Aeson.Types.Parser Text
      case kind of
        "module" -> Parsed . Text.unpack <$> s .: "path"
        "generated" -> pure Generated
        "preprocessed" -> pure PreprocessedByFlags
        "no-source" -> NoSource <$> s .: "reason"
        _ -> fail ("unknown source kind: " <> Text.unpack kind)
    other = Aeson.withObject "other" $ \x -> do
      kind <- x .: "kind" :: Aeson.Types.Parser Text
      case kind of
        "module" -> do
          r <- x .: "ref"
          p <- x .: "path"
          (c, sc) <- markers x
          pure
            [ ModuleItem
                { ref = r,
                  source = Parsed (Text.unpack p),
                  change = c,
                  settingsChanged = sc
                }
            ]
        _ -> pure []

-- | Does it get a parse (and an index slot)?
isSlot :: ModuleItem -> Bool
isSlot m = case m.source of
  Parsed _ -> True
  _ -> False

-- | Is it changed, so indexed first in diff mode (D-317)? From the
-- markers (D-325). The product goes by blob and settings, which is
-- close enough: this only picks the samples.
isChanged :: ModuleItem -> Bool
isChanged m = isJust m.change || m.settingsChanged

-- | The modules we parse on one side, in background queue order
-- (D-158). That's tree order: see @indexJobs@ in
-- "HaskellReviewer.Core.IndexQueue".
queueOrder :: Side -> [ModuleItem] -> [Queued]
queueOrder side items =
  [ Queued {side = side, ref = m.ref, path = p}
  | m@ModuleItem {source = Parsed p} <- items
  ]

-- | Diff mode's queue order, head and base trees given (D-317): head's
-- changed, base's changed, head's other modules, base's other modules.
diffQueueOrder :: [ModuleItem] -> [ModuleItem] -> [Queued]
diffQueueOrder headItems baseItems =
  pick Head isChanged headItems
    <> pick Base isChanged baseItems
    <> pick Head (not . isChanged) headItems
    <> pick Base (not . isChanged) baseItems
  where
    pick side p = queueOrder side . filter p

-- | The queue order of the trees from 'Bench.Mode.modeSides': tree order
-- in browse mode, 'diffQueueOrder' in diff mode.
modeQueueOrder :: NonEmpty SideModules -> [Queued]
modeQueueOrder (t :| []) = queueOrder t.side t.items
modeQueueOrder (h :| b : _) = diffQueueOrder h.items b.items

-- * Picking

-- | The on-demand samples, from the queue with each file's size in
-- bytes. Each kind once, in 'SampleKind' order; fewer if two kinds pick
-- the same module. Empty if nothing is parsed.
pickSamples :: [Sized] -> [Sample]
pickSamples list = case NE.nonEmpty list of
  Nothing -> []
  Just queue ->
    dedupe
      [ sample LastQueued (NE.last queue),
        -- First of the largest, if two have the same size.
        sample Largest (NE.head (NE.sortOn (Down . (.bytes)) queue)),
        sample Middle (list !! (length list `div` 2))
      ]
  where
    sample k q = Sample {kind = k, side = q.side, ref = q.ref}

-- | Diff mode's samples (D-357): 'pickSamples' on head's part of the
-- queue (sizes from the checkout, which is at head), then the last of
-- the whole queue on base: the base side is indexed last.
pickDiffSamples :: [Sized] -> [Queued] -> [Sample]
pickDiffSamples headSized queue =
  dedupe (pickSamples headSized <> lastBase)
  where
    lastBase = case reverse [q | q <- queue, q.side == Base] of
      q : _ -> [Sample {kind = LastQueued, side = Base, ref = q.ref}]
      [] -> []

-- | Each module once, the first kept.
dedupe :: [Sample] -> [Sample]
dedupe = foldr (\s acc -> s : filter (\t -> (t.side, t.ref) /= (s.side, s.ref)) acc) []

-- | The name in the report.
sampleKindText :: SampleKind -> Text
sampleKindText LastQueued = "last-queued"
sampleKindText Largest = "largest"
sampleKindText Middle = "middle"

-- | Which pages to open, and how long to wait before each, for the
-- navigation with idle gaps: @n@ steps, the index under @modules@.
-- Fixed by @seed@, so runs differ but every bench run asks the same.
-- Empty if @modules@ is 0.
idlePlan :: Int -> Int -> Int -> [IdleStep]
idlePlan _ _ 0 = []
idlePlan seed n modules = take n (pairs (iterate next (seed * 7919 + 1)))
  where
    -- A plain linear congruential generator: no random package needed,
    -- and nothing here needs good randomness.
    next x = (x * 1103515245 + 12345) `mod` 2147483648
    -- The low bits of this generator repeat quickly: use the high ones.
    pairs (a : b : rest) =
      IdleStep {index = high a `mod` modules, gapMs = 200 + high b `mod` 801} : pairs rest
    pairs _ = []
    high x = x `div` 65536
