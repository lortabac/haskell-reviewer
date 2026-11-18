-- | Resolving @--diff@ to two commits, before the URL line (SPEC §2,
-- D-316).
module HaskellReviewer.Effects.Revisions
  ( -- * Types
    DiffError (..),

    -- * Resolving
    resolveDiff,
    renderDiffError,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Core.Git (History (..), ObjectId, RepoInfo (..))
import HaskellReviewer.Core.Revspec
  ( BaseFrom (..),
    DiffInfo (..),
    Range (..),
    RangeForm (..),
    printable,
    renderRange,
  )
import HaskellReviewer.Core.Sides (Pair (..))
import HaskellReviewer.Effects.Git (Git, GitError, mergeBase, resolveCommit)

-- | Why the revisions of a range don't give two commits. Exit 2.
data DiffError
  = -- | An end that names no commit: unknown, or a blob or tree. The
    -- end as typed.
    DiffUnknownRev Text History
  | -- | @BASE...HEAD@ with no merge base.
    DiffNoMergeBase Range History
  deriving (Eq, Show)

-- | Message for the user, without the program name. One line: names
-- are 'printable'.
renderDiffError :: DiffError -> Text
renderDiffError = \case
  DiffUnknownRev rev h ->
    "--diff: unknown revision or not a commit: "
      <> printable rev
      <> shallow h "; this is a shallow clone, its history may be cut"
  DiffNoMergeBase r h ->
    "--diff: no merge base for "
      <> printable (renderRange r)
      <> shallow h "; this is a shallow clone: fetch more history"
  where
    shallow h hint = case h of
      ShallowClone -> hint
      FullHistory -> ""

-- | Resolve both ends, and the merge base for @...@. The outer 'Left' is
-- git failing; the inner one, revisions that don't work. Base first, so
-- a bad base is reported first. One call at a time: two at once risks
-- Q-RTS-BLACKHOLE and saves only a few ms.
resolveDiff ::
  Git -> RepoInfo -> Range -> IO (Either GitError (Either DiffError DiffInfo))
resolveDiff git info range =
  commit range.base >>= \case
    Left e -> pure (Left e)
    Right Nothing -> unknown range.base
    Right (Just b) ->
      commit range.head >>= \case
        Left e -> pure (Left e)
        Right Nothing -> unknown range.head
        Right (Just h) -> case range.form of
          TwoDot -> found b h BaseTyped
          ThreeDot ->
            mergeBase git b h >>= \case
              Left e -> pure (Left e)
              Right [] -> pure (Right (Left (DiffNoMergeBase range info.history)))
              Right bases@(m : _) -> found m h (MergeBase (length bases))
  where
    commit :: Text -> IO (Either GitError (Maybe ObjectId))
    commit rev = resolveCommit git (Text.unpack rev)
    unknown rev = pure (Right (Left (DiffUnknownRev rev info.history)))
    found b h from =
      pure . Right . Right $
        DiffInfo {range = range, sides = Pair {head = h, base = b}, baseFrom = from}
