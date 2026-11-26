-- | Reads @api\/session@ (D-332) and checks that the product runs the
-- mode and commits the bench asked for (D-357).
module Bench.Session
  ( -- * Types
    SessionAnswer (..),
    DiffSides (..),
    SessionError (..),

    -- * Reading and checking
    parseSession,
    checkSession,
  )
where

import Bench.Mode (Mode (..), modeText)
import Bench.Projects (Project (..))
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

-- | What @api\/session@ says.
data SessionAnswer
  = BrowseSession
  | DiffSession DiffSides
  deriving (Eq, Show)

-- | The commits of a diff session.
data DiffSides = DiffSides
  { baseCommit :: Text,
    -- | 'Nothing' for the working tree.
    headCommit :: Maybe Text
  }
  deriving (Eq, Show)

-- | Not a session answer: aeson's message.
newtype SessionError = SessionError String
  deriving (Eq, Show)

-- | Parse the body.
parseSession :: ByteString -> Either SessionError SessionAnswer
parseSession body = either (Left . SessionError) Right $ do
  v <- Aeson.eitherDecodeStrict body
  Aeson.Types.parseEither parser v
  where
    parser = Aeson.withObject "session" $ \o -> do
      mode <- o .: "mode" :: Aeson.Types.Parser Text
      case mode of
        "browse" -> pure BrowseSession
        "diff" -> do
          b <- o .: "base"
          h <- o .: "head"
          bc <- Aeson.withObject "base" (.: "commit") b
          hc <- Aeson.withObject "head" (.: "commit") h
          pure (DiffSession DiffSides {baseCommit = bc, headCommit = hc})
        _ -> fail ("unknown mode: " <> Text.unpack mode)

-- | Is it the session we asked for? Range: both pinned commits.
-- WorkTree: the pinned base, and the working tree as head. The message
-- says what's wrong.
checkSession :: Mode -> Project -> SessionAnswer -> Either Text ()
checkSession mode project answer = case (mode, answer) of
  (Browse, BrowseSession) -> Right ()
  (Range, DiffSession d) -> sides d (Just project.sha)
  (WorkTree, DiffSession d) -> sides d Nothing
  _ -> Left ("api/session: not a " <> modeText mode <> " session")
  where
    sides d headWanted
      | Just d.baseCommit /= project.base =
          wrong "base" (Just d.baseCommit) project.base
      | d.headCommit /= headWanted = wrong "head" d.headCommit headWanted
      | otherwise = Right ()
    wrong what got wanted =
      Left ("api/session: " <> what <> " is " <> want got <> ", expected " <> want wanted)
    want = fromMaybe "the working tree"
