-- | The search queries the bench sends (D-208), and how it reads the
-- answers.
module Bench.Queries
  ( -- * Queries
    queries,

    -- * Answers
    SearchAnswer (..),
    SearchAnswerError (..),
    parseSearchAnswer,
  )
where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

-- | 20 fixed queries, the same as T11a's measure: one char, prefixes,
-- camel case, operators, qualified, module names, and no match. Not
-- encoded: "Bench.Url" does that.
queries :: NonEmpty Text
queries =
  "a"
    :| [ "m",
         "x",
         "map",
         "insert",
         "fromJ",
         "parse",
         "fM",
         "toJS",
         "<$>",
         ">>=",
         "<>",
         "+",
         "Data.Map.insert",
         "M.lookup",
         "Text.pack",
         "T.unpack",
         "Text.Pandoc.Readers",
         "readerExtensions",
         "zzqqxx"
       ]

-- | The parts of an @api\/search@ answer (D-207) the bench looks at.
data SearchAnswer = SearchAnswer
  { partial :: Bool,
    matches :: Int
  }
  deriving (Eq, Show)

-- | The body is not a search answer (D-207): aeson's message.
newtype SearchAnswerError = SearchAnswerError String
  deriving (Eq, Show)

-- | Read an @api\/search@ body.
parseSearchAnswer :: ByteString -> Either SearchAnswerError SearchAnswer
parseSearchAnswer body = either (Left . SearchAnswerError) Right $ do
  v <- Aeson.eitherDecodeStrict body
  Aeson.Types.parseEither parser v
  where
    parser = Aeson.withObject "search" $ \o -> do
      p <- o .: "partial"
      m <- o .: "matches"
      _ <- o .: "results" :: Aeson.Types.Parser [Aeson.Value]
      pure SearchAnswer {partial = p, matches = m}
