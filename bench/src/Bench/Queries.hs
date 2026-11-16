-- | The search queries (D-208), find-references queries (D-303) and
-- text search queries (D-308) the bench sends, and how it reads the
-- answers.
module Bench.Queries
  ( -- * Search queries
    queries,

    -- * Refs queries
    RefsQuery (..),
    refsQueries,
    refsQueryText,

    -- * Text search queries
    textQueries,
    textLimitBox,
    textLimitPage,

    -- * Answers
    SearchAnswer (..),
    SearchAnswerError (..),
    parseSearchAnswer,
    RefsAnswer (..),
    RefsAnswerError (..),
    parseRefsAnswer,
    parseNotFoundWhat,
    TextAnswer (..),
    TextAnswerError (..),
    parseTextAnswer,
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

-- | One find-references request: a module ref (as on @api\/module@)
-- and a declaration id (@type:Maybe@, @val:map@, ...). Not encoded.
data RefsQuery = RefsQuery
  { moduleRef :: Text,
    decl :: Text
  }
  deriving (Eq, Show)

-- | The refs queries for a reference project (D-303): six each, heavy,
-- medium and rare names of several kinds. Long-lived public names, so
-- a pin move is unlikely to break them; if it does, the bench says so
-- (a 404 is a broken run). Other projects have none.
refsQueries :: Text -> [RefsQuery]
refsQueries = \case
  "wire-server" ->
    [ RefsQuery "Data.Id" "type:UserId",
      RefsQuery "Data.Qualified" "type:Qualified",
      RefsQuery "Data.Qualified" "con:Qualified",
      RefsQuery "Data.Qualified" "val:tUnqualified",
      RefsQuery "Data.Qualified" "field:Qualified.qUnqualified",
      RefsQuery "Data.Qualified" "val:tSplit"
    ]
  "pandoc" ->
    [ RefsQuery "Text.Pandoc.Class.PandocMonad" "class:PandocMonad",
      RefsQuery "Text.Pandoc.Options" "type:ReaderOptions",
      RefsQuery "Text.Pandoc.Options" "field:ReaderOptions.readerExtensions",
      RefsQuery "Text.Pandoc.Class.PandocMonad" "val:report",
      RefsQuery "Text.Pandoc.Shared" "val:stringify",
      RefsQuery "Text.Pandoc.Shared" "val:trimr"
    ]
  "optparse-applicative" ->
    [ RefsQuery "Options.Applicative.Types" "type:Parser",
      RefsQuery "Options.Applicative.Types" "type:ParserInfo",
      RefsQuery "Options.Applicative.Types" "field:ParserInfo.infoParser",
      RefsQuery "Options.Applicative.Builder" "val:long",
      RefsQuery "Options.Applicative.Builder" "val:strOption",
      RefsQuery "Options.Applicative.Extra" "val:hsubparser"
    ]
  _ -> []

-- | @module decl@, for messages and the report.
refsQueryText :: RefsQuery -> Text
refsQueryText q = q.moduleRef <> " " <> q.decl

-- | 10 fixed text queries (D-308): one char that hits nearly every
-- line, common words, an operator, no match, non-ASCII, a long line
-- (71 chars, a line of wire-server's AGPL licence header), inner
-- quotes, a marker. Raw text; 'Bench.Url.textSearchPath' encodes it.
textQueries :: NonEmpty Text
textQueries =
  "e"
    :| [ "where",
         "import qualified",
         "ByteString",
         "<$>",
         "zzqqxx",
         "ü",
         "-- This program is free software: you can redistribute it and/or modify",
         "a \"b",
         "TODO"
       ]

-- | The limit the search box sends (D-308's target is for it).
textLimitBox :: Int
textLimitBox = 8

-- | The limit of the results page (report only).
textLimitPage :: Int
textLimitPage = 1000

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

-- | The parts of an @api\/refs@ answer (D-294) the bench looks at.
data RefsAnswer = RefsAnswer
  { partial :: Bool,
    -- | All the refs found.
    total :: Int,
    -- | Those in the answer (capped at 1000).
    sent :: Int
  }
  deriving (Eq, Show)

-- | The body is not a refs answer: aeson's message.
newtype RefsAnswerError = RefsAnswerError String
  deriving (Eq, Show)

-- | Read an @api\/refs@ body.
parseRefsAnswer :: ByteString -> Either RefsAnswerError RefsAnswer
parseRefsAnswer body = either (Left . RefsAnswerError) Right $ do
  v <- Aeson.eitherDecodeStrict body
  Aeson.Types.parseEither parser v
  where
    parser = Aeson.withObject "refs" $ \o -> do
      p <- o .: "partial"
      t <- o .: "total"
      rs <- o .: "results" :: Aeson.Types.Parser [Aeson.Value]
      pure RefsAnswer {partial = p, total = t, sent = length rs}

-- | The @what@ of a 404 body (@module@ or @decl@), if it is one.
parseNotFoundWhat :: ByteString -> Maybe Text
parseNotFoundWhat body = do
  v <- Aeson.decodeStrict body
  Aeson.Types.parseMaybe parser v
  where
    parser = Aeson.withObject "not-found" $ \o -> do
      k <- o .: "kind"
      if k == ("not-found" :: Text) then o .: "what" else fail "not a not-found"

-- | The parts of an @api\/text-search@ answer (D-307) the bench looks
-- at.
data TextAnswer = TextAnswer
  { stopped :: Bool,
    -- | Every matching line.
    total :: Int,
    files :: Int,
    -- | Results in the answer.
    sent :: Int
  }
  deriving (Eq, Show)

-- | The body is not a text search answer: aeson's message.
newtype TextAnswerError = TextAnswerError String
  deriving (Eq, Show)

-- | Read an @api\/text-search@ body.
parseTextAnswer :: ByteString -> Either TextAnswerError TextAnswer
parseTextAnswer body = either (Left . TextAnswerError) Right $ do
  v <- Aeson.eitherDecodeStrict body
  Aeson.Types.parseEither parser v
  where
    parser = Aeson.withObject "text-search" $ \o -> do
      st <- o .: "stopped"
      t <- o .: "total"
      f <- o .: "files"
      rs <- o .: "results" :: Aeson.Types.Parser [Aeson.Value]
      pure TextAnswer {stopped = st, total = t, files = f, sent = length rs}
