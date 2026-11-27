-- | Reads an @api\/diff\/\<path\>@ answer (CBOR, D-331): only its kind.
-- The bench times files with a real line diff (D-358).
module Bench.DiffAnswer
  ( -- * Types
    DiffKind (..),
    DiffAnswerError (..),

    -- * Reading
    parseDiffAnswer,
    worthTiming,
  )
where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Server.CborJson (cborToJson, renderCborJsonError)

-- | What the answer's @diff@ is.
data DiffKind
  = -- | Lines with at least one delete or insert run.
    LineDiff
  | -- | Lines, but only equal runs: an exact rename, a mode-only change,
    -- a BOM-only change.
    EqualLines
  | -- | Over the time limit (D-331). Only text gets diffed, so both
    -- sides are text.
    DiffTooSlow
  | -- | A side is binary, too large, a submodule, not local.
    DiffNotText
  deriving (Eq, Show)

-- | Not CBOR, or not a diff answer.
newtype DiffAnswerError = DiffAnswerError String
  deriving (Eq, Show)

-- | Parse the CBOR body.
parseDiffAnswer :: ByteString -> Either DiffAnswerError DiffKind
parseDiffAnswer body = either (Left . DiffAnswerError) Right $ do
  json <-
    either (Left . Text.unpack . renderCborJsonError) Right $
      cborToJson (LBS.fromStrict body)
  v <- Aeson.eitherDecode json
  Aeson.Types.parseEither parser v
  where
    parser = Aeson.withObject "answer" $ \o -> o .: "diff" >>= diffKind
    diffKind = Aeson.withObject "diff" $ \o -> do
      k <- o .: "kind" :: Aeson.Types.Parser Text
      case k of
        "lines" -> do
          runs <- o .: "runs" :: Aeson.Types.Parser [(Text, Int)]
          pure (if any ((/= "equal") . fst) runs then LineDiff else EqualLines)
        "too-slow" -> pure DiffTooSlow
        "not-text" -> pure DiffNotText
        other -> fail ("unknown diff kind: " <> Text.unpack other)

-- | Does the file go into the bench's sample? A line diff, or a text
-- diff too slow for the product: that one is the case the target is
-- about, so it stays (and the report warns).
worthTiming :: DiffKind -> Bool
worthTiming = \case
  LineDiff -> True
  DiffTooSlow -> True
  EqualLines -> False
  DiffNotText -> False
