-- | Search while indexing (plan §2.3): the queries in a loop from the
-- first accept until an answer is no longer partial. Report only (D-208
-- sets no target during indexing). Its own runs: the load slows
-- indexing down, so it can't share a start-up run.
module Bench.SearchDuring
  ( -- * Types
    QueryTime (..),
    SearchDuringRun (..),

    -- * Running
    timeQuery,
    runSearchDuring,
  )
where

import Bench.Http (Gzip (..), HttpResponse (..))
import Bench.Machine (LoadAvg, getLoadAvg)
import Bench.Queries (SearchAnswer (..), parseSearchAnswer, queries)
import Bench.Run
  ( DoneAt (..),
    RunError,
    Session,
    SessionConfig,
    Timed (..),
    awaitDone,
    failWith,
    getApi,
    withSession,
  )
import Bench.Url (searchPath)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)

-- | One search request.
data QueryTime = QueryTime
  { query :: Text,
    -- | From request to the last byte.
    us :: Integer,
    partial :: Bool,
    matches :: Int
  }

-- | Ask once. Fails the run unless it's a 200 with a search answer.
timeQuery :: Session -> Text -> IO QueryTime
timeQuery s q = do
  t <- getApi s Gzip (searchPath q)
  let what = "search " <> show q
  if t.response.status /= 200
    then failWith (what <> ": expected 200, got " <> show t.response.status)
    else case parseSearchAnswer t.response.body of
      Left e -> failWith (what <> ": " <> show e)
      Right a ->
        pure QueryTime {query = q, us = t.us, partial = a.partial, matches = a.matches}

-- | One run of searches while indexing.
data SearchDuringRun = SearchDuringRun
  { queryTimes :: [QueryTime],
    -- | Rounds of every query.
    rounds :: Int,
    -- | When the done line came.
    doneUs :: Integer,
    loadAvg :: Maybe LoadAvg
  }

-- | One run: rounds of every query until the last answer of a round is
-- full, at most 'maxRounds'; then wait for the done line and stop.
runSearchDuring :: SessionConfig -> IO (Either RunError SearchDuringRun)
runSearchDuring cfg = fmap fst <$> withSession cfg steps
  where
    steps s = do
      (ts, n) <- loop s 1 []
      doneAt <- awaitDone s
      load <- getLoadAvg
      pure
        SearchDuringRun
          { queryTimes = concatMap NE.toList (reverse ts),
            rounds = n,
            doneUs = doneAt.atUs,
            loadAvg = load
          }
    loop :: Session -> Int -> [NonEmpty QueryTime] -> IO ([NonEmpty QueryTime], Int)
    loop s n acc = do
      ts <- mapM (timeQuery s) queries
      if (NE.last ts).partial && n < maxRounds
        then loop s (n + 1) (ts : acc)
        else pure (ts : acc, n)

-- | A bound, so a server that never finishes can't keep us here. Way
-- more than wire-server needs (a few rounds).
maxRounds :: Int
maxRounds = 200
