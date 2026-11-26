-- | Find references while indexing (D-303): the project's refs queries
-- in a loop from the first accept until an answer is no longer
-- partial. Back to back, the worst case. Report only (D-208 sets no
-- target during indexing). Its own runs, like "Bench.SearchDuring".
module Bench.RefsDuring
  ( -- * Types
    RefsDuringRun (..),

    -- * Running
    timeRefs,
    runRefsDuring,
  )
where

import Bench.Http (Gzip (..), HttpResponse (..))
import Bench.IndexLog (IndexDone)
import Bench.Machine (LoadAvg, getLoadAvg)
import Bench.Mode (Side)
import Bench.Queries
  ( RefsAnswer (..),
    RefsQuery,
    parseNotFoundWhat,
    parseRefsAnswer,
    refsQueryText,
  )
import Bench.Refs (RefsTime (..), staleQueryMessage)
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
import Bench.SearchDuring (maxRounds)
import Bench.Url (refsPath)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as Text

-- | Ask once. Fails the run unless it's a 200 with a refs answer. A
-- 404 means the query list is stale for this pin: the message says
-- where to fix it.
timeRefs :: Session -> Side -> RefsQuery -> IO RefsTime
timeRefs s side q = do
  t <- getApi s Gzip (refsPath side q)
  let what = "refs query " <> Text.unpack (refsQueryText q)
  case t.response.status of
    200 -> case parseRefsAnswer t.response.body of
      Left e -> failWith (what <> ": " <> show e)
      Right a ->
        pure
          RefsTime
            { side = side,
              query = q,
              us = t.us,
              startUs = t.endUs - t.us,
              partial = a.partial,
              total = a.total,
              sent = a.sent,
              wireBytes = t.response.wireBytes
            }
    404 ->
      failWith
        (Text.unpack (staleQueryMessage side q (parseNotFoundWhat t.response.body)))
    other -> failWith (what <> ": expected 200, got " <> show other)

-- | One run of refs queries while indexing.
data RefsDuringRun = RefsDuringRun
  { refsTimes :: [RefsTime],
    -- | Rounds of every query.
    rounds :: Int,
    -- | When the done line came, since the session started.
    doneUs :: Integer,
    done :: IndexDone,
    loadAvg :: Maybe LoadAvg
  }

-- | One run on one side: rounds of every query until the last answer
-- of a round is full, at most 'maxRounds'; then wait for the done line
-- and stop (an idle server, Note [Second SIGTERM] in "Bench.Run"). The
-- same loop as 'Bench.SearchDuring.runSearchDuring'.
runRefsDuring ::
  SessionConfig -> Side -> NonEmpty RefsQuery -> IO (Either RunError RefsDuringRun)
runRefsDuring cfg side qs = fmap fst <$> withSession cfg steps
  where
    steps s = do
      (ts, n) <- loop s 1 []
      doneAt <- awaitDone s
      load <- getLoadAvg
      pure
        RefsDuringRun
          { refsTimes = concatMap NE.toList (reverse ts),
            rounds = n,
            doneUs = doneAt.atUs,
            done = doneAt.line,
            loadAvg = load
          }
    loop :: Session -> Int -> [NonEmpty RefsTime] -> IO ([NonEmpty RefsTime], Int)
    loop s n acc = do
      ts <- mapM (timeRefs s side) qs
      if (NE.last ts).partial && n < maxRounds
        then loop s (n + 1) (ts : acc)
        else pure (ts : acc, n)
