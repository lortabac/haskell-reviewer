-- | A start-up run (plan §2.1): from the spawn to "UI usable", one
-- on-demand module page while indexing, the end of indexing, idle
-- memory, stop.
module Bench.Startup
  ( -- * Types
    StartupRun (..),
    UiParts (..),
    OnDemand (..),
    FileList (..),
    FileSource (..),

    -- * Running
    runStartup,
  )
where

import Bench.Http (Gzip (..), HttpResponse (..))
import Bench.IndexLog (IndexDone)
import Bench.Machine (LoadAvg, getLoadAvg)
import Bench.ModuleList
  ( OnDemandStatus,
    OnDemandTimes (..),
    Sample (..),
    onDemandStatus,
    parseModuleList,
  )
import Bench.Queries (SearchAnswer (..), parseSearchAnswer, queries)
import Bench.RtsStats (RtsStats)
import Bench.Run
  ( DoneAt (..),
    Ended (..),
    ProcMemory (..),
    RunError,
    Session (..),
    SessionConfig,
    Timed (..),
    awaitDone,
    failWith,
    getApi,
    getPath,
    procMemory,
    requireRts,
    traceSpans,
    withSession,
  )
import Bench.Shell (assetPaths)
import Bench.Url (modulePath, searchPath)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..), mapConcurrently)
import Control.Monad (unless, when)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as Text
import HaskellReviewer.Server.CborJson (cborToJson, renderCborJsonError)

-- * Types

-- | Everything one start-up run measures. @...Us@ fields are
-- microseconds since right before the spawn (exec and RTS init
-- included, D-85), unless they say otherwise.
data StartupRun = StartupRun
  { urlUs :: Integer,
    -- | First 200 on @GET \/\<token\>\/@: "accepting".
    serveUs :: Integer,
    filesUs :: Integer,
    -- | The last of: the shell, its assets, the preferences, the module
    -- tree, the first search, the module page. See 'UiParts'. Over
    -- HTTP only, so a lower bound for a real browser (D-215).
    uiUsableUs :: Integer,
    parts :: UiParts,
    -- | Bytes on the wire for all of "UI usable" (gzipped where the
    -- server gzips).
    uiWireBytes :: Int,
    -- | Was the first search's answer partial?
    searchPartial :: Bool,
    -- | 'Nothing' if the project has no module we parse.
    onDemand :: Maybe OnDemand,
    -- | When the done line came.
    doneUs :: Integer,
    done :: IndexDone,
    startupSpanUs :: Maybe Integer,
    filesSpanUs :: Maybe Integer,
    indexingSpanUs :: Maybe Integer,
    -- | 1 s after the done line; Linux only.
    idleRssBytes :: Maybe Integer,
    hwmBytes :: Maybe Integer,
    loadAvg :: Maybe LoadAvg,
    -- | From the stop signal to exit.
    stopUs :: Integer,
    rts :: RtsStats,
    files :: FileList
  }

-- | When each part of "UI usable" was there. The assets go out right
-- after the shell; the API requests after a look at
-- @api\/indexing\/tree@, all at once, like a browser's requests.
data UiParts = UiParts
  { assetsUs :: Integer,
    prefsUs :: Integer,
    modulesUs :: Integer,
    searchUs :: Integer,
    moduleUs :: Maybe Integer
  }

-- | The module page asked for while indexing.
data OnDemand = OnDemand
  { sample :: Sample,
    -- | From request to the last byte (not since start).
    us :: Integer,
    status :: OnDemandStatus,
    -- | ready + failed from @api\/indexing\/tree@ just before, of total.
    doneBefore :: Int,
    total :: Int
  }

-- | @api\/files\/tree@: how many entries, and where from.
data FileList = FileList
  { count :: Int,
    source :: FileSource
  }

-- | Where the product read the file list from (@api\/files\/tree@'s
-- @source@ field).
data FileSource = FromGit | FromDisk
  deriving (Eq, Show)

-- * Running

-- | One run. The sample is the module page to ask for while indexing.
runStartup :: SessionConfig -> Maybe Sample -> IO (Either RunError StartupRun)
runStartup cfg sample = (>>= finish) <$> withSession cfg (steps sample)
  where
    finish (build, ended) = build ended <$> requireRts ended

-- | What the API part of "UI usable" got back.
data ApiAnswers = ApiAnswers
  { progress :: Progress,
    prefs :: Timed,
    modules :: Timed,
    search :: Timed,
    modulePage :: Maybe Timed,
    files :: Timed
  }

steps :: Maybe Sample -> Session -> IO (Ended -> RtsStats -> StartupRun)
steps sample s = do
  shell <- getApi s Gzip ""
  ok "/" shell
  let assets = assetPaths shell.response.body
  when (null assets) $ failWith "/: no assets in the shell"

  -- The assets go out at once, like a browser's. Next to them, first
  -- a look at indexing (if it's done already, the module page below is
  -- not on demand; this waits for the file list, like every API
  -- route), then the API requests the first page needs.
  (assetTs, api) <-
    runConcurrently $
      (,)
        <$> Concurrently (mapConcurrently (getPath s Gzip) assets)
        <*> Concurrently (apiRequests sample s)

  mapM_ (\(p, t) -> ok p t) (zip assets assetTs)
  mapM_ (\t -> unless (BS.length t.response.body > 0) (failWith "empty asset")) assetTs
  ok "prefs" api.prefs
  _ <- decodeJson "prefs" api.prefs
  ok "modules" api.modules
  either
    (failWith . ("modules: " <>) . show)
    (const (pure ()))
    (parseModuleList api.modules.response.body)
  ok "search" api.search
  answer <-
    either (failWith . ("search: " <>) . show) pure $
      parseSearchAnswer api.search.response.body
  sequence_ (checkModulePage <$> sample <*> api.modulePage)
  ok "files" api.files
  fileList <- checkFilesBody api.files.response.body

  let uiSet =
        [shell, api.prefs, api.modules, api.search] <> assetTs <> maybe [] pure api.modulePage
      assetsUs = maximum (map (.endUs) assetTs)
      parts =
        UiParts
          { assetsUs = assetsUs,
            prefsUs = api.prefs.endUs,
            modulesUs = api.modules.endUs,
            searchUs = api.search.endUs,
            moduleUs = (.endUs) <$> api.modulePage
          }

  doneAt <- awaitDone s
  let onDemand = do
        m <- sample
        t <- api.modulePage
        let status =
              onDemandStatus
                api.progress.finished
                OnDemandTimes {startUs = t.endUs - t.us, doneUs = doneAt.atUs}
        Just
          OnDemand
            { sample = m,
              us = t.us,
              status = status,
              doneBefore = api.progress.doneCount,
              total = api.progress.total
            }
  spans <- traceSpans s ["startup", "files", "indexing"]
  (startupSpan, filesSpan, indexingSpan) <- case spans of
    [a, b, c] -> pure (a, b, c)
    _ -> failWith "trace spans: wrong count"
  -- Idle memory: indexing is over, the server waits.
  threadDelay 1_000_000
  mem <- procMemory s
  load <- getLoadAvg

  pure $ \ended rts ->
    StartupRun
      { urlUs = s.urlUs,
        serveUs = shell.endUs,
        filesUs = api.files.endUs,
        uiUsableUs = maximum (map (.endUs) uiSet),
        parts = parts,
        uiWireBytes = sum (map (.response.wireBytes) uiSet),
        searchPartial = answer.partial,
        onDemand = onDemand,
        doneUs = doneAt.atUs,
        done = doneAt.line,
        startupSpanUs = startupSpan,
        filesSpanUs = filesSpan,
        indexingSpanUs = indexingSpan,
        idleRssBytes = mem.rssBytes,
        hwmBytes = mem.hwmBytes,
        loadAvg = load,
        stopUs = ended.stopUs,
        rts = rts,
        files = fileList
      }
  where
    checkModulePage m t = do
      ok (Text.unpack m.ref) t
      case cborToJson (LBS.fromStrict t.response.body) of
        Left e -> failWith (Text.unpack m.ref <> ": " <> Text.unpack (renderCborJsonError e))
        Right _ -> pure ()

-- | The indexing look, then the API requests in parallel. @api\/prefs@
-- is there because the app waits for it before its first render.
apiRequests :: Maybe Sample -> Session -> IO ApiAnswers
apiRequests sample s = do
  progress <- indexingProgress s
  runConcurrently $
    ApiAnswers progress
      <$> Concurrently (getApi s Gzip "api/prefs")
      <*> Concurrently (getApi s Gzip "api/modules/tree")
      <*> Concurrently (getApi s Gzip (searchPath (NE.head queries)))
      <*> Concurrently (traverse (\m -> getApi s Gzip (modulePath m.ref)) sample)
      <*> Concurrently (getApi s Gzip "api/files/tree")

-- | Fail the run unless it's a 200.
ok :: String -> Timed -> IO ()
ok what t =
  unless (t.response.status == 200) $
    failWith (what <> ": expected 200, got " <> show t.response.status)

decodeJson :: String -> Timed -> IO Aeson.Value
decodeJson what t =
  either (failWith . ((what <> ": ") <>)) pure (Aeson.eitherDecodeStrict t.response.body)

-- | @api\/indexing\/tree@ (D-160).
data Progress = Progress
  { doneCount :: Int,
    total :: Int,
    -- | ready + failed = total.
    finished :: Bool
  }

indexingProgress :: Session -> IO Progress
indexingProgress s = do
  t <- getApi s NoGzip "api/indexing/tree"
  ok "indexing" t
  v <- decodeJson "indexing" t
  either (failWith . ("indexing: " <>)) pure (Aeson.Types.parseEither parser v)
  where
    parser = Aeson.withObject "indexing" $ \o -> do
      total <- o .: "total"
      ready <- o .: "ready"
      failed <- o .: "failed"
      let n = ready + failed
      pure Progress {doneCount = n, total = total, finished = n == total}

checkFilesBody :: ByteString -> IO FileList
checkFilesBody body = case Aeson.eitherDecodeStrict body of
  Left e -> failWith ("files: cannot decode JSON: " <> e)
  Right v -> case Aeson.Types.parseEither parser v of
    Left e -> failWith ("files: " <> e)
    Right r -> pure r
  where
    parser = Aeson.withObject "files" $ \o -> do
      entries <- o .: "entries" :: Aeson.Types.Parser [Aeson.Value]
      srcText <- o .: "source" :: Aeson.Types.Parser Text
      truncated <- o .: "truncated"
      src <- case srcText of
        "git" -> pure FromGit
        "disk" -> pure FromDisk
        other -> fail ("unknown source: " <> Text.unpack other)
      if null entries
        then fail "entries is empty"
        else
          if truncated
            then fail "truncated is true"
            else pure FileList {count = length entries, source = src}
