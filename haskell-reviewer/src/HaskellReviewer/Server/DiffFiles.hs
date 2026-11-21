-- | GET api\/diff-files: the changed files of a diff session (D-325).
module HaskellReviewer.Server.DiffFiles
  ( diffFilesApp,
  )
where

import HaskellReviewer.Server.Api (json, serverError)
import HaskellReviewer.Server.Security (methodNotAllowed, notFound)
import HaskellReviewer.Session.Diff
  ( DiffProblem (..),
    DiffReady (..),
    DiffSlot,
    awaitDiff,
  )
import Network.HTTP.Types (methodGet)
import Network.Wai (Application, Request (..))

-- | The method first, then the mode: browse mode ('Nothing') is a plain
-- 404. Waits for the pairing.
diffFilesApp :: Maybe DiffSlot -> Application
diffFilesApp slot req respond
  | req.requestMethod /= methodGet = respond (methodNotAllowed "GET")
  | Just s <- slot =
      awaitDiff s >>= \case
        Right ready -> respond (json ready.body)
        Left DiffNotListed -> respond (serverError "cannot list files")
        Left (DiffNotPaired _) -> respond (serverError "cannot pair files")
  | otherwise = respond notFound
