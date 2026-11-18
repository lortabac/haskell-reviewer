-- | Git blob ids, computed here: the id git gives some bytes, as
-- @git hash-object --no-filters@ does. For the working-tree side
-- (D-311, D-318).
module HaskellReviewer.Core.BlobId
  ( blobId,
    emptyBlobId,
  )
where

import Crypto.Hash (Digest, SHA1, SHA256, hashFinalize, hashInit, hashUpdates)
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Text.Encoding qualified as Text.Encoding
import HaskellReviewer.Core.Git (ObjectFormat (..), ObjectId (..))

-- | The id of a blob with these bytes: the hash of
-- @blob \<size>\\0\<bytes>@, in lowercase hex.
blobId :: ObjectFormat -> ByteString -> ObjectId
blobId format bytes = case format of
  Sha1 -> hex (hashFinalize (hashUpdates hashInit chunks) :: Digest SHA1)
  Sha256 -> hex (hashFinalize (hashUpdates hashInit chunks) :: Digest SHA256)
  where
    chunks = [header, bytes]
    header = "blob " <> Char8.pack (show (BS.length bytes)) <> "\0"
    hex d = ObjectId (Text.Encoding.decodeLatin1 (convertToBase Base16 d))

-- | The id of the empty blob. git keeps a recorded size of 0 for it,
-- so a 0 there is not a sign of a smudged entry.
emptyBlobId :: ObjectFormat -> ObjectId
emptyBlobId format = blobId format ""
