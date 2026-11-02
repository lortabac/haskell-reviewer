-- | The assets the app shell (@index.html@) loads, so "UI usable" can
-- wait for them like a browser would.
module Bench.Shell
  ( -- * Assets
    assetPaths,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.List (nub)

-- | The @src@ and @href@ values under @\/assets\/@, in order, once each.
-- The server already rewrote them to @\/\<token\>\/assets\/\<name\>@
-- (Note [Embedded bundle] in "HaskellReviewer.Server.Assets"). Only
-- what the HTML names: the events worker is loaded by the script, not
-- here.
assetPaths :: ByteString -> [String]
assetPaths html = nub (filter isAsset (attrs "src=\"" html <> attrs "href=\"" html))
  where
    isAsset v = "/assets/" `BS.isInfixOf` Char8.pack v

-- | Every value of one attribute (its opening, e.g. @src="@).
attrs :: ByteString -> ByteString -> [String]
attrs open = go
  where
    go s =
      let (_, rest) = BS.breakSubstring open s
       in if BS.null rest
            then []
            else
              let afterOpen = BS.drop (BS.length open) rest
                  (value, more) = Char8.break (== '"') afterOpen
               in Char8.unpack value : go more
