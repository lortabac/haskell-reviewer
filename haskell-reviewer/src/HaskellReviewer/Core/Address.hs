-- | Where the server lives: port, session token, URL, allowed Host
-- values (SPEC §2). Pure.
module HaskellReviewer.Core.Address
  ( -- * Types
    Port (..),
    Token,

    -- * Token
    tokenByteCount,
    tokenFromBytes,
    tokenText,
    tokenMatches,

    -- * URL and Host
    sessionUrl,
    hostAllowed,
  )
where

import Data.ByteArray qualified as ByteArray
import Data.ByteArray.Encoding (Base (Base64URLUnpadded), convertToBase)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Word (Word16)

-- | A TCP port, 1 to 65535.
--
-- The constructor is exported, so the range is not enforced by the type.
-- It is checked where ports come in (the CLI parser); a smart constructor
-- isn't worth it.
newtype Port = Port Word16
  deriving (Eq, Show)

-- | The session token: the first path segment of every URL.
--
-- Invariant: 32 base64url characters (from 24 random bytes). The
-- constructor is hidden: build one with 'tokenFromBytes'.
newtype Token = Token ByteString
  deriving (Eq, Show)

-- | How many random bytes go into a token. 24 bytes give 32 base64url
-- characters, with no padding.
tokenByteCount :: Int
tokenByteCount = 24

-- | Encode random bytes as a token. Pass 'tokenByteCount' bytes, or the
-- token won't be 32 characters long.
tokenFromBytes :: ByteString -> Token
tokenFromBytes bytes = Token (convertToBase Base64URLUnpadded bytes)

-- | The token as it appears in the URL.
tokenText :: Token -> Text
tokenText (Token t) = Text.Encoding.decodeASCII t

-- | Does a path segment match the token? Constant time (for a given
-- length), so timing doesn't leak how many characters were right.
tokenMatches :: Token -> Text -> Bool
tokenMatches (Token t) segment =
  ByteArray.constEq t (Text.Encoding.encodeUtf8 segment)

-- | The URL of the session, with a final slash.
sessionUrl :: Port -> Token -> Text
sessionUrl (Port p) token =
  "http://127.0.0.1:" <> Text.pack (show p) <> "/" <> tokenText token <> "/"

-- | Is this @Host@ header OK? Only @127.0.0.1:\<port\>@ and
-- @localhost:\<port\>@ are, with our port. This blocks DNS rebinding: a
-- page from @evil.com@ that resolves to 127.0.0.1 still sends
-- @Host: evil.com@.
--
-- Host names are case-insensitive, so @LocalHost@ is fine. No header, no
-- port, another port, @[::1]@ or a trailing dot: rejected.
hostAllowed :: Port -> Maybe ByteString -> Bool
hostAllowed (Port p) = \case
  Nothing -> False
  Just host ->
    let h = Char8.map toLower host
     in h == "127.0.0.1:" <> portBytes || h == "localhost:" <> portBytes
  where
    portBytes = Char8.pack (show p)
