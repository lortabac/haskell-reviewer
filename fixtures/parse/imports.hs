{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ExplicitLevelImports #-}

module Imports {-# WARNING in "x-unstable" ["Not stable", "Really"] #-} where

import Data.List
import qualified Data.Map as M
import Data.Set qualified as S
import Data.Maybe (fromMaybe, Maybe (..), (<|>))
import Data.Char hiding (isSpace, ord)
import {-# SOURCE #-} Loop (Loop)
import safe Data.Bits
import "base" Data.Word (Word8)
import splice Language.Haskell.TH (Q)
import Control.Monad quote (when)
import qualified "text" Data.Text as T (Text, pack)
import Data.Function ()
