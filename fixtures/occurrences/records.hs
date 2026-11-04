{-# LANGUAGE RecordWildCards, NamedFieldPuns, OverloadedRecordDot, DuplicateRecordFields, PatternSynonyms #-}
module Records where

import Data.Foreign (Ext (..))

data Config = Config {port :: Int, host :: String}

data Other = Other {port :: Int}

mk p = Config {port = p, host = "h"}

pun Config {port} = port

wild Config {..} = host <> show port

partial Config {port = p, ..} = (p, host)

fromExt Ext {..} = extField + missing

update c = c {host = "x"}

dup o = o {port = 1}

dot c = c.host <> (.host) c

conPun host = Config {port = 1, host}

w1 Ext {..} = top

top = 1

pattern P {port, pb} = (port, pb)
