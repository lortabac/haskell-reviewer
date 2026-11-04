{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module TH where

import Language.Haskell.TH

helper = 1

$(pure [])

quoted = [| helper + 1 |]

names = ('helper, ''Int)

spliced = $(let local = helper in [| local |])

qq = [expr|1 + 2|]

typed = $$(liftTyped helper)

decls = [d| inner = 1; other = inner |]
