module Main (main) where

import Helper (specHelper)
import P.Api (api)
import "p" P.Api qualified as Api
import P.Internal (internal)

main :: IO ()
main = print (api + Api.api + specHelper + internal)
