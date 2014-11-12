module Main (main) where

import Rest.Gen.Types (ModuleName(ModuleName))
import Rest.Gen (generate)
import Rest.Gen.Config (configFromArgs)

import Server (api)

main :: IO ()
main = do
  config <- configFromArgs "crm-gen-client"
  generate config "Crm" api [] [] []
