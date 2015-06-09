module Main where

import qualified Rest.Gen        as Gen
import qualified Rest.Gen.Config as Gen
import           Crm.Server.Base (api)

main :: IO ()
main = do
  config <- Gen.configFromArgs "crm-gen"
  Gen.generate config "Crm" api [] [] []
