{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core (route, writeBS, method, Snap, Method(GET), putResponse, emptyResponse)
import Snap.Http.Server (quickHttpServe)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  route [("/api",
    route [("/company/new", do 
      liftIO $ putStrLn "AHOJ" 
      method GET $ putResponse emptyResponse
    )]
  )]
