{-# LANGUAGE OverloadedStrings #-}
module Main where

import Snap.Core (route, writeBS, method, Snap, Method(GET))
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
  route [("/api",
    route [("/foo",
      method GET (writeBS "bar")
    )]
  )]
