{-# LANGUAGE PackageImports #-}

module Server (
  main, api
) where

import Crm.Server.Base
import Crm.Server.Types
import Crm.Server.DB

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)

import Snap.Http.Server (quickHttpServe)
import "snap-core" Snap.Core (Snap(..))

import Rest.Driver.Snap (apiToHandler')

runDependencies :: Dependencies a -> Snap a
runDependencies deps = liftIO $ withConnection (\c -> runReaderT deps $ c)

main :: IO ()
main = quickHttpServe $ apiToHandler' runDependencies api
