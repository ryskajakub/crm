module Server (
  main, api) where

import Data.IORef               (newIORef)

import Control.Monad.Reader     (ReaderT, runReaderT)

import Network.Wai.Handler.Warp (run)
import Rest.Driver.Wai          (apiToApplication)

import Crm.Server.Base
import Crm.Server.Types
import Crm.Server.DB

main :: IO ()
main = do
  putStrLn "Starting warp server on http://localhost:8000"
  cache <- newIORef []
  run 8000 $ apiToApplication (runDependencies $ Cache cache) api

runDependencies :: Cache -> Dependencies a -> IO a
runDependencies cache deps = withConnection $ \c -> runReaderT deps (cache, c)
