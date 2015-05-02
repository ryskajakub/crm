module Server (
  main, api
) where

import Crm.Server.Base
import Crm.Server.Types
import Crm.Server.DB

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ReaderT, runReaderT)

import Network.Wai.Handler.Warp (run)
import Rest.Driver.Wai (apiToApplication)

import Control.Monad.Trans.Except (ExceptT)

main :: IO ()
main = do
  putStrLn "Starting warp server on http://localhost:8000"
  run 8000 $ apiToApplication runDependencies api

runDependencies :: Dependencies a -> IO a
runDependencies deps = withConnection (\c -> runReaderT deps $ c)
