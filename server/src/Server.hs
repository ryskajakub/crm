module Server (
  main, api) where

import           Data.IORef                 (newIORef)
import qualified Data.Map                   as M
import           Data.Profunctor            (lmap)

import           Control.Monad.Reader       (ReaderT, runReaderT)
import           Control.Monad.Trans.Except (runExceptT)

import           Network.Wai.Handler.Warp   (run)
import           Rest.Driver.Wai            (apiToApplication)
import           Network.Wai

import           Crm.Server.CachedCore      (recomputeWhole)
import           Crm.Server.Base
import           Crm.Server.Types
import           Crm.Server.DB


main :: IO ()
main = do
  putStrLn "Starting warp server on http://localhost:8000"
  cache' <- newIORef M.empty
  let cache = Cache cache'
  _ <- withConnection $ \c -> runExceptT $ recomputeWhole c cache
  let 
    app = apiToApplication (runDependencies cache) api
    removeApiPrefix = lmap $ \r -> r { pathInfo = tail . pathInfo $ r }
  run 8000 (removeApiPrefix app)


runDependencies :: Cache -> Dependencies a -> IO a
runDependencies cache deps = withConnection $ \c -> runReaderT deps (cache, c)
