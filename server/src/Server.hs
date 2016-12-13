module Server (
  main, api) where

import           Data.IORef                 (newIORef)
import qualified Data.Map                   as M
import           Data.Profunctor            (lmap)

import           Control.Concurrent.MVar    (takeMVar)
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.Trans.Except (runExceptT)

import           Network.Wai
import           Network.Wai.Handler.Warp   (run)
import           Rest.Driver.Wai            (apiToApplication)

import           Crm.Server.Base
import           Crm.Server.CachedCore      (recomputeWhole')
import           Crm.Server.DB
import           Crm.Server.Types

import           Data.Pool                  (createPool)
import           Database.PostgreSQL.Simple (close)


main :: IO ()
main = do
  putStrLn "Starting warp server on http://localhost:8000"
  pool <- createPsqlConnPool
  cache' <- newIORef M.empty
  let cache = Cache cache'
  daemon <- runExceptT $ recomputeWhole' pool cache
  either (const . return $ ()) takeMVar daemon
  let
    app = apiToApplication (runDependencies pool cache) api
    removeApiPrefix = lmap $ \r -> r { pathInfo = tail . pathInfo $ r }
  run 8000 (removeApiPrefix app)


createPsqlConnPool :: IO ConnectionPool
createPsqlConnPool = createPool
  initiateConnection
  close
  1
  (fromInteger 10)
  10


runDependencies :: ConnectionPool -> Cache -> Dependencies a -> IO a
runDependencies pool cache deps = runReaderT deps (cache, pool)
