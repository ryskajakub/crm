{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.CachedCore where

import           Control.Monad              (forM, forM_)
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.MVar    (newEmptyMVar, putMVar, MVar)
import           Control.Concurrent.Async   (async, wait, Async)
import           Data.Maybe                 (mapMaybe)
import           Data.IORef                 (atomicModifyIORef', readIORef)
import           Data.Pool                  (withResource)

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Except (mapExceptT, runExceptT)
import qualified Data.Map                   as Map
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye                    (runQuery, queryTable)
import           TupleTH                    (proj)
import           Control.Monad.Trans.Except (ExceptT)
import           Rest.Types.Error           (Reason)
import           Safe                       (minimumMay)

import qualified Crm.Shared.Company         as C
import qualified Crm.Shared.Machine         as M
import qualified Crm.Shared.YearMonthDay    as YMD

import           Crm.Server.Core            (nextServiceDate, Planned(..))
import           Crm.Server.DB
import           Crm.Server.Helpers         (today, dayToYmd)
import           Crm.Server.Types 


recomputeWhole' ::
  forall r m .
  (MonadIO m) =>
  ConnectionPool -> 
  Cache -> 
  ExceptT (Reason r) m (MVar ())
recomputeWhole' pool (cache @ (Cache c)) = do
  companyRows <- liftIO $ withResource pool $ \connection -> runQuery connection (queryTable companiesTable)
  let companies = convert companyRows :: [CompanyMapped]
  let companyIds = fmap $(proj 3 0) companies
  _ <- liftIO $ atomicModifyIORef' c $ const (Map.empty, ())
  daemon <- liftIO $ newEmptyMVar 
  _ <- liftIO $ forkIO $ do
    liftIO . putStrLn $ "starting cache computation"
    _ <- liftIO $ let
      forkRecomputation :: [C.CompanyId] -> IO (Async (Either (Reason r) [()]))
      forkRecomputation companyIdsBatch = async $ withResource pool $ \connection -> 
        runExceptT $ forM companyIdsBatch $ \companyId -> recomputeSingle companyId connection cache
      batchedCompanyIds = batch companyIds 5
      in do
        computations <- forM batchedCompanyIds forkRecomputation
        forM_ computations $ \computation -> wait computation >> (return ())
    liftIO . putStrLn $ "stopping cache computation"
    putMVar daemon ()  
  return daemon


batch :: [a] -> Int -> [[a]]
batch [] _ = []
batch list batchSize = take batchSize list : batch (drop batchSize list) batchSize


recomputeWhole :: 
  MonadIO m => 
  ConnectionPool -> 
  Cache -> 
  ExceptT (Reason r) m ()
recomputeWhole pool cache = do
  _ <- recomputeWhole' pool cache
  return ()


recomputeSingle :: (MonadIO m)
                => C.CompanyId 
                -> Connection 
                -> Cache 
                -> ExceptT (Reason r) m ()
recomputeSingle companyId connection (Cache cache) = mapExceptT liftIO $ do
  companyRow' <- liftIO $ runQuery connection (companyByIdQuery $ C.getCompanyId companyId)
  companyRow <- singleRowOrColumn companyRow'
  let company = convert companyRow :: CompanyMapped
  machines <- liftIO $ runMachinesInCompanyQuery (C.getCompanyId companyId) connection
  nextDays <- liftIO $ forM machines $ \machine -> do
    (computationMethod, nextServiceDay) <- addNextDates $(proj 7 0) $(proj 7 1) machine connection
    return $ case computationMethod of
      Planned -> Nothing
      Computed -> Just nextServiceDay
  let 
    nextDay = minimumMay $ mapMaybe id nextDays
    value = ($(proj 3 1) company, nextDay, $(proj 3 2) company)
    modify mapCache = Map.insert companyId value mapCache
  _ <- liftIO $ atomicModifyIORef' cache $ \a -> let 
    modifiedA = modify a
    in (modifiedA, ())
  return ()


getCacheContent :: Cache -> IO (Map.Map C.CompanyId (C.Company, Maybe YMD.YearMonthDay, Maybe C.Coordinates))
getCacheContent (Cache cache) = readIORef cache


addNextDates :: (a -> M.MachineId)
             -> (a -> M.Machine)
             -> a
             -> Connection
             -> IO (Planned, YMD.YearMonthDay)
addNextDates getMachineId getMachine a = \conn -> do
  upkeepRows <- runQuery conn (nextServiceUpkeepsQuery $ M.getMachineId $ getMachineId a)
  upkeepSequenceRows <- runQuery conn (nextServiceUpkeepSequencesQuery $ M.getMachineId $ getMachineId a)
  today' <- today
  let
    upkeeps = convert upkeepRows :: [UpkeepMapped] 
    upkeepSequences = fmap (\r -> $(proj 2 1) (convert r :: UpkeepSequenceMapped)) upkeepSequenceRows
    upkeepSequenceTuple = case upkeepSequences of
      [] -> undefined
      x : xs -> (x, xs)
    (nextServiceDay, computationMethod) = nextServiceDate (getMachine a) upkeepSequenceTuple (fmap $(proj 2 1) upkeeps) today'
  return (computationMethod, dayToYmd nextServiceDay)
