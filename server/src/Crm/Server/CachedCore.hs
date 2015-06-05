{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.CachedCore where

import           Database.PostgreSQL.Simple (Connection)
import           Opaleye                    (runQuery)
import           TupleTH                    (proj)

import qualified Crm.Shared.Company         as C
import qualified Crm.Shared.Machine         as M
import qualified Crm.Shared.YearMonthDay    as YMD

import           Crm.Server.Core            (nextServiceDate, Planned)
import           Crm.Server.DB
import           Crm.Server.Helpers         (today, dayToYmd)
import           Crm.Server.Types 

recompute :: [CoreData] -> Cache -> IO ()
recompute = undefined

recomputeSingle :: C.CompanyId -> Cache -> IO ()
recomputeSingle = undefined

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
    (nextServiceDay, computationMethod) = nextServiceDate (getMachine a) upkeepSequenceTuple (fmap $(proj 3 2) upkeeps) today'
  return (computationMethod, dayToYmd nextServiceDay)
