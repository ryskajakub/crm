{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.Company.UpkeepResource (
  upkeepResource) where

import           Opaleye.RunQuery              (runQuery)

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Reader          (ask)
import           Control.Applicative           (pure, (<*>))

import           Data.Tuple.All                (sel1, sel2, sel3)

import           Rest.Resource                 (Resource, Void, schema, name,
                                               list, mkResourceId)
import qualified Rest.Schema                   as S
import           Rest.Dictionary.Combinators   (jsonO)
import           Rest.Handler                  (ListHandler)

import qualified Crm.Shared.Api                as A
import qualified Crm.Shared.Company            as C
import           Crm.Shared.MyMaybe

import           Crm.Server.Helpers 
import           Crm.Server.Boilerplate        ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler            (mkListing')


companyUpkeepsListing :: ListHandler (IdDependencies' C.CompanyId)
companyUpkeepsListing = mkListing' jsonO $ const $ do
  ((_,conn), companyId) <- ask 
  rows <- liftIO $ runQuery conn (expandedUpkeepsByCompanyQuery $ C.getCompanyId companyId)
  let 
    mappedResults = mapResultsToList 
      sel1
      (\(upkeepCols,_,_,employeeCols) -> let
        upkeep = convert upkeepCols :: UpkeepMapped
        employee = convert employeeCols :: MaybeEmployeeMapped
        employeeInsideMyMaybe = toMyMaybe $ pure (,) <*> sel1 employee <*> sel2 employee
        in (sel1 upkeep, sel3 upkeep, employeeInsideMyMaybe))
      (\(_,upkeepMachine',machineType',_) -> let
        upkeepMachineMapped = convert upkeepMachine' :: UpkeepMachineMapped
        upkeepMachine = sel3 upkeepMachineMapped
        machineType = sel2 (convert machineType' :: MachineTypeMapped)
        machineId = sel2 upkeepMachineMapped
        in (upkeepMachine, machineType, machineId))
      rows
  return $ map (\((upkeepId, upkeep, maybeEmployee), upkeepMachines) -> 
    (upkeepId, upkeep, upkeepMachines, maybeEmployee)) mappedResults

upkeepResource :: Resource (IdDependencies' C.CompanyId) (IdDependencies' C.CompanyId) Void () Void
upkeepResource = mkResourceId {
  name = A.upkeep ,
  schema = S.withListing () $ S.named [] ,
  list = const companyUpkeepsListing }
