{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.Company.UpkeepResource (
  upkeepResource) where

import           Opaleye.RunQuery                  (runQuery)

import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Reader              (ask)
import           Control.Monad                     (forM)
import           Control.Lens                      (view, mapped, over, _1)

import           Data.Pool                         (withResource)

import           Rest.Resource                     (Resource, Void, schema, name, get, 
                                                   list, mkResourceId)
import qualified Rest.Schema                       as S
import           Rest.Dictionary.Combinators       (jsonO)
import           Rest.Handler                      (ListHandler, Handler)

import qualified Crm.Shared.Api                    as A
import qualified Crm.Shared.Company                as C
import qualified Crm.Shared.Upkeep                 as U
import qualified Crm.Shared.UpkeepMachine          as UM
import qualified Crm.Shared.Photo                  as P

import           Crm.Server.Helpers 
import           Crm.Server.Boilerplate            ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler                (mkListing', mkConstHandler')
import           Crm.Server.Api.UpkeepResource     (loadNextServiceTypeHint)

import qualified Crm.Server.Database.UpkeepMachine as UMD
import           Crm.Server.Database.MachineType

import           TupleTH                           (proj, catTuples)


companyUpkeepsListing :: ListHandler (IdDependencies' C.CompanyId)
companyUpkeepsListing = mkListing' jsonO $ const $ do
  ((_, pool), companyId) <- ask
  rows <- liftIO $ withResource pool $ \connection ->
      over (mapped . mapped . _1 . upkeepSuper) (\x -> fmap (U.UpkeepId) (U.getUpkeepId x)) $ runQuery 
    connection (expandedUpkeepsByCompanyQuery companyId)
  let
    mappedResults = mapResultsToList
      fst
      (\(upkeepCols :: UpkeepRow,_,_,_) -> let
        u = view upkeep upkeepCols
        upkeepMarkup = u {
          U.recommendation = parseMarkupOrPlain . U.recommendation $ u ,
          U.workDescription = parseMarkupOrPlain . U.workDescription $ u }
        in (view upkeepPK upkeepCols, upkeepMarkup))
      (\(_, upkeepMachineMapped :: UMD.UpkeepMachineRow, machine' :: MachineRecord, machineType' :: MachineTypeRecord) -> let
        upkeepMachine = view UMD.upkeepMachine upkeepMachineMapped
        upkeepMachineMarkup = upkeepMachine {
          UM.endNote = UM.endNote upkeepMachine ,
          UM.upkeepMachineNote = UM.upkeepMachineNote upkeepMachine }
        machineTypeBody = _machineType machineType'
        machineBody = _machine machine'
        machineId = view UMD.machineFK upkeepMachineMapped
        in (upkeepMachineMarkup, machineBody, machineTypeBody, machineId))
      rows
    flattened = fmap (\((upkeepId, upkeep'), upkeepMachines) ->
      (upkeepId, upkeep', upkeepMachines)) mappedResults
  withEmployeesAndPhotos <- withResource pool $ \connection -> liftIO $ forM flattened $ \(r @ (upkeepId, _, _)) -> do
    employeeResults <- runQuery connection (employeesInUpkeep upkeepId)
    let mappedEmployees = fmap (\row -> convert row) employeeResults :: [EmployeeMapped]
    photoIdsRaw <- runQuery connection (photosInUpkeepQuery upkeepId)
    let photoIds = fmap P.PhotoId photoIdsRaw
    return $ $(catTuples 4 1) ($(catTuples 3 1) r mappedEmployees) photoIds
  return withEmployeesAndPhotos

newUpkeepData :: Handler (IdDependencies' C.CompanyId)
newUpkeepData = mkConstHandler' jsonO $ do
  ((_, pool), companyId) <- ask
  machines' <- withResource pool $ \connection -> liftIO $ 
    runQuery connection (machinesQ companyId)
  let machines = map (\(m, mt) -> (m :: MachineRecord, mt :: MachineTypeRecord)) machines'
  machines'' <- withResource pool $ \connection -> loadNextServiceTypeHint machines connection
  return $ map (\(m, mt, nextUpkeepSequence) -> 
    (_machinePK m, _machine m, $(proj 2 1) mt, nextUpkeepSequence)) machines''

upkeepResource :: Resource (IdDependencies' C.CompanyId) (IdDependencies' C.CompanyId) () () Void
upkeepResource = mkResourceId {
  name = A.upkeep ,
  schema = S.withListing () $ S.named [(A.single, S.singleBy . const $ ())] ,
  get = Just newUpkeepData ,
  list = const companyUpkeepsListing }
