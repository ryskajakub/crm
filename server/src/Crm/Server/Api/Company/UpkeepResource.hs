{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.Company.UpkeepResource (
  upkeepResource) where

import           Opaleye.RunQuery                  (runQuery)

import           Control.Lens                      (mapped, over, view, _1, _2)
import           Control.Monad                     (forM)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Reader              (ask)

import           Data.Pool                         (withResource)

import           Rest.Dictionary.Combinators       (jsonO)
import           Rest.Handler                      (Handler, ListHandler)
import           Rest.Resource                     (Resource, Void, get, list,
                                                    mkResourceId, name, schema)
import qualified Rest.Schema                       as S

import qualified Crm.Shared.Api                    as A
import qualified Crm.Shared.Company                as C
import qualified Crm.Shared.Photo                  as P
import qualified Crm.Shared.Upkeep                 as U
import qualified Crm.Shared.UpkeepMachine          as UM

import           Crm.Server.Api.UpkeepResource     (loadNextServiceTypeHint)
import           Crm.Server.Boilerplate            ()
import           Crm.Server.DB
import           Crm.Server.Handler                (mkConstHandler', mkListing')
import           Crm.Server.Helpers
import           Crm.Server.Types

import           Crm.Server.Database.MachineType
import qualified Crm.Server.Database.UpkeepMachine as UMD

import           TupleTH                           (catTuples, proj)


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
    return $ (\((a,b,c),d,e) -> (a,b,c,d,e)) (r, mappedEmployees, photoIds)
  return withEmployeesAndPhotos

newUpkeepData :: Handler (IdDependencies' C.CompanyId)
newUpkeepData = mkConstHandler' jsonO $ do
  ((_, pool), companyId) <- ask
  machines' <- withResource pool $ \connection -> liftIO $
    runQuery connection (machinesQ companyId)
  let machines = map (\(m, mt) -> (m :: MachineRecord, mt :: MachineTypeRecord)) machines'
  machines'' <- withResource pool $ \connection -> loadNextServiceTypeHint machines connection
  return $ map (\(m, mt, nextUpkeepSequence) ->
    (_machinePK m, _machine m, view _2 mt, nextUpkeepSequence)) machines''

upkeepResource :: Resource (IdDependencies' C.CompanyId) (IdDependencies' C.CompanyId) () () Void
upkeepResource = mkResourceId {
  name = A.upkeep ,
  schema = S.withListing () $ S.named [(A.single, S.singleBy . const $ ())] ,
  get = Just newUpkeepData ,
  list = const companyUpkeepsListing }
