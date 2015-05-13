{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.Company.UpkeepResource (
  upkeepResource) where

import Database.PostgreSQL.Simple (Connection)

import Opaleye.PGTypes (pgInt4, pgDay, pgBool, pgString)
import Opaleye.Manipulation (runInsertReturning)
import Opaleye.RunQuery (runQuery)

import Control.Monad.IO.Class (liftIO)
import Control.Applicative (pure, (<*>))

import Data.Tuple.All (sel1, sel2, sel3, sel4, upd3)

import Rest.Resource (Resource, Void, schema, name, create, list, get, mkResourceReaderWith)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, jsonI)
import Rest.Handler (mkInputHandler, Handler, ListHandler, mkListing, mkConstHandler)

import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Employee as E
import Crm.Shared.MyMaybe
import Crm.Server.Api.UpkeepResource (insertUpkeepMachines)

import Crm.Server.Helpers 
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

companyUpkeepsListing :: ListHandler IdDependencies
companyUpkeepsListing = mkListing jsonO (const $ withConnId (\conn id'' -> do
  rows <- liftIO $ runQuery conn (expandedUpkeepsByCompanyQuery id'')
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
        machineId = sel1 upkeepMachineMapped
        in (upkeepMachine, machineType, machineId))
      rows
  return $ map (\((upkeepId, upkeep, maybeEmployee), upkeepMachines) -> 
    (upkeepId, upkeep, upkeepMachines, maybeEmployee)) mappedResults ))

getUpkeep :: Handler IdDependencies
getUpkeep = mkConstHandler jsonO $ withConnId (\conn upkeepId -> do
  rows <- liftIO $ runQuery conn $ expandedUpkeepsQuery2 upkeepId
  let result = mapUpkeeps rows
  singleRowOrColumn (map (\x -> (sel2 x, sel3 x, sel4 x)) result))

addUpkeep :: Connection
          -> (U.Upkeep, [(UM.UpkeepMachine, M.MachineId)], Maybe E.EmployeeId)
          -> IO U.UpkeepId -- ^ id of the upkeep
addUpkeep connection (upkeep, upkeepMachines, employeeId) = do
  upkeepIds <- runInsertReturning
    connection
    upkeepsTable (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep,
      pgBool $ U.upkeepClosed upkeep, maybeToNullable $ (pgInt4 . E.getEmployeeId) `fmap` employeeId, 
      pgString $ U.workHours upkeep, pgString $ U.workDescription upkeep, 
      pgString $ U.recommendation upkeep)
    sel1
  let upkeepId = U.UpkeepId $ head upkeepIds
  insertUpkeepMachines connection upkeepId upkeepMachines
  return upkeepId

createUpkeepHandler :: Handler IdDependencies
createUpkeepHandler = mkInputHandler (jsonO . jsonI) (\newUpkeep -> let 
  (_,_,selectedEmployeeId) = newUpkeep
  newUpkeep' = upd3 (toMaybe selectedEmployeeId) newUpkeep
  in withConnId (\connection _ ->
    -- todo check that the machines are belonging to this company
    liftIO $ addUpkeep connection newUpkeep'))

upkeepResource :: Resource IdDependencies IdDependencies UrlId () Void
upkeepResource = (mkResourceReaderWith prepareReaderIdentity) {
  name = A.upkeep ,
  schema = S.withListing () $ S.unnamedSingle readMay' ,
  list = const companyUpkeepsListing ,
  get = Just getUpkeep ,
  create = Just createUpkeepHandler }
