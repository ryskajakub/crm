{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.Company.MachineResource ( 
  machineResource ) where

import Database.PostgreSQL.Simple (Connection)

import Opaleye.PGTypes (pgInt4, pgString, pgDay, pgBool)
import Opaleye.Manipulation (runInsert, runInsertReturning)
import Opaleye (runQuery)

import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM_)
import Control.Monad.Trans.Except (ExceptT)

import Data.Tuple.All (sel1)

import Rest.Resource (Resource, Void, schema, name, create, mkResourceId, list)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, jsonI)
import Rest.Handler (mkInputHandler, Handler, mkListing, ListHandler)
import Rest.Types.Error (Reason)

import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.MachineKind as MK
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.Compressor as MC
import qualified Crm.Shared.Dryer as MD
import qualified Crm.Shared.ContactPerson as CP
import qualified Crm.Shared.Api as A
import Crm.Shared.MyMaybe (toMaybe)

import Crm.Server.Helpers (withConnId, ymdToDay, maybeToNullable)
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

import TupleTH (proj)

createMachineHandler :: Handler IdDependencies
createMachineHandler = mkInputHandler (jsonO . jsonI) 
    (\(newMachine, machineType, contactPersonId, linkedMachineId, machineSpecificData) -> let
  contactPersonId' = toMaybe contactPersonId
  in withConnId (\connection companyId -> 
    addMachine connection newMachine companyId machineType contactPersonId' (toMaybe linkedMachineId) machineSpecificData))

addMachine :: Connection
           -> M.Machine
           -> Int
           -> MT.MyEither
           -> Maybe CP.ContactPersonId
           -> Maybe M.MachineId
           -> MK.MachineKindEnum
           -> ExceptT (Reason r) IdDependencies Int -- ^ id of newly created machine
addMachine connection machine companyId' machineType contactPersonId linkedMachineId machineSpecificData = do
  machineTypeId <- liftIO $ case machineType of
    MT.MyInt id' -> return $ id'
    MT.MyMachineType (MT.MachineType kind name' manufacturer, upkeepSequences) -> do
      newMachineTypeId <- runInsertReturning
        connection
        machineTypesTable (Nothing, pgInt4 $ MK.kindToDbRepr kind, pgString name', pgString manufacturer)
        sel1
      let machineTypeId = head newMachineTypeId -- todo safe
      forM_ upkeepSequences (\(US.UpkeepSequence displayOrdering label repetition oneTime) -> runInsert
        connection
        upkeepSequencesTable 
        (pgInt4 displayOrdering, pgString label, 
          pgInt4 repetition, pgInt4 machineTypeId, pgBool oneTime))
      return machineTypeId
  let
    M.Machine machineOperationStartDate' initialMileage mileagePerYear note 
      serialNumber yearOfManufacture = machine
  machineIds <- liftIO $ runInsertReturning
    connection
    machinesTable 
    (Nothing, pgInt4 companyId', maybeToNullable $ fmap (pgInt4 . CP.getContactPersonId) contactPersonId, 
      pgInt4 machineTypeId, maybeToNullable $ (pgInt4 . M.getMachineId) `fmap` linkedMachineId,
      maybeToNullable $ fmap (pgDay . ymdToDay) machineOperationStartDate',
      pgInt4 initialMileage, pgInt4 mileagePerYear, pgString note, 
      pgString serialNumber, pgString yearOfManufacture)
    sel1
  let machineId = head machineIds
  return machineId -- todo safe

listing :: ListHandler IdDependencies
listing = mkListing jsonO $ const $ withConnId $ \connection companyId -> do
  otherMachines <- liftIO $ runQuery connection (otherMachinesInCompanyQuery companyId)
  let 
    machinesMapped = convert otherMachines :: [MachineMapped]
    result = fmap (\mm -> ($(proj 6 0) mm,$(proj 6 5) mm)) machinesMapped
  return result

machineResource :: Resource IdDependencies IdDependencies Void () Void
machineResource = mkResourceId {
  name = A.machines ,
  schema = S.withListing () $ S.named [] ,
  list = const listing ,
  create = Just createMachineHandler }
