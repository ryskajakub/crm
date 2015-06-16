{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.Company.MachineResource ( 
  machineResource ) where

import           Control.Monad               (forM_)

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Except  (ExceptT)
import           Control.Monad.Reader        (ask)
import           Data.Tuple.All              (sel1)
import           Data.Text                   (Text)
import           Database.PostgreSQL.Simple  (Connection)
import           Opaleye.PGTypes             (pgInt4, pgStrictText, pgDay, pgBool)
import           Opaleye.Manipulation        (runInsert, runInsertReturning)
import           Opaleye                     (runQuery)
import           Rest.Resource               (Resource, Void, schema, name, 
                                             create, mkResourceId, list)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (Handler, ListHandler)
import           Rest.Types.Error            (Reason)

import qualified Crm.Shared.UpkeepSequence   as US
import qualified Crm.Shared.MachineType      as MT
import qualified Crm.Shared.MachineKind      as MK
import qualified Crm.Shared.Machine          as M
import qualified Crm.Shared.ContactPerson    as CP
import qualified Crm.Shared.Company          as C
import qualified Crm.Shared.ExtraField       as EF
import qualified Crm.Shared.Api              as A
import           Crm.Shared.MyMaybe          (toMaybe)

import           Crm.Server.Helpers          (ymdToDay, maybeToNullable)
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler          (mkInputHandler', mkListing')
import           Crm.Server.CachedCore       (recomputeSingle)

import TupleTH (proj)


createMachineHandler :: Handler (IdDependencies' C.CompanyId)
createMachineHandler = mkInputHandler' (jsonO . jsonI) $
    \(newMachine, machineType, contactPersonIdentification', linkedMachineId, machineSpecificData) -> do
  ((cache, connection), companyId) <- ask
  let contactPersonIdentification = toMaybe contactPersonIdentification'
  contactPersonId' <- case contactPersonIdentification of
    Just (M.ContactPersonId contactPersonId) -> return . Just $ contactPersonId
    Just (M.ContactPerson contactPerson) -> do
      contactPersonNewIds <- liftIO $ runInsertReturning connection contactPersonsTable
        (Nothing, pgInt4 . C.getCompanyId $ companyId, pgStrictText . CP.name $ contactPerson,
          pgStrictText . CP.name $ contactPerson, pgStrictText . CP.name $ contactPerson)
        $(proj 5 0)
      contactPersonNewId <- singleRowOrColumn contactPersonNewIds
      return . Just . CP.ContactPersonId $ contactPersonNewId
    Nothing -> return Nothing
  machineId <- addMachine connection newMachine (C.getCompanyId companyId) machineType 
    contactPersonId' (toMaybe linkedMachineId) machineSpecificData
  recomputeSingle companyId connection cache
  return $ M.MachineId machineId
    

addMachine :: Connection
           -> M.Machine
           -> Int
           -> MT.MyEither
           -> Maybe CP.ContactPersonId
           -> Maybe M.MachineId
           -> [(EF.ExtraFieldId, Text)]
           -> ExceptT (Reason r) (IdDependencies' C.CompanyId) Int -- ^ id of newly created machine
addMachine connection machine companyId' machineType contactPersonId linkedMachineId extraFields = do
  machineTypeId <- liftIO $ case machineType of
    MT.MyInt id' -> return $ id'
    MT.MyMachineType (MT.MachineType kind name' manufacturer, upkeepSequences) -> do
      newMachineTypeId <- runInsertReturning
        connection
        machineTypesTable (Nothing, pgInt4 $ MK.kindToDbRepr kind, pgStrictText name', pgStrictText manufacturer)
        sel1
      let machineTypeId = head newMachineTypeId -- todo safe
      forM_ upkeepSequences (\(US.UpkeepSequence displayOrdering label repetition oneTime) -> runInsert
        connection
        upkeepSequencesTable 
        (pgInt4 displayOrdering, pgStrictText label, 
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
      pgInt4 initialMileage, pgInt4 mileagePerYear, pgStrictText note, 
      pgStrictText serialNumber, pgStrictText yearOfManufacture)
    sel1
  let machineId = head machineIds -- todo safe
  liftIO $ insertExtraFields (M.MachineId machineId) extraFields connection
  return machineId 

listing :: ListHandler (IdDependencies' C.CompanyId)
listing = mkListing' jsonO $ const $ do
  ((_, connection), companyId) <- ask
  otherMachines <- liftIO $ runQuery connection (otherMachinesInCompanyQuery $ C.getCompanyId companyId)
  let 
    machinesMapped = convert otherMachines :: [MachineMapped]
    result = fmap (\mm -> ($(proj 6 0) mm,$(proj 6 5) mm)) machinesMapped
  return result

machineResource :: Resource (IdDependencies' C.CompanyId) (IdDependencies' C.CompanyId) Void () Void
machineResource = mkResourceId {
  name = A.machines ,
  schema = S.withListing () $ S.named [] ,
  list = const listing ,
  create = Just createMachineHandler }
