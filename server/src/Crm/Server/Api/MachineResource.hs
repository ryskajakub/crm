{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.MachineResource where

import           Opaleye.RunQuery            (runQuery)
import           Opaleye.PGTypes             (pgInt4, pgStrictText, pgDay)

import           Data.Tuple.All              (sel2, sel1, sel3)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad               (forM_)

import           Rest.Resource               (Resource, Void, schema, list, name, 
                                             mkResourceReaderWith, get, update, remove)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (ListHandler, Handler)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Machine          as M
import qualified Crm.Shared.MachineType      as MT
import qualified Crm.Shared.ContactPerson    as CP
import           Crm.Shared.MyMaybe

import           Crm.Server.Helpers 
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Core             (nextServiceDate)
import           Crm.Server.Handler          (mkInputHandler', mkConstHandler', mkListing')

import           TupleTH


machineResource :: Resource Dependencies IdDependencies UrlId () Void
machineResource = (mkResourceReaderWith prepareReaderTuple) {
  list = const machineListing ,
  get = Just machineSingle ,
  update = Just machineUpdate ,
  name = A.machines ,
  remove = Just machineDelete ,
  schema = S.withListing () (S.unnamedSingle readMay') }
    
machineDelete :: Handler IdDependencies
machineDelete = deleteRows' [createDeletion machinesTable]

machineUpdate :: Handler IdDependencies
machineUpdate = mkInputHandler' (jsonI . jsonO) $ \(machine', linkedMachineId, contactPersonId, extraFields) -> 
    withConnId $ \conn recordId -> do

  let 
    machineReadToWrite (_,companyId,_,machineTypeId,_,_,_,_,_,_,_) =
      (Nothing, companyId, maybeToNullable $ (pgInt4 . CP.getContactPersonId) `fmap` toMaybe contactPersonId, 
        machineTypeId, maybeToNullable $ (pgInt4 . M.getMachineId) `fmap` (toMaybe linkedMachineId),
        maybeToNullable $ fmap (pgDay . ymdToDay) (M.machineOperationStartDate machine'),
        pgInt4 $ M.initialMileage machine', pgInt4 $ M.mileagePerYear machine', 
        pgStrictText $ M.note machine', pgStrictText $ M.serialNumber machine',
        pgStrictText $ M.yearOfManufacture machine')
    updateMachine = prepareUpdate machinesTable machineReadToWrite

  liftIO $ forM_ [updateMachine] (\updation -> updation recordId conn)
  liftIO $ createDeletion' ($(proj 3 1)) extraFieldsTable recordId conn
  liftIO $ insertExtraFields (M.MachineId recordId) extraFields conn

  return ()

machineSingle :: Handler IdDependencies
machineSingle = mkConstHandler' jsonO $ withConnId $ \conn id'' -> do
  rows <- liftIO $ runQuery conn (machineDetailQuery id'')
  row @ (_,_,_) <- singleRowOrColumn rows
  let 
    (machineId, machine, companyId, machineTypeId, machineType, contactPersonId, otherMachineId) = let
      m = convert $ sel1 row :: MachineMapped
      mt = convert $ sel2 row :: MachineTypeMapped
      cp = convert $ sel3 row :: MaybeContactPersonMapped
      in (sel1 m, $(proj 6 5) m, sel2 m, sel1 mt, sel2 mt, toMyMaybe $ sel1 cp, toMyMaybe $ $(proj 6 4) m)
  upkeepSequenceRows <- liftIO $ runQuery conn (upkeepSequencesByIdQuery $ pgInt4 $ MT.getMachineTypeId machineTypeId)
  upkeepRows <- liftIO $ runQuery conn (upkeepsDataForMachine $ M.getMachineId machineId)
  today' <- liftIO today
  extraFields <- liftIO $ runQuery conn (extraFieldsForMachineQuery $ M.getMachineId machineId)
  let 
    extraFieldsConvert (ef', efs') = let
      ef = convert $ ef' :: ExtraFieldMapped
      efs = convert $ efs' :: ExtraFieldSettingsMapped
      in ($(proj 3 0) ef, snd efs, $(proj 3 2) ef)
    extraFields' = extraFieldsConvert `fmap` extraFields
    upkeepSequences = fmap (\row' -> sel2 $ (convert row' :: UpkeepSequenceMapped)) upkeepSequenceRows
    upkeepsData = fmap (\((upkeep', upkeepMachine'), maybeEmployee') -> let
      maybeEmployee = convert maybeEmployee' :: MaybeEmployeeMapped
      upkeep = convert upkeep' :: UpkeepMapped
      upkeepMachine = convert upkeepMachine' :: UpkeepMachineMapped
      in (sel1 upkeep, sel3 upkeep, sel3 upkeepMachine, toMyMaybe $ sel2 maybeEmployee)) upkeepRows
    upkeeps = fmap sel2 upkeepsData
    upkeepSequenceTuple = case upkeepSequences of
      [] -> undefined
      x : xs -> (x,xs)
    nextServiceYmd = fst $ nextServiceDate machine upkeepSequenceTuple upkeeps today'
  return -- the result needs to be in nested tuples, because there can be max 7-tuple
    ((companyId, machine, machineTypeId, (machineType, upkeepSequences)),
    (dayToYmd $ nextServiceYmd, contactPersonId, upkeepsData, otherMachineId, MT.kind machineType, extraFields'))

machineListing :: ListHandler Dependencies
machineListing = mkListing' (jsonO) (const $ do
  ask >>= \(_,conn) -> liftIO $ runExpandedMachinesQuery Nothing conn)
