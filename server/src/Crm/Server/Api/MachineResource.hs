{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.MachineResource where

import           Opaleye.RunQuery            (runQuery)
import           Opaleye.PGTypes             (pgInt4, pgStrictText, pgDay, pgBool)

import           Data.Tuple.All              (sel2, sel1, sel3)
import           Data.Pool                   (withResource)

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
import qualified Crm.Shared.Upkeep           as U
import qualified Crm.Shared.UpkeepMachine    as UM
import qualified Crm.Shared.Employee         as E
import           Crm.Shared.MyMaybe

import           Crm.Server.Helpers 
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Core             (nextServiceDate, getMaybe)
import           Crm.Server.Handler          (mkInputHandler', mkConstHandler', mkListing')
import           Crm.Server.CachedCore       (recomputeWhole)

import           TupleTH


machineResource :: Resource Dependencies (IdDependencies' M.MachineId) M.MachineId () Void
machineResource = (mkResourceReaderWith prepareReaderTuple) {
  list = const machineListing ,
  get = Just machineSingle ,
  update = Just machineUpdate ,
  name = A.machines ,
  remove = Just machineDelete ,
  schema = S.withListing () $ S.unnamedSingleRead id }

    
machineDelete :: Handler (IdDependencies' M.MachineId)
machineDelete = mkConstHandler' jsonO $ do
  ((cache, pool), machineId) <- ask
  liftIO $ withResource pool $ \connection ->
    createDeletion machinesTable (M.getMachineId machineId) connection
  recomputeWhole pool cache


machineUpdate :: Handler (IdDependencies' M.MachineId)
machineUpdate = mkInputHandler' (jsonI . jsonO) $ \(machine', linkedMachineId, contactPersonId, extraFields) -> do
  ((cache, pool), machineId) <- ask

  let 
    unwrappedId = M.getMachineId machineId
    machineReadToWrite (mId,companyId,_,machineTypeId,_,_,_,_,_,_,_,_,_) =
      (Just mId, companyId, maybeToNullable $ (pgInt4 . CP.getContactPersonId) `fmap` toMaybe contactPersonId, 
        machineTypeId, maybeToNullable $ (pgInt4 . M.getMachineId) `fmap` (toMaybe linkedMachineId),
        maybeToNullable $ fmap (pgDay . ymdToDay) (M.machineOperationStartDate machine'),
        pgInt4 $ M.initialMileage machine', pgInt4 . M.mileagePerYear $ machine', 
        pgStrictText . M.label_ $ machine', pgStrictText . M.serialNumber $ machine',
        pgStrictText . M.yearOfManufacture $ machine', pgBool . M.archived $ machine', 
        pgStrictText . M.note $ machine')
    updateMachine = prepareUpdate machinesTable machineReadToWrite

  withResource pool $ \connection -> liftIO $ forM_ [updateMachine] $ \updation -> updation unwrappedId connection
  withResource pool $ \connection -> liftIO $ createDeletion' ($(proj 3 1)) extraFieldsTable unwrappedId connection
  withResource pool $ \connection -> liftIO $ insertExtraFields machineId extraFields connection

  recomputeWhole pool cache

  return ()


machineSingle :: Handler (IdDependencies' M.MachineId)
machineSingle = mkConstHandler' jsonO $ do
  ((_,pool), machineId') <- ask 
  rows <- withResource pool $ \connection -> liftIO $ runQuery connection (machineDetailQuery $ M.getMachineId machineId')
  row @ (_,_,_) <- singleRowOrColumn rows
  let 
    (machineId, machine, companyId, machineTypeId, machineType, contactPersonId, otherMachineId) = let
      m = convert $ sel1 row :: MachineMapped
      mt = convert $ sel2 row :: MachineTypeMapped
      cp = convert $ sel3 row :: MaybeContactPersonMapped
      in (sel1 m, $(proj 6 5) m, sel2 m, sel1 mt, sel2 mt, toMyMaybe $ sel1 cp, toMyMaybe $ $(proj 6 4) m)
  upkeepSequenceRows <- withResource pool $ \connection -> liftIO $ 
    runQuery connection (upkeepSequencesByIdQuery $ pgInt4 $ MT.getMachineTypeId machineTypeId)
  upkeepRows <- withResource pool $ \connection -> 
    liftIO $ runQuery connection (upkeepsDataForMachine $ M.getMachineId machineId)
  today' <- liftIO today
  extraFields <- withResource pool $ \connection -> liftIO $ 
    runQuery connection (extraFieldsForMachineQuery $ M.getMachineId machineId)
  let 
    extraFieldsConvert (ef', efs') = let
      ef = convert $ ef' :: ExtraFieldMapped
      efs = convert $ efs' :: ExtraFieldSettingsMapped
      in ($(proj 3 0) ef, snd efs, $(proj 3 2) ef)
    extraFields' = extraFieldsConvert `fmap` extraFields
    upkeepSequences = fmap (\row' -> sel2 $ (convert row' :: UpkeepSequenceMapped)) upkeepSequenceRows
    upkeepsData = 
        (fmap (\(b, c) -> $(catTuples 3 1) b c)
        . mapResultsToList
          $(proj 3 0)
          $(takeTuple 4 3)
          $(proj 4 3)
        . fmap (\(upkeep', upkeepMachine', employee') -> let
          upkeep = convert upkeep' :: UpkeepMapped
          upkeepMachine = convert upkeepMachine' :: UpkeepMachineMapped
          employee = convert employee' :: EmployeeMapped
          intermediate = (($(proj 2 0) upkeep, $(proj 2 1) upkeep, $(proj 3 2) upkeepMachine, $(proj 2 1) employee)
            :: (U.UpkeepId, U.Upkeep, UM.UpkeepMachine, E.Employee))
          in intermediate)
        $ upkeepRows)
      :: [(U.UpkeepId, U.Upkeep, UM.UpkeepMachine, [E.Employee])]
    upkeeps = fmap $(proj 4 1) upkeepsData
    upkeepSequenceTuple = case upkeepSequences of
      [] -> undefined
      x : xs -> (x,xs)
    nextServiceYmd = getMaybe $ nextServiceDate machine upkeepSequenceTuple upkeeps today'
  return -- the result needs to be in nested tuples, because there can be max 7-tuple
    ((companyId, machine, machineTypeId, (machineType, upkeepSequences)),
    (dayToYmd `fmap` nextServiceYmd, contactPersonId, upkeepsData, otherMachineId, MT.kind machineType, extraFields'))


machineListing :: ListHandler Dependencies
machineListing = mkListing' (jsonO) (const $
  ask >>= \(_, pool) -> withResource pool $ \connection -> liftIO $ runExpandedMachinesQuery Nothing connection)
