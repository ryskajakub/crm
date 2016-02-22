{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.MachineResource where

import           Opaleye                     ((.===), (.==), runUpdate, runDelete, runQuery,
                                               pgInt4, pgStrictText, pgDay, pgBool)

import           Data.Maybe                  (catMaybes)
import           Data.Tuple.All              (sel2, sel1, sel3)
import           Data.Pool                   (withResource)

import           Control.Monad               (forM)
import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Arrow               (first, second)
import           Control.Lens                (view)

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
import qualified Crm.Shared.Photo            as P
import qualified Crm.Shared.YearMonthDay     as YMD
import           Crm.Shared.MyMaybe

import           Crm.Server.Helpers 
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import qualified Crm.Server.Database.UpkeepMachine as UMD
import           Crm.Server.Core             (nextServiceDate, getMaybe)
import           Crm.Server.Handler          (mkInputHandler', mkConstHandler', mkListing')
import           Crm.Server.CachedCore       (recomputeWhole)

import qualified Crm.Server.Database.UpkeepSequence as USD
import           Crm.Server.Database.MachineType

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
  liftIO $ withResource pool $ \connection -> do
    _ <- runDelete connection extraFieldsTable (((.==) (pgInt4 . M.getMachineId $ machineId)) . $(proj 3 1))
    _ <- runDelete connection machinesTable (((.===) (fmap pgInt4 machineId)) . _machinePK)
    return ()
  recomputeWhole pool cache


machineUpdate :: Handler (IdDependencies' M.MachineId)
machineUpdate = mkInputHandler' (jsonI . jsonO) $ \(machine', machineTypeId, linkedMachineId, contactPersonId, extraFields) -> do
  ((cache, pool), machineId :: M.MachineId) <- ask

  let 
    unwrappedId = M.getMachineId machineId
    machineReadToWrite machineRow =
      (machineRow {
        _machinePK = fmap (Just . pgInt4) machineId ,
        _machineTypeFK = fmap pgInt4 machineTypeId ,
        _contactPersonFK =
          (maybeToNullable $ (pgInt4 . CP.getContactPersonId) `fmap` toMaybe contactPersonId) ,
        _linkageFK = 
          M.MachineId
            (maybeToNullable $ (pgInt4 . M.getMachineId) `fmap` (toMaybe linkedMachineId)) ,
        _machine = M.Machine {
          M.machineOperationStartDate =
            (maybeToNullable $ fmap (pgDay . YMD.ymdToDay) (M.machineOperationStartDate machine')) ,
          M.initialMileage = pgInt4 . M.initialMileage $ machine' ,
          M.mileagePerYear = pgInt4 . M.mileagePerYear $ machine' ,
          M.label_ = pgStrictText . M.label_ $ machine' ,
          M.serialNumber = pgStrictText . M.serialNumber $ machine' ,
          M.yearOfManufacture = pgStrictText . M.yearOfManufacture $ machine' ,
          M.archived = pgBool . M.archived $ machine' ,
          M.furtherSpecification = pgStrictText . M.furtherSpecification $ machine' ,
          M.upkeepBy = pgInt4 . M.upkeepByEncode . M.upkeepBy $ machine' }})

  _ <- withResource pool $ \connection -> liftIO $ runUpdate connection machinesTable machineReadToWrite 
    (((.===) (fmap pgInt4 machineId)) . _machinePK)
  withResource pool $ \connection -> liftIO $ createDeletion' ($(proj 3 1)) extraFieldsTable unwrappedId connection
  withResource pool $ \connection -> liftIO $ insertExtraFields machineId extraFields connection

  recomputeWhole pool cache

  return ()


machineSingle :: Handler (IdDependencies' M.MachineId)
machineSingle = mkConstHandler' jsonO $ do
  ((_,pool), machineId') <- ask 
  rows <- withResource pool $ \connection -> liftIO $ runQuery connection (machineDetailQuery machineId')
  row @ (_,_,_) <- singleRowOrColumn rows
  let 
    (machineId, machine', companyId, machineTypeId, machineType', contactPersonId, otherMachineId) = let
      (m :: MachineRecord) = sel1 row
      (mt :: MachineTypeRecord) = sel2 row
      cp = convert $ sel3 row :: MaybeContactPersonMapped
      in (_machinePK m, _machine m, _companyFK m, _machineTypePK mt, _machineType mt, toMyMaybe . sel1 $ cp, _linkageFK m)
  upkeepSequenceRows <- withResource pool $ \connection -> liftIO $ 
    runQuery connection (upkeepSequencesByIdQuery machineTypeId)
  upkeepRows <- withResource pool $ \connection -> 
    liftIO $ runQuery connection (upkeepsDataForMachine machineId)
  today' <- liftIO today
  extraFields <- withResource pool $ \connection -> liftIO $ 
    runQuery connection (extraFieldsForMachineQuery machineId)
  let 
    extraFieldsConvert (ef', efs') = let
      ef = convert $ ef' :: ExtraFieldMapped
      efs = convert $ efs' :: ExtraFieldSettingsMapped
      in ($(proj 3 0) ef, snd efs, $(proj 3 2) ef)
    extraFields' = extraFieldsConvert `fmap` extraFields
    upkeepSequences = fmap (\(row' :: USD.UpkeepSequenceRecord) -> USD._upkeepSequence row') upkeepSequenceRows
    upkeepsData = 
          fmap (\x -> $(catTuples 3 1) ($(takeTuple 4 3) x) (catMaybes . $(proj 4 3) $ x))
        . fmap (\(b, c) -> $(catTuples 3 1) b c)
        . mapResultsToList
          $(proj 3 0)
          $(takeTuple 4 3)
          $(proj 4 3)
        . fmap (\(upkeep'', upkeepMachine :: UMD.UpkeepMachineRow, employee') -> let
          employee = convert employee' :: MaybeEmployeeMapped
          intermediate = ((_upkeepPK upkeep'', _upkeep upkeep'', view UMD.upkeepMachine upkeepMachine, $(proj 2 1) employee)
            :: (U.UpkeepId, U.Upkeep, UM.UpkeepMachine, Maybe E.Employee))
          in intermediate)
        $ upkeepRows
      :: [(U.UpkeepId, U.Upkeep, UM.UpkeepMachine, [E.Employee])]
    upkeeps = fmap (second $(proj 4 2) . first $(proj 4 1) . (\a -> (a,a))) upkeepsData
    upkeepSequenceTuple = case upkeepSequences of
      [] -> undefined
      x : xs -> (x,xs)
    nextServiceYmd = getMaybe $ nextServiceDate machine' upkeepSequenceTuple upkeeps today'
  upkeepsDataWithPhotos <- forM upkeepsData $ \data' -> do
    let upkeepId = $(proj 4 0) data'
    photosFromDb <- withResource pool $ \connection -> liftIO $ runQuery connection $ photosInUpkeepQuery upkeepId
    let photoIds = P.PhotoId `fmap` photosFromDb
    return $ $(catTuples 4 1) data' photoIds
  return -- the result needs to be in nested tuples, because there can be max 7-tuple
    ((companyId, machine', machineTypeId, (machineType', upkeepSequences)),
    (toMyMaybe $ YMD.dayToYmd `fmap` nextServiceYmd, contactPersonId, 
      upkeepsDataWithPhotos, otherMachineId, MT.kind machineType', extraFields'))


machineListing :: ListHandler Dependencies
machineListing = mkListing' jsonO (const $
  ask >>= \(_, pool) -> withResource pool $ \connection -> liftIO $ runExpandedMachinesQuery Nothing connection)
