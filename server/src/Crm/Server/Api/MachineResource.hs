{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Crm.Server.Api.MachineResource where

import           Opaleye                            (pgBool, pgDay, pgInt4,
                                                     pgStrictText, runDelete,
                                                     runQuery, runUpdate, (.==),
                                                     (.===))

import           Data.Maybe                         (catMaybes)
import           Data.Pool                          (withResource)
import           Data.Text                          (Text)
import           Data.Tuple.All                     (sel1, sel2, sel3)

import           Control.Arrow                      (first, second)
import           Control.Lens                       (mapped, over, view, _1, _2,
                                                     _3, _4, _5)
import           Control.Monad                      (forM)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (ask)

import           Rest.Dictionary.Combinators        (jsonI, jsonO)
import           Rest.Handler                       (Handler, ListHandler)
import           Rest.Resource                      (Resource, Void, get, list,
                                                     mkResourceReaderWith, name,
                                                     remove, schema, update)
import qualified Rest.Schema                        as S

import qualified Crm.Shared.Api                     as A
import qualified Crm.Shared.ContactPerson           as CP
import qualified Crm.Shared.Employee                as E
import qualified Crm.Shared.Machine                 as M
import qualified Crm.Shared.MachineType             as MT
import           Crm.Shared.MyMaybe
import qualified Crm.Shared.Photo                   as P
import qualified Crm.Shared.Upkeep                  as U
import qualified Crm.Shared.UpkeepMachine           as UM
import qualified Crm.Shared.YearMonthDay            as YMD

import           Crm.Server.Boilerplate             ()
import           Crm.Server.CachedCore              (recomputeWhole)
import           Crm.Server.Core                    (getMaybe, nextServiceDate)
import qualified Crm.Server.Database.UpkeepMachine  as UMD
import           Crm.Server.DB
import           Crm.Server.Handler                 (mkConstHandler',
                                                     mkInputHandler',
                                                     mkListing')
import           Crm.Server.Helpers
import           Crm.Server.Types

import           Crm.Server.Database.MachineType
import qualified Crm.Server.Database.UpkeepSequence as USD

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
    _ <- runDelete connection extraFieldsTable (((.==) (pgInt4 . M.getMachineId $ machineId)) . view _2)
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
  withResource pool $ \connection -> liftIO $ createDeletion' (view _2) extraFieldsTable unwrappedId connection
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
  upkeepRows :: [(UpkeepRow, UMD.UpkeepMachineRow, (Maybe Int, Maybe Text, Maybe Text, Maybe Text, Maybe Text))] <-
    withResource pool $ \connection ->
      liftIO $ over (mapped . mapped . _1 . upkeepSuper) (\x -> fmap (U.UpkeepId) (U.getUpkeepId x)) $ runQuery connection (upkeepsDataForMachine machineId)
  extraFields <- withResource pool $ \connection -> liftIO $
    runQuery connection (extraFieldsForMachineQuery machineId)
  let
    extraFieldsConvert (ef', efs') = let
      ef = convert $ ef' :: ExtraFieldMapped
      efs = convert $ efs' :: ExtraFieldSettingsMapped
      in (view _1 ef, snd efs, view _2 ef)
    extraFields' = extraFieldsConvert `fmap` extraFields
    upkeepSequences = fmap (\(row' :: USD.UpkeepSequenceRecord) -> USD._upkeepSequence row') upkeepSequenceRows
    upkeepsData =
          fmap (\((a1, a2, a3, a4), b) -> (a1, a2, a3, a4, catMaybes b))
        . mapResultsToList
          (view _1)
          (\(a1, a2, a3, a4, _) -> (a1, a2, a3, a4))
          (view _5)
        . fmap (\(upkeep'', upkeepMachine :: UMD.UpkeepMachineRow, employee') -> let
          employee = convert employee' :: MaybeEmployeeMapped
          intermediate = ((_upkeepPK upkeep'', _upkeepSuper upkeep'' , _upkeep upkeep'', view UMD.upkeepMachine upkeepMachine, view _2 employee)
            :: (U.UpkeepId, Maybe U.UpkeepId, U.Upkeep, UM.UpkeepMachine, Maybe E.Employee))
          in intermediate)
        $ upkeepRows
      :: [(U.UpkeepId, Maybe U.UpkeepId, U.Upkeep, UM.UpkeepMachine, [E.Employee])]
    upkeeps = fmap (second (view _4) . first (view _3) . (\a -> (a,a))) upkeepsData
    upkeepSequenceTuple = case upkeepSequences of
      []     -> undefined
      x : xs -> (x,xs)
    nextServiceYmd = getMaybe $ nextServiceDate machine' upkeepSequenceTuple upkeeps
  upkeepsDataWithPhotos <- forM upkeepsData $ \data' -> do
    let upkeepId = view _1 data'
    photosFromDb <- withResource pool $ \connection -> liftIO $ runQuery connection $ photosInUpkeepQuery upkeepId
    let photoIds = P.PhotoId `fmap` photosFromDb
    return $ (\((a1, a2, a3, a4, a5),pId) -> (a1, toMyMaybe a2, a3, a4, a5, pId)) (data', photoIds)
  return -- the result needs to be in nested tuples, because there can be max 7-tuple
    ((companyId, machine', machineTypeId, (machineType', upkeepSequences)),
    (toMyMaybe $ YMD.dayToYmd `fmap` nextServiceYmd, contactPersonId,
      upkeepsDataWithPhotos, otherMachineId, MT.kind machineType', extraFields'))


machineListing :: ListHandler Dependencies
machineListing = mkListing' jsonO (const $
  ask >>= \(_, pool) -> withResource pool $ \connection -> liftIO $ runExpandedMachinesQuery Nothing connection)
