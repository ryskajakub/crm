{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.MachineResource where

import Opaleye.RunQuery (runQuery)
import Opaleye.PGTypes (pgInt4, PGInt4, pgString, pgDay)
import Opaleye.Table (Table)
import Opaleye.Column (Column)

import Data.Tuple.All (sel2, sel1, sel3, sel5)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (liftA3)
import Control.Monad (forM_)

import Rest.Resource (Resource, Void, schema, list, name, mkResourceReaderWith, get, update, remove)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, jsonI)
import Rest.Handler (ListHandler, mkListing, Handler, mkConstHandler, mkInputHandler)

import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.MachineKind as MK
import qualified Crm.Shared.Compressor as MC
import qualified Crm.Shared.Dryer as MD
import Crm.Shared.MyMaybe

import Crm.Server.Helpers (prepareReaderTuple, readMay', dayToYmd, today, deleteRows',
  withConnId, updateRows, ymdToDay, maybeToNullable, createDeletion, prepareUpdate)
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB
import Crm.Server.Core (nextServiceDate)

machineResource :: Resource Dependencies IdDependencies UrlId () Void
machineResource = (mkResourceReaderWith prepareReaderTuple) {
  list = const machineListing ,
  get = Just machineSingle ,
  update = Just machineUpdate ,
  name = A.machines ,
  remove = Just machineDelete ,
  schema = S.withListing () (S.unnamedSingle readMay') }
    
machineDelete :: Handler IdDependencies
machineDelete = deleteRows' [createDeletion dryersTable, createDeletion compressorsTable, createDeletion machinesTable]

machineUpdate :: Handler IdDependencies
machineUpdate = mkInputHandler (jsonI . jsonO) (\(machine', machineSpecificData) -> withConnId (\conn recordId -> do
  let 

    machineReadToWrite (_,companyId,contactPersonId,machineTypeId,_,_,_,_,_,_) =
      (Nothing, companyId, contactPersonId, machineTypeId,
        maybeToNullable $ fmap (pgDay . ymdToDay) (M.machineOperationStartDate machine'),
        pgInt4 $ M.initialMileage machine', pgInt4 $ M.mileagePerYear machine', 
        pgString $ M.note machine', pgString $ M.serialNumber machine',
        pgString $ M.yearOfManufacture machine')
    updateMachine = prepareUpdate machinesTable machineReadToWrite

    updateMachineSpecifictData = case machineSpecificData of
      MK.CompressorSpecific compressor -> prepareUpdate compressorsTable compressorsReadToWrite where
        compressorsReadToWrite = const (pgInt4 recordId, pgString $ MC.note compressor)
      MK.DryerSpecific dryer -> prepareUpdate dryersTable dryersReadToWrite where
        dryersReadToWrite = const (pgInt4 recordId, pgString $ MD.note dryer)

  liftIO $ forM_ [updateMachine, updateMachineSpecifictData] (\updation -> updation recordId conn)
  return ()))

machineSingle :: Handler IdDependencies
machineSingle = mkConstHandler jsonO $ withConnId (\conn id'' -> do
  rows <- liftIO $ runQuery conn (machineDetailQuery id'')
  row @ (_,_,_) <- singleRowOrColumn rows
  let 
    (machineId, machine, companyId, machineTypeId, machineType, contactPersonId) = let
      m = convert $ sel1 row :: MachineMapped
      mt = convert $ sel2 row :: MachineTypeMapped
      cp = convert $ sel3 row :: MaybeContactPersonMapped
      in (sel1 m, sel5 m, sel2 m, sel1 mt, sel2 mt, toMyMaybe $ sel1 cp)
  machineSpecificData <- case (machineSpecificQuery (MT.kind machineType) machineId) of
    Left compressorQuery -> do
      compressorRows <- liftIO $ runQuery conn compressorQuery
      compressorRow <- singleRowOrColumn compressorRows
      let compressorMapped = convert compressorRow :: CompressorMapped
      return $ MK.CompressorSpecific $ sel2 compressorMapped
    Right dryerQuery -> do
      dryerRows <- liftIO $ runQuery conn dryerQuery
      dryerRow <- singleRowOrColumn dryerRows
      let dryerMapped = convert dryerRow :: DryerMapped
      return $ MK.DryerSpecific $ sel2 dryerMapped
  upkeepSequenceRows <- liftIO $ runQuery conn (upkeepSequencesByIdQuery $ pgInt4 machineTypeId)
  upkeepRows <- liftIO $ runQuery conn (upkeepsDataForMachine machineId)
  today' <- liftIO today
  let 
    upkeepSequences = fmap (\row' -> sel2 $ (convert row' :: UpkeepSequenceMapped)) upkeepSequenceRows
    upkeepsData = fmap (\(((uId::Int,a,b,_::(Maybe Int),c,d,e),
        (_::Int,f,_::Int,g,h)),(_::(Maybe Int),eName::Maybe String,eC::Maybe String,eCap::Maybe String)) -> let
      maybeEmployee = liftA3 E.Employee eName eC eCap
      upkeep = U.Upkeep (dayToYmd a) b c d e
      upkeepMachine = UM.UpkeepMachine f g h
      in (uId, upkeep, upkeepMachine, toMyMaybe maybeEmployee)) upkeepRows
    upkeeps = fmap sel2 upkeepsData
    upkeepSequenceTuple = case upkeepSequences of
      [] -> undefined
      x : xs -> (x,xs)
    nextServiceYmd = nextServiceDate machine upkeepSequenceTuple upkeeps today'
  return -- the result needs to be in nested tuples, because there can be max 7-tuple
    ((companyId, machine, machineTypeId, (machineType, upkeepSequences)),
    (dayToYmd $ nextServiceYmd, contactPersonId, upkeepsData, machineSpecificData)))

machineListing :: ListHandler Dependencies
machineListing = mkListing (jsonO) (const $ do
  ask >>= \conn -> liftIO $ runExpandedMachinesQuery Nothing conn)
