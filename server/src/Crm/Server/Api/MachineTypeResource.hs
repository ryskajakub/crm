{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.MachineTypeResource (
  machineTypeResource) where

import           Opaleye.Manipulation               (runDelete, runInsert,
                                                     runUpdate)
import           Opaleye.Operators                  ((.===))
import           Opaleye.PGTypes                    (pgBool, pgInt4,
                                                     pgStrictText)
import           Opaleye.RunQuery                   (runQuery)

import           Control.Lens                       (over)
import           Control.Monad                      (forM_)
import           Control.Monad.Error.Class          (throwError)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (ask)

import           Data.Int                           (Int64)
import           Data.Pool                          (withResource)
import           Data.Text                          (Text)

import           Rest.Dictionary.Combinators        (jsonI, jsonO)
import           Rest.Handler                       (Handler, ListHandler)
import           Rest.Resource                      (Resource, Void, get, list,
                                                     mkResourceReaderWith, name,
                                                     remove, schema, update)
import qualified Rest.Schema                        as S
import           Rest.Types.Error                   (Reason (NotFound, UnsupportedRoute))

import qualified Crm.Shared.Api                     as A
import qualified Crm.Shared.MachineKind             as MK
import qualified Crm.Shared.MachineType             as MT
import           Crm.Shared.MyMaybe
import qualified Crm.Shared.UpkeepSequence          as US

import           Crm.Server.Boilerplate             ()
import           Crm.Server.CachedCore              (recomputeWhole)
import           Crm.Server.DB
import           Crm.Server.Handler                 (mkConstHandler',
                                                     mkInputHandler',
                                                     mkListing')
import           Crm.Server.Helpers                 (prepareReaderTuple)
import           Crm.Server.Types

import           Crm.Server.Database.MachineType
import           Crm.Server.Database.UpkeepSequence
import qualified Crm.Server.Database.UpkeepSequence as USD


machineTypeResource ::
  Resource Dependencies MachineTypeDependencies MachineTypeSid MachineTypeMid Void
machineTypeResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.machineTypes ,
  list = machineTypesListing ,
  update = Just updateMachineType ,
  get = Just machineTypesSingle ,
  remove = Just removeHandler ,
  schema = autocompleteSchema }

removeHandler :: Handler MachineTypeDependencies
removeHandler = mkConstHandler' jsonO $ do
  ((_, pool), machineTypeSid) <- ask
  case machineTypeSid of
    MachineTypeById (machineTypeId) ->
      liftIO $ withResource pool $ \connection -> do
        _ <- runDelete
          connection
          upkeepSequencesTable
          $ \row -> USD._machineTypeFK row .=== fmap pgInt4 machineTypeId
        _ <- runDelete
          connection
          machineTypesTable
          $ \row -> _machineTypePK row .=== fmap pgInt4 machineTypeId
        return ()
    _ -> return ()

machineTypesListing :: MachineTypeMid -> ListHandler Dependencies
machineTypesListing (Autocomplete mid) = mkListing' jsonO $ const $
  ask >>= \(_,pool) -> withResource pool $ \connection -> liftIO $ runMachineTypesQuery' mid connection
machineTypesListing (AutocompleteManufacturer mid) = mkListing' jsonO $ const $
  ask >>= \(_,pool) -> withResource pool $ \connection ->
    liftIO $ ((runQuery connection (machineManufacturersQuery mid)) :: IO [Text])
machineTypesListing CountListing = mkListing' jsonO $ const $ do
  rows <- ask >>= \(_,pool) -> withResource pool $ \connection -> liftIO $
    runQuery connection machineTypesWithCountQuery
  let
    mapRow (mtRow :: MachineTypeRecord, count :: Int64) =
      ((_machineTypePK mtRow, _machineType mtRow), fromIntegral count :: Int)
    mappedRows = map mapRow rows
  return mappedRows

updateMachineType :: Handler MachineTypeDependencies
updateMachineType = mkInputHandler' (jsonO . jsonI) $ \(machineType', upkeepSequences :: [US.UpkeepSequence]) -> do
  ((cache, pool), sid) <- ask
  case sid of
    MachineTypeByName _ -> throwError UnsupportedRoute
    MachineTypeById machineTypeId -> do
      liftIO $ do
        let
          readToWrite row = (over machineTypePK (fmap Just) row) {
            _machineType = MT.MachineType {
              MT.kind = pgInt4 . MK.kindToDbRepr . MT.kind $ machineType' ,
              MT.machineTypeName = pgStrictText . MT.machineTypeName $ machineType' ,
              MT.machineTypeManufacturer = pgStrictText . MT.machineTypeManufacturer $ machineType' } }
          condition machineTypeRow = _machineTypePK machineTypeRow .=== fmap pgInt4 machineTypeId
        _ <- withResource pool $ \connection -> runUpdate connection machineTypesTable readToWrite condition
        _ <- withResource pool $ \connection ->
          runDelete connection upkeepSequencesTable (\table -> USD._machineTypeFK table .=== fmap pgInt4 machineTypeId)
        forM_ upkeepSequences $ \(US.UpkeepSequence displayOrder label repetition oneTime) ->
          withResource pool $ \connection -> runInsert connection upkeepSequencesTable $
            UpkeepSequenceRow {
              USD._machineTypeFK = fmap pgInt4 machineTypeId ,
              USD._upkeepSequence = US.UpkeepSequence {
                US.displayOrdering = pgInt4 displayOrder ,
                US.label_ = pgStrictText label ,
                US.repetition = pgInt4 repetition ,
                US.oneTime = pgBool oneTime }}
      recomputeWhole pool cache

machineTypesSingle :: Handler MachineTypeDependencies
machineTypesSingle = mkConstHandler' jsonO $ do
  ((_, pool), machineTypeSid) <- ask
  let
    performQuery parameter = withResource pool $ \connection ->
      liftIO $ runQuery connection (singleMachineTypeQuery parameter)
    (onEmptyResult, result) = case machineTypeSid of
      MachineTypeById (MT.MachineTypeId machineTypeIdInt) ->
        (throwError NotFound, performQuery . Right $ machineTypeIdInt)
      MachineTypeByName (mtName) ->
        (return MyNothing, performQuery . Left $ mtName)
  rows <- result
  case rows of
    x:xs | null xs -> do
      let mt = x :: MachineTypeRecord
      upkeepSequences <- withResource pool $ \connection -> liftIO $
        runQuery connection (upkeepSequencesByIdQuery . _machineTypePK $ mt)
      let us = fmap (\(row :: UpkeepSequenceRecord) -> _upkeepSequence row) upkeepSequences
      return $ MyJust (_machineTypePK mt, _machineType mt, us)
    [] -> onEmptyResult
    _ -> throwError NotFound

autocompleteSchema :: S.Schema MachineTypeSid MachineTypeMid Void
autocompleteSchema = S.withListing CountListing $ S.named [
  (A.autocompleteManufacturer, S.listingBy AutocompleteManufacturer) ,
  (A.autocomplete, S.listingBy Autocomplete) ,
  (A.byName, S.singleBy MachineTypeByName) ,
  (A.byId, S.singleRead MachineTypeById) ]
