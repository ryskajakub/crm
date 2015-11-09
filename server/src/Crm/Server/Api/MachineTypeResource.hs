{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Crm.Server.Api.MachineTypeResource (
  machineTypeResource) where

import           Opaleye.RunQuery            (runQuery)
import           Opaleye.Operators           ((.==))
import           Opaleye.PGTypes             (pgInt4, pgStrictText, pgBool)
import           Opaleye.Manipulation        (runInsert, runUpdate, runDelete)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Error.Class   (throwError)
import           Control.Monad               (forM_)

import           Data.Tuple.All              (sel1, sel2, sel4)
import           Data.Int                    (Int64)
import           Data.Text                   (Text)
import           Data.Pool                   (withResource)

import           Rest.Types.Error            (Reason(NotFound, UnsupportedRoute))
import           Rest.Resource               (Resource, Void, schema, list, name, 
                                             mkResourceReaderWith, get, update)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (ListHandler, Handler)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.MachineType      as MT
import qualified Crm.Shared.UpkeepSequence   as US
import           Crm.Shared.MyMaybe

import           Crm.Server.Helpers          (prepareReaderTuple)
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler          (mkInputHandler', mkConstHandler', mkListing')
import           Crm.Server.CachedCore       (recomputeWhole)

import           TupleTH                     (proj)


machineTypeResource :: Resource Dependencies MachineTypeDependencies MachineTypeSid MachineTypeMid Void
machineTypeResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.machineTypes ,
  list = machineTypesListing ,
  update = Just updateMachineType ,
  get = Just machineTypesSingle ,
  schema = autocompleteSchema }

machineTypesListing :: MachineTypeMid -> ListHandler Dependencies
machineTypesListing (Autocomplete mid) = mkListing' jsonO $ const $ 
  ask >>= \(_,pool) -> withResource pool $ \connection -> liftIO $ runMachineTypesQuery' mid connection
machineTypesListing (AutocompleteManufacturer mid) = mkListing' jsonO $ const $
  ask >>= \(_,pool) -> withResource pool $ \connection -> 
    liftIO $ ((runQuery connection (machineManufacturersQuery mid)) :: IO [Text])
machineTypesListing CountListing = mkListing' jsonO $ const $ do
  rows <- ask >>= \(_,pool) -> withResource pool $ \connection -> liftIO $ runQuery connection machineTypesWithCountQuery 
  let 
    mapRow :: ((Int,Int,Text,Text),Int64) -> ((MT.MachineTypeId, MT.MachineType), Int)
    mapRow (mtRow, count) = (convert mtRow :: MachineTypeMapped, fromIntegral count)
    mappedRows = map mapRow rows
  return mappedRows

updateMachineType :: Handler MachineTypeDependencies
updateMachineType = mkInputHandler' (jsonO . jsonI) $ \(machineType, upkeepSequences :: [US.UpkeepSequence]) -> do
  ((cache, pool), sid) <- ask
  case sid of
    MachineTypeByName _ -> throwError UnsupportedRoute
    MachineTypeById (MT.MachineTypeId machineTypeIdInt) -> do 
      liftIO $ do
        let 
          readToWrite row = (Just . $(proj 4 0) $ row, sel2 row, pgStrictText $ MT.machineTypeName machineType, 
            pgStrictText $ MT.machineTypeManufacturer machineType)
          condition machineTypeRow = sel1 machineTypeRow .== pgInt4 machineTypeIdInt
        _ <- withResource pool $ \connection -> runUpdate connection machineTypesTable readToWrite condition
        _ <- withResource pool $ \connection -> 
          runDelete connection upkeepSequencesTable (\table -> sel4 table .== pgInt4 machineTypeIdInt)
        forM_ upkeepSequences $ \(US.UpkeepSequence displayOrder label repetition oneTime) -> 
          withResource pool $ \connection -> runInsert connection upkeepSequencesTable (pgInt4 displayOrder,
            pgStrictText label, pgInt4 repetition, pgInt4 machineTypeIdInt, pgBool oneTime)
      recomputeWhole pool cache

machineTypesSingle :: Handler MachineTypeDependencies
machineTypesSingle = mkConstHandler' jsonO $ do
  ((_, pool), machineTypeSid) <- ask
  let 
    performQuery parameter = withResource pool $ \connection -> 
      liftIO $ runQuery connection (singleMachineTypeQuery parameter)
    (onEmptyResult, result) = case machineTypeSid of
      MachineTypeById (MT.MachineTypeId machineTypeIdInt) -> 
        (throwError NotFound, performQuery $ Right machineTypeIdInt)
      MachineTypeByName (mtName) -> 
        (return MyNothing, performQuery . Left $ mtName)
  rows <- result
  case rows of
    x:xs | null xs -> do 
      let mt = convert x :: MachineTypeMapped
      upkeepSequences <- withResource pool $ \connection -> liftIO $ 
        runQuery connection (upkeepSequencesByIdQuery $ pgInt4 $ MT.getMachineTypeId $ sel1 mt)
      let mappedUpkeepSequences = fmap (\row -> sel2 (convert row :: UpkeepSequenceMapped)) upkeepSequences
      return $ MyJust (sel1 mt, sel2 mt, mappedUpkeepSequences)
    [] -> onEmptyResult
    _ -> throwError NotFound

autocompleteSchema :: S.Schema MachineTypeSid MachineTypeMid Void
autocompleteSchema = S.withListing CountListing $ S.named [
  (A.autocompleteManufacturer, S.listingBy AutocompleteManufacturer) ,
  (A.autocomplete, S.listingBy Autocomplete) ,
  (A.byName, S.singleBy MachineTypeByName) , 
  (A.byId, S.singleRead MachineTypeById) ]
