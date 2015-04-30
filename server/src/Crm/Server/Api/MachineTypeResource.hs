{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.MachineTypeResource (
  machineTypeResource) where

import Opaleye.RunQuery (runQuery)
import Opaleye.Operators ((.==))
import Opaleye.PGTypes (pgInt4, pgString, pgBool)
import Opaleye.Manipulation (runInsert, runUpdate, runDelete)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad (forM_)

import Data.Tuple.All (sel1, sel2, sel4)
import Data.Int (Int64)

import Rest.Types.Error (Reason(NotFound, UnsupportedRoute))
import Rest.Resource (Resource, Void, schema, list, name, mkResourceReaderWith, get, update)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler, mkConstHandler)

import qualified Crm.Shared.Api as A
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.UpkeepSequence as US

import Crm.Server.Helpers (prepareReaderTuple, maybeId, readMay', mappedUpkeepSequences)
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB
import Crm.Shared.MyMaybe

machineTypeResource :: Resource Dependencies MachineTypeDependencies MachineTypeSid MachineTypeMid Void
machineTypeResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.machineTypes ,
  list = machineTypesListing ,
  update = Just updateMachineType ,
  get = Just machineTypesSingle ,
  schema = autocompleteSchema }

machineTypesListing :: MachineTypeMid -> ListHandler Dependencies
machineTypesListing (Autocomplete mid) = mkListing (jsonO . someO) (const $ 
  ask >>= \conn -> liftIO $ runMachineTypesQuery' mid conn)
machineTypesListing (AutocompleteManufacturer mid) = mkListing (jsonO . someO) (const $
  ask >>= \conn -> liftIO $ ((runQuery conn (machineManufacturersQuery mid)) :: IO [String]))
machineTypesListing CountListing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runQuery conn machineTypesWithCountQuery 
  let 
    mapRow :: ((Int,Int,String,String),Int64) -> ((Int, MT.MachineType), Int)
    mapRow (mtRow, count) = (mapMachineType mtRow, fromIntegral count)
    mappedRows = map mapRow rows
  return mappedRows )

updateMachineType :: Handler MachineTypeDependencies
updateMachineType = mkInputHandler (jsonO . jsonI . someI . someO) (\(machineType, upkeepSequences) ->
  ask >>= \(conn, sid) -> case sid of
    MachineTypeByName _ -> throwError UnsupportedRoute
    MachineTypeById machineTypeId' -> maybeId machineTypeId' (\machineTypeId -> liftIO $ do
      let 
        readToWrite row = (Nothing, sel2 row, pgString $ MT.machineTypeName machineType, 
          pgString $ MT.machineTypeManufacturer machineType)
        condition machineTypeRow = sel1 machineTypeRow .== pgInt4 machineTypeId
      _ <- runUpdate conn machineTypesTable readToWrite condition
      _ <- runDelete conn upkeepSequencesTable (\table -> sel4 table .== pgInt4 machineTypeId)
      forM_ upkeepSequences (\ (US.UpkeepSequence displayOrder label repetition oneTime) -> 
        runInsert conn upkeepSequencesTable (pgInt4 displayOrder,
          pgString label, pgInt4 repetition, pgInt4 machineTypeId, pgBool oneTime))))

machineTypesSingle :: Handler MachineTypeDependencies
machineTypesSingle = mkConstHandler (jsonO . someO) (do
  (conn, machineTypeSid) <- ask
  let 
    performQuery parameter = liftIO $ runQuery conn (singleMachineTypeQuery parameter)
    (onEmptyResult, result) = case machineTypeSid of
      MachineTypeById(Right(mtId)) -> (throwError NotFound, performQuery $ Right mtId)
      MachineTypeById(Left(_)) -> (undefined, throwError NotFound)
      MachineTypeByName(mtName) -> (return MyNothing, performQuery $ Left mtName)
  rows <- result
  case rows of
    x:xs | null xs -> do 
      let mt = mapMachineType x
      upkeepSequences <- liftIO $ runQuery conn (upkeepSequencesByIdQuery $ pgInt4 $ sel1 mt)
      return $ MyJust (sel1 mt, sel2 mt, mappedUpkeepSequences upkeepSequences)
    [] -> onEmptyResult
    _ -> throwError NotFound)

autocompleteSchema :: S.Schema MachineTypeSid MachineTypeMid Void
autocompleteSchema = S.withListing CountListing $ S.named [(
  A.autocompleteManufacturer, S.listingBy (\str -> AutocompleteManufacturer str)),(
  A.autocomplete, S.listingBy (\str -> Autocomplete str)),(
  A.byName, S.singleBy (\str -> MachineTypeByName str)),(
  A.byId, S.singleBy (\mtId -> MachineTypeById $ readMay' mtId))]
