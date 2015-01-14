module Crm.Server.Api.MachineResource where

import Opaleye.RunQuery (runQuery)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Rest.Resource (Resource, Void, schema, list, name, mkResourceReaderWith, get, update)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler, mkConstHandler)

import qualified Crm.Shared.Api as A

import Crm.Server.Helpers (prepareReaderTuple, maybeId, readMay', mappedUpkeepSequences)
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

machineResource :: Resource Dependencies IdDependencies UrlId () Void
machineResource = (mkResourceReaderWith prepareReaderTuple) {
  list = const machineListing ,
  get = Just machineSingle ,
  update = Just machineUpdate ,
  name = A.machines ,
  schema = S.withListing () (S.unnamedSingle readMay') }
    
machineUpdate :: Handler IdDependencies
machineUpdate = mkInputHandler (jsonI . someI) (\(machineTypeId, machine) ->
  ask >>= \(conn, id') -> maybeId id' (\machineId -> do
    _ <- liftIO $ runMachineUpdate (machineId, machineTypeId, machine) conn
    -- todo singal error if the update didn't hit a row
    return ()))

machineSingle :: Handler IdDependencies
machineSingle = mkConstHandler (jsonO . someO) (
  ask >>= (\(conn,id') -> maybeId id' (\id'' -> do
    rows <- liftIO $ runExpandedMachinesQuery (Just id'') conn
    (machineId, machine, companyId, machineTypeId, machineType) <- singleRowOrColumn rows
    upkeepSequences <- liftIO $ runQuery conn (upkeepSequencesByIdQuery machineTypeId)
    nextServiceYmd <- nextService machineId machine machineTypeId fst
    return (machine, companyId, machineTypeId, 
      (machineType, mappedUpkeepSequences upkeepSequences), nextServiceYmd))))

machineListing :: ListHandler Dependencies
machineListing = mkListing (jsonO . someO) (const $ do
  ask >>= \conn -> liftIO $ runExpandedMachinesQuery Nothing conn)
