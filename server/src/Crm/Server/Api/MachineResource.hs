{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.MachineResource where

import Opaleye.RunQuery (runQuery)
import Opaleye.PGTypes (pgInt4)
import Opaleye.Manipulation (runDelete)
import Opaleye.Operators ((.==))

import Data.Tuple.All (uncurryN)
import Data.Traversable (forM)
import Data.Tuple.All (sel2, sel1)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Rest.Resource (Resource, Void, schema, list, name, mkResourceReaderWith, get, update, remove)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler, mkConstHandler)

import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.Employee as E
import Crm.Shared.MyMaybe

import Crm.Server.Helpers (prepareReaderTuple, maybeId, readMay', mappedUpkeepSequences, dayToYmd, today)
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
machineDelete = mkConstHandler (jsonO . someO) (do
  (connection, maybeInt) <- ask
  maybeId maybeInt (\machineId -> let
    deleteMachineRow machine = sel1 machine .== pgInt4 machineId
    in liftIO $ runDelete connection machinesTable deleteMachineRow))

machineUpdate :: Handler IdDependencies
machineUpdate = mkInputHandler (jsonI . someI) (\(machine) ->
  ask >>= \(conn, id') -> maybeId id' (\machineId -> do
    _ <- liftIO $ runMachineUpdate (machineId, machine) conn
    -- todo singal error if the update didn't hit a row
    return ()))

machineSingle :: Handler IdDependencies
machineSingle = mkConstHandler (jsonO . someO) (
  ask >>= (\(conn,id') -> maybeId id' (\id'' -> do
    rows <- liftIO $ runExpandedMachinesQuery (Just id'') conn
    (machineId, machine, companyId, machineTypeId, machineType) <- singleRowOrColumn rows
    upkeepSequenceRows <- liftIO $ runQuery conn (upkeepSequencesByIdQuery $ pgInt4 machineTypeId)
    upkeepRows <- liftIO $ runQuery conn (upkeepsDataForMachine machineId)
    today' <- liftIO today
    let 
      upkeepSequences = fmap (\(a,b,c,d) -> US.UpkeepSequence a b c d) upkeepSequenceRows
      upkeepsData = fmap (\(((uId::Int,a,b,_::(Maybe Int),c,d,e),
          (_::Int,f,_::Int,g,h)),(_::(Maybe Int),eName::Maybe String)) -> let
        maybeEmployee = fmap E.Employee eName
        upkeep = U.Upkeep (dayToYmd a) b c d e
        upkeepMachine = UM.UpkeepMachine f g h
        in (uId, upkeep, upkeepMachine, toMyMaybe maybeEmployee)) upkeepRows
      upkeeps = fmap sel2 upkeepsData
      upkeepSequenceTuple = case upkeepSequences of
        [] -> undefined
        x : xs -> (x,xs)
      nextServiceYmd = nextServiceDate machine upkeepSequenceTuple upkeeps today'
    return (machine, companyId, machineTypeId, (machineType, 
      upkeepSequences), dayToYmd $ nextServiceYmd, upkeepsData))))

machineListing :: ListHandler Dependencies
machineListing = mkListing (jsonO . someO) (const $ do
  ask >>= \conn -> liftIO $ runExpandedMachinesQuery Nothing conn)
