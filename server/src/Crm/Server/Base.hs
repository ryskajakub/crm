{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Server.Base where

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)

import Opaleye.QueryArr (Query, QueryArr)
import Opaleye.Table (Table(Table), required, queryTable, optional)
import Opaleye.Column (Column, toNullable, Nullable)
import qualified Opaleye.Column as COL
import Opaleye.Order (orderBy, asc, limit, desc)
import Opaleye.RunQuery (runQuery)
import Opaleye.Operators ((.==), (.&&), (.||), restrict, lower, (.<))
import qualified Opaleye.Operators as OO
import Opaleye.PGTypes (pgInt4, PGDate, pgDay, PGBool, PGInt4, PGInt8, PGText, pgString, pgBool)
import Opaleye.Manipulation (runInsert, runUpdate, runInsertReturning, runDelete)
import qualified Opaleye.Aggregate as AGG

import Control.Monad.Reader (ReaderT, ask, runReaderT, mapReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Error (ErrorT)
import Control.Monad.Trans.Class (lift)
import Control.Arrow (returnA)
import Control.Monad (forM_, forM)

import Data.Profunctor.Product (p1, p2, p3, p4, p5, p6)
import Data.Maybe (maybeToList)
import Data.Functor.Identity (runIdentity)
import Data.Time.Calendar (Day, addDays)
import Data.Int (Int64)
import Data.List (intersperse, sortBy)
import Data.Tuple.All (Sel1, sel1, sel2, sel3, sel4, upd3, uncurryN)

import Rest.Api (Api, mkVersion, Some1(Some1), Router, route, root, compose)
import Rest.Resource (Resource, mkResourceId, Void, schema, list, name, create, mkResourceReaderWith, get ,
  update )
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler, mkConstHandler)
import Rest.Types.Error (DataError(ParseError), Reason(InputError, IdentError, 
  NotFound, NotAllowed, UnsupportedRoute))

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.UpkeepMachine as UM
import qualified Crm.Shared.YearMonthDay as D
import qualified Crm.Shared.Employee as E
import qualified Crm.Shared.UpkeepSequence as US

import Crm.Server.Helpers (ymdToDay, dayToYmd, maybeId, readMay', mapUpkeeps, 
  prepareReaderIdentity, prepareReaderTuple, mappedUpkeepSequences)
import Crm.Server.Boilerplate ()
import Crm.Server.DB
import Crm.Server.Types
import Crm.Server.Api.CompanyResource (companyResource)
import Crm.Server.Api.MachineResource (machineResource)
import qualified Crm.Server.Api.UpkeepResource as UR
import Crm.Server.Api.MachineTypeResource (machineTypeResource)
import Crm.Server.Api.EmployeeResource (employeeResource)

import Safe (readMay, minimumMay)

import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

schema' :: S.Schema Void () Void
schema' = S.withListing () (S.named [])

companyUpkeepsListing :: ListHandler IdDependencies
companyUpkeepsListing = mkListing (jsonO . someO) (const $
  ask >>= \(conn,id') -> maybeId id' (\id'' -> do
    rows <- liftIO $ runCompanyUpkeepsQuery id'' conn
    return $ map (\(id''',u1,u2,_) -> (id''', U.Upkeep (dayToYmd u1) u2)) rows))

getUpkeep :: Handler IdDependencies
getUpkeep = mkConstHandler (jsonO . someO) ( do
  rows <- ask >>= \(conn, upkeepId') -> maybeId upkeepId' (\upkeepId ->
    liftIO $ runSingleUpkeepQuery conn upkeepId)
  let result = mapUpkeeps rows
  singleRowOrColumn (map snd result))

insertUpkeepMachines :: Connection -> Int -> [(UM.UpkeepMachine, Int)] -> IO ()
insertUpkeepMachines connection upkeepId upkeepMachines = let
  insertUpkeepMachine (upkeepMachine', upkeepMachineId) = do
    _ <- runInsert
      connection
      upkeepMachinesTable (
        pgInt4 upkeepId ,
        pgString $ UM.upkeepMachineNote upkeepMachine' ,
        pgInt4 upkeepMachineId ,
        pgInt4 $ UM.recordedMileage upkeepMachine' )
    return ()
  in forM_ upkeepMachines insertUpkeepMachine

-- type UpkeepWriteTable = (Maybe DBInt, DBDate, DBBool, Maybe (Column (Nullable PGInt4)))

maybeToNullable :: Maybe (Column a) -> Column (Nullable a)
maybeToNullable (Just a) = toNullable a
maybeToNullable Nothing = COL.null

updateUpkeep :: Connection
             -> Int
             -> (U.Upkeep, [(UM.UpkeepMachine, Int)], Maybe Int)
             -> IO ()
updateUpkeep conn upkeepId (upkeep, upkeepMachines, employeeId) = do
  _ <- let
    condition (upkeepId',_,_,_) = upkeepId' .== pgInt4 upkeepId
    readToWrite _ =
      (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep, pgBool $ U.upkeepClosed upkeep, maybeToNullable $ fmap pgInt4 employeeId)
    in runUpdate conn upkeepTable readToWrite condition
  _ <- runDelete conn upkeepMachinesTable (\(upkeepId',_,_,_) -> upkeepId' .== pgInt4 upkeepId)
  insertUpkeepMachines conn upkeepId upkeepMachines
  return ()

addUpkeep :: Connection
          -> (U.Upkeep, [(UM.UpkeepMachine, Int)], Maybe Int)
          -> IO Int -- ^ id of the upkeep
addUpkeep connection (upkeep, upkeepMachines, employeeId) = do
  upkeepIds <- runInsertReturning
    connection
    upkeepTable (Nothing, pgDay $ ymdToDay $ U.upkeepDate upkeep, 
      pgBool $ U.upkeepClosed upkeep, maybeToNullable $ fmap pgInt4 employeeId)
    sel1
  let upkeepId = head upkeepIds
  insertUpkeepMachines connection upkeepId upkeepMachines
  return upkeepId

addMachine :: Connection
           -> M.Machine
           -> Int
           -> MT.MyEither
           -> IO Int -- ^ id of newly created machine
addMachine connection machine companyId' machineType = do
  machineTypeId <- case machineType of
    MT.MyInt id' -> return $ id'
    MT.MyMachineType (MT.MachineType name' manufacturer, upkeepSequences) -> do
      newMachineTypeId <- runInsertReturning
        connection
        machineTypesTable (Nothing, pgString name', pgString manufacturer)
        sel1
      let machineTypeId = head newMachineTypeId -- todo safe
      forM_ upkeepSequences (\(US.UpkeepSequence displayOrdering label repetition oneTime) -> runInsert
        connection
        upkeepSequencesTable (pgInt4 displayOrdering, pgString label, 
          pgInt4 repetition, pgInt4 machineTypeId, pgBool oneTime))
      return machineTypeId
  let
    M.Machine machineOperationStartDate' initialMileage mileagePerYear = machine
  machineId <- runInsertReturning
    connection
    machinesTable (Nothing, pgInt4 companyId', pgInt4 machineTypeId, pgDay $ ymdToDay machineOperationStartDate',
      pgInt4 initialMileage, pgInt4 mileagePerYear)
    (\(id',_, _, _,_,_) -> id')
  return $ head machineId -- todo safe

createMachineHandler :: Handler IdDependencies
createMachineHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\(newMachine,machineType) ->
  ask >>= \(connection, maybeInt) -> maybeId maybeInt (\companyId -> 
    liftIO $ addMachine connection newMachine companyId machineType))

updateUpkeepHandler :: Handler IdDependencies
updateUpkeepHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\(upkeep,machines,employeeId) -> let 
  employeeListToMaybe = case employeeId of
    x : _ -> Just x
    _ -> Nothing
  upkeepTriple = (upkeep, machines, employeeListToMaybe)
  in ask >>= \(connection, maybeInt) -> maybeId maybeInt (\upkeepId ->
    liftIO $ updateUpkeep connection upkeepId upkeepTriple))

createUpkeepHandler :: Handler IdDependencies
createUpkeepHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\newUpkeep ->
  let 
    (_,_,selectedEmployeeId) = newUpkeep
    employeeListToMaybe = case selectedEmployeeId of
      x : _ -> Just x
      _ -> Nothing
    newUpkeep' = upd3 employeeListToMaybe newUpkeep
    in ask >>= \(connection, maybeInt) -> maybeId maybeInt (
      -- todo check that the machines are belonging to this company
      const $ liftIO $ addUpkeep connection newUpkeep'))

companyMachineResource :: Resource IdDependencies IdDependencies Void Void Void
companyMachineResource = mkResourceId {
  name = A.machines ,
  schema = S.noListing $ S.named [] ,
  create = Just createMachineHandler }

upkeepResource :: Resource IdDependencies IdDependencies UrlId () Void
upkeepResource = (mkResourceReaderWith prepareReaderIdentity) {
  name = A.upkeep ,
  schema = S.withListing () $ S.unnamedSingle readMay' ,
  list = const $ companyUpkeepsListing ,
  get = Just getUpkeep ,
  update = Just updateUpkeepHandler ,
  create = Just createUpkeepHandler }

router' :: Router Dependencies Dependencies
router' = root `compose` ((route companyResource `compose` route companyMachineResource)
                                                 `compose` route upkeepResource)
               `compose` route machineResource
               `compose` route UR.upkeepResource
               `compose` route machineTypeResource
               `compose` route employeeResource

api :: Api Dependencies
api = [(mkVersion 1 0 0, Some1 $ router')]
