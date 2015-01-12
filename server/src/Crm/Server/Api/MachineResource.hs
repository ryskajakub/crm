{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Server.Api.MachineResource where

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

import "mtl" Control.Monad.Reader (ReaderT, ask, runReaderT, mapReaderT)
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

import Crm.Server.Helpers (ymdToDay, dayToYmd, prepareReaderTuple, maybeId, readMay' , 
  mappedUpkeepSequences)
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

import Safe (readMay, minimumMay)

import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

schema'' :: S.Schema UrlId () Void
schema'' = S.withListing () (S.unnamedSingle readMay')

machineResource :: Resource Dependencies IdDependencies UrlId () Void
machineResource = (mkResourceReaderWith prepareReaderTuple) {
  list = const machineListing ,
  get = Just machineSingle ,
  update = Just machineUpdate ,
  name = A.machines ,
  schema = schema'' }
    
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
