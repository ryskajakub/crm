{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Server.Api.CompanyResource where

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

import Crm.Server.Helpers (ymdToDay, dayToYmd, prepareReaderTuple, maybeId, readMay')
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

import Safe (readMay, minimumMay)

import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

createCompanyHandler :: Handler Dependencies
createCompanyHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\newCompany ->
  ask >>= \conn -> liftIO $ addCompany conn newCompany )

listing :: ListHandler Dependencies
listing = mkListing (jsonO . someO) (const $ do
  rows <- ask >>= \conn -> liftIO $ runCompaniesQuery conn
  ask >>= (\conn -> do 
    unsortedResult <- forM rows (\companyRow -> do
      let companyId = sel1 companyRow
      machines <- liftIO $ runMachinesInCompanyQuery companyId conn
      nextDays <- forM machines (\(machineId, machine, _, machineTypeId, _) -> do
        nextServiceDay <- nextService machineId machine machineTypeId id
        return nextServiceDay )
      return $ (companyId, (uncurryN $ const C.Company) companyRow , maybeToList $ minimumMay nextDays))
    return $ sortBy (\r1 r2 -> let 
      date1 = sel3 r1  
      date2 = sel3 r2
      in case (date1, date2) of
        (date1' : _, date2' : _) -> date1' `compare` date2'
        ([],[]) -> EQ
        ([],_) -> GT
        _ -> LT ) unsortedResult ))

singleCompany :: Handler IdDependencies
singleCompany = mkConstHandler (jsonO . someO) (
  ask >>= \(conn, id') -> maybeId id' (\companyId -> do
    rows <- liftIO $ runCompanyWithMachinesQuery companyId conn
    company <- singleRowOrColumn rows
    machines <- liftIO $ runMachinesInCompanyQuery companyId conn
    return ((uncurryN (const C.Company)) company , machines)))

updateCompany :: Handler IdDependencies
updateCompany = mkInputHandler (jsonI . someI . jsonO . someO) (\company ->
  ask >>= \(conn, companyId') -> maybeId companyId' (\companyId -> do
    let
      readToWrite = const (Nothing, pgString $ C.companyName company, pgString $ C.companyPlant company ,
        pgString $ C.companyAddress company, pgString $ C.companyPerson company, pgString $ C.companyPhone company)
      condition = (pgInt4 companyId .==) . sel1
    _ <- liftIO $ runUpdate conn companiesTable readToWrite condition
    return ()))

companyResource :: Resource Dependencies IdDependencies UrlId () Void
companyResource = (mkResourceReaderWith prepareReaderTuple) {
  list = const listing ,
  create = Just createCompanyHandler ,
  name = A.companies ,
  get = Just singleCompany ,
  update = Just updateCompany ,
  schema = companySchema }

companySchema :: S.Schema UrlId () Void
companySchema = S.withListing () $ S.unnamedSingle readMay'
