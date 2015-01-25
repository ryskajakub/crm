{-# LANGUAGE RankNTypes #-}

module Crm.Server.Api.CompanyResource where

import Opaleye.Operators ((.==))
import Opaleye.PGTypes (pgInt4, pgString)
import Opaleye.Manipulation (runUpdate)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM)

import Data.List (sortBy)
import Data.Tuple.All (sel1, sel2, sel3, uncurryN)

import Rest.Resource (Resource, Void, schema, list, name, create, mkResourceReaderWith, get ,
  update )
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkOrderedListing, mkInputHandler, Handler, mkConstHandler)

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Direction as DIR
import qualified Crm.Shared.Api as A
import Crm.Shared.MyMaybe

import Crm.Server.Helpers (prepareReaderTuple, maybeId, readMay')
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

import Safe (minimumMay, readMay)

createCompanyHandler :: Handler Dependencies
createCompanyHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\newCompany ->
  ask >>= \conn -> liftIO $ addCompany conn newCompany )

listing :: ListHandler Dependencies
listing = mkOrderedListing (jsonO . someO) (\(_, rawOrder, rawDirection) -> do
  let 
    order = rawOrder >>= readMay
    direction = rawDirection >>= readMay
    -- "negate" the ordering when it is descending
    orderingByDirection :: Ordering -> Ordering
    orderingByDirection = case direction of
      Nothing -> id
      Just(DIR.Asc) -> id
      Just(DIR.Desc) -> \ord -> case ord of
        LT -> GT
        GT -> LT
        EQ -> EQ
  conn <- ask 
  rows <- liftIO $ runCompaniesQuery conn
  unsortedResult <- forM rows (\companyRow -> do
    let companyId = sel1 companyRow
    machines <- liftIO $ runMachinesInCompanyQuery companyId conn
    nextDays <- forM machines (\(machineId, machine, _, machineTypeId, _) -> do
      nextServiceDay <- nextService machineId machine machineTypeId id
      return nextServiceDay )
    return $ (companyId, (uncurryN $ const C.Company) companyRow, toMyMaybe $ minimumMay nextDays))
  return $ sortBy (\r1 r2 -> case order of
    Nothing -> EQ
    Just C.CompanyName ->
      orderingByDirection $ (C.companyName $ sel2 r1) `compare` (C.companyName $ sel2 r2)
    Just C.NextService ->
      case (sel3 r1, sel3 r2) of
        (MyJust (date1'), MyJust(date2')) -> orderingByDirection $ date1' `compare` date2'
        -- when the date is missing, the results will always go to the bottom
        (MyNothing,MyNothing) -> EQ
        (MyNothing,_) -> GT
        _ -> LT
      ) unsortedResult)

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
  schema = S.withListing () $ S.unnamedSingle readMay' }
