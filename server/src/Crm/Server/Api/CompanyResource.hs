{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PackageImports #-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Server.Api.CompanyResource where

import Opaleye.Operators ((.==))
import Opaleye.PGTypes (pgInt4, pgString)
import Opaleye.Manipulation (runUpdate)

import "mtl" Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM)

import Data.Maybe (maybeToList)
import Data.List (sortBy)
import Data.Tuple.All (sel1, sel3, uncurryN)

import Rest.Resource (Resource, Void, schema, list, name, create, mkResourceReaderWith, get ,
  update )
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler, mkConstHandler)

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.Api as A

import Crm.Server.Helpers (prepareReaderTuple, maybeId, readMay')
import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

import Safe (minimumMay)

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
