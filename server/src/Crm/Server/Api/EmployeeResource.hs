{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.EmployeeResource where

import           Opaleye.RunQuery            (runQuery)
import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgStrictText)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

import           Data.Tuple.All              (sel2)
import           Data.Pool                   (withResource)
import           Data.Text                   (Text)

import           Rest.Resource               (Resource, Void, schema, list, name, 
                                             create, mkResourceReaderWith, get, update)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (ListHandler, Handler)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Employee         as E

import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Helpers          (prepareReaderTuple)
import           Crm.Server.Handler          (mkConstHandler', mkInputHandler', mkListing', updateRows'')

import           TupleTH                     (proj)

data EmployeeListing = EmployeesListing | ColoursListing

employeeResource :: Resource Dependencies (IdDependencies' E.EmployeeId) E.EmployeeId EmployeeListing Void
employeeResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.employees ,
  schema = S.withListing EmployeesListing $ S.named [
    ("single", S.singleRead id) ,
    ("colours", S.listing ColoursListing)] ,
  list = \listingType -> case listingType of
    EmployeesListing -> employeesListing 
    ColoursListing -> coloursListing ,
  get = Just getEmployeeHandler ,
  update = Just updateEmployeeHandler ,
  create = Just createEmployeeHandler }

coloursListing :: ListHandler Dependencies
coloursListing = mkListing' jsonO $ const $ do
  (_, pool) <- ask
  (rows :: [Text]) <- withResource pool $ \connection -> liftIO $ runQuery connection takenColoursQuery
  return rows

getEmployeeHandler :: Handler (IdDependencies' E.EmployeeId)
getEmployeeHandler = mkConstHandler' jsonO $ do
  ((_, pool), theId) <- ask
  rows <- withResource pool $ \connection -> liftIO $ runQuery connection (singleEmployeeQuery . E.getEmployeeId $ theId)
  let rowsMapped = fmap (\row -> sel2 $ (convert row :: EmployeeMapped)) rows
  singleRowOrColumn rowsMapped

updateEmployeeHandler :: Handler (IdDependencies' E.EmployeeId)
updateEmployeeHandler = let
  readToWrite employee = \eRow -> (Just . $(proj 5 0) $ eRow, pgStrictText $ E.name employee, 
    pgStrictText $ E.contact employee, pgStrictText $ E.capabilities employee, pgStrictText . E.colour $ employee)
  in (updateRows'' employeesTable readToWrite E.getEmployeeId (const $ const $ const $ return ()))

createEmployeeHandler :: Handler Dependencies
createEmployeeHandler = mkInputHandler' (jsonO . jsonI) (\newEmployee -> do
  (_, pool) <- ask 
  _ <- liftIO $ withResource pool $ \connection -> runInsert connection employeesTable (Nothing, pgStrictText $ E.name newEmployee,
    pgStrictText $ E.contact newEmployee, pgStrictText $ E.capabilities newEmployee, pgStrictText . E.colour $ newEmployee)
  return () )

employeesListing :: ListHandler Dependencies 
employeesListing = mkListing' (jsonO) $ const $ do
  (_, pool) <- ask
  rawRows <- withResource pool $ \connection -> liftIO $ runQuery connection employeesQuery
  let rowsMapped = convert rawRows :: [EmployeeMapped]
  return rowsMapped
