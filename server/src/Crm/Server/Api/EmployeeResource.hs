module Crm.Server.Api.EmployeeResource where

import Opaleye.RunQuery (runQuery)
import Opaleye.Manipulation (runInsert)
import Opaleye.PGTypes (pgStrictText)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Data.Tuple.All (sel2)

import Rest.Resource (Resource, Void, schema, list, name, create, mkResourceReaderWith, get, update)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, jsonI)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler, mkConstHandler)

import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Employee as E

import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB
import Crm.Server.Helpers (prepareReaderTuple, withConnId, readMay', updateRows)

employeeResource :: Resource Dependencies IdDependencies UrlId () Void
employeeResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.employees ,
  schema = S.withListing () $ S.unnamedSingle readMay' ,
  list = const employeesListing ,
  get = Just getEmployeeHandler ,
  update = Just updateEmployeeHandler ,
  create = Just createEmployeeHandler }

getEmployeeHandler :: Handler IdDependencies
getEmployeeHandler = mkConstHandler (jsonO) $ withConnId (\connection theId -> do
  rows <- liftIO $ runQuery connection (singleEmployeeQuery theId)
  let rowsMapped = fmap (\row -> sel2 $ (convert row :: EmployeeMapped)) rows
  singleRowOrColumn rowsMapped)

updateEmployeeHandler :: Handler IdDependencies
updateEmployeeHandler = let
  readToWrite employee = const (Nothing, pgStrictText $ E.name employee, 
    pgStrictText $ E.contact employee, pgStrictText $ E.capabilities employee)
  in updateRows employeesTable readToWrite

createEmployeeHandler :: Handler Dependencies
createEmployeeHandler = mkInputHandler (jsonO . jsonI) (\newEmployee -> do
  conn <- ask 
  _ <- liftIO $ runInsert conn employeesTable (Nothing, pgStrictText $ E.name newEmployee,
    pgStrictText $ E.contact newEmployee, pgStrictText $ E.capabilities newEmployee)
  return () )

employeesListing :: ListHandler Dependencies 
employeesListing = mkListing (jsonO) (const $
  ask >>= \conn -> do 
    rawRows <- liftIO $ runQuery conn employeesQuery
    let rowsMapped = convert rawRows :: [EmployeeMapped]
    return rowsMapped )
