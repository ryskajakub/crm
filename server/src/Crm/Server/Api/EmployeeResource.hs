module Crm.Server.Api.EmployeeResource where

import Opaleye.RunQuery (runQuery)
import Opaleye.Manipulation (runInsert, runUpdate)
import Opaleye.PGTypes (pgString, pgInt4)
import Opaleye.Operators ((.==))

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Data.Tuple.All (uncurryN, sel1)

import Rest.Resource (Resource, Void, schema, list, name, create, mkResourceId, mkResourceReaderWith, get, update)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
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
getEmployeeHandler = mkConstHandler (jsonO . someO) $ withConnId (\connection theId -> do
  rows <- liftIO $ runQuery connection (singleEmployeeQuery theId)
  let r' = rows :: [(Int, String, String, String)]
  let result = fmap (\(employeeFields) -> (uncurryN (const E.Employee)) employeeFields) rows
  singleRowOrColumn result)

updateEmployeeHandler :: Handler IdDependencies
updateEmployeeHandler = let
  readToWrite employee = const (Nothing, pgString $ E.name employee, 
    pgString $ E.contact employee, pgString $ E.capabilities employee)
  in updateRows employeesTable readToWrite

createEmployeeHandler :: Handler Dependencies
createEmployeeHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\newEmployee -> do
  conn <- ask 
  _ <- liftIO $ runInsert conn employeesTable (Nothing, pgString $ E.name newEmployee,
    pgString $ E.contact newEmployee, pgString $ E.capabilities newEmployee)
  return () )

employeesListing :: ListHandler Dependencies 
employeesListing = mkListing (jsonO . someO) (const $
  ask >>= \conn -> do 
    rawRows <- liftIO $ runQuery conn employeesQuery
    let rowsMapped = map (\(eId,employeeName,b,c) -> (eId :: Int, E.Employee employeeName b c)) rawRows 
    return rowsMapped )
