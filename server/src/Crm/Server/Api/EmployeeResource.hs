module Crm.Server.Api.EmployeeResource where

import Opaleye.RunQuery (runQuery)
import Opaleye.Manipulation (runInsert)
import Opaleye.PGTypes (pgString)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Rest.Resource (Resource, Void, schema, list, name, create, mkResourceId, mkResourceReaderWith)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (ListHandler, mkListing, mkInputHandler, Handler)

import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Employee as E

import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB
import Crm.Server.Helpers (prepareReaderTuple)

employeeResource :: Resource Dependencies Dependencies Void () Void
employeeResource = mkResourceId {
  name = A.employees ,
  schema = S.withListing () $ S.named [] ,
  list = const $ employeesListing ,
  create = Just createEmployeeHandler }

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
