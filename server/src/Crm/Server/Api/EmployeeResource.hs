module Crm.Server.Api.EmployeeResource where

import           Opaleye.RunQuery            (runQuery)
import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgStrictText)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

import           Data.Tuple.All              (sel2)

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

employeeResource :: Resource Dependencies (IdDependencies' E.EmployeeId) E.EmployeeId () Void
employeeResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.employees ,
  schema = S.withListing () $ S.unnamedSingleRead id ,
  list = const employeesListing ,
  get = Just getEmployeeHandler ,
  update = Just updateEmployeeHandler ,
  create = Just createEmployeeHandler }

getEmployeeHandler :: Handler (IdDependencies' E.EmployeeId)
getEmployeeHandler = mkConstHandler' jsonO $ do
  ((_, connection), theId) <- ask
  rows <- liftIO $ runQuery connection (singleEmployeeQuery . E.getEmployeeId $ theId)
  let rowsMapped = fmap (\row -> sel2 $ (convert row :: EmployeeMapped)) rows
  singleRowOrColumn rowsMapped

updateEmployeeHandler :: Handler (IdDependencies' E.EmployeeId)
updateEmployeeHandler = let
  readToWrite employee = const (Nothing, pgStrictText $ E.name employee, 
    pgStrictText $ E.contact employee, pgStrictText $ E.capabilities employee)
  in (updateRows'' employeesTable readToWrite E.getEmployeeId (const $ const $ const $ return ()))

createEmployeeHandler :: Handler Dependencies
createEmployeeHandler = mkInputHandler' (jsonO . jsonI) (\newEmployee -> do
  (_,conn) <- ask 
  _ <- liftIO $ runInsert conn employeesTable (Nothing, pgStrictText $ E.name newEmployee,
    pgStrictText $ E.contact newEmployee, pgStrictText $ E.capabilities newEmployee)
  return () )

employeesListing :: ListHandler Dependencies 
employeesListing = mkListing' (jsonO) $ const $ do
  (_,conn) <- ask
  rawRows <- liftIO $ runQuery conn employeesQuery
  let rowsMapped = convert rawRows :: [EmployeeMapped]
  return rowsMapped
