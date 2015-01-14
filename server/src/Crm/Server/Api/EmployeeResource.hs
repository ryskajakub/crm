module Crm.Server.Api.EmployeeResource where

import Opaleye.RunQuery (runQuery)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Rest.Resource (Resource, Void, schema, list, name, mkResourceId)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO)
import Rest.Handler (ListHandler, mkListing)

import qualified Crm.Shared.Api as A
import qualified Crm.Shared.Employee as E

import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB

employeeResource :: Resource Dependencies Dependencies Void () Void
employeeResource = mkResourceId {
  name = A.employees ,
  schema = S.withListing () $ S.named [] ,
  list = const $ employeesListing }

employeesListing :: ListHandler Dependencies 
employeesListing = mkListing (jsonO . someO) (const $
  ask >>= \conn -> do 
    rawRows <- liftIO $ runQuery conn employeesQuery
    let rowsMapped = map (\(eId,employeeName) -> (eId :: Int, E.Employee employeeName)) rawRows 
    return rowsMapped )
