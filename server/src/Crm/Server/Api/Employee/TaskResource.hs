module Crm.Server.Api.Employee.TaskResource (
  resource ) where

import           Control.Monad.Reader          (ask)
import           Control.Monad.IO.Class        (liftIO)

import           Data.Pool                     (withResource)

import           Opaleye                       (runQuery)

import           Rest.Resource                 (Resource, Void, schema, name,
                                               list, mkResourceId)
import qualified Rest.Schema                   as S
import           Rest.Dictionary.Combinators   (jsonO)
import           Rest.Handler                  (ListHandler)

import qualified Crm.Shared.Api                as A
import qualified Crm.Shared.Employee           as E

import           Crm.Server.DB
import           Crm.Server.Boilerplate        ()
import           Crm.Server.Types
import           Crm.Server.Handler


resource :: Resource (IdDependencies' E.EmployeeId) (IdDependencies' E.EmployeeId) Void () Void
resource = mkResourceId {
  name = "task" ,
  schema = S.withListing () $ S.named [] ,
  list = const tasksListing }

tasksListing :: ListHandler (IdDependencies' E.EmployeeId)
tasksListing = mkGenHandler' jsonO $ \env -> do
  ((_, pool), employeeId) <- ask
  withResource pool $ \connection -> liftIO $ do
    upkeeps <- runQuery connection (tasksForEmployeeQuery employeeId)
    let employeeTasks = convert upkeeps :: [EmployeeTaskMapped]
    return employeeTasks
