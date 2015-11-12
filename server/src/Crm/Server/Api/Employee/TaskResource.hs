{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.Employee.TaskResource (
  resource ) where

import           Control.Monad.Reader          (ask)
import           Control.Monad.IO.Class        (liftIO)

import           Data.Pool                     (withResource)
import           Data.Tuple.All                (sel1)

import           Opaleye                       (runQuery, pgBool, runInsert, pgDay, pgString, pgStrictText,
                                               runInsertReturning, pgInt4)

import           Rest.Resource                 (Resource, Void, schema, name, create,
                                               list, mkResourceId)
import qualified Rest.Schema                   as S
import           Rest.Dictionary.Combinators   (jsonO, jsonI)
import           Rest.Handler                  (ListHandler, Handler)

import qualified Crm.Shared.Api                as A
import qualified Crm.Shared.Employee           as E
import qualified Crm.Shared.Task               as T

import           Crm.Server.DB
import           Crm.Server.Boilerplate        ()
import           Crm.Server.Types
import           Crm.Server.Handler
import           Crm.Server.Helpers            (ymdToDay, maybeToNullable)


resource :: Resource (IdDependencies' E.EmployeeId) (IdDependencies' E.EmployeeId) Void () Void
resource = mkResourceId {
  name = "task" ,
  create = Just createHandler ,
  schema = S.withListing () $ S.named [] ,
  list = const tasksListing }

tasksListing :: ListHandler (IdDependencies' E.EmployeeId)
tasksListing = mkGenHandler' jsonO $ \env -> do
  ((_, pool), employeeId) <- ask
  withResource pool $ \connection -> liftIO $ do
    tasks <- runQuery connection (tasksForEmployeeQuery employeeId)
    let employeeTasks = convert tasks :: [TaskMapped]
    return employeeTasks

createHandler :: Handler (IdDependencies' E.EmployeeId)
createHandler = mkInputHandler' (jsonO . jsonI) $ \newTask -> do
  ((_, pool), (E.EmployeeId employeeId)) <- ask
  withResource pool $ \connection -> do
    taskIds <- liftIO $ runInsertReturning
      connection
      tasksTable
      (Nothing, pgDay . ymdToDay . T.startDate $ newTask,
        pgStrictText . T.description $ newTask, maybeToNullable . fmap (pgDay . ymdToDay) . T.endDate $ newTask)
      sel1
    (taskId :: Int) <- singleRowOrColumn taskIds
    liftIO $ runInsert
      connection
      taskEmployeesTable
      (pgInt4 taskId, pgInt4 employeeId)
  return ()
