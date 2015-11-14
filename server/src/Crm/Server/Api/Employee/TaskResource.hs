{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.Employee.TaskResource (
  resource ) where

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

import           Data.Pool                   (withResource)
import           Data.Tuple.All              (sel1)
import           Data.Text                   (pack)

import           Opaleye                     (runQuery, pgBool, runInsert, pgDay, pgString, pgStrictText,
                                             runInsertReturning, pgInt4)

import           Rest.Resource               (Resource, Void, schema, name, create,
                                             list, mkResourceId)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (ListHandler, Handler)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Employee         as E
import qualified Crm.Shared.Task             as T
import qualified Crm.Shared.ServerRender     as SR

import           Crm.Server.DB
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.Handler
import           Crm.Server.Helpers          (ymdToDay, maybeToNullable, catchError)
import           Crm.Server.Parsers          (parseMarkup)


data TasksListing = TasksListingNormal | TasksListingMarkup

tasksListingMarkup :: ListHandler (IdDependencies' E.EmployeeId)
tasksListingMarkup = mkGenHandler' jsonO $ \env -> do
  ((_, pool), employeeId) <- ask
  withResource pool $ \connection -> liftIO $ do
    tasks <- runQuery connection (tasksForEmployeeQuery employeeId)
    let 
      employeeTasks = convert tasks :: [TaskMapped]
      mkEmployeeTasksMarkup = map $ \(taskId, task) -> let
        taskMarkup = task { 
          T.description = maybe ((:[]). SR.PlainText . pack $ "") (\x -> x) . 
            catchError . parseMarkup . T.description $ task }
        in taskMarkup
    return . mkEmployeeTasksMarkup $ employeeTasks

resource :: Resource (IdDependencies' E.EmployeeId) (IdDependencies' E.EmployeeId) Void TasksListing Void
resource = mkResourceId {
  name = "task" ,
  create = Just createHandler ,
  schema = S.withListing TasksListingNormal $ 
    S.named [("markup-tasks", S.listing TasksListingMarkup)] ,
  list = \tasksListingType -> case tasksListingType of
    TasksListingNormal -> tasksListing
    TasksListingMarkup -> tasksListingMarkup }

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
