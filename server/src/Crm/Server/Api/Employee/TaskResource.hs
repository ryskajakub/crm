{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.Employee.TaskResource (
  resource ) where

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Arrow               (second)

import           Data.Pool                   (withResource)
import           Data.Tuple.All              (sel1)
import           Data.Text                   (pack, append)
import qualified Data.Text                   as TT
import           Data.Aeson.Types            (ToJSON)
import           Data.Typeable               (Typeable)
import           Data.JSON.Schema            (JSONSchema)
import           Data.Maybe                  (isJust)

import           Opaleye                     (runQuery, pgBool, runInsert, pgDay, pgString, pgStrictText,
                                             runInsertReturning, pgInt4)

import           Rest.Resource               (Resource, Void, schema, name, create,
                                             list, mkResourceId, get)
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


type Task' = (T.TaskId, T.Task)

tasksListing' :: ConnectionPool -> E.EmployeeId -> ([Task'] -> [Task'] -> a) -> IO a
tasksListing' pool employeeId modifyTasks =
  withResource pool $ \connection -> do
    tasks <- runQuery connection (tasksForEmployeeQuery employeeId)
    let 
      employeeTasks = convert tasks :: [TaskMapped]
      isClosed = isJust . T.endDate . snd
      isOpen = not . isClosed
      closedTasks = filter isClosed employeeTasks
      openTasks = filter isOpen employeeTasks
    return . modifyTasks openTasks $ closedTasks

tasksListingMarkup :: ListHandler (IdDependencies' E.EmployeeId)
tasksListingMarkup = mkListing' jsonO $ const $ do
  ((_, pool), employeeId) <- ask
  let 
    mkEmployeeTasksMarkup = map $ \(taskId, task) -> let
      taskMarkup = task { 
        T.description = maybe ((:[]). SR.PlainText . pack $ "") (\x -> x) . 
          catchError . parseMarkup . T.description $ task }
      in (taskId, taskMarkup)
  liftIO $ tasksListing' pool employeeId (\o _ -> mkEmployeeTasksMarkup o)

resource :: Resource (IdDependencies' E.EmployeeId) (IdDependencies' E.EmployeeId) () () Void
resource = mkResourceId {
  name = "task" ,
  create = Just createHandler ,
  get = Just tasksListing ,
  schema = S.withListing () $ 
    S.named [
      ("markup-tasks", S.listing ()) ,
      ("tasks", S.single ()) ] ,
  list = const tasksListingMarkup }

tasksListing :: Handler (IdDependencies' E.EmployeeId)
tasksListing = mkConstHandler' jsonO $ do
  ((_, pool), employeeId) <- ask
  liftIO $ tasksListing' pool employeeId (\o c -> (trimTo50tasks o, trimTo50tasks c)) where
    trimTo50tasks tasks = fmap (second trimTo50) tasks
    trimTo50 task = task { T.description = if (50 <) . TT.length . T.description $ task 
      then (`append` (pack "...")) . TT.take 50 . T.description $ task
      else T.description task }

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
