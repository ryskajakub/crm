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
import qualified Crm.Shared.EmployeeTask       as ET

import           Crm.Server.DB
import           Crm.Server.Boilerplate        ()
import           Crm.Server.Types
import           Crm.Server.Handler
import           Crm.Server.Helpers            (ymdToDay)


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
    upkeeps <- runQuery connection (tasksForEmployeeQuery employeeId)
    let employeeTasks = convert upkeeps :: [EmployeeTaskMapped]
    return employeeTasks

createHandler :: Handler (IdDependencies' E.EmployeeId)
createHandler = mkInputHandler' (jsonO . jsonI) $ \newTask -> do
  ((_, pool), (E.EmployeeId employeeId)) <- ask
  withResource pool $ \connection -> do
    upkeepIds <- liftIO $ runInsertReturning
      connection
      upkeepsTable
      (Nothing, pgDay . ymdToDay . ET.date $ newTask, pgBool False, pgString "", 
        pgStrictText . ET.task $ newTask, pgString "")
      sel1
    (upkeepId :: Int) <- singleRowOrColumn upkeepIds
    liftIO $ runInsert
      connection
      upkeepEmployeesTable      
      (pgInt4 upkeepId, pgInt4 employeeId, pgInt4 0)
  return ()
