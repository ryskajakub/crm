{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.TaskResource (
  resource ) where

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

import           Data.Pool                   (withResource)
import           Data.Tuple.All              (sel1)

import           Opaleye                     (runQuery, pgBool, runInsert, pgDay, pgString, pgStrictText,
                                             runInsertReturning, pgInt4, runUpdate, (.==))

import           Rest.Resource               (Resource, Void, schema, name, create, get, mkResourceReaderWith, update) 
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonO, jsonI)
import           Rest.Handler                (ListHandler, Handler)

import           TupleTH                     (proj)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Employee         as E
import qualified Crm.Shared.Task             as T

import           Crm.Server.DB
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Types
import           Crm.Server.Handler
import           Crm.Server.Helpers          (ymdToDay, maybeToNullable, prepareReaderTuple)


resource :: Resource Dependencies (IdDependencies' T.TaskId) T.TaskId Void Void
resource = (mkResourceReaderWith prepareReaderTuple) {
  name = "task" ,
  schema = S.noListing $ S.unnamedSingleRead id ,
  get = Just getTask ,
  update = Just closeTask }


getTask :: Handler (IdDependencies' T.TaskId)
getTask = mkConstHandler' jsonO $ do
  ((_, pool), taskId) <- ask
  task <- withResource pool $ \connection -> liftIO $ runQuery connection (getTaskQuery taskId)
  singleTask <- singleRowOrColumn task
  let mappedTask = $(proj 2 1) (convert singleTask :: TaskMapped)
  return mappedTask


closeTask :: Handler (IdDependencies' T.TaskId)
closeTask = mkInputHandler' (jsonI . jsonO) $ \task -> do
  ((_, pool), T.TaskId taskIdInt) <- ask
  _ <- liftIO $ withResource pool $ \connection -> runUpdate
    connection
    tasksTable
    (\taskRow -> (Just . $(proj 4 0) $ taskRow, pgDay . ymdToDay . T.startDate $ task,
      pgStrictText . T.description $ task, maybeToNullable . fmap (pgDay . ymdToDay) . T.endDate $ task))
    (\taskRow -> $(proj 4 0) taskRow .== pgInt4 taskIdInt)
  return ()

