{-# LANGUAGE ScopedTypeVariables #-}

module Crm.Server.Api.TaskResource (
  resource ) where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)

import           Data.Pool                   (withResource)
import           Data.Tuple.All              (sel1, sel2)

import           Opaleye                     (pgDay, pgInt4, pgStrictText,
                                              runQuery, runUpdate, (.==))

import           Rest.Dictionary.Combinators (jsonI, jsonO)
import           Rest.Handler                (Handler)
import           Rest.Resource               (Resource, Void, get,
                                              mkResourceReaderWith, name,
                                              schema, update)
import qualified Rest.Schema                 as S

import           TupleTH                     (proj)

import qualified Crm.Shared.Task             as T
import qualified Crm.Shared.YearMonthDay     as YMD

import           Crm.Server.Boilerplate      ()
import           Crm.Server.DB
import           Crm.Server.Handler
import           Crm.Server.Helpers          (maybeToNullable,
                                              prepareReaderTuple)
import           Crm.Server.Types


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
  let mappedTask = sel2 (convert singleTask :: TaskMapped)
  return mappedTask


closeTask :: Handler (IdDependencies' T.TaskId)
closeTask = mkInputHandler' (jsonI . jsonO) $ \task -> do
  ((_, pool), T.TaskId taskIdInt) <- ask
  _ <- liftIO $ withResource pool $ \connection -> runUpdate
    connection
    tasksTable
    (\taskRow -> (Just . sel1 $ taskRow, pgDay . YMD.ymdToDay . T.startDate $ task,
      pgStrictText . T.description $ task, maybeToNullable . fmap (pgDay . YMD.ymdToDay) . T.endDate $ task))
    (\taskRow -> sel1 taskRow .== pgInt4 taskIdInt)
  return ()

