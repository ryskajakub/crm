{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Crm.Server.Api.TaskResource (
  resource ) where

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

import           Data.Pool                   (withResource)
import           Data.Tuple.All              (sel1)

import           Opaleye                     (runQuery, pgBool, runInsert, pgDay, pgString, pgStrictText,
                                             runInsertReturning, pgInt4)

import           Rest.Resource               (Resource, Void, schema, name, create, get, mkResourceReaderWith)
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
  get = Just getTask }


getTask :: Handler (IdDependencies' T.TaskId)
getTask = mkConstHandler' jsonO $ do
  ((_, pool), taskId) <- ask
  task <- withResource pool $ \connection -> liftIO $ runQuery connection (getTaskQuery taskId)
  singleTask <- singleRowOrColumn task
  let mappedTask = $(proj 2 1) (convert singleTask :: TaskMapped)
  return mappedTask
