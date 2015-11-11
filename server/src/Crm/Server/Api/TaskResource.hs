module Crm.Server.Api.TaskResource (
  resource ) where

import           Opaleye
import           Opaleye                       (runQuery, pgDay, pgBool, pgStrictText, pgString)

import           Data.Pool                     (withResource)

import           Control.Monad.Reader          (ask)
import           Control.Monad.IO.Class        (liftIO)

import           Rest.Resource                 (Resource, Void, schema, name, create,
                                               list, mkResourceReaderWith)
import qualified Rest.Schema                   as S
import           Rest.Dictionary.Combinators   (jsonO, jsonI)
import           Rest.Handler                  (Handler)

import           Crm.Server.Boilerplate        ()
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Handler
import           Crm.Server.Helpers            (prepareReaderTuple, createDeletion, ymdToDay)

import qualified Crm.Shared.EmployeeTask       as ET


createHandler :: Handler Dependencies
createHandler = mkInputHandler' (jsonO . jsonI) $ \newTask -> do
  (_, pool) <- ask
  withResource pool $ \connection -> liftIO $ runInsert
    connection
    upkeepsTable
    (Nothing, pgDay . ymdToDay . ET.date $ newTask, pgBool False, pgString "", 
      pgStrictText . ET.task $ newTask, pgString "")
  return ()

resource :: Resource Dependencies (IdDependencies' ET.EmployeeTaskId) ET.EmployeeTaskId Void Void
resource = (mkResourceReaderWith prepareReaderTuple) {
  name = "task" ,
  schema = S.noListing . S.named $ [] ,
  create = Just createHandler }
