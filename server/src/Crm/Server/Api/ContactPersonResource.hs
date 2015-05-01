module Crm.Server.Api.ContactPersonResource (resource) where

import Opaleye (runQuery, (.==), pgInt4, pgString, runUpdate)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Data.Tuple.All (sel1, sel2, sel3)

import Rest.Resource (Resource, Void, schema, list, name, mkResourceReaderWith, get, update)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO, jsonI, someI)
import Rest.Handler (Handler, mkConstHandler, mkInputHandler)

import qualified Crm.Shared.Api as A
import qualified Crm.Shared.ContactPerson as CP

import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB
import Crm.Server.Helpers (prepareReaderTuple, withConnId, readMay')

resource :: Resource Dependencies IdDependencies UrlId Void Void
resource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.contactPersons ,
  schema = S.noListing $ S.unnamedSingle readMay' ,
  update = Just updateHandler ,
  get = Just getHandler }

getHandler :: Handler IdDependencies
getHandler = mkConstHandler (jsonO . someO) $ withConnId (\connection theId -> do
  rows <- liftIO $ runQuery connection (singleContactPersonQuery theId)
  row <- singleRowOrColumn rows
  return $ sel3 $ (convert row :: ContactPersonMapped))

updateHandler :: Handler IdDependencies
updateHandler = mkInputHandler (jsonO . jsonI . someI . someO) (\contactPerson -> withConnId (\conn theId -> liftIO $ let
  readToWrite row = (Nothing, sel2 row, pgString $ CP.name contactPerson ,
    pgString $ CP.phone contactPerson, pgString $ CP.position contactPerson)
  condition row = sel1 row .== pgInt4 theId
  in runUpdate conn contactPersonsTable readToWrite condition))
