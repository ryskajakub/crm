module Crm.Server.Api.ContactPersonResource (resource) where

import Opaleye.RunQuery (runQuery)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Data.Tuple.All (sel3)

import Rest.Resource (Resource, Void, schema, list, name, mkResourceReaderWith, get)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonO, someO)
import Rest.Handler (Handler, mkConstHandler)

import qualified Crm.Shared.Api as A

import Crm.Server.Boilerplate ()
import Crm.Server.Types
import Crm.Server.DB
import Crm.Server.Helpers (prepareReaderTuple, maybeId, readMay')

resource :: Resource Dependencies IdDependencies UrlId Void Void
resource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.contactPersons ,
  schema = S.noListing $ S.unnamedSingle readMay' ,
  get = Just getHandler }

getHandler :: Handler IdDependencies
getHandler = mkConstHandler (jsonO . someO) $ do
  (connection, maybeInt) <- ask
  maybeId maybeInt (\theId -> do
    rows <- liftIO $ runQuery connection (singleContactPersonQuery theId)
    row <- singleRowOrColumn rows
    return $ sel3 $ mapContactPerson row)
