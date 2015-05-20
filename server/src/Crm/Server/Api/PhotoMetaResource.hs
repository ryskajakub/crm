module Crm.Server.Api.PhotoMetaResource ( 
  photoMetaResource ) where

import Opaleye.Manipulation (runInsert)
import Opaleye.PGTypes (pgInt4, pgStrictText)

import Rest.Resource (Resource, Void, schema, name, mkResourceReaderWith, update)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (jsonI)
import Rest.Handler (mkInputHandler, Handler)

import Control.Monad.IO.Class (liftIO)

import Crm.Server.Boilerplate ()
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.PhotoMeta as PM
import Crm.Server.Types
import Crm.Server.DB (photosMetaTable)
import Crm.Server.Helpers (withConnId, readMay', prepareReaderTuple)

photoMetaResource :: Resource Dependencies IdDependencies UrlId Void Void
photoMetaResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.photoMeta ,
  schema = S.noListing $ S.unnamedSingle readMay' ,
  update = Just setPhotoMetaDataHandler }

setPhotoMetaDataHandler :: Handler IdDependencies
setPhotoMetaDataHandler = mkInputHandler (jsonI) (\photoMeta -> withConnId (\conn photoId -> do
  _ <- liftIO $ runInsert conn photosMetaTable 
    (pgInt4 photoId, pgStrictText $ PM.mimeType photoMeta, pgStrictText $ PM.fileName photoMeta)
  return ()))
