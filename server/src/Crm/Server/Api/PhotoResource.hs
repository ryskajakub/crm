module Crm.Server.Api.PhotoResource ( 
  photoResource ) where

import Opaleye.RunQuery (runQuery)

import Rest.Resource (Resource, Void, schema, name, get, mkResourceReaderWith)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (fileO)
import Rest.Handler (Handler, mkConstHandler)

import Control.Monad.IO.Class (liftIO)

import qualified Crm.Shared.Api as A
import Crm.Server.Types
import Crm.Server.DB (singleRowOrColumn, getMachinePhoto, photoMetaQuery)
import Crm.Server.Helpers (withConnId, readMay', prepareReaderTuple)

photoResource :: Resource Dependencies IdDependencies UrlId Void Void
photoResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.photos ,
  schema = S.noListing $ S.unnamedSingle readMay' ,
  get = Just getPhotoHandler }

getPhotoHandler :: Handler IdDependencies
getPhotoHandler = mkConstHandler (fileO) $ withConnId (\conn photoId -> do
  photo <- liftIO $ getMachinePhoto conn photoId
  photoMetas <- liftIO $ runQuery conn (photoMetaQuery photoId)
  photoMeta <- singleRowOrColumn photoMetas
  let (_, mimeType, _) = photoMeta :: (Int, String, String)
  return (photo, mimeType))
