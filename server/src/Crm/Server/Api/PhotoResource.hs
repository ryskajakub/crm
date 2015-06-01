module Crm.Server.Api.PhotoResource ( 
  photoResource ) where

import           Opaleye.RunQuery            (runQuery)

import           Rest.Resource               (Resource, Void, schema, name, 
                                             get, mkResourceReaderWith, remove)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (fileO)
import           Rest.Handler                (Handler)

import           Control.Monad.IO.Class      (liftIO)

import qualified Crm.Shared.Api              as A
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Helpers
import           Crm.Server.Handler          (mkConstHandler')

photoResource :: Resource Dependencies IdDependencies UrlId Void Void
photoResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.photos ,
  schema = S.noListing $ S.unnamedSingle readMay' ,
  remove = Just removeHandler ,
  get = Just getPhotoHandler }

removeHandler :: Handler IdDependencies
removeHandler = deleteRows' [
  createDeletion machinePhotosTable , 
  createDeletion photosMetaTable ,
  flip deletePhoto ]

getPhotoHandler :: Handler IdDependencies
getPhotoHandler = mkConstHandler' fileO $ withConnId (\conn photoId -> do
  photo <- liftIO $ getMachinePhoto conn photoId
  photoMetas <- liftIO $ runQuery conn (photoMetaQuery photoId)
  photoMeta <- singleRowOrColumn photoMetas
  let (_, mimeType, _) = photoMeta :: (Int, String, String)
  return (photo, mimeType))
