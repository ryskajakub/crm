module Crm.Server.Api.PhotoResource ( 
  photoResource ) where

import           Opaleye.RunQuery            (runQuery)

import           Rest.Resource               (Resource, Void, schema, name, 
                                             get, mkResourceReaderWith, remove)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (fileO, jsonO)
import           Rest.Handler                (Handler)

import qualified Data.ByteString.Base64.Lazy as BB64
import           Data.Pool                   (withResource)

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)

import qualified Crm.Shared.Photo            as P

import qualified Crm.Shared.Api              as A
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Helpers
import           Crm.Server.Handler          (mkConstHandler', deleteRows'')


photoResource :: Resource Dependencies (IdDependencies' P.PhotoId) P.PhotoId Void Void
photoResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.photos ,
  schema = S.noListing $ S.unnamedSingleRead id ,
  remove = Just removeHandler ,
  get = Just getPhotoHandler }


removeHandler :: Handler (IdDependencies' P.PhotoId)
removeHandler = mkConstHandler' jsonO $ do
  ((_, pool), P.PhotoId photoIdInt) <- ask
  deleteRows'' [
    createDeletion upkeepPhotosTable ,
    createDeletion machinePhotosTable , 
    createDeletion photosMetaTable ,
    flip deletePhoto ] photoIdInt pool


getPhotoHandler :: Handler (IdDependencies' P.PhotoId)
getPhotoHandler = mkConstHandler' fileO $ do
  ((_, pool), P.PhotoId photoIdInt) <- ask
  photo <- withResource pool $ \connection -> liftIO $ getPhoto connection photoIdInt
  photoMetas <- withResource pool $ \connection -> liftIO $ runQuery connection (photoMetaQuery photoIdInt)
  photoMeta <- singleRowOrColumn photoMetas
  let (_, _, fileName) = photoMeta :: (Int, String, String)
  return (BB64.encode photo, fileName, False)
