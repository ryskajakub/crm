module Crm.Server.Api.PhotoResource ( 
  photoResource ) where

import           Opaleye.RunQuery            (runQuery)

import           Rest.Resource               (Resource, Void, schema, name, 
                                             get, mkResourceReaderWith, remove)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (fileO, jsonO, stringO)
import           Rest.Handler                (Handler)

import qualified Data.ByteString.Base64.Lazy as BB64

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)

import qualified Crm.Shared.Photo            as P

import qualified Crm.Shared.Api              as A
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Helpers
import           Crm.Server.Handler          (mkConstHandler', deleteRows'')

import Data.ByteString.Lazy.Char8 as CH8

photoResource :: Resource Dependencies (IdDependencies' P.PhotoId) P.PhotoId Void Void
photoResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.photos ,
  schema = S.noListing $ S.unnamedSingleRead id ,
  remove = Just removeHandler ,
  get = Just getPhotoHandler }


removeHandler :: Handler (IdDependencies' P.PhotoId)
removeHandler = mkConstHandler' jsonO $ do
  ((_, connection), P.PhotoId photoIdInt) <- ask
  deleteRows'' [
    createDeletion machinePhotosTable , 
    createDeletion photosMetaTable ,
    flip deletePhoto ] photoIdInt connection


getPhotoHandler :: Handler (IdDependencies' P.PhotoId)
getPhotoHandler = mkConstHandler' stringO $ do
  ((_, conn), P.PhotoId photoIdInt) <- ask
  photo <- liftIO $ getMachinePhoto conn photoIdInt
  photoMetas <- liftIO $ runQuery conn (photoMetaQuery photoIdInt)
  photoMeta <- singleRowOrColumn photoMetas
  let (_, mimeType, _) = photoMeta :: (Int, String, String)
  return (CH8.unpack . BB64.encode $ photo)
