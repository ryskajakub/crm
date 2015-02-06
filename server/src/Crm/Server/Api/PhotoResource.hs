module Crm.Server.Api.PhotoResource ( 
  photoResource ) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (ByteString)

import Opaleye.RunQuery (runQuery)

import Rest.Resource (Resource, Void, schema, name, create, mkResourceId, get, mkResourceReaderWith)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (fileI, someI, fileO, someO, jsonO)
import Rest.Handler (mkInputHandler, Handler, mkConstHandler)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import qualified Crm.Shared.Api as A
import Crm.Server.Types
import Crm.Server.DB (addMachinePhoto, machinePhotosQuery, singleRowOrColumn, getMachinePhoto)
import Crm.Server.Helpers (maybeId, readMay', prepareReaderTuple)

photoResource :: Resource Dependencies IdDependencies UrlId Void Void
photoResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.photos ,
  schema = S.noListing $ S.unnamedSingle readMay' ,
  get = Just getPhotoHandler }

getPhotoHandler :: Handler IdDependencies
getPhotoHandler = mkConstHandler (fileO . someO) (do
  photo <- ask >>= (\(conn, photoId') -> maybeId photoId' (\photoId ->
    liftIO $ getMachinePhoto conn ))
  return (photo, "image/jpeg"))
