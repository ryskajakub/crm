module Crm.Server.Api.Machine.PhotoResource ( 
  photoResource ) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (ByteString)

import Opaleye.RunQuery (runQuery)

import Rest.Resource (Resource, Void, schema, name, create, mkResourceId, get)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (fileI, someI, fileO, someO, jsonO)
import Rest.Handler (mkInputHandler, Handler, mkConstHandler)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import qualified Crm.Shared.Api as A
import Crm.Server.Types
import Crm.Server.DB (addMachinePhoto, machinePhotosQuery, singleRowOrColumn, getMachinePhoto)
import Crm.Server.Helpers (maybeId, readMay')

photoResource :: Resource IdDependencies IdDependencies UrlId Void Void
photoResource = mkResourceId {
  name = A.photos ,
  schema = S.noListing $ S.unnamedSingle readMay' ,
  create = Just addPhotoHandler ,
  get = Just getPhoto }

addPhotoHandler :: Handler IdDependencies
addPhotoHandler = mkInputHandler (fileI . someI . jsonO . someO) (\photo -> do 
  (connection, maybeMachineIdInt) <- ask
  newIds <- maybeId maybeMachineIdInt (\machineId ->
    liftIO $ addMachinePhoto connection machineId photo)
  singleRowOrColumn newIds )

getPhoto :: Handler IdDependencies
getPhoto = mkConstHandler (fileO . someO) (do
  photo <- ask >>= (\(conn, machineId') -> maybeId machineId' (\machineId ->
    liftIO $ getMachinePhoto conn ))
  return (photo, "image/jpeg"))
