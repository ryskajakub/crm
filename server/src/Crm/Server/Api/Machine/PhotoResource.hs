module Crm.Server.Api.Machine.PhotoResource ( 
  photoResource ) where

import Data.ByteString.Lazy.Char8 (pack)

import Opaleye.RunQuery (runQuery)

import Rest.Resource (Resource, Void, schema, name, create, mkResourceId, get)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (fileI, someI, fileO, someO)
import Rest.Handler (mkInputHandler, Handler, mkConstHandler)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import qualified Crm.Shared.Api as A
import Crm.Server.Types
import Crm.Server.DB (addMachinePhoto, machinePhotosQuery)
import Crm.Server.Helpers (maybeId, readMay')

photoResource :: Resource IdDependencies IdDependencies UrlId Void Void
photoResource = mkResourceId {
  name = A.photos ,
  schema = S.noListing $ S.unnamedSingle readMay' ,
  create = Just addPhotoHandler ,
  get = Just getPhoto }

addPhotoHandler :: Handler IdDependencies
addPhotoHandler = mkInputHandler (fileI . someI) (\photo -> do 
  (connection, maybeMachineIdInt) <- ask
  maybeId maybeMachineIdInt (\machineId ->
    liftIO $ addMachinePhoto connection machineId photo))

getPhoto :: Handler IdDependencies
getPhoto = mkConstHandler (fileO . someO) (do
  rows <- ask >>= (\(conn, machineId') -> maybeId machineId' (\machineId ->
    liftIO $ runQuery conn machinePhotosQuery))
  let (_, row) = head rows :: (Int, String)
  return (pack row, "image/jpeg"))
