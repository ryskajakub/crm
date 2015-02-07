module Crm.Server.Api.PhotoMetaResource ( 
  photoMetaResource ) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (ByteString)

import Opaleye.RunQuery (runQuery)
import Opaleye.Manipulation (runInsert)
import Opaleye.PGTypes (pgInt4, pgString)

import Rest.Resource (Resource, Void, schema, name, create, mkResourceId, get, mkResourceReaderWith, 
  update)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (fileI, someI, fileO, someO, jsonO, jsonI)
import Rest.Handler (mkInputHandler, Handler, mkConstHandler)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import Crm.Server.Boilerplate ()
import qualified Crm.Shared.Api as A
import qualified Crm.Shared.PhotoMeta as PM
import Crm.Server.Types
import Crm.Server.DB (addMachinePhoto, machinePhotosQuery, singleRowOrColumn, getMachinePhoto, 
  photosMetaTable )
import Crm.Server.Helpers (maybeId, readMay', prepareReaderTuple)

photoMetaResource :: Resource Dependencies IdDependencies UrlId Void Void
photoMetaResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.photoMeta ,
  schema = S.noListing $ S.unnamedSingle readMay' ,
  update = Just setPhotoMetaDataHandler }

setPhotoMetaDataHandler :: Handler IdDependencies
setPhotoMetaDataHandler = mkInputHandler (jsonI . someI) (\photoMeta -> do
  _ <- ask >>= (\(conn, photoId') -> maybeId photoId' (\photoId ->
    liftIO $ runInsert conn photosMetaTable (pgInt4 photoId, pgString $ PM.mimeType photoMeta)))
  return ())