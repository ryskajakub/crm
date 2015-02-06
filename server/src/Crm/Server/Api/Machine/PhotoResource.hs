module Crm.Server.Api.Machine.PhotoResource ( 
  photoResource ) where

import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (ByteString)

import Opaleye.RunQuery (runQuery)
import Opaleye.Manipulation (runInsert)
import Opaleye.PGTypes (pgInt4)

import Rest.Resource (Resource, Void, schema, name, create, mkResourceId, get, list)
import qualified Rest.Schema as S
import Rest.Dictionary.Combinators (fileI, someI, fileO, someO, jsonO)
import Rest.Handler (mkInputHandler, Handler, mkConstHandler, mkListing, ListHandler)

import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)

import qualified Crm.Shared.Api as A
import Crm.Server.Types
import Crm.Server.DB (addMachinePhoto, machinePhotosQuery, singleRowOrColumn, getMachinePhoto, 
  machinePhotosByMachineId, machinePhotosTable)
import Crm.Server.Helpers (maybeId, readMay')

photoResource :: Resource IdDependencies IdDependencies UrlId () Void
photoResource = mkResourceId {
  name = A.photos ,
  schema = S.withListing () $ S.unnamedSingle readMay' ,
  create = Just addPhotoHandler ,
  list = const listPhotoHandler }

addPhotoHandler :: Handler IdDependencies
addPhotoHandler = mkInputHandler (fileI . someI . jsonO . someO) (\photo -> do 
  (connection, maybeMachineIdInt) <- ask
  maybeId maybeMachineIdInt (\machineId -> do
    newPhotoIds <- liftIO $ addMachinePhoto connection machineId photo
    newPhotoId <- singleRowOrColumn newPhotoIds
    liftIO $ runInsert connection machinePhotosTable (pgInt4 newPhotoId, pgInt4 machineId) 
    return newPhotoId))

listPhotoHandler :: ListHandler IdDependencies
listPhotoHandler = mkListing (jsonO . someO) (const $
  ask >>= (\(conn, machineId') -> maybeId machineId' (\machineId ->
    liftIO $ (runQuery conn (machinePhotosByMachineId machineId) :: IO [Int] ))))
