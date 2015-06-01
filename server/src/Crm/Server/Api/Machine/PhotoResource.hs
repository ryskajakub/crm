module Crm.Server.Api.Machine.PhotoResource ( 
  photoResource ) where

import           Opaleye.RunQuery            (runQuery)
import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgInt4)

import           Rest.Resource               (Resource, Void, schema, name, create, mkResourceId, list)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (fileI, jsonO)
import           Rest.Handler                (Handler, ListHandler)

import           Control.Monad.IO.Class      (liftIO)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Photo            as P
import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Helpers          (withConnId, readMay')
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Handler          (mkInputHandler', mkListing')


photoResource :: Resource IdDependencies IdDependencies UrlId () Void
photoResource = mkResourceId {
  name = A.photos ,
  schema = S.withListing () $ S.unnamedSingle readMay' ,
  create = Just addPhotoHandler ,
  list = const listPhotoHandler }

addPhotoHandler :: Handler IdDependencies
addPhotoHandler = mkInputHandler' (fileI . jsonO) (\photo -> withConnId (\connection machineId -> do 
  newPhotoIds <- liftIO $ addMachinePhoto connection machineId photo
  newPhotoId <- singleRowOrColumn newPhotoIds
  _ <- liftIO $ runInsert connection machinePhotosTable (pgInt4 newPhotoId, pgInt4 machineId) 
  return $ P.PhotoId newPhotoId))

listPhotoHandler :: ListHandler IdDependencies
listPhotoHandler = mkListing' jsonO $ const $ withConnId (\conn machineId -> do 
  rows <- liftIO $ runQuery conn (machinePhotosByMachineId machineId)
  return $ (convert rows :: [PhotoMetaMapped]))
