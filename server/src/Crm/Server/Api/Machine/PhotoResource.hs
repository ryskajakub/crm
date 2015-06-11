module Crm.Server.Api.Machine.PhotoResource ( 
  photoResource ) where

import           Opaleye.RunQuery            (runQuery)
import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgInt4)

import           Rest.Resource               (Resource, Void, schema, name, create, mkResourceId, list)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (fileI, jsonO)
import           Rest.Handler                (Handler, ListHandler)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Photo            as P
import qualified Crm.Shared.Machine          as M

import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Handler          (mkInputHandler', mkListing')


photoResource :: Resource (IdDependencies' M.MachineId) (IdDependencies' M.MachineId) M.MachineId () Void
photoResource = mkResourceId {
  name = A.photos ,
  schema = S.withListing () $ S.named [] ,
  create = Just addPhotoHandler ,
  list = const listPhotoHandler }

addPhotoHandler :: Handler (IdDependencies' M.MachineId)
addPhotoHandler = mkInputHandler' (fileI . jsonO) $ \photo -> do 
  ((_, connection), M.MachineId machineIdInt) <- ask
  newPhotoIds <- liftIO $ addMachinePhoto connection machineIdInt photo
  newPhotoId <- singleRowOrColumn newPhotoIds
  _ <- liftIO $ runInsert connection machinePhotosTable (pgInt4 newPhotoId, pgInt4 machineIdInt)
  return $ P.PhotoId newPhotoId

listPhotoHandler :: ListHandler (IdDependencies' M.MachineId)
listPhotoHandler = mkListing' jsonO $ const $ do
  ((_, conn), M.MachineId machineIdInt) <- ask
  rows <- liftIO $ runQuery conn (machinePhotosByMachineId machineIdInt)
  return $ (convert rows :: [PhotoMetaMapped])
