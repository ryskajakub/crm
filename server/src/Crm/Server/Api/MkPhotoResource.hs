module Crm.Server.Api.MkPhotoResource
  (mkPhotoResource) where


import           Opaleye                     (Table)
import           Opaleye.QueryArr            (Query)
import           Opaleye.RunQuery            (runQuery)
import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgInt4)

import           Data.Pool                   (withResource)

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
import           Crm.Server.Database.Types   (DBInt)
import           Crm.Server.DB
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Handler          (mkInputHandler', mkListing')


mkPhotoResource :: 
  (a -> Int) ->
  (a -> Query PhotosMetaTable) ->
  Table (DBInt, DBInt) b ->
  Resource (IdDependencies' a) (IdDependencies' a) Void () Void
mkPhotoResource toInt readPhotos table = mkResourceId {
  name = A.photos ,
  schema = S.withListing () $ S.named [] ,
  create = Just $ addPhotoHandler toInt table ,
  list = const $ listPhotoHandler readPhotos }

addPhotoHandler :: 
  (a -> Int) ->
  Table (DBInt, DBInt) b ->
  Handler (IdDependencies' a)
addPhotoHandler toInt table = mkInputHandler' (fileI . jsonO) $ \photo -> do 
  ((_, pool), wrappedInt) <- ask
  newPhotoIds <- withResource pool $ \connection -> liftIO $ addPhoto connection photo
  newPhotoId <- singleRowOrColumn newPhotoIds
  _ <- withResource pool $ \connection -> liftIO $ runInsert 
    connection table (pgInt4 newPhotoId, pgInt4 . toInt $ wrappedInt)
  return $ P.PhotoId newPhotoId

listPhotoHandler :: 
  (a -> Query PhotosMetaTable) ->
  ListHandler (IdDependencies' a)
listPhotoHandler readPhotos = mkListing' jsonO $ const $ do
  ((_, pool), wrappedInt) <- ask
  rows <- withResource pool $ \connection -> liftIO $ 
    runQuery connection (readPhotos wrappedInt)
  return $ (convert rows :: [PhotoMetaMapped])
