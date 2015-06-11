module Crm.Server.Api.PhotoMetaResource ( 
  photoMetaResource ) where

import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgInt4, pgStrictText)

import           Rest.Resource               (Resource, Void, schema, name, 
                                             mkResourceReaderWith, update)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (jsonI)
import           Rest.Handler                (Handler)

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Reader        (ask)

import           Crm.Server.Boilerplate      ()
import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.PhotoMeta        as PM
import qualified Crm.Shared.Photo            as P
import           Crm.Server.Types
import           Crm.Server.DB               (photosMetaTable)
import           Crm.Server.Helpers          (prepareReaderTuple)
import           Crm.Server.Handler          (mkInputHandler')

photoMetaResource :: Resource Dependencies (IdDependencies' P.PhotoId) P.PhotoId Void Void
photoMetaResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.photoMeta ,
  schema = S.noListing $ S.unnamedSingleRead id ,
  update = Just setPhotoMetaDataHandler }

setPhotoMetaDataHandler :: Handler (IdDependencies' P.PhotoId)
setPhotoMetaDataHandler = mkInputHandler' jsonI $ \photoMeta -> do
  ((_,connection), photoId) <- ask
  _ <- liftIO $ runInsert connection photosMetaTable 
    (pgInt4 $ P.getPhotoId photoId, pgStrictText $ PM.mimeType photoMeta, pgStrictText $ PM.fileName photoMeta)
  return ()
