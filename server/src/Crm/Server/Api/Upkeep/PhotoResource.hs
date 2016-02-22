module Crm.Server.Api.Upkeep.PhotoResource ( 
  resource ) where

import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgInt4)

import           Data.Pool                   (withResource)

import           Rest.Resource               (Resource, Void, schema, name, create, mkResourceId)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (fileI, jsonO)
import           Rest.Handler                (Handler)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Photo            as P
import qualified Crm.Shared.Upkeep           as U

import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Handler          (mkInputHandler')


resource :: Resource (IdDependencies' U.UpkeepId) (IdDependencies' U.UpkeepId) Void Void Void
resource = mkResourceId {
  name = A.photos ,
  schema = S.noListing $ S.named [] ,
  create = Just addPhotoHandler }

addPhotoHandler :: Handler (IdDependencies' U.UpkeepId)
addPhotoHandler = mkInputHandler' (fileI . jsonO) $ \photo -> do 
  ((_, pool), upkeepId) <- ask
  let U.UpkeepId upkeepIdInt = upkeepId
  newPhotoIds <- withResource pool $ \connection -> liftIO $ addPhoto connection photo
  newPhotoId <- singleRowOrColumn newPhotoIds
  _ <- withResource pool $ \connection -> liftIO $ runInsert 
    connection upkeepPhotosTable (pgInt4 newPhotoId, pgInt4 upkeepIdInt)
  return $ P.PhotoId newPhotoId
