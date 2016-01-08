module Crm.Server.Api.Upkeep.PhotoResource ( 
  resource ) where

import           Graphics.GD                 (loadJpegByteString, saveJpegByteString,
                                             rotateImage, resizeImage, imageSize, Image)

import           Opaleye.RunQuery            (runQuery)
import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgInt4)

import           Data.Pool                   (withResource)
import           Data.ByteString.Lazy        (fromStrict, toStrict, ByteString)

import           Rest.Resource               (Resource, Void, schema, name, create, mkResourceId, list)
import qualified Rest.Schema                 as S
import           Rest.Dictionary.Combinators (fileI, jsonO)
import           Rest.Handler                (Handler, ListHandler)

import           Control.Monad.Reader        (ask)
import           Control.Monad.IO.Class      (liftIO)

import qualified Crm.Shared.Api              as A
import qualified Crm.Shared.Photo            as P
import qualified Crm.Shared.Upkeep           as U

import           Crm.Server.Types
import           Crm.Server.DB
import           Crm.Server.Boilerplate      ()
import           Crm.Server.Handler          (mkInputHandler', mkListing')


resource :: Resource (IdDependencies' U.UpkeepId) (IdDependencies' U.UpkeepId) Void Void Void
resource = mkResourceId {
  name = A.photos ,
  schema = S.noListing $ S.named [] ,
  create = Just addPhotoHandler }

editPhoto :: ByteString -> IO ByteString
editPhoto =
  fmap fromStrict .
  (saveJpegByteString (-1) =<<) .
  (resize =<<) .
  (rotateImage 3 =<<) .
  loadJpegByteString .
  toStrict
  where
  resize :: Image -> IO Image
  resize image = do
    (width, height) <- imageSize image
    let 
      widthRatio = (fromIntegral width / (1140 :: Double)) :: Double
      futureHeight = (fromIntegral height) / widthRatio :: Double
      futureHeightInt = round futureHeight :: Int
    resizeImage 1140 futureHeightInt image

addPhotoHandler :: Handler (IdDependencies' U.UpkeepId)
addPhotoHandler = mkInputHandler' (fileI . jsonO) $ \photo -> do 
  ((_, pool), upkeepId) <- ask
  let U.UpkeepId upkeepIdInt = upkeepId
  photo' <- liftIO . editPhoto $ photo
  newPhotoIds <- withResource pool $ \connection -> liftIO $ addPhoto connection photo'
  newPhotoId <- singleRowOrColumn newPhotoIds
  _ <- withResource pool $ \connection -> liftIO $ runInsert 
    connection upkeepPhotosTable (pgInt4 newPhotoId, pgInt4 upkeepIdInt)
  return $ P.PhotoId newPhotoId
