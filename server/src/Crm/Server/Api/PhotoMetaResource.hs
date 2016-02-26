{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Server.Api.PhotoMetaResource ( 
  photoMetaResource ) where

import           Prelude                     hiding (writeFile)

import           Graphics.GD                 (loadJpegByteString, saveJpegByteString,
                                             rotateImage, resizeImage, imageSize, Image)

import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgInt4, pgStrictText)

import           Data.Pool                   (withResource)
import           Data.ByteString.Lazy        (fromStrict, toStrict, ByteString, writeFile)
import           Data.UnixTime               (getUnixTime, UnixTime(utMicroSeconds, utSeconds))
import           Data.Text                   (pack)

import           System.Process              (system)

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
import           Crm.Server.DB
import           Crm.Server.Helpers          (prepareReaderTuple)
import           Crm.Server.Handler          (mkInputHandler')

photoMetaResource :: Resource Dependencies (IdDependencies' P.PhotoId) P.PhotoId Void Void
photoMetaResource = (mkResourceReaderWith prepareReaderTuple) {
  name = A.photoMeta ,
  schema = S.noListing $ S.unnamedSingleRead id ,
  update = Just setPhotoMetaDataHandler }

setPhotoMetaDataHandler :: Handler (IdDependencies' P.PhotoId)
setPhotoMetaDataHandler = mkInputHandler' jsonI $ \photoMeta -> do
  ((_, pool), photoId) <- ask
  let photoIdInt = P.getPhotoId photoId
  let mimeType = PM.mimeType photoMeta
  _ <- liftIO $ withResource pool $ \connection -> runInsert connection photosMetaTable 
    (pgInt4 photoIdInt, pgStrictText mimeType, pgStrictText . PM.fileName $ photoMeta)
  photoData <- liftIO $ withResource pool $ \connection -> getPhoto connection photoIdInt
  if 
    | mimeType == pack "application/pdf" -> liftIO $ do
      now <- getUnixTime
      let randomHash = (show . utSeconds $ now) ++ (show . utMicroSeconds $ now)
      let pdfFileName = "/tmp/" ++ randomHash ++ ".pdf"
      let jpegFileName = "/tmp/" ++ randomHash ++ ".jpeg"
      writeFile pdfFileName photoData
      _ <- system $ "convert " ++ pdfFileName ++ " " ++ jpegFileName
      return ()
    | mimeType == pack "image/jpeg" -> do
      editedPhoto <- liftIO $ editPhoto (PM.source photoMeta == PM.IPhone) photoData
      _ <- liftIO $ withResource pool $ \connection -> updatePhoto connection photoIdInt editedPhoto
      return ()

editPhoto :: Bool -> ByteString -> IO ByteString
editPhoto rotateFlag =
  fmap fromStrict .
  (saveJpegByteString (-1) =<<) .
  (resize =<<) .
  (rotate =<<) .
  loadJpegByteString .
  toStrict
  where
  rotate = if rotateFlag
    then rotateImage 3
    else return
  resize :: Image -> IO Image
  resize image = do
    (width, height) <- imageSize image
    let 
      widthRatio = (fromIntegral width / (1140 :: Double)) :: Double
      futureHeight = (fromIntegral height) / widthRatio :: Double
      futureHeightInt = round futureHeight :: Int
    resizeImage 1140 futureHeightInt image
