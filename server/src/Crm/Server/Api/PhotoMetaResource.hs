{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Server.Api.PhotoMetaResource ( 
  photoMetaResource ) where

import           Prelude                     hiding (writeFile, readFile)

import           Graphics.GD                 (loadJpegByteString, saveJpegByteString,
                                             rotateImage, resizeImage, imageSize, Image)

import           Opaleye.Manipulation        (runInsert)
import           Opaleye.PGTypes             (pgInt4, pgStrictText, pgString)
import           Opaleye                     (runQuery)

import           Data.Pool                   (withResource)
import           Data.ByteString.Lazy        (fromStrict, toStrict, ByteString, writeFile, readFile)
import           Data.UnixTime               (getUnixTime, UnixTime(utMicroSeconds, utSeconds))
import           Data.Text                   (pack)
import           Data.Foldable               (for_)

import           System.Process              (system)
import           System.FilePath.Glob        (namesMatching)

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
import           Crm.Server.Helpers          (prepareReaderTuple, createDeletion)
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
  photoData <- liftIO $ withResource pool $ \connection -> getPhoto connection photoIdInt
  if 
    | mimeType == pack "application/pdf" -> do
      now <- liftIO getUnixTime
      let randomHash = (show . utSeconds $ now) ++ (show . utMicroSeconds $ now)
      let pdfFileName = "/tmp/" ++ randomHash ++ ".pdf"
      let jpegFileName = "/tmp/" ++ randomHash ++ ".jpeg"
      liftIO $ writeFile pdfFileName photoData
      _ <- liftIO . system $ "convert " ++ pdfFileName ++ " " ++ jpegFileName
      images <- liftIO . namesMatching $ "/tmp/" ++ randomHash ++ "*.jpeg"
      upkeepIdInt' <- withResource pool $ \connection -> do
        upkeepIdInts <- liftIO $ runQuery connection (upkeepForPhotoQ photoId)
        upkeepIdInt <- singleRowOrColumn upkeepIdInts
        liftIO $ createDeletion upkeepPhotosTable photoIdInt connection
        liftIO $ deletePhoto connection photoIdInt
        return upkeepIdInt
      for_ images $ \image ->
        withResource pool $ \connection -> do
          imageBits <- liftIO $ readFile image
          photoIdInts <- liftIO $ addPhoto connection imageBits
          photoIdInt' <- singleRowOrColumn photoIdInts
          _ <- liftIO $ runInsert connection photosMetaTable 
            (pgInt4 photoIdInt', pgString "image/jpeg", pgString image)
          _ <- liftIO $ runInsert connection upkeepPhotosTable
            (pgInt4 photoIdInt', pgInt4 upkeepIdInt')
          return ()
    | mimeType == pack "image/jpeg" -> do
      editedPhoto <- liftIO $ editPhoto (PM.source photoMeta == PM.IPhone) photoData
      _ <- liftIO $ withResource pool $ \connection -> updatePhoto connection photoIdInt editedPhoto
      _ <- liftIO $ withResource pool $ \connection -> runInsert connection photosMetaTable 
        (pgInt4 photoIdInt, pgStrictText mimeType, pgStrictText . PM.fileName $ photoMeta)
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
