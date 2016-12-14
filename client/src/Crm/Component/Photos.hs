{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Crm.Component.Photos where

import           Prelude                          hiding (span, div, elem, id)
import           Data.Text                        (fromString, showInt, (<>), pack)
import           FFI                              (Defined(..))

import           HaskellReact
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Bootstrap.Button    as BTN
import qualified HaskellReact.Tag.Image           as IMG
import qualified HaskellReact.Bootstrap.Modal     as BM
import           HaskellReact.Bootstrap.Carousel  (carousel)

import qualified JQuery                           as JQ
import qualified Crm.Runtime                      as Runtime
import qualified Crm.Shared.Api                   as A

import qualified Crm.Shared.Photo                 as P
import           Crm.Server                       (deletePhoto)
import           Crm.Helpers                      (reload)

import           Crm.Router 

data PhotoModal = PhotoModal {
  photoModalElement :: DOMElement ,
  mkPhotoModalDisplayButton :: [P.PhotoId] -> ([P.PhotoId] -> Fay ()) -> [DOMElement] ,
  fetchPhotosToModal :: Fay () }

mkPhotoModal :: [P.PhotoId] -> Bool -> CrmRouter -> PhotoModal
mkPhotoModal photoIds deletable router = let

  BM.ModalPair modalButtonProps modalElementBase = BM.mkModalPair 

  photoModalElement' = modalElementBase . div' (class' "upkeep-photos") . map mkPhotoRegion $ photoIds
    where
    mkPhotoRegion photoId = div [
      let
        buttonProps = BTN.buttonProps {
          BTN.disabled = Defined . not $ deletable ,
          BTN.bsStyle = Defined "danger" ,
          BTN.onClick = Defined $ const clickHandler }
        clickHandler = deletePhoto photoId reload router
        in BTN.button' buttonProps [G.arrowDown, text2DOM " Smazat fotku"] ,
      IMG.image' 
        (mkAttrs { id = Defined . (<>) "photo-" . showInt . P.getPhotoId $ photoId})
        ((IMG.mkImageAttrs "") { IMG.width = Defined 1140 } ) ]
  
  displayPhotos differentPhotoIds setPhotosInModal = let
    clickHandler = setPhotosInModal differentPhotoIds
    in case differentPhotoIds of
      [] -> []
      _ -> [BTN.buttonP' modalButtonProps BTN.NormalButton BTN.DefaultButton (const clickHandler)
        [G.picture, span $ " (" <> (showInt . length $ differentPhotoIds) <> ")" ]]

  in PhotoModal photoModalElement' displayPhotos (fetchPhotos photoIds router)

fetchPhotos :: [P.PhotoId] -> CrmRouter -> Fay ()
fetchPhotos photoIds router = forM_ photoIds $ \photoId -> Runtime.passwordAjax
  (pack A.photos <> "/" <> (showInt . P.getPhotoId $ photoId))
  (\imageData -> do
    let photoHTMLId = ((<>) "#photo-" . showInt . P.getPhotoId $ photoId)
    photoHtmlElement <- JQ.select photoHTMLId
    _ <- JQ.setAttr "src" ("data:image/jpeg;base64," <> imageData) photoHtmlElement 
    return ())
  Nothing
  Runtime.get
  Nothing
  Nothing
  router

carouselWithPhotos :: [P.PhotoId] -> CrmRouter -> (DOMElement, Fay ())
carouselWithPhotos [] _ = (p "Žádné fotky", return ())
carouselWithPhotos photoIds router = let
  mkPhoto photoId = IMG.image' 
    (mkAttrs { id = Defined . (<>) "photo-" . showInt . P.getPhotoId $ photoId} ) 
    (IMG.mkImageAttrs "")
  theCarousel = carousel "my-carousel" (map mkPhoto photoIds)
  fetchPhotos' = fetchPhotos photoIds router
  in (theCarousel, fetchPhotos')
