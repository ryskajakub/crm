{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Crm.Component.Photos where

import           Prelude                          hiding (span, div, elem, id)
import           Data.Text                        (fromString, showInt, (<>), pack)
import           Data.LocalStorage                (removeLocalStorage)
import           FFI                                   (Defined(..))

import           HaskellReact
import           HaskellReact.Bootstrap           (navBar, nav)
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Tag.Hyperlink       as A
import qualified HaskellReact.Bootstrap.Button         as BTN
import qualified HaskellReact.Tag.Image                as IMG
import qualified HaskellReact.Bootstrap.Modal          as BM
import qualified Moment                           as M
import qualified JQuery                                as JQ
import qualified Crm.Runtime                           as Runtime
import qualified Crm.Shared.Api                        as A

import qualified Crm.Shared.YearMonthDay          as YMD
import qualified Crm.Shared.Photo                 as P

import           Crm.Router 

data PhotoModal = PhotoModal {
  photoModalElement :: DOMElement ,
  mkPhotoModalDisplayButton :: [P.PhotoId] -> ([P.PhotoId] -> Fay ()) -> [DOMElement] ,
  fetchPhotosToModal :: CrmRouter -> Fay () }

mkPhotoModal :: [P.PhotoId] -> PhotoModal
mkPhotoModal photoIds = let
  
  BM.ModalPair modalButtonProps modalElementBase = BM.mkModalPair 

  photoModalElement = modalElementBase . div' (class' "upkeep-photos") . map mkPhotoRegion $ photoIds
    where
    mkPhotoRegion photoId = IMG.image' 
      (mkAttrs { id = Defined . (<>) "photo-" . showInt . P.getPhotoId $ photoId})
      (IMG.mkImageAttrs "")
  
  displayPhotos differentPhotoIds setPhotosInModal = let
    clickHandler = setPhotosInModal differentPhotoIds
    in case differentPhotoIds of
      [] -> []
      _ -> [BTN.buttonP' modalButtonProps BTN.NormalButton BTN.DefaultButton (const clickHandler)
        [G.picture, span $ " (" <> (showInt . length $ differentPhotoIds) <> ")" ]]

  in PhotoModal photoModalElement displayPhotos (fetchPhotos photoIds)

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
