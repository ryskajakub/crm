{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepPhoto (
  addPhotoToUpkeepList ,
  upkeepPhotos ) where

import           Data.Text                        (fromString, Text, showInt)
import qualified Data.Text                        as T
import           Prelude                          hiding (div, span, id)
import           FFI                              (Defined(..), ffi)
import           Data.Var                         (Var, modify)
import qualified DOM

import           HaskellReact                     as HR
import qualified HaskellReact.Bootstrap           as B
import qualified HaskellReact.Bootstrap.Button    as BB
import qualified HaskellReact.Jasny               as J
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Bootstrap.Alert     as A
import qualified JQuery                           as JQ

import qualified Crm.Shared.Company               as C
import qualified Crm.Shared.Machine               as M
import qualified Crm.Shared.Upkeep                as U
import qualified Crm.Shared.Employee              as E
import qualified Crm.Shared.PhotoMeta             as PM
import qualified Crm.Shared.MachineKind           as MK

import qualified Crm.Router                       as R
import           Crm.Helpers
import           Crm.Server
import qualified Crm.Data.Data                    as D

addPhotoToUpkeepList :: 
  R.CrmRouter -> 
  [[(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company, [(M.MachineId, Text, Text, MK.MachineKindEnum)], [E.Employee'])]] -> 
  DOMElement
addPhotoToUpkeepList router upkeeps = let
  pageInfo' = pageInfo "Aktuální servisy - přidej fotky" $ (Nothing :: Maybe DOMElement)
  table'' = B.table [head', body] where
    head' = thead $ tr [
      th "Servis č." ,
      th "Název firmy" ,
      th "Přidat fotky" ,
      th "Datum" ]
    renderUpkeepRow (upkeepId, upkeep, _, company, _, _) = tr [
      td $ R.link (showInt . U.getUpkeepId $ upkeepId) (R.replanUpkeep upkeepId) router ,
      td . C.companyName $ company ,
      td $ BB.buttonP 
        BB.LargeButton
        BB.PrimaryButton
        (const $ R.navigate (R.upkeepPhotoAdd upkeepId) router)
        [G.camera, text2DOM " Přidat fotky"] ,
      td . displayDate . U.upkeepDate $ upkeep ]
    body = tbody $ map renderUpkeepRow (concat upkeeps)
  in (B.grid $ B.row $
    pageInfo' ++
    [B.col (B.mkColProps 12) $ main table''])


upkeepPhotos ::
  R.CrmRouter ->
  Var D.AppState ->  
  U.UpkeepId ->
  U.Upkeep ->
  C.Company ->
  D.ConfirmPhotoAdded ->
  DOMElement
upkeepPhotos router appVar upkeepId upkeep company confirmPhotoAdded = let

  alertDisplayed = case confirmPhotoAdded of
    D.ConfirmPhotoAddedOK -> [B.fullCol [A.alert A.Info $ p "Obrázek i metadata byly nahrány na server."]]
    D.NoPhotoAdded -> []

  rows =
    alertDisplayed ++
    [B.fullCol [C.companyName company, displayDate . U.upkeepDate $ upkeep]] ++
    [photo]
  photo = let
    imageUploadHandler = const $ do
      fileUpload <- JQ.select "#file-upload"
      files <- getFileList fileUpload
      file <- fileListElem 0 files
      type' <- fileType file
      name <- fileName file
      let
        photoSource = if isIPhone
          then PM.IPhone
          else PM.Other
      uploadUpkeepPhotoData upkeepId file $ \photoId -> let
        callback = do
          T.putStrLn "ahoj"
          modify appVar $ \appState -> appState {
            D.navigation = case D.navigation appState of
              (D.AddPhotoToUpkeep a b c _) -> D.AddPhotoToUpkeep a b c D.ConfirmPhotoAddedOK }
          _ <- DOM.setTimeout 3000 $ const reload
          return ()
        in uploadPhotoMeta (PM.PhotoMeta type' name photoSource) photoId callback router
    imageUploadLabel = "Nahraj fotku"
    in B.fullCol [
      J.fileUploadI18n "Vyber obrázek" "Dej jiný obrázek" ,
      BB.button'
        (BB.buttonProps {
          BB.bsStyle = Defined "primary" ,
          BB.onClick = Defined imageUploadHandler })
        imageUploadLabel ]
  in (B.grid $ B.row rows)

isIPhone :: Bool
isIPhone = ffi "/iPad|iPhone|iPod/.test(navigator.userAgent) && !window.MSStream"
