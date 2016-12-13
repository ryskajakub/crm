{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Component.FileUpload where

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
import qualified Crm.Shared.Photo                 as P

import qualified Crm.Router                       as R
import           Crm.Helpers
import           Crm.Server
import qualified Crm.Data.Data                    as D
import           Crm.Data.CommonData              as CD
import           Crm.Data.MachineData             as MD

import           Debug.Trace (traceShow)

photo :: 
  (File -> (P.PhotoId -> Fay ()) -> Fay ()) -> 
  R.CrmRouter -> 
  Var D.AppState -> 
  CD.ConfirmPhotoAdded ->
  [DOMElement]
photo uupd router appVar cpa = let
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
    uupd file $ \photoId -> let
      callback = do
        modify appVar $ \appState -> appState {
          D.navigation = case D.navigation appState of
            (D.MachineScreen (md @ MD.MachineData {})) -> 
              D.MachineScreen $ md { MD.photoAdded = CD.ConfirmPhotoAddedOK }
            (mte @ D.MachineTypeEdit {}) -> mte { D.machineTypeImageAdded = CD.ConfirmPhotoAddedOK }
            (D.AddPhotoToUpkeep a b c _) -> D.AddPhotoToUpkeep a b c CD.ConfirmPhotoAddedOK
            x -> traceShow x x }
        _ <- DOM.setTimeout 3000 $ const reload
        return ()
      in uploadPhotoMeta (PM.PhotoMeta type' name photoSource) photoId callback router
  imageUploadLabel = "Nahraj fotku"
  alertDisplayed = case cpa of
    CD.ConfirmPhotoAddedOK -> [B.fullCol [A.alert A.Info $ p "Obrázek i metadata byly nahrány na server."]]
    CD.NoPhotoAdded -> []
  fileUploadForm = B.fullCol [
    J.fileUploadI18n "Vyber obrázek" "Dej jiný obrázek" ,
    BB.button'
      (BB.buttonProps {
        BB.bsStyle = Defined "primary" ,
        BB.onClick = Defined imageUploadHandler })
      imageUploadLabel ]
  in fileUploadForm : alertDisplayed

isIPhone :: Bool
isIPhone = ffi "/iPad|iPhone|iPod/.test(navigator.userAgent) && !window.MSStream"
