{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepPhoto (
  addPhotoToUpkeepList ,
  upkeepPhotos ) where

import           Data.Text                        (fromString, Text, showInt)
import           Prelude                          hiding (div, span, id)
import           Data.Var                         (Var)

import           HaskellReact                     as HR
import qualified HaskellReact.Bootstrap           as B
import qualified HaskellReact.Bootstrap.Button    as BB
import qualified HaskellReact.Bootstrap.Glyphicon as G

import qualified Crm.Shared.Company               as C
import qualified Crm.Shared.Machine               as M
import qualified Crm.Shared.Upkeep                as U
import qualified Crm.Shared.Employee              as E
import qualified Crm.Shared.MachineKind           as MK

import qualified Crm.Router                       as R
import           Crm.Helpers
import           Crm.Server
import qualified Crm.Data.Data                    as D
import qualified Crm.Data.CommonData              as CD
import qualified Crm.Component.FileUpload         as FU


addPhotoToUpkeepList :: 
  R.CrmRouter -> 
  [[(U.UpkeepId, U.Upkeep, Maybe U.UpkeepId, C.CompanyId, C.Company, [(M.MachineId, Text, Text, MK.MachineKindEnum)], [E.Employee'])]] -> 
  DOMElement
addPhotoToUpkeepList router upkeeps = let
  pageInfo' = pageInfo "Aktuální servisy - přidej fotky" $ (Nothing :: Maybe DOMElement)
  table'' = B.table [head', body] where
    head' = thead $ tr [
      th "Servis č." ,
      th "Název firmy" ,
      th "Přidat fotky" ,
      th "Datum" ]
    renderUpkeepRow (upkeepId, upkeep, _, _, company, _, _) = tr [
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
  CD.ConfirmPhotoAdded ->
  DOMElement
upkeepPhotos router appVar upkeepId upkeep company confirmPhotoAdded = let
  rows =
    B.fullCol [C.companyName company, displayDate . U.upkeepDate $ upkeep] :
    photo
  photo = FU.photo (uploadUpkeepPhotoData upkeepId) router appVar confirmPhotoAdded
  in (B.grid $ B.row rows)
