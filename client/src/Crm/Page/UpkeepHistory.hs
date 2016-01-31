{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepHistory (
  upkeepHistory ) where

import           Data.Text                        (fromString, showInt, (<>))
import qualified Data.Text                        as T
import           Prelude                          hiding (div, span, id, intercalate)
import           FFI                              (Defined(Defined))
import           Data.Var                            (Var, modify)

import           HaskellReact
import qualified HaskellReact.Bootstrap           as B
import qualified HaskellReact.Bootstrap.Nav       as BN
import qualified HaskellReact.Bootstrap.Button    as BTN
import qualified HaskellReact.BackboneRouter      as BR
import qualified HaskellReact.Bootstrap.Table     as BT

import qualified Crm.Shared.Upkeep                as U
import qualified Crm.Shared.UpkeepMachine         as UM
import qualified Crm.Shared.Machine               as M
import qualified Crm.Shared.MachineType           as MT
import qualified Crm.Shared.Employee              as E
import qualified Crm.Shared.Company               as C
import qualified Crm.Shared.Photo                 as P
import qualified Crm.Shared.ServerRender          as SR

import qualified Crm.Data.Data                    as D
import           Crm.Helpers                      (displayDate, renderMarkup, displayFullMachine, mkColours)
import           Crm.Router
import           Crm.Server                       (deleteUpkeep, reopenUpkeep)
import qualified Crm.Component.Navigation         as N
import qualified Crm.Component.Photos             as PH


byThrees :: [a] -> [[a]]
byThrees list = let
  go :: [a] -> [[a]] -> [[a]]
  go inputList acc = if not . null $ inputList
    then go (drop 3 inputList) (acc ++ [take 3 inputList])
    else acc
  in go list []

upkeepHistory :: 
  [(U.UpkeepId, U.Upkeep2Markup, [(UM.UpkeepMachine, M.Machine, MT.MachineType, M.MachineId)], [E.Employee'], [P.PhotoId])] -> 
  [(M.MachineId, M.Machine, MT.MachineTypeId, MT.MachineType)] ->
  C.CompanyId -> 
  Bool ->
  [P.PhotoId] ->
  Var D.AppState ->
  CrmRouter -> 
  (DOMElement, Fay ())
upkeepHistory upkeepsInfo machinesInCompany companyId deletable photosInModal var router = let

  basicDeleteButtonProps = BTN.buttonProps {
    BTN.disabled = Defined . not $ deletable }

  setPhotosInModal :: [P.PhotoId] -> Fay ()
  setPhotosInModal photoIds = modify var $ \appState -> appState {
    D.navigation = case D.navigation appState of
      uh @ (D.UpkeepHistory {}) -> uh { D.photosInModal = photoIds }
      _ -> D.navigation appState }

  PH.PhotoModal photoModalElement mkPhotoModalButton photoFetch = PH.mkPhotoModal photosInModal

  upkeepRenderHtml3 (oneToThreeUpkeeps @ (upkeep1:restUpkeeps)) = 
    (BT.table' (class' "break-words") Nothing [cols, thead header', tbody bodyCells]) where
    
    cols = colgroup $ map (const $ col' (class' "col-md-3") "") [(1::Int)..4]

    cellsToPad = [0..(2 - length oneToThreeUpkeeps)]
    emptyCell = td ""
    paddingCells = map (const emptyCell) cellsToPad

    isEmptyMarkup (SR.PlainText pt:_) = T.null pt
    isEmptyMarkup _ = True

    getUpkeepId (u1,_,_,_,_) = u1
    getUpkeep (_,u2,_,_,_) = u2
    getUpkeepMachines (_,_,u3,_,_) = u3
    getEmployees (_,_,_,u4,_) = u4
    getPhotos (_,_,_,_,u5) = u5

    mkTextualCell pick upkeep = td (renderMarkup . pick . getUpkeep $ upkeep)

    isRowEmpty isUpkeepAspectEmpty = length (takeWhile isUpkeepAspectEmpty oneToThreeUpkeeps) == length oneToThreeUpkeeps

    renderTextualRow pick header'' = if isRowEmpty (isEmptyMarkup . pick . getUpkeep)
      then []
      else [tr $ (th header'') : map (mkTextualCell pick) oneToThreeUpkeeps ++ paddingCells]
    mkUpkeepLink (upkeepId, upkeepData, _, _, _) = if U.upkeepClosed upkeepData
      then let
        buttonProps = (BTN.buttonProps {
          BTN.bsStyle = Defined "primary" ,
          BTN.onClick = Defined $ const clickHandler })
        clickHandler = reopenUpkeep upkeepId goEditUpkeep router
        goEditUpkeep = navigate (upkeepDetail upkeepId) router
        in BTN.button' buttonProps "Otevřít"
      else let
        buttonProps = (BTN.buttonProps {
          BTN.onClick = Defined $ const clickHandler })
        clickHandler = navigate (upkeepDetail upkeepId) router
        in BTN.button' buttonProps "Uzavření"

    mkDeleteButton upkeepId = let
      clickHandler = deleteUpkeep upkeepId BR.refresh router
      buttonProps = basicDeleteButtonProps {
        BTN.bsStyle = Defined "danger" ,
        BTN.onClick = Defined $ const clickHandler }
      in BTN.button' buttonProps "Smazat"

    ifNonEmptyEmployees code = if isRowEmpty (null . getEmployees)
      then []
      else code
    ifNonEmptyPhotos code = if isRowEmpty (null . getPhotos)
      then []
      else code

    bodyCells = map mkMachineRow machinesInCompany ++
      renderTextualRow U.workDescription "Popis práce" ++
      renderTextualRow U.recommendation "Doporučení" ++
      ifNonEmptyEmployees 
        [tr $ th "Servisáci" : map (td . mkColours . map snd . getEmployees) oneToThreeUpkeeps ++ paddingCells] ++
      ifNonEmptyPhotos
        [tr $ th "Fotky" : map (td . ((flip mkPhotoModalButton) setPhotosInModal) . getPhotos) oneToThreeUpkeeps ++ paddingCells] ++
      [tr $ th "Změna stavu" : map (td . mkUpkeepLink) oneToThreeUpkeeps ++ paddingCells] ++
      [tr $ th "Smazat" : map (td . mkDeleteButton . getUpkeepId) oneToThreeUpkeeps ++ paddingCells]
      
    mkMachineRow (machineId, machine, _, machineType) = let 
      mkUpkeepMachineInfo upkeep = let
        um = find (\(_,_,_,machineId') -> machineId' == machineId) (getUpkeepMachines upkeep)
        result = maybe ([text2DOM ""]) (\(upkeepMachine,_,_,_) -> displayUM upkeepMachine) um
        displayUM upkeepMachine = let
          mkCellContent labelType upkeepT content' =
            [span' (class'' ["label", "label-" <> labelType]) upkeepT, text2DOM $ " " <> content']
          panel = case UM.upkeepType upkeepMachine of

            UM.Repair -> mkCellContent "danger" "O" -- (\x -> [span' (class'' ["label", "label-danger"]) "O", text2DOM $ " " <> x])
            UM.Regular -> mkCellContent "info" "S" -- (\x -> [span' (class'' ["label", "label-info"]) "S", text2DOM $ " " <> x])
            UM.Check -> mkCellContent "default" "K" -- (\x -> [span' (class'' ["label", "label-default"]) "K", text2DOM $ " " <> x])
          content = if UM.recordedMileage upkeepMachine == 0
            then ""
            else showInt . UM.recordedMileage $ upkeepMachine
          in panel content
        in td result
      in tr (
        (th (link 
            (displayFullMachine machine machineType)
            (machineDetail machineId)
            router)) :
          map mkUpkeepMachineInfo oneToThreeUpkeeps ++ 
          paddingCells)

    header' = tr ([
      th "" ,
      th (displayDate . U.upkeepDate . getUpkeep $ upkeep1) ] ++
      map (th . displayDate . U.upkeepDate . getUpkeep) restUpkeeps ++ 
      map (const $ th "") cellsToPad)

  upkeepsHtml = map upkeepRenderHtml3 . byThrees $ upkeepsInfo
  flattenedUpkeepsHtml = upkeepsHtml
  
  header = B.row $ B.col (B.mkColProps 12) (h2 "Historie servisů")
  navigation = B.row $ B.col (B.mkColProps 12) $
    BN.nav [ 
      N.backToCompany companyId router ,
      let 
        buttonProps = (BTN.buttonProps {
          BTN.bsStyle = Defined "danger" ,
          BTN.onClick = Defined $ const clickHandler })
        clickHandler = modify var $ \appState -> appState {
          D.navigation = case D.navigation appState of
            dh @ (D.UpkeepHistory {}) -> dh { D.deletable = not deletable }
            _ -> D.navigation appState }
        label'' = if deletable then "Zakázat smazávání" else "Povolit smazávání"
        button = BTN.button' buttonProps label''
        in form' (class' "navbar-form") button ]
  in (div $ B.grid (header : navigation : photoModalElement : flattenedUpkeepsHtml), photoFetch router)
