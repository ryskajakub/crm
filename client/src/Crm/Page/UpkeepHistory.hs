{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepHistory (
  upkeepHistory ) where

import           Data.Text                     (fromString, showInt, (<>), intercalate, pack)
import           Prelude                       hiding (div, span, id, intercalate)
import           FFI                           (Defined(Defined))
import           Data.Var                         (Var, modify)

import qualified JQuery                        as JQ

import           HaskellReact
import qualified HaskellReact.Bootstrap        as B
import qualified HaskellReact.Bootstrap.Nav    as BN
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.BackboneRouter   as BR
import qualified HaskellReact.Tag.Image        as IMG
import qualified HaskellReact.Bootstrap.Glyphicon as G

import qualified Crm.Shared.Upkeep             as U
import qualified Crm.Shared.UpkeepMachine      as UM
import qualified Crm.Shared.Machine            as M
import qualified Crm.Shared.MachineType        as MT
import qualified Crm.Shared.MachineKind        as MK
import qualified Crm.Shared.Employee           as E
import qualified Crm.Shared.Company            as C
import qualified Crm.Shared.Photo              as P
import qualified Crm.Shared.Api                as A

import qualified Crm.Data.Data                    as D
import           Crm.Helpers                   (displayDate, renderMarkup, reload, displayMachine)
import           Crm.Router
import           Crm.Server                    (deleteUpkeep, reopenUpkeep, deletePhoto)
import qualified Crm.Runtime                   as Runtime


byThrees :: [a] -> [[a]]
byThrees list = let
  go :: [a] -> [[a]] -> [[a]]
  go inputList acc = if not . null $ inputList
    then go (drop 3 inputList) (acc ++ [take 3 inputList])
    else acc
  in go list []

upkeepHistory :: 
  [(U.UpkeepId, U.Upkeep2Markup, [(UM.UpkeepMachineMarkup, M.Machine, MT.MachineType, M.MachineId)], [E.Employee'], [P.PhotoId])] -> 
  [(M.MachineId, M.Machine, MT.MachineTypeId, MT.MachineType)] ->
  C.CompanyId -> 
  Bool ->
  Var D.AppState ->
  CrmRouter -> 
  (DOMElement, Fay ())
upkeepHistory upkeepsInfo machinesInCompany companyId deletable var router = let

  basicDeleteButtonProps = BTN.buttonProps {
    BTN.disabled = Defined . not $ deletable }

  upkeepRenderHtml3 ((upkeep1:restUpkeeps)) = 
    ([header] ++ map mkMachineRow machinesInCompany) where

    mkMachineRow (_, machine, _, machineType) = B.row [
      B.colSize 3 $ MT.machineTypeName machineType ]
      

    header = B.row ([
      B.col ((B.mkColProps 3) { B.mdOffset = Defined 3 }) . displayDate . U.upkeepDate $ upkeep1 ] ++ 
      map (B.colSize 3 . displayDate . U.upkeepDate) restUpkeeps)

  upkeepsHtml = map upkeepRenderHtml3 . byThrees . map (\(_,u2,_,_,_) -> u2) $ upkeepsInfo
  flattenedUpkeepsHtml = foldl (++) [] upkeepsHtml
  
  header = B.row $ B.col (B.mkColProps 12) (h2 "Historie servisů")
  linkToCompany = B.row $ B.col (B.mkColProps 12) $
    BN.nav [ 
      link "Zpátky na firmu" (companyDetail companyId) router ,
      let 
        buttonProps = (BTN.buttonProps {
          BTN.bsStyle = Defined "danger" ,
          BTN.onClick = Defined $ const clickHandler })
        clickHandler = modify var $ \appState -> appState {
          D.navigation = case D.navigation appState of
            dh @ (D.UpkeepHistory {}) -> dh { D.deletable = not deletable }
            _ -> D.navigation appState }
        label = if deletable then "Zakázat smazávání" else "Povolit smazávání"
        in BTN.button' buttonProps label ]
  in (div $ B.grid (header : linkToCompany : flattenedUpkeepsHtml), return ())
