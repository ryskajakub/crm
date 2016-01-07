{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepPhoto (
  addPhotoToUpkeepList ) where

import           Data.Text                        (fromString, Text, showInt, (<>), empty)
import qualified Data.Text                        as T
import           Prelude                          hiding (div, span, id)
import qualified Prelude                          as Prelude

import           HaskellReact                     as HR
import qualified HaskellReact.Bootstrap           as B

import qualified Crm.Shared.Company               as C
import qualified Crm.Shared.Machine               as M
import qualified Crm.Shared.Upkeep                as U
import qualified Crm.Shared.Employee              as E

import qualified Crm.Router                       as R
import           Crm.Helpers

addPhotoToUpkeepList :: 
  R.CrmRouter -> 
  [[(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company, [(M.MachineId, Text, Text)], [E.Employee'])]] -> 
  DOMElement
addPhotoToUpkeepList router upkeeps = let
  pageInfo' = pageInfo "Aktuální servisy - přidej fotky" $ (Nothing :: Maybe DOMElement)
  table = B.table [head', body] where
    head' = thead $ tr [
      th "Název firmy" ,
      th "Datum" ,
      th "Přidat fotky" ]
    renderUpkeepRow (upkeepId, upkeep, _, company, _, _) = tr [
      td . C.companyName $ company ,
      td . displayDate . U.upkeepDate $ upkeep ,
      td "link to photo" ]
    body = tbody $ map renderUpkeepRow (concat upkeeps)
  in (B.grid $ B.row $
    pageInfo' ++
    [B.col (B.mkColProps 12) $ main table])
