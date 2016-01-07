{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepPhoto (
  addPhotoToUpkeepList ) where

import           Data.Text                        (fromString, Text, showInt, (<>), empty)
import qualified Data.Text                        as T
import           Prelude                          hiding (div, span, id)
import qualified Prelude                          as Prelude

import           HaskellReact                     as HR

import qualified Crm.Shared.Company               as C
import qualified Crm.Shared.Machine               as M
import qualified Crm.Shared.Upkeep                as U
import qualified Crm.Shared.Employee              as E

import qualified Crm.Router                       as R

addPhotoToUpkeepList :: 
  R.CrmRouter -> 
  [[(U.UpkeepId, U.Upkeep, C.CompanyId, C.Company, [(M.MachineId, Text, Text)], [E.Employee'])]] -> 
  DOMElement
addPhotoToUpkeepList router upkeeps = div "Photos"
