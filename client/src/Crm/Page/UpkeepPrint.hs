{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.UpkeepPrint (
  upkeepPrint ) where

import           Data.Text    (fromString)
import           Prelude      hiding (div)

import           HaskellReact

upkeepPrint :: DOMElement
upkeepPrint = div ""
