{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.Login (
  login ) where

import Data.Text    (fromString, Text, length)
import Prelude      hiding (div, span, id, length)

import HaskellReact

login :: DOMElement
login = div "login"
