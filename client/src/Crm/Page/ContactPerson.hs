{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.ContactPerson (
  contactPersonForm ) where

import "fay-base" Data.Text (fromString) 
import "fay-base" Prelude hiding (div)

import HaskellReact as HR

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.ContactPerson as CP

contactPersonForm :: CP.ContactPerson
                  -> C.CompanyId
                  -> DOMElement
contactPersonForm _ _ = div ""
