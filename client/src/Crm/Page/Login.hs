{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.Login (
  login ) where

import           Data.Text                        (fromString, Text, length)
import           Prelude                          hiding (div, span, id, length)
import           FFI                              (Defined (Defined))
import           Data.Var                         (Var, modify)

import           HaskellReact
import qualified HaskellReact.Bootstrap           as B
import qualified HaskellReact.Bootstrap.Button    as BTN
import qualified HaskellReact.Bootstrap.Glyphicon as G

import           Crm.Server                       (createEmployee, updateEmployee)
import           Crm.Component.Form
import qualified Crm.Data.Data                    as D
import qualified Crm.Data.EmployeeData            as ED
import qualified Crm.Shared.Employee              as E
import           Crm.Router                       (CrmRouter, navigate, newEmployee)
import qualified Crm.Router                       as R
import           Crm.Helpers                      (pageInfo, validationHtml)

login :: DOMElement
login = div ""
