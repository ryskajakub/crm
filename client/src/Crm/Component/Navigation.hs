{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Crm.Component.Navigation where

import Data.Text (fromString)
import Prelude hiding (span, div, elem)
import FFI (Defined(Defined))

import HaskellReact
import HaskellReact.Bootstrap (navBar' , nav)
import qualified HaskellReact.Bootstrap.Glyphicon as G

import Crm.Router (link, defaultFrontPage, CrmRouter, plannedUpkeeps, machineTypesList, 
  employeePage, dashboard, extraFields)

navigation' :: CrmRouter 
            -> (DOMElement, Fay ())
            -> Fay ()
navigation' router (body, callbacks) = 
  simpleReactBody' ( div [
    navBar' (\attrs -> attrs { key = Defined "1" } ) $ nav [
      li' (row "0") $ link [G.home, text2DOM " Seznam firem"] defaultFrontPage router ,
      li' (row "2") $ link [G.tasks, text2DOM " Naplánované servisy"] plannedUpkeeps router ,
      li' (row "3") $ link [G.thList, text2DOM " Editace typů zařízení"] machineTypesList router ,
      li' (row "4") $ link [G.user, text2DOM " Servismani"] employeePage router ,
      li' (row "5") $ link [G.dashboard, text2DOM " Nástěnka"] dashboard router ,
      li' (row "6") $ link [G.asterisk, text2DOM " Speciální pole"] extraFields router ] ,
    div' (row "2") body ] ) callbacks 

navigation :: CrmRouter
           -> DOMElement
           -> Fay ()
navigation router body =
  navigation' router (body, return ())
