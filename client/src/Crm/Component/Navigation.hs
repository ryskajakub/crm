{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Component.Navigation where

import "fay-base" Data.Text (fromString)
import "fay-base" Prelude hiding (span, div, elem)

import HaskellReact
import HaskellReact.Bootstrap (navBar, nav)
import qualified HaskellReact.Bootstrap.Glyphicon as G

import Crm.Router (link, defaultFrontPage, CrmRouter, plannedUpkeeps, machineTypesList)

navigation' :: CrmRouter 
            -> (DOMElement, Fay ())
            -> Fay ()
navigation' router (body, callbacks) = 
  simpleReactBody' ( div [
    navBar $ nav [
      li $ link [G.home, text2DOM " Seznam firem"] defaultFrontPage router ,
      li $ link [G.tasks, text2DOM " Naplánované servisy"] plannedUpkeeps router ,
      li $ link [G.thList, text2DOM " Editace typů zařízení"] machineTypesList router ] ,
    body ] ) callbacks 

navigation :: CrmRouter
           -> DOMElement
           -> Fay ()
navigation router body =
  navigation' router (body, return ())
