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

import Crm.Router (link, frontPage, CrmRouter, plannedUpkeeps)

navigation' :: CrmRouter 
            -> (DOMElement, Fay ())
            -> Fay ()
navigation' router (body, callbacks) = 
  simpleReactBody' ( div [
    navBar $ nav [
      li $ link [G.list, text2DOM " Seznam firem"] frontPage router ,
      li $ link [G.list, text2DOM " Naplánované servisy"] plannedUpkeeps router ,
      li $ link [G.list, text2DOM " Editace typů zařízení"] plannedUpkeeps router ] ,
    body ] ) callbacks 

navigation :: CrmRouter
           -> DOMElement
           -> Fay ()
navigation router body =
  navigation' router (body, return ())
