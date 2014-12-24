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

navigation :: CrmRouter -> DOMElement -> Fay ()
navigation router body = 
  simpleReactBody $ div [
    navBar $ nav [
      li $ link [G.list, text2DOM " Seznam firem"] frontPage router ,
      li $ link [G.list, text2DOM " Naplánované servisy"] plannedUpkeeps router ] ,
    body ]
