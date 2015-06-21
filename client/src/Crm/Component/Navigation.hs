{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Crm.Component.Navigation where

import           Prelude                          hiding (span, div, elem)
import           Data.Text                        (fromString)

import           HaskellReact
import           HaskellReact.Bootstrap           (navBar, nav)
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified Moment                           as M

import           Crm.Router 
import           Crm.Helpers

import qualified Crm.Shared.YearMonthDay          as YMD

navigation' :: CrmRouter 
            -> (DOMElement, Fay ())
            -> Fay ()
navigation' router (body, callbacks) = do
  let
    moment = M.requireMoment
    today = M.now moment
    todayFormatted = M.format today "D.M.YYYY"
  simpleReactBody' ( div [
    navBar $ nav [
      li $ link [G.home, text2DOM " Seznam firem"] defaultFrontPage router ,
      li $ link [G.tasks, text2DOM " Naplánované servisy"] plannedUpkeeps router ,
      li $ link [G.thList, text2DOM " Editace typů zařízení"] machineTypesList router ,
      li $ link [G.user, text2DOM " Servismani"] employeePage router ,
      li $ link [G.dashboard, text2DOM " Nástěnka"] dashboard router ,
      li $ link [G.asterisk, text2DOM " Speciální pole"] extraFields router ,
      li $ link [G.print, text2DOM " Tisk denních plánů"] (printDailyPlan todayFormatted Nothing) router ] ,
    div body ]) callbacks 

navigation :: CrmRouter
           -> DOMElement
           -> Fay ()
navigation router body =
  navigation' router (body, return ())
