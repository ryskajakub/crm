{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Crm.Component.Navigation where

import           Prelude                          hiding (span, div, elem)
import           Data.Text                        (fromString)
import           Data.LocalStorage                (removeLocalStorage)

import           HaskellReact
import           HaskellReact.Bootstrap           (navBar, nav)
import qualified HaskellReact.Bootstrap.Glyphicon as G
import qualified HaskellReact.Tag.Hyperlink       as A
import qualified Moment                           as M

import qualified Crm.Shared.YearMonthDay          as YMD

import           Crm.Router 

navigation' :: CrmRouter 
            -> (DOMElement, Fay ())
            -> Fay ()
navigation' router (body, callbacks) = do
  let
    moment = M.requireMoment
    (y, m, d) = M.day . M.now $ moment
    todayYMD = YMD.YearMonthDay y m d YMD.DayPrecision
    logout = do
      removeLocalStorage "password"
      navigate login router

  simpleReactBody' ( div [
    navBar $ nav [
      li $ link [G.home, text2DOM " Seznam firem"] defaultFrontPage router ,
      li $ link [G.tasks, text2DOM " Naplánované servisy"] plannedUpkeeps router ,
      li $ link [G.thList, text2DOM " Editace typů zařízení"] machineTypesList router ,
      li $ link [G.user, text2DOM " Servismani"] employeePage router ,
      li $ link [G.dashboard, text2DOM " Nástěnka"] dashboard router ,
      li $ link [G.asterisk, text2DOM " Speciální pole"] extraFields router ,
      li $ link [G.print, text2DOM " Tisk denních plánů"] (dailyPlan todayYMD Nothing) router ,
      li $ link [G.camera, text2DOM " Fotky servisů"] upkeepPhotos router ,
      li $ A.a''' (click logout) [G.logOut, text2DOM " Odhlášení"]] ,
    div body ]) callbacks 

navigation :: CrmRouter
           -> DOMElement
           -> Fay ()
navigation router body =
  navigation' router (body, return ())
