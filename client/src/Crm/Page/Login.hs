{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.Login (
  login,
  restart ) where

import           Data.Text              (fromString, Text)
import           Prelude                hiding (div, span, id, length)
import           FFI                    (Defined(Defined))
import           Data.Var               (Var, modify)
import           Data.LocalStorage

import           HaskellReact
import qualified HaskellReact.Tag.Input as I
import qualified HaskellReact.Bootstrap as B
import qualified JQuery                 as JQ

import qualified Crm.Data.Data          as D
import           Crm.Component.Form
import           Crm.Router             (CrmRouter, navigate, defaultFrontPage)
import           Crm.Server             (testEmployeesPage, status)


enter :: Int
enter = 13

login :: 
  Var D.AppState -> 
  CrmRouter -> 
  Text -> 
  Bool -> 
  DOMElement
login appVar router password wrongPassword = formWrapper $ B.grid $ [headerRow, passwordRow, submitRow] ++ errorRow where
  modify' f = modify appVar $ \appState -> appState {
    D.navigation = case D.navigation appState of 
      l @ (D.Login _ _) -> f l
      _ -> D.navigation appState }
  pageHeader = "Přihlášení"
  headerRow = B.row $ B.col (B.mkColProps 12) $ h2 pageHeader
  formWrapper element = div' (mkAttrs { className = Defined "form-horizontal" }) element
  passwordRow = oneElementRow "Heslo" passwordInput where
    inputAttrs = I.mkInputAttrs {
      I.value_ = Defined password ,
      I.onChange = Defined $ eventValue >=> \password' -> modify' . const $ D.Login password' False ,
      I.onKeyUp = Defined $ keyCode >=> \code -> if code == enter then submitButtonHandler else return () }
    passwordInput = I.password inputNormalAttrs inputAttrs
  submitButtonHandler = testEmployeesPage
    password
    (storePassword >> navigate defaultFrontPage router)
    (\e _ _ -> if status e == 401
      then modify' $ \l -> l { D.wrongPassword = True }
      else return ())
    router
  submitRow = B.row $ buttonRow "Přihlásit se" submitButtonHandler
  storePassword = setLocalStorage "password" password
  errorRow = if wrongPassword
    then (B.row $ B.col (B.mkColProps 12) $ p "Špatné heslo") : []
    else []

restart :: 
  Var D.AppState -> 
  CrmRouter -> 
  Text -> 
  DOMElement
restart appVar router password = formWrapper ( B.grid [headerRow, passwordRow, submitRow] ) where
  modify' f = modify appVar $ \appState -> appState {
    D.navigation = case D.navigation appState of 
      l @ (D.Restart _) -> f l
      _ -> D.navigation appState }
  pageHeader = "Restartovat"
  headerRow = B.row $ B.col (B.mkColProps 12) $ h2 pageHeader
  formWrapper element = div' (mkAttrs { className = Defined "form-horizontal" }) element
  passwordRow = oneElementRow "Heslo" passwordInput where
    inputAttrs = I.mkInputAttrs {
      I.value_ = Defined password ,
      I.onChange = Defined $ eventValue >=> \password' -> modify' . const $ D.Restart password' ,
      I.onKeyUp = Defined $ keyCode >=> \code -> if code == enter then submitButtonHandler else return () }
    passwordInput = I.password inputNormalAttrs inputAttrs
  submitButtonHandler = JQ.ajax' (JQ.defaultAjaxSettings {
      JQ.type' = Defined "POST" ,
      JQ.data' = Defined password ,
      JQ.url = Defined "/restart/" ,
      JQ.contentType = Defined "text/plain" })
  submitRow = B.row $ buttonRow "Restartovat" submitButtonHandler
