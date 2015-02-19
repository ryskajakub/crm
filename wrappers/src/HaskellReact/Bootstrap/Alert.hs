{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Alert (
  alert , 
  AlertType(..) ) where

import "fay-base" Prelude
import "fay-base" Data.Text (fromString)

import HaskellReact 
import HaskellReact.Bootstrap

data AlertType = Success | Info | Warning | Danger

technicalAttr :: Technical1
technicalAttr = Technical1 "alert"

alert :: Renderable a
      => AlertType
      -> a
      -> DOMElement
alert alertType element =
  constructDOMElement "div" (class'' ["alert", class2]) technicalAttr element
    where 
      class2 = case alertType of
        Success -> "alert-success"
        Info -> "alert-info"
        Warning -> "alert-warning"
        Danger -> "alert-danger"
