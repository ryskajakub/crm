{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Modal where

import "fay-base" Prelude hiding (id)
import "fay-base" Data.Text (Text, fromString)
import "fay-base" FFI (Defined(Undefined, Defined))

import HaskellReact
import HaskellReact.Bootstrap hiding (data_toggle)

import HaskellReact.Bootstrap.Button

data ModalPair = ModalPair {
  baseButtonProps :: ExtraButtonProps ,
  modal :: DOMElement }

mkModalPair :: ModalPair
mkModalPair = let
  baseButtonProps' = extraButtonProps {
    data_toggle = Defined "modal" ,
    data_target = Defined "#photosModal" }
  modalElement = div' ((class'' ["modal", "fade"]) { id = Defined "photosModal" }) $
    div' (class' "modal-dialog") $
      div' (class' "modal-content") $
        div' (class' "modal-body") "AHOJ"
  in ModalPair {
    baseButtonProps = baseButtonProps' ,
    modal = modalElement }
    
