{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Modal where

import "fay-base" Prelude hiding (id)
import "fay-base" Data.Text (fromString)
import "fay-base" FFI (Defined(Defined))

import HaskellReact

import HaskellReact.Bootstrap.Button

data ModalPair = ModalPair {
  baseButtonProps :: ExtraButtonProps ,
  modal :: DOMElement -> DOMElement }

mkModalPair :: ModalPair
mkModalPair = let
  baseButtonProps' = extraButtonProps {
    data_toggle = Defined "modal" ,
    data_target = Defined "#photosModal" }
  modalElement modalBody = div' ((class'' ["modal", "fade"]) { id = Defined "photosModal" }) $
    div' (class'' ["modal-dialog", "modal-extra-lg"]) $
      div' (class' "modal-content") $
        div' (class' "modal-body") modalBody
  in ModalPair {
    baseButtonProps = baseButtonProps' ,
    modal = modalElement }
    
