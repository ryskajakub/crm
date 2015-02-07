{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Jasny where

import "fay-base" FFI (Defined(Defined))
import "fay-base" Data.Text (fromString, Text)
import "fay-base" Prelude hiding (div, id)

import HaskellReact
import HaskellReact.Tag.Input (file, input, mkInputAttrs, name, type_)

data FileUploadTechnical1 = FileUploadTechnical1 {
  data_provides :: Text }

fut1 :: FileUploadTechnical1
fut1 = FileUploadTechnical1 "fileinput"

data FileUploadTechnical2 = FileUploadTechnical2 {
  data_trigger :: Text }

fut2 :: FileUploadTechnical2
fut2 = FileUploadTechnical2 "fileinput"

fileUpload :: DOMElement
fileUpload = constructDOMElement "div" (class'' ["fileinput", "fileinput-new"]) fut1 [
  constructDOMElement "div" (class'' ["fileinput-preview", "thumbnail"]) fut2 ([] :: [DOMElement]) ,
  div [
    span' (class'' ["btn", "btn-default", "btn-file"]) [
      span' (class' "fileinput-new") "Select image" ,
      span' (class' "fileinput-exists") "Change" ,
      input (mkAttrs {
          id = Defined "file-upload"
        }) (mkInputAttrs { 
        type_ = file ,
        name = Defined $ "file-upload" })]]]
