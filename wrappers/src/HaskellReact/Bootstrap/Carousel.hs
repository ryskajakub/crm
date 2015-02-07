{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module HaskellReact.Bootstrap.Carousel (
  carousel ) where

import "fay-base" FFI (Defined(Defined))
import "fay-base" Data.Text (fromString, Text, showInt, (<>))
import "fay-base" Prelude hiding (span, id)

import HaskellReact as HR
import HaskellReact.Bootstrap.Popover as P
import HaskellReact.ReactCalendar as RC
import qualified HaskellReact.Tag.Input as I
import qualified HaskellReact.Tag.Hyperlink as A
import qualified HaskellReact.Bootstrap.Glyphicon as G

data Technical1 = Technical1 {
  data_ride :: Text }

data Technical2 = Technical2 {
  data_target :: Text ,
  data_slide_to :: Text }

data Technical3 = 
  Technical3 {
    role :: Text } |
  Technical4 {
    href :: Text ,
    role :: Text ,
    data_slide :: Text }

technical1 :: Technical1
technical1 = Technical1 {
  data_ride = "carousel" }

technical2 :: Int -> Text -> Technical2 
technical2 slideNumber id' = Technical2 {
  data_target = "#" <> id' ,
  data_slide_to = showInt slideNumber }

technical3 :: Technical3
technical3 = Technical3 { 
  role = "listbox" }

technical4 :: Text -> Text -> Technical3
technical4 id' dataSlide = Technical4 {
  href = "#" <> id' ,
  role = "button" ,
  data_slide = dataSlide }

carousel :: Text -> [DOMElement] -> DOMElement
carousel id' slides =
  constructDOMElement "div" ((class'' ["carousel", "slide"]) { id = Defined id' } ) technical1 [
    ol' (class' "carousel-indicators") [
      constructDOMElement "li" mkAttrs (technical2 0 id') ([]::[DOMElement]) , 
      constructDOMElement "li" mkAttrs (technical2 1 id') ([]::[DOMElement]) ] ,
    constructDOMElement "div" (class' "carousel-inner") technical3 [
      div' (class'' ["item", "active"]) $ h1 "Text big" ,
      div' (class' "item") $ h1 "Text huge" ] ,
    constructDOMElement "a" (class'' ["left", "carousel-control"]) (technical4 id' "prev") [
      G.chevronLeft ] ,
    constructDOMElement "a" (class'' ["right", "carousel-control"]) (technical4 id' "next") [
      G.chevronRight ] ]
