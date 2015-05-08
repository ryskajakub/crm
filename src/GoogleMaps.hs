{-# LANGUAGE PackageImports #-}

module GoogleMaps (
  Map ,
  MapOptions (..) ,
  LatLng ,
  mkMap , 
  mkLatLng ,
  startMapOnLoad ) where

import FFI
import "fay-base" Prelude
import DOM (Element)

data Map

data LatLng

data MapOptions = MapOptions {
  zoom :: Defined Int ,
  center :: Defined LatLng }

mkMap :: Element -> MapOptions -> Fay Map
mkMap = ffi " new google.maps.Map(%1,%2) "

mkLatLng :: Double -> Double -> LatLng
mkLatLng = ffi " new google.maps.LatLng(%1,%2) "

startMapOnLoad :: Fay () -> Fay ()
startMapOnLoad = ffi " google.maps.event.addDomListener(window, 'load', %1) "
