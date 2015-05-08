{-# LANGUAGE PackageImports #-}

module GoogleMaps (
  Map ,
  MapOptions (..) ,
  LatLng ,
  mkMap , 
  mkLatLng ,
  startMapOnLoad ,
  computeCoordinates ) where

import FFI
import "fay-base" Data.Text (Text)
import "fay-base" Prelude
import DOM (Element)
import Data.Nullable
import Data.Maybe

data Map

data LatLng

data Geocoder
data GeocoderResults
data GeocoderResult
data GeocoderRequest 

data MapOptions = MapOptions {
  zoom :: Defined Int ,
  center :: Defined LatLng }

mkMap :: Element -> MapOptions -> Fay Map
mkMap = ffi " new google.maps.Map(%1,%2) "

mkLatLng :: Double -> Double -> LatLng
mkLatLng = ffi " new google.maps.LatLng(%1,%2) "

startMapOnLoad :: Fay () -> Fay ()
startMapOnLoad = ffi " google.maps.event.addDomListener(window, 'load', %1) "


addMarker :: Double -> Double -> Map -> Fay ()
addMarker lat lng map' = let
  position = mkLatLng lat lng
  in mkMarker' position map'

mkMarker' :: LatLng -> Map -> Fay ()
mkMarker' = ffi " new google.maps.Marker({position: %1, map: %2})  "


mkGeocoderRequest :: Text -> GeocoderRequest
mkGeocoderRequest = ffi " { \"address\":%1 } "

computeCoordinates :: Text -> (Maybe (Double, Double) -> Fay ()) -> Fay ()
computeCoordinates text callback = do
  g <- mkGeocoder
  let 
    gr = mkGeocoderRequest text
    callback' results = let
      headResult = fromNullable $ getResultsHead results
      coordinates = getCoordinates `onJust` headResult
      in callback coordinates
  geocode g gr callback'

mkGeocoder :: Fay Geocoder
mkGeocoder = ffi " new google.maps.Geocoder() "

geocode :: Geocoder -> GeocoderRequest -> (GeocoderResults -> Fay ()) -> Fay ()
geocode = ffi " %1['geocode'](%2, %3) "

getResultsHead :: GeocoderResults -> Nullable GeocoderResult
getResultsHead = ffi " (function () { var results = %1; return (results.length > 0 ? results[0] : null ); })() "

getCoordinates :: GeocoderResult -> (Double, Double)
getCoordinates = ffi " (function () { var location = %1['geometry']['location']; return [location.A, location.F] })() "
