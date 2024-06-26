{-# LANGUAGE PackageImports #-}

module GoogleMaps (
  Map ,
  MapOptions ,
  InfoWindow ,
  mkMapOptions ,
  LatLng ,
  mkInfoWindow ,
  openInfoWindow ,
  mkMap , 
  addMarker ,
  mkLatLng ,
  startMapOnLoad ,
  addClickListener ,
  computeCoordinates ) where


import FFI
import Data.Text     (Text, pack, (<>))
import               DOM (Element)
import Data.Nullable
import Data.Maybe


data MapOptions
data Map
data MapMarker
data PinElement

data InfoWindow
  
data LatLng

data Geocoder
data GeocoderResults
data GeocoderResult
data GeocoderRequest 

mkMapOptions :: Int -> LatLng -> Text -> MapOptions
mkMapOptions = ffi " {zoom: %1, center: %2, mapId: %3} "

mkMap :: Element -> MapOptions -> Fay Map
mkMap = ffi " new google.maps.Map(%1,%2) "

mkLatLng :: Double -> Double -> LatLng
mkLatLng = ffi " new google.maps.LatLng(%1,%2) "

startMapOnLoad :: Fay () -> Fay ()
startMapOnLoad = ffi " google.maps.event.addDomListener(window, 'load', %1) "

mkPinElement :: Text -> Fay PinElement
mkPinElement = ffi " new google.maps.marker.PinElement({ background: %1, borderColor: 'black', glyphColor: 'black' }).element "

addMarker :: Double -> Double -> Text -> Map -> Fay MapMarker
addMarker lat lng color map' = let
  position = mkLatLng lat lng
  in do 
    pinElement <- mkPinElement (pack "#" <> color)
    mkMarker' position map' pinElement

mkMarker' :: LatLng -> Map -> PinElement -> Fay MapMarker
mkMarker' = ffi " new google.maps.marker.AdvancedMarkerElement({position: %1, map: %2, content: %3}) "
-- mkMarker' = ffi " new google.maps.Marker({position: %1, map: %2, icon: %3}) "

mkInfoWindow :: Text -> Fay InfoWindow
mkInfoWindow = ffi " new google.maps.InfoWindow({content: %1}) " 

openInfoWindow :: InfoWindow -> Map -> MapMarker -> Fay ()
openInfoWindow = ffi " %1['open'](%2, %3) "
  
addClickListener :: MapMarker -> Fay () -> Fay ()
addClickListener = ffi " google.maps.event.addListener(%1, 'click', %2) "

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
getCoordinates = ffi " (function () { var location = %1['geometry']['location']; return [location.lat(), location.lng()] })() "
