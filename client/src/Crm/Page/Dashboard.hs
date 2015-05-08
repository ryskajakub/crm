{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Dashboard (
  dashboard ) where

import "fay-base" Data.Text (pack)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Maybe (onJust)
import FFI
import DOM

import HaskellReact
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Shared.Company as C

import GoogleMaps


collect :: (a -> Maybe b) -> [a] -> [b]
collect f list = foldr (\e acc -> case f e of
  Just elem' -> elem' : acc 
  Nothing -> acc) [] list


dashboard :: [(C.CompanyId, C.Company, Maybe C.Coordinates)] -> (DOMElement, Fay ())
dashboard companies = let

  constructMap = do
    let 
      czCenter = mkLatLng 49.7437400818 15.3386173248
      mapOptions = mkMapOptions 8 czCenter
      companiesWithCoords = collect (\(a,b,coords) -> (\x -> (a,b,x)) `onJust` coords) companies
    mapContainer <- getElementById $ pack "dashboard-map"
    googleMap <- mkMap mapContainer mapOptions
    mapM_ (\(_,_,C.Coordinates lat lng) -> addMarker lat lng googleMap) companiesWithCoords
    return ()
    
  element = B.grid $ B.row $ B.col (B.mkColProps 12) $ 
    div' (mkAttrs { id = Defined $ pack "half-container" }) $ 
      div' (mkAttrs { id = Defined $ pack "dashboard-map" }) (pack "")

  in (element, constructMap)
