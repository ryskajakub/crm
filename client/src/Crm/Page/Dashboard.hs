{-# LANGUAGE MultiWayIf #-}

module Crm.Page.Dashboard (
  dashboard ) where

import           Data.Text               (pack, unpack)
import           Prelude                 hiding (div, span, id, length)
import           Data.Maybe              (onJust, mapMaybe)
import           FFI
import           DOM

import           HaskellReact
import qualified HaskellReact.Bootstrap  as B

import           GoogleMaps

import qualified Crm.Shared.Company      as C
import           Crm.Helpers
import qualified Crm.Router              as R


dashboard :: 
  R.CrmRouter -> 
  [(C.CompanyId, C.Company, C.CompanyState, Maybe C.Coordinates)] -> 
  (DOMElement, Fay ())
dashboard _ companies = (element, constructMap) where

  constructMap = do
    let 
      czCenter = mkLatLng 49.7437400818 15.3386173248
      mapOptions = mkMapOptions 8 czCenter (pack "b58810a96a89ef00")
      companiesWithCoords = mapMaybe (\(a,b,mbYmd,coords) -> (\x -> (a,b,mbYmd,x)) `onJust` coords) companies
    mapContainer <- getElementById $ pack "dashboard-map"
    googleMap <- mkMap mapContainer mapOptions
    forM_ companiesWithCoords $ \(companyId,company,date,C.Coordinates lat lng) -> do
      let color' = case date of
           C.ExactDate ymd -> computeColor ymd
           C.CantTellDate -> pack "777"
           C.Planned -> pack "fff"
           C.Inactive -> pack "000"
      let theLink = R.routeToText . R.companyDetail $ companyId
      marker <- addMarker lat lng color' googleMap
      infoWindow <- mkInfoWindow . pack $ "<h3><a href=\"" ++ unpack theLink ++ "\">" ++ (unpack . C.companyName $ company) ++ "</a></h3>"
      let handler = openInfoWindow infoWindow googleMap marker
      addClickListener marker handler
    return ()
    
  element = B.grid $ B.row $
    info ++ [B.col (B.mkColProps 12) $ 
      div' (mkAttrs { id = Defined $ pack "half-container" }) $ 
        div' (mkAttrs { id = Defined $ pack "dashboard-map" }) (pack "") ]
    where
    info = pageInfo (pack "Nástěnka") $ Just $ pack "Mapa firem. Firma se na mapě zobrazí podle vyplněné adresy."
