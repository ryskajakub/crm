{-# LANGUAGE MultiWayIf #-}

module Crm.Page.Dashboard (
  dashboard ) where

import           Data.Text               (pack)
import           Prelude                 hiding (div, span, id, length)
import           Data.Maybe              (onJust, mapMaybe)
import           FFI
import           DOM

import           HaskellReact
import qualified HaskellReact.Bootstrap  as B

import           GoogleMaps

import qualified Crm.Shared.Company      as C
import qualified Crm.Shared.YearMonthDay as YMD
import           Crm.Helpers
import qualified Crm.Router              as R


dashboard :: R.CrmRouter
          -> [(C.CompanyId, C.Company, Maybe YMD.YearMonthDay, Maybe C.Coordinates)] 
          -> (DOMElement, Fay ())
dashboard router companies = (element, constructMap) where

  constructMap = do
    let 
      czCenter = mkLatLng 49.7437400818 15.3386173248
      mapOptions = mkMapOptions 8 czCenter
      companiesWithCoords = mapMaybe (\(a,b,mbYmd,coords) -> (\x -> (a,b,mbYmd,x)) `onJust` coords) companies
    mapContainer <- getElementById $ pack "dashboard-map"
    googleMap <- mkMap mapContainer mapOptions
    forM_ companiesWithCoords $ \(companyId,_,date,C.Coordinates lat lng) -> do
      let color' = maybe (pack "777777") computeColor date 
      marker <- addMarker lat lng color' googleMap
      let handler = R.navigate (R.companyDetail companyId) router
      addClickListener marker handler
    return ()
    
  element = B.grid $ B.row $
    info ++ [B.col (B.mkColProps 12) $ 
      div' (mkAttrs { id = Defined $ pack "half-container" }) $ 
        div' (mkAttrs { id = Defined $ pack "dashboard-map" }) (pack "") ]
    where
    info = pageInfo (pack "Nástěnka") $ Just $ pack "Mapa firem. Firma se na mapě zobrazí podle vyplněné adresy."
