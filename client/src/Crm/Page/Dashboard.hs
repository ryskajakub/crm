{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE MultiWayIf #-}

module Crm.Page.Dashboard (
  dashboard ) where

import "fay-base" Data.Text (pack, (<>), Text)
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Maybe (onJust, mapMaybe)
import FFI
import DOM

import HaskellReact
import qualified HaskellReact.Bootstrap as B

import GoogleMaps
import qualified Moment as M

import qualified Crm.Shared.Company as C
import qualified Crm.Shared.YearMonthDay as YMD
import Crm.Helpers (pageInfo)


toHexa :: Int -> Text
toHexa = ffi " (%1).toString(16) "


computeColor :: YMD.YearMonthDay -> Fay Text
computeColor (YMD.YearMonthDay y m d _) = do
  let moment = M.requireMoment
  let today = M.now moment
  let 
    theOtherDay = M.dayPrecision y m d moment
    diff' = fromIntegral $ M.diff today theOtherDay M.Days
    year = 366 :: Double
    colorScaleSize = 256 :: Double
    diff = if diff' > year then year else diff'
    halfYear = year / 2 
    unit = colorScaleSize / halfYear
    firstPart = if diff > halfYear then halfYear else diff
    secondPart = if diff - halfYear > 0 then diff - halfYear else 0
    redPart = toHexa $ truncate $ colorScaleSize - secondPart * unit
    greenPart = toHexa $ truncate $ unit * firstPart
  return $ pack "#" <> redPart <> greenPart <> pack "00"
  

dashboard :: [(C.CompanyId, C.Company, Maybe YMD.YearMonthDay, Maybe C.Coordinates)] -> (DOMElement, Fay ())
dashboard companies = let

  constructMap = do
    let 
      czCenter = mkLatLng 49.7437400818 15.3386173248
      mapOptions = mkMapOptions 8 czCenter
      companiesWithCoords = mapMaybe (\(a,b,mbYmd,coords) -> (\x -> (a,b,mbYmd,x)) `onJust` coords) companies
    mapContainer <- getElementById $ pack "dashboard-map"
    googleMap <- mkMap mapContainer mapOptions
    mapM_ (\(_,_,_,C.Coordinates lat lng) -> addMarker lat lng googleMap) companiesWithCoords
    return ()
    
  info = pageInfo (pack "Nástěnka") $ Just $ pack "Mapa firem. Firma se na mapě zobrazí podle vyplněné adresy."
  element = B.grid $ B.row $
    info ++ [B.col (B.mkColProps 12) $ 
      div' (mkAttrs { id = Defined $ pack "half-container" }) $ 
        div' (mkAttrs { id = Defined $ pack "dashboard-map" }) (pack "") ]

  in (element, constructMap)
