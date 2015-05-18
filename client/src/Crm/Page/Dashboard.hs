{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE MultiWayIf #-}

module Crm.Page.Dashboard (
  dashboard ) where

import "fay-base" Data.Text (pack, (<>), Text, length)
import "fay-base" Prelude hiding (div, span, id, length)
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


toHexa' :: Int -> Text
toHexa' = ffi " (%1).toString(16) "


toHexa :: Int -> Text
toHexa int = let
  hexa = toHexa' int
  in if length hexa == 1 then pack "0" <> hexa else hexa


computeColor :: YMD.YearMonthDay -> Text
computeColor (YMD.YearMonthDay y m d _) = let
  moment = M.requireMoment
  today = M.now moment
  theOtherDay = M.dayPrecision y m d moment
  diff' = fromIntegral $ M.diff today theOtherDay M.Days
  year = 366 :: Double
  colorScaleSize = 255 :: Double
  diff = abs $ if diff' > year then year else diff'
  halfYear = year / 2 
  unit = colorScaleSize / halfYear
  firstPart = if diff > halfYear then halfYear else diff
  secondPartDiff = if diff - halfYear > 0 then diff - halfYear else 0
  redPart = truncate $ colorScaleSize - (secondPartDiff * unit)
  greenPart = truncate $ unit * firstPart
  in toHexa redPart <> toHexa greenPart <> pack "00"
  

dashboard :: [(C.CompanyId, C.Company, Maybe YMD.YearMonthDay, Maybe C.Coordinates)] -> (DOMElement, Fay ())
dashboard companies = let

  constructMap = do
    let 
      czCenter = mkLatLng 49.7437400818 15.3386173248
      mapOptions = mkMapOptions 8 czCenter
      companiesWithCoords = mapMaybe (\(a,b,mbYmd,coords) -> (\x -> (a,b,mbYmd,x)) `onJust` coords) companies
    mapContainer <- getElementById $ pack "dashboard-map"
    googleMap <- mkMap mapContainer mapOptions
    forM_ companiesWithCoords $ \(_,_,date,C.Coordinates lat lng) -> let
      color = maybe (pack "777777") computeColor date 
      in addMarker lat lng color googleMap
    return ()
    
  info = pageInfo (pack "Nástěnka") $ Just $ pack "Mapa firem. Firma se na mapě zobrazí podle vyplněné adresy."
  element = B.grid $ B.row $
    info ++ [B.col (B.mkColProps 12) $ 
      div' (mkAttrs { id = Defined $ pack "half-container" }) $ 
        div' (mkAttrs { id = Defined $ pack "dashboard-map" }) (pack "") ]

  in (element, constructMap)
