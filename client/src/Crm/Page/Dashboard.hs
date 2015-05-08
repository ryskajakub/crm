{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Dashboard (
  dashboard ) where

import "fay-base" Data.Text (pack)
import "fay-base" Prelude hiding (div, span, id)
import FFI
import DOM

import HaskellReact
import qualified HaskellReact.Bootstrap as B

import qualified Crm.Shared.Company as C

import GoogleMaps

dashboard :: [(C.CompanyId, C.Company, Maybe C.Coordinates)] -> (DOMElement, Fay ())
dashboard _ = let

  constructMap = do
    let czCenter = mkLatLng 49.7437400818 15.3386173248
    let mapOptions = MapOptions { zoom = Defined 8 , center = Defined czCenter }
    mapContainer <- getElementById $ pack "dashboard-map"
    _ <- mkMap mapContainer mapOptions
    return ()
    
  element = B.grid $ B.row $ B.col (B.mkColProps 12) $ 
    div' (mkAttrs { id = Defined $ pack "half-container" }) $ 
      div' (mkAttrs { id = Defined $ pack "dashboard-map" }) (pack "")

  in (element, startMapOnLoad constructMap )
