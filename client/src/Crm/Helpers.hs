{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Helpers where

import "fay-base" Data.Text (Text, (<>), showInt, pack, fromString)
import "fay-base" Prelude hiding (div, span, id)
import FFI (Nullable, ffi)
import Data.Nullable (fromNullable)

import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.Company as C

import qualified HaskellReact.Bootstrap.CalendarInput as CI
import HaskellReact

parseInt :: Text -> Nullable Int
parseInt = ffi " (function() { var int = parseInt(%1); ret = ((typeof int) === 'number' && !isNaN(int)) ? int : null; return ret; })() "

parseSafely :: Text -> Maybe Int
parseSafely possibleNumber = fromNullable $ parseInt possibleNumber

displayDate :: YMD.YearMonthDay -> Text
displayDate (YMD.YearMonthDay y m d _) =
  showInt d <> "." <> showInt m <> "." <> showInt y

showCompanyId :: C.CompanyId -> Text
showCompanyId = showInt . C.getCompanyId

displayPrecision :: YMD.Precision -> CI.DisplayDatePrecision
displayPrecision displayPrecision' = case displayPrecision' of
  YMD.MonthPrecision -> CI.Month
  _ -> CI.Day

lmap :: (a -> a') -> (a,b) -> (a',b)
lmap f (a,b) = (f(a),b)

rmap :: (b -> b') -> (a,b) -> (a,b')
rmap f (a,b) = (a,f(b))

formRow :: Renderable a
        => Text -- ^ label of field
        -> a -- ^ the other field
        -> DOMElement
formRow labelText otherField = 
  div' (class' "form-group") [ 
    label' (class'' ["control-label", "col-md-3"]) (span labelText) , 
    div' (class' "col-md-9") otherField ]
