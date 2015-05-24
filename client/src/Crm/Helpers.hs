{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Helpers where

import Data.Text as T (Text, showInt, fromString, (<>), length)
import Prelude as P hiding (div, span, id)
import FFI (Nullable, ffi)
import Data.Nullable (fromNullable)

import qualified Crm.Shared.YearMonthDay as YMD
import qualified Crm.Shared.Company as C

import HaskellReact
import qualified HaskellReact.Bootstrap.CalendarInput as CI
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Alert as A

import Moment as M

import qualified JQuery as JQ

data FileList
data File
data FileContents

validationHtml :: (Renderable a) => [a] -> DOMElement
validationHtml validationMessages = let
  validationMessagesHtml = map (\message -> p message) validationMessages
  in if null validationMessages
    then text2DOM ""
    else B.grid $ B.row $ B.col (B.mkColProps 12) (A.alert A.Danger validationMessagesHtml)

zipWithIndex :: [a] -> [(Int, a)] 
zipWithIndex sequences = let
  lastIndex = P.length sequences - 1
  indices = [0..lastIndex]
  in zip indices sequences

pageInfo :: Renderable a
         => Text
         -> Maybe a
         -> [DOMElement]
pageInfo header alertContent' = [
  B.col (B.mkColProps 12) (h2 header) ] ++ 
  case alertContent' of 
    Just alertContent -> [ B.col (B.mkColProps 12) (A.alert A.Info alertContent) ]
    Nothing -> []

getFileList :: JQ.JQuery -> Fay FileList
getFileList = ffi " %1['prop']('files') "

fileListLength :: FileList -> Fay Int
fileListLength = ffi " %1['length'] "

fileListElem :: Int -> FileList -> Fay File
fileListElem = ffi " %2[%1] "

fileType :: File -> Fay Text
fileType = ffi " %1['type'] "

fileName :: File -> Fay Text
fileName = ffi " %1['name'] "

fileContents :: File 
             -> (FileContents -> Fay ())
             -> Fay ()
fileContents = ffi "\
\ (function () {\
  \ var reader = new FileReader(); \
  \ reader.onload = function (theFile) { %2(reader.result) }; \
  \ reader.readAsBinaryString(%1); \
\ })() \ 
\ "

parseInt :: Text -> Nullable Int
parseInt = ffi " (function() { var int = parseInt(%1); ret = ((typeof int) === 'number' && !isNaN(int)) ? int : null; return ret; })() "

parseSafely :: Text -> Maybe Int
parseSafely possibleNumber = fromNullable $ parseInt possibleNumber

displayDate :: YMD.YearMonthDay -> Text
displayDate (YMD.YearMonthDay y m d precision) = let
  dateAsMoment = dayPrecision y m d requireMoment
  in format dateAsMoment (case precision of
    YMD.MonthPrecision -> "MMMM YYYY"
    _ -> "LL")

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

eventInt' :: (Int -> Fay ()) -> (Text -> Fay ()) -> Text -> Fay ()
eventInt' success errorFun text = case parseSafely text of
  Just(int) -> success int
  Nothing | text == "" -> success 0
  Nothing -> errorFun text

eventInt :: (Int -> Fay ()) -> Text -> Fay ()
eventInt fun = eventInt' fun (const $ return ())


toHexa' :: Int -> Text
toHexa' = ffi " (%1).toString(16) "

toHexa :: Int -> Text
toHexa int = let
  hexa = toHexa' int
  in if T.length hexa == 1 then "0" <> hexa else hexa

computeColor :: YMD.YearMonthDay -> Text
computeColor (YMD.YearMonthDay y m d _) = let
  moment = M.requireMoment
  today = M.now moment
  theOtherDay = M.dayPrecision y m d moment
  diff' = abs $ fromIntegral $ M.diff today theOtherDay M.Days
  year = 366 :: Double
  colorScaleSize = 255 :: Double
  daysDiff = if diff' > year then year else diff'
  halfYear = year / 2 
  unit = colorScaleSize / halfYear
  firstPart = if daysDiff > halfYear then halfYear else daysDiff
  secondPartDiff = if daysDiff - halfYear > 0 then daysDiff - halfYear else 0
  redPart = truncate $ colorScaleSize - (secondPartDiff * unit)
  greenPart = truncate $ unit * firstPart
  in toHexa redPart <> toHexa greenPart <> "00"
