{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Shared.Company where

#ifndef FAY
import GHC.Generics
import Data.Data
import Rest.Info                  (Info(..))
import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
#endif
import Data.Text                  (Text, pack)

import Crm.Shared.YearMonthDay    as YMD

newtype CompanyId' id = CompanyId { getCompanyId :: id }
#ifdef FAY
  deriving (Show)
#else
  deriving (Generic, Typeable, Data, Ord, Eq, Show)
#endif
type CompanyId = CompanyId' Int

#ifndef FAY
instance Functor CompanyId' where
  f `fmap` (CompanyId companyId) = CompanyId . f $ companyId
#endif

#ifndef FAY
instance Info CompanyId where
  describe _ = "companyId"
instance Read CompanyId where
  readsPrec i = fmap (\(a,b) -> (CompanyId a, b)) `fmap` readsPrec i
#endif

data OrderType = CompanyName | NextService
  deriving (Show, Read)

data CompanyState =
  ExactDate YMD.YearMonthDay |
  CantTellDate |
  NotImportant
#ifndef FAY
  deriving (Generic, Typeable, Data, Ord, Eq, Show)
#endif

getDate :: CompanyState -> Maybe YMD.YearMonthDay
getDate (ExactDate ed) = Just ed
getDate _ = Nothing

data Coordinates' latitude longitude = Coordinates {
  latitude :: latitude ,
  longitude :: longitude }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif
type Coordinates = Coordinates' Double Double
type CoordinatesJoin = Coordinates' (Maybe Double) (Maybe Double)

data Company' name note address = Company {
  companyName :: name ,
  companyNote :: note ,
  companyAddress :: address }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif
type Company = Company' Text Text Text

mkCoordinates :: (Double, Double) -> Coordinates
mkCoordinates (lat, lng) = Coordinates lat lng

newCompany :: Company
newCompany = Company (pack "") (pack "") (pack "")

#ifndef FAY
mapCoordinates :: Coordinates' (Maybe a) (Maybe b) -> Maybe (Coordinates' a b) 
mapCoordinates coordinates = pure Coordinates <*> latitude coordinates <*> longitude coordinates

makeAdaptorAndInstance' ''Company'
makeAdaptorAndInstance' ''Coordinates'
makeAdaptorAndInstance' ''CompanyId'
#endif
