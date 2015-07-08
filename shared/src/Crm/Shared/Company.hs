{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}

module Crm.Shared.Company where

#ifndef FAY
import GHC.Generics
import Data.Data
import Rest.Info    (Info(..))
#endif
import Data.Text    (Text, pack)

newtype CompanyId' id = CompanyId { getCompanyId :: id }
#ifdef FAY
  deriving (Show)
#else
  deriving (Generic, Typeable, Data, Ord, Eq, Show)
#endif
type CompanyId = CompanyId' Int

instance Functor CompanyId' where
  f `fmap` (CompanyId companyId) = CompanyId . f $ companyId

#ifndef FAY
instance Info CompanyId where
  describe _ = "companyId"
instance Read CompanyId where 
  readsPrec i = fmap (\(a,b) -> (CompanyId a, b)) `fmap` readsPrec i
#endif

data OrderType = CompanyName | NextService
  deriving (Show, Read)

data Coordinates' latitude longitude = Coordinates {
  latitude :: latitude ,
  longitude :: longitude }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif
type Coordinates = Coordinates' Double Double
type CoordinatesJoin = Coordinates' (Maybe Double) (Maybe Double)

data Company' name plant address = Company {
  companyName :: name , 
  companyPlant :: plant ,
  companyAddress :: address }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif
type Company = Company' Text Text Text

mkCoordinates :: (Double, Double) -> Coordinates
mkCoordinates (lat, lng) = Coordinates lat lng

newCompany :: Company
newCompany = Company (pack "") (pack "") (pack "")
