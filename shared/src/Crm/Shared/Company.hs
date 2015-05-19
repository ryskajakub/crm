{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.Company where

#ifndef FAY
import GHC.Generics
import Data.Data
#endif
import Data.Text (Text, pack)

newtype CompanyId = CompanyId { getCompanyId :: Int }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show)
#endif

data OrderType = CompanyName | NextService
  deriving (Show, Read)

data Coordinates = Coordinates {
  latitude :: Double ,
  longitude :: Double }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif

data Company = Company {
  companyName :: Text , 
  companyPlant :: Text ,
  companyAddress :: Text }
#ifndef FAY
  deriving (Generic, Typeable, Data)
#endif

mkCoordinates :: (Double, Double) -> Coordinates
mkCoordinates (lat, lng) = Coordinates lat lng

newCompany :: Company
newCompany = Company (pack "") (pack "") (pack "")
