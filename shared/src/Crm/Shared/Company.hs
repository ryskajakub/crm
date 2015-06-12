{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.Company where

#ifndef FAY
import GHC.Generics
import Data.Data
import Rest.Info    (Info(..))
#endif
import Data.Text    (Text, pack)

newtype CompanyId = CompanyId { getCompanyId :: Int }
#ifdef FAY
  deriving (Show)
#else
  deriving (Generic, Typeable, Data, Ord, Eq, Show)
#endif

#ifndef FAY
instance Info CompanyId where
  describe _ = "companyId"
instance Read CompanyId where 
  readsPrec i = fmap (\(a,b) -> (CompanyId a, b)) `fmap` readsPrec i
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
