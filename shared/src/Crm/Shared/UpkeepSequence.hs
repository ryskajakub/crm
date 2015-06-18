{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.UpkeepSequence where

#ifndef FAY
import GHC.Generics
import Data.Data
#endif
import Data.Text (Text, pack)

data UpkeepSequence = UpkeepSequence {
  displayOrdering :: Int , 
  label_ :: Text ,
  repetition :: Int ,
  oneTime :: Bool }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show, Eq)
#endif

newUpkeepSequence :: UpkeepSequence
newUpkeepSequence = UpkeepSequence 0 (pack "") 0 False
