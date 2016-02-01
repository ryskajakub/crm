{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Crm.Shared.UpkeepSequence where

#ifndef FAY
import GHC.Generics
import Data.Data
import Data.Profunctor.Product.TH (makeAdaptorAndInstance')
#endif
import Data.Text (Text, pack)

data UpkeepSequence' displayOrdering label repetition oneTime = UpkeepSequence {
  displayOrdering :: displayOrdering , 
  label_ :: label ,
  repetition :: repetition ,
  oneTime :: oneTime }
#ifndef FAY
  deriving (Generic, Typeable, Data, Show, Eq)
#endif

type UpkeepSequence = UpkeepSequence' Int Text Int Bool

newUpkeepSequence :: UpkeepSequence
newUpkeepSequence = UpkeepSequence 0 (pack "") 0 False

#ifndef FAY
makeAdaptorAndInstance' ''UpkeepSequence'
#endif
