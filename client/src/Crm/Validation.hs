{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Validation where

import           Data.Text          (fromString, Text, showInt, (<>))
import           Prelude            hiding (fail)
import qualified Crm.Shared.Machine as M


data Validation = Validation [ValidationFail]

data ValidationFail = 
  MthNumber M.MachineId |
  MachineUsageNumber |
  MachineInitialMileageNumber
  deriving Eq


message :: ValidationFail -> Text
message validationFail = case validationFail of
  MthNumber companyId -> "Do motohodin se můžou vyplňovat pouze čísla (u stroje s id: " <> (showInt $ M.getMachineId companyId) <> ")"
  MachineUsageNumber -> "Provoz musí být kladné číslo"
  MachineInitialMileageNumber -> "Úvodní stav motodin musí být číslo"

messages :: Validation -> [Text]
messages (Validation validation) = map message validation

new :: Validation
new = Validation []

add :: ValidationFail -> Validation -> Validation
add fail (Validation v) = case find (fail ==) v of
  Just _ -> Validation v
  Nothing -> Validation $ fail : v

remove :: ValidationFail -> Validation -> Validation
remove fail (Validation validations) = Validation $ filter (fail /=) validations

ok :: Validation -> Bool
ok (Validation v) = null v
