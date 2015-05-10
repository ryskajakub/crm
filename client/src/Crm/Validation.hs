{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Validation where

import "fay-base" Data.Text (fromString, Text, showInt, (<>))
import "fay-base" Prelude hiding (fail)
import qualified Crm.Shared.Machine as M

data Validation = Validation [ValidationFail]

data ValidationFail = MthNumber M.MachineId
  deriving Eq

message :: ValidationFail -> Text
message validationFail = case validationFail of
  MthNumber companyId -> "Do motohodin se můžou vyplňovat pouze čísla (u stroje s id: " <> (showInt $ M.getMachineId companyId) <> ")"

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
