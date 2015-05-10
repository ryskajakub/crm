{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Validation where

import "fay-base" Prelude hiding (fail)
import qualified Crm.Shared.Machine as M

data Validation = Validation [ValidationFail]

data ValidationFail = MthNumber M.MachineId
  deriving Eq

new :: Validation
new = Validation []

add :: ValidationFail -> Validation -> Validation
add fail (Validation v) = Validation $ fail : v

remove :: ValidationFail -> Validation -> Validation
remove fail (Validation validations) = Validation $ filter (fail /=) validations
