module Crm.Shared.Data (
  Company(..)
) where

data Company = Company {
  name :: String
  , plant :: String
}
