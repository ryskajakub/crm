{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Crm.Shared.ServerRender where

#ifndef FAY
import Data.Data
import GHC.Generics
#endif
import Data.Text    (Text)

data Markup = 
  PlainText { getPlainText :: Text } |
  UnorderedList { getUnorderedList :: [Text] } |
  Header { getHeader :: Text }
#ifndef FAY
  deriving (Generic, Typeable, Data, Eq)

instance Show Markup where
  show (PlainText pt) = "PT " ++ (take 100 $ show pt) ++ "\n" 
  show (UnorderedList ul) = concat $ map (\t -> "UL " ++ (take 100 $ show t) ++ "\n") ul
  show (Header h) = "H " ++ (take 100 $ show h) ++ "\n"
#endif
