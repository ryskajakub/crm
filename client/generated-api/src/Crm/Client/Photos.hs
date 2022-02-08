{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Photos where
import Data.Text (pack)
import qualified Data.Text
import qualified Crm.Runtime
import qualified Crm.Router
import qualified Crm.Shared.Photo

remove ::
       Identifier -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
remove photos callback router
  = Crm.Runtime.passwordAjax
      (pack $ "photos" ++ "/" ++ getInt' photos)
      callback
      Nothing
      Crm.Runtime.delete
      Nothing
      Nothing
      router

type Identifier = Crm.Shared.Photo.PhotoId

getInt' :: Crm.Shared.Photo.PhotoId -> String
getInt' (Crm.Shared.Photo.PhotoId int) = "/" ++ show int

getInt :: Crm.Shared.Photo.PhotoId -> String
getInt (Crm.Shared.Photo.PhotoId int) = show int