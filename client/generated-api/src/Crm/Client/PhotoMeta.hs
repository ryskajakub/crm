{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.PhotoMeta where
import Data.Text (pack)
import qualified Data.Text
import qualified Crm.Shared.PhotoMeta
import qualified Crm.Runtime
import qualified Crm.Router
import qualified Crm.Shared.Photo

saveByPhotoId ::
              Crm.Shared.Photo.PhotoId ->
                Crm.Shared.PhotoMeta.PhotoMeta ->
                  (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
saveByPhotoId photoId input callback router
  = Crm.Runtime.passwordAjax
      (pack $ "photo-meta" ++ "/" ++ getInt photoId)
      callback
      (Just input)
      Crm.Runtime.put
      Nothing
      Nothing
      router

type Identifier = Crm.Shared.Photo.PhotoId

getInt' :: Crm.Shared.Photo.PhotoId -> String
getInt' (Crm.Shared.Photo.PhotoId int) = "/" ++ show int

getInt :: Crm.Shared.Photo.PhotoId -> String
getInt (Crm.Shared.Photo.PhotoId int) = show int