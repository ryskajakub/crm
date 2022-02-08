{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Machines.Photos where
import Data.Text (pack)
import qualified Crm.Client.Machines as Machines
import qualified Data.Text
import qualified Crm.Shared.Photo
import qualified Crm.Shared.PhotoMeta
import qualified Crm.Runtime
import qualified Crm.Router

list ::
     [(String, String)] ->
       Machines.Identifier ->
         ([((,) (Crm.Shared.Photo.PhotoId)
              (Crm.Shared.PhotoMeta.PhotoMeta))]
            -> Fay ())
           -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) machines callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "machines" ++ "/" ++ Machines.getInt' machines ++ "/" ++ "photos"
         ++ "?" ++ pName ++ "=" ++ pValue
         ++
         concat
           (map (\ (pName', pValue') -> "&" ++ pName' ++ "=" ++ pValue') px))
      (callback . Crm.Runtime.items)
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router