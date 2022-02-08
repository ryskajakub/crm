{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.MachineTypes.Photos where
import Data.Text (pack)
import qualified Crm.Client.MachineTypes as MachineTypes
import qualified Data.Text
import qualified Crm.Shared.Photo
import qualified Crm.Shared.PhotoMeta
import qualified Crm.Runtime
import qualified Crm.Router

list ::
     [(String, String)] ->
       MachineTypes.Identifier ->
         ([((,) (Crm.Shared.Photo.PhotoId)
              (Crm.Shared.PhotoMeta.PhotoMeta))]
            -> Fay ())
           -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) machineTypes callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "machine-types" ++ "/" ++
           MachineTypes.getInt' machineTypes ++ "/" ++ "photos"
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