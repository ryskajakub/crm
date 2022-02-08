{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.MachineKind where
import Data.Text (pack)
import qualified Data.Text
import qualified Crm.Shared.MachineKind
import qualified Crm.Shared.ExtraField
import qualified Crm.Runtime
import qualified Crm.Router

byString ::
         String ->
           ([((,) (Crm.Shared.MachineKind.MachineKindEnum)
                ([((,) (Crm.Shared.ExtraField.ExtraFieldId)
                     (Crm.Shared.MachineKind.MachineKindSpecific))]))]
              -> Fay ())
             -> Crm.Router.CrmRouter -> Fay ()
byString string callback router
  = Crm.Runtime.passwordAjax (pack $ "machine-kind" ++ "/" ++ string)
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

saveByString ::
             String ->
               [((,) (Crm.Shared.MachineKind.MachineKindEnum)
                   ([((,) (Crm.Shared.ExtraField.ExtraFieldIdentification)
                        (Crm.Shared.MachineKind.MachineKindSpecific))]))]
                 -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
saveByString string input callback router
  = Crm.Runtime.passwordAjax (pack $ "machine-kind" ++ "/" ++ string)
      callback
      (Just input)
      Crm.Runtime.put
      Nothing
      Nothing
      router