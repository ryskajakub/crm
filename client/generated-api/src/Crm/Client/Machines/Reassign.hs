{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Machines.Reassign where
import Data.Text (pack)
import qualified Crm.Client.Machines as Machines
import qualified Data.Text
import qualified Crm.Server.Boilerplate
import qualified Crm.Runtime
import qualified Crm.Router

do_ ::
    Machines.Identifier ->
      Crm.Server.Boilerplate.ReassignPayload ->
        (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
do_ machines input callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "machines" ++ "/" ++
           Machines.getInt' machines ++ "/" ++ "reassign" ++ "/" ++ "do")
      callback
      (Just input)
      Crm.Runtime.modify
      Nothing
      Nothing
      router