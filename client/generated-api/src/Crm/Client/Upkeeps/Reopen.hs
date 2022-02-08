{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Upkeeps.Reopen where
import Data.Text (pack)
import qualified Crm.Client.Upkeeps as Upkeeps
import qualified Data.Text
import qualified Crm.Runtime
import qualified Crm.Router

create ::
       Upkeeps.Identifier ->
         Double -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
create upkeeps input callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "upkeeps" ++ "/" ++ Upkeeps.getInt' upkeeps ++ "/" ++ "reopen")
      callback
      (Just input)
      Crm.Runtime.post
      Nothing
      Nothing
      router