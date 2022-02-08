{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Task where
import Data.Text (pack)
import qualified Data.Text
import qualified Crm.Shared.Task
import qualified Crm.Runtime
import qualified Crm.Router

byTaskId ::
         Crm.Shared.Task.TaskId ->
           (Crm.Shared.Task.Task' (Data.Text.Text) -> Fay ()) ->
             Crm.Router.CrmRouter -> Fay ()
byTaskId taskId callback router
  = Crm.Runtime.passwordAjax (pack $ "task" ++ "/" ++ getInt taskId)
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

saveByTaskId ::
             Crm.Shared.Task.TaskId ->
               Crm.Shared.Task.Task' (Data.Text.Text) ->
                 (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
saveByTaskId taskId input callback router
  = Crm.Runtime.passwordAjax (pack $ "task" ++ "/" ++ getInt taskId)
      callback
      (Just input)
      Crm.Runtime.put
      Nothing
      Nothing
      router

type Identifier = Crm.Shared.Task.TaskId

getInt' :: Crm.Shared.Task.TaskId -> String
getInt' (Crm.Shared.Task.TaskId int) = "/" ++ show int

getInt :: Crm.Shared.Task.TaskId -> String
getInt (Crm.Shared.Task.TaskId int) = show int