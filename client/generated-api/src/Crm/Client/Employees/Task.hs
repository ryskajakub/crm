{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Employees.Task where
import Data.Text (pack)
import qualified Crm.Client.Employees as Employees
import qualified Data.Text
import qualified Crm.Shared.Task
import qualified Crm.Shared.ServerRender
import qualified Crm.Runtime
import qualified Crm.Router

list ::
     [(String, String)] ->
       Employees.Identifier ->
         ([((,) (Crm.Shared.Task.TaskId)
              (Crm.Shared.Task.Task' ([(Crm.Shared.ServerRender.Markup)])))]
            -> Fay ())
           -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) employees callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "employees" ++ "/" ++ Employees.getInt' employees ++ "/" ++ "task"
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

listMarkupTasks ::
                [(String, String)] ->
                  Employees.Identifier ->
                    ([((,) (Crm.Shared.Task.TaskId)
                         (Crm.Shared.Task.Task' ([(Crm.Shared.ServerRender.Markup)])))]
                       -> Fay ())
                      -> Crm.Router.CrmRouter -> Fay ()
listMarkupTasks ((pName, pValue) : px) employees callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "employees" ++ "/" ++
           Employees.getInt' employees ++ "/" ++
             "task" ++ "/" ++ "markup-tasks"
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

tasks ::
      Employees.Identifier ->
        ((,)
           ([((,) (Crm.Shared.Task.TaskId)
                (Crm.Shared.Task.Task' (Data.Text.Text)))])
           ([((,) (Crm.Shared.Task.TaskId)
                (Crm.Shared.Task.Task' (Data.Text.Text)))])
           -> Fay ())
          -> Crm.Router.CrmRouter -> Fay ()
tasks employees callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "employees" ++ "/" ++
           Employees.getInt' employees ++ "/" ++ "task" ++ "/" ++ "tasks")
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

create ::
       Employees.Identifier ->
         Crm.Shared.Task.Task' (Data.Text.Text) ->
           (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
create employees input callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "employees" ++ "/" ++ Employees.getInt' employees ++ "/" ++ "task")
      callback
      (Just input)
      Crm.Runtime.post
      Nothing
      Nothing
      router