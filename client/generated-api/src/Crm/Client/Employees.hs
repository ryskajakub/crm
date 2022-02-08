{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Employees where
import Data.Text (pack)
import qualified Data.Text
import qualified Crm.Shared.Employee
import qualified Crm.Runtime
import qualified Crm.Router

list ::
     [(String, String)] ->
       ([((,) (Crm.Shared.Employee.EmployeeId)
            (Crm.Shared.Employee.Employee))]
          -> Fay ())
         -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) callback router
  = Crm.Runtime.passwordAjax
      (pack $ "employees" ++ "?" ++ pName ++ "=" ++ pValue ++
         concat
           (map (\ (pName', pValue') -> "&" ++ pName' ++ "=" ++ pValue') px))
      (callback . Crm.Runtime.items)
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

bySingle ::
         Crm.Shared.Employee.EmployeeId ->
           (Crm.Shared.Employee.Employee -> Fay ()) ->
             Crm.Router.CrmRouter -> Fay ()
bySingle employeeId callback router
  = Crm.Runtime.passwordAjax
      (pack $ "employees" ++ "/" ++ "single" ++ "/" ++ getInt employeeId)
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

saveBySingle ::
             Crm.Shared.Employee.EmployeeId ->
               Crm.Shared.Employee.Employee ->
                 (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
saveBySingle employeeId input callback router
  = Crm.Runtime.passwordAjax
      (pack $ "employees" ++ "/" ++ "single" ++ "/" ++ getInt employeeId)
      callback
      (Just input)
      Crm.Runtime.put
      Nothing
      Nothing
      router

listColours ::
            [(String, String)] ->
              ([(Data.Text.Text)] -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
listColours ((pName, pValue) : px) callback router
  = Crm.Runtime.passwordAjax
      (pack $ "employees" ++ "/" ++ "colours" ++
         "?" ++ pName ++ "=" ++ pValue
         ++
         concat
           (map (\ (pName', pValue') -> "&" ++ pName' ++ "=" ++ pValue') px))
      (callback . Crm.Runtime.items)
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

create ::
       Crm.Shared.Employee.Employee ->
         (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
create input callback router
  = Crm.Runtime.passwordAjax (pack $ "employees") callback
      (Just input)
      Crm.Runtime.post
      Nothing
      Nothing
      router

type Identifier = Crm.Shared.Employee.EmployeeId

getInt' :: Crm.Shared.Employee.EmployeeId -> String
getInt' (Crm.Shared.Employee.EmployeeId int)
  = "single/" ++ show int

getInt :: Crm.Shared.Employee.EmployeeId -> String
getInt (Crm.Shared.Employee.EmployeeId int) = show int