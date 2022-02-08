{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Print where
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
      (pack $ "print" ++ "?" ++ pName ++ "=" ++ pValue ++
         concat
           (map (\ (pName', pValue') -> "&" ++ pName' ++ "=" ++ pValue') px))
      (callback . Crm.Runtime.items)
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router