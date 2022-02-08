{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Employees.Upkeeps where
import Data.Text (pack)
import qualified Crm.Client.Employees as Employees
import qualified Data.Text
import qualified Crm.Shared.Upkeep
import qualified Crm.Shared.YearMonthDay
import qualified Crm.Shared.ServerRender
import qualified Crm.Shared.Company
import qualified Crm.Shared.Employee
import qualified Crm.Shared.Machine
import qualified Crm.Shared.MachineType
import qualified Crm.Shared.MachineKind
import qualified Crm.Shared.MyMaybe
import qualified Crm.Shared.ContactPerson
import qualified Crm.Shared.UpkeepMachine
import qualified Crm.Runtime
import qualified Crm.Router

listPrint ::
          [(String, String)] ->
            Employees.Identifier ->
              ([((,,,)
                   (Crm.Shared.Upkeep.UpkeepGen''
                      (Crm.Shared.YearMonthDay.YearMonthDay)
                      (Bool)
                      (Data.Text.Text)
                      (Bool)
                      ([(Crm.Shared.ServerRender.Markup)])
                      (Data.Text.Text))
                   (Crm.Shared.Company.Company' (Data.Text.Text) (Data.Text.Text)
                      (Data.Text.Text)
                      (Bool))
                   ([((,) (Crm.Shared.Employee.EmployeeId)
                        (Crm.Shared.Employee.Employee))])
                   ([((,,,)
                        (Crm.Shared.Machine.Machine'
                           (Maybe (Crm.Shared.YearMonthDay.YearMonthDay))
                           (Int)
                           (Int)
                           (Data.Text.Text)
                           (Data.Text.Text)
                           (Data.Text.Text)
                           (Bool)
                           (Data.Text.Text)
                           (Crm.Shared.Machine.UpkeepBy))
                        (Crm.Shared.MachineType.MachineType'
                           (Crm.Shared.MachineKind.MachineKindEnum)
                           (Data.Text.Text)
                           (Data.Text.Text))
                        (Crm.Shared.MyMaybe.MyMaybe
                           (Crm.Shared.ContactPerson.ContactPerson))
                        ((,)
                           (Crm.Shared.UpkeepMachine.UpkeepMachineGen' (Data.Text.Text) (Int)
                              (Bool)
                              (Data.Text.Text)
                              (Crm.Shared.UpkeepMachine.UpkeepType))
                           (Crm.Shared.MyMaybe.MyMaybe
                              ([(Crm.Shared.ServerRender.Markup)]))))]))]
                 -> Fay ())
                -> Crm.Router.CrmRouter -> Fay ()
listPrint ((pName, pValue) : px) employees callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "employees" ++ "/" ++
           Employees.getInt' employees ++ "/" ++ "upkeeps" ++ "/" ++ "print"
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