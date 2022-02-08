{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Upkeeps where
import Data.Text (pack)
import qualified Data.Text
import qualified Crm.Shared.Upkeep
import qualified Crm.Shared.YearMonthDay
import qualified Crm.Shared.UpkeepMachine
import qualified Crm.Shared.Machine
import qualified Crm.Runtime
import qualified Crm.Router
import qualified Crm.Shared.Company
import qualified Crm.Shared.MachineKind
import qualified Crm.Shared.Employee
import qualified Crm.Shared.MyMaybe
import qualified Crm.Shared.ServerRender
import qualified Crm.Shared.MachineType
import qualified Crm.Shared.ContactPerson
import qualified Crm.Shared.UpkeepSequence

list ::
     [(String, String)] ->
       ([((,,,) (Crm.Shared.Upkeep.UpkeepId' (Int))
            (Maybe (Crm.Shared.Upkeep.UpkeepId' (Int)))
            (Crm.Shared.Upkeep.UpkeepGen''
               (Crm.Shared.YearMonthDay.YearMonthDay)
               (Bool)
               (Data.Text.Text)
               (Bool)
               (Data.Text.Text)
               (Data.Text.Text))
            ([((,)
                 (Crm.Shared.UpkeepMachine.UpkeepMachineGen' (Data.Text.Text) (Int)
                    (Bool)
                    (Data.Text.Text)
                    (Crm.Shared.UpkeepMachine.UpkeepType))
                 (Crm.Shared.Machine.MachineId' (Int)))]))]
          -> Fay ())
         -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) callback router
  = Crm.Runtime.passwordAjax
      (pack $ "upkeeps" ++ "?" ++ pName ++ "=" ++ pValue ++
         concat
           (map (\ (pName', pValue') -> "&" ++ pName' ++ "=" ++ pValue') px))
      (callback . Crm.Runtime.items)
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

listCalled ::
           [(String, String)] ->
             ([([((,,,,,) (Crm.Shared.Upkeep.UpkeepId' (Int))
                    (Crm.Shared.Upkeep.UpkeepGen''
                       (Crm.Shared.YearMonthDay.YearMonthDay)
                       (Bool)
                       (Data.Text.Text)
                       (Bool)
                       (Data.Text.Text)
                       (Data.Text.Text))
                    (Crm.Shared.Company.CompanyId' (Int))
                    (Crm.Shared.Company.Company' (Data.Text.Text) (Data.Text.Text)
                       (Data.Text.Text)
                       (Bool))
                    ([((,,,) (Crm.Shared.Machine.MachineId' (Int)) (Data.Text.Text)
                         (Data.Text.Text)
                         (Crm.Shared.MachineKind.MachineKindEnum))])
                    ([((,) (Crm.Shared.Employee.EmployeeId)
                         (Crm.Shared.Employee.Employee))]))])]
                -> Fay ())
               -> Crm.Router.CrmRouter -> Fay ()
listCalled ((pName, pValue) : px) callback router
  = Crm.Runtime.passwordAjax
      (pack $ "upkeeps" ++ "/" ++ "called" ++
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

listPlanned ::
            [(String, String)] ->
              ([([((,,,,,,) (Crm.Shared.Upkeep.UpkeepId' (Int))
                     (Crm.Shared.Upkeep.UpkeepGen''
                        (Crm.Shared.YearMonthDay.YearMonthDay)
                        (Bool)
                        (Data.Text.Text)
                        (Bool)
                        (Data.Text.Text)
                        (Data.Text.Text))
                     (Crm.Shared.MyMaybe.MyMaybe (Crm.Shared.Upkeep.UpkeepId' (Int)))
                     (Crm.Shared.Company.CompanyId' (Int))
                     (Crm.Shared.Company.Company' (Data.Text.Text) (Data.Text.Text)
                        (Data.Text.Text)
                        (Bool))
                     ([((,,,) (Crm.Shared.Machine.MachineId' (Int)) (Data.Text.Text)
                          (Data.Text.Text)
                          (Crm.Shared.MachineKind.MachineKindEnum))])
                     ([((,) (Crm.Shared.Employee.EmployeeId)
                          (Crm.Shared.Employee.Employee))]))])]
                 -> Fay ())
                -> Crm.Router.CrmRouter -> Fay ()
listPlanned ((pName, pValue) : px) callback router
  = Crm.Runtime.passwordAjax
      (pack $ "upkeeps" ++ "/" ++ "planned" ++
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

listPrint ::
          [(String, String)] ->
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
listPrint ((pName, pValue) : px) callback router
  = Crm.Runtime.passwordAjax
      (pack $ "upkeeps" ++ "/" ++ "print" ++
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

bySingle ::
         Crm.Shared.Upkeep.UpkeepId' (Int) ->
           ((,,) (Crm.Shared.Company.CompanyId' (Int))
              ((,,,)
                 (Crm.Shared.MyMaybe.MyMaybe (Crm.Shared.Upkeep.UpkeepId' (Int)))
                 (Crm.Shared.Upkeep.UpkeepGen''
                    (Crm.Shared.YearMonthDay.YearMonthDay)
                    (Bool)
                    (Data.Text.Text)
                    (Bool)
                    (Data.Text.Text)
                    (Data.Text.Text))
                 ([((,)
                      (Crm.Shared.UpkeepMachine.UpkeepMachineGen' (Data.Text.Text) (Int)
                         (Bool)
                         (Data.Text.Text)
                         (Crm.Shared.UpkeepMachine.UpkeepType))
                      (Crm.Shared.Machine.MachineId' (Int)))])
                 ([(Crm.Shared.Employee.EmployeeId)]))
              ([((,,,) (Crm.Shared.Machine.MachineId' (Int))
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
                   (Crm.Shared.UpkeepSequence.UpkeepSequence' (Int) (Data.Text.Text)
                      (Int)
                      (Bool)))])
              -> Fay ())
             -> Crm.Router.CrmRouter -> Fay ()
bySingle upkeepId callback router
  = Crm.Runtime.passwordAjax
      (pack $ "upkeeps" ++ "/" ++ "single" ++ "/" ++ getInt upkeepId)
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

saveBySingle ::
             Crm.Shared.Upkeep.UpkeepId' (Int) ->
               (,,)
                 (Crm.Shared.Upkeep.UpkeepGen''
                    (Crm.Shared.YearMonthDay.YearMonthDay)
                    (Bool)
                    (Data.Text.Text)
                    (Bool)
                    (Data.Text.Text)
                    (Data.Text.Text))
                 ([((,)
                      (Crm.Shared.UpkeepMachine.UpkeepMachineGen' (Data.Text.Text) (Int)
                         (Bool)
                         (Data.Text.Text)
                         (Crm.Shared.UpkeepMachine.UpkeepType))
                      (Crm.Shared.Machine.MachineId' (Int)))])
                 ([(Crm.Shared.Employee.EmployeeId)])
                 -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
saveBySingle upkeepId input callback router
  = Crm.Runtime.passwordAjax
      (pack $ "upkeeps" ++ "/" ++ "single" ++ "/" ++ getInt upkeepId)
      callback
      (Just input)
      Crm.Runtime.put
      Nothing
      Nothing
      router

create ::
       (,,,)
         (Crm.Shared.Upkeep.UpkeepGen''
            (Crm.Shared.YearMonthDay.YearMonthDay)
            (Bool)
            (Data.Text.Text)
            (Bool)
            (Data.Text.Text)
            (Data.Text.Text))
         ([((,)
              (Crm.Shared.UpkeepMachine.UpkeepMachineGen' (Data.Text.Text) (Int)
                 (Bool)
                 (Data.Text.Text)
                 (Crm.Shared.UpkeepMachine.UpkeepType))
              (Crm.Shared.Machine.MachineId' (Int)))])
         ([(Crm.Shared.Employee.EmployeeId)])
         (Crm.Shared.MyMaybe.MyMaybe (Crm.Shared.Upkeep.UpkeepId' (Int)))
         ->
         (Crm.Shared.Upkeep.UpkeepId' (Int) -> Fay ()) ->
           Crm.Router.CrmRouter -> Fay ()
create input callback router
  = Crm.Runtime.passwordAjax (pack $ "upkeeps") callback (Just input)
      Crm.Runtime.post
      Nothing
      Nothing
      router

remove ::
       Identifier -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
remove upkeeps callback router
  = Crm.Runtime.passwordAjax
      (pack $ "upkeeps" ++ "/" ++ getInt' upkeeps)
      callback
      Nothing
      Crm.Runtime.delete
      Nothing
      Nothing
      router

type Identifier = Crm.Shared.Upkeep.UpkeepId' (Int)

getInt' :: Crm.Shared.Upkeep.UpkeepId' (Int) -> String
getInt' (Crm.Shared.Upkeep.UpkeepId int) = "single/" ++ show int

getInt :: Crm.Shared.Upkeep.UpkeepId' (Int) -> String
getInt (Crm.Shared.Upkeep.UpkeepId int) = show int