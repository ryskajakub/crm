{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Machines where
import Data.Text (pack)
import qualified Data.Text
import qualified Crm.Shared.Machine
import qualified Crm.Shared.YearMonthDay
import qualified Crm.Shared.Company
import qualified Crm.Shared.MachineType
import qualified Crm.Shared.MachineKind
import qualified Crm.Runtime
import qualified Crm.Router
import qualified Crm.Shared.UpkeepSequence
import qualified Crm.Shared.MyMaybe
import qualified Crm.Shared.ContactPerson
import qualified Crm.Shared.Upkeep
import qualified Crm.Shared.UpkeepMachine
import qualified Crm.Shared.Employee
import qualified Crm.Shared.Photo
import qualified Crm.Shared.ExtraField

list ::
     [(String, String)] ->
       ([((,,,,) (Crm.Shared.Machine.MachineId' (Int))
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
            (Crm.Shared.Company.CompanyId' (Int))
            (Crm.Shared.MachineType.MachineTypeId' (Int))
            (Crm.Shared.MachineType.MachineType'
               (Crm.Shared.MachineKind.MachineKindEnum)
               (Data.Text.Text)
               (Data.Text.Text)))]
          -> Fay ())
         -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) callback router
  = Crm.Runtime.passwordAjax
      (pack $ "machines" ++ "?" ++ pName ++ "=" ++ pValue ++
         concat
           (map (\ (pName', pValue') -> "&" ++ pName' ++ "=" ++ pValue') px))
      (callback . Crm.Runtime.items)
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

byMachineId ::
            Crm.Shared.Machine.MachineId' (Int) ->
              ((,)
                 ((,,,) (Crm.Shared.Company.CompanyId' (Int))
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
                    (Crm.Shared.MachineType.MachineTypeId' (Int))
                    ((,)
                       (Crm.Shared.MachineType.MachineType'
                          (Crm.Shared.MachineKind.MachineKindEnum)
                          (Data.Text.Text)
                          (Data.Text.Text))
                       ([(Crm.Shared.UpkeepSequence.UpkeepSequence' (Int) (Data.Text.Text)
                            (Int)
                            (Bool))])))
                 ((,,,,,)
                    (Crm.Shared.MyMaybe.MyMaybe (Crm.Shared.YearMonthDay.YearMonthDay))
                    (Crm.Shared.MyMaybe.MyMaybe
                       (Crm.Shared.ContactPerson.ContactPersonId))
                    ([((,,,,,) (Crm.Shared.Upkeep.UpkeepId' (Int))
                         (Crm.Shared.MyMaybe.MyMaybe (Crm.Shared.Upkeep.UpkeepId' (Int)))
                         (Crm.Shared.Upkeep.UpkeepGen''
                            (Crm.Shared.YearMonthDay.YearMonthDay)
                            (Bool)
                            (Data.Text.Text)
                            (Bool)
                            (Data.Text.Text)
                            (Data.Text.Text))
                         (Crm.Shared.UpkeepMachine.UpkeepMachineGen' (Data.Text.Text) (Int)
                            (Bool)
                            (Data.Text.Text)
                            (Crm.Shared.UpkeepMachine.UpkeepType))
                         ([(Crm.Shared.Employee.Employee)])
                         ([(Crm.Shared.Photo.PhotoId)]))])
                    (Crm.Shared.Machine.MachineId' (Maybe (Int)))
                    (Crm.Shared.MachineKind.MachineKindEnum)
                    ([((,,) (Crm.Shared.ExtraField.ExtraFieldId)
                         (Crm.Shared.MachineKind.MachineKindSpecific)
                         (Data.Text.Text))]))
                 -> Fay ())
                -> Crm.Router.CrmRouter -> Fay ()
byMachineId machineId callback router
  = Crm.Runtime.passwordAjax
      (pack $ "machines" ++ "/" ++ getInt machineId)
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

saveByMachineId ::
                Crm.Shared.Machine.MachineId' (Int) ->
                  (,,,,)
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
                    (Crm.Shared.MachineType.MachineTypeId' (Int))
                    (Crm.Shared.MyMaybe.MyMaybe (Crm.Shared.Machine.MachineId' (Int)))
                    (Crm.Shared.MyMaybe.MyMaybe
                       (Crm.Shared.ContactPerson.ContactPersonId))
                    ([((,) (Crm.Shared.ExtraField.ExtraFieldId) (Data.Text.Text))])
                    -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
saveByMachineId machineId input callback router
  = Crm.Runtime.passwordAjax
      (pack $ "machines" ++ "/" ++ getInt machineId)
      callback
      (Just input)
      Crm.Runtime.put
      Nothing
      Nothing
      router

remove ::
       Identifier -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
remove machines callback router
  = Crm.Runtime.passwordAjax
      (pack $ "machines" ++ "/" ++ getInt' machines)
      callback
      Nothing
      Crm.Runtime.delete
      Nothing
      Nothing
      router

type Identifier = Crm.Shared.Machine.MachineId' (Int)

getInt' :: Crm.Shared.Machine.MachineId' (Int) -> String
getInt' (Crm.Shared.Machine.MachineId int) = "/" ++ show int

getInt :: Crm.Shared.Machine.MachineId' (Int) -> String
getInt (Crm.Shared.Machine.MachineId int) = show int