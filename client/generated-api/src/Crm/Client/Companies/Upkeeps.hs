{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Companies.Upkeeps where
import Data.Text (pack)
import qualified Crm.Client.Companies as Companies
import qualified Data.Text
import qualified Crm.Shared.Upkeep
import qualified Crm.Shared.YearMonthDay
import qualified Crm.Shared.ServerRender
import qualified Crm.Shared.UpkeepMachine
import qualified Crm.Shared.Machine
import qualified Crm.Shared.MachineType
import qualified Crm.Shared.MachineKind
import qualified Crm.Shared.Employee
import qualified Crm.Shared.Photo
import qualified Crm.Runtime
import qualified Crm.Router
import qualified Crm.Shared.UpkeepSequence

list ::
     [(String, String)] ->
       Companies.Identifier ->
         ([((,,,,) (Crm.Shared.Upkeep.UpkeepId' (Int))
              (Crm.Shared.Upkeep.UpkeepGen''
                 (Crm.Shared.YearMonthDay.YearMonthDay)
                 (Bool)
                 (Data.Text.Text)
                 (Bool)
                 ([(Crm.Shared.ServerRender.Markup)])
                 ([(Crm.Shared.ServerRender.Markup)]))
              ([((,,,)
                   (Crm.Shared.UpkeepMachine.UpkeepMachineGen' (Data.Text.Text) (Int)
                      (Bool)
                      (Data.Text.Text)
                      (Crm.Shared.UpkeepMachine.UpkeepType))
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
                   (Crm.Shared.Machine.MachineId' (Int)))])
              ([((,) (Crm.Shared.Employee.EmployeeId)
                   (Crm.Shared.Employee.Employee))])
              ([(Crm.Shared.Photo.PhotoId)]))]
            -> Fay ())
           -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) companies callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "companies" ++ "/" ++
           Companies.getInt' companies ++ "/" ++ "upkeeps"
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

bySingle ::
         Companies.Identifier ->
           String ->
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
                     (Bool)))]
                -> Fay ())
               -> Crm.Router.CrmRouter -> Fay ()
bySingle companies string callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "companies" ++ "/" ++
           Companies.getInt' companies ++ "/" ++
             "upkeeps" ++ "/" ++ "single" ++ "/" ++ string)
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router