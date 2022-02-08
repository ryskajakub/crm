{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Companies.Machines where
import Data.Text (pack)
import qualified Crm.Client.Companies as Companies
import qualified Data.Text
import qualified Crm.Shared.Machine
import qualified Crm.Shared.YearMonthDay
import qualified Crm.Runtime
import qualified Crm.Router
import qualified Crm.Shared.MachineType
import qualified Crm.Shared.MyMaybe
import qualified Crm.Shared.ExtraField

list ::
     [(String, String)] ->
       Companies.Identifier ->
         ([((,) (Crm.Shared.Machine.MachineId' (Int))
              (Crm.Shared.Machine.Machine'
                 (Maybe (Crm.Shared.YearMonthDay.YearMonthDay))
                 (Int)
                 (Int)
                 (Data.Text.Text)
                 (Data.Text.Text)
                 (Data.Text.Text)
                 (Bool)
                 (Data.Text.Text)
                 (Crm.Shared.Machine.UpkeepBy)))]
            -> Fay ())
           -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) companies callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "companies" ++ "/" ++
           Companies.getInt' companies ++ "/" ++ "machines"
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

create ::
       Companies.Identifier ->
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
           (Crm.Shared.MachineType.MyEither)
           (Crm.Shared.MyMaybe.MyMaybe
              (Crm.Shared.Machine.ContactPersonForMachine))
           (Crm.Shared.MyMaybe.MyMaybe (Crm.Shared.Machine.MachineId' (Int)))
           ([((,) (Crm.Shared.ExtraField.ExtraFieldId) (Data.Text.Text))])
           ->
           (Crm.Shared.Machine.MachineId' (Int) -> Fay ()) ->
             Crm.Router.CrmRouter -> Fay ()
create companies input callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "companies" ++ "/" ++
           Companies.getInt' companies ++ "/" ++ "machines")
      callback
      (Just input)
      Crm.Runtime.post
      Nothing
      Nothing
      router
