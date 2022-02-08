{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Companies where
import Data.Text (pack)
import qualified Data.Text
import qualified Crm.Shared.Company
import qualified Crm.Shared.MachineKind
import qualified Crm.Runtime
import qualified Crm.Router
import qualified Crm.Shared.ContactPerson
import qualified Crm.Shared.Machine
import qualified Crm.Shared.YearMonthDay
import qualified Crm.Shared.MachineType
import qualified Crm.Shared.MyMaybe
import qualified Crm.Shared.Upkeep

list ::
     [(String, String)] ->
       ([((,,,) (Crm.Shared.Company.CompanyId' (Int))
            (Crm.Shared.Company.Company' (Data.Text.Text) (Data.Text.Text)
               (Data.Text.Text)
               (Bool))
            (Crm.Shared.Company.CompanyState)
            ([(Crm.Shared.MachineKind.MachineKindEnum)]))]
          -> Fay ())
         -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) callback router
  = Crm.Runtime.passwordAjax
      (pack $ "companies" ++ "?" ++ pName ++ "=" ++ pValue ++
         concat
           (map (\ (pName', pValue') -> "&" ++ pName' ++ "=" ++ pValue') px))
      (callback . Crm.Runtime.items)
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

bySingle ::
         Crm.Shared.Company.CompanyId' (Int) ->
           ((,,)
              (Crm.Shared.Company.Company' (Data.Text.Text) (Data.Text.Text)
                 (Data.Text.Text)
                 (Bool))
              ([((,) (Crm.Shared.ContactPerson.ContactPersonId)
                   (Crm.Shared.ContactPerson.ContactPerson))])
              ([((,)
                   ((,,,,,,,) (Crm.Shared.Machine.MachineId' (Int))
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
                         (Data.Text.Text))
                      (Crm.Shared.MyMaybe.MyMaybe
                         (Crm.Shared.ContactPerson.ContactPerson))
                      (Crm.Shared.Machine.MachineId' (Maybe (Int)))
                      (Crm.Shared.MyMaybe.MyMaybe
                         (Crm.Shared.Upkeep.UpkeepGen''
                            (Crm.Shared.YearMonthDay.YearMonthDay)
                            (Bool)
                            (Data.Text.Text)
                            (Bool)
                            (Data.Text.Text)
                            (Data.Text.Text))))
                   (Crm.Shared.MyMaybe.MyMaybe
                      (Crm.Shared.YearMonthDay.YearMonthDay)))])
              -> Fay ())
             -> Crm.Router.CrmRouter -> Fay ()
bySingle companyId callback router
  = Crm.Runtime.passwordAjax
      (pack $ "companies" ++ "/" ++ "single" ++ "/" ++ getInt companyId)
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

saveBySingle ::
             Crm.Shared.Company.CompanyId' (Int) ->
               (,)
                 (Crm.Shared.Company.Company' (Data.Text.Text) (Data.Text.Text)
                    (Data.Text.Text)
                    (Bool))
                 (Crm.Shared.MyMaybe.MyMaybe
                    (Crm.Shared.Company.Coordinates' (Double) (Double)))
                 -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
saveBySingle companyId input callback router
  = Crm.Runtime.passwordAjax
      (pack $ "companies" ++ "/" ++ "single" ++ "/" ++ getInt companyId)
      callback
      (Just input)
      Crm.Runtime.put
      Nothing
      Nothing
      router

listMap ::
        [(String, String)] ->
          ([((,,,,) (Crm.Shared.Company.CompanyId' (Int))
               (Crm.Shared.Company.Company' (Data.Text.Text) (Data.Text.Text)
                  (Data.Text.Text)
                  (Bool))
               (Crm.Shared.Company.CompanyState)
               (Crm.Shared.MyMaybe.MyMaybe
                  (Crm.Shared.Company.Coordinates' (Double) (Double)))
               ([(Crm.Shared.MachineKind.MachineKindEnum)]))]
             -> Fay ())
            -> Crm.Router.CrmRouter -> Fay ()
listMap ((pName, pValue) : px) callback router
  = Crm.Runtime.passwordAjax
      (pack $ "companies" ++ "/" ++ "map" ++
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
       (,)
         (Crm.Shared.Company.Company' (Data.Text.Text) (Data.Text.Text)
            (Data.Text.Text)
            (Bool))
         (Crm.Shared.MyMaybe.MyMaybe
            (Crm.Shared.Company.Coordinates' (Double) (Double)))
         ->
         (Crm.Shared.Company.CompanyId' (Int) -> Fay ()) ->
           Crm.Router.CrmRouter -> Fay ()
create input callback router
  = Crm.Runtime.passwordAjax (pack $ "companies") callback
      (Just input)
      Crm.Runtime.post
      Nothing
      Nothing
      router

remove ::
       Identifier -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
remove companies callback router
  = Crm.Runtime.passwordAjax
      (pack $ "companies" ++ "/" ++ getInt' companies)
      callback
      Nothing
      Crm.Runtime.delete
      Nothing
      Nothing
      router

type Identifier = Crm.Shared.Company.CompanyId' (Int)

getInt' :: Crm.Shared.Company.CompanyId' (Int) -> String
getInt' (Crm.Shared.Company.CompanyId int) = "single/" ++ show int

getInt :: Crm.Shared.Company.CompanyId' (Int) -> String
getInt (Crm.Shared.Company.CompanyId int) = show int