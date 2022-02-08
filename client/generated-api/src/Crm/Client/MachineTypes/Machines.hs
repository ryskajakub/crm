{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.MachineTypes.Machines where
import Data.Text (pack)
import qualified Crm.Client.MachineTypes as MachineTypes
import qualified Data.Text
import qualified Crm.Shared.Machine
import qualified Crm.Shared.YearMonthDay
import qualified Crm.Shared.Company
import qualified Crm.Runtime
import qualified Crm.Router

list ::
     [(String, String)] ->
       MachineTypes.Identifier ->
         ([((,)
              ((,) (Crm.Shared.Machine.MachineId' (Int))
                 (Crm.Shared.Machine.Machine'
                    (Maybe (Crm.Shared.YearMonthDay.YearMonthDay))
                    (Int)
                    (Int)
                    (Data.Text.Text)
                    (Data.Text.Text)
                    (Data.Text.Text)
                    (Bool)
                    (Data.Text.Text)
                    (Crm.Shared.Machine.UpkeepBy)))
              ((,) (Crm.Shared.Company.CompanyId' (Int))
                 (Crm.Shared.Company.Company' (Data.Text.Text) (Data.Text.Text)
                    (Data.Text.Text)
                    (Bool))))]
            -> Fay ())
           -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) machineTypes callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "machine-types" ++ "/" ++
           MachineTypes.getInt' machineTypes ++ "/" ++ "machines"
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