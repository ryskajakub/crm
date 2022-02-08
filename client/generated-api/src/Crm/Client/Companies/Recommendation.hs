{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Companies.Recommendation where
import Data.Text (pack)
import qualified Crm.Client.Companies as Companies
import qualified Data.Text
import qualified Crm.Shared.MyMaybe
import qualified Crm.Shared.Upkeep
import qualified Crm.Shared.YearMonthDay
import qualified Crm.Runtime
import qualified Crm.Router

access ::
       Companies.Identifier ->
         (Crm.Shared.MyMaybe.MyMaybe
            ((,) (Crm.Shared.Upkeep.UpkeepId' (Int))
               (Crm.Shared.Upkeep.UpkeepGen''
                  (Crm.Shared.YearMonthDay.YearMonthDay)
                  (Bool)
                  (Data.Text.Text)
                  (Bool)
                  (Data.Text.Text)
                  (Data.Text.Text)))
            -> Fay ())
           -> Crm.Router.CrmRouter -> Fay ()
access companies callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "companies" ++ "/" ++
           Companies.getInt' companies ++ "/" ++ "recommendation")
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router