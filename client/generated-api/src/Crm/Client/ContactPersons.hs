{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.ContactPersons where
import Data.Text (pack)
import qualified Data.Text
import qualified Crm.Shared.ContactPerson
import qualified Crm.Shared.Company
import qualified Crm.Runtime
import qualified Crm.Router

byContactPersonId ::
                  Crm.Shared.ContactPerson.ContactPersonId ->
                    ((,) (Crm.Shared.ContactPerson.ContactPerson)
                       (Crm.Shared.Company.CompanyId' (Int))
                       -> Fay ())
                      -> Crm.Router.CrmRouter -> Fay ()
byContactPersonId contactPersonId callback router
  = Crm.Runtime.passwordAjax
      (pack $ "contact-persons" ++ "/" ++ getInt contactPersonId)
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

saveByContactPersonId ::
                      Crm.Shared.ContactPerson.ContactPersonId ->
                        Crm.Shared.ContactPerson.ContactPerson ->
                          (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
saveByContactPersonId contactPersonId input callback router
  = Crm.Runtime.passwordAjax
      (pack $ "contact-persons" ++ "/" ++ getInt contactPersonId)
      callback
      (Just input)
      Crm.Runtime.put
      Nothing
      Nothing
      router

remove ::
       Identifier -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
remove contactPersons callback router
  = Crm.Runtime.passwordAjax
      (pack $ "contact-persons" ++ "/" ++ getInt' contactPersons)
      callback
      Nothing
      Crm.Runtime.delete
      Nothing
      Nothing
      router

type Identifier = Crm.Shared.ContactPerson.ContactPersonId

getInt' :: Crm.Shared.ContactPerson.ContactPersonId -> String
getInt' (Crm.Shared.ContactPerson.ContactPersonId int)
  = "/" ++ show int

getInt :: Crm.Shared.ContactPerson.ContactPersonId -> String
getInt (Crm.Shared.ContactPerson.ContactPersonId int) = show int