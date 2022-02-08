{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.Companies.ContactPersons where
import Data.Text (pack)
import qualified Crm.Client.Companies as Companies
import qualified Data.Text
import qualified Crm.Shared.ContactPerson
import qualified Crm.Runtime
import qualified Crm.Router

list ::
     [(String, String)] ->
       Companies.Identifier ->
         ([((,) (Crm.Shared.ContactPerson.ContactPersonId)
              (Crm.Shared.ContactPerson.ContactPerson))]
            -> Fay ())
           -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) companies callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "companies" ++ "/" ++
           Companies.getInt' companies ++ "/" ++ "contact-persons"
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
         Crm.Shared.ContactPerson.ContactPerson ->
           (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
create companies input callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "companies" ++ "/" ++
           Companies.getInt' companies ++ "/" ++ "contact-persons")
      callback
      (Just input)
      Crm.Runtime.post
      Nothing
      Nothing
      router