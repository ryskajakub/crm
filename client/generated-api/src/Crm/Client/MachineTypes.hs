{-# OPTIONS_GHC -fno-warn-unused-imports#-}
module Crm.Client.MachineTypes where
import Data.Text (pack)
import qualified Data.Text
import qualified Crm.Shared.MachineType
import qualified Crm.Shared.MachineKind
import qualified Crm.Runtime
import qualified Crm.Router
import qualified Crm.Shared.MyMaybe
import qualified Crm.Shared.UpkeepSequence

list ::
     [(String, String)] ->
       ([((,)
            ((,) (Crm.Shared.MachineType.MachineTypeId' (Int))
               (Crm.Shared.MachineType.MachineType'
                  (Crm.Shared.MachineKind.MachineKindEnum)
                  (Data.Text.Text)
                  (Data.Text.Text)))
            (Int))]
          -> Fay ())
         -> Crm.Router.CrmRouter -> Fay ()
list ((pName, pValue) : px) callback router
  = Crm.Runtime.passwordAjax
      (pack $ "machine-types" ++ "?" ++ pName ++ "=" ++ pValue ++
         concat
           (map (\ (pName', pValue') -> "&" ++ pName' ++ "=" ++ pValue') px))
      (callback . Crm.Runtime.items)
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

listByAutocompleteManufacturer ::
                               [(String, String)] ->
                                 String ->
                                   ([(Data.Text.Text)] -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
listByAutocompleteManufacturer ((pName, pValue) : px) string
  callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "machine-types" ++ "/" ++
           "autocomplete-manufacturer" ++ "/" ++ string
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

listByAutocomplete ::
                   [(String, String)] ->
                     String ->
                       ([(Data.Text.Text)] -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
listByAutocomplete ((pName, pValue) : px) string callback router
  = Crm.Runtime.passwordAjax
      (pack $ "machine-types" ++ "/" ++ "autocomplete" ++ "/" ++ string
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

byByName ::
         String ->
           (Crm.Shared.MyMaybe.MyMaybe
              ((,,) (Crm.Shared.MachineType.MachineTypeId' (Int))
                 (Crm.Shared.MachineType.MachineType'
                    (Crm.Shared.MachineKind.MachineKindEnum)
                    (Data.Text.Text)
                    (Data.Text.Text))
                 ([(Crm.Shared.UpkeepSequence.UpkeepSequence' (Int) (Data.Text.Text)
                      (Int)
                      (Bool))]))
              -> Fay ())
             -> Crm.Router.CrmRouter -> Fay ()
byByName string callback router
  = Crm.Runtime.passwordAjax
      (pack $ "machine-types" ++ "/" ++ "by-name" ++ "/" ++ string)
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

saveByByName ::
             String ->
               (,)
                 (Crm.Shared.MachineType.MachineType'
                    (Crm.Shared.MachineKind.MachineKindEnum)
                    (Data.Text.Text)
                    (Data.Text.Text))
                 ([(Crm.Shared.UpkeepSequence.UpkeepSequence' (Int) (Data.Text.Text)
                      (Int)
                      (Bool))])
                 -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
saveByByName string input callback router
  = Crm.Runtime.passwordAjax
      (pack $ "machine-types" ++ "/" ++ "by-name" ++ "/" ++ string)
      callback
      (Just input)
      Crm.Runtime.put
      Nothing
      Nothing
      router

byById ::
       Crm.Shared.MachineType.MachineTypeId' (Int) ->
         (Crm.Shared.MyMaybe.MyMaybe
            ((,,) (Crm.Shared.MachineType.MachineTypeId' (Int))
               (Crm.Shared.MachineType.MachineType'
                  (Crm.Shared.MachineKind.MachineKindEnum)
                  (Data.Text.Text)
                  (Data.Text.Text))
               ([(Crm.Shared.UpkeepSequence.UpkeepSequence' (Int) (Data.Text.Text)
                    (Int)
                    (Bool))]))
            -> Fay ())
           -> Crm.Router.CrmRouter -> Fay ()
byById machineTypeId callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "machine-types" ++ "/" ++ "by-id" ++ "/" ++ getInt machineTypeId)
      callback
      Nothing
      Crm.Runtime.get
      Nothing
      Nothing
      router

saveByById ::
           Crm.Shared.MachineType.MachineTypeId' (Int) ->
             (,)
               (Crm.Shared.MachineType.MachineType'
                  (Crm.Shared.MachineKind.MachineKindEnum)
                  (Data.Text.Text)
                  (Data.Text.Text))
               ([(Crm.Shared.UpkeepSequence.UpkeepSequence' (Int) (Data.Text.Text)
                    (Int)
                    (Bool))])
               -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
saveByById machineTypeId input callback router
  = Crm.Runtime.passwordAjax
      (pack $
         "machine-types" ++ "/" ++ "by-id" ++ "/" ++ getInt machineTypeId)
      callback
      (Just input)
      Crm.Runtime.put
      Nothing
      Nothing
      router

remove ::
       Identifier -> (() -> Fay ()) -> Crm.Router.CrmRouter -> Fay ()
remove machineTypes callback router
  = Crm.Runtime.passwordAjax
      (pack $ "machine-types" ++ "/" ++ getInt' machineTypes)
      callback
      Nothing
      Crm.Runtime.delete
      Nothing
      Nothing
      router

type Identifier = Crm.Shared.MachineType.MachineTypeId' (Int)

getInt' :: Crm.Shared.MachineType.MachineTypeId' (Int) -> String
getInt' (Crm.Shared.MachineType.MachineTypeId int)
  = "by-id/" ++ show int

getInt :: Crm.Shared.MachineType.MachineTypeId' (Int) -> String
getInt (Crm.Shared.MachineType.MachineTypeId int) = show int