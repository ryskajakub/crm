{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Crm.Page.Other.MachineType (
  machineTypesList ,
  machineTypePhase1Form ,
  machineTypeForm ) where

import "fay-base" Data.Text (fromString, unpack, pack, showInt, (<>))
import "fay-base" Prelude hiding (div, span, id)
import "fay-base" Data.Var (Var, modify)
import "fay-base" FFI (Defined(Defined))
import "fay-base" Data.Maybe (isNothing, isJust)

import HaskellReact
import qualified HaskellReact.Bootstrap as B
import qualified HaskellReact.Bootstrap.Button as BTN
import qualified HaskellReact.Tag.Input as II

import qualified Crm.Shared.MachineType as MT
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.Company as C

import qualified Crm.Router as R
import qualified Crm.Data as D
import Crm.Helpers (formRow', parseSafely, saveButtonRow, saveButtonRow' , lmap, editingInput, eventInt, formRow, inputNormalAttrs, rmap)
import Crm.Server (updateMachineType, fetchMachineType, fetchMachineTypesAutocomplete)
import Crm.Component.Autocomplete (autocompleteInput)

mkSetMachineType :: Var D.AppState -> MT.MachineType -> Fay ()
mkSetMachineType appVar modifiedMachineType = 
  D.modifyState appVar (\navig -> navig { D.machineTypeTuple = lmap (const modifiedMachineType) (D.machineTypeTuple navig) })

machineTypePhase1Form :: Maybe MT.MachineTypeId
                      -> (MT.MachineType, [US.UpkeepSequence])
                      -> Var D.AppState
                      -> R.CrmRouter
                      -> C.CompanyId
                      -> (DOMElement, Fay ())
machineTypePhase1Form machineTypeId (machineType, upkeepSequences) appVar crmRouter companyId = let
  setMachineTypeId :: Maybe MT.MachineTypeId -> Fay ()
  setMachineTypeId machineTypeId' = 
    D.modifyState appVar (\navig -> navig { D.maybeMachineTypeId = machineTypeId' })
  setMachineType = mkSetMachineType appVar
  (machineTypeInput, afterRenderCallback) =
    autocompleteInput 
      inputNormalAttrs
      (\text -> do
        setMachineType (machineType { MT.machineTypeName = unpack text })
        setMachineTypeId Nothing)
      (\text -> if text /= "" 
        then fetchMachineType text (\maybeTuple -> case maybeTuple of
          Just (machineTypeId', machineType', upkeepSequences) -> do
            setMachineType machineType'
            setMachineTypeId $ Just machineTypeId'
          Nothing -> return () )
        else return () )
      fetchMachineTypesAutocomplete
      "machine-type-autocomplete"
      (II.mkInputAttrs {
        II.defaultValue = Defined $ pack $ MT.machineTypeName machineType })
  submitButtonHandler = do
    modify appVar (\appState -> appState {
      D.machineTypeFromPhase1 = (machineType, upkeepSequences) ,
      D.maybeMachineIdFromPhase1 = machineTypeId })
    R.navigate (R.newMachinePhase2 companyId) crmRouter
  submitButtonLabel = text2DOM "Dále"
  in (machineTypeForm' machineTypeId (machineType, upkeepSequences) appVar setMachineType machineTypeInput
    submitButtonLabel submitButtonHandler, afterRenderCallback)

machineTypeForm' :: Maybe MT.MachineTypeId
                 -> (MT.MachineType, [US.UpkeepSequence])
                 -> Var D.AppState
                 -> (MT.MachineType -> Fay ()) -- ^ set machine type
                 -> DOMElement -- ^ first row input field
                 -> DOMElement -- ^ submit button label
                 -> Fay () -- ^ submit button handler
                 -> DOMElement
machineTypeForm' machineTypeId (machineType, upkeepSequences) appVar
    setMachineType typeInputField submitButtonLabel submitButtonHandler = let
    
  modifyUpkeepSequence :: Int -> (US.UpkeepSequence -> US.UpkeepSequence) -> Fay ()
  modifyUpkeepSequence displayOrder modifier = let
    modifiedUpkeepSequences = map (\(us @ (US.UpkeepSequence displayOrder' _ _)) -> 
      if displayOrder == displayOrder' 
      then modifier us
      else us ) upkeepSequences
    in D.modifyState appVar (\navig -> navig { D.machineTypeTuple = (machineType, modifiedUpkeepSequences)})

  upkeepSequenceRows = map (\(US.UpkeepSequence displayOrder label repetition) -> let
    labelField = editingInput label (eventString >=> (\modifiedLabel -> modifyUpkeepSequence displayOrder
      (\us -> us { US.label_ = modifiedLabel }))) True
    mthField = editingInput (show repetition) (eventInt (\modifiedRepetition -> modifyUpkeepSequence displayOrder
      (\us -> us { US.repetition = modifiedRepetition }))) True
    inputColumn = div [text2DOM "Označení: ", labelField, text2DOM " Počet motohodin: ", mthField]
    removeButtonHandler = let
      modifiedUpkeepSequences = foldl (\upkeepSeqs (us @ (US.UpkeepSequence displayOrder' _ _)) ->
        if displayOrder' == displayOrder 
        then upkeepSeqs
        else upkeepSeqs ++ [us { US.displayOrdering = length upkeepSeqs + 1 }]) [] upkeepSequences
      in D.modifyState appVar (\navig -> navig { D.machineTypeTuple = 
        rmap (const modifiedUpkeepSequences) (D.machineTypeTuple navig) })
    removeButtonProps = BTN.buttonProps {
      BTN.onClick = Defined $ const removeButtonHandler }
    removeButton = BTN.button' removeButtonProps "Odeber"
    in formRow [removeButton, text2DOM $ "Řada " <> showInt displayOrder] inputColumn) upkeepSequences

  result = form' (mkAttrs { className = Defined "form-horizontal" }) $
    B.grid $
      B.row $ [
        formRow
          "Typ zařízení"
          typeInputField ,
        formRow'
          "Výrobce"
          (MT.machineTypeManufacturer machineType)
          (eventString >=> (\string -> setMachineType (machineType { MT.machineTypeManufacturer = string })))
          (isNothing machineTypeId) ] ++ upkeepSequenceRows ++ [
        formRow
          (let 
            addUpkeepSequenceRow = let
              newUpkeepSequence = US.newUpkeepSequence {
                US.displayOrdering = length upkeepSequences + 1 }
              newUpkeepSequences = upkeepSequences ++ [newUpkeepSequence]
              in D.modifyState appVar (\navig -> navig { D.machineTypeTuple = (machineType, newUpkeepSequences) })
            buttonProps = BTN.buttonProps {
              BTN.onClick = Defined $ const addUpkeepSequenceRow }
            in BTN.button' buttonProps "Přidat servisní řadu")
           (text2DOM "") ,
        let buttonEnabled = (not $ null upkeepSequences) || isJust machineTypeId
        in saveButtonRow' buttonEnabled submitButtonLabel submitButtonHandler ]
  in result

machineTypeForm :: Var D.AppState
                -> MT.MachineTypeId
                -> (MT.MachineType, [US.UpkeepSequence])
                -> DOMElement
machineTypeForm appVar machineTypeId (machineType, upkeepSequences) = let
  setMachineType = mkSetMachineType appVar
  machineTypeInput = editingInput
    (MT.machineTypeName machineType)
    (eventString >=> (\str -> setMachineType (machineType { MT.machineTypeName = str })))
    True
  submitButtonLabel = text2DOM "Uložit"
  submitButtonHandler = updateMachineType (machineTypeId, machineType, upkeepSequences) (return ())
  in machineTypeForm' Nothing (machineType, upkeepSequences) appVar 
    setMachineType machineTypeInput submitButtonLabel submitButtonHandler

machineTypesList :: R.CrmRouter
                 -> [(MT.MachineType', Int)]
                 -> DOMElement
machineTypesList router machineTypes = let
  head' =
    thead $ tr [
      th "Název typu" , 
      th "Výrobce" , 
      th "Počet zařízení v systému" ]
  body = tbody $ map (\((machineTypeId,(MT.MachineType name manufacturer)), count) ->
    tr [
      td $ R.link (pack name) (R.machineTypeEdit machineTypeId) router ,
      td $ pack manufacturer , 
      td $ showInt count ]) machineTypes
  in main [
    section $
      B.table [
        head' , 
        body ] ]
