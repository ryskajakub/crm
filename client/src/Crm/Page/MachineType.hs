{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Crm.Page.MachineType (
  machineTypesList ,
  machineTypePhase1Form ,
  machineTypeForm ) where

import           Data.Text                             (fromString, showInt, (<>), Text)
import           Prelude                               hiding (div, span, id)
import           Data.Var                              (Var, modify)
import           FFI                                   (Defined(Defined))
import           Data.Maybe                            (isJust, fromJust)
import           Data.LocalStorage

import           HaskellReact
import qualified HaskellReact.Bootstrap                as B
import qualified HaskellReact.Bootstrap.Button         as BTN
import qualified HaskellReact.Tag.Input                as II
import qualified HaskellReact.Bootstrap.Alert          as A
import qualified HaskellReact.Tag.Hyperlink            as AA
import qualified HaskellReact.Bootstrap.ButtonDropdown as BD

import qualified Crm.Shared.MachineType                as MT
import qualified Crm.Shared.UpkeepSequence             as US
import qualified Crm.Shared.Company                    as C
import qualified Crm.Shared.MachineKind                as MK

import qualified Crm.Router                            as R
import qualified Crm.Data.Data                         as D
import           Crm.Component.Form
import           Crm.Helpers
import           Crm.Server 
import           Crm.Component.Autocomplete            (autocompleteInput)


data MachineTypeForm = Phase1 | Edit
  deriving Eq


mkSetMachineType :: Var D.AppState -> MT.MachineType -> Fay ()
mkSetMachineType appVar modifiedMachineType = 
  D.modifyState appVar $
    \navig -> navig { D.machineTypeTuple = lmap (const modifiedMachineType) (D.machineTypeTuple navig) }

machineTypePhase1Form :: Maybe MT.MachineTypeId
                      -> (MT.MachineType, [(US.UpkeepSequence, Text)])
                      -> Var D.AppState
                      -> R.CrmRouter
                      -> C.CompanyId
                      -> (DOMElement, Fay ())
machineTypePhase1Form machineTypeId (machineType, upkeepSequences) appVar crmRouter companyId = let

  setMachineTypeId :: Maybe MT.MachineTypeId -> Fay ()
  setMachineTypeId machineTypeId' = do
    if hasLocalStorage 
      then case machineTypeId' of
        Just machineTypeId'' -> setLocalStorage "mt.id" $ showInt $ MT.getMachineTypeId machineTypeId''
        Nothing -> removeLocalStorage "mt.id"
      else return ()
    D.modifyState appVar (\navig -> navig { D.maybeMachineTypeId = machineTypeId' })

  storeMachineTypeIntoLocalStorage :: MT.MachineType -> Fay ()
  storeMachineTypeIntoLocalStorage machineType' = if hasLocalStorage
    then do
      let MT.MachineType kind name manufacturer = machineType'
      setLocalStorage "mt.name" name
      setLocalStorage "mt.kind" (showInt $ MK.kindToDbRepr kind)
      setLocalStorage "mt.manufacturer" manufacturer
    else return ()

  setMachineType machineType' = setMachineWhole (machineType', upkeepSequences)

  setMachineWhole :: (MT.MachineType, [(US.UpkeepSequence, Text)]) -> Fay ()
  setMachineWhole machineTypeTuple = do
    storeMachineTypeIntoLocalStorage (fst machineTypeTuple)
    D.modifyState appVar (\navig -> navig { D.machineTypeTuple = machineTypeTuple })

  displayManufacturer = let
    manufacturerField = input Editing False (SetValue $ MT.machineTypeManufacturer machineType)
      (const $ return ())
    in case machineTypeId of
      Nothing -> Nothing  
      _ -> Just manufacturerField
  
  (machineTypeInput, afterRenderCallback) =
    autocompleteInput 
      inputNormalAttrs
      (\text -> do
        setMachineType (machineType { MT.machineTypeName = text })
        setMachineTypeId Nothing)
      (\text -> if text /= "" 
        then fetchMachineType text (\maybeTuple -> case maybeTuple of
          Just (machineTypeId', machineType', sequences) -> do
            setMachineWhole (machineType', map (\x -> (x, showInt $ US.repetition x)) sequences)
            setMachineTypeId $ Just machineTypeId'
          Nothing -> return ())
        else return ())
      fetchMachineTypesAutocomplete
      "machine-type-autocomplete"
      (II.mkInputAttrs {
        II.defaultValue = Defined $ MT.machineTypeName machineType })

  storeUpkeepSequencesIntoLS :: [US.UpkeepSequence] -> Fay ()
  storeUpkeepSequencesIntoLS sequences = let
    seqsWithIndices = zipWithIndex sequences
    showBool :: Bool -> Text
    showBool b = if b then "True" else "False"
    storeUpkeepSequence (i, us) = do
      let index = showInt i
      setLocalStorage ("us." <> index <> ".displayOrdering") (showInt $ US.displayOrdering us)
      setLocalStorage ("us." <> index <> ".label") (US.label_ us)
      setLocalStorage ("us." <> index <> ".repetition") (showInt $ US.repetition us)
      setLocalStorage ("us." <> index <> ".oneTime") (showBool $ US.oneTime us)
    in do 
      forM_ seqsWithIndices storeUpkeepSequence
      setLocalStorage "us.length" (showInt $ length sequences)

  submitButtonHandler = do
    modify appVar (\appState -> appState {
      D.machineTypeFromPhase1 = (machineType, map fst upkeepSequences) ,
      D.maybeMachineIdFromPhase1 = machineTypeId })
    storeUpkeepSequencesIntoLS $ map fst upkeepSequences
    R.navigate (R.newMachinePhase2 companyId) crmRouter
  submitButtonLabel = text2DOM "Dále"

  (result, callback) = machineTypeForm' Phase1 displayManufacturer machineTypeId (machineType, 
    upkeepSequences) appVar setMachineType machineTypeInput submitButtonLabel submitButtonHandler
  in (result, callback >> afterRenderCallback)

machineTypeForm' :: MachineTypeForm
                 -> Maybe DOMElement -- ^ substitute the manufacturer autocomplete with another field
                 -> Maybe MT.MachineTypeId
                 -> (MT.MachineType, [(US.UpkeepSequence, Text)])
                 -> Var D.AppState
                 -> (MT.MachineType -> Fay ()) -- ^ set machine type
                 -> DOMElement -- ^ first row input field
                 -> DOMElement -- ^ submit button label
                 -> Fay () -- ^ submit button handler
                 -> (DOMElement, Fay ())
machineTypeForm' machineTypeFormType manufacturerAutocompleteSubstitution machineTypeId
    (machineType, upkeepSequences) appVar setMachineType typeInputField submitButtonLabel
    submitButtonHandler = let

  set1YearUpkeepSequences :: Fay ()
  set1YearUpkeepSequences = let
    us = US.newUpkeepSequence {
      US.oneTime = False ,
      US.repetition = 8760 } -- 1 year
    usTuple = (us, showInt $ US.repetition us)
    in D.modifyState appVar (\navig -> navig { D.machineTypeTuple = rmap (const [usTuple]) (D.machineTypeTuple navig) })
    
  modifyUpkeepSequence :: Int -> ((US.UpkeepSequence,Text) -> (US.UpkeepSequence,Text)) -> Fay ()
  modifyUpkeepSequence displayOrder modifier = let
    modifiedUpkeepSequences = map (\((us @ (US.UpkeepSequence displayOrder' _ _ _),repetitionText)) -> 
      if displayOrder == displayOrder' 
      then modifier (us, repetitionText)
      else (us, repetitionText)) upkeepSequences
    in D.modifyState appVar (\navig -> navig { D.machineTypeTuple = (machineType, modifiedUpkeepSequences)})

  upkeepSequenceRows = map (\((US.UpkeepSequence displayOrder sequenceLabel _ oneTime, rawTextRepetition)) -> let
    labelField = input
      Editing
      True
      (SetValue sequenceLabel)
      (\modifiedLabel -> modifyUpkeepSequence displayOrder
        (\us -> ((fst us) { US.label_ = modifiedLabel }, snd us)))
    mthField = input
      Editing
      True
      (SetValue rawTextRepetition)
      (\modifiedRepetition ->
        case parseSafely modifiedRepetition of
          Just (int) -> modifyUpkeepSequence displayOrder
            (\(us,_) -> (us {US.repetition = int },modifiedRepetition))
          Nothing -> modifyUpkeepSequence displayOrder
            (\(us,_) -> (us, modifiedRepetition)))
    firstServiceField = checkbox
      Editing
      oneTime
      (\oneTimeSequence -> modifyUpkeepSequence displayOrder (\us -> ((fst us) { US.oneTime = oneTimeSequence }, snd us)))
    inputColumns = [
      (label' (class'' ["control-label", "col-md-1"]) "Označení") ,
      (div' (class' "col-md-2") labelField) ,
      (label' (class'' ["control-label", "col-md-2"]) "Počet motohodin") ,
      (div' (class' "col-md-1") mthField) ,
      (label' (class'' ["control-label", "col-md-2"]) "První servis") ,
      (div' (class' "col-md-1") firstServiceField) ]
    removeButtonHandler = let
      modifiedUpkeepSequences = foldl (\upkeepSeqs ((us @ (US.UpkeepSequence displayOrder' _ _ _)),repetitionText) ->
        if displayOrder' == displayOrder 
        then upkeepSeqs
        else upkeepSeqs ++ [(us { US.displayOrdering = length upkeepSeqs + 1 }, repetitionText)]) [] upkeepSequences
      in D.modifyState appVar (\navig -> navig { D.machineTypeTuple = 
        rmap (const modifiedUpkeepSequences) (D.machineTypeTuple navig) })
    removeButtonProps = BTN.buttonProps {
      BTN.onClick = Defined $ const removeButtonHandler }
    removeButton = BTN.button' removeButtonProps "Odeber"
    in div' ((class' "form-group") {key = Defined $ "key-" <> showInt displayOrder}) ([
      div' (class'' ["col-md-1", "col-md-offset-1"]) removeButton ,
      label' (class'' ["control-label", "col-md-1"]) "Řada"] ++ inputColumns)) upkeepSequences

  (countOfOneTimeSequences, parseOk, repetitionValidationOk) = foldl
    (\(countOfOneTimeSequencesAcc, parseOkAcc, repetitionValuesOk) (us,repetitionText) -> let
      countOfOneTimeSequencesAccNew = if US.oneTime us then countOfOneTimeSequencesAcc + 1 else countOfOneTimeSequencesAcc
      (parseOkAccNew, repValuesOkNew) = case parseSafely repetitionText of
        Just int -> (parseOkAcc && True, int > 0 && repetitionValuesOk)
        Nothing -> (False, repetitionValuesOk)
      in (countOfOneTimeSequencesAccNew, parseOkAccNew, repValuesOkNew))
    (0 :: Int, True, True)
    upkeepSequences
  validationMessages1 = if machineTypeFormType == Phase1 && isJust machineTypeId
    then []
    else if null upkeepSequences 
      then ["Je třeba vytvořit alespoň jednu servisní řadu."]
      else if length upkeepSequences == countOfOneTimeSequences
        then ["Musí existovat alespoň jedna pravidelná servisní řada."]
        else []
  validationMessages2 = if countOfOneTimeSequences > 1
    then ["Může být pouze jeden úvodní servis."]
    else []
  validationMessages3 = if parseOk
    then []
    else ["Do políčka \"Počet motohodin\" se smí vyplňovat pouze čísla."]
  validationMessages4 = if repetitionValidationOk
    then []
    else ["Počet motohodin musí být kladné číslo."]
  validationMessages = validationMessages1 ++ validationMessages2 ++ validationMessages3 ++ validationMessages4

  phase1Advice = p $ text2DOM "Tady vybereš typ stroje, např. " : strong "BK 100" : text2DOM " pokud už tento typ existuje v systému, pak se výrobce (např." : strong "REMEZA" : text2DOM ") doplní sám, pokud ne, tak zadáš výrobce ručně. Potom jdeš dál, kde zadáš další informace." : []
  advices phase1 = div $ (if phase1 then phase1Advice else text2DOM "") : [
    h4 "Servisní řada" ,
    text2DOM "Servisní řada znamená, jak často - po kolika motohodinách se stroj opravuje. U každého typu stroje musí být alespoň jednou, jinak program neví, jak vypočítat datum, kdy se pojede na další servis. Příklad řad může být například: " ,
    ul [
      li "Generální oprava po 50000 mth" ,
      li "Střední oprava po 25000 mth" ,
      li "Běžná oprava po 5000 mth" ,
      li "Úvodní servis po 500 mth" ]]

  editInfo = pageInfo "Editace stroje" $ Just $ advices False
    
  phase1PageInfo = pageInfo "Nový stroj - fáze 1 - výběr typu stroje" $ Just $ advices True

  (autocompleteManufacturerField, autocompleteManufacturerCb) = case manufacturerAutocompleteSubstitution of
    Just substitution -> (substitution, return ())
    Nothing -> let 
      onChange text = setMachineType (machineType { MT.machineTypeManufacturer = text })
      in autocompleteInput
        inputNormalAttrs
        onChange
        onChange
        fetchMachineTypesManufacturer 
        "machine-type-manufacturer-autocomplete"
        (II.mkInputAttrs {
          II.defaultValue = Defined $ MT.machineTypeManufacturer machineType })

  kindSelect = let
    buttonLabel = [
      text2DOM $ fromJust $ lookup (MT.kind machineType) MK.machineKinds, 
      text2DOM " ", 
      span' (class' "caret") "" ]
    mkLink (kindId, kindLabel) = let
      selectAction = do
        setMachineType (machineType { MT.kind = kindId })
        case kindId of
          MK.RotaryScrewCompressor -> return ()
          _ -> set1YearUpkeepSequences
      in li $ AA.a''' (click selectAction) kindLabel
    selectElements = map mkLink MK.machineKinds
    in BD.buttonDropdown' (not $ isJust machineTypeId && machineTypeFormType == Phase1) buttonLabel selectElements

  fixedUpkeepSequences = case MT.kind machineType of
    MK.RotaryScrewCompressor | (isJust machineTypeId && machineTypeFormType == Phase1) -> True
    MK.RotaryScrewCompressor -> False
    _ -> True

  result =
    (B.grid $ B.row $
      case machineTypeFormType of
        Edit -> editInfo
        Phase1 -> phase1PageInfo) :
    (form' (class'' ["form-horizontal", "upkeep-sequence-form", "container"]) ([
      oneElementRow
        "Druh"
        kindSelect ,
      oneElementRow
        "Typ zařízení"
        typeInputField ,
      oneElementRow
        "Výrobce"
        autocompleteManufacturerField] ++ 
        (if fixedUpkeepSequences
          then []
          else upkeepSequenceRows) ++ [
            oneElementRow
              (let 
                addUpkeepSequenceRow = let
                  newUpkeepSequence = US.newUpkeepSequence {
                    US.repetition = 8760 ,
                    US.label_ = if (null upkeepSequenceRows) then "běžný" else "" ,
                    US.displayOrdering = length upkeepSequences + 1 }
                  newUpkeepSequences = upkeepSequences ++ 
                    [(newUpkeepSequence, showInt $ US.repetition newUpkeepSequence)]
                  in D.modifyState appVar (\navig -> 
                    navig { D.machineTypeTuple = (machineType, newUpkeepSequences)})
                disabledProps = if (fixedUpkeepSequences)
                  then BTN.buttonProps { BTN.disabled = Defined True }
                  else BTN.buttonProps 
                buttonProps = disabledProps {
                  BTN.onClick = Defined $ const addUpkeepSequenceRow }
                in BTN.button' buttonProps "Přidat servisní řadu")
               (text2DOM "") ,
            div' (class' "form-group") (buttonRow' (buttonStateFromBool . null $ validationMessages)
              submitButtonLabel submitButtonHandler)])) : (
    if null validationMessages
    then []
    else let
      validationMessagesHtml = map (\message -> p message) validationMessages
      in [B.grid $ B.row $ (B.col (B.mkColProps 12)) (A.alert A.Danger validationMessagesHtml)])
  in (div result, autocompleteManufacturerCb)

machineTypeForm :: R.CrmRouter
                -> Var D.AppState
                -> MT.MachineTypeId
                -> (MT.MachineType, [(US.UpkeepSequence, Text)])
                -> (DOMElement, Fay ())
machineTypeForm router appVar machineTypeId (machineType, upkeepSequences) = let
  setMachineType = mkSetMachineType appVar
  machineTypeInput = input
    Editing
    True
    (SetValue $ MT.machineTypeName machineType)
    (\str -> setMachineType (machineType { MT.machineTypeName = str }))
  submitButtonLabel = text2DOM "Uložit"
  submitButtonHandler = 
    updateMachineType (machineTypeId, machineType, map fst upkeepSequences) 
      (R.navigate R.machineTypesList router)
  in machineTypeForm' Edit Nothing (Just machineTypeId) (machineType, upkeepSequences) appVar 
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
  body = tbody $ map (\((machineTypeId,(MT.MachineType _ name manufacturer)), count) ->
    tr [
      td $ R.link name (R.machineTypeEdit machineTypeId) router ,
      td manufacturer , 
      td $ showInt count]) machineTypes
  alertInfo = text2DOM "Tady edituješ typ stroje - který je společný pro více strojů. Například, když je výrobce " : strong "REMEZA" : text2DOM " a typ " : strong "BK 150" : text2DOM " a ty při zadávání údajů uděláš chybu a napíšeš třeba " : strong "BK 150a" : text2DOM ", pak to tady můžeš opravit a ta oprava se projeví u všech strojů, ne jenom u tohoto jednoho. Potom v budoucnosti, pokud se budou evidovat díly u strojů a zařízení, tak se ty díly budou přidávat tady." : []
  in B.grid $ B.row $ 
    (pageInfo "Editace typů strojů" $ Just alertInfo) ++ [
      B.col (B.mkColProps 12) $ main $ section $
        B.table [ head', body ]]
