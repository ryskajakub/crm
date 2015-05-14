module Crm.Server.Core where

import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.Upkeep as U

import Crm.Server.Helpers (ymdToDay)

import Data.Time.Calendar (Day, addDays)

import Safe.Foldable (minimumMay)

-- | Signs, if the next service date is computed from the past upkeeps, or if the next planned service is taken.
data Planned = 
  Planned | -- ^ day taken from next planned service
  Computed -- ^ day computed from the past upkeeps and into operation
  deriving (Eq, Show)

nextServiceDate :: M.Machine -- ^ machine for which the next service date is computed
                -> (US.UpkeepSequence, [US.UpkeepSequence]) -- ^ upkeep sequences belonging to the machine - must be at least one element
                -> [U.Upkeep] -- ^ upkeeps belonging to this machine
                -> Day -- ^ today
                -> (Day, Planned) -- ^ computed next service date for this machine
nextServiceDate machine sequences upkeeps today = let

  computeBasedOnPrevious :: Day -> [US.UpkeepSequence] -> Day
  computeBasedOnPrevious referenceDay filteredSequences = let
    upkeepRepetition = minimum $ fmap US.repetition filteredSequences
    mileagePerYear = M.mileagePerYear machine
    yearsToNextService = (fromIntegral upkeepRepetition / fromIntegral mileagePerYear) :: Double
    daysToNextService = truncate $ yearsToNextService * 365
    nextServiceDay = addDays daysToNextService referenceDay
    in nextServiceDay
    
  nonEmptySequences = fst sequences : snd sequences

  computeFromSequence = case upkeeps of
    [] -> let 
      operationStartDate = case M.machineOperationStartDate machine of 
        Just operationStartDate' -> ymdToDay operationStartDate'
        Nothing -> today
      filteredSequences = case filter US.oneTime nonEmptySequences of
        x : _ -> [x]
        [] -> nonEmptySequences
      in computeBasedOnPrevious operationStartDate filteredSequences
    xs -> let
      lastServiceDate = ymdToDay $ maximum $ fmap (U.upkeepDate) xs
      repeatedSequences = filter (not . US.oneTime) nonEmptySequences
      in computeBasedOnPrevious lastServiceDate repeatedSequences

  earliestPlannedUpkeep = case filter (not . U.upkeepClosed) upkeeps of
    [] -> Nothing
    openUpkeeps -> fmap ymdToDay $ minimumMay $ fmap U.upkeepDate openUpkeeps
  in case earliestPlannedUpkeep of
    Just plannedUpkeepDay -> (plannedUpkeepDay, Planned)
    Nothing -> (computeFromSequence, Computed)
