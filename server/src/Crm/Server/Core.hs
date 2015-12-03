module Crm.Server.Core where

import           Data.List                 (partition, minimumBy, find, maximumBy)
import           Data.Maybe                (fromMaybe)

import           Data.Time.Calendar        (Day, addDays)
import           Safe.Foldable             (minimumMay)

import qualified Crm.Shared.Machine        as M
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.UpkeepMachine  as UM
import qualified Crm.Shared.Upkeep         as U

import           Crm.Server.Helpers        (ymdToDay)


-- | Signs, if the next service date is computed from the past upkeeps, or if the next planned service is taken.
data NextServiceDate a = 
  Planned a | -- ^ day taken from next planned service
  Computed a | -- ^ day computed from the past upkeeps and into operation
  Inactive -- ^ day not returned, machine is marked inactive
  deriving (Eq, Show)

instance Functor NextServiceDate where
  fmap f (Planned d) = Planned $ f d
  fmap f (Computed d) = Computed $ f d
  fmap _ Inactive = Inactive

getMaybe :: NextServiceDate a -> Maybe a
getMaybe (Planned a) = Just a
getMaybe (Computed a) = Just a
getMaybe Inactive = Nothing

nextServiceDate :: M.Machine -- ^ machine for which the next service date is computed
                -> (US.UpkeepSequence, [US.UpkeepSequence]) -- ^ upkeep sequences belonging to the machine - must be at least one element
                -> [U.Upkeep] -- ^ upkeeps belonging to this machine
                -> Day -- ^ today
                -> NextServiceDate Day -- ^ computed next service date for this machine
nextServiceDate machine sequences upkeeps today = let

  computeBasedOnPrevious :: Day -> [US.UpkeepSequence] -> Day
  computeBasedOnPrevious referenceDay filteredSequences = nextServiceDay where
    upkeepRepetition   = minimum $ fmap US.repetition filteredSequences
    mileagePerYear     = M.mileagePerYear machine
    yearsToNextService = (fromIntegral upkeepRepetition / fromIntegral mileagePerYear) :: Double
    daysToNextService  = truncate $ yearsToNextService * 365
    nextServiceDay     = addDays daysToNextService referenceDay

  computedUpkeep = case upkeeps of
    [] -> let 
      intoOperationDate = case M.machineOperationStartDate machine of 
        Just operationStartDate' -> ymdToDay operationStartDate'
        Nothing                  -> today
      filteredSequences = case oneTimeSequences of
        x : _ -> [x]
        []    -> nonEmptySequences
      in computeBasedOnPrevious intoOperationDate filteredSequences
    nonEmptyUpkeeps -> let
      lastServiceDate = ymdToDay $ maximum $ fmap U.upkeepDate nonEmptyUpkeeps
      in computeBasedOnPrevious lastServiceDate repeatedSequences
    where
    (oneTimeSequences, repeatedSequences) = partition (US.oneTime) nonEmptySequences
    nonEmptySequences = fst sequences : snd sequences

  openUpkeeps = filter (not . U.upkeepClosed) upkeeps
  in case openUpkeeps of
    (_:_) | 
      let nextOpenUpkeep' = fmap ymdToDay $ minimumMay $ fmap U.upkeepDate openUpkeeps, 
      Just nextOpenUpkeep <- nextOpenUpkeep' -> Planned nextOpenUpkeep
    _ -> Computed computedUpkeep 

compareRepetition :: US.UpkeepSequence -> US.UpkeepSequence -> Ordering
compareRepetition this that = US.repetition this `compare` US.repetition that

nextServiceTypeHint :: (US.UpkeepSequence, [US.UpkeepSequence])
                    -> [UM.UpkeepMachine]
                    -> US.UpkeepSequence
nextServiceTypeHint (seq', seqs) [] = fromMaybe
  (minimumBy compareRepetition (seq':seqs))
  (find (US.oneTime) (seq':seqs))
nextServiceTypeHint (seq', seqs) ums = let
  lastUpkeepMthSeq = maximumBy (\this that -> UM.recordedMileage this `compare` UM.recordedMileage that) ums
  repeatedSeqs = filter (not . US.oneTime) (seq':seqs)
  repeatedSeqsWithNextUpkeepMth = map (\repeatedSeq -> let
    numberOfPreviousUpkeeps = truncate $ 
      (fromIntegral . UM.recordedMileage $ lastUpkeepMthSeq :: Double) / 
      (fromIntegral . US.repetition $ repeatedSeq)
    numberOfNextUpkeep = numberOfPreviousUpkeeps + 1
    mthOfNextUpkeep = numberOfNextUpkeep * US.repetition repeatedSeq
    in (repeatedSeq, mthOfNextUpkeep)) repeatedSeqs
  minimumNextUpkeepMth = minimum $ map snd repeatedSeqsWithNextUpkeepMth
  findNextServiceSequence = maximumBy compareRepetition . map fst . filter (\(_, nextUpkeepMth) -> nextUpkeepMth == minimumNextUpkeepMth)
  in findNextServiceSequence repeatedSeqsWithNextUpkeepMth
