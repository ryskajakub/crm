{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Main where

import           Control.Monad.Error.Class (Error)
import           Control.Monad             (forM_)

import           Data.Time.Calendar        (Day, fromGregorian)
import           Data.Time.Format          (readTime)
import           Data.Time.Clock           (utctDay, UTCTime)
import           Data.Bits                 (shiftL)
import           Data.Word                 (Word32)
import           Data.List.Unique          (repeated)
import           Data.Text                 (pack)

import           System.Locale             (defaultTimeLocale)
import           System.Random             (next, mkStdGen, StdGen)
import           System.Random.Shuffle     (shuffle')

import           Test.Tasty.HUnit
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Random

import           Crm.Server.Core           (nextServiceDate, Planned(..), nextServiceTypeHint)
import           Crm.Server.Helpers

import qualified Crm.Shared.Upkeep         as U
import qualified Crm.Shared.Machine        as M
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.UpkeepMachine  as UM

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [
  nextServiceDayTests ,
  hintNextServiceTypeTests ]

hintNextServiceTypeTests :: TestTree
hintNextServiceTypeTests = testGroup "Next service type: All tests" [unitTests', propertyTests']

nextServiceDayTests :: TestTree
nextServiceDayTests = testGroup "Next service day : All tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Next service day : Unit tests" [
  testCase "When there are no upkeeps, then the day is computed from day into operation" noUpkeeps ,
  testCase "When there are only past upkeeps, then the day is computed from the last one" closedUpkeeps ,
  testCase "When there are upkeep sequences and a past upkeep, then the smallest repeated in taken" pickSmallestRepeatedSequence ,
  testCase "When there is a non-repeat upkeep sequence and no past upkeeps, then it is taken" firstUpkeep ,
  testCase "When the operation start date is not specified in machine, today is taken" missingOperationStartDate ]

unitTests' :: TestTree
unitTests' = testGroup "Next service type hint : Unit tests" [
  testCase "When there time for big maintanance, the big upkeep sequence is picked." bigUpkeepAssertion ,
  testCase "When there time for small maintanance, the small upkeep sequence is picked." smallUpkeepAssertion ]

machine :: M.Machine
machine = M.Machine {
  M.machineOperationStartDate = Just $ dayToYmd $ fromGregorian 2000 1 1 ,
  M.mileagePerYear = 5000 }

upkeepSequence :: US.UpkeepSequence
upkeepSequence = US.UpkeepSequence {
  US.oneTime = False ,
  US.repetition = 10000 }

upkeepDate :: Day
upkeepDate = fromGregorian 2000 1 1

upkeep :: U.Upkeep
upkeep = U.Upkeep {
  U.upkeepDate = dayToYmd upkeepDate ,
  U.upkeepClosed = True }

smallUpkeepAssertion :: Assertion
smallUpkeepAssertion = let
  previousUpkeep = UM.newUpkeepMachine { UM.recordedMileage = 57000 }
  smallUpkeep = upkeepSequence { US.repetition = 2000 }
  bigUpkeep = upkeepSequence { US.repetition = 6000 }
  result = nextServiceTypeHint (smallUpkeep, [bigUpkeep]) [previousUpkeep]
  expectedResult = smallUpkeep
  in assertEqual "The 2000 sequence must be picked"
    (US.repetition expectedResult) (US.repetition result)

bigUpkeepAssertion :: Assertion
bigUpkeepAssertion = let
  previousUpkeep = UM.newUpkeepMachine { UM.recordedMileage = 59000 }
  smallUpkeep = upkeepSequence { US.repetition = 2000 }
  bigUpkeep = upkeepSequence { US.repetition = 6000 }
  result = nextServiceTypeHint (smallUpkeep, [bigUpkeep]) [previousUpkeep]
  expectedResult = bigUpkeep
  in assertEqual "The 6000 sequence must be picked"
    (US.repetition expectedResult) (US.repetition result)

missingOperationStartDate :: Assertion
missingOperationStartDate = let
  machine' = machine {
    M.machineOperationStartDate = Nothing }
  today = fromGregorian 2015 1 1
  result = nextServiceDate machine' (upkeepSequence, []) [] today
  expectedResult = (fromGregorian 2016 12 31, Computed)
  in assertEqual "Date must be +2 years from today, that is: 2016 12 31"
    expectedResult result

firstUpkeep :: Assertion
firstUpkeep = let
  firstUpkeepSequence = US.UpkeepSequence {
    US.oneTime = True ,
    US.repetition = 5000 }
  upkeepSequence2 = US.UpkeepSequence {
    US.repetition = 1000 }
  result = nextServiceDate machine (upkeepSequence, [firstUpkeepSequence, upkeepSequence2]) [] undefined
  expectedResult = (fromGregorian 2000 12 31, Computed)
  in assertEqual "Date must be +1 years, that is: 2000 12 31"
    expectedResult result

pickSmallestRepeatedSequence :: Assertion
pickSmallestRepeatedSequence = let
  upkeepSequence2 = upkeepSequence {
    US.repetition = 2500 }
  upkeepSequence3 = upkeepSequence {
    US.repetition = 20000 }
  oneTimeUpkeepSequence = US.UpkeepSequence {
    US.repetition = 1000 ,
    US.oneTime = True }
  result = nextServiceDate machine (upkeepSequence, [upkeepSequence2, upkeepSequence3, oneTimeUpkeepSequence]) [upkeep] undefined
  expectedResult = (fromGregorian 2000 7 1, Computed)
  in assertEqual "Date must be +1/2 year, that is: 2000 7 1"
    expectedResult result

planned :: Assertion
planned = let
  date = fromGregorian 2000 1 1
  ymdDate = dayToYmd date
  upkeep1 = U.Upkeep {
    U.upkeepClosed = False ,
    U.upkeepDate = ymdDate }
  upkeep2 = upkeep1 {
    U.upkeepDate = dayToYmd $ fromGregorian 2005 1 1 }
  upkeep3 = upkeep1 {
    U.upkeepDate = dayToYmd $ fromGregorian 2009 1 1 }
  upkeep4 = U.Upkeep {
    U.upkeepClosed = True ,
    U.upkeepDate = dayToYmd $ fromGregorian 1999 1 1 }
  result = nextServiceDate undefined undefined [upkeep4, upkeep2, upkeep1, upkeep3] undefined
  expectedResult = (date, Planned)
  in assertEqual "Date must be minimum from planned: 2000 1 1" 
    expectedResult result

noUpkeeps :: Assertion
noUpkeeps = let
  result = nextServiceDate machine (upkeepSequence, []) [] undefined
  expectedResult = (fromGregorian 2001 12 31, Computed)
  in assertEqual "Date must be +2 years from into service: 2001 12 31"
    expectedResult result

closedUpkeeps :: Assertion
closedUpkeeps = let
  upkeep1 = U.Upkeep {
    U.upkeepClosed = True ,
    U.upkeepDate = dayToYmd $ fromGregorian 2000 1 1 }
  upkeep2 = upkeep1 {
    U.upkeepDate = dayToYmd $ fromGregorian 2005 1 1 }
  result = nextServiceDate machine (upkeepSequence, []) [upkeep1, upkeep2] undefined
  expectedResult = (fromGregorian 2007 1 1, Computed)
  in assertEqual "Date must be +2 year from the last service: 2007 1 1"
    expectedResult result

propertyTests :: TestTree
propertyTests = let
  random = mkQCGen 0
  option = QuickCheckReplay $ Just (random, 0)
  in localOption option $ testGroup "Next service day: Property tests" [
    testProperty "When there are planned upkeeps, the earliest is taken" $ plannedUpkeepsProperty random ]

dayGen :: Gen Day
dayGen = do 
  word32 <- choose (minBound, maxBound) :: Gen Word32
  let string = show word32
  let utctime = readTime defaultTimeLocale "%s" string
  return $ utctDay utctime

instance Arbitrary U.Upkeep where
  shrink = shrinkNothing
  arbitrary = do
    day <- dayGen
    uc <- arbitrary
    return $ U.Upkeep {
      U.upkeepClosed = uc ,
      U.upkeepDate = dayToYmd day }
    
instance Arbitrary Day where
  shrink = shrinkNothing
  arbitrary = dayGen

instance Arbitrary US.UpkeepSequence where
  shrink = shrinkNothing
  arbitrary = do
    oneTime <- arbitrary
    label_ <- arbitrary
    return US.newUpkeepSequence {
      US.label_ = pack label_ ,
      US.oneTime = oneTime }

instance Arbitrary UM.UpkeepMachine where
  shrink = shrinkNothing
  arbitrary = return UM.newUpkeepMachine

plannedUpkeepsProperty :: QCGen -> NonEmptyList Day -> [Day] -> Bool
plannedUpkeepsProperty random plannedUpkeepDays closedUpkeepDays = let
  plannedUpkeeps = fmap (\day -> U.Upkeep { U.upkeepClosed = False , U.upkeepDate = dayToYmd $ day }) (getNonEmpty plannedUpkeepDays)
  closedUpkeeps' = fmap (\day -> U.Upkeep { U.upkeepClosed = True , U.upkeepDate = dayToYmd $ day }) closedUpkeepDays
  upkeeps = (plannedUpkeeps ++ closedUpkeeps')
  upkeepsShuffled = shuffle' upkeeps (length upkeeps) random
  earliestDay = minimum (getNonEmpty plannedUpkeepDays)
  in nextServiceDate undefined undefined upkeepsShuffled undefined == (earliestDay, Planned)


propertyTests' :: TestTree
propertyTests' = let
  random = mkQCGen 0
  option = QuickCheckReplay $ Just (random, 0)
  in localOption option $ testGroup "Next service type hint: Property tests" [
    testProperty "When there are no previous upkeeps, the onetime is preferrably picked" $ noPreviousUpkeeps ,
    testProperty "When there are previous upkeeps, the onetime is not picked" $ previousUpkeeps random ,
    testProperty "The result is element of the input list" $ resultFromInputList random ]

resultFromInputList :: QCGen -> [US.UpkeepSequence] -> [UM.UpkeepMachine] -> Bool
resultFromInputList random inputSequences upkeepMachines = let
  seq = US.newUpkeepSequence { US.oneTime = False }
  seqs = seq : inputSequences
  (seq':seqs') = shuffle' seqs (length seqs) random
  result = nextServiceTypeHint (seq', seqs') upkeepMachines
  in elem result (seq':seqs')

noPreviousUpkeeps :: NonEmptyList US.UpkeepSequence -> Bool
noPreviousUpkeeps (NonEmpty (sequences @ (seq:seqs))) = let
  result = nextServiceTypeHint (seq, seqs) []
  in if any (US.oneTime) sequences
    then US.oneTime result
    else not . US.oneTime $ result

previousUpkeeps :: QCGen -> [US.UpkeepSequence] -> NonEmptyList UM.UpkeepMachine -> Bool
previousUpkeeps random sequences (NonEmpty upkeepMachines) = let
  repeatedUpkeepSequence = US.newUpkeepSequence { US.oneTime = False }
  sequencesWithRepeated = repeatedUpkeepSequence : sequences
  (seq:seqs) = shuffle' sequencesWithRepeated (length sequencesWithRepeated) random
  result = nextServiceTypeHint (seq, seqs) upkeepMachines
  in not . US.oneTime $ result
