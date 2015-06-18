{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Main where

import Crm.Server.Core (nextServiceDate, Planned(..), nextServiceTypeHint)
import Crm.Server.Helpers

import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.UpkeepSequence as US

import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Random

import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Format (readTime)
import Data.Time.Clock (utctDay, UTCTime)
import Data.Bits (shiftL)
import Data.Word (Word32)
import Data.List.Unique (repeated)

import System.Locale (defaultTimeLocale)
import System.Random (next, mkStdGen, StdGen)
import System.Random.Shuffle (shuffle')

import Control.Monad.Error.Class (Error)
import Control.Monad (forM_)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [
  nextServiceDayTests ,
  hintNextServiceTypeTests ]

hintNextServiceTypeTests :: TestTree
hintNextServiceTypeTests = testGroup "Next service type: All tests" [propertyTests']

nextServiceDayTests :: TestTree
nextServiceDayTests = testGroup "Next service day : All tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Next service day : Unit tests" [
  testCase "When there are no upkeeps, then the day is computed from day into operation" noUpkeeps ,
  testCase "When there are only past upkeeps, then the day is computed from the last one" closedUpkeeps ,
  testCase "When there are upkeep sequences and a past upkeep, then the smallest repeated in taken" pickSmallestRepeatedSequence ,
  testCase "When there is a non-repeat upkeep sequence and no past upkeeps, then it is taken" firstUpkeep ,
  testCase "When the operation start date is not specified in machine, today is taken" missingOperationStartDate ]

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
    return US.newUpkeepSequence {
      US.oneTime = oneTime }


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
    testProperty "When there are no previous upkeeps, the onetime on is picked" $ noPreviousUpkeeps ,
    testProperty "When there are previous upkeeps, the onetime is not picked" $ previousUpkeeps ]

noPreviousUpkeeps :: NonEmptyList US.UpkeepSequence -> Bool
noPreviousUpkeeps (NonEmpty (sequences @ (seq:seqs))) = let
  result = nextServiceTypeHint (seq, seqs) []
  in if any (US.oneTime) sequences
    then US.oneTime result
    else not . US.oneTime $ result

previousUpkeeps :: Bool
previousUpkeeps = False
