module Main where

import Crm.Server.Core (nextServiceDate)
import Crm.Server.Helpers

import qualified Crm.Shared.Upkeep as U
import qualified Crm.Shared.Machine as M
import qualified Crm.Shared.UpkeepSequence as US

import Test.Tasty.HUnit
import Test.Tasty

import Data.Time.Calendar (Day, fromGregorian)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Next service day : Unit tests" [
  testCase "When there are planned upkeeps, the earliest is taken" planned ,
  testCase "When there are no upkeeps, then the day is computed from day into operation" noUpkeeps ,
  testCase "When there are only past upkeeps, then the day is computed from the last one" closedUpkeeps ,
  testCase "When there are upkeep sequences and a past upkeep, then the smallest repeated in taken" pickSmallestRepeatedSequence ,
  testCase "When there is a non-repeat upkeep sequence and no past upkeeps, then it is taken" firstUpkeep ]

machine :: M.Machine
machine = M.Machine {
  M.machineOperationStartDate = dayToYmd $ fromGregorian 2000 1 1 ,
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

firstUpkeep :: Assertion
firstUpkeep = let
  firstUpkeepSequence = US.UpkeepSequence {
    US.oneTime = True ,
    US.repetition = 5000 }
  upkeepSequence2 = US.UpkeepSequence {
    US.repetition = 1000 }
  result = nextServiceDate machine (upkeepSequence, [firstUpkeepSequence, upkeepSequence2]) []
  expectedResult = fromGregorian 2001 1 1
  in assertEqual "Date must be +1 years, that is:"
    expectedResult result

pickSmallestRepeatedSequence :: Assertion
pickSmallestRepeatedSequence = let
  -- todo add one non-repeated sequence
  upkeepSequence2 = upkeepSequence {
    US.repetition = 2500 }
  upkeepSequence3 = upkeepSequence {
    US.repetition = 20000 }
  result = nextServiceDate machine (upkeepSequence, [upkeepSequence2, upkeepSequence3]) [upkeep]
  expectedResult = fromGregorian 2000 7 1
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
  result = nextServiceDate undefined undefined [upkeep4, upkeep2, upkeep1, upkeep3]
  expectedResult = date
  in assertEqual "Date must be minimum from planned: 2000 1 1" 
    expectedResult result

noUpkeeps :: Assertion
noUpkeeps = let
  result = nextServiceDate machine (upkeepSequence, []) []
  expectedResult = fromGregorian 2001 12 31
  in assertEqual "Date must be +2 years from into service: 2001 12 31"
    expectedResult result

closedUpkeeps :: Assertion
closedUpkeeps = let
  upkeep1 = U.Upkeep {
    U.upkeepClosed = True ,
    U.upkeepDate = dayToYmd $ fromGregorian 2000 1 1 }
  upkeep2 = upkeep1 {
    U.upkeepDate = dayToYmd $ fromGregorian 2005 1 1 }
  result = nextServiceDate machine (upkeepSequence, []) [upkeep1, upkeep2]
  expectedResult = fromGregorian 2007 1 1
  in assertEqual "Date must be +2 year from the last service: 2007 1 1"
    expectedResult result
