module Main where

import Crm.Server.Core (nextServiceDate)
import Crm.Server.Helpers

import Crm.Shared.Upkeep as U
import Crm.Shared.Machine as M
import Crm.Shared.UpkeepSequence as US

import Test.Tasty.HUnit
import Test.Tasty

import Data.Time.Calendar (Day, fromGregorian)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Next service day : Unit tests" [
  testCase "When there are planned upkeeps, the earliest is taken" planned ,
  testCase "When there are no upkeeps, then the day is computed from day into operation" noUpkeeps ]

machine :: M.Machine
machine = M.Machine {
  M.machineOperationStartDate = dayToYmd $ fromGregorian 2000 1 1 ,
  M.mileagePerYear = 5000 }

upkeepSequence :: US.UpkeepSequence
upkeepSequence = US.UpkeepSequence {
  US.repetition = 10000 }

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
  result = nextServiceDate undefined undefined [upkeep4 , upkeep2 , upkeep1 , upkeep3]
  expectedResult = date
  in assertEqual "Date must be minimum from planned: 2000 1 1" 
    expectedResult result

noUpkeeps :: Assertion
noUpkeeps = let
  result = nextServiceDate machine (upkeepSequence, []) []
  expectedResult = fromGregorian 2001 12 31
  in assertEqual "Date must be +2 years from into service, 2001 12 31"
    expectedResult result
