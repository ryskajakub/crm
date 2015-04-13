module Main where

import Crm.Server.Core (nextServiceDate)
import Crm.Server.Helpers

import Crm.Shared.Upkeep as U

import Test.Tasty.HUnit
import Test.Tasty

import Data.Time.Calendar (Day, fromGregorian)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Next service day : Unit tests" [testCase "Planned next service" planned]

planned :: Assertion
planned = let
  date = fromGregorian 2000 1 1
  ymdDate = dayToYmd date
  upkeep = U.Upkeep {
    U.upkeepClosed = False ,
    U.upkeepDate = ymdDate }
  result = nextServiceDate undefined undefined [upkeep]
  expectedResult = date
  in assertEqual "When the next planned day is specified, then it is taken without any further computations" 
    expectedResult result
