module Main where

import Crm.Server.Core (nextServiceDate)
import Crm.Server.Helpers

import Crm.Shared.Upkeep as U

import Test.HUnit

import Data.Time.Calendar (Day, fromGregorian)

main :: IO ()
main = do
  runTestTT $ TestList [ 
    TestCase planned ]
  return ()

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
