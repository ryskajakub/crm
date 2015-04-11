module Main where

import Test.HUnit

main :: IO ()
main = do
  runTestTT $ TestList [ TestCase test1, TestCase test2 ]
  return ()

test1 :: Assertion
test1 = (assertBool "test" True)

test2 :: Assertion
test2 = (assertBool "fail" False)
