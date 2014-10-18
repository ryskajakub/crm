module Main where

import Test.HUnit(Assertion, assertBool)
import Test.Framework(defaultMainWithOpts)
import Test.Framework.Providers.HUnit(testCase)
import Data.Monoid(mempty)

sampleTest :: Assertion
sampleTest = assertBool "x" False

main :: IO ()
main = defaultMainWithOpts
       [testCase "push-pop" sampleTest]
       mempty
