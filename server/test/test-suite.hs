{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Main where

import           Control.Monad.Error.Class (Error)
import           Control.Monad             (forM_)
import           Data.Monoid               ((<>))

import           Data.Time.Calendar        (Day, fromGregorian)
import           Data.Time.Format          (parseTimeOrError, defaultTimeLocale)
import           Data.Time.Clock           (utctDay, UTCTime)
import           Data.Bits                 (shiftL)
import           Data.Word                 (Word32)
import           Data.Text                 (pack, Text)
import qualified Data.Text                 as Text

import           System.Random             (next, mkStdGen, StdGen)
import           System.Random.Shuffle     (shuffle')

import           Test.Tasty.HUnit
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Random

import           Crm.Server.Core           (nextServiceDate, NextServiceDate(..), nextServiceTypeHint)
import           Crm.Server.Helpers
import           Crm.Server.Parsers        (parseMarkup, parseDate)

import qualified Crm.Shared.Upkeep         as U
import qualified Crm.Shared.Machine        as M
import qualified Crm.Shared.UpkeepSequence as US
import qualified Crm.Shared.UpkeepMachine  as UM
import qualified Crm.Shared.ServerRender   as SR
import           Crm.Shared.YearMonthDay   (dayToYmd)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [
  nextServiceDayTests ,
  hintNextServiceTypeTests ,
  parserTests ]

parserTests :: TestTree
parserTests = testGroup "Parser test" [
  testCase "Header parser test" headerParserTest ,
  testCase "Test parsing of a text with multiple lists" multipleLists ,
  testCase "Parse list should survive no newline at the end." noNewlineAtTheEnd ,
  testProperty "Day parse" parseDayProp ,
  parseMarkupProperties ]

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
  testCase "When the operation start date is not specified in machine, today is taken" missingOperationStartDate ,
  testCase "When the machine is inactive, nothing else is looked at and no date is computed" inactiveMachine ,
  testCase "When the machine upkeep is just a repair, it is skipped in computation of next date" repairUpkeep ,
  testCase "When the machine upkeep is installation, then the next service is first" installationUpkeep ,
  testCase "When there is an installation, then it is taken instead of the into operation as base for computing the first service" installationSupercedesIntoOperation ,
  testCase "When there is a planned call, then the earliest is taken as computed" plannedCallUpkeep ]

unitTests' :: TestTree
unitTests' = testGroup "Next service type hint : Unit tests" [
  testCase "When there time for big maintanance, the big upkeep sequence is picked." bigUpkeepAssertion ,
  testCase "When there time for small maintanance, the small upkeep sequence is picked." smallUpkeepAssertion ]

machine :: M.Machine
machine = M.Machine {
  M.machineOperationStartDate = Just $ dayToYmd $ fromGregorian 2000 1 1 ,
  M.archived = False ,
  M.mileagePerYear = 5000 }

upkeepMachine :: UM.UpkeepMachine
upkeepMachine = UM.newUpkeepMachine

upkeepSequence :: US.UpkeepSequence
upkeepSequence = US.UpkeepSequence {
  US.oneTime = False ,
  US.repetition = 10000 }

oneTimeUpkeepSequence = US.UpkeepSequence {
  US.repetition = 1000 ,
  US.oneTime = True }

upkeepDate :: Day
upkeepDate = fromGregorian 2000 1 1

upkeep :: U.Upkeep
upkeep = U.Upkeep {
  U.upkeepDate = dayToYmd upkeepDate ,
  U.upkeepClosed = True ,
  U.setDate = False }

noNewlineAtTheEnd :: Assertion
noNewlineAtTheEnd = let
  list = "item1 \n item2"
  Right result = parseMarkup . pack $ list
  expectedResult = map (SR.PlainText . pack) [
    "item1 ", " item2" ]
  in assertEqual "Newline should be added at the end"
    expectedResult result

headerParserTest :: Assertion
headerParserTest = let
  input = unlines [
    "+ header" ,
    "plain text" ,
    "+noheader" ,
    "+ header 2" ,
    "- list elem 1" ,
    "-  list elem 2" ]
  result = parseMarkup (pack input)
  expectedResult = Right [
    SR.Header . pack $ "header" ,
    SR.PlainText . pack $ "plain text" ,
    SR.PlainText . pack $ "+noheader" ,
    SR.Header . pack $ "header 2" ,
    SR.UnorderedList [
      pack "list elem 1" ,
      pack " list elem 2" ] ]
  in assertEqual "Headers should be parsed correctly." expectedResult result

multipleLists :: Assertion
multipleLists = let
  input = unlines [
    "plain text" ,
    "another plain text" ,
    "-  list elem 1 " ,
    "- list elem 2 " ,
    "another another plain text 2 " ,
    "- list elem 3 " ]
  result = parseMarkup (pack input)
  expectedResult = Right [
    SR.PlainText . pack $ "plain text" ,
    SR.PlainText . pack $ "another plain text" ,
    SR.UnorderedList [
      pack $ " list elem 1 " ,
      pack $ "list elem 2 " ] ,
    SR.PlainText . pack $ "another another plain text 2 " ,
    SR.UnorderedList [ pack $ "list elem 3 " ]]
  in assertEqual "Sample string should be parsed." expectedResult result

smallUpkeepAssertion :: Assertion
smallUpkeepAssertion = let
  previousUpkeep = UM.newUpkeepMachine { UM.recordedMileage = 57000 }
  smallUpkeep = upkeepSequence { US.repetition = 2000 }
  bigUpkeep = upkeepSequence { US.repetition = 6000 }
  result = nextServiceTypeHint (smallUpkeep, [bigUpkeep]) [previousUpkeep]
  expectedResult = smallUpkeep
  in assertEqual "The 2000 sequence must be picked"
    (US.repetition expectedResult) (US.repetition result)

parseMarkupProperty :: NonEmptyList SR.Markup -> Bool
parseMarkupProperty (NonEmpty markups') =
  (Right markups ==) . parseMarkup . Text.unlines . concat . map markupToText $ markups
    where
    markupToText (SR.Header h) = [pack "+ " <> h]
    markupToText (SR.PlainText pt) = [pt]
    markupToText (SR.UnorderedList l) = map (\listElement -> pack "- " <> listElement) l
    markups = foldr foldStep [] markups'
      where
      foldStep a' acc' = case (acc', a') of
        (SR.UnorderedList ul : rest, SR.UnorderedList ul') -> (SR.UnorderedList $ ul ++ ul') : rest
        (pts, a) -> a : pts

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
  expectedResult = Computed $ fromGregorian 1970 1 1
  in assertEqual "Date must be 1970 1 1"
    expectedResult result

repairUpkeep :: Assertion
repairUpkeep = let
  upkeepMachine' = upkeepMachine { UM.upkeepType = UM.Repair }
  later = upkeep {
    U.upkeepDate = dayToYmd $ fromGregorian 2016 1 1 }
  earlier = upkeep {
    U.upkeepDate = dayToYmd $ fromGregorian 2015 1 1 }
  expectedResult = Computed $ fromGregorian 2016 12 31
  result = nextServiceDate machine (upkeepSequence, []) [(earlier, upkeepMachine), (later, upkeepMachine')] undefined
  in assertEqual "Repair must be skipped" expectedResult result

installationUpkeep :: Assertion
installationUpkeep = let
  upkeepMachine' = upkeepMachine { UM.upkeepType = UM.Installation }
  result = nextServiceDate machine (upkeepSequence, [oneTimeUpkeepSequence]) [(upkeep, upkeepMachine')] undefined
  expectedResult = Computed $ fromGregorian 2000 3 14
  in assertEqual "Installation must not be counted as a first service" expectedResult result

plannedCallUpkeep :: Assertion
plannedCallUpkeep = let
  resultDate = fromGregorian 2005 1 1
  upkeep' = upkeep {
    U.upkeepClosed = False ,
    U.upkeepDate = dayToYmd resultDate ,
    U.setDate = True }
  upkeep2' = upkeep' {
    U.upkeepDate = dayToYmd $ fromGregorian 2006 1 1 }
  upkeepClosed = upkeep {
    U.upkeepDate = dayToYmd $ fromGregorian 2003 1 1 }
  result = nextServiceDate machine (upkeepSequence, [])
    [(upkeep', upkeepMachine), (upkeep2', upkeepMachine), (upkeepClosed, upkeepMachine)] undefined
  expectedResult = Computed resultDate
  in assertEqual "The earliest planned call must be taken" expectedResult result

installationSupercedesIntoOperation :: Assertion
installationSupercedesIntoOperation = let
  upkeepMachine' = upkeepMachine { UM.upkeepType = UM.Installation }
  upkeepLater = upkeep {
    U.upkeepDate = dayToYmd $ fromGregorian 2005 1 1 }
  result = nextServiceDate machine (upkeepSequence, []) [(upkeepLater, upkeepMachine')] undefined
  expectedResult = Computed $ fromGregorian 2007 1 1 
  in assertEqual "Installation must be taken instead of into operation field" expectedResult result

inactiveMachine :: Assertion
inactiveMachine = let
  machine' = machine {
    M.archived = True }
  result = nextServiceDate machine' undefined undefined undefined
  expectedResult = Inactive
  in assertEqual "Inactive should be returned" expectedResult result

firstUpkeep :: Assertion
firstUpkeep = let
  firstUpkeepSequence = US.UpkeepSequence {
    US.oneTime = True ,
    US.repetition = 5000 }
  upkeepSequence2 = US.UpkeepSequence {
    US.repetition = 1000 }
  result = nextServiceDate machine (upkeepSequence, [firstUpkeepSequence, upkeepSequence2]) [] undefined
  expectedResult = Computed $ fromGregorian 2000 12 31
  in assertEqual "Date must be +1 years, that is: 2000 12 31"
    expectedResult result

pickSmallestRepeatedSequence :: Assertion
pickSmallestRepeatedSequence = let
  upkeepSequence2 = upkeepSequence {
    US.repetition = 2500 }
  upkeepSequence3 = upkeepSequence {
    US.repetition = 20000 }
  result = nextServiceDate machine (upkeepSequence, [upkeepSequence2, upkeepSequence3, oneTimeUpkeepSequence]) [(upkeep, UM.newUpkeepMachine)] undefined
  expectedResult = Computed $ fromGregorian 2000 7 1
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
  result = nextServiceDate undefined undefined (fmap (,UM.newUpkeepMachine) [upkeep4, upkeep2, upkeep1, upkeep3]) undefined
  expectedResult = Planned date
  in assertEqual "Date must be minimum from planned: 2000 1 1" 
    expectedResult result

noUpkeeps :: Assertion
noUpkeeps = let
  result = nextServiceDate machine (upkeepSequence, []) [] undefined
  expectedResult = Computed $ fromGregorian 2001 12 31
  in assertEqual "Date must be +2 years from into service: 2001 12 31"
    expectedResult result

closedUpkeeps :: Assertion
closedUpkeeps = let
  upkeep1 = U.Upkeep {
    U.upkeepClosed = True ,
    U.upkeepDate = dayToYmd $ fromGregorian 2000 1 1 }
  upkeep2 = upkeep1 {
    U.upkeepDate = dayToYmd $ fromGregorian 2005 1 1 }
  result = nextServiceDate machine (upkeepSequence, []) (fmap (,UM.newUpkeepMachine) [upkeep1, upkeep2]) undefined
  expectedResult = Computed $ fromGregorian 2007 1 1
  in assertEqual "Date must be +2 year from the last service: 2007 1 1"
    expectedResult result

propertyTests :: TestTree
propertyTests = let
  random = mkQCGen 0
  option = QuickCheckReplay $ Just (random, 0)
  in localOption option $ testGroup "Next service day: Property tests" [
    testProperty "When there are planned upkeeps, the earliest is taken" $ plannedUpkeepsProperty random ]

newtype Day' = Day' Int
  deriving Show
instance Arbitrary Day' where
  arbitrary = fmap Day' $ choose (1, 31)

newtype Month = Month Int
  deriving Show
instance Arbitrary Month where
  arbitrary = fmap Month $ choose (1, 21)

newtype Year = Year Int
  deriving Show
instance Arbitrary Year where
  arbitrary = fmap Year $ choose (1, 9999)

parseDayProp :: Day' -> Month -> Year -> Bool
parseDayProp (Day' day) (Month month) (Year year) = 
  (parseDate input) == expectedResult
  where
  expectedResult = Right (day, month, year)
  input = show day <> "." <> show month <> "." <> show year

parseMarkupProperties :: TestTree
parseMarkupProperties = let
  random = mkQCGen 0
  option = QuickCheckReplay $ Just (random, 0)
  in localOption option $ testGroup "Markup parser: Property tests" [
    testProperty "Correctly parse any input lines" parseMarkupProperty ]

dayGen :: Gen Day
dayGen = do 
  word32 <- choose (minBound, maxBound) :: Gen Word32
  let string = show word32
  let utctime = parseTimeOrError False defaultTimeLocale "%s" string
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

instance Arbitrary Text where
  shrink = shrinkNothing
  arbitrary = do
    string <- listOf ( suchThat arbitrary $ \c -> c /= '\n' && c /= '\r' )
    return . pack $ string

instance Arbitrary SR.Markup where
  shrink = shrinkNothing
  arbitrary = do
    let 
      plainTextGen = do
        t <- arbitrary
        return . SR.PlainText $ t
      unorderedListGen = do
        NonEmpty list <- arbitrary
        return . SR.UnorderedList $ list
      headerGen = do
        t <- arbitrary
        return . SR.Header $ t
    oneof [plainTextGen, unorderedListGen, headerGen]

plannedUpkeepsProperty :: QCGen -> NonEmptyList Day -> [Day] -> Bool
plannedUpkeepsProperty random plannedUpkeepDays closedUpkeepDays = let
  plannedUpkeeps = fmap (\day -> upkeep { U.upkeepClosed = False , U.upkeepDate = dayToYmd $ day }) (getNonEmpty plannedUpkeepDays)
  closedUpkeeps' = fmap (\day -> upkeep { U.upkeepClosed = True , U.upkeepDate = dayToYmd $ day }) closedUpkeepDays
  upkeeps = fmap (,UM.newUpkeepMachine) (plannedUpkeeps ++ closedUpkeeps')
  upkeepsShuffled = shuffle' upkeeps (length upkeeps) random
  earliestDay = minimum (getNonEmpty plannedUpkeepDays)
  nonArchivedMachine = M.Machine { M.archived = False }
  in nextServiceDate nonArchivedMachine undefined upkeepsShuffled undefined == Planned earliestDay

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
