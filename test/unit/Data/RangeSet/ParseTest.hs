module Data.RangeSet.ParseTest 
   ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Data.RangeSet.Parse
import qualified Data.RangeSet.List as R

tests :: TestTree
tests = testGroup "Data.RangeSet.Parse"
  [ testCase "parseRangeSet-1"
    (parseRangeSet "" @?= (Just (R.fromList [] :: R.RSet Bool)))
  , testCase "parseRangeSet-2"
    (parseRangeSet "1-10" @?= (Just (R.fromRangeList [(1, 10)] :: R.RSet Int)))
  , testCase "parseRangeSet-3"
    (parseRangeSet "1-10,20-30"
       @?= (Just (R.fromRangeList [(1, 10), (20, 30)] :: R.RSet Int)))
  , testCase "showRangeSet-1"
    (showRangeSet (R.fromRangeList ([] :: [(Int,Int)]))
       @?= "")
  , testCase "showRangeSet-2"
    (showRangeSet (R.fromRangeList ([(1, 10)] :: [(Int,Int)]))
       @?= "1-10")
  ]
