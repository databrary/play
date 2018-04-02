{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Data.Csv.ContribTest (tests) where

import qualified Data.HashMap.Strict as HMP
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Data.Csv.Contrib

tests :: TestTree
tests = testGroup "Data.Csv.ContribTest"
    [ testCase "extractColumnDefaulting"
        (extractColumnDefaulting "c1" (V.fromList [(HMP.fromList [("c1", "val1")])]) @?= ["val1"])
    , testCase "extractColumnsDistinctSample"
        (extractColumnsDistinctSample
           1
           (V.fromList ["c1"])
           (V.fromList
              [ (HMP.fromList [("c1", "val1")])
              , (HMP.fromList [("c1", "val1")])
              , (HMP.fromList [("c1", "val2")])
              ])
           @?= [("c1", ["val1"])])
    , testCase "repairCarriageReturnOnly-1"
        (repairCarriageReturnOnly "abc\r\n" @?= "abc\r\n")
    , testCase "repairCarriageReturnOnly-2"
        (repairCarriageReturnOnly "abc\r" @?= "abc\r\n")
    , testCase "repairDuplicateLineEndings-fix"
        (repairDuplicateLineEndings "abc\r\r\n" @?= "abc\r\n")
    , testCase "repairDuplicateLineEndings-nofix"
        (repairDuplicateLineEndings "abc\r\n" @?= "abc\r\n")
    , testCase "removeBomPrefix-fix"
        (removeBomPrefix "\357\273\277abc" @?= "abc")
    , testCase "removeBomPrefix-nofix"
        (removeBomPrefix "abc" @?= "abc")
    ]
