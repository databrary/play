module Databrary.Model.TimeTest
   ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Time (fromGregorian)
import Databrary.Model.Time

tests :: TestTree
tests = testGroup "Databrary.Model.Time"
  [ testCase "dateYear-1"
      (dateYear (fromGregorian 2017 1 2) @?= 2017)
  , testCase "maskDateIf-1"
      (maskedYear (maskDateIf True (fromGregorian 2017 1 2)) @?= 2017)
  ]
