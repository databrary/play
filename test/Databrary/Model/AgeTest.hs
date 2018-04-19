{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.AgeTest where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Time (fromGregorian, secondsToDiffTime)

import Databrary.Model.Age
import Databrary.Model.Time

test_age :: [TestTree]
test_age = 
    [ testCase "example" (do
          runAge 1 3 @?= 2)
    , testCase "typical" (do
          runAge 10 3 @?= (-7))
    ]

type Days = Int

type DayOfMonth = Int

runAge :: DayOfMonth -> DayOfMonth -> Days
runAge d1 d2 = ageDays (age (mkDate d1) (mkDate d2))
  where
    mkDate :: DayOfMonth -> Date
    mkDate = fromGregorian 2000 1

test_yearsAge :: [TestTree]
test_yearsAge =
    [ testCase "example" 
          (yearsAge (1 :: Double) @?= Age 366)
    ]

daySeconds :: Integer
daySeconds = 60*60*24

test_ageTime :: [TestTree]
test_ageTime =
    [ testCase "example"
          (ageTime (Age 1) @?= secondsToDiffTime daySeconds)
    ]
