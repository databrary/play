module Databrary.JSONTest
   ( tests )
where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.JSON
import Data.Aeson

tests :: TestTree
tests = testGroup "Databrary.JSON"
  [ testCase "eitherJSON-1"
      (eitherJSON (toJSON (1 :: Int)) @?= Right (1 :: Int))
  ]
