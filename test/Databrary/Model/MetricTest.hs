{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.MetricTest where

import qualified Data.HashMap.Strict as HMP
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Metric

tests :: TestTree
tests = testGroup "Databrary.Model.Metric"
    [ testCase "tbd"
        (True @?= True)
    ]
