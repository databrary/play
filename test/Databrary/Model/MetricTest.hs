{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.MetricTest where

import qualified Data.HashMap.Strict as HMP
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Metric

tests :: TestTree
tests = testGroup "Databrary.Model.Metric"
    [ testCase "lookupParticipantMetricBySymbolicName-1"
        (lookupParticipantMetricBySymbolicName "birthweight" @?= Just participantMetricBirthWeight)
    , testCase "lookupParticipantMetricBySymbolicName-2"
        (lookupParticipantMetricBySymbolicName "junk" @?= Nothing)
    , testCase "validateParticipantId-2"
        (validateParticipantId "01" @?= Just "01")
    ]
