{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.MetricTest where

import Data.Time (fromGregorian)
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Metric

test_all :: TestTree
test_all = testGroup "all"
    [ testCase "lookupParticipantMetricBySymbolicName-1"
        (lookupParticipantMetricBySymbolicName "birthweight" @?= Just participantMetricBirthWeight)
    , testCase "lookupParticipantMetricBySymbolicName-2"
        (lookupParticipantMetricBySymbolicName "junk" @?= Nothing)
    , testCase "validateParticipantId-1"
        (validateParticipantId "01" @?= Just (Just "01"))
    , testCase "validateParticipantId-required"
        (validateParticipantId "" @?= Nothing)
    , testCase "validateParticipantInfo-1"
        (validateParticipantInfo "details" @?= Just (Just "details"))
    , testCase "validateParticipantInfo-optional"
        (validateParticipantInfo "" @?= Just Nothing)
    , testCase "validateParticipantDescription-1"
        (validateParticipantDescription "a description here" @?= Just (Just "a description here"))
    , testCase "validateParticipantDescription-optional"
        (validateParticipantDescription "" @?= Just Nothing)
    , testCase "validateParticipantGender-1"
        (validateParticipantGender "Male" @?= Just (Just "Male"))
    , testCase "validateParticipantGender-2"
        (validateParticipantGender "m" @?= Nothing)
    , testCase "validateParticipantGender-optional"
        (validateParticipantGender "" @?= Just Nothing)
    , testCase "validateParticipantCountry-1"
        (validateParticipantCountry "UK" @?= Just (Just "UK"))
    , testCase "validateParticipantRace-1"
        (validateParticipantRace "White" @?= Just (Just "White"))
    , testCase "validateParticipantEthnicity-1"
        (validateParticipantEthnicity "Hispanic or Latino" @?= Just (Just "Hispanic or Latino"))
    , testCase "validateParticipantPregnancyTerm-1"
        (validateParticipantPregnancyTerm "Preterm" @?= Just (Just "Preterm"))
    , testCase "validateParticipantState-1"
        (validateParticipantState "MA" @?= Just (Just "MA"))
    , testCase "validateParticipantSetting-1"
        (validateParticipantSetting "Lab" @?= Just (Just "Lab"))
    , testCase "validateParticipantDisability-1"
        (validateParticipantDisability "none" @?= Just (Just "none"))
    , testCase "validateParticipantGestationalAge-1"
        (validateParticipantGestationalAge "3" @?= Just (Just 3))
    , testCase "validateParticipantGestationalAge-1"
        (validateParticipantGestationalAge "3.5" @?= Just (Just 3.5))
    , testCase "validateParticipantBirthWeight-1"
        (validateParticipantBirthWeight "10.2" @?= Just (Just 10.2))
    , testCase "validateParticipantBirthWeight-2"
        (validateParticipantBirthWeight "x" @?= Nothing)
    , testCase "validateParticipantBirthWeight-3"
        (validateParticipantBirthWeight "" @?= Just Nothing)
    , testCase "validateParticipantBirthDate-1"
        (validateParticipantBirthdate "1/2/2014" @?= Nothing)
    , testCase "validateParticipantBirthDate-2"
        (validateParticipantBirthdate "2014-01-02" @?= Just (Just (fromGregorian 2014 1 2)))
    , testCase "validateParticipantBirthDate-3"
        (validateParticipantBirthdate "01/02/14" @?= Nothing)
    , testCase "validateParticipantBirthDate-4"
        (validateParticipantBirthdate "2014-1-2" @?= Nothing)
    , testCase "validateParticipantBirthDate-optional"
        (validateParticipantBirthdate "" @?= Just Nothing)
    , testCase "validateParticipantLanguage-1"
        (validateParticipantLanguage "english" @?= Just (Just "english"))
    ]
