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
    , testCase "validateParticipantId-1"
        (validateParticipantId "01" @?= Just "01")
    , testCase "validateParticipantId-required"
        (validateParticipantId "" @?= Nothing)
    , testCase "validateParticipantInfo-1"
        (validateParticipantInfo "details" @?= Just "details")
    , testCase "validateParticipantInfo-optional"
        (validateParticipantInfo "" @?= Just "")
    , testCase "validateParticipantDescription-1"
        (validateParticipantDescription "a description here" @?= Just "a description here")
    , testCase "validateParticipantDescription-optional"
        (validateParticipantDescription "" @?= Just "")
    , testCase "validateParticipantGender-1"
        (validateParticipantGender "Male" @?= Just "Male")
    , testCase "validateParticipantGender-2"
        (validateParticipantGender "m" @?= Nothing)
    , testCase "validateParticipantGender-optional"
        (validateParticipantGender "" @?= Just "")
    , testCase "validateParticipantCountry-1"
        (validateParticipantCountry "UK" @?= Just "UK")
    , testCase "validateParticipantRace-1"
        (validateParticipantRace "White" @?= Just "White")
    , testCase "validateParticipantEthnicity-1"
        (validateParticipantEthnicity "Hispanic or Latino" @?= Just "Hispanic or Latino")
    , testCase "validateParticipantPregnancyTerm-1"
        (validateParticipantPregnancyTerm "Preterm" @?= Just "Preterm")
    , testCase "validateParticipantState-1"
        (validateParticipantState "MA" @?= Just "MA")
    , testCase "validateParticipantSetting-1"
        (validateParticipantSetting "Lab" @?= Just "Lab")
    , testCase "validateParticipantDisability-1"
        (validateParticipantDisability "none" @?= Just "none")
    , testCase "validateParticipantGestationalAge-1"
        (validateParticipantGestationalAge "3" @?= Just "3")
    , testCase "validateParticipantGestationalAge-1"
        (validateParticipantGestationalAge "3.5" @?= Just "3.5")
    , testCase "validateParticipantBirthWeight-1"
        (validateParticipantBirthWeight "10.2" @?= Just "10.2")
    , testCase "validateParticipantBirthWeight-1"
        (validateParticipantBirthWeight "10.2" @?= Just "10.2")
    , testCase "validateParticipantLanguage-1"
        (validateParticipantLanguage "english" @?= Just "english")
    ]
