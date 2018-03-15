{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Metric.TypesTest where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Metric.Types

participantFieldMapping1 :: ParticipantFieldMapping
participantFieldMapping1 =
    ParticipantFieldMapping
        { pfmId = Just "col1"
        , pfmInfo = Nothing
        , pfmDescription = Nothing
        , pfmBirthdate = Nothing
        , pfmGender = Just "col2"
        , pfmRace = Nothing
        , pfmEthnicity = Nothing
        , pfmGestationalAge = Nothing
        , pfmPregnancyTerm = Nothing
        , pfmBirthWeight = Nothing
        , pfmDisability = Nothing
        , pfmLanguage = Nothing
        , pfmCountry = Nothing
        , pfmState = Nothing
        , pfmSetting = Nothing
        }

tests :: TestTree
tests = testGroup "Databrary.Model.Metric.Types"
    [
    ]
