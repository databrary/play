{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Metric.TypesTest where

import Data.Text (Text)
import Test.Tasty

import Databrary.Model.Release.Types
import Databrary.Model.Id.Types
import Databrary.Model.Category.TypesTest
import Databrary.Model.Metric.Types
import Databrary.Model.Metric

mkParticipantFieldMapping2' :: [(Metric, Text)] -> ParticipantFieldMapping2
mkParticipantFieldMapping2' ms =
    let Right mp = mkParticipantFieldMapping2 ms
    in mp

participantFieldMapping1 :: ParticipantFieldMapping2
participantFieldMapping1 =
    mkParticipantFieldMapping2'
      [ (participantMetricId, "col1")
      , (participantMetricGender, "col2")
      ]

participantFieldMappingAll :: ParticipantFieldMapping2
participantFieldMappingAll =
    mkParticipantFieldMapping2'
      [ (participantMetricId, "id")
      , (participantMetricInfo, "info")
      , (participantMetricDescription, "description")
      , (participantMetricBirthdate, "birthdate")
      , (participantMetricGender, "gender")
      , (participantMetricRace, "race")
      , (participantMetricEthnicity, "ethnicity")
      , (participantMetricGestationalAge, "gestationalage")
      , (participantMetricPregnancyTerm, "pregnancyterm")
      , (participantMetricBirthWeight, "birthweight")
      , (participantMetricDisability, "disability")
      , (participantMetricLanguage, "language")
      , (participantMetricCountry, "country")
      , (participantMetricState, "state")
      , (participantMetricSetting, "setting")
      ]

emptyParticipantFieldMapping :: ParticipantFieldMapping2
emptyParticipantFieldMapping =
    mkParticipantFieldMapping2' []

testMeasureType1 :: MeasureType
testMeasureType1 = MeasureTypeText

testMetric1 :: Metric
testMetric1 =
  Metric {
      metricId = Id (-900)
    , metricCategory = testCategory1
    , metricName = "ID"
    , metricRelease = Just ReleaseEXCERPTS
    , metricType = MeasureTypeText
    , metricOptions = []
    , metricAssumed = Nothing
    , metricDescription = Nothing -- where does this come from?
    , metricRequired = Nothing -- where does this come from?
  }

test_all :: [TestTree]
test_all =
    [
    ]
