{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Record.TypesTest where

import qualified Data.ByteString as BS
import Data.Time (fromGregorian)
import Hedgehog
import qualified Hedgehog.Gen as Gen
-- import Test.Tasty

import Databrary.Model.Category
import Databrary.Model.Category.TypesTest
import Databrary.Model.Id.Types
import Databrary.Model.Metric
import Databrary.Model.Record.Types
import Databrary.Model.Volume.Types

-- TODO: expand these to really generate random measure values
genBirthdateMeasure :: Gen (Metric, BS.ByteString)
genBirthdateMeasure =
    pure (participantMetricBirthdate, "1990-01-02")

genGenderMeasure :: Gen (Metric, BS.ByteString)
genGenderMeasure =
    pure (participantMetricGender, "Male")

genParticipantMetricValue :: Gen (Metric, BS.ByteString)
genParticipantMetricValue =
    Gen.choice [genBirthdateMeasure, genGenderMeasure]

genCreateMeasure :: Gen Measure
genCreateMeasure = do
    (mtrc, val) <- genParticipantMetricValue
    Measure
        <$> (pure . error) "measure record not set yet"
        <*> pure mtrc
        <*> pure val

genCategory :: Gen Category
genCategory = Gen.element allCategories

genCreateRecord :: Volume -> Gen Record
genCreateRecord vol = do
    -- repeats some logic from blankRecord
    Record
        <$> (RecordRow <$> (pure . error) "Id set after saved" <*> genCategory)
        <*> pure []
        <*> Gen.maybe Gen.enumBounded
        <*> pure vol

participantRecordAll :: ParticipantRecord
participantRecordAll =
    ParticipantRecord
         { prdId = Field "1" "1"
         , prdInfo = Field "infoval" "infoval"
         , prdDescription = Field "descval" "descval"
         , prdBirthdate = Field "2011-06-17" (fromGregorian 2011 6 17)
         , prdGender = Field "Male" "Male"
         , prdRace = Field "White" "White"
         , prdEthnicity = Field "Hispanic or Latino" "Hispanic or Latino"
         , prdGestationalAge = Field "2.5" 2.5
         , prdPregnancyTerm = Field "Preterm" "Preterm"
         , prdBirthWeight = Field "10.5" 10.5
         , prdDisability = Field "normal" "normal"
         , prdLanguage = Field "English" "English"
         , prdCountry = Field "USA" "USA"
         , prdState = Field "MA" "MA"
         , prdSetting = Field "Lab" "Lab"
         }

participantRecordId :: BS.ByteString -> ParticipantRecord
participantRecordId idVal =
    emptyParticipantRecord { prdId = Field idVal idVal }

emptyParticipantRecord :: ParticipantRecord
emptyParticipantRecord =
    ParticipantRecord
         { prdId = FieldUnused
         , prdInfo = FieldUnused
         , prdDescription = FieldUnused
         , prdBirthdate = FieldUnused
         , prdGender = FieldUnused
         , prdRace = FieldUnused
         , prdEthnicity = FieldUnused
         , prdGestationalAge = FieldUnused
         , prdPregnancyTerm = FieldUnused
         , prdBirthWeight = FieldUnused
         , prdDisability = FieldUnused
         , prdLanguage = FieldUnused
         , prdCountry = FieldUnused
         , prdState = FieldUnused
         , prdSetting = FieldUnused
         }

testRecordRow1 :: RecordRow
testRecordRow1 =
    RecordRow {
        recordId = Id 100
      , recordCategory = testCategory1
    }
