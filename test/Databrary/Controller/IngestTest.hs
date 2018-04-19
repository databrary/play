{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Controller.IngestTest where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.Maybe as MB
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Controller.Ingest
import Databrary.Model.Metric
import Databrary.Model.Record
import Databrary.Model.Record.TypesTest

test_all :: [TestTree]
test_all =
    [ testCase "parseMapping-1"
        ((parseEither mappingParser ((MB.fromJust . decode) "[]" :: Value)) @?= (Right []))
    , testCase "parseMapping-2"
        ((parseEither mappingParser ((MB.fromJust . decode) "[{\"csv_field\": \"col1\", \"metric\": \"id\"}]" :: Value))
           @?= (Right [(participantMetricId, "col1")]))
    , testCase "buildParticipantRecordAction-1"
        ((case buildParticipantRecordAction (participantRecordId "1") Create of
             ParticipantRecordAction Create [Upsert m "1"] ->
                 m == participantMetricId
             _ ->
                 False)
           @? "Expected create with upsert id val to 1")
    , testCase "buildParticipantRecordAction-all"
        ((case buildParticipantRecordAction participantRecordAll Create of
             ParticipantRecordAction
               Create
               [ Upsert m1 "1"
               , Upsert m2 "infoval"
               , Upsert m3 "descval"
               , Upsert m4 "2011-06-17"
               , Upsert m5 "Male"
               , Upsert m6 "White"
               , Upsert m7 "Hispanic or Latino"
               , Upsert m8 "2.5"
               , Upsert m9 "Preterm"
               , Upsert m10 "10.5"
               , Upsert m11 "normal"
               , Upsert m12 "English"
               , Upsert m13 "USA"
               , Upsert m14 "MA"
               , Upsert m15 "Lab"
               ] ->
                    m1 == participantMetricId
                 && m2 == participantMetricInfo
                 && m3 == participantMetricDescription
                 && m4 == participantMetricBirthdate
                 && m5 == participantMetricGender
                 && m6 == participantMetricRace
                 && m7 == participantMetricEthnicity
                 && m8 == participantMetricGestationalAge
                 && m9 == participantMetricPregnancyTerm
                 && m10 == participantMetricBirthWeight
                 && m11 == participantMetricDisability
                 && m12 == participantMetricLanguage
                 && m13 == participantMetricCountry
                 && m14 == participantMetricState
                 && m15 == participantMetricSetting
             _ ->
                 False)
           @? "Expected create with upsert for each metric")
    , testCase "buildParticipantRecordAction-3"
        ((case buildParticipantRecordAction
                 (participantRecordIdGender "1" Nothing)
                 Create of
             ParticipantRecordAction Create [Upsert m "1", NoAction m2] ->
                    m == participantMetricId
                 && m2 == participantMetricGender
             _ ->
                 False)
           @? "Expected create with upsert id val to 1 and no action gender")
    , testCase "buildParticipantRecordAction-4"
        ((case buildParticipantRecordAction
                 (participantRecordIdGender "1" Nothing)
                 (Found (blankRecord undefined undefined)) of
             ParticipantRecordAction (Found _) [Upsert m "1", Delete m2] ->
                    m == participantMetricId
                 && m2 == participantMetricGender
             _ ->
                 False)
           @? "Expected create with upsert id val to 1 and delete gender")
    ]

participantRecordIdGender :: BS.ByteString -> Maybe BS.ByteString -> ParticipantRecord
participantRecordIdGender idVal mGen =
    (participantRecordId idVal) { prdGender = Just (fmap (\v -> (v,v)) mGen) }
