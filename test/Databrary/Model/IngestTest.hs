{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.IngestTest where

-- import qualified Data.HashMap.Strict as HMP
-- import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Data.Aeson as Aeson
import qualified Data.Either as E
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Metric
import Databrary.Model.Record
import Databrary.Model.Ingest
import Databrary.Model.Metric.TypesTest

tests :: TestTree
tests = testGroup "Databrary.Model.Ingest"
    [ testCase "parseParticipantFieldMapping-1"
        (parseParticipantFieldMapping [] (Map.fromList []) @?= Right emptyParticipantFieldMapping)
    , testCase "parseParticipantFieldMapping-2"
        (parseParticipantFieldMapping
           [participantMetricId, participantMetricGender]
           (Map.fromList
                [ (participantMetricId, "col1")
                , (participantMetricGender, "col2")
                ])
           @?= Right participantFieldMapping1)
    , testCase "participantFieldMappingToJSON"
        (participantFieldMappingToJSON emptyParticipantFieldMapping @?= Aeson.toJSON ([] :: [Bool]))
    , testCase "attemptParseRows-1"
        (attemptParseRows emptyParticipantFieldMapping "id,gender\n1,male\n" @?=
           Right (V.fromList ["id", "gender"], V.fromList [emptyParticipantRecord]))
    , testCase "attemptParseRows-2"
        (attemptParseRows participantFieldMappingId "id,gender\n1,male\n" @?=
           Right (V.fromList ["id", "gender"], V.fromList [participantRecordId "1"]))
    , testCase "attemptParseRows-all"
        (attemptParseRows
           participantFieldMappingAll
           allValuesOneRow @?=
           Right (allHeaders, V.fromList [participantRecordAll]))
    , testCase "determineMapping-1"
        (determineMapping [] [] @?= Right emptyParticipantFieldMapping)
    , testCase "determineMapping-2"
        (determineMapping [participantMetricId] ["id"] @?= Right participantFieldMappingId)
    , testCase "determineMapping-3"
        (E.isLeft (determineMapping [participantMetricId] ["junkcol"]) @? "expected left")
    ]

allHeaders :: V.Vector BS.ByteString
allHeaders =
    V.fromList
        ["id","info","description","birthdate","gender","race"
        ,"ethnicity","gestationalage","pregnancyterm","birthweight"
        ,"disability","language","country","state","setting"]  

allValuesOneRow :: BS.ByteString
allValuesOneRow =
    "id,info,description,birthdate,gender,race" <>
    ",ethnicity,gestationalage,pregnancyterm,birthweight" <>
    ",disability,language,country,state,setting\n" <>
    "1,\"infoval\",\"descval\",\"06/17/2011\",\"Male\",\"White\"" <>
    ",\"Hispanic or Latino\",2.5,\"Preterm\",10.5" <>
    ",\"normal\",\"English\",\"USA\",\"MA\",\"Lab\"\n"

participantFieldMappingId :: ParticipantFieldMapping
participantFieldMappingId = emptyParticipantFieldMapping { pfmId = Just "id" }

participantRecordAll :: ParticipantRecord
participantRecordAll =
    ParticipantRecord
         { prdId = Just "1"
         , prdInfo = Just "infoval"
         , prdDescription = Just "descval"
         , prdBirthdate = Just "06/17/2011"
         , prdGender = Just "Male"
         , prdRace = Just "White"
         , prdEthnicity = Just "Hispanic or Latino"
         , prdGestationalAge = Just "2.5"
         , prdPregnancyTerm = Just "Preterm"
         , prdBirthWeight = Just "10.5"
         , prdDisability = Just "normal"
         , prdLanguage = Just "English"
         , prdCountry = Just "USA"
         , prdState = Just "MA"
         , prdSetting = Just "Lab"
         }

participantRecordId :: BS.ByteString -> ParticipantRecord
participantRecordId idVal =
    emptyParticipantRecord { prdId = Just idVal }

emptyParticipantRecord :: ParticipantRecord
emptyParticipantRecord =
    ParticipantRecord
         { prdId = Nothing
         , prdInfo = Nothing
         , prdDescription = Nothing
         , prdBirthdate = Nothing
         , prdGender = Nothing
         , prdRace = Nothing
         , prdEthnicity = Nothing
         , prdGestationalAge = Nothing
         , prdPregnancyTerm = Nothing
         , prdBirthWeight = Nothing
         , prdDisability = Nothing
         , prdLanguage = Nothing
         , prdCountry = Nothing
         , prdState = Nothing
         , prdSetting = Nothing
         }
