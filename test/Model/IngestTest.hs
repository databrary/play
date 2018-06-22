{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.IngestTest where

import qualified Data.Vector as V
import qualified Data.Aeson as Aeson
import qualified Data.Either as E
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Test.Tasty
import Test.Tasty.HUnit

import Model.Metric
import Model.Record
import Model.Ingest
import Model.Metric.TypesTest
import Model.Record.TypesTest
import Model.TypeOrphans ()

test_all :: [TestTree]
test_all =
    [ testCase "parseParticipantFieldMapping-1"
        (parseParticipantFieldMapping [] [] @?= Right emptyParticipantFieldMapping)
    , testCase "parseParticipantFieldMapping-2"
        (parseParticipantFieldMapping
           [participantMetricId, participantMetricGender]
           [ (participantMetricId, "col1")
           , (participantMetricGender, "col2")
           ]
           @?= Right participantFieldMapping1)
    , testCase "participantFieldMappingToJSON"
        (participantFieldMappingToJSON emptyParticipantFieldMapping @?= Aeson.toJSON ([] :: [Bool]))
    , testCase "attemptParseRows-1"
        (attemptParseRows emptyParticipantFieldMapping "id,gender\n1,male\n" @?=
           Right (V.fromList ["id", "gender"], V.fromList [emptyParticipantRecord]))
    , testCase "attemptParseRows-2"
        (attemptParseRows participantFieldMappingId "id,gender\n1,male\n" @?=
           Right (V.fromList ["id", "gender"], V.fromList [participantRecordId "1"]))
    , testCase "attemptParseRows-3"
        (attemptParseRows participantFieldMappingIdGender "id,gender\n1, \n" @?=
           Right
               ( V.fromList ["id", "gender"]
               , V.fromList
                   [emptyParticipantRecord { prdId = Field "1" "1", prdGender = FieldEmpty } ]))
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
    "1,\"infoval\",\"descval\",\"2011-06-17\",\"Male\",\"White\"" <>
    ",\"Hispanic or Latino\",2.5,\"Preterm\",10.5" <>
    ",\"normal\",\"English\",\"USA\",\"MA\",\"Lab\"\n"

participantFieldMappingId :: ParticipantFieldMapping2
participantFieldMappingId = mkParticipantFieldMapping2' [(participantMetricId, "id")]

participantFieldMappingIdGender :: ParticipantFieldMapping2
participantFieldMappingIdGender =
    mkParticipantFieldMapping2'
        [ (participantMetricId, "id")
        , (participantMetricGender, "gender")
        ]


