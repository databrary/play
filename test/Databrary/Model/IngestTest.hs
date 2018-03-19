{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.IngestTest where

-- import qualified Data.HashMap.Strict as HMP
import qualified Data.Vector as V
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map as Map
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
    , testCase "determineMapping-1"
        (determineMapping [] [] @?= Right emptyParticipantFieldMapping)
    , testCase "determineMapping-2"
        (determineMapping [participantMetricId] ["id"] @?= Right participantFieldMappingId)
    ]

participantFieldMappingId :: ParticipantFieldMapping
participantFieldMappingId = emptyParticipantFieldMapping { pfmId = Just "id" }

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
