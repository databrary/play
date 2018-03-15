{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.IngestTest where

-- import qualified Data.HashMap.Strict as HMP
-- import qualified Data.Vector as V
import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Metric
import Databrary.Model.Ingest
import Databrary.Model.Metric.TypesTest

tests :: TestTree
tests = testGroup "Databrary.Model.Ingest"
    [ testCase "parseParticipantFieldMapping-1"
        (parseParticipantFieldMapping [] [] (Map.fromList []) @?= Right emptyParticipantFieldMapping)
    , testCase "parseParticipantFieldMapping-2"
        (parseParticipantFieldMapping
           [participantMetricId, participantMetricGender]
           ["col1", "col2"]
           (Map.fromList
                [ (participantMetricId, "col1")
                , (participantMetricGender, "col2")
                ])
           @?= Right participantFieldMapping1)
    ]
