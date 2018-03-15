{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Controller.IngestTest where

import Data.Aeson
import Data.Aeson.Types
-- import qualified Data.Either as E
import qualified Data.Maybe as MB
import qualified Data.Map as Map
-- import qualified Data.HashMap.Strict as HMP
-- import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Controller.Ingest
import Databrary.Model.Metric

tests :: TestTree
tests = testGroup "Databrary.Controller.Ingest"
    [ testCase "parseMapping-1"
        ((parseEither mappingParser ((MB.fromJust . decode) "[]" :: Value)) @?= (Right (Map.fromList [])))
    , testCase "parseMapping-2"
        ((parseEither mappingParser ((MB.fromJust . decode) "[{\"csv_field\": \"col1\", \"metric\": \"id\"}]" :: Value))
           @?= (Right (Map.fromList [(participantMetricId, "col1")])))
    ]
