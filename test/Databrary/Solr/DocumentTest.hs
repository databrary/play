module Databrary.Solr.DocumentTest where

import Data.Aeson
-- import Data.Text
import Test.Tasty.HUnit

import Databrary.Model.Id
import Databrary.Model.Metric
import Databrary.Model.Segment
import Databrary.Model.Tag
import Databrary.Solr.Document

-- Session demonstrating how to use Document's functions
unit_Document_examples :: Assertion
unit_Document_examples = do
    metricField participantMetricBirthWeight @?= "record_numeric_birth_weight"
    toJSON (SolrRecordMeasures [(participantMetricId, "1")]) @?= object [("record_text_ID",String "1")]
    toJSON (SolrSegment emptySegment) @?= String "empty"
    toJSON (SolrTagId "1s" (Id 1) (TagName "tagone")) @?=
        object [("tag_name",String "tagone"),("tag_id",Number 1.0),("content_type",String "tag_id"),("id",String "1s")]
