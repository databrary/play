{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Record
  ( htmlRecordForm
  , htmlRecordMeasureForm
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Monoid (mempty)

import Databrary.Action
import Databrary.Model.Volume
import Databrary.Model.Record
import Databrary.Model.RecordCategory
import Databrary.Model.Metric
import Databrary.View.Form

import {-# SOURCE #-} Databrary.Controller.Record

htmlRecordForm :: Volume -> AppRequest -> FormHtml f
htmlRecordForm vol = htmlForm "Create record"
  createRecord (HTML, volumeId vol)
  (field "category" $ inputSelect Nothing $ ("", "<record>") : map (\c -> (BSC.pack $ show $ recordCategoryId c, recordCategoryName c)) allRecordCategories)
  (const mempty)

htmlRecordMeasureForm :: Record -> Metric -> AppRequest -> FormHtml f
htmlRecordMeasureForm rec met = htmlForm "Set measure"
  postRecordMeasure (HTML, recordId rec, metricId met)
  (field "datum" $ inputText (Nothing :: Maybe String))
  (const mempty)
