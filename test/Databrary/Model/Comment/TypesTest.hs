{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Comment.TypesTest where

import Data.Time
-- import Test.Tasty.HUnit

import Databrary.Model.Comment.Types
import Databrary.Model.Id.Types
import Databrary.Model.Segment
import Databrary.Model.Slot.Types

commentRow1 :: CommentRow
commentRow1 =
  CommentRow {
        commentRowId = Id 1
      , commentRowWhoId = Id 2
      , commentRowSlotId = SlotId (Id 3) fullSegment
      , commentRowTime = UTCTime (fromGregorian 2017 1 2) (secondsToDiffTime 0)
      , commentRowText = "a comment"
      }
