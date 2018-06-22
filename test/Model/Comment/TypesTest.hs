{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.Comment.TypesTest where

import Data.Time
-- import Test.Tasty.HUnit

import Model.Comment.Types
import Model.Id.Types
import Model.Segment
import Model.Slot.Types

commentRow1 :: CommentRow
commentRow1 =
  CommentRow {
        commentRowId = Id 1
      , commentRowWhoId = Id 2
      , commentRowSlotId = SlotId (Id 3) fullSegment
      , commentRowTime = UTCTime (fromGregorian 2017 1 2) (secondsToDiffTime 0)
      , commentRowText = "a comment"
      }
