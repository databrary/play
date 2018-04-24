{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Slot.TypesTest where

import Test.Tasty.HUnit

import Databrary.Model.Container
import Databrary.Model.Segment
import Databrary.Model.Slot.Types

unit_containerSlot :: Assertion
unit_containerSlot =
  -- example
  (slotSegment . containerSlot) (blankContainer undefined) @?= fullSegment
  -- typical
  -- edge cases
