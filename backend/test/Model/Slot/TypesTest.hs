{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.Slot.TypesTest where

import Test.Tasty.HUnit

import Model.Container
import Model.Segment
import Model.Slot.Types

unit_containerSlot :: Assertion
unit_containerSlot =
  -- example
  (slotSegment . containerSlot) (blankContainer undefined) @?= fullSegment
  -- typical
  -- edge cases
