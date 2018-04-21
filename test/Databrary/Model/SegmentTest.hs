{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.SegmentTest where

-- import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Segment

unit_segmentIntersect_example :: Assertion
unit_segmentIntersect_example = do
    segmentIntersect fullSegment emptySegment @?= emptySegment
