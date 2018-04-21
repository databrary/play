{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.OffsetTest where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Offset

unit_readOffset_example :: Assertion
unit_readOffset_example = do
    let offset = "11:22:33.0"
    (show . (read :: String -> Offset)) offset @?= offset
