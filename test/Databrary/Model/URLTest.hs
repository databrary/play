{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.URLTest where

import Test.Tasty
import Test.Tasty.HUnit
-- import Data.Time (fromGregorian, secondsToDiffTime)

import Databrary.Model.URL

unit_validHDL_example :: Assertion
unit_validHDL_example = do
    not (validHDL "a") @? "HDL should start with a digit"
