{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.VolumeTest where

import Test.Tasty.HUnit

import Databrary.Model.Volume

unit_getVolumeAlias :: Assertion
unit_getVolumeAlias =
    -- example
    getVolumeAlias blankVolume @?= Nothing
    -- typical
    -- edge cases
