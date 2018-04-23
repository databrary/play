{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.AssetTest where

import Test.Tasty.HUnit

import Databrary.Model.Asset

unit_assetBacked :: Assertion
unit_assetBacked =
  -- example
  (not . assetBacked) (blankAsset undefined) @? "asset without a content hash isn't backed"
  -- typical
  -- edge cases
