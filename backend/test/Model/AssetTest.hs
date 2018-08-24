{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.AssetTest where

import Test.Tasty.HUnit

import Model.Asset

unit_assetBacked :: Assertion
unit_assetBacked =
  -- example
  (not . assetBacked) (blankAsset undefined) @? "asset without a content hash isn't backed"
  -- typical
  -- edge cases
