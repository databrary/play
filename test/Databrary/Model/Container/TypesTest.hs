{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Container.TypesTest where

import Test.Tasty.HUnit

import Databrary.Model.Container
import Databrary.Model.Release.Types

unit_getContainerRelease :: Assertion
unit_getContainerRelease =
  -- example
  (effRelPrivate . getContainerRelease) (blankContainer undefined) @?= ReleasePRIVATE
  -- typical
  -- edge cases
