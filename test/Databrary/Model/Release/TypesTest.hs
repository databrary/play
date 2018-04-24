{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Release.TypesTest where

import Data.Monoid
import Test.Tasty.HUnit

import Databrary.Model.Release.Types

unit_releaseMonoid :: Assertion
unit_releaseMonoid =
  -- example
  ReleasePRIVATE `mappend` ReleasePUBLIC @?= ReleasePUBLIC
  -- typical
  -- edge cases

