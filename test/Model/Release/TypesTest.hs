{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.Release.TypesTest where

-- import Data.Monoid
import Test.Tasty.HUnit

import Model.Release.Types

unit_releaseMonoid :: Assertion
unit_releaseMonoid =
  -- example
  ReleasePRIVATE `mappend` ReleasePUBLIC @?= ReleasePUBLIC
  -- typical
  -- edge cases

