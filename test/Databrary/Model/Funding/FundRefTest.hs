{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Funding.FundRefTest where

import Test.Tasty.HUnit

import Databrary.Model.Funding.FundRef
import Databrary.Model.Funding.Types
import Databrary.Model.Id.Types

funder1 :: Funder
funder1 = Funder { funderId = Id 1, funderName = "fund1" }

unit_annotateFunder :: Assertion
unit_annotateFunder =
  -- example
  annotateFunder funder1 ["a", "b"] (Just "c") @?= funder1 { funderName = "fund1 (a, b), c" }
  -- typical
  -- edge cases
