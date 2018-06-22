{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.Funding.FundRefTest where

import Test.Tasty.HUnit

import Model.Funding.FundRef
import Model.Funding.Types
import Model.Id.Types
import Model.TypeOrphans ()

funder1 :: Funder
funder1 = Funder { funderId = Id 1, funderName = "fund1" }

unit_annotateFunder :: Assertion
unit_annotateFunder =
  -- example
  annotateFunder funder1 ["a", "b"] (Just "c") @?= funder1 { funderName = "fund1 (a, b), c" }
  -- typical
  -- edge cases
