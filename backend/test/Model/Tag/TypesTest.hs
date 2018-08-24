{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.Tag.TypesTest where

import Test.Tasty.HUnit

import Model.Tag.Types
import Model.TypeOrphans ()

unit_validateTag :: Assertion
unit_validateTag =
  -- example
  -- typical
  -- edge cases
  validateTag "efg123" @?= Nothing
