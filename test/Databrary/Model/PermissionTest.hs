{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.PermissionTest where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Permission

unit_readRelease_example :: Assertion
unit_readRelease_example = do
    readRelease PermissionNONE @?= Nothing
