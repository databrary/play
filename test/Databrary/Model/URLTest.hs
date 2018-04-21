{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.URLTest where

import Network.URI
import Test.Tasty
import Test.Tasty.HUnit
-- import Data.Time (fromGregorian, secondsToDiffTime)

import Databrary.Model.URL

unit_validHDL_example :: Assertion
unit_validHDL_example = do
    validHDL "120.1/" @? "[0-9]+(.[0-9]+)+/.+ is valid"

unit_validHDL_typical :: Assertion
unit_validHDL_typical = do
    not (validHDL "a") @? "HDL should start with a digit"

unit_validHDL_corner :: Assertion
unit_validHDL_corner = do
    validHDL "0.0.0/" @? "digits and dots ending with slash is okay"

unit_parseURL_example :: Assertion
unit_parseURL_example = do
    parseURL "http://google.com" @?= parseURI "http://google.com"
