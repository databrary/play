{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.ORCIDTest where

import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.ORCID

unit_readORCID_example :: Assertion
unit_readORCID_example = do
    (show . (read :: String -> ORCID)) "http://orcid.org/0000-0001-2345-6789" @?= "0000-0001-2345-6789"
