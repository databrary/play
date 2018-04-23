{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.GeoNamesTest where

-- import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.GeoNames
import Databrary.Model.Id

unit_parseGeoNameRef_example :: Assertion
unit_parseGeoNameRef_example = do
    parseGeoNameRef "http://sws.geonames.org/3" @?= Just (Id 3)
