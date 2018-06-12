{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.GeoNamesTest where

import Data.Aeson
import Data.Aeson.Types
-- import Hedgehog
-- import Hedgehog.Gen as Gen
-- import Hedgehog.Range as Range
-- import Test.Tasty
import Test.Tasty.HUnit

import Databrary.HTTP.Client
import Databrary.Model.GeoNames
import Databrary.Model.Id

unit_parseGeoNameRef :: Assertion
unit_parseGeoNameRef = do
    -- example
    parseGeoNameRef "http://sws.geonames.org/3" @?= Just (Id 3)

unit_parseGeoName :: Assertion
unit_parseGeoName =
    -- example
    parseEither
        parseGeoName
        (object
           [("geonameId", toJSON (6252001 :: Int)), ("name", "United States")])
        @?= Right geoNameUS

-- not run so as not to tax the service unnecessarily; TODO: change this to ignoreTest
_unit_lookupGeoName :: Assertion
_unit_lookupGeoName = do
    -- example
    hc <- initHTTPClient
    mGeo <- lookupGeoName (Id 6252001) hc
    mGeo @?= Just geoNameUS
