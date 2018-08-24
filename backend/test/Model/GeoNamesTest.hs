{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Model.GeoNamesTest where

import Data.Aeson
import Data.Aeson.Types
-- import Hedgehog
-- import Hedgehog.Gen as Gen
-- import Hedgehog.Range as Range
-- import Test.Tasty
import Test.Tasty.HUnit

import HTTP.Client
import Model.GeoNames
import Model.Id
import Model.TypeOrphans ()

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

-- not run so that we don't encounter rate limits
_unit_lookupGeoName :: Assertion
_unit_lookupGeoName = do
    -- example
    hc <- initHTTPClient
    mGeo <- lookupGeoName (Id 6252001) hc
    case mGeo of
      Nothing -> pure () -- if internet connection is not live. TODO: make error more precise
      Just geo -> geo @?= geoNameUS
