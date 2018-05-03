{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.OffsetTest where

import Data.Aeson
import Data.Aeson.Types
import Data.Fixed
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
-- import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Offset

-- offsetMillis :

-- diffTimeOffset

-- offsetDiffTime

-- Show
-- Read
unit_readOffset :: Assertion
unit_readOffset = do
    -- example
    let offset = "11:22:33.0"
    (show . (read :: String -> Offset)) offset @?= offset

unit_toJSON_Offset :: Assertion
unit_toJSON_Offset =
    -- example
    encode (Offset 90.123) @?= "90123"

unit_parseJSON_Offset :: Assertion
unit_parseJSON_Offset =
    -- example
    parseEither parseJSON (Number 1234) @?= (Right . Offset) 1.234

genOffset :: Milli -> Gen Offset
genOffset totalLength =
    Offset <$> Gen.realFrac_ (Range.constant 0 totalLength)
