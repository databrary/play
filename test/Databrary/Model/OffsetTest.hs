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

unit_Offset_examples :: Assertion
unit_Offset_examples = do
    diffTimeOffset 12.345678 @?= Offset 12.345
    offsetDiffTime (Offset 12.345) @?= 12.345
    offsetMillis (Offset 1.234) @?= 1234
    encode (Offset 90.123) @?= "90123"
    parseEither parseJSON (Number 1234) @?= (Right . Offset) 1.234

-- TOOD: property difftimeoffset . offsetDifftime o === o

unit_Offset_readshow_inverts :: Assertion
unit_Offset_readshow_inverts = do
    let offset = "11:22:33.0"
    (show . (read :: String -> Offset)) offset @?= offset

genOffset :: Milli -> Gen Offset
genOffset totalLength =
    Offset <$> Gen.realFrac_ (Range.constant 0 totalLength)
