module Model.TimeTest
where

import Data.Aeson
import Data.Time
-- import Hedgehog
-- import Hedgehog.Gen as Gen
-- import Hedgehog.Range as Range
-- import Test.Tasty
import Test.Tasty.HUnit

import Model.Time

unit_dateYear :: Assertion
unit_dateYear =
    -- example
    dateYear (fromGregorian 2017 1 2) @?= 2017

unit_maskDateIf :: Assertion
unit_maskDateIf = do
   -- example
   maskedYear (maskDateIf True (fromGregorian 2017 1 2)) @?= 2017

-- potential properties:
-- formattime on a masked date will not show month or day
-- formatTime on unmasked date will show everything
-- all dates will show at least the year

unit_formatTime_MaskedDate :: Assertion
unit_formatTime_MaskedDate = do
    -- example
    formatTime defaultTimeLocale "%F" (maskDateIf True (fromGregorian 2017 1 2)) @?= "2017-XX-XX"

unit_toJSON_MaskedDate :: Assertion
unit_toJSON_MaskedDate = do
    -- example
    encode (maskDateIf True (fromGregorian 2017 1 2)) @?= "2017"
    -- typical
    encode (maskDateIf False (fromGregorian 2017 1 2)) @?= "\"2017-01-02\""


