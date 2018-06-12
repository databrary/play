{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Factories where

-- import Data.Text (Text)
-- import Data.Time
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
-- import Test.Tasty.HUnit

import Databrary.Model.Age

----- general utilities ------


----- value objects ------

genAge :: Gen Age
genAge =
  let maxAgeTypicallyStudied = 14
  in Age <$> Gen.integral (Range.constant 0 (maxAgeTypicallyStudied*365))


----- entities ------ 

