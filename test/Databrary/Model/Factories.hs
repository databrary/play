{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Factories where

import Data.Fixed
-- import Data.Text (Text)
-- import Data.Time
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
-- import Test.Tasty.HUnit

import Databrary.Model.Age
import Databrary.Model.Offset

----- general utilities ------


----- value objects ------

-- id
-- release
-- permission
-- orcid
-- metric
-- category
-- format
-- url
-- ...

-- offset
genOffset :: Milli -> Gen Offset
genOffset totalLength =
    Offset <$> Gen.realFrac_ (Range.constant 0 totalLength)

-- segment
genAge :: Gen Age
genAge =
  let maxAgeTypicallyStudied = 14
  in Age <$> Gen.integral (Range.constant 0 (maxAgeTypicallyStudied*365))

----- entities ------ 

-- party
-- identity
-- token
-- authorize

-- volume
-- vol acc

-- container
-- slot

-- asset
-- assetslot
-- assetsegment
-- assetrevision
-- excerpt
-- transcode

-- vol metric
-- record
-- measure
-- recordslot

-- citation
-- funding

-- notification
-- audit
-- ingest
-- vol state
-- activity
-- stats
-- comment, tag
