{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Factories where

import Data.Maybe
import Data.Fixed
import Data.Monoid ((<>))
-- import Data.Text (Text)
import Data.Time
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.URI
-- import Test.Tasty.HUnit

import Databrary.Model.Age
import Databrary.Model.Format
import Databrary.Model.Offset
import Databrary.Model.Time

----- general utilities ------


----- value objects ------

-- id -- usually have db generate db
-- date, maskeddate
genDate :: Gen Date
genDate =
      fromGregorian
          <$> Gen.integral (Range.constant 1990 2015)
          <*> Gen.integral (Range.constant 1 12)
          <*> Gen.integral (Range.constant 1 28)

genMaskedDate :: Gen MaskedDate
genMaskedDate = do
    dt <- genDate
    mask <- Gen.bool
    pure (maskDateIf mask dt)

-- release
-- permission
-- orcid
-- url
----- gen doi value
----- gen hdl value
----- gen doi url
----- gen hdl url
genGeneralURI :: Gen URI
genGeneralURI = do
    domain <- Gen.string (Range.constant 1 20) Gen.alphaNum
    pathSeg1 <- Gen.string (Range.constant 1 20) Gen.alphaNum -- TODO: generate multiple segs, allowed chars?
    scheme1 <- Gen.element ["http", "https"]
    pure
        (nullURI {
              uriScheme = scheme1
            , uriAuthority = Just (URIAuth "" ("www." <> domain <> ".com") "") -- TODO: choose on prefix and suffix?
            , uriPath = "/" <> pathSeg1
            })

-- offset
genOffset :: Milli -> Gen Offset
genOffset totalLength =
    Offset <$> Gen.realFrac_ (Range.constant 0 totalLength)
-- segment
genAge :: Gen Age
genAge =
  let maxAgeTypicallyStudied = 14
  in Age <$> Gen.integral (Range.constant 0 (maxAgeTypicallyStudied*365))
-- format
genFormat :: Gen Format
genFormat = Gen.element allFormats

genAVFormat :: Gen Format
genAVFormat = Gen.element (filter formatIsAV allFormats)

genNotAVFormat :: Gen Format
genNotAVFormat = Gen.element (filter formatNotAV allFormats)

genTranscodeOutputFormat :: Gen Format
genTranscodeOutputFormat = Gen.element (catMaybes (fmap formatTranscodable allFormats))
-- metric
-- category
-- ...

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
