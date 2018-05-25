{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Volume.TypesTest where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Semigroup
import qualified Data.Text as T
import Data.Time
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
-- import Test.Tasty
-- import Test.Tasty.Hedgehog

import Databrary.Model.Permission.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.TypesTest
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types

genVolumeId :: Gen (Id Volume)
genVolumeId = Id <$> Gen.integral (Range.constant 1 10000)

genVolumeName :: Gen T.Text  -- Verify this and next two with real data profile
genVolumeName = Gen.text (Range.constant 0 200) Gen.alphaNum

genVolumeBody :: Gen T.Text
genVolumeBody = Gen.text (Range.constant 0 300) Gen.alphaNum

genVolumeAlias :: Gen T.Text
genVolumeAlias = Gen.text (Range.constant 0 60) Gen.alphaNum

genVolumeDOI :: Gen BS.ByteString
genVolumeDOI = pure "10.17910/B7159Q" -- TODO: good generator for this?

genVolumeRowSimple :: Gen VolumeRow
genVolumeRowSimple =
    VolumeRow
        <$> genVolumeId
        <*> genVolumeName
        <*> Gen.maybe genVolumeBody
        <*> Gen.maybe genVolumeAlias
        <*> Gen.maybe genVolumeDOI

genVolumeOwner :: Gen VolumeOwner
genVolumeOwner = do
    pr <- partyRow <$> genPartySimple
    pure (partyId pr, partySortName pr <> ", " <> (fromMaybe "" . partyPreName) pr)

genVolumeCreationTime :: Gen UTCTime
genVolumeCreationTime =
    UTCTime
          <$> (fromGregorian
                   <$> Gen.integral (Range.constant 2000 2018)
                   <*> Gen.integral (Range.constant 1 12)
                   <*> Gen.integral (Range.constant 1 28))
          <*> (secondsToDiffTime <$> Gen.integral (Range.constant 0 86399))

genVolumeRolePolicy :: Gen VolumeRolePolicy
genVolumeRolePolicy = do
    perm <- Gen.enumBounded
    mShareFull <- Gen.maybe (Gen.bool)
    pure (volumeAccessPolicyWithDefault perm mShareFull)

genVolumeSimple :: Gen Volume
genVolumeSimple = do
    Volume
        <$> genVolumeRowSimple
        <*> genVolumeCreationTime
        <*> Gen.list (Range.constant 1 3) genVolumeOwner
        <*> genVolumeRolePolicy
