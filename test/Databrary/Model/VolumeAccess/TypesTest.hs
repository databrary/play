{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.VolumeAccess.TypesTest where

-- import qualified Data.ByteString as BS
-- import Data.Time (fromGregorian)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
-- import Test.Tasty

import Databrary.Model.Party.Types
import Databrary.Model.Party
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.VolumeAccess.Types

genGroupPermission :: Party -> Gen (Permission, Maybe Bool)
genGroupPermission p
  | partyRow p == partyRow rootParty =
        Gen.element [(PermissionNONE, Nothing), (PermissionSHARED, Just False), (PermissionSHARED, Just True)]
  | partyRow p == partyRow nobodyParty =
        Gen.element [(PermissionNONE, Nothing), (PermissionPUBLIC, Just False), (PermissionPUBLIC, Just True)]
  | otherwise = error "only known group parties that should get volume access are root party and nobody party"

genGroupVolumeAccess :: Maybe Party -> Volume -> Gen VolumeAccess
genGroupVolumeAccess mGroup vol = do
    group <- maybe (Gen.element [nobodyParty, rootParty]) pure mGroup
    (perm, mShareFull) <- genGroupPermission group
    VolumeAccess
       <$> pure perm
       <*> pure perm
       <*> Gen.maybe (Gen.integral (Range.constant 1 20)) -- TODO: what does this field mean?
       <*> pure mShareFull
       <*> pure group
       <*> pure vol

{-
-- TODO: expand these to really generate random measure values
genBirthdateMeasure :: Gen (Metric, BS.ByteString)
genBirthdateMeasure =
    pure (participantMetricBirthdate, "1990-01-02")

genGenderMeasure :: Gen (Metric, BS.ByteString)
genGenderMeasure =
    pure (participantMetricGender, "Male")

genParticipantMetricValue :: Gen (Metric, BS.ByteString)
genParticipantMetricValue =
    Gen.choice [genBirthdateMeasure, genGenderMeasure]

genCreateMeasure :: Gen Measure
genCreateMeasure = do
    (mtrc, val) <- genParticipantMetricValue
    Measure
        <$> (pure . error) "measure record not set yet"
        <*> pure mtrc
        <*> pure val

genCategory :: Gen Category
genCategory = Gen.element allCategories

genCreateRecord :: Volume -> Gen Record
genCreateRecord vol = do
    -- repeats some logic from blankRecord
    Record
        <$> (RecordRow <$> (pure . error) "Id set after saved" <*> genCategory)
        <*> pure []
        <*> Gen.maybe Gen.enumBounded
        <*> pure vol
-}
