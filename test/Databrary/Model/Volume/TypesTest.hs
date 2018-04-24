{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Volume.TypesTest where

import Data.Maybe (fromMaybe)
import Data.Semigroup
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

genVolumeRowSimple :: Gen VolumeRow
genVolumeRowSimple =
    VolumeRow
        <$> (Id <$> Gen.integral (Range.constant 1 10000))
        <*> Gen.text (Range.constant 0 200) Gen.alphaNum
        <*> (Just <$> Gen.text (Range.constant 0 300) Gen.alphaNum)
        <*> (Just <$> Gen.text (Range.constant 0 60) Gen.alphaNum)
        <*> pure Nothing

genVolumeOwner :: Gen VolumeOwner
genVolumeOwner = do
    pr <- partyRow <$> genPartySimple
    pure (partyId pr, partySortName pr <> ", " <> (fromMaybe "" . partyPreName) pr)

genVolumeSimple :: Gen Volume
genVolumeSimple =
    Volume
        <$> genVolumeRowSimple
        <*> (UTCTime
                 <$> (fromGregorian
                          <$> Gen.integral (Range.constant 2000 2018)
                          <*> Gen.integral (Range.constant 1 12)
                          <*> Gen.integral (Range.constant 1 28))
                 <*> (secondsToDiffTime <$> Gen.integral (Range.constant 0 86399)))
        <*> Gen.list (Range.constant 1 3) genVolumeOwner
        <*> pure PermissionPUBLIC
        <*> pure PermLevelDefault
