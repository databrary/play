{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.Container.TypesTest where

import Data.Text (Text)
import Data.Time
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty.HUnit

import Databrary.Model.Container
-- import Databrary.Model.Permission.Types
-- import Databrary.Model.Party.Types
-- import Databrary.Model.Party.TypesTest
import Databrary.Model.Release.Types
import Databrary.Model.Id.Types
-- import Databrary.Model.Volume.Types
import Databrary.Model.Volume.TypesTest

genContainerTestDay :: Gen Day
genContainerTestDay =
    fromGregorian
                 <$> Gen.integral (Range.constant 2000 2018)
                 <*> Gen.integral (Range.constant 1 12)
                 <*> Gen.integral (Range.constant 1 28)

genContainerName :: Gen Text
genContainerName = Gen.text (Range.constant 0 80) Gen.alphaNum

genContainerRowSimple :: Gen ContainerRow
genContainerRowSimple =
    -- TODO: differentiate between session and container generator
    ContainerRow
        <$> (Id <$> Gen.integral (Range.constant 1 20000))
        <*> Gen.bool
        <*> (Just <$> genContainerName)
        <*> (Just <$> genContainerTestDay)

genContainerSimple :: Gen Container
genContainerSimple =
    Container
        <$> genContainerRowSimple
        <*> (pure . Just) ReleaseSHARED
        <*> genVolumeSimple

genCreateContainerRow :: Gen ContainerRow
genCreateContainerRow =
   -- some redundancy with blankContainer
    ContainerRow
        <$> (pure . error) "container id created after save"
        <*> pure False -- Gen.bool -- TODO: when is top valid?
        <*> Gen.maybe genContainerName
        <*> Gen.maybe genContainerTestDay

genCreateContainer :: Gen Container
genCreateContainer =
   -- some redundancy with blankContainer
   Container
        <$> genCreateContainerRow
        <*> Gen.maybe Gen.enumBounded
        <*> (pure . error) "container volume not specified"

unit_getContainerRelease :: Assertion
unit_getContainerRelease =
  -- example
  (effRelPrivate . getContainerRelease) (blankContainer undefined) @?= ReleasePRIVATE
  -- typical
  -- edge cases
