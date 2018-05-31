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
