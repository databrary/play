{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Databrary.Model.PermissionTest where

-- import Test.Tasty
import Test.Tasty.HUnit

import Databrary.Model.Identity.Types
import Databrary.Model.Permission
import Databrary.Model.Release.Types
import TestHarness

unit_readRelease_example :: Assertion
unit_readRelease_example = do
    readRelease PermissionNONE @?= Nothing

-- working up towards eventual pure tests of checkMemberAdmin
unit_accessMember :: Assertion
unit_accessMember = do
    -- example
    accessMember (TestContext { ctxIdentity = Just NotLoggedIn }) @?= PermissionNONE
    -- typical
    -- accessMember (TestContext { ctxIdentity = mkSession ... }) @?= PermissionNONE
    -- edge case
    accessMember (TestContext { ctxIdentity = Just IdentityNotNeeded }) @?= PermissionNONE

runDataPermission4 :: (Release, Release) -> VolumeRolePolicy -> Permission
runDataPermission4 (relPub, relPriv) rolePolicy =
  dataPermission4 (const (EffectiveRelease {effRelPublic = relPub, effRelPrivate = relPriv})) (const rolePolicy) ()

-- <volume shared level>AS<current user's access to that volume>
fullySharedAsPublic, partiallySharedAsPublic, fullyOrPartiallyOrDatabrarySharedAsDatabraryMember :: VolumeRolePolicy
privateOrDatabrarySharedAsPublic, privateAsDatabraryMember :: VolumeRolePolicy
privateAsVolumeAffiliate, privateAsOwner :: VolumeRolePolicy
fullySharedAsPublic = RolePublicViewer PublicNoPolicy
partiallySharedAsPublic = RolePublicViewer PublicRestrictedPolicy
fullyOrPartiallyOrDatabrarySharedAsDatabraryMember = RoleSharedViewer SharedNoPolicy
privateOrDatabrarySharedAsPublic = RoleNone
privateAsDatabraryMember = RoleNone
privateAsVolumeAffiliate = RoleEditor
privateAsOwner = RoleAdmin

publiclyReleasedExcerpt :: (Release, Release)
publiclyReleasedExcerpt = (ReleasePUBLIC, ReleasePUBLIC)

publiclyReleasedAsset, databraryReleasedAssetOrExcerpt, excerptsReleasedAssetOrExcerpt, privatelyReleasedAssetOrExcerpt :: (Release, Release)
publiclyReleasedAsset = (ReleasePUBLIC, ReleasePRIVATE)
excerptsReleasedAssetOrExcerpt = (ReleaseEXCERPTS, ReleasePRIVATE)  -- is it possible to set an excerpt to "excerpts' release level?
databraryReleasedAssetOrExcerpt = (ReleaseSHARED, ReleasePRIVATE)  -- is this shared or private?
privatelyReleasedAssetOrExcerpt = (ReleasePRIVATE, ReleasePRIVATE)

-- TODO: use more idiomatic hunit
unit_dataPermission4_combinations :: Assertion
unit_dataPermission4_combinations = do
  runDataPermission4 publiclyReleasedExcerpt fullySharedAsPublic @?= PermissionPUBLIC
  runDataPermission4 publiclyReleasedExcerpt partiallySharedAsPublic @?= PermissionPUBLIC
  runDataPermission4 publiclyReleasedExcerpt fullyOrPartiallyOrDatabrarySharedAsDatabraryMember @?= PermissionSHARED

  runDataPermission4 publiclyReleasedAsset fullySharedAsPublic @?= PermissionPUBLIC
  runDataPermission4 publiclyReleasedAsset partiallySharedAsPublic @?= PermissionNONE
  runDataPermission4 publiclyReleasedAsset fullyOrPartiallyOrDatabrarySharedAsDatabraryMember @?= PermissionSHARED

  runDataPermission4 databraryReleasedAssetOrExcerpt fullySharedAsPublic @?= PermissionNONE
  runDataPermission4 databraryReleasedAssetOrExcerpt fullyOrPartiallyOrDatabrarySharedAsDatabraryMember @?= PermissionSHARED

  runDataPermission4 excerptsReleasedAssetOrExcerpt fullySharedAsPublic @?= PermissionNONE -- because perm public < perm shared
  runDataPermission4 excerptsReleasedAssetOrExcerpt partiallySharedAsPublic @?= PermissionNONE
  runDataPermission4 excerptsReleasedAssetOrExcerpt fullyOrPartiallyOrDatabrarySharedAsDatabraryMember @?= PermissionSHARED

  runDataPermission4 privatelyReleasedAssetOrExcerpt fullySharedAsPublic @?= PermissionNONE
  runDataPermission4 privatelyReleasedAssetOrExcerpt partiallySharedAsPublic @?= PermissionNONE
  runDataPermission4 privatelyReleasedAssetOrExcerpt fullyOrPartiallyOrDatabrarySharedAsDatabraryMember @?= PermissionNONE
  runDataPermission4 privatelyReleasedAssetOrExcerpt privateOrDatabrarySharedAsPublic @?= PermissionNONE
  runDataPermission4 privatelyReleasedAssetOrExcerpt privateAsDatabraryMember @?= PermissionNONE
  runDataPermission4 privatelyReleasedAssetOrExcerpt privateAsVolumeAffiliate @?= PermissionEDIT
  runDataPermission4 privatelyReleasedAssetOrExcerpt privateAsOwner @?= PermissionADMIN
