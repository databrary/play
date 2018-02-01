{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission
  ( module Databrary.Model.Permission.Types
  , permissionVIEW
  , permissionPRIVATE
  , readPermission
  , readRelease
  , dataPermission3
  , accessJSON
  -- testing only
  , testdataPermission3
  ) where

import Data.Monoid ((<>))

import Databrary.Has (Has, view)
import qualified Databrary.JSON as JSON
import Databrary.Model.Release.Types
import Databrary.Model.Permission.Types

-- |Level at which things become visible.
permissionVIEW :: Permission
permissionVIEW = PermissionPUBLIC

-- |Alias for READ. Grants full access to private data, bypassing consent permissions.
permissionPRIVATE :: Permission
permissionPRIVATE = PermissionREAD

-- |The necessary permission level to read a data object with the given release.
-- Equivalent to the SQL function read_permission.
readPermission :: Release -> Permission
readPermission ReleasePUBLIC   = PermissionPUBLIC
readPermission ReleaseSHARED   = PermissionSHARED
readPermission ReleaseEXCERPTS = PermissionSHARED
readPermission ReleasePRIVATE  = permissionPRIVATE

-- |The most restrictive data release level that the current user may access under the given permission.
-- Equivalent to the SQL function read_release.  Inverse of 'readPermission' module meaning of @Nothing@.
readRelease :: Permission -> Maybe Release
readRelease PermissionNONE   = Nothing
readRelease PermissionPUBLIC = Just ReleasePUBLIC
readRelease PermissionSHARED = Just ReleaseSHARED
readRelease _                = Just ReleasePRIVATE

-- |The effective permission for data objects with the given attributes, effectively collapsing ineffective permissions NONE.
releasePermission :: Release -> Permission -> Permission
releasePermission r p
  | p >= readPermission r = p
  | otherwise = PermissionNONE

-- START future testing code
runDataPermission3 :: (Release, Release) -> (Permission, VolumeAccessPolicy) -> Permission
runDataPermission3 (relPub, relPriv) (perm, policy) =
  dataPermission3 (const (EffectiveRelease {effRelPublic = relPub, effRelPrivate = relPriv})) (const (perm, policy)) ()

partiallySharedAsPublic, fullySharedAsPublic, databrarySharedAsDatabraryMember :: (Permission, VolumeAccessPolicy)
partiallySharedAsPublic = (PermissionPUBLIC, PublicRestricted)
fullySharedAsPublic = (PermissionPUBLIC, PermLevelDefault)
databrarySharedAsDatabraryMember = (PermissionSHARED, PermLevelDefault)

publiclyReleasedExcerpt, databrarySharedExcerpt :: (Release, Release)
publiclyReleasedExcerpt = (ReleasePUBLIC, ReleasePUBLIC)
databrarySharedExcerpt = (ReleaseSHARED, ReleasePRIVATE)
publiclyReleasedAsset = (ReleasePUBLIC, ReleasePRIVATE)
privatelyReleasedAsset = (ReleasePRIVATE, ReleasePRIVATE)

testdataPermission3 =
  [ (runDataPermission3 publiclyReleasedExcerpt partiallySharedAsPublic, PermissionPUBLIC)
  , (runDataPermission3 publiclyReleasedExcerpt fullySharedAsPublic, PermissionPUBLIC)
  , (runDataPermission3 publiclyReleasedAsset partiallySharedAsPublic, PermissionNONE)
  , (runDataPermission3 publiclyReleasedAsset fullySharedAsPublic, PermissionPUBLIC)
  , (runDataPermission3 publiclyReleasedExcerpt databrarySharedAsDatabraryMember, PermissionSHARED)
  , (runDataPermission3 databrarySharedExcerpt databrarySharedAsDatabraryMember, PermissionSHARED)
  , (runDataPermission3 databrarySharedExcerpt databrarySharedAsDatabraryMember, PermissionSHARED)
  , (runDataPermission3 privatelyReleasedAsset databrarySharedAsDatabraryMember, PermissionNONE)
  , (runDataPermission3 privatelyReleasedAsset partiallySharedAsPublic, PermissionNONE)
  , (runDataPermission3 privatelyReleasedAsset fullySharedAsPublic, PermissionNONE)
  ]
-- END future testing code


dataPermission3 :: (a -> EffectiveRelease) -> (a -> (Permission, VolumeAccessPolicy)) -> a -> Permission
dataPermission3 getObjEffectiveRelease getCurrentUserPermLevel obj =
  let
   effRelease = getObjEffectiveRelease obj
  in 
    case getCurrentUserPermLevel obj of
      (p@PermissionPUBLIC, PublicRestricted) -> releasePermission (effRelPrivate effRelease) p
      -- other levels that behave more like private (options: none, shared, read, edit, admin) ? 
      (p, _) -> releasePermission (effRelPublic effRelease) p

accessJSON :: JSON.ToObject o => Access -> o
accessJSON Access{..} =
     "site" JSON..= accessSite'
  <> "member" JSON..= accessMember'
