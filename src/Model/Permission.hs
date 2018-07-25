{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Permission
  ( module Model.Permission.Types
  -- , permissionVIEW
  , permissionPRIVATE
  , readPermission
  , readRelease
  , dataPermission4
  , canReadData2
  , accessJSON
  -- * Checking permissioned objects
  , checkPermission
  , PermissionResponse (..)
  -- * New
  , requestAccess
  , Permissioned
  , mkPermissioned
  ) where

import Data.Monoid ((<>))

import qualified JSON
import Model.Release.Types
import Model.Permission.Types

-- | Represents a permissioned object. The constructor is not exported: use
-- 'mkPermissioned' and 'requestAccess' instead.
data Permissioned a = Permissioned
    { unsafeAccess :: a
    , grantedPermission :: Permission
    }

-- | Smart constructor for Permissioned.
--
-- As one can tell from the first argument, this assumes that objects already
-- have some way of being mapped to the permissions granted on them. This is
-- generally true because of how the existing code works. It might change in the
-- future, for example if database queries return a 'Permissioned' value
-- directly, obsoleting this function.
mkPermissioned :: (a -> Permission) -> a -> Permissioned a
mkPermissioned getPerm o = Permissioned o (getPerm o)

-- | How to get access to a permissioned object. It's not a great design, but it
-- makes a concrete concept out of an existing pattern in the codebase. A better
-- design could perhaps couple the access request to the action that needs the
-- access.
requestAccess
    :: Permission
    -- ^ Requested permission
    -> Permissioned a
    -- ^ object
    -> Maybe a
    -- ^ Maybe the unwrapped object
requestAccess requestedPerm obj =
    if requestedPerm <= grantedPermission obj
    then Just (unsafeAccess obj)
    else Nothing

-- |Level at which things become visible. ; TODO: use this somewhere?
-- permissionVIEW :: Permission
-- permissionVIEW = PermissionPUBLIC

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
releasePermission effectiveReleaseOnData currentUserAllowedPermissionOnVolume
  | currentUserAllowedPermissionOnVolume >= readPermission effectiveReleaseOnData = currentUserAllowedPermissionOnVolume
  | otherwise = PermissionNONE

dataPermission4 :: (a -> EffectiveRelease) -> (a -> VolumeRolePolicy) -> a -> Permission
dataPermission4 getObjEffectiveRelease getCurrentUserVolumeRole obj =
  let
   effRelease = getObjEffectiveRelease obj
  in
    case getCurrentUserVolumeRole obj of
      RolePublicViewer PublicRestrictedPolicy ->
        releasePermission (effRelPrivate effRelease) PermissionPUBLIC
      RoleSharedViewer SharedRestrictedPolicy ->
        releasePermission (effRelPrivate effRelease) PermissionSHARED
      -- other levels that behave more like private (options: none, shared, read, edit, admin) ?
      rp ->
         releasePermission (effRelPublic effRelease) (extractPermissionIgnorePolicy rp)

canReadData2 :: (a -> EffectiveRelease) -> (a -> VolumeRolePolicy) -> a -> Bool
canReadData2 getObjEffectiveRelease getCurrentUserVolumeRole obj =
  dataPermission4 getObjEffectiveRelease getCurrentUserVolumeRole obj > PermissionNONE

accessJSON :: JSON.ToObject o => Access -> o
accessJSON Access{..} =
     "site" JSON..= accessSite'
  <> "member" JSON..= accessMember'

-- | Responses to 'checkPermission'
data PermissionResponse a
    = PermissionGranted a
    -- ^ Whatever you wanted, you got it!
    | PermissionDenied
    -- ^ No.

-- | Decorate some permissioned object with a permission response
-- TODO: Wouldn't it be great if this had type
-- @@Permissioned a -> Permission -> PermissionResponse a@@ ?
--
-- NB: This clashes with 'Controller.Permission.checkPermission', which it
-- should replace.
checkPermission
    :: (a -> Permission) -- ^ Extract the object's permission rules
    -> a -- ^ The object in question
    -> Permission -- ^ The requested permission
    -> PermissionResponse a
    -- ^ The object decorated with the permission response
checkPermission getGrantedPerms obj requestedPerms =
    case compare (getGrantedPerms obj) requestedPerms of
        LT -> PermissionDenied
        GT -> PermissionGranted obj
        EQ -> PermissionGranted obj
