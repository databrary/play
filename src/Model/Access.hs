-- | A collection of access request methods.
module Model.Access
    ( accessSlot
    , accessVolume
    , AccessResult (..)
    ) where

import Model.Container.Types
import Model.Id.Types
import Model.Identity.Types
import Model.Permission
import Model.Slot
import Model.Volume
import Service.DB

-- | Captures possible request responses.
-- NOTE: This was designed to mimic existing code and responses. LookupFailed
-- does NOT mean "does not exist". It means that 'lookupVolume' (for example)
-- returned Nothing. This could mean either the id is a valid id, or the user
-- doesn't have access to the volume.
--
-- TODO: Monad Transformer?
data AccessResult a
    = LookupFailed
    | AccessDenied
    | AccessResult a

-- | Lookup a Slot by its Id, requesting the given permission.
accessSlot
    :: (MonadDB c m, MonadHasIdentity c m)
    => Permission
    -> Id Slot
    -> m (AccessResult Slot)
accessSlot requestedPerm = accessPermissionedObject
    lookupSlot
    (extractPermissionIgnorePolicy
    . volumeRolePolicy
    . containerVolume
    . slotContainer
    )
    requestedPerm

-- | Lookup a Volume by its Id, requesting the given permission.
accessVolume
    :: (MonadDB c m, MonadHasIdentity c m)
    => Permission
    -> Id Volume
    -> m (AccessResult Volume)
accessVolume requestedPerm = accessPermissionedObject
    lookupVolume
    (extractPermissionIgnorePolicy . volumeRolePolicy)
    requestedPerm

-- | Internal, generic version for accessing a permissioned object. Used as the
-- basis for the exported accessors.
accessPermissionedObject
    :: MonadDB c m
    => (Id a -> m (Maybe a))
    -- ^ How to get the object from the database
    -> (a -> Permission)
    -- ^ Map the object to the permissions granted on it
    -> Permission
    -- ^ Requested access level to the object
    -> Id a
    -- ^ Id of the object to access
    -> m (AccessResult a)
    -- ^ Access response
accessPermissionedObject lookupObj getPermission requestedPerm =
    fmap (maybe LookupFailed mkRequest) . lookupObjP
  where
    mkRequest =
        maybe AccessDenied AccessResult . requestAccess requestedPerm
    lookupObjP = fmap (fmap (mkPermissioned getPermission)) . lookupObj
