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
-- TODO: Monad Transformer
data AccessResult a
    = LookupFailed
    | AccessDenied
    | AccessResult a

-- | Lookup a Slot by its Id, requesting the given permission.
--
-- NOTE: Intentionally implemented exactly like accessVolume. Implementations
-- should be collected in a single module and merged.
accessSlot
    :: (MonadDB c m, MonadHasIdentity c m)
    => Permission
    -> Id Slot
    -> m (AccessResult Slot)
accessSlot requestedPerm = fmap (maybe LookupFailed mkRequest) . lookupSlotP
  where
    mkRequest :: Permissioned Slot -> AccessResult Slot
    mkRequest =
        maybe AccessDenied AccessResult . requestAccess requestedPerm
    --
    lookupSlotP
        :: (MonadDB c m, MonadHasIdentity c m)
        => Id Slot
        -> m (Maybe (Permissioned Slot))
    lookupSlotP = fmap (fmap wrapPermission) . lookupSlot
    --
    wrapPermission :: Slot -> Permissioned Slot
    wrapPermission = mkPermissioned
        (extractPermissionIgnorePolicy
        . volumeRolePolicy
        . containerVolume
        . slotContainer
        )

-- | Lookup a Volume by its Id, requesting the given permission.
accessVolume
    :: (MonadDB c m, MonadHasIdentity c m)
    => Permission
    -> Id Volume
    -> m (AccessResult Volume)
accessVolume requestedPerm =
    fmap (maybe LookupFailed mkRequest) . lookupVolumeP
  where
    mkRequest :: Permissioned Volume -> AccessResult Volume
    mkRequest =
        maybe AccessDenied AccessResult . requestAccess requestedPerm
    --
    lookupVolumeP
        :: (MonadDB c m, MonadHasIdentity c m)
        => Id Volume
        -> m (Maybe (Permissioned Volume))
    lookupVolumeP = fmap (fmap wrapPermission) . lookupVolume
    --
    wrapPermission :: Volume -> Permissioned Volume
    wrapPermission = mkPermissioned
        (extractPermissionIgnorePolicy . volumeRolePolicy)

