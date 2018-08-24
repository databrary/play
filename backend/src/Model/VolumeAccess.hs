{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings, ScopedTypeVariables, DataKinds #-}
module Model.VolumeAccess
  ( module Model.VolumeAccess.Types
  , lookupVolumeAccess
  , lookupVolumeAccessParty
  , lookupPartyVolumeAccess
  , lookupPartyVolumes
  , setDefaultVolumeAccessesForCreated
  , changeVolumeAccess
  , volumeAccessProvidesADMIN
  , volumeAccessJSON
  , volumeAccessPartyJSON
  , volumeAccessVolumeJSON
  , lookupVolumeShareActivity
  ) where

import Data.Int (Int64)
import Data.Monoid ((<>))

import Ops
import Has (peek, view)
import qualified JSON
import Service.DB
import Model.SQL
import Model.SQL.Select (selectDistinctQuery)
import Model.Time
import Model.Id.Types
import Model.Permission.Types
import Model.Identity.Types
import Model.Audit
import Model.Party
import Model.Volume
import Model.Volume.SQL
import Model.VolumeAccess.Types
import Model.VolumeAccess.SQL

lookupVolumeAccess :: (MonadDB c m, MonadHasIdentity c m) => Volume -> Permission -> m [VolumeAccess]
lookupVolumeAccess vol perm = do
  ident <- peek
  dbQuery $(selectQuery (selectVolumeAccess 'vol 'ident) "$WHERE volume_access.individual >= ${perm} ORDER BY individual DESC, sort")

lookupVolumeAccessParty :: (MonadDB c m, MonadHasIdentity c m) => Volume -> Id Party -> m (Maybe VolumeAccess)
lookupVolumeAccessParty vol p = do
  ident <- peek
  dbQuery1 $(selectQuery (selectVolumeAccessParty 'vol 'ident) "WHERE party.id = ${p}")

lookupPartyVolumeAccess :: (MonadDB c m, MonadHasIdentity c m) => Party -> Permission -> m [VolumeAccess]
lookupPartyVolumeAccess p perm = do
  ident <- peek
  dbQuery $(selectQuery (selectPartyVolumeAccess 'p 'ident) "$WHERE volume_access.individual >= ${perm} ORDER BY individual DESC, children DESC")

lookupPartyVolumes :: (MonadDB c m, MonadHasIdentity c m) => Party -> Permission -> m [Volume]
lookupPartyVolumes p perm = do
  ident <- peek
  dbQuery $(selectDistinctQuery (Just ["volume.id"]) (selectVolume 'ident) "$JOIN volume_access_view ON volume.id = volume_access_view.volume WHERE party = ${partyId $ partyRow p} AND access >= ${perm}")

setDefaultVolumeAccessesForCreated :: (MonadAudit c m) => Party -> Volume -> m ()
setDefaultVolumeAccessesForCreated owner v = do
    _ <-
        changeVolumeAccess $
            VolumeAccess PermissionADMIN PermissionADMIN Nothing (getShareFullDefault owner PermissionADMIN) owner v
    let volumeCreatePublicShareFullDefault = Just False
    _ <-
        changeVolumeAccess $
            VolumeAccess PermissionPUBLIC PermissionPUBLIC Nothing volumeCreatePublicShareFullDefault nobodyParty v
    _ <-
        changeVolumeAccess $
            VolumeAccess PermissionSHARED PermissionSHARED Nothing volumeCreatePublicShareFullDefault rootParty v
    pure ()

changeVolumeAccess :: (MonadAudit c m) => VolumeAccess -> m Bool
changeVolumeAccess va = do
  ident <- getAuditIdentity
  if volumeAccessIndividual va == PermissionNONE
    then dbExecute1 $(deleteVolumeAccess 'ident 'va)
    else (0 <) . fst <$> updateOrInsert
      $(updateVolumeAccess 'ident 'va)
      $(insertVolumeAccess 'ident 'va)

volumeAccessProvidesADMIN :: VolumeAccess -> Bool
volumeAccessProvidesADMIN VolumeAccess{ volumeAccessChildren   = PermissionADMIN, volumeAccessParty = p } = accessMember     p == PermissionADMIN
volumeAccessProvidesADMIN VolumeAccess{ volumeAccessIndividual = PermissionADMIN, volumeAccessParty = p } = accessPermission p == PermissionADMIN
volumeAccessProvidesADMIN _ = False

volumeAccessJSON :: JSON.ToObject o => VolumeAccess -> o
volumeAccessJSON VolumeAccess{..} =
     "individual" `JSON.kvObjectOrEmpty` (volumeAccessIndividual `useWhen` (volumeAccessIndividual >= PermissionNONE))
  <> "children"   `JSON.kvObjectOrEmpty` (volumeAccessChildren   `useWhen` (volumeAccessChildren   >= PermissionNONE))
  <> "sort" `JSON.kvObjectOrEmpty` volumeAccessSort
  <> "share_full" `JSON.kvObjectOrEmpty` volumeAccessShareFull

volumeAccessPartyJSON :: JSON.ToNestedObject o u => VolumeAccess -> o
volumeAccessPartyJSON va@VolumeAccess{..} = volumeAccessJSON va
  <> "party" JSON..=: partyJSON volumeAccessParty

volumeAccessVolumeJSON :: JSON.ToNestedObject o u => VolumeAccess -> o
volumeAccessVolumeJSON va@VolumeAccess{..} = volumeAccessJSON va
  <> "volume" JSON..=: volumeJSONSimple volumeAccessVolume

lookupVolumeShareActivity :: (MonadDB c m, MonadHasIdentity c m) => Int -> m [(Timestamp, Volume)]
lookupVolumeShareActivity limit = do
  ident :: Identity <- peek
  dbQuery $(selectQuery (selectVolumeActivity 'ident) "$WHERE audit.audit_action = 'add' AND audit.party = 0 AND audit.children > 'NONE' ORDER BY audit.audit_time DESC LIMIT ${fromIntegral limit :: Int64}")
