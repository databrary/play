{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Model.TypeOrphans where

import Model.Age
import Model.Asset.Types
import Model.AssetSegment.Types
import Model.AssetSlot.Types
import Model.Citation.Types
import Model.Format.Types
import Model.Funding.Types
import Model.GeoNames
import Model.Metric.Types
import Model.ORCID
import Model.Paginate
import Model.Party.Types
import Model.Permission
import Model.Release.Types
import Model.Record.Types
import Model.Slot.Types
import Model.Tag.Types
import Model.Volume.Types
import Model.Volume (LookupResult (..))

deriving instance Show Age

deriving instance Show Access

deriving instance Show AssetRow
deriving instance Show Asset

deriving instance Show AssetSlot

deriving instance Show AssetSegment

deriving instance Eq Citation
deriving instance Show Citation

deriving instance Eq ParticipantFieldMapping2
deriving instance Show ParticipantFieldMapping2

deriving instance Eq EffectiveRelease
deriving instance Show EffectiveRelease

deriving instance Eq a => Eq (FieldUse a)
deriving instance Show a => Show (FieldUse a)

deriving instance Show Format

deriving instance Eq Funder
deriving instance Show Funder

-- deriving instance Eq Funding
-- deriving instance Show Funding

deriving instance Eq GeoName
deriving instance Show GeoName

deriving instance Eq ORCID

deriving instance Eq Paginate
deriving instance Show Paginate

deriving instance Eq ParticipantRecord
deriving instance Show ParticipantRecord

deriving instance Eq PartyRow
deriving instance Show PartyRow

deriving instance Eq PublicPolicy
deriving instance Show PublicPolicy

deriving instance Eq RecordRow
deriving instance Show RecordRow

deriving instance Eq SharedPolicy
deriving instance Show SharedPolicy

-- deriving instance Eq Slot
instance Show Slot where 
  show _ = "Slot"
-- deriving instance Show Slot

deriving instance Eq TagName
deriving instance Show TagName

-- offset, release
deriving instance Eq VolumeRolePolicy
deriving instance Show VolumeRolePolicy

deriving instance Eq Volume
deriving instance Show Volume

deriving instance Eq VolumeRow
deriving instance Show VolumeRow

deriving instance Show a => Show (LookupResult a)
deriving instance Eq a => Eq (LookupResult a)

deriving instance Show a => Show (PermissionResponse a)
deriving instance Eq a => Eq (PermissionResponse a)
