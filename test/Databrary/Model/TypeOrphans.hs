{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Databrary.Model.TypeOrphans where

import Databrary.Model.Age
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSegment.Types
import Databrary.Model.AssetSlot.Types
import Databrary.Model.Metric.Types
import Databrary.Model.Format.Types
import Databrary.Model.ORCID
import Databrary.Model.Party.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Release.Types
import Databrary.Model.Volume.Types

deriving instance Show Age

deriving instance Show Access

deriving instance Show AssetRow
deriving instance Show Asset

deriving instance Show AssetSlot

deriving instance Show AssetSegment

deriving instance Eq ParticipantFieldMapping2
deriving instance Show ParticipantFieldMapping2

deriving instance Eq EffectiveRelease
deriving instance Show EffectiveRelease

deriving instance Show Format

deriving instance Eq ORCID

deriving instance Eq PartyRow
deriving instance Show PartyRow

deriving instance Eq PublicPolicy
deriving instance Show PublicPolicy

deriving instance Eq SharedPolicy
deriving instance Show SharedPolicy

-- offset, release
deriving instance Eq VolumeRolePolicy
deriving instance Show VolumeRolePolicy

deriving instance Eq Volume
deriving instance Show Volume

deriving instance Eq VolumeRow
deriving instance Show VolumeRow

