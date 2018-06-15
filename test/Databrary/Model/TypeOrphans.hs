{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Databrary.Model.TypeOrphans where

import Databrary.Model.Asset.Types
import Databrary.Model.AssetSegment.Types
import Databrary.Model.AssetSlot.Types
import Databrary.Model.ORCID
import Databrary.Model.Party.Types
import Databrary.Model.Volume.Types

deriving instance Eq ORCID

deriving instance Eq PartyRow
deriving instance Show PartyRow

-- offset, release

deriving instance Show AssetRow
deriving instance Show Asset

deriving instance Show AssetSlot

deriving instance Show AssetSegment

deriving instance Eq Volume
deriving instance Show Volume

deriving instance Eq VolumeRow
deriving instance Show VolumeRow
