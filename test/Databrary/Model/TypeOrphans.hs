{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
module Databrary.Model.TypeOrphans where

import Databrary.Model.Asset.Types
import Databrary.Model.AssetSegment.Types
import Databrary.Model.AssetSlot.Types
import Databrary.Model.Metric.Types
import Databrary.Model.ORCID
import Databrary.Model.Party.Types

deriving instance Eq ORCID

deriving instance Eq PartyRow
deriving instance Show PartyRow

-- offset, release

deriving instance Show AssetRow
deriving instance Show Asset

deriving instance Show AssetSlot

deriving instance Show AssetSegment

deriving instance Eq ParticipantFieldMapping2
deriving instance Show ParticipantFieldMapping2
