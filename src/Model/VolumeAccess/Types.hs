{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Model.VolumeAccess.Types
  ( VolumeAccess(..)
  , getShareFullDefault
  ) where

import Data.Int (Int16)

-- import Has (Has(..))
import Model.Id.Types
import Model.Permission.Types
import Model.Volume.Types
import Model.Party.Types

data VolumeAccess = VolumeAccess
  { volumeAccessIndividual, volumeAccessChildren :: Permission
  , volumeAccessSort :: Maybe Int16
  , volumeAccessShareFull :: Maybe Bool
  , volumeAccessParty :: Party
  , volumeAccessVolume :: Volume
  }

getShareFullDefault :: Party -> Permission -> Maybe Bool
getShareFullDefault targetParty individualAccessLevel =
    if (getPartyId targetParty, individualAccessLevel) == (nobodyId, PermissionPUBLIC)
    then nobodyPublicLegacyDefault
    else generalDefault
  where
    nobodyId :: Id Party
    nobodyId = (getPartyId . accountParty . siteAccount) nobodySiteAuth
    nobodyPublicLegacyDefault = Just True
    generalDefault = Nothing

{-
instance Has Volume VolumeAccess where
  view = volumeAccessVolume
instance Has (Id Volume) VolumeAccess where
  view = (Has.view . volumeAccessVolume)
instance Has Party VolumeAccess where
  view = volumeAccessParty
instance Has (Id Party) VolumeAccess where
  view = (view . volumeAccessParty)
-}
